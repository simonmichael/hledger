{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ParallelListComp  #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE RecordWildCards   #-}
{-|

The @roi@ command prints internal rate of return and time-weighted rate of return for and investment.

-}

module Hledger.Cli.Commands.Roi (
  roimode
  , roi
) where

import Control.Monad
import System.Exit
import Data.Time.Calendar
import Text.Printf
import Data.Function (on)
import Data.List
import Data.Maybe (fromMaybe)
import Numeric.RootFinding
import Data.Decimal
import qualified Data.Text as T
import qualified Data.Text.Lazy.IO as TL
import System.Console.CmdArgs.Explicit as CmdArgs

import Text.Tabular as Tbl
import Text.Tabular.AsciiWide as Ascii

import Hledger
import Hledger.Cli.CliOptions


roimode = hledgerCommandMode
  $(embedFileRelative "Hledger/Cli/Commands/Roi.txt")
  [flagNone ["cashflow"] (setboolopt "cashflow") "show all amounts that were used to compute returns"
  ,flagReq ["investment"] (\s opts -> Right $ setopt "investment" s opts) "QUERY"
    "query to select your investment transactions"
  ,flagReq ["profit-loss","pnl"] (\s opts -> Right $ setopt "pnl" s opts) "QUERY"
    "query to select profit-and-loss or appreciation/valuation transactions"
  ]
  [generalflagsgroup1]
  hiddenflags
  ([], Just $ argsFlag "[QUERY]")

-- One reporting span,
data OneSpan = OneSpan
  Day -- start date, inclusive
  Day   -- end date, exclusive
  MixedAmount -- value of investment at the beginning of day on spanBegin_
  MixedAmount -- value of investment at the end of day on spanEnd_
  [(Day,MixedAmount)] -- all deposits and withdrawals (but not changes of value) in the DateSpan [spanBegin_,spanEnd_)
  [(Day,MixedAmount)] -- all PnL changes of the value of investment in the DateSpan [spanBegin_,spanEnd_)
 deriving (Show)


roi ::  CliOpts -> Journal -> IO ()
roi CliOpts{rawopts_=rawopts, reportspec_=rspec@ReportSpec{rsOpts=ReportOpts{..}}} j = do
  d <- getCurrentDay
  -- We may be converting posting amounts to value, per hledger_options.m4.md "Effect of --value on reports".
  let
    tvalue t@Transaction{..} = t{tpostings=map pvalue tpostings}
      where
        pvalue = maybe id
          (postingApplyValuation (journalPriceOracle infer_value_ j) (journalCommodityStyles j) periodlast (rsToday rspec))
          value_
          where
            periodlast  = fromMaybe (rsToday rspec) $ reportPeriodOrJournalLastDay rspec j
  let
    ropts = rsOpts rspec
    showCashFlow = boolopt "cashflow" rawopts
    prettyTables = pretty_tables_
    makeQuery flag = do
        q <- either usageError (return . fst) . parseQuery d . T.pack $ stringopt flag rawopts
        return . simplifyQuery $ And [queryFromFlags ropts{period_=PeriodAll}, q]

  investmentsQuery <- makeQuery "investment"
  pnlQuery         <- makeQuery "pnl"

  let
    trans = dbg3 "investments" $ map tvalue $ jtxns $ filterJournalTransactions investmentsQuery j

    journalSpan =
        let dates = map transactionDate2 trans in
        DateSpan (Just $ minimum dates) (Just $ addDays 1 $ maximum dates)

    requestedSpan = periodAsDateSpan period_
    requestedInterval = interval_

    wholeSpan = spanDefaultsFrom requestedSpan journalSpan

  when (null trans) $ do
    putStrLn "No relevant transactions found. Check your investments query"
    exitFailure

  let spans = case requestedInterval of
        NoInterval -> [wholeSpan]
        interval ->
            splitSpan interval $
            spanIntersect journalSpan wholeSpan

  tableBody <- forM spans $ \(DateSpan (Just spanBegin) (Just spanEnd)) -> do
    -- Spans are [spanBegin,spanEnd), and spanEnd is 1 day after then actual end date we are interested in
    let
      valueBefore =
        total trans (And [ investmentsQuery
                         , Date (DateSpan Nothing (Just spanBegin))])

      valueAfter  =
        total trans (And [investmentsQuery
                         , Date (DateSpan Nothing (Just spanEnd))])

      cashFlow =
        calculateCashFlow trans (And [ Not investmentsQuery
                                     , Not pnlQuery
                                     , Date (DateSpan (Just spanBegin) (Just spanEnd)) ] )

      pnl =
        calculateCashFlow trans (And [ Not investmentsQuery
                                     , pnlQuery
                                     , Date (DateSpan (Just spanBegin) (Just spanEnd)) ] )

      thisSpan = dbg3 "processing span" $
                 OneSpan spanBegin spanEnd valueBefore valueAfter cashFlow pnl

    irr <- internalRateOfReturn showCashFlow prettyTables thisSpan
    twr <- timeWeightedReturn showCashFlow prettyTables investmentsQuery trans thisSpan
    let cashFlowAmt = negate $ sum $ map snd cashFlow
    let smallIsZero x = if abs x < 0.01 then 0.0 else x
    return [ showDate spanBegin
           , showDate (addDays (-1) spanEnd)
           , T.pack $ showMixedAmount valueBefore
           , T.pack $ showMixedAmount cashFlowAmt
           , T.pack $ showMixedAmount valueAfter
           , T.pack $ showMixedAmount (valueAfter - (valueBefore + cashFlowAmt))
           , T.pack $ printf "%0.2f%%" $ smallIsZero irr
           , T.pack $ printf "%0.2f%%" $ smallIsZero twr ]

  let table = Table
              (Tbl.Group NoLine (map (Header . T.pack . show) (take (length tableBody) [1..])))
              (Tbl.Group DoubleLine
               [ Tbl.Group SingleLine [Header "Begin", Header "End"]
               , Tbl.Group SingleLine [Header "Value (begin)", Header "Cashflow", Header "Value (end)", Header "PnL"]
               , Tbl.Group SingleLine [Header "IRR", Header "TWR"]])
              tableBody

  TL.putStrLn $ Ascii.render prettyTables id id id table

timeWeightedReturn showCashFlow prettyTables investmentsQuery trans (OneSpan spanBegin spanEnd valueBeforeAmt valueAfter cashFlow pnl) = do
  let valueBefore = unMix valueBeforeAmt
  let initialUnitPrice = 100 :: Decimal
  let initialUnits = valueBefore / initialUnitPrice
  let changes =
        -- If cash flow and PnL changes happen on the same day, this
        -- will sort PnL changes to come before cash flows (on any
        -- given day), so that we will have better unit price computed
        -- first for processing cash flow. This is why pnl changes are Left
        -- and cashflows are Right
        sort
        $ (++) (map (\(date,amt) -> (date,Left (-amt))) pnl )
        -- Aggregate all entries for a single day, assuming that intraday interest is negligible
        $ map (\date_cash -> let (dates, cash) = unzip date_cash in (head dates, Right (sum cash)))
        $ groupBy ((==) `on` fst)
        $ sortOn fst
        $ map (\(d,a) -> (d, negate a))
        $ cashFlow

  let units =
        tail $
        scanl
          (\(_, _, unitPrice, unitBalance) (date, amt) ->
             let valueOnDate = unMix $ total trans (And [investmentsQuery, Date (DateSpan Nothing (Just date))])
             in
             case amt of
               Right amt ->
                 -- we are buying or selling
                 let unitsBoughtOrSold = unMix amt / unitPrice
                 in (valueOnDate, unitsBoughtOrSold, unitPrice, unitBalance + unitsBoughtOrSold)
               Left pnl ->
                 -- PnL change
                 let valueAfterDate = valueOnDate + unMix pnl
                     unitPrice' = valueAfterDate/unitBalance
                 in (valueOnDate, 0, unitPrice', unitBalance))
          (0, 0, initialUnitPrice, initialUnits)
          changes

  let finalUnitBalance = if null units then initialUnits else let (_,_,_,u) = last units in u
      finalUnitPrice = if finalUnitBalance == 0 then initialUnitPrice
                       else (unMix valueAfter) / finalUnitBalance
      -- Technically, totalTWR should be (100*(finalUnitPrice - initialUnitPrice) / initialUnitPrice), but initalUnitPrice is 100, so 100/100 == 1
      totalTWR = roundTo 2 $ (finalUnitPrice - initialUnitPrice)
      years = fromIntegral (diffDays spanEnd spanBegin) / 365 :: Double
      annualizedTWR = 100*((1+(realToFrac totalTWR/100))**(1/years)-1) :: Double

  when showCashFlow $ do
    printf "\nTWR cash flow for %s - %s\n" (showDate spanBegin) (showDate (addDays (-1) spanEnd))
    let (dates', amounts) = unzip changes
        cashflows' = map (either (\_ -> 0) id) amounts
        pnls' = map (either id (\_ -> 0)) amounts
        (valuesOnDate',unitsBoughtOrSold', unitPrices', unitBalances') = unzip4 units
        add x lst = if valueBefore/=0 then x:lst else lst
        dates = add spanBegin dates'
        cashflows = add valueBeforeAmt cashflows'
        pnls = add 0 pnls'
        unitsBoughtOrSold = add initialUnits unitsBoughtOrSold'
        unitPrices = add initialUnitPrice unitPrices'
        unitBalances = add initialUnits unitBalances'
        valuesOnDate = add 0 valuesOnDate'

    TL.putStr $ Ascii.render prettyTables id id T.pack
      (Table
       (Tbl.Group NoLine (map (Header . showDate) dates))
       (Tbl.Group DoubleLine [ Tbl.Group SingleLine [Header "Portfolio value", Header "Unit balance"]
                         , Tbl.Group SingleLine [Header "Pnl", Header "Cashflow", Header "Unit price", Header "Units"]
                         , Tbl.Group SingleLine [Header "New Unit Balance"]])
       [ [value, oldBalance, pnl, cashflow, prc, udelta, balance]
       | value <- map showDecimal valuesOnDate
       | oldBalance <- map showDecimal (0:unitBalances)
       | balance <- map showDecimal unitBalances
       | pnl <- map showMixedAmount pnls
       | cashflow <- map showMixedAmount cashflows
       | prc <- map showDecimal unitPrices
       | udelta <- map showDecimal unitsBoughtOrSold ])

    printf "Final unit price: %s/%s units = %s\nTotal TWR: %s%%.\nPeriod: %.2f years.\nAnnualized TWR: %.2f%%\n\n"
      (showMixedAmount valueAfter) (showDecimal finalUnitBalance) (showDecimal finalUnitPrice) (showDecimal totalTWR) years annualizedTWR

  return annualizedTWR

internalRateOfReturn showCashFlow prettyTables (OneSpan spanBegin spanEnd valueBefore valueAfter cashFlow _pnl) = do
  let prefix = (spanBegin, negate valueBefore)

      postfix = (spanEnd, valueAfter)

      totalCF = filter ((/=0) . snd) $ prefix : (sortOn fst cashFlow) ++ [postfix]

  when showCashFlow $ do
    printf "\nIRR cash flow for %s - %s\n" (showDate spanBegin) (showDate (addDays (-1) spanEnd))
    let (dates, amounts) = unzip totalCF
    TL.putStrLn $ Ascii.render prettyTables id id id
      (Table
       (Tbl.Group NoLine (map (Header . showDate) dates))
       (Tbl.Group SingleLine [Header "Amount"])
       (map ((:[]) . T.pack . showMixedAmount) amounts))

  -- 0% is always a solution, so require at least something here
  case totalCF of
    [] -> return 0
    _ -> case ridders (RiddersParam 100 (AbsTol 0.00001))
                      (0.000000000001,10000)
                      (interestSum spanEnd totalCF) of
        Root rate    -> return ((rate-1)*100)
        NotBracketed -> error' $ "Error (NotBracketed): No solution for Internal Rate of Return (IRR).\n"
                        ++       "  Possible causes: IRR is huge (>1000000%), balance of investment becomes negative at some point in time."
        SearchFailed -> error' $ "Error (SearchFailed): Failed to find solution for Internal Rate of Return (IRR).\n"
                        ++       "  Either search does not converge to a solution, or converges too slowly."

type CashFlow = [(Day, MixedAmount)]

interestSum :: Day -> CashFlow -> Double -> Double
interestSum referenceDay cf rate = sum $ map go cf
    where go (t,m) = fromRational (toRational (unMix m)) * (rate ** (fromIntegral (referenceDay `diffDays` t) / 365))


calculateCashFlow :: [Transaction] -> Query -> CashFlow
calculateCashFlow trans query = filter ((/=0).snd) $ map go trans
    where
    go t = (transactionDate2 t, total [t] query)

total :: [Transaction] -> Query -> MixedAmount
total trans query = sumPostings $  filter (matchesPosting query) $ concatMap realPostings trans

unMix :: MixedAmount -> Quantity
unMix a =
  case (unifyMixedAmount $ mixedAmountCost a) of
    Just a -> aquantity a
    Nothing -> error' $ "Amounts could not be converted to a single cost basis: " ++ show (map showAmount $ amounts a) ++
               "\nConsider using --value to force all costs to be in a single commodity." ++
               "\nFor example, \"--value cost,<commodity> --infer-value\", where commodity is the one that was used to pay for the investment."

-- Show Decimal rounded to two decimal places, unless it has less places already. This ensures that "2" won't be shown as "2.00"
showDecimal :: Decimal -> String
showDecimal d = if d == rounded then show d else show rounded
  where
    rounded = roundTo 2 d


