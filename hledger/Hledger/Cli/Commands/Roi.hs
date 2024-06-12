{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ParallelListComp  #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ViewPatterns #-}
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
import Data.Bifunctor (second)
import Data.Either (fromLeft, fromRight, isLeft)
import Data.Function (on)
import Data.List
import Numeric.RootFinding
import Data.Decimal
import qualified Data.Text as T
import qualified Data.Text.Lazy.IO as TL
import Safe (headDef, tailDef)
import System.Console.CmdArgs.Explicit as CmdArgs

import Text.Tabular.AsciiWide as Tab

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
  cligeneralflagsgroups1
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
roi CliOpts{rawopts_=rawopts, reportspec_=rspec@ReportSpec{_rsReportOpts=ReportOpts{..}}} j = do
  -- We may be converting posting amounts to value, per hledger_options.m4.md "Effect of --value on reports".
  let
    -- lbl = lbl_ "roi"
    today = _rsDay rspec
    priceOracle = journalPriceOracle infer_prices_ j
    styles = journalCommodityStylesWith HardRounding j
    mixedAmountValue periodlast date =
        -- These calculations can generate very precise decimals. To avoid showing too many digits:
        -- If we have no style for the valuation commodity, generate one that will limit the precision ?
        -- But it's not easy to find out the valuation commodity (or commodities) here if it's implicit,
        -- as that information is buried in the price graph.
        -- Instead, do what we don't like to do: hard code a max precision, overriding commodity styles.
        mixedAmountSetPrecisionMax defaultMaxPrecision
      . maybe id (mixedAmountApplyValuation priceOracle styles periodlast today date) value_
      . maybe id (mixedAmountToCost styles) conversionop_

  let
    ropts = _rsReportOpts rspec
    wd = whichDate ropts
    showCashFlow = boolopt "cashflow" rawopts
    prettyTables = pretty_
    makeQuery flag = do
        q <- either usageError (return . fst) . parseQuery today . T.pack $ stringopt flag rawopts
        return . simplifyQuery $ And [queryFromFlags ropts{period_=PeriodAll}, q]

  investmentsQuery <- makeQuery "investment"
  pnlQuery         <- makeQuery "pnl"

  let
    filteredj = filterJournalTransactions investmentsQuery j
    trans = dbg3 "investments" $ jtxns filteredj

  when (null trans) $ do
    putStrLn "No relevant transactions found. Check your investments query"
    exitFailure

  let (fullPeriod, spans) = reportSpan filteredj rspec

  let priceDirectiveDates = dbg3 "priceDirectiveDates" $ map pddate $ jpricedirectives j

  let processSpan (DateSpan Nothing _) = error "Undefined start of the period - will be unable to compute the rates of return"
      processSpan (DateSpan _ Nothing) = error "Undefined end of the period - will be unable to compute the rates of return"
      processSpan spn@(DateSpan (Just begin) (Just end)) = do
        -- Spans are [begin,end), and end is 1 day after the actual end date we are interested in
        let
          b = fromEFDay begin
          e = fromEFDay end
          cashFlowApplyCostValue = map (\(d,amt) -> (d,mixedAmountValue e d amt))

          valueBefore = dbg3 "valueBefore" $
            mixedAmountValue e b $
            total trans (And [ investmentsQuery
                             , Date (DateSpan Nothing (Just begin))])

          valueAfter  = dbg3 "valueAfter" $
            mixedAmountValue e e $
            total trans (And [investmentsQuery
                             , Date (DateSpan Nothing (Just end))])

          priceDates = dbg3 "priceDates" $ nub $ filter (spanContainsDate spn) priceDirectiveDates
          cashFlow =
            ((map (,nullmixedamt) priceDates)++) $
            cashFlowApplyCostValue $
            calculateCashFlow wd trans (And [ Not investmentsQuery
                                            , Not pnlQuery
                                            , Date spn ] )

          pnl =
            cashFlowApplyCostValue $
            calculateCashFlow wd trans (And [ Not investmentsQuery
                                            , pnlQuery
                                            , Date spn ] )

          thisSpan = dbg3 "processing span" $
                     OneSpan b e valueBefore valueAfter cashFlow pnl

        irr <- internalRateOfReturn styles showCashFlow prettyTables thisSpan
        (periodTwr, annualizedTwr) <- timeWeightedReturn styles showCashFlow prettyTables investmentsQuery trans mixedAmountValue thisSpan
        let cashFlowAmt = maNegate . maSum $ map snd cashFlow
        let smallIsZero x = if abs x < 0.01 then 0.0 else x
        return [ showDate b
               , showDate (addDays (-1) e)
               , T.pack $ showMixedAmountOneLineWithoutCost False $ styleAmounts styles $ valueBefore
               , T.pack $ showMixedAmountOneLineWithoutCost False $ styleAmounts styles $ cashFlowAmt
               -- , T.pack $ showMixedAmount $
               --   -- dbg0With (lbl "cashflow after styling".showMixedAmountOneLine) $
               --   mapMixedAmount (amountSetFullPrecisionUpTo (Just defaultMaxPrecision)) $
               --   styleAmounts (styles
               --                 -- & dbg0With (lbl "styles".show))
               --   cashFlowAmt
               --   -- & dbg0With (lbl "cashflow before styling".showMixedAmountOneLine)
               , T.pack $ showMixedAmountOneLineWithoutCost False $ styleAmounts styles $ valueAfter
               , T.pack $ showMixedAmountOneLineWithoutCost False $ styleAmounts styles $ (valueAfter `maMinus` (valueBefore `maPlus` cashFlowAmt))
               , T.pack $ printf "%0.2f%%" $ smallIsZero irr
               , T.pack $ printf "%0.2f%%" $ smallIsZero periodTwr
               , T.pack $ printf "%0.2f%%" $ smallIsZero annualizedTwr ]

  periodRows <- forM spans processSpan
  totalRow <- processSpan fullPeriod

  let rowTitles = Tab.Group Tab.NoLine (map (Header . T.pack . show) (take (length periodRows) [1..]))

  let isSingleSpan = length spans == 1

  let table = Table
              (if isSingleSpan
                then rowTitles
                else Tab.Group Tab.SingleLine  [ rowTitles, Tab.Group Tab.NoLine [ Header "Total" ]]
              )
              (Tab.Group Tab.DoubleLine
               [ Tab.Group Tab.SingleLine [Header "Begin", Header "End"]
               , Tab.Group Tab.SingleLine [Header "Value (begin)", Header "Cashflow", Header "Value (end)", Header "PnL"]
               , Tab.Group Tab.SingleLine [Header "IRR"]
               , Tab.Group Tab.SingleLine [Header "TWR/period", Header "TWR/year"]])
              (if isSingleSpan then periodRows else periodRows ++ [totalRow])

  TL.putStrLn $ Tab.render prettyTables id id id table

timeWeightedReturn styles showCashFlow prettyTables investmentsQuery trans mixedAmountValue (OneSpan begin end valueBeforeAmt valueAfter cashFlow pnl) = do
  let valueBefore = unMix valueBeforeAmt
  let initialUnitCost = 100 :: Decimal
  let initialUnits = valueBefore / initialUnitCost
  let changes =
        -- If cash flow and PnL changes happen on the same day, this
        -- will sort PnL changes to come before cash flows (on any
        -- given day), so that we will have better unit price computed
        -- first for processing cash flow. This is why pnl changes are Left
        -- and cashflows are Right.
        -- However, if the very first date in the changes list has both
        -- PnL and CashFlow, we would not be able to apply pnl change to 0 unit,
        -- which would lead to an error. We make sure that we have at least one
        -- cashflow entry at the front, and we know that there would be at most
        -- one for the given date, by construction. Empty CashFlows added
        -- because of a begin date before the first transaction are not seen as
        -- a valid cashflow entry at the front.
        zeroUnitsNeedsCashflowAtTheFront
        $ sort
        $ datedCashflows ++ datedPnls
        where
          zeroUnitsNeedsCashflowAtTheFront changes1 =
            if initialUnits > 0 then changes1
            else
              let (leadingEmptyCashFlows, rest) = span isEmptyCashflow changes1
                  (leadingPnls, rest') = span (isLeft . snd) rest
                  (firstCashflow, rest'') = splitAt 1 rest'
              in leadingEmptyCashFlows ++ firstCashflow ++ leadingPnls ++ rest''

          isEmptyCashflow (_date, amt) = case amt of
            Right amt' -> mixedAmountIsZero amt'
            Left _     -> False

          datedPnls = map (second Left) $ aggregateByDate pnl

          datedCashflows = map (second Right) $ aggregateByDate cashFlow

          aggregateByDate datedAmounts =
            -- Aggregate all entries for a single day, assuming that intraday interest is negligible
            sort
            $ map (\datecashes -> let (dates, cash) = unzip datecashes in (headDef (error' "Roi.hs: datecashes was null, please report a bug") dates, maSum cash))
            $ groupBy ((==) `on` fst)
            $ sortOn fst
            $ map (second maNegate)
            $ datedAmounts

  let units =
        tailDef (error' "Roi.hs units was null, please report a bug") $
        scanl
          (\(_, _, unitCost, unitBalance) (date, amt) ->
             let valueOnDate = unMix $ mixedAmountValue end date $ total trans (And [investmentsQuery, Date (DateSpan Nothing (Just $ Exact date))])
             in
             case amt of
               Right amt' ->
                 -- we are buying or selling
                 let unitsBoughtOrSold = unMix amt' / unitCost
                 in (valueOnDate, unitsBoughtOrSold, unitCost, unitBalance + unitsBoughtOrSold)
               Left pnl' ->
                 -- PnL change
                 let valueAfterDate = valueOnDate + unMix pnl'
                     unitCost' = valueAfterDate/unitBalance
                 in (valueOnDate, 0, unitCost', unitBalance))
          (0, 0, initialUnitCost, initialUnits)
          $ dbg3 "changes" changes

  let finalUnitBalance = if null units then initialUnits else let (_,_,_,u) = last units in u
      finalUnitCost = if finalUnitBalance == 0 then
                         if null units then initialUnitCost
                         else let (_,_,lastUnitCost,_) = last units in lastUnitCost
                       else (unMix valueAfter) / finalUnitBalance
      -- Technically, totalTWR should be (100*(finalUnitCost - initialUnitCost) / initialUnitCost), but initalUnitCost is 100, so 100/100 == 1
      totalTWR = roundTo 2 $ (finalUnitCost - initialUnitCost)
      (startYear, _, _) = toGregorian begin
      years = fromIntegral (diffDays end begin) / (if isLeapYear startYear then 366 else 365) :: Double
      annualizedTWR = 100*((1+(realToFrac totalTWR/100))**(1/years)-1) :: Double

  when showCashFlow $ do
    printf "\nTWR cash flow for %s - %s\n" (showDate begin) (showDate (addDays (-1) end))
    let (dates', amts) = unzip changes
        cashflows' = map (fromRight nullmixedamt) amts
        pnls = map (fromLeft nullmixedamt) amts
        (valuesOnDate,unitsBoughtOrSold', unitPrices', unitBalances') = unzip4 units
        add x lst = if valueBefore/=0 then x:lst else lst
        dates = add begin dates'
        cashflows = add valueBeforeAmt cashflows'
        unitsBoughtOrSold = add initialUnits unitsBoughtOrSold'
        unitPrices = add initialUnitCost unitPrices'
        unitBalances = add initialUnits unitBalances'

    TL.putStr $ Tab.render prettyTables id id T.pack
      (Table
       (Tab.Group NoLine (map (Header . showDate) dates))
       (Tab.Group DoubleLine [ Tab.Group Tab.SingleLine [Tab.Header "Portfolio value", Tab.Header "Unit balance"]
                         , Tab.Group Tab.SingleLine [Tab.Header "Pnl", Tab.Header "Cashflow", Tab.Header "Unit price", Tab.Header "Units"]
                         , Tab.Group Tab.SingleLine [Tab.Header "New Unit Balance"]])
       [ [val, oldBalance, pnl', cashflow, prc, udelta, balance]
       | val <- map showDecimal valuesOnDate
       | oldBalance <- map showDecimal (0:unitBalances)
       | balance <- map showDecimal unitBalances
       | pnl' <- map (showMixedAmountOneLineWithoutCost False . styleAmounts styles) pnls
       | cashflow <- map (showMixedAmountOneLineWithoutCost False . styleAmounts styles) cashflows
       | prc <- map showDecimal unitPrices
       | udelta <- map showDecimal unitsBoughtOrSold ])

    printf "Final unit price: %s/%s units = %s\nTotal TWR: %s%%.\nPeriod: %.2f years.\nAnnualized TWR: %.2f%%\n\n"
      (showMixedAmountOneLineWithoutCost False $ styleAmounts styles valueAfter) (showDecimal finalUnitBalance) (showDecimal finalUnitCost) (showDecimal totalTWR) years annualizedTWR

  return ((realToFrac totalTWR) :: Double, annualizedTWR)

internalRateOfReturn styles showCashFlow prettyTables (OneSpan begin end valueBefore valueAfter cashFlow _pnl) = do
  let prefix = (begin, maNegate valueBefore)

      postfix = (end, valueAfter)

      totalCF = filter (maIsNonZero . snd) $ prefix : (sortOn fst cashFlow) ++ [postfix]

  when showCashFlow $ do
    printf "\nIRR cash flow for %s - %s\n" (showDate begin) (showDate (addDays (-1) end))
    let (dates, amts) = unzip totalCF
    TL.putStrLn $ Tab.render prettyTables id id id
      (Table
       (Tab.Group Tab.NoLine (map (Header . showDate) dates))
       (Tab.Group Tab.SingleLine [Header "Amount"])
       (map ((:[]) . T.pack . showMixedAmountOneLineWithoutCost False . styleAmounts styles) amts))

  -- 0% is always a solution, so require at least something here
  case totalCF of
    [] -> return 0
    _ -> case ridders (RiddersParam 100 (AbsTol 0.00001))
                      (0.000000000001,10000)
                      (interestSum end totalCF) of
        Root rate    -> return ((rate-1)*100)
        NotBracketed -> error' $ "Error (NotBracketed): No solution for Internal Rate of Return (IRR).\n"
                        ++       "  Possible causes: IRR is huge (>1000000%), balance of investment becomes negative at some point in time."
        SearchFailed -> error' $ "Error (SearchFailed): Failed to find solution for Internal Rate of Return (IRR).\n"
                        ++       "  Either search does not converge to a solution, or converges too slowly."

type CashFlow = [(Day, MixedAmount)]

interestSum :: Day -> CashFlow -> Double -> Double
interestSum referenceDay cf rate = sum $ map go cf
  where go (t,m) = realToFrac (unMix m) * rate ** (fromIntegral (referenceDay `diffDays` t) / 365)


calculateCashFlow :: WhichDate -> [Transaction] -> Query -> CashFlow
calculateCashFlow wd trans query =
  [ (postingDateOrDate2 wd p, pamount p) | p <- concatMap (filter (matchesPosting query) . realPostings) trans, maIsNonZero (pamount p) ]

total :: [Transaction] -> Query -> MixedAmount
total trans query = sumPostings (concatMap (filter (matchesPosting query) . realPostings) trans)

unMix :: MixedAmount -> Quantity
unMix a =
  case (unifyMixedAmount a) of
    Just a' -> aquantity a'
    Nothing -> error' $ "Amounts could not be converted to a single commodity: " ++ show (map showAmount $ amounts a) ++
               "\nConsider using --value to force all costs to be in a single commodity." ++
               "\nFor example, \"--value=end,<commodity> --infer-market-prices\", where commodity is the one that was used for investment valuations."

-- Show Decimal rounded to two decimal places, unless it has less places already. This ensures that "2" won't be shown as "2.00"
showDecimal :: Decimal -> String
showDecimal d = if d == rounded then show d else show rounded
  where
    rounded = roundTo 2 d
