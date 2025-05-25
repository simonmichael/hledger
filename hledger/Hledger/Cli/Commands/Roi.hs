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
import Data.Time.Calendar
import Text.Printf
import Data.Bifunctor (second)
import Data.Function (on)
import Data.List
import Numeric.RootFinding
import Data.Decimal
import qualified Data.Text as T
import qualified Data.Text.Lazy.IO as TL
import Safe (headDef)
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

  when (null trans) $
    error' "No relevant transactions found. Check your investments query"

  let (fullPeriod, spans) = reportSpan filteredj rspec

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

          cashFlow = dbg3 "cashFlow" $
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
  totalRow <- case periodRows of
    [singleRow] -> return singleRow
    _           -> processSpan fullPeriod

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

-- Entry for TWR computation, capturing all cashflows that are potentially accompanied by pnl change on the same day (if not, it is zero)
data TwrPeriod = TwrPeriod { twrStartDate :: Day, twrEndDate :: Day, twrStartValue :: Decimal, twrValueBeforeCashflow :: Decimal, twrPnl :: Decimal, twrCashflow :: Decimal, twrValueAfterCashflow :: Decimal } deriving (Eq, Show)

timeWeightedReturn _styles showCashFlow prettyTables investmentsQuery trans mixedAmountValue (OneSpan begin end valueBeforeAmt valueAfterAmt cashflows pnls) = do
  let datedCashflows =
        -- Aggregate all entries for a single day, assuming that intraday interest is negligible
        dbg3 "datedCashflows"
        $ sort
        $ map (\datecashes -> let (dates, cash) = unzip datecashes in (headDef (error' "Roi.hs: datecashes was null, please report a bug") dates, maSum cash))
        $ groupBy ((==) `on` fst)
        $ sortOn fst
        $ map (second maNegate)
        $ cashflows

      valueBefore = dbg3 ("value at the start of the interval, "++show begin) $ unMix valueBeforeAmt
      valueAfter = dbg3 ("value at the end of the interval, "++show end) $ unMix valueAfterAmt
      
      investmentPostings = concatMap (filter (matchesPosting investmentsQuery) . realPostings) trans

      totalInvestmentPostingsTill date = sumPostings $ filter (matchesPosting (Date (DateSpan Nothing (Just $ Exact date)))) investmentPostings

      -- filter span is (-infinity, date+1), which gives us effectively (-infinity, date]
      valueAfterDate date = unMix $ mixedAmountValue end date $ totalInvestmentPostingsTill (addDays 1 date)

      pnlOn date = unMix $ maNegate $ sum $ map snd $ filter ((==date).fst) pnls

  -- We are dividing the period [begin, end) into subperiods on each cashflow, and then compute
  -- the rate of return for each subperiod. For this we need to know the value of the investment
  -- at the beginning and end of each subperiod, adjusted for cashflow.
  -- 
  -- Subperiods are going to be [valueBefore ... (c_0,v_0)][... (c_1, v_1)][... (c_2,v_2)] ... [... (c_n,v_n)][... valueAfter]
  -- , where v_i is the value of investment computed immediately after cashflow c_i
  --
  -- Calculate interest for each subperiod, adjusting the value at the start of the period by the cashflow
  -- For subperiods [valueBefore ... (c_0,v_0)][... (c_1, v_1)][... (c_2,v_2)] ... [... (c_n,v_n)][... valueAfter], the computation is going to be
  -- 1 + twr = (v_0 - c_0)/valueBefore + (v_1 - c_1) / v_0 +  ... + valueAfter/v_n
  -- See https://en.wikipedia.org/wiki/Time-weighted_return#Time-weighted_return_compensating_for_external_flows
  let calculateSubPeriods (startDate,startValue) [] =
        let subPeriodReturn =
              if startValue == 0 || valueAfter == 0
              then 0
              else valueAfter/startValue - 1
        in
        [(subPeriodReturn, TwrPeriod startDate end startValue valueAfter 0 0 valueAfter)]
      calculateSubPeriods (startDate,startValue) ((date,cashflow):rest) =
        let (valueBeforeCashflow, valueAfterCashflow, pnl) =
              let valueAfterPrevDay = valueAfterDate (addDays (-1) date)
                  pnlOnDay = pnlOn date
              in
                -- If value was zero at the start of the period, then any PnL on cashflow date would accrue after it, not before.
                -- If there was some value already, we can assume that PnL contributes to this period's rate
                if startValue == 0
                then (valueAfterPrevDay, valueAfterDate date - pnlOnDay, 0)
                else (valueAfterPrevDay + pnl, valueAfterDate date, pnlOnDay)
            subPeriodReturn =
              if valueBeforeCashflow == 0 || startValue == 0
              then 0
              else (valueBeforeCashflow / startValue) - 1
        in
        (subPeriodReturn, (TwrPeriod startDate date startValue valueBeforeCashflow pnl (unMix cashflow) valueAfterCashflow)) : calculateSubPeriods (date,valueAfterCashflow) rest

  let subPeriods = dbg3 "subPeriods" $ calculateSubPeriods (begin,valueBefore) datedCashflows

  -- Compute overall time-weighted rate of return
  let twr =
        dbg3 "twr" $
        if subPeriods == []
        then if valueBefore == 0 then 0 else (valueAfter - valueBefore)/valueBefore
        else foldl (\acc periodRate -> (1+acc)*(1+periodRate)-1) 0 (map fst subPeriods)
      (startYear, _, _) = toGregorian begin
      years = fromIntegral (diffDays end begin) / (if isLeapYear startYear then 366 else 365) :: Double
      periodTWR = roundTo 2 $ 100 * twr
      annualizedTWR = 100*((1+(realToFrac twr))**(1/years)-1) :: Double

  when showCashFlow $ do
    printf "\nTWR cash flow entries and subperiod rates for period %s - %s\n" (showDate begin) (showDate (addDays (-1) end))
    let showDecimalT = T.pack . showDecimal
    TL.putStr $ Tab.render prettyTables T.pack id id
      (Table
       (Tab.Group Tab.NoLine [ Header (show n) | n <-[1..length subPeriods]])
       (Tab.Group DoubleLine [ Tab.Group Tab.SingleLine [Tab.Header "Subperiod start", Tab.Header "Cashflow date"]
                             , Tab.Group Tab.SingleLine [Tab.Header "Value at start", Tab.Header "Value before cashflow (inc PnL)", Tab.Header "PnL on day", Tab.Header "Cashflow",  Tab.Header "Value after cashflow"]
                             , Tab.Group Tab.SingleLine [Tab.Header "Subperiod rate, %"]])
       [ [ showDate (twrStartDate sp), showDate (twrEndDate sp)
         , showDecimalT (twrStartValue sp), showDecimalT (twrValueBeforeCashflow sp), showDecimalT (twrPnl sp), showDecimalT (twrCashflow sp), showDecimalT (twrValueAfterCashflow sp)
         , showDecimalT (roundTo 2 (100*rate)) ]
       | (rate, sp) <-  subPeriods
       ])

    printf "Total period TWR: %s%%.\nPeriod: %.2f years.\nAnnualized TWR: %.2f%%\n\n"
      (showDecimal periodTWR) years annualizedTWR

  return ((realToFrac periodTWR) :: Double, annualizedTWR)

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
