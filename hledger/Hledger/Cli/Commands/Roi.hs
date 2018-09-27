{-# LANGUAGE QuasiQuotes,ParallelListComp #-}
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
import Data.Ord
import Statistics.Math.RootFinding
import Data.Decimal
import Data.String.Here
import System.Console.CmdArgs.Explicit as CmdArgs

import Text.Tabular as Tbl
import Text.Tabular.AsciiWide as Ascii

import Hledger
import Hledger.Cli.CliOptions


roimode = (defCommandMode $ ["roi"]) {
  modeHelp = "shows return on investment for your portfolio."
  ,modeHelpSuffix=lines [here|
This command will show you time-weighted (TWR) and money-weighted (IRR) rate of return on your investments.

Command assumes that you have account(s) that hold nothing but your investments and whenever you record current appraisal/valuation of these investments you offset unrealized profit and loss into account(s) that, again, hold nothing but unrealized profit and loss.

Any transactions affecting balance of investment account(s) and not originating from unrealized profit and loss account(s) are assumed to be your investments or withdrawals.

At a minimum, you need to supply query (which could be just an account name) to select your investments with `--inv`, and another query to identify your profit and loss transactions with `--pnl`.

Command will compute and display internalized rate of return (IRR) and time-weighted rate of return (TWR) for your investments for the time period requested. Both rates of return are annualized before display, regardless of the length of reporting interval.
|]
  ,modeGroupFlags = CmdArgs.Group {
    groupUnnamed = [
       flagNone ["cashflow"] (setboolopt "cashflow") "show all amounts that were used to compute returns"
       , flagReq ["investment"] (\s opts -> Right $ setopt "investment" s opts) "QUERY"
         "query to select your investment transactions"
       , flagReq ["profit-loss","pnl"] (\s opts -> Right $ setopt "pnl" s opts) "QUERY"
         "query to select profit-and-loss or appreciation/valuation transactions"
       ]
    , groupHidden = []
    ,groupNamed = [generalflagsgroup1]
    }
  }

-- One reporting span, 
data OneSpan = OneSpan 
  Day -- start date, inclusive
  Day   -- end date, exclusive
  Quantity -- value of investment at the beginning of day on spanBegin_
  Quantity  -- value of investment at the end of day on spanEnd_
  [(Day,Quantity)] -- all deposits and withdrawals (but not changes of value) in the DateSpan [spanBegin_,spanEnd_)
 deriving (Show)
  

roi ::  CliOpts -> Journal -> IO ()
roi CliOpts{rawopts_=rawopts, reportopts_=ropts} j = do
  d <- getCurrentDay
  let 
    investmentsQuery = queryFromOpts d $ ropts{query_ = stringopt "investment" rawopts,period_=PeriodAll}
    pnlQuery         = queryFromOpts d $ ropts{query_ = stringopt "pnl" rawopts,period_=PeriodAll}
    showCashFlow      = boolopt "cashflow" rawopts
    prettyTables     = pretty_tables_ ropts
    
    trans = dbg3 "investments" $ jtxns $ filterJournalTransactions investmentsQuery j
  
    journalSpan = 
        let dates = map transactionDate2 trans in 
        DateSpan (Just $ minimum dates) (Just $ addDays 1 $ maximum dates)            
        
    requestedSpan = periodAsDateSpan $ period_ ropts
    requestedInterval = interval_ ropts
    
    wholeSpan = spanDefaultsFrom requestedSpan journalSpan 

  when (null trans) $ do
    putStrLn "No relevant transactions found. Check your investments query"
    exitFailure
  
  let spans = case requestedInterval of
        NoInterval -> [wholeSpan]
        interval ->
            splitSpan interval $
            spanIntersect journalSpan wholeSpan

  tableBody <- (flip mapM) spans $ \(DateSpan (Just spanBegin) (Just spanEnd)) -> do
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
      
      thisSpan = dbg3 "processing span" $ 
                 OneSpan spanBegin spanEnd valueBefore valueAfter cashFlow
      
    irr <- internalRateOfReturn showCashFlow prettyTables thisSpan
    twr <- timeWeightedReturn showCashFlow prettyTables investmentsQuery trans thisSpan
    let cashFlowAmt = negate $ sum $ map snd cashFlow
    return [ showDate spanBegin
           , showDate (addDays (-1) spanEnd)
           , show valueBefore
           , show cashFlowAmt
           , show valueAfter
           , show (valueAfter - (valueBefore + cashFlowAmt))
           , printf "%0.2f%%" irr
           , printf "%0.2f%%" twr ]

  let table = Table 
              (Tbl.Group NoLine (map (Header . show) (take (length tableBody) [1..]))) 
              (Tbl.Group DoubleLine 
               [ Tbl.Group SingleLine [Header "Begin", Header "End"]
               , Tbl.Group SingleLine [Header "Value (begin)", Header "Cashflow", Header "Value (end)", Header "PnL"]
               , Tbl.Group SingleLine [Header "IRR", Header "TWR"]])
              tableBody
  
  putStrLn $ Ascii.render prettyTables id id id table

timeWeightedReturn showCashFlow prettyTables investmentsQuery trans (OneSpan spanBegin spanEnd valueBefore valueAfter cashFlow) = do
  let initialUnitPrice = 100
  let initialUnits = valueBefore / initialUnitPrice
  let cashflow = 
        -- Aggregate all entries for a single day, assuming that intraday interest is negligible
        map (\date_cash -> let (dates, cash) = unzip date_cash in (head dates, sum cash))
        $ groupBy ((==) `on` fst)
        $ sortBy (comparing fst) 
        $ map (\(d,a) -> (d, negate a)) 
        $ filter ((/=0).snd) cashFlow
    
  let units = 
        tail $
        (flip scanl) 
        (0,0,0,initialUnits)
        (\(_,_,_,unitBalance) (date, amt) -> 
          let valueOnDate = 
                total trans (And [investmentsQuery, Date (DateSpan Nothing (Just date))])
              unitPrice = if unitBalance == 0.0 then initialUnitPrice else valueOnDate / unitBalance
              unitsBoughtOrSold = amt / unitPrice
          in
           (valueOnDate, unitsBoughtOrSold, unitPrice, unitBalance + unitsBoughtOrSold)
        )  
        cashflow
  
  let finalUnitBalance = if null units then initialUnits else let (_,_,_,u) = last units in u
      finalUnitPrice = valueAfter / finalUnitBalance
      totalTWR = roundTo 2 $ (finalUnitPrice - initialUnitPrice)
      years = (fromIntegral $ diffDays spanEnd spanBegin)/365 :: Double
      annualizedTWR = 100*((1+(realToFrac totalTWR/100))**(1/years)-1) :: Double
        
  let s d = show $ roundTo 2 d 
  when showCashFlow $ do
    printf "\nTWR cash flow for %s - %s\n" (showDate spanBegin) (showDate (addDays (-1) spanEnd))
    let (dates', amounts') = unzip cashflow
        (valuesOnDate',unitsBoughtOrSold', unitPrices', unitBalances') = unzip4 units
        add x lst = if valueBefore/=0 then x:lst else lst
        dates = add spanBegin dates'
        amounts = add valueBefore amounts'
        unitsBoughtOrSold = add initialUnits unitsBoughtOrSold'
        unitPrices = add initialUnitPrice unitPrices'
        unitBalances = add initialUnits unitBalances'
        valuesOnDate = add 0 valuesOnDate'
        
    putStr $ Ascii.render prettyTables id id id 
      (Table 
       (Tbl.Group NoLine (map (Header . showDate) dates))
       (Tbl.Group DoubleLine [ Tbl.Group SingleLine [Header "Portfolio value", Header "Unit balance"] 
                         , Tbl.Group SingleLine [Header "Cash", Header "Unit price", Header "Units"]
                         , Tbl.Group SingleLine [Header "New Unit Balance"]])
       [ [value, oldBalance, amt, prc, udelta, balance] 
       | value <- map s valuesOnDate
       | oldBalance <- map s (0:unitBalances)
       | balance <- map s unitBalances
       | amt <- map s amounts
       | prc <- map s unitPrices
       | udelta <- map s unitsBoughtOrSold ])
  
    printf "Final unit price: %s/%s=%s U.\nTotal TWR: %s%%.\nPeriod: %.2f years.\nAnnualized TWR: %.2f%%\n\n" (s valueAfter) (s finalUnitBalance) (s finalUnitPrice) (s totalTWR) years annualizedTWR
  
  return annualizedTWR
  

internalRateOfReturn showCashFlow prettyTables (OneSpan spanBegin spanEnd valueBefore valueAfter cashFlow) = do 
  let prefix = (spanBegin, negate valueBefore)

      postfix = (spanEnd, valueAfter)

      totalCF = filter ((/=0) . snd) $ prefix : (sortBy (comparing fst) cashFlow) ++ [postfix]

  when showCashFlow $ do
    printf "\nIRR cash flow for %s - %s\n" (showDate spanBegin) (showDate (addDays (-1) spanEnd)) 
    let (dates, amounts) = unzip totalCF
    putStrLn $ Ascii.render prettyTables id id id 
      (Table 
       (Tbl.Group NoLine (map (Header . showDate) dates))
       (Tbl.Group SingleLine [Header "Amount"])
       (map ((:[]) . show) amounts))
                             
  -- 0% is always a solution, so require at least something here
  case ridders 0.00001 (0.000000000001,10000) (interestSum spanEnd totalCF) of
    Root rate -> return ((rate-1)*100)
    NotBracketed -> error "Error: No solution -- not bracketed."
    SearchFailed -> error "Error: Failed to find solution."

type CashFlow = [(Day, Quantity)]

interestSum :: Day -> CashFlow -> Double -> Double
interestSum referenceDay cf rate = sum $ map go cf
    where go (t,m) = (fromRational $ toRational m) * (rate ** (fromIntegral (referenceDay `diffDays` t) / 365))


calculateCashFlow :: [Transaction] -> Query -> CashFlow
calculateCashFlow trans query = map go trans
    where
    go t = (transactionDate2 t, total [t] query)

total :: [Transaction] -> Query -> Quantity
total trans query = unMix $ sumPostings $ filter (matchesPosting query) $ concatMap realPostings trans
    
unMix :: MixedAmount -> Quantity   
unMix a = 
  case (normaliseMixedAmount $ costOfMixedAmount a) of
    (Mixed [a]) -> aquantity a
    _ -> error "MixedAmount failed to normalize"

