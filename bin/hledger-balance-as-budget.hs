#!/usr/bin/env stack
{- stack runghc --verbosity info
   --package hledger-lib
   --package hledger
-}

{-# OPTIONS_GHC -Wno-missing-signatures -Wno-name-shadowing #-}

{-| Construct two balance reports for two different time periods and use one of the as "budget" for
    the other, thus comparing them
-}  
import System.Environment (getArgs)
import Hledger.Cli

------------------------------------------------------------------------------
cmdmode = hledgerCommandMode
  (unlines ["balance-as-budget"
           ,"Generate two balance reports and use first of them as budget for the second."
           ," "
           ,"Pass two sets of hledger-compatible options, separated by --."
           ,"For example, to use Jan 2019 as budget for Jan 2020, use:"
           ,"-f 2019.journal -p 2019-01 -- -f 2020.journal -p 2020-01"
           ," "
           ,"Display features in the report are driven by the second set of args"
           ])
  [] 
  [generalflagsgroup1]
  []
  ([], Just $ argsFlag "[QUERY]")
------------------------------------------------------------------------------

main :: IO ()
main = do
  args <- getArgs
  let report1args = takeWhile (/= "--") args
  let report2args = drop 1 $ dropWhile (/= "--") args
  (_,report1) <- mbReport report1args
  (ropts2,report2) <- mbReport report2args
  let pastAsBudget = combineBudgetAndActual report1{prDates=prDates report2} report2 
  putStrLn $ budgetReportAsText ropts2 pastAsBudget
  where
    mbReport args = do
      opts@CliOpts{reportopts_=ropts} <- getHledgerCliOpts' cmdmode args
      d <- getCurrentDay
      report <- withJournalDo opts (return . multiBalanceReport d ropts)
      return (ropts,report)
