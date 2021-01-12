#!/usr/bin/env stack
-- stack runghc --verbosity info --package hledger
-- Run from inside the hledger source tree, or compile with compile.sh.
-- See hledger-check-fancyassertions.hs.

-- {-# OPTIONS_GHC -Wno-missing-signatures -Wno-name-shadowing #-}

{-| Construct two balance reports for two different time periods and use one of the as "budget" for
    the other, thus comparing them
-}
import Data.Text.Lazy.IO as TL
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
  (_,_,report1) <- mbReport report1args
  (ropts2,j,report2) <- mbReport report2args
  let pastAsBudget = combineBudgetAndActual ropts2 j report1{prDates=prDates report2} report2
  TL.putStrLn $ budgetReportAsText ropts2 pastAsBudget
  where
    mbReport args = do
      opts@CliOpts{reportspec_=rspec} <- getHledgerCliOpts' cmdmode args
      d <- getCurrentDay
      (report,j) <- withJournalDo opts $ \j -> return (multiBalanceReport rspec j, j)
      return (rsOpts rspec,j,report)
