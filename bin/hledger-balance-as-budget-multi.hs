#!/usr/bin/env stack
-- stack runghc --verbosity info --package hledger
-- Run from inside the hledger source tree, or compile with compile.sh.
-- See hledger-check-fancyassertions.hs.

-- {-# OPTIONS_GHC -Wno-missing-signatures #-}

{-| Construct two balance reports for two different time periods and use one of the as "budget" for
    the other, thus comparing them
-}
import Data.Text.Lazy.IO qualified as TL
import System.Environment (getArgs)
import Hledger.Cli.Script
import Hledger.Cli.Commands.Balance

------------------------------------------------------------------------------
cmdmode = hledgerCommandMode
  (unlines ["balance-as-budget-multi"
           ,"Read two journal files and generate multiple balance reports that use first of them as budget for the second."
           ," "
           ,"Pass two journal names and a file that contains sets of 'hledger balance'-compatible options, one per line"
           ,"For example, to use Jan 2019 as budget for Jan 2020, use:"
           ,"2019.journal 2020.journal commands.txt"
           ,"and put '\"assets\" --depth 3 --value=$,then' in the commands.txt"
           ])
  [] 
  [generalflagsgroup1]
  []
  ([], Just $ argsFlag "BUDGET_JOURNAL ACTUAL_JOURNAL COMMAND_FILE")
------------------------------------------------------------------------------

main :: IO ()
main = do
  args <- getArgs
  case args of
    [budget_f, real_f, commands_f] -> runAllCommands budget_f real_f commands_f
    _ -> error' "expected exactly three arguments"

runAllCommands :: String -> String -> String -> IO ()
runAllCommands budget_f real_f commands_f = do
  d <- getCurrentDay
  budget <- readJournalFile' budget_f
  real <- readJournalFile' real_f
  let styles = journalCommodityStylesWith HardRounding real
  commands <- lines <$> readFile commands_f
  forM_ commands $ \command -> do
    let args = words' command
    case args of
      [] -> return ()
      "echo":args -> putStrLn $ unwords args
      _ -> do
        opts@CliOpts{reportspec_=rspec} <- getHledgerCliOpts' balancemode args 
        let b = multiBalanceReport rspec budget
        let r = multiBalanceReport rspec real
        let reportopts = _rsReportOpts rspec
        let combined = combineBudgetAndActual reportopts real b{prDates=prDates r} r
        writeOutputLazyText opts $ budgetReportAsText reportopts $ styleAmounts styles $ combined
