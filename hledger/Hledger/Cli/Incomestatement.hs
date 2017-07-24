{-# LANGUAGE QuasiQuotes, TemplateHaskell, OverloadedStrings, NoCPP #-}
{-|

The @incomestatement@ command prints a simple income statement (profit & loss report).

-}

module Hledger.Cli.Incomestatement (
  incomestatementmode
 ,incomestatement
 ,tests_Hledger_Cli_Incomestatement
) where

import Data.String.Here
import System.Console.CmdArgs.Explicit
import Test.HUnit

import Hledger
import Hledger.Cli.CliOptions
import Hledger.Cli.BalanceCommand

incomestatementSpec = BalanceCommandSpec {
  bcname     = "incomestatement",
  bcaliases  = ["is"],
  bchelp     = [here|
This command displays a simple income statement, showing revenues
and expenses during a period. It assumes that these accounts are under a 
top-level `revenue` or `income` or `expense` account (case insensitive,
plural forms also allowed).
  |],
  bctitle    = "Income Statement",
  bcqueries  = [ ("Revenues", journalIncomeAccountQuery),
                 ("Expenses", journalExpenseAccountQuery)
               ],
  bctype     = PeriodChange
}

incomestatementmode :: Mode RawOpts
incomestatementmode = balanceCommandMode incomestatementSpec

incomestatement :: CliOpts -> Journal -> IO ()
incomestatement = balanceCommand incomestatementSpec

tests_Hledger_Cli_Incomestatement :: Test
tests_Hledger_Cli_Incomestatement = TestList
 [
 ]
