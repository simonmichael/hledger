{-# LANGUAGE QuasiQuotes, TemplateHaskell, OverloadedStrings, NoCPP #-}
{-|

The @incomestatement@ command prints a simple income statement (profit & loss report).

-}

module Hledger.Cli.Commands.Incomestatement (
  incomestatementmode
 ,incomestatement
 ,tests_Hledger_Cli_Commands_Incomestatement
) where

import Data.String.Here
import System.Console.CmdArgs.Explicit
import Test.HUnit

import Hledger
import Hledger.Cli.CliOptions
import Hledger.Cli.CompoundBalanceCommand

incomestatementSpec = CompoundBalanceCommandSpec {
  cbcname     = "incomestatement",
  cbcaliases  = ["is"],
  cbchelp     = [here|
This command displays a simple income statement, showing revenues
and expenses during a period. It assumes that these accounts are under a 
top-level `revenue` or `income` or `expense` account (case insensitive,
plural forms also allowed).
  |],
  cbctitle    = "Income Statement",
  cbcqueries  = [ ("Revenues", journalIncomeAccountQuery, Just NormalNegative),
                  ("Expenses", journalExpenseAccountQuery, Just NormalPositive)
                ],
  cbctype     = PeriodChange
}

incomestatementmode :: Mode RawOpts
incomestatementmode = compoundBalanceCommandMode incomestatementSpec

incomestatement :: CliOpts -> Journal -> IO ()
incomestatement = compoundBalanceCommand incomestatementSpec

tests_Hledger_Cli_Commands_Incomestatement :: Test
tests_Hledger_Cli_Commands_Incomestatement = TestList
 [
 ]
