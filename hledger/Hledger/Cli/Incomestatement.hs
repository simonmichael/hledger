{-# LANGUAGE QuasiQuotes, TemplateHaskell, OverloadedStrings, NoCPP #-}
{-|

The @incomestatement@ command prints a simple income statement (profit & loss) report.

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
import Hledger.Cli.BalanceView

isBV = BalanceView {
         bvmode     = "incomestatement",
         bvaliases  = ["is"],
         bvhelp     = [here|This command displays a simple income statement, showing revenues
and expenses during a period. It assumes that these accounts are under a 
top-level `revenue` or `income` or `expense` account (plural forms 
also allowed).

          |],
         bvtitle    = "Income Statement",
         bvqueries  = [ ("Revenues", journalIncomeAccountQuery),
                        ("Expenses", journalExpenseAccountQuery)
                      ],
         bvdeftype  = PeriodChange,
         bvtypes    = [CumulativeChange, HistoricalBalance]
      }

incomestatementmode :: Mode RawOpts
incomestatementmode = balanceviewmode isBV

incomestatement :: CliOpts -> Journal -> IO ()
incomestatement = balanceviewReport isBV

tests_Hledger_Cli_Incomestatement :: Test
tests_Hledger_Cli_Incomestatement = TestList
 [
 ]
