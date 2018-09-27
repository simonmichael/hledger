{-# LANGUAGE QuasiQuotes, TemplateHaskell, OverloadedStrings #-}
{-|

The @incomestatement@ command prints a simple income statement (profit & loss report).

-}

module Hledger.Cli.Commands.Incomestatement (
  incomestatementmode
 ,incomestatement
) where

import Data.String.Here
import System.Console.CmdArgs.Explicit

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

Note this report shows all account balances with normal positive sign
(like conventional financial statements, unlike balance/print/register)
(experimental).
  |],
  cbctitle    = "Income Statement",
  cbcqueries  = [
     CBCSubreportSpec{
      cbcsubreporttitle="Revenues"
     ,cbcsubreportquery=journalRevenueAccountQuery
     ,cbcsubreportnormalsign=NormallyNegative
     ,cbcsubreportincreasestotal=True
     }
    ,CBCSubreportSpec{
      cbcsubreporttitle="Expenses"
     ,cbcsubreportquery=journalExpenseAccountQuery
     ,cbcsubreportnormalsign=NormallyPositive
     ,cbcsubreportincreasestotal=False
     }
    ],
  cbctype     = PeriodChange
}

incomestatementmode :: Mode RawOpts
incomestatementmode = compoundBalanceCommandMode incomestatementSpec

incomestatement :: CliOpts -> Journal -> IO ()
incomestatement = compoundBalanceCommand incomestatementSpec
