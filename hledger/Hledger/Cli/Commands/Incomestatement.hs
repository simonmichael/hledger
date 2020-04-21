{-# LANGUAGE QuasiQuotes, TemplateHaskell, OverloadedStrings #-}
{-|

The @incomestatement@ command prints a simple income statement (profit & loss report).

-}

module Hledger.Cli.Commands.Incomestatement (
  incomestatementmode
 ,incomestatement
) where

import System.Console.CmdArgs.Explicit
import Data.Text as T

import Hledger
import Hledger.Cli.CliOptions
import Hledger.Cli.CompoundBalanceCommand

incomestatementSpec j = CompoundBalanceCommandSpec {
  cbcdoc      = $(embedFileRelative "Hledger/Cli/Commands/Incomestatement.txt"),
  cbctitle    = "Income Statement",
  cbcqueries  = [
     CBCSubreportSpec{
      cbcsubreporttitle=T.unpack $ getAccountTypeTitle Revenue "Revenues" j
     ,cbcsubreportquery=journalRevenueAccountQuery
     ,cbcsubreportnormalsign=NormallyNegative
     ,cbcsubreportincreasestotal=True
     }
    ,CBCSubreportSpec{
      cbcsubreporttitle=T.unpack $ getAccountTypeTitle Expense "Expenses" j
     ,cbcsubreportquery=journalExpenseAccountQuery
     ,cbcsubreportnormalsign=NormallyPositive
     ,cbcsubreportincreasestotal=False
     }
    ],
  cbctype     = PeriodChange
}

incomestatementmode :: Mode RawOpts
incomestatementmode = compoundBalanceCommandMode $ incomestatementSpec nulljournal

incomestatement :: CliOpts -> Journal -> IO ()
incomestatement opts j = compoundBalanceCommand (incomestatementSpec j) opts j
