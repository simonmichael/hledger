{-# LANGUAGE QuasiQuotes, TemplateHaskell, OverloadedStrings #-}
{-|

The @incomestatement@ command prints a simple income statement (profit & loss report).

-}

module Hledger.Cli.Commands.Incomestatement (
  incomestatementmode
 ,incomestatement
) where

import System.Console.CmdArgs.Explicit

import Hledger
import Hledger.Cli.CliOptions
import Hledger.Cli.CompoundBalanceCommand

incomestatementSpec = CompoundBalanceCommandSpec {
  cbcdoc      = $(embedFileRelative "Hledger/Cli/Commands/Incomestatement.txt"),
  cbctitle    = "Income Statement",
  cbcqueries  = [
     CBCSubreportSpec{
      cbcsubreporttitle="Revenues"
     ,cbcsubreportquery=journalRevenueAccountQuery
     ,cbcsubreportoptions=(\ropts -> ropts{normalbalance_=Just NormallyNegative})
     ,cbcsubreporttransform=fmap maNegate
     ,cbcsubreportincreasestotal=True
     }
    ,CBCSubreportSpec{
      cbcsubreporttitle="Expenses"
     ,cbcsubreportquery=journalExpenseAccountQuery
     ,cbcsubreportoptions=(\ropts -> ropts{normalbalance_=Just NormallyPositive})
     ,cbcsubreporttransform=id
     ,cbcsubreportincreasestotal=False
     }
    ],
  cbctype     = PeriodChange
}

incomestatementmode :: Mode RawOpts
incomestatementmode = compoundBalanceCommandMode incomestatementSpec

incomestatement :: CliOpts -> Journal -> IO ()
incomestatement = compoundBalanceCommand incomestatementSpec
