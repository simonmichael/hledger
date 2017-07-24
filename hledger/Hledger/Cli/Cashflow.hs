{-# LANGUAGE QuasiQuotes, RecordWildCards, NoCPP #-}
{-|

The @cashflow@ command prints a simplified cashflow statement.  It just
shows the change in all "cash" accounts for the period (without the
traditional segmentation into operating, investing, and financing
cash flows.)

-}

module Hledger.Cli.Cashflow (
  cashflowmode
 ,cashflow
 ,tests_Hledger_Cli_Cashflow
) where

import Data.String.Here
import System.Console.CmdArgs.Explicit
import Test.HUnit

import Hledger
import Hledger.Cli.CliOptions
import Hledger.Cli.BalanceView

cashflowBV = BalanceView {
         bvmode     = "cashflow",
         bvaliases  = ["cf"],
         bvhelp     = [here|
This command displays a simple cashflow statement, showing changes
in "cash" accounts. It assumes that these accounts are under a top-level 
`asset` account (case insensitive, plural forms also allowed) and do not 
contain `receivable` or `A/R` in their name. 
          |],
         bvtitle    = "Cashflow Statement",
         bvqueries  = [("Cash flows", journalCashAccountQuery)],
         bvtype     = PeriodChange
      }

cashflowmode :: Mode RawOpts
cashflowmode = balanceviewmode cashflowBV

cashflow :: CliOpts -> Journal -> IO ()
cashflow = balanceviewReport cashflowBV

tests_Hledger_Cli_Cashflow :: Test
tests_Hledger_Cli_Cashflow = TestList
 [
 ]
