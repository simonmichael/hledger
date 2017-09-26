{-# LANGUAGE QuasiQuotes, RecordWildCards, NoCPP #-}
{-|

The @cashflow@ command prints a simplified cashflow statement.  It just
shows the change in all "cash" accounts for the period (without the
traditional segmentation into operating, investing, and financing
cash flows.)

-}

module Hledger.Cli.Commands.Cashflow (
  cashflowmode
 ,cashflow
 ,tests_Hledger_Cli_Commands_Cashflow
) where

import Data.String.Here
import System.Console.CmdArgs.Explicit
import Test.HUnit

import Hledger
import Hledger.Cli.CliOptions
import Hledger.Cli.CompoundBalanceCommand

cashflowSpec = CompoundBalanceCommandSpec {
  cbcname     = "cashflow",
  cbcaliases  = ["cf"],
  cbchelp     = [here|
This command displays a simple cashflow statement, showing changes
in "cash" accounts. It assumes that these accounts are under a top-level 
`asset` account (case insensitive, plural forms also allowed) and do not 
contain `receivable` or `A/R` in their name. 
  |],
  cbctitle    = "Cashflow Statement",
  cbcqueries  = [("Cash flows", journalCashAccountQuery, Just NormalPositive)],
  cbctype     = PeriodChange
}

cashflowmode :: Mode RawOpts
cashflowmode = compoundBalanceCommandMode cashflowSpec

cashflow :: CliOpts -> Journal -> IO ()
cashflow = compoundBalanceCommand cashflowSpec

tests_Hledger_Cli_Commands_Cashflow :: Test
tests_Hledger_Cli_Commands_Cashflow = TestList
 [
 ]
