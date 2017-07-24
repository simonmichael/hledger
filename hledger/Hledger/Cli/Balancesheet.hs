{-# LANGUAGE QuasiQuotes, RecordWildCards, NoCPP #-}
{-|

The @balancesheet@ command prints a simple balance sheet.

-}

module Hledger.Cli.Balancesheet (
  balancesheetmode
 ,balancesheet
 ,tests_Hledger_Cli_Balancesheet
) where

import Data.String.Here
import System.Console.CmdArgs.Explicit
import Test.HUnit

import Hledger
import Hledger.Cli.CliOptions
import Hledger.Cli.BalanceCommand

balancesheetSpec = BalanceCommandSpec {
  bcname     = "balancesheet",
  bcaliases  = ["bs"],
  bchelp     = [here|
This command displays a simple balance sheet, showing historical ending
balances of asset and liability accounts (ignoring any report begin date). 
It assumes that these accounts are under a top-level `asset` or `liability`
account (case insensitive, plural forms also  allowed).
  |],
  bctitle    = "Balance Sheet",
  bcqueries  = [ ("Assets"     , journalAssetAccountQuery),
                 ("Liabilities", journalLiabilityAccountQuery)
               ],
  bctype     = HistoricalBalance
}

balancesheetmode :: Mode RawOpts
balancesheetmode = balanceCommandMode balancesheetSpec

balancesheet :: CliOpts -> Journal -> IO ()
balancesheet = balanceCommand balancesheetSpec

tests_Hledger_Cli_Balancesheet :: Test
tests_Hledger_Cli_Balancesheet = TestList
 [
 ]
