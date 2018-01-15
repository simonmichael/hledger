{-# LANGUAGE QuasiQuotes, RecordWildCards, NoCPP #-}
{-|

The @balancesheet@ command prints a simple balance sheet.

-}

module Hledger.Cli.Commands.Balancesheet (
  balancesheetmode
 ,balancesheet
 ,tests_Hledger_Cli_Commands_Balancesheet
) where

import Data.String.Here
import System.Console.CmdArgs.Explicit
import Test.HUnit

import Hledger
import Hledger.Cli.CliOptions
import Hledger.Cli.CompoundBalanceCommand

balancesheetSpec = CompoundBalanceCommandSpec {
  cbcname     = "balancesheet",
  cbcaliases  = ["bs"],
  cbchelp     = [here|
This command displays a simple balance sheet, showing historical ending
balances of asset and liability accounts (ignoring any report begin date). 
It assumes that these accounts are under a top-level `asset` or `liability`
account (case insensitive, plural forms also  allowed).
  |],
  cbctitle    = "Balance Sheet",
  cbcqueries  = [ ("Assets"     , journalAssetAccountQuery,     Just NormallyPositive),
                  ("Liabilities", journalLiabilityAccountQuery, Just NormallyNegative)
                ],
  cbctype     = HistoricalBalance
}

balancesheetmode :: Mode RawOpts
balancesheetmode = compoundBalanceCommandMode balancesheetSpec

balancesheet :: CliOpts -> Journal -> IO ()
balancesheet = compoundBalanceCommand balancesheetSpec

tests_Hledger_Cli_Commands_Balancesheet :: Test
tests_Hledger_Cli_Commands_Balancesheet = TestList
 [
 ]
