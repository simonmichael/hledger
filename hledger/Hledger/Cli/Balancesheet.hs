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
import Hledger.Cli.BalanceView

balancesheetBV = BalanceView {
         bvmode     = "balancesheet",
         bvaliases  = ["bs"],
         bvhelp     = [here|
This command displays a simple balance sheet, showing historical ending
balances of asset and liability accounts (ignoring any report begin date). 
It assumes that these accounts are under a top-level `asset` or `liability`
account (plural forms also  allowed).
          |],
         bvtitle    = "Balance Sheet",
         bvqueries  = [ ("Assets"     , journalAssetAccountQuery),
                        ("Liabilities", journalLiabilityAccountQuery)
                      ],
         bvtype     = HistoricalBalance
      }

balancesheetmode :: Mode RawOpts
balancesheetmode = balanceviewmode balancesheetBV

balancesheet :: CliOpts -> Journal -> IO ()
balancesheet = balanceviewReport balancesheetBV

tests_Hledger_Cli_Balancesheet :: Test
tests_Hledger_Cli_Balancesheet = TestList
 [
 ]
