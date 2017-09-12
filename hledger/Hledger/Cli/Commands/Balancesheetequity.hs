{-# LANGUAGE QuasiQuotes, RecordWildCards, NoCPP #-}
{-|

The @balancesheetequity@ command prints a simple balance sheet.

-}

module Hledger.Cli.Commands.Balancesheetequity (
  balancesheetequitymode
 ,balancesheetequity
) where

import Data.String.Here
import System.Console.CmdArgs.Explicit

import Hledger
import Hledger.Cli.CliOptions
import Hledger.Cli.CompoundBalanceCommand

balancesheetequitySpec = CompoundBalanceCommandSpec {
  cbcname     = "balancesheetequity",
  cbcaliases  = ["bse"],
  cbchelp     = [here|This command displays a simple balance sheet, showing historical ending
balances of asset, liability and equity accounts (ignoring any report begin date). 
It assumes that these accounts are under a top-level `asset`, `liability` and `equity`
account (plural forms also  allowed).
  |],
  cbctitle    = "Balance Sheet With Equity",
  cbcqueries  = [ ("Assets"     , journalAssetAccountQuery),
                ("Liabilities", journalLiabilityAccountQuery),
                ("Equity", journalEquityAccountQuery)
              ],
  cbctype     = HistoricalBalance
}

balancesheetequitymode :: Mode RawOpts
balancesheetequitymode = compoundBalanceCommandMode balancesheetequitySpec

balancesheetequity :: CliOpts -> Journal -> IO ()
balancesheetequity = compoundBalanceCommand balancesheetequitySpec
