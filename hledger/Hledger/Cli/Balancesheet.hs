{-# LANGUAGE OverloadedStrings, QuasiQuotes, RecordWildCards, NoCPP #-}
{-|

The @balancesheet@ command prints a simple balance sheet.

-}

module Hledger.Cli.Balancesheet (
  balancesheetmode
 -- ,balancesheet
 ,tests_Hledger_Cli_Balancesheet
) where

import Data.String.Here
import System.Console.CmdArgs.Explicit
import Test.HUnit

import Hledger
import Hledger.Cli.CliOptions
import Hledger.Cli.CompoundBalanceCommand

balancesheetSpec = CompoundBalanceCommandSpec {
  cbcname     = "balancesheet",
  -- cbcaliases  = ["bs"],
  cbctitle    = Just "Balance Sheet",
  cbcqueries  = [ ("Assets"     , "^assets?(:|$)"),
                  ("Liabilities", "^(debts?|liabilit(y|ies))(:|$)")
                ],
  cbctype     = Just HistoricalBalance
}

-- balancesheetmode :: Mode RawOpts
-- balancesheetmode = compoundBalanceCommandMode balancesheetSpec [here|
-- This command displays a simple balance sheet, showing historical ending
-- balances of asset and liability accounts (ignoring any report begin date). 
-- It assumes that these accounts are under a top-level `asset` or `liability`
-- account (case insensitive, plural forms also  allowed).
-- |]

tests_Hledger_Cli_Balancesheet :: Test
tests_Hledger_Cli_Balancesheet = TestList
 [
 ]
