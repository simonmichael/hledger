{-# LANGUAGE QuasiQuotes, RecordWildCards, NoCPP #-}
{-|

The @balancesheet@ command prints a simple balance sheet.

-}

module Hledger.Cli.Balancesheet (
  balancesheetmode
 ,balancesheet
 ,tests_Hledger_Cli_Balancesheet
) where

import qualified Data.Text.Lazy.IO as LT
import System.Console.CmdArgs.Explicit
import Test.HUnit
import Text.Shakespeare.Text

import Hledger
import Hledger.Cli.CliOptions
import Hledger.Cli.Balance
import Hledger.Cli.BalanceView


balancesheetmode :: Mode RawOpts
balancesheetmode = (defCommandMode $ ["balancesheet"]++aliases) {
  modeHelp = "show a balance sheet" `withAliases` aliases
 ,modeGroupFlags = Group {
     groupUnnamed = [
      flagNone ["flat"] (\opts -> setboolopt "flat" opts) "show accounts as a list"
     ,flagReq  ["drop"] (\s opts -> Right $ setopt "drop" s opts) "N" "flat mode: omit N leading account name parts"
     ]
    ,groupHidden = []
    ,groupNamed = [generalflagsgroup1]
    }
 }
  where aliases = ["bs"]

balancesheet :: CliOpts -> Journal -> IO ()
balancesheet = balanceviewReport bv
  where
    bv = BV "Balance Sheet"
            [ ("Assets", journalAssetAccountQuery)
            , ("Liabilities", journalLiabilityAccountQuery)
            ]

tests_Hledger_Cli_Balancesheet :: Test
tests_Hledger_Cli_Balancesheet = TestList
 [
 ]
