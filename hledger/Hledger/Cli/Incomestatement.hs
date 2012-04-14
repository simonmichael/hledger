{-# LANGUAGE QuasiQuotes, TemplateHaskell, OverloadedStrings #-}
{-|

The @incomestatement@ command prints a fairly standard income statement (profit & loss) report.

-}

module Hledger.Cli.Incomestatement (
  incomestatement
 ,tests_Hledger_Cli_Incomestatement
) where

import qualified Data.Text.Lazy.IO as LT
import Test.HUnit
import Text.Shakespeare.Text

import Hledger
import Hledger.Cli.Options
import Hledger.Cli.Balance

-- | Print a standard income statement.
incomestatement :: CliOpts -> Journal -> IO ()
incomestatement CliOpts{reportopts_=ropts} j = do
  let report = accountsReport2 ropts (journalProfitAndLossAccountQuery j) j
  LT.putStr $ [lt|Income Statement

#{unlines $ accountsReportAsText ropts report}|]

tests_Hledger_Cli_Incomestatement :: Test
tests_Hledger_Cli_Incomestatement = TestList
 [
 ]
