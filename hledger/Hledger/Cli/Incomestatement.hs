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
  let incomereport@(_,income)    = accountsReport2 ropts (journalIncomeAccountQuery j) j
      expensereport@(_,expenses) = accountsReport2 ropts (journalExpenseAccountQuery j) j
      total = income + expenses
  LT.putStr $ [lt|Income Statement

Revenues:
#{unlines $ accountsReportAsText ropts incomereport}
Expenses:
#{unlines $ accountsReportAsText ropts expensereport}

Total: #{show total}
|]

tests_Hledger_Cli_Incomestatement :: Test
tests_Hledger_Cli_Incomestatement = TestList
 [
 ]
