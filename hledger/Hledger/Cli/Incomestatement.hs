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
  d <- getCurrentDay
  let m = queryFromOpts ropts d
      incomereport@(_,income)    = accountsReport2 ropts (And [m, journalIncomeAccountQuery j]) j
      expensereport@(_,expenses) = accountsReport2 ropts (And [m, journalExpenseAccountQuery j]) j
      total = income + expenses
  LT.putStr $ [lt|Income Statement

Revenues:
#{unlines $ accountsReportAsText ropts incomereport}
Expenses:
#{unlines $ accountsReportAsText ropts expensereport}

Total:
--------------------
#{padleft 20 $ showMixedAmountWithoutPrice total}
|]

tests_Hledger_Cli_Incomestatement :: Test
tests_Hledger_Cli_Incomestatement = TestList
 [
 ]
