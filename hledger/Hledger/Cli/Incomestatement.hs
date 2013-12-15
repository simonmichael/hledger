{-# LANGUAGE QuasiQuotes, TemplateHaskell, OverloadedStrings, NoCPP #-}
{-|

The @incomestatement@ command prints a simple income statement (profit & loss) report.

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

-- | Print a simple income statement.
incomestatement :: CliOpts -> Journal -> IO ()
incomestatement CliOpts{reportopts_=ropts} j = do
  d <- getCurrentDay
  let q = queryFromOpts d ropts
      incomereport@(_,income)    = balanceReport ropts (And [q, journalIncomeAccountQuery j]) j
      expensereport@(_,expenses) = balanceReport ropts (And [q, journalExpenseAccountQuery j]) j
      total = income + expenses
  LT.putStr $ [lt|Income Statement

Revenues:
#{unlines $ balanceReportAsText ropts incomereport}
Expenses:
#{unlines $ balanceReportAsText ropts expensereport}

Total:
--------------------
#{padleft 20 $ showMixedAmountWithoutPrice total}
|]

tests_Hledger_Cli_Incomestatement :: Test
tests_Hledger_Cli_Incomestatement = TestList
 [
 ]
