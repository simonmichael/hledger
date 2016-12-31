{-# LANGUAGE QuasiQuotes, TemplateHaskell, OverloadedStrings, NoCPP #-}
{-|

The @incomestatement@ command prints a simple income statement (profit & loss) report.

-}

module Hledger.Cli.Incomestatement (
  incomestatementmode
 ,incomestatement
 ,tests_Hledger_Cli_Incomestatement
) where

import qualified Data.Text.Lazy.IO as LT
import System.Console.CmdArgs.Explicit
import Test.HUnit
import Text.Shakespeare.Text

import Hledger
import Hledger.Cli.CliOptions
import Hledger.Cli.Balance


incomestatementmode :: Mode RawOpts
incomestatementmode = (defCommandMode $ ["incomestatement"]++aliases) {
  modeHelp = "show an income statement" `withAliases` aliases
 ,modeGroupFlags = Group {
     groupUnnamed = [
      flagNone ["flat"] (\opts -> setboolopt "flat" opts) "show accounts as a list"
     ,flagReq  ["drop"] (\s opts -> Right $ setopt "drop" s opts) "N" "flat mode: omit N leading account name parts"
     ]
    ,groupHidden = []
    ,groupNamed = [generalflagsgroup1]
    }
 }
  where aliases = ["is"]

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
#{balanceReportAsText ropts incomereport}
Expenses:
#{balanceReportAsText ropts expensereport}
Total:
--------------------
#{padleft 20 $ showMixedAmountWithoutPrice total}
|]

tests_Hledger_Cli_Incomestatement :: Test
tests_Hledger_Cli_Incomestatement = TestList
 [
 ]
