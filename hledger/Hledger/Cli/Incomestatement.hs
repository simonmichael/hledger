{-# LANGUAGE QuasiQuotes, TemplateHaskell, OverloadedStrings, NoCPP #-}
{-|

The @incomestatement@ command prints a simple income statement (profit & loss) report.

-}

module Hledger.Cli.Incomestatement (
  incomestatementmode
 ,incomestatement
 ,tests_Hledger_Cli_Incomestatement
) where

import Data.Maybe (fromMaybe)
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
     ,flagNone ["value","V"] (setboolopt "value") "show amounts as their current market value in their default valuation commodity"
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
      valuedate = fromMaybe d $ queryEndDate False $ queryFromOpts d ropts
      incomereport@(_,income)    = balanceReport ropts (And [q, journalIncomeAccountQuery j]) j
      expensereport@(_,expenses) = balanceReport ropts (And [q, journalExpenseAccountQuery j]) j
      total = income + expenses
      convertReport | value_ ropts = balanceReportValue j valuedate
                    | otherwise    = id
      convertTotal  | value_ ropts = mixedAmountValue j valuedate
                    | otherwise    = id
  LT.putStr $ [lt|Income Statement

Revenues:
#{balanceReportAsText ropts (convertReport incomereport)}
Expenses:
#{balanceReportAsText ropts (convertReport expensereport)}
Total:
--------------------
#{padleft 20 $ showMixedAmountWithoutPrice (convertTotal total)}
|]

tests_Hledger_Cli_Incomestatement :: Test
tests_Hledger_Cli_Incomestatement = TestList
 [
 ]
