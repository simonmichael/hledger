{-# LANGUAGE QuasiQuotes, RecordWildCards, NoCPP #-}
{-|

The @cashflow@ command prints a simplified cashflow statement.  It just
shows the change in all "cash" accounts for the period (without the
traditional segmentation into operating, investing, and financing
cash flows.)

-}

module Hledger.Cli.Cashflow (
  cashflowmode
 ,cashflow
 ,tests_Hledger_Cli_Cashflow
) where

import qualified Data.Text.Lazy.IO as LT
import System.Console.CmdArgs.Explicit
import Test.HUnit
import Text.Shakespeare.Text

import Hledger
import Hledger.Cli.Options
import Hledger.Cli.Balance


cashflowmode :: Mode RawOpts
cashflowmode = (defCommandMode ["cashflow","cf"]) {
  modeHelp = "show a cashflow statement" `withAliases` ["cf"]
 ,modeGroupFlags = Group {
     groupUnnamed = [
      flagNone ["flat"] (\opts -> setboolopt "flat" opts) "show accounts as a list"
     ,flagReq  ["drop"] (\s opts -> Right $ setopt "drop" s opts) "N" "flat mode: omit N leading account name parts"
     ]
    ,groupHidden = []
    ,groupNamed = [generalflagsgroup1]
    }
 }

-- | Print a simple cashflow statement.
cashflow :: CliOpts -> Journal -> IO ()
cashflow CliOpts{reportopts_=ropts} j = do
  -- let lines = case formatFromOpts ropts of Left err, Right ...
  d <- getCurrentDay
  let q = queryFromOpts d ropts
      cashreport@(_,total) = balanceReport ropts (And [q, journalCashAccountQuery j]) j
      -- operatingreport@(_,operating) = balanceReport ropts (And [q, journalOperatingAccountMatcher j]) j
      -- investingreport@(_,investing) = balanceReport ropts (And [q, journalInvestingAccountMatcher j]) j
      -- financingreport@(_,financing) = balanceReport ropts (And [q, journalFinancingAccountMatcher j]) j
      -- total = operating + investing + financing
  LT.putStr $ [lt|Cashflow Statement

Cash flows:
#{unlines $ balanceReportAsText ropts cashreport}
Total:
--------------------
#{padleft 20 $ showMixedAmountWithoutPrice total}
|]

tests_Hledger_Cli_Cashflow :: Test
tests_Hledger_Cli_Cashflow = TestList
 [
 ]
