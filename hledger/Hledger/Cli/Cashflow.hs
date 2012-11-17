{-# LANGUAGE QuasiQuotes, RecordWildCards, NoCPP #-}
{-|

The @cashflow@ command prints a simplified cashflow statement.  It just
shows the change in all "cash" accounts for the period (without the
traditional segmentation into operating, investing, and financing
cash flows.)

-}

module Hledger.Cli.Cashflow (
  cashflow
 ,tests_Hledger_Cli_Cashflow
) where

import qualified Data.Text.Lazy.IO as LT
import Test.HUnit
import Text.Shakespeare.Text

import Hledger
import Hledger.Cli.Options
import Hledger.Cli.Balance


-- | Print a simple cashflow statement.
cashflow :: CliOpts -> Journal -> IO ()
cashflow CliOpts{reportopts_=ropts} j = do
  -- let lines = case formatFromOpts ropts of Left err, Right ...
  d <- getCurrentDay
  let q = queryFromOpts d (withoutBeginDate ropts)
      cashreport@(_,total) = accountsReport ropts (And [q, journalCashAccountQuery j]) j
      -- operatingreport@(_,operating) = accountsReport ropts (And [q, journalOperatingAccountMatcher j]) j
      -- investingreport@(_,investing) = accountsReport ropts (And [q, journalInvestingAccountMatcher j]) j
      -- financingreport@(_,financing) = accountsReport ropts (And [q, journalFinancingAccountMatcher j]) j
      -- total = operating + investing + financing
  LT.putStr $ [lt|Cashflow Statement

Cash flows:
#{unlines $ accountsReportAsText ropts cashreport}

Total:
--------------------
#{padleft 20 $ showMixedAmountWithoutPrice total}
|]

withoutBeginDate :: ReportOpts -> ReportOpts
withoutBeginDate ropts@ReportOpts{..} = ropts{begin_=Nothing, period_=p}
  where p = case period_ of Nothing -> Nothing
                            Just (i, DateSpan _ e) -> Just (i, DateSpan Nothing e)

tests_Hledger_Cli_Cashflow = TestList
 [
 ]
