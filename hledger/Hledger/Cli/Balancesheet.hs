{-# LANGUAGE QuasiQuotes, RecordWildCards #-}
{-|

The @balancesheet@ command prints a simple balance sheet.

-}

module Hledger.Cli.Balancesheet (
  balancesheet
 ,tests_Hledger_Cli_Balancesheet
) where

import qualified Data.Text.Lazy.IO as LT
import Test.HUnit
import Text.Shakespeare.Text

import Hledger
import Prelude hiding (putStr)
import Hledger.Utils.UTF8IOCompat (putStr)
import Hledger.Cli.Options
import Hledger.Cli.Balance


-- | Print a simple balance sheet.
balancesheet :: CliOpts -> Journal -> IO ()
balancesheet CliOpts{reportopts_=ropts} j = do
  -- let lines = case formatFromOpts ropts of Left err, Right ...
  d <- getCurrentDay
  let q = queryFromOpts d (withoutBeginDate ropts)
      assetreport@(_,assets)          = accountsReport ropts (And [q, journalAssetAccountQuery j]) j
      liabilityreport@(_,liabilities) = accountsReport ropts (And [q, journalLiabilityAccountQuery j]) j
      equityreport@(_,equity)         = accountsReport ropts (And [q, journalEquityAccountQuery j]) j
      total = assets + liabilities + equity
  LT.putStr $ [lt|Balance Sheet

Assets:
#{unlines $ accountsReportAsText ropts assetreport}
Liabilities:
#{unlines $ accountsReportAsText ropts liabilityreport}
Equity:
#{unlines $ accountsReportAsText ropts equityreport}

Total:
--------------------
#{padleft 20 $ showMixedAmountWithoutPrice total}
|]

withoutBeginDate :: ReportOpts -> ReportOpts
withoutBeginDate ropts@ReportOpts{..} = ropts{begin_=Nothing, period_=p}
  where p = case period_ of Nothing -> Nothing
                            Just (i, DateSpan _ e) -> Just (i, DateSpan Nothing e)

tests_Hledger_Cli_Balancesheet = TestList
 [
 ]
