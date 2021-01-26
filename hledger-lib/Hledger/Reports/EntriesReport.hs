{-# LANGUAGE OverloadedStrings, RecordWildCards, FlexibleInstances, ScopedTypeVariables #-}
{-|

Journal entries report, used by the print command.

-}

module Hledger.Reports.EntriesReport (
  EntriesReport,
  EntriesReportItem,
  entriesReport,
  -- * Tests
  tests_EntriesReport
)
where

import Data.List (sortBy)
import Data.Maybe (fromMaybe)
import Data.Ord (comparing)
import Data.Time (fromGregorian)

import Hledger.Data
import Hledger.Query
import Hledger.Reports.ReportOptions
import Hledger.Utils


-- | A journal entries report is a list of whole transactions as
-- originally entered in the journal (mostly). This is used by eg
-- hledger's print command and hledger-web's journal entries view.
type EntriesReport = [EntriesReportItem]
type EntriesReportItem = Transaction

-- | Select transactions for an entries report.
entriesReport :: ReportSpec -> Journal -> EntriesReport
entriesReport rspec@ReportSpec{rsOpts=ropts@ReportOpts{..}} j@Journal{..} =
  sortBy (comparing getdate) $ filter (rsQuery rspec `matchesTransaction`) $ map tvalue jtxns
  where
    getdate = transactionDateFn ropts
    -- We may be converting posting amounts to value, per hledger_options.m4.md "Effect of --value on reports".
    tvalue t@Transaction{..} = t{tpostings=map pvalue tpostings}
      where
        pvalue = postingApplyCostValuation (journalPriceOracle infer_value_ j) (journalCommodityStyles j) periodlast (rsToday rspec) cost_ value_
          where periodlast  = fromMaybe (rsToday rspec) $ reportPeriodOrJournalLastDay rspec j

tests_EntriesReport = tests "EntriesReport" [
  tests "entriesReport" [
     test "not acct" $ (length $ entriesReport defreportspec{rsQuery=Not . Acct $ toRegex' "bank"} samplejournal) @?= 1
    ,test "date" $ (length $ entriesReport defreportspec{rsQuery=Date $ DateSpan (Just $ fromGregorian 2008 06 01) (Just $ fromGregorian 2008 07 01)} samplejournal) @?= 3
  ]
 ]

