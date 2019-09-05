{-# LANGUAGE OverloadedStrings, RecordWildCards, DeriveDataTypeable, FlexibleInstances, ScopedTypeVariables #-}
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

import Data.List
import Data.Maybe
import Data.Ord

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
entriesReport :: ReportOpts -> Query -> Journal -> EntriesReport
entriesReport ropts@ReportOpts{..} q j@Journal{..} =
  sortBy (comparing getdate) $ filter (q `matchesTransaction`) $ map tvalue jtxns
  where
    getdate = transactionDateFn ropts
    -- We may be converting posting amounts to value, per hledger_options.m4.md "Effect of --value on reports".
    tvalue t@Transaction{..} = t{tpostings=map pvalue tpostings}
      where
        pvalue p = maybe p
          (postingApplyValuation (journalPriceOracle j) (journalCommodityStyles j) periodlast mreportlast today False p)
          value_
          where
            periodlast  = fromMaybe today $ reportPeriodOrJournalLastDay ropts j
            mreportlast = reportPeriodLastDay ropts
            today       = fromMaybe (error' "erValue: could not pick a valuation date, ReportOpts today_ is unset") today_  -- should not happen

tests_EntriesReport = tests "EntriesReport" [
  tests "entriesReport" [
     test "not acct" $ (length $ entriesReport defreportopts (Not $ Acct "bank") samplejournal) `is` 1
    ,test "date" $ (length $ entriesReport defreportopts (Date $ mkdatespan "2008/06/01" "2008/07/01") samplejournal) `is` 3
  ]
 ]

