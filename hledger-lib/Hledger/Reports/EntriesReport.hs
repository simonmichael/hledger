{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
import Data.Ord (comparing)
import Data.Time (fromGregorian)

import Hledger.Data
import Hledger.Query (Query(..), filterQuery, queryIsDepth)
import Hledger.Reports.ReportOptions
import Hledger.Utils


-- | A journal entries report is a list of whole transactions as
-- originally entered in the journal (mostly). This is used by eg
-- hledger's print command and hledger-web's journal entries view.
type EntriesReport = [EntriesReportItem]
type EntriesReportItem = Transaction

-- | Select transactions for an entries report.
entriesReport :: ReportSpec -> Journal -> EntriesReport
entriesReport rspec@ReportSpec{_rsReportOpts=ropts} =
      sortBy (comparing $ transactionDateFn ropts)
    . map  (if invert_ ropts then transactionNegate else id)
    . jtxns
    -- #2371: filter after valuation not before - even though it's safe to do here - for consistency with postingsReport
    . filterJournalTransactions (filterQuery (not.queryIsDepth) $ _rsQuery rspec)
    . journalApplyValuationFromOpts (setDefaultConversionOp NoConversionOp rspec)

tests_EntriesReport = testGroup "EntriesReport" [
  testGroup "entriesReport" [
     testCase "not acct" $ (length $ entriesReport defreportspec{_rsQuery=Not . Acct $ toRegex' "bank"} samplejournal) @?= 1
    ,testCase "date" $ (length $ entriesReport defreportspec{_rsQuery=Date $ DateSpan (Just $ Exact $ fromGregorian 2008 06 01) (Just $ Exact $ fromGregorian 2008 07 01)} samplejournal) @?= 3
  ]
 ]

