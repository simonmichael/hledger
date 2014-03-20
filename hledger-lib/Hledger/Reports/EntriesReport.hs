{-# LANGUAGE RecordWildCards, DeriveDataTypeable, FlexibleInstances #-}
{-|

Journal entries report, used by the print command.

-}

module Hledger.Reports.EntriesReport (
  EntriesReport,
  EntriesReportItem,
  entriesReport,
  -- * Tests
  tests_Hledger_Reports_EntriesReport
)
where

import Data.List
import Data.Ord
import Test.HUnit

import Hledger.Data
import Hledger.Query
import Hledger.Reports.ReportOptions


-- | A journal entries report is a list of whole transactions as
-- originally entered in the journal (mostly). This is used by eg
-- hledger's print command and hledger-web's journal entries view.
type EntriesReport = [EntriesReportItem]
type EntriesReportItem = Transaction

-- | Select transactions for an entries report.
entriesReport :: ReportOpts -> Query -> Journal -> EntriesReport
entriesReport opts q j =
  sortBy (comparing date) $ filter (q `matchesTransaction`) ts
    where
      date = transactionDateFn opts
      ts = jtxns $ journalSelectingAmountFromOpts opts j

tests_entriesReport :: [Test]
tests_entriesReport = [
  "entriesReport" ~: do
    assertEqual "not acct" 1 (length $ entriesReport defreportopts (Not $ Acct "bank") samplejournal)
    let sp = mkdatespan "2008/06/01" "2008/07/01"
    assertEqual "date" 3 (length $ entriesReport defreportopts (Date sp) samplejournal)
 ]

tests_Hledger_Reports_EntriesReport :: Test
tests_Hledger_Reports_EntriesReport = TestList $
 tests_entriesReport

