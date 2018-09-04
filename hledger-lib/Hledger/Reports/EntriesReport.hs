{-# LANGUAGE OverloadedStrings, RecordWildCards, DeriveDataTypeable, FlexibleInstances #-}
{-|

Journal entries report, used by the print command.

-}

module Hledger.Reports.EntriesReport (
  EntriesReport,
  EntriesReportItem,
  entriesReport,
  -- * Tests
  easytests_EntriesReport
)
where

import Data.List
import Data.Ord

import Hledger.Data
import Hledger.Query
import Hledger.Reports.ReportOptions
import Hledger.Utils hiding (is)


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

is :: (Eq a, Show a, HasCallStack) => a -> a -> Test ()
is = flip expectEq'

easytests_EntriesReport = tests "EntriesReport" [
  tests "entriesReport" [
     test "not acct" $ (length $ entriesReport defreportopts (Not $ Acct "bank") samplejournal) `is` 1
    ,test "date" $ (length $ entriesReport defreportopts (Date $ mkdatespan "2008/06/01" "2008/07/01") samplejournal) `is` 3
  ]
 ]

