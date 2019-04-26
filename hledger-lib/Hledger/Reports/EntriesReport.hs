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

import Control.Applicative ((<|>))
import Data.List
import Data.Maybe
import Data.Ord
import Data.Time.Calendar (Day, addDays)

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
entriesReport opts q j =
  (if value_ opts then erValue opts j else id) $
  sortBy (comparing date) $ filter (q `matchesTransaction`) ts
    where
      date = transactionDateFn opts
      ts = jtxns $ journalSelectingAmountFromOpts opts j

-- | Convert all the posting amounts in an EntriesReport to their
-- default valuation commodities. This means using the Journal's most
-- recent applicable market prices before the valuation date.
-- The valuation date is set with --value-date and can be:
-- a custom date;
-- the posting date;
-- the last day in the report period, or in the journal if no period
-- (or the posting date, if journal is empty - shouldn't happen);
-- or today's date (gives an error if today_ is not set in ReportOpts).
erValue :: ReportOpts -> Journal -> EntriesReport -> EntriesReport
erValue ropts@ReportOpts{..} j ts =
  map txnvalue ts
  where
    txnvalue t@Transaction{..} = t{tpostings=map postingvalue tpostings}
    postingvalue p@Posting{..} = p{pamount=mixedAmountValue prices d pamount}
      where
        -- prices are in parse order - sort into date then parse order,
        -- & reversed for quick lookup of the latest price.
        prices = reverse $ sortOn mpdate $ jmarketprices j

        -- Get the last day of the report period.
        -- Will be Nothing if no report period is specified, or also
        -- if ReportOpts does not have today_ set, since we need that
        -- to get the report period robustly.
        mperiodlastday :: Maybe Day = do
          t <- today_
          let q = queryFromOpts t ropts
          qend <- queryEndDate False q
          return $ addDays (-1) qend

        mperiodorjournallastday = mperiodlastday <|> journalEndDate False j

        d = case value_at_ of
          AtTransaction -> postingDate p
          AtPeriod      -> fromMaybe (postingDate p) mperiodorjournallastday
          AtNow         -> case today_ of
                             Just d  -> d
                             Nothing -> error' "ReportOpts today_ is unset so could not satisfy --value-at=now"
          AtDate d      -> d

tests_EntriesReport = tests "EntriesReport" [
  tests "entriesReport" [
     test "not acct" $ (length $ entriesReport defreportopts (Not $ Acct "bank") samplejournal) `is` 1
    ,test "date" $ (length $ entriesReport defreportopts (Date $ mkdatespan "2008/06/01" "2008/07/01") samplejournal) `is` 3
  ]
 ]

