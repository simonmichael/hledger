{-# LANGUAGE OverloadedStrings, RecordWildCards, DeriveDataTypeable, FlexibleInstances #-}
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
entriesReport opts q j =
  (if value_ opts then erValue opts j else id) $
  sortBy (comparing date) $ filter (q `matchesTransaction`) ts
    where
      date = transactionDateFn opts
      ts = jtxns $ journalSelectingAmountFromOpts opts j

-- | Convert all the posting amounts in an EntriesReport to their
-- default valuation commodities. This means using the Journal's most
-- recent applicable market prices before the valuation date.
-- The valuation date is the specified report end date if any,
-- otherwise the current date, otherwise the journal's end date.
erValue :: ReportOpts -> Journal -> EntriesReport -> EntriesReport
erValue ropts j ts =
  let mvaluationdate = periodEnd (period_ ropts) <|> today_ ropts <|> journalEndDate False j
  in case mvaluationdate of
    Nothing -> ts
    Just d  -> map valuetxn ts
      where
        -- prices are in parse order - sort into date then parse order,
        -- & reversed for quick lookup of the latest price.
        prices = reverse $ sortOn mpdate $ jmarketprices j

        valuetxn t@Transaction{..} = t{tpostings=map valueposting tpostings}
        valueposting p@Posting{..} = p{pamount=mixedAmountValue prices d pamount}


tests_EntriesReport = tests "EntriesReport" [
  tests "entriesReport" [
     test "not acct" $ (length $ entriesReport defreportopts (Not $ Acct "bank") samplejournal) `is` 1
    ,test "date" $ (length $ entriesReport defreportopts (Date $ mkdatespan "2008/06/01" "2008/07/01") samplejournal) `is` 3
  ]
 ]

