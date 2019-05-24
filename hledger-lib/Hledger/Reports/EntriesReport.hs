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
entriesReport ropts@ReportOpts{..} q j@Journal{..} =
  sortBy (comparing datefn) $ filter (q `matchesTransaction`) $ map tvalue jtxns
  where
    datefn = transactionDateFn ropts
    tvalue t@Transaction{..} = t{tpostings=map pvalue tpostings}
    pvalue p@Posting{..} = case value_ of
      Nothing             -> p
      Just (AtCost _mc)   -> postingToCost (journalCommodityStyles j) p
      Just (AtEnd _mc)    -> postingValue jmarketprices (fromMaybe (postingDate p)  -- XXX shouldn't happen
                                                         mperiodorjournallastday) p
      Just (AtNow _mc)    -> postingValue jmarketprices (
        case today_ of Just d  -> d
                       Nothing -> error' "erValue: ReportOpts today_ is unset so could not satisfy --value=now") p
      Just (AtDate d _mc) -> postingValue jmarketprices d p
      where
        mperiodorjournallastday = mperiodlastday <|> journalEndDate False j
          where
            -- The last day of the report period.
            -- Will be Nothing if no report period is specified, or also
            -- if ReportOpts does not have today_ set, since we need that
            -- to get the report period robustly.
            mperiodlastday :: Maybe Day = do
              t <- today_
              let q = queryFromOpts t ropts
              qend <- queryEndDate False q
              return $ addDays (-1) qend

tests_EntriesReport = tests "EntriesReport" [
  tests "entriesReport" [
     test "not acct" $ (length $ entriesReport defreportopts (Not $ Acct "bank") samplejournal) `is` 1
    ,test "date" $ (length $ entriesReport defreportopts (Date $ mkdatespan "2008/06/01" "2008/07/01") samplejournal) `is` 3
  ]
 ]

