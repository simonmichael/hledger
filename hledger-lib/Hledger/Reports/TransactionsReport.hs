{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-|

A transactions report. Like an EntriesReport, but with more
information such as a running balance.

-}

module Hledger.Reports.TransactionsReport (
  TransactionsReport,
  TransactionsReportItem,
  transactionsReport,
  transactionsReportByCommodity,
  triOrigTransaction,
  triDate,
  triAmount,
  triBalance,
  triCommodityAmount,
  triCommodityBalance,
  tests_TransactionsReport
)
where

import Data.List (sortBy)
import Data.List.Extra (nubSort)
import Data.Ord (comparing)
import Data.Text (Text)

import Hledger.Data
import Hledger.Query
import Hledger.Reports.ReportOptions
import Hledger.Reports.AccountTransactionsReport
import Hledger.Utils


-- | A transactions report includes a list of transactions touching multiple accounts
-- (posting-filtered and unfiltered variants), a running balance, and some
-- other information helpful for rendering a register view with or without a notion
-- of current account(s). Two kinds of report use this data structure, see transactionsReport
-- and accountTransactionsReport below for details.
type TransactionsReport = [TransactionsReportItem] -- line items, one per transaction
type TransactionsReportItem = (Transaction -- the original journal transaction, unmodified
                              ,Transaction -- the transaction as seen from a particular account, with postings maybe filtered
                              ,Bool        -- is this a split, ie more than one other account posting
                              ,Text        -- a display string describing the other account(s), if any
                              ,MixedAmount -- the amount posted to the current account(s) by the filtered postings (or total amount posted)
                              ,MixedAmount -- the running total of item amounts, starting from zero;
                                           -- or with --historical, the running total including items
                                           -- (matched by the report query) preceding the report period
                              )

triOrigTransaction (torig,_,_,_,_,_) = torig
triDate (_,tacct,_,_,_,_) = tdate tacct
triAmount (_,_,_,_,a,_) = a
triBalance (_,_,_,_,_,a) = a
triCommodityAmount c = filterMixedAmountByCommodity c  . triAmount
triCommodityBalance c = filterMixedAmountByCommodity c  . triBalance

-- | Select transactions from the whole journal. This is similar to a
-- "postingsReport" except with transaction-based report items which
-- are ordered most recent first. XXX Or an EntriesReport - use that instead ?
-- This is used by hledger-web's journal view.
transactionsReport :: ReportSpec -> Journal -> Query -> TransactionsReport
transactionsReport rspec j q = items
   where
     -- XXX items' first element should be the full transaction with all postings
     items = reverse $ accountTransactionsReportItems q None nullmixedamt id ts
     ts    = sortBy (comparing date) $ filter (q `matchesTransaction`) $ jtxns $ journalApplyValuationFromOpts rspec j
     date = transactionDateFn $ rsOpts rspec

-- | Split a transactions report whose items may involve several commodities,
-- into one or more single-commodity transactions reports.
transactionsReportByCommodity :: TransactionsReport -> [(CommoditySymbol, TransactionsReport)]
transactionsReportByCommodity tr =
  [(c, filterTransactionsReportByCommodity c tr) | c <- transactionsReportCommodities tr]
  where
    transactionsReportCommodities = nubSort . map acommodity . concatMap (amounts . triAmount)

-- Remove transaction report items and item amount (and running
-- balance amount) components that don't involve the specified
-- commodity. Other item fields such as the transaction are left unchanged.
filterTransactionsReportByCommodity :: CommoditySymbol -> TransactionsReport -> TransactionsReport
filterTransactionsReportByCommodity c =
    fixTransactionsReportItemBalances . concatMap (filterTransactionsReportItemByCommodity c)
  where
    filterTransactionsReportItemByCommodity c (t,t2,s,o,a,bal)
      | c `elem` cs = [item']
      | otherwise   = []
      where
        cs = map acommodity $ amounts a
        item' = (t,t2,s,o,a',bal)
        a' = filterMixedAmountByCommodity c a

    fixTransactionsReportItemBalances [] = []
    fixTransactionsReportItemBalances [i] = [i]
    fixTransactionsReportItemBalances items = reverse $ i:(go startbal is)
      where
        i:is = reverse items
        startbal = filterMixedAmountByCommodity c $ triBalance i
        go _ [] = []
        go bal ((t,t2,s,o,amt,_):is) = (t,t2,s,o,amt,bal'):go bal' is
          where bal' = bal `maPlus` amt

-- tests

tests_TransactionsReport = tests "TransactionsReport" [
 ]
