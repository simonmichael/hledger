{-# LANGUAGE OverloadedStrings, RecordWildCards, DeriveDataTypeable, FlexibleInstances #-}
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

import Data.List
import Data.Ord

import Hledger.Data
import Hledger.Query
import Hledger.Reports.ReportOptions
import Hledger.Reports.AccountTransactionsReport
import Hledger.Utils


-- | A transactions report includes a list of transactions touching multiple accounts
-- (posting-filtered and unfiltered variants), a running balance, and some
-- other information helpful for rendering a register view (a flag
-- indicating multiple other accounts and a display string describing
-- them) with or without a notion of current account(s).
-- Two kinds of report use this data structure, see transactionsReport
-- and accountTransactionsReport below for details.
type TransactionsReport = (String                   -- label for the balance column, eg "balance" or "total"
                          ,[TransactionsReportItem] -- line items, one per transaction
                          )
type TransactionsReportItem = (Transaction -- the original journal transaction, unmodified
                              ,Transaction -- the transaction as seen from a particular account, with postings maybe filtered
                              ,Bool        -- is this a split, ie more than one other account posting
                              ,String      -- a display string describing the other account(s), if any
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

totallabel   = "Period Total"

-- | Select transactions from the whole journal. This is similar to a
-- "postingsReport" except with transaction-based report items which
-- are ordered most recent first. XXX Or an EntriesReport - use that instead ?
-- This is used by hledger-web's journal view.
transactionsReport :: ReportOpts -> Journal -> Query -> TransactionsReport
transactionsReport opts j q = (totallabel, items)
   where
     -- XXX items' first element should be the full transaction with all postings
     items = reverse $ accountTransactionsReportItems q None nullmixedamt id ts
     ts    = sortBy (comparing date) $ filter (q `matchesTransaction`) $ jtxns $ journalSelectingAmountFromOpts opts j
     date  = transactionDateFn opts

-- | Split a transactions report whose items may involve several commodities,
-- into one or more single-commodity transactions reports.
transactionsReportByCommodity :: TransactionsReport -> [(CommoditySymbol, TransactionsReport)]
transactionsReportByCommodity tr =
  [(c, filterTransactionsReportByCommodity c tr) | c <- transactionsReportCommodities tr]
  where
    transactionsReportCommodities (_,items) =
      nub $ sort $ map acommodity $ concatMap (amounts . triAmount) items

-- Remove transaction report items and item amount (and running
-- balance amount) components that don't involve the specified
-- commodity. Other item fields such as the transaction are left unchanged.
filterTransactionsReportByCommodity :: CommoditySymbol -> TransactionsReport -> TransactionsReport
filterTransactionsReportByCommodity c (label,items) =
  (label, fixTransactionsReportItemBalances $ concat [filterTransactionsReportItemByCommodity c i | i <- items])
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
          where bal' = bal + amt

-- tests

tests_TransactionsReport = tests "TransactionsReport" [
 ]
