{-# LANGUAGE RecordWildCards, DeriveDataTypeable, FlexibleInstances #-}
{-|

Here are several variants of a transactions report.
Transactions reports are like a postings report, but more
transaction-oriented, and (in the account-centric variant) relative to
a some base account.  They are used by hledger-web.

-}

module Hledger.Reports.TransactionsReports (
  TransactionsReport,
  TransactionsReportItem,
  AccountTransactionsReport,
  AccountTransactionsReportItem,
  triOrigTransaction,
  triDate,
  triAmount,
  triBalance,
  triCommodityAmount,
  triCommodityBalance,
  journalTransactionsReport,
  accountTransactionsReport,
  transactionsReportByCommodity

  -- -- * Tests
  -- tests_Hledger_Reports_TransactionsReports
)
where

import Data.List
import Data.Ord
-- import Test.HUnit

import Hledger.Data
import Hledger.Query
import Hledger.Reports.ReportOptions


-- | A transactions report includes a list of transactions
-- (posting-filtered and unfiltered variants), a running balance, and some
-- other information helpful for rendering a register view (a flag
-- indicating multiple other accounts and a display string describing
-- them) with or without a notion of current account(s).
-- Two kinds of report use this data structure, see journalTransactionsReport
-- and accountTransactionsReport below for detais.
type TransactionsReport = (String                   -- label for the balance column, eg "balance" or "total"
                          ,[TransactionsReportItem] -- line items, one per transaction
                          )
type TransactionsReportItem = (Transaction -- the original journal transaction, unmodified
                              ,Transaction -- the transaction as seen from a particular account
                              ,Bool        -- is this a split, ie more than one other account posting
                              ,String      -- a display string describing the other account(s), if any
                              ,MixedAmount -- the amount posted to the current account(s) (or total amount posted)
                              ,MixedAmount -- the running balance for the current account(s) after this transaction
                              )

triOrigTransaction (torig,_,_,_,_,_) = torig
triDate (_,tacct,_,_,_,_) = tdate tacct
triAmount (_,_,_,_,a,_) = a
triBalance (_,_,_,_,_,a) = a
triCommodityAmount c = filterMixedAmountByCommodity c  . triAmount
triCommodityBalance c = filterMixedAmountByCommodity c  . triBalance

-------------------------------------------------------------------------------

-- | Select transactions from the whole journal. This is similar to a
-- "postingsReport" except with transaction-based report items which
-- are ordered most recent first. XXX Or an EntriesReport - use that instead ?
-- This is used by hledger-web's journal view.
journalTransactionsReport :: ReportOpts -> Journal -> Query -> TransactionsReport
journalTransactionsReport opts j q = (totallabel, items)
   where
     -- XXX items' first element should be the full transaction with all postings
     items = reverse $ accountTransactionsReportItems q None nullmixedamt id ts
     ts    = sortBy (comparing date) $ filter (q `matchesTransaction`) $ jtxns $ journalSelectingAmountFromOpts opts j
     date  = transactionDateFn opts

-------------------------------------------------------------------------------

-- | An account transactions report represents transactions affecting
-- a particular account (or possibly several accounts, but we don't
-- use that). It is used by hledger-web's (and hledger-ui's) account
-- register view, where we want to show one row per journal
-- transaction, with:
--
-- - the total increase/decrease to the current account
--
-- - the names of the other account(s) posted to/from
--
-- - transaction dates, adjusted to the date of the earliest posting to
--   the current account if those postings have their own dates
--
-- Currently, reporting intervals are not supported, and report items
-- are most recent first.
--
type AccountTransactionsReport =
  (String                          -- label for the balance column, eg "balance" or "total"
  ,[AccountTransactionsReportItem] -- line items, one per transaction
  )

type AccountTransactionsReportItem =
  (
   Transaction -- the original journal transaction
  ,Transaction -- the adjusted account transaction
  ,Bool        -- is this a split, ie with more than one posting to other account(s)
  ,String      -- a display string describing the other account(s), if any
  ,MixedAmount -- the amount posted to the current account(s) (or total amount posted)
  ,MixedAmount -- the running balance for the current account(s) after this transaction
  )

accountTransactionsReport :: ReportOpts -> Journal -> Query -> Query -> AccountTransactionsReport
accountTransactionsReport opts j q thisacctquery = (label, items)
 where
     -- transactions with excluded currencies removed
     ts1 = jtxns $
           filterJournalAmounts (filterQuery queryIsSym q) $
           journalSelectingAmountFromOpts opts j
     -- affecting this account
     ts2 = filter (matchesTransaction thisacctquery) ts1
     -- with dates adjusted for account transactions report
     ts3 = map (setTransactionDateToPostingDate q thisacctquery) ts2
     -- and sorted
     ts = sortBy (comparing tdate) ts3

     -- starting balance: if we are filtering by a start date and nothing else,
     -- the sum of postings to this account before that date; otherwise zero.
     (startbal,label) | queryIsNull q                        = (nullmixedamt,        balancelabel)
                      | queryIsStartDateOnly (date2_ opts) q = (sumPostings priorps, balancelabel)
                      | otherwise                            = (nullmixedamt,        totallabel)
                      where
                        priorps = -- ltrace "priorps" $
                                  filter (matchesPosting
                                          (-- ltrace "priormatcher" $
                                           And [thisacctquery, tostartdatequery]))
                                         $ transactionsPostings ts
                        tostartdatequery = Date (DateSpan Nothing startdate)
                        startdate = queryStartDate (date2_ opts) q

     items = reverse $ -- see also registerChartHtml
             accountTransactionsReportItems q thisacctquery startbal negate ts

-- | Adjust a transaction's date to the earliest date of postings to a
-- particular account, if any, after filtering with a certain query.
setTransactionDateToPostingDate :: Query -> Query -> Transaction -> Transaction
setTransactionDateToPostingDate query thisacctquery t = t'
  where
    queryps = tpostings $ filterTransactionPostings query t
    thisacctps = filter (matchesPosting thisacctquery) queryps
    t' = case thisacctps of
          [] -> t
          _  -> t{tdate=d}
            where
              d | null ds   = tdate t
                | otherwise = minimum ds
              ds = map postingDate thisacctps
                   -- no opts here, don't even bother with that date/date2 rigmarole

totallabel = "Running Total"
balancelabel = "Historical Balance"

-- | Generate transactions report items from a list of transactions,
-- using the provided query and current account queries, starting
-- balance, sign-setting function and balance-summing function. With a
-- "this account" query of None, this can be used the for the
-- journalTransactionsReport also.
accountTransactionsReportItems :: Query -> Query -> MixedAmount -> (MixedAmount -> MixedAmount) -> [Transaction] -> [TransactionsReportItem]
accountTransactionsReportItems _ _ _ _ [] = []
accountTransactionsReportItems query thisacctquery bal signfn (torig:ts) =
    -- This is used for both accountTransactionsReport and journalTransactionsReport,
    -- which makes it a bit overcomplicated
    case i of Just i' -> i':is
              Nothing -> is
    where
      -- XXX I've lost my grip on this, let's just hope for the best
      origps = tpostings torig
      tacct@Transaction{tpostings=queryps} = filterTransactionPostings query torig
      (thisacctps, otheracctps) = partition (matchesPosting thisacctquery) origps
      amt = negate $ sum $ map pamount thisacctps
      numotheraccts = length $ nub $ map paccount otheracctps
      otheracctstr | thisacctquery == None = summarisePostingAccounts origps
                   | numotheraccts == 0    = summarisePostingAccounts thisacctps
                   | otherwise             = summarisePostingAccounts otheracctps
      (i,bal') = case queryps of
           [] -> (Nothing,bal)
           _  -> (Just (torig, tacct, numotheraccts > 1, otheracctstr, a, b), b)
                 where
                  a = signfn amt
                  b = bal + a
      is = accountTransactionsReportItems query thisacctquery bal' signfn ts

-- -- | Generate a short readable summary of some postings, like
-- -- "from (negatives) to (positives)".
-- summarisePostings :: [Posting] -> String
-- summarisePostings ps =
--     case (summarisePostingAccounts froms, summarisePostingAccounts tos) of
--        ("",t) -> "to "++t
--        (f,"") -> "from "++f
--        (f,t)  -> "from "++f++" to "++t
--     where
--       (froms,tos) = partition (fromMaybe False . isNegativeMixedAmount . pamount) ps

-- | Generate a simplified summary of some postings' accounts.
summarisePostingAccounts :: [Posting] -> String
summarisePostingAccounts = intercalate ", " . map accountSummarisedName . nub . map paccount

filterTransactionPostings :: Query -> Transaction -> Transaction
filterTransactionPostings m t@Transaction{tpostings=ps} = t{tpostings=filter (m `matchesPosting`) ps}

-------------------------------------------------------------------------------

-- | Split a transactions report whose items may involve several commodities,
-- into one or more single-commodity transactions reports.
transactionsReportByCommodity :: TransactionsReport -> [(Commodity, TransactionsReport)]
transactionsReportByCommodity tr =
  [(c, filterTransactionsReportByCommodity c tr) | c <- transactionsReportCommodities tr]
  where
    transactionsReportCommodities (_,items) =
      nub $ sort $ map acommodity $ concatMap (amounts . triAmount) items

-- Remove transaction report items and item amount (and running
-- balance amount) components that don't involve the specified
-- commodity. Other item fields such as the transaction are left unchanged.
filterTransactionsReportByCommodity :: Commodity -> TransactionsReport -> TransactionsReport
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

-------------------------------------------------------------------------------

