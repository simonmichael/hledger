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
  transactionsReportByCommodity,
  transactionRegisterDate

  -- -- * Tests
  -- tests_Hledger_Reports_TransactionsReports
)
where

import Data.List
import Data.Ord
-- import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Calendar
-- import Test.HUnit

import Hledger.Data
import Hledger.Query
import Hledger.Reports.ReportOptions
--import Hledger.Utils.Debug


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
-- use that). It is used eg by hledger-ui's and hledger-web's account
-- register view, where we want to show one row per transaction, in
-- the context of the current account. Report items consist of:
--
-- - the transaction, unmodified
--
-- - the transaction as seen in the context of the current account and query,
--   which means:
--
--   - the transaction date is set to the "transaction context date",
--     which can be different from the transaction's general date:
--     if postings to the current account (and matched by the report query)
--     have their own dates, it's the earliest of these dates.
--
--   - the transaction's postings are filtered, excluding any which are not
--     matched by the report query
--
-- - a text description of the other account(s) posted to/from
--
-- - a flag indicating whether there's more than one other account involved
--
-- - the total increase/decrease to the current account
--
-- - the report transactions' running total after this transaction;
--   or if historical balance is requested, the historical running total,
--   including transactions (filtered by the report query) from before
--   the report start date
--
-- Items are sorted by transaction context date, most recent first.
-- Reporting intervals are currently ignored.
--
type AccountTransactionsReport =
  (String                          -- label for the balance column, eg "balance" or "total"
  ,[AccountTransactionsReportItem] -- line items, one per transaction
  )

type AccountTransactionsReportItem =
  (
   Transaction -- the transaction, unmodified
  ,Transaction -- the transaction, as seen from the current account
  ,Bool        -- is this a split (more than one posting to other accounts) ?
  ,String      -- a display string describing the other account(s), if any
  ,MixedAmount -- the amount posted to the current account(s) (or total amount posted)
  ,MixedAmount -- the register's running total or the current account(s)'s historical balance, after this transaction
  )

accountTransactionsReport :: ReportOpts -> Journal -> Query -> Query -> AccountTransactionsReport
accountTransactionsReport opts j reportq thisacctq = (label, items)
  where
    -- a depth limit does not affect the account transactions report
    q  = -- filterQuery (not . queryIsDepth) -- seems unnecessary for some reason XXX
         reportq
    -- get all transactions, with amounts converted to cost basis if -B
    ts1 = jtxns $ journalSelectingAmountFromOpts opts j
    -- apply any cur:SYM filters in q
    symq  = filterQuery queryIsSym q
    ts2 = (if queryIsNull symq then id else map (filterTransactionAmounts symq)) ts1
    -- keep just the transactions affecting this account (via possibly realness or status-filtered postings)
    realq = filterQuery queryIsReal q
    statusq = filterQuery queryIsStatus q
    ts3 = filter (matchesTransaction thisacctq . filterTransactionPostings (And [realq, statusq])) ts2
    -- better sort by the transaction's register date, for accurate starting balance
    ts = sortBy (comparing (transactionRegisterDate reportq thisacctq)) ts3

    (startbal,label)
      -- queryIsNull q                            = (nullmixedamt,        balancelabel)
      | balancetype_ opts == HistoricalBalance = (sumPostings priorps, balancelabel)
      | otherwise                              = (nullmixedamt,        totallabel)
      where
        priorps = -- ltrace "priorps" $
                  filter (matchesPosting
                          (-- ltrace "priormatcher" $
                           And [thisacctq, realq, statusq, tostartdatequery]))
                         $ transactionsPostings ts
        tostartdatequery =
          case mstartdate of
            Just _  -> Date (DateSpan Nothing mstartdate)
            Nothing -> None  -- no start date specified, don't add up any prior postings
        mstartdate = queryStartDate (date2_ opts) q

    items = reverse $ -- see also registerChartHtml
            accountTransactionsReportItems q thisacctq startbal negate ts

totallabel = "Period Total"
balancelabel = "Historical Total"

-- | Generate transactions report items from a list of transactions,
-- using the provided user-specified report query, a query specifying
-- which account to use as the focus, a starting balance, a sign-setting
-- function and a balance-summing function. Or with a None current account
-- query, this can also be used for the journalTransactionsReport.
accountTransactionsReportItems :: Query -> Query -> MixedAmount -> (MixedAmount -> MixedAmount) -> [Transaction] -> [TransactionsReportItem]
accountTransactionsReportItems _ _ _ _ [] = []
accountTransactionsReportItems reportq thisacctq bal signfn (torig:ts) =
    case i of Just i' -> i':is
              Nothing -> is
    -- 201403: This is used for both accountTransactionsReport and journalTransactionsReport, which makes it a bit overcomplicated
    -- 201407: I've lost my grip on this, let's just hope for the best
    -- 201606: we now calculate change and balance from filtered postings, check this still works well for all callers XXX
    where
      tfiltered@Transaction{tpostings=reportps} = filterTransactionPostings reportq torig
      tacct = tfiltered{tdate=transactionRegisterDate reportq thisacctq tfiltered}
      (i,bal') = case reportps of
           [] -> (Nothing,bal)  -- no matched postings in this transaction, skip it
           _  -> (Just (torig, tacct, numotheraccts > 1, otheracctstr, a, b), b)
                 where
                  (thisacctps, otheracctps) = partition (matchesPosting thisacctq) reportps
                  numotheraccts = length $ nub $ map paccount otheracctps
                  otheracctstr | thisacctq == None  = summarisePostingAccounts reportps     -- no current account ? summarise all matched postings
                               | numotheraccts == 0 = summarisePostingAccounts thisacctps   -- only postings to current account ? summarise those
                               | otherwise          = summarisePostingAccounts otheracctps  -- summarise matched postings to other account(s)
                  a = signfn $ negate $ sum $ map pamount thisacctps
                  b = bal + a
      is = accountTransactionsReportItems reportq thisacctq bal' signfn ts

-- | What is the transaction's date in the context of a particular account
-- (specified with a query) and report query, as in an account register ?
-- It's normally the transaction's general date, but if any posting(s)
-- matched by the report query and affecting the matched account(s) have
-- their own earlier dates, it's the earliest of these dates.
-- Secondary transaction/posting dates are ignored.
transactionRegisterDate :: Query -> Query -> Transaction -> Day
transactionRegisterDate reportq thisacctq t
  | null thisacctps = tdate t
  | otherwise       = minimum $ map postingDate thisacctps
  where
    reportps   = tpostings $ filterTransactionPostings reportq t
    thisacctps = filter (matchesPosting thisacctq) reportps

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
-- To reduce noise, if there are both real and virtual postings, show only the real ones.
summarisePostingAccounts :: [Posting] -> String
summarisePostingAccounts ps =
  (intercalate ", " . map (T.unpack . accountSummarisedName) . nub . map paccount) displayps -- XXX pack
  where
    realps = filter isReal ps
    displayps | null realps = ps
              | otherwise   = realps

-------------------------------------------------------------------------------

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

-------------------------------------------------------------------------------

