{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-|

An account-centric transactions report.

-}

module Hledger.Reports.AccountTransactionsReport (
  AccountTransactionsReport,
  AccountTransactionsReportItem,
  accountTransactionsReport,
  accountTransactionsReportItems,
  transactionRegisterDate,
  triOrigTransaction,
  triDate,
  triAmount,
  triBalance,
  triCommodityAmount,
  triCommodityBalance,
  accountTransactionsReportByCommodity,
  tests_AccountTransactionsReport
)
where

import Data.List (mapAccumR, nub, partition, sortBy)
import Data.List.Extra (nubSort)
import Data.Maybe (catMaybes)
import Data.Ord (Down(..), comparing)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Calendar (Day)

import Hledger.Data
import Hledger.Query
import Hledger.Reports.ReportOptions
import Hledger.Utils


-- | An account transactions report represents transactions affecting
-- a particular account (or possibly several accounts, but we don't
-- use that). It is used eg by hledger-ui's and hledger-web's register
-- view, and hledger's aregister report, where we want to show one row
-- per transaction, in the context of the current account. Report
-- items consist of:
--
-- - the transaction, unmodified
--
-- - the transaction as seen in the context of the current account and query,
--   which means:
--
--   - the transaction date is set to the "transaction context date":
--     the earliest of the transaction date and any other posting dates
--     of postings to the current account (matched by the report query).
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
--   or if historical balance is requested (-H), the historical running total.
--   The historical running total includes transactions from before the
--   report start date if one is specified, filtered by the report query.
--   The historical running total may or may not be the account's historical
--   running balance, depending on the report query.
--
-- Items are sorted by transaction register date (the earliest date the transaction
-- posts to the current account), most recent first.
-- Reporting intervals are currently ignored.
--
type AccountTransactionsReport = [AccountTransactionsReportItem] -- line items, one per transaction

type AccountTransactionsReportItem =
  (
   Transaction -- the transaction, unmodified
  ,Transaction -- the transaction, as seen from the current account
  ,Bool        -- is this a split (more than one posting to other accounts) ?
  ,Text        -- a display string describing the other account(s), if any
  ,MixedAmount -- the amount posted to the current account(s) (or total amount posted)
  ,MixedAmount -- the register's running total or the current account(s)'s historical balance, after this transaction
  )

triOrigTransaction (torig,_,_,_,_,_) = torig
triDate (_,tacct,_,_,_,_) = tdate tacct
triAmount (_,_,_,_,a,_) = a
triBalance (_,_,_,_,_,a) = a
triCommodityAmount c = filterMixedAmountByCommodity c  . triAmount
triCommodityBalance c = filterMixedAmountByCommodity c  . triBalance

accountTransactionsReport :: ReportSpec -> Journal -> Query -> AccountTransactionsReport
accountTransactionsReport rspec@ReportSpec{_rsReportOpts=ropts} j thisacctq = items
  where
    -- A depth limit should not affect the account transactions report; it should show all transactions in/below this account.
    -- Queries on currency or amount are also ignored at this stage; they are handled earlier, before valuation.
    reportq = simplifyQuery $ And [aregisterq, periodq]
      where
        aregisterq = filterQuery (not . queryIsCurOrAmt) . filterQuery (not . queryIsDepth) $ _rsQuery rspec
        periodq = Date . periodAsDateSpan $ period_ ropts
    amtq = filterQuery queryIsCurOrAmt $ _rsQuery rspec
    queryIsCurOrAmt q = queryIsSym q || queryIsAmt q

    -- Note that within this functions, we are only allowed limited
    -- transformation of the transaction postings: this is due to the need to
    -- pass the original transactions into accountTransactionsReportItem.
    -- Generally, we either include a transaction in full, or not at all.
    -- Do some limited filtering and valuing of the journal's transactions:
    -- - filter them by the account query if any,
    -- - discard amounts not matched by the currency and amount query if any,
    -- - then apply valuation if any.
    acctJournal =
          ptraceAtWith 5 (("ts3:\n"++).pshowTransactions.jtxns)
        -- maybe convert these transactions to cost or value
        . journalApplyValuationFromOpts rspec
        . ptraceAtWith 5 (("ts2:\n"++).pshowTransactions.jtxns)
        -- apply any cur:SYM filters in reportq
        . (if queryIsNull amtq then id else filterJournalAmounts amtq)
        -- only consider transactions which match thisacctq (possibly excluding postings
        -- which are not real or have the wrong status)
        . traceAt 3 ("thisacctq: "++show thisacctq)
        $ ptraceAtWith 5 (("ts1:\n"++).pshowTransactions.jtxns)
          j{jtxns = filter (matchesTransaction thisacctq . relevantPostings) $ jtxns j}
      where
        relevantPostings
          | queryIsNull realq && queryIsNull statusq = id
          | otherwise = filterTransactionPostings . simplifyQuery $ And [realq, statusq]
        realq   = filterQuery queryIsReal reportq
        statusq = filterQuery queryIsStatus reportq

    startbal
      | balanceaccum_ ropts == Historical = sumPostings priorps
      | otherwise                         = nullmixedamt
      where
        priorps = dbg5 "priorps" . journalPostings $ filterJournalPostings priorq acctJournal
        priorq = dbg5 "priorq" $ And [thisacctq, tostartdateq, datelessreportq]
        tostartdateq =
          case mstartdate of
            Just _  -> Date (DateSpan Nothing mstartdate)
            Nothing -> None  -- no start date specified, there are no prior postings
        mstartdate = queryStartDate (date2_ ropts) reportq
        datelessreportq = filterQuery (not . queryIsDateOrDate2) reportq

    items =
        accountTransactionsReportItems reportq thisacctq startbal maNegate
      -- sort by the transaction's register date, then index, for accurate starting balance
      . ptraceAtWith 5 (("ts4:\n"++).pshowTransactions.map snd)
      . sortBy (comparing (Down . fst) <> comparing (Down . tindex . snd))
      . map (\t -> (transactionRegisterDate reportq thisacctq t, t))
      $ jtxns acctJournal

pshowTransactions :: [Transaction] -> String
pshowTransactions = pshow . map (\t -> unwords [show $ tdate t, T.unpack $ tdescription t])

-- | Generate transactions report items from a list of transactions,
-- using the provided user-specified report query, a query specifying
-- which account to use as the focus, a starting balance, and a sign-setting
-- function.
-- Each transaction is accompanied by the date that should be shown for it
-- in the report, which is not necessarily the transaction date; it is
-- the earliest of the posting dates which match both thisacctq and reportq,
-- otherwise the transaction's date if there are no matching postings.
accountTransactionsReportItems :: Query -> Query -> MixedAmount -> (MixedAmount -> MixedAmount)
                               -> [(Day, Transaction)] -> [AccountTransactionsReportItem]
accountTransactionsReportItems reportq thisacctq bal signfn =
    catMaybes . snd . mapAccumR (accountTransactionsReportItem reportq thisacctq signfn) bal

accountTransactionsReportItem :: Query -> Query -> (MixedAmount -> MixedAmount) -> MixedAmount
                              -> (Day, Transaction) -> (MixedAmount, Maybe AccountTransactionsReportItem)
accountTransactionsReportItem reportq thisacctq signfn bal (d, torig)
    -- 201407: I've lost my grip on this, let's just hope for the best
    -- 201606: we now calculate change and balance from filtered postings, check this still works well for all callers XXX
    | null reportps = (bal, Nothing)  -- no matched postings in this transaction, skip it
    | otherwise     = (b, Just (torig, tacct{tdate=d}, numotheraccts > 1, otheracctstr, a, b))
    where
      tacct@Transaction{tpostings=reportps} = filterTransactionPostings reportq torig
      (thisacctps, otheracctps) = partition (matchesPosting thisacctq) reportps
      numotheraccts = length $ nub $ map paccount otheracctps
      otheracctstr | thisacctq == None  = summarisePostingAccounts reportps     -- no current account ? summarise all matched postings
                   | numotheraccts == 0 = summarisePostingAccounts thisacctps   -- only postings to current account ? summarise those
                   | otherwise          = summarisePostingAccounts otheracctps  -- summarise matched postings to other account(s)
      a = signfn . maNegate $ sumPostings thisacctps
      b = bal `maPlus` a

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
summarisePostingAccounts :: [Posting] -> Text
summarisePostingAccounts ps =
    T.intercalate ", " . map accountSummarisedName . nub $ map paccount displayps
  where
    realps = filter isReal ps
    displayps | null realps = ps
              | otherwise   = realps

-- | Split an  account transactions report whose items may involve several commodities,
-- into one or more single-commodity account transactions reports.
accountTransactionsReportByCommodity :: AccountTransactionsReport -> [(CommoditySymbol, AccountTransactionsReport)]
accountTransactionsReportByCommodity tr =
  [(c, filterAccountTransactionsReportByCommodity c tr) | c <- commodities tr]
  where
    commodities = nubSort . map acommodity . concatMap (amounts . triAmount)

-- | Remove account transaction report items and item amount (and running
-- balance amount) components that don't involve the specified
-- commodity. Other item fields such as the transaction are left unchanged.
filterAccountTransactionsReportByCommodity :: CommoditySymbol -> AccountTransactionsReport -> AccountTransactionsReport
filterAccountTransactionsReportByCommodity c =
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

tests_AccountTransactionsReport = testGroup "AccountTransactionsReport" [
 ]
