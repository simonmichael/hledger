{-# LANGUAGE RecordWildCards, DeriveDataTypeable, FlexibleInstances #-}
{-|

Whole-journal, account-centric, and per-commodity transactions reports, used by hledger-web.

-}

module Hledger.Reports.TransactionsReports (
  TransactionsReport,
  TransactionsReportItem,
  triDate,
  triBalance,
  triSimpleBalance,
  journalTransactionsReport,
  accountTransactionsReport,
  transactionsReportByCommodity

  -- -- * Tests
  -- tests_Hledger_Reports_TransactionsReports
)
where

import Data.List
import Data.Maybe
import Data.Ord
import Test.HUnit

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
type TransactionsReportItem = (Transaction -- the corresponding transaction
                              ,Transaction -- the transaction with postings to the current account(s) removed
                              ,Bool        -- is this a split, ie more than one other account posting
                              ,String      -- a display string describing the other account(s), if any
                              ,MixedAmount -- the amount posted to the current account(s) (or total amount posted)
                              ,MixedAmount -- the running balance for the current account(s) after this transaction
                              )

triDate (t,_,_,_,_,_) = tdate t
triAmount (_,_,_,_,a,_) = a
triBalance (_,_,_,_,_,a) = a
triSimpleBalance (_,_,_,_,_,Mixed a) = case a of [] -> "0"
                                                 (Amount{aquantity=q}):_ -> show q

-------------------------------------------------------------------------------

-- | Select transactions from the whole journal. This is similar to a
-- "postingsReport" except with transaction-based report items which
-- are ordered most recent first. This is used by eg hledger-web's journal view.
journalTransactionsReport :: ReportOpts -> Journal -> Query -> TransactionsReport
journalTransactionsReport _ Journal{jtxns=ts} m = (totallabel, items)
   where
     ts' = sortBy (comparing tdate) $ filter (not . null . tpostings) $ map (filterTransactionPostings m) ts
     items = reverse $ accountTransactionsReportItems m Nothing nullmixedamt id ts'
     -- XXX items' first element should be the full transaction with all postings

-------------------------------------------------------------------------------

-- | Select transactions within one or more current accounts, and make a
-- transactions report relative to those account(s). This means:
--
-- 1. it shows transactions from the point of view of the current account(s).
--    The transaction amount is the amount posted to the current account(s).
--    The other accounts' names are provided. 
--
-- 2. With no transaction filtering in effect other than a start date, it
--    shows the accurate historical running balance for the current account(s).
--    Otherwise it shows a running total starting at 0.
--
-- This is used by eg hledger-web's account register view. Currently,
-- reporting intervals are not supported, and report items are most
-- recent first.
accountTransactionsReport :: ReportOpts -> Journal -> Query -> Query -> TransactionsReport
accountTransactionsReport opts j m thisacctquery = (label, items)
 where
     -- transactions affecting this account, in date order
     ts = sortBy (comparing tdate) $ filter (matchesTransaction thisacctquery) $ jtxns $
          journalSelectingAmountFromOpts opts j
     -- starting balance: if we are filtering by a start date and nothing else,
     -- the sum of postings to this account before that date; otherwise zero.
     (startbal,label) | queryIsNull m                           = (nullmixedamt,        balancelabel)
                      | queryIsStartDateOnly (date2_ opts) m = (sumPostings priorps, balancelabel)
                      | otherwise                                 = (nullmixedamt,        totallabel)
                      where
                        priorps = -- ltrace "priorps" $
                                  filter (matchesPosting
                                          (-- ltrace "priormatcher" $
                                           And [thisacctquery, tostartdatequery]))
                                         $ transactionsPostings ts
                        tostartdatequery = Date (DateSpan Nothing startdate)
                        startdate = queryStartDate (date2_ opts) m
     items = reverse $ accountTransactionsReportItems m (Just thisacctquery) startbal negate ts

totallabel = "Total"
balancelabel = "Balance"

-- | Generate transactions report items from a list of transactions,
-- using the provided query and current account queries, starting balance,
-- sign-setting function and balance-summing function.
accountTransactionsReportItems :: Query -> Maybe Query -> MixedAmount -> (MixedAmount -> MixedAmount) -> [Transaction] -> [TransactionsReportItem]
accountTransactionsReportItems _ _ _ _ [] = []
accountTransactionsReportItems query thisacctquery bal signfn (t:ts) =
    -- This is used for both accountTransactionsReport and journalTransactionsReport,
    -- which makes it a bit overcomplicated
    case i of Just i' -> i':is
              Nothing -> is
    where
      tmatched@Transaction{tpostings=psmatched} = filterTransactionPostings query t
      (psthisacct,psotheracct) = case thisacctquery of Just m  -> partition (matchesPosting m) psmatched
                                                       Nothing -> ([],psmatched)
      numotheraccts = length $ nub $ map paccount psotheracct
      amt = negate $ sum $ map pamount psthisacct
      acct | isNothing thisacctquery = summarisePostings psmatched -- journal register
           | numotheraccts == 0 = "transfer between " ++ summarisePostingAccounts psthisacct
           | otherwise          = prefix              ++ summarisePostingAccounts psotheracct
           where prefix = maybe "" (\b -> if b then "from " else "to ") $ isNegativeMixedAmount amt
      (i,bal') = case psmatched of
           [] -> (Nothing,bal)
           _  -> (Just (t, tmatched, numotheraccts > 1, acct, a, b), b)
                 where
                  a = signfn amt
                  b = bal + a
      is = accountTransactionsReportItems query thisacctquery bal' signfn ts

-- | Generate a short readable summary of some postings, like
-- "from (negatives) to (positives)".
summarisePostings :: [Posting] -> String
summarisePostings ps =
    case (summarisePostingAccounts froms, summarisePostingAccounts tos) of
       ("",t) -> "to "++t
       (f,"") -> "from "++f
       (f,t)  -> "from "++f++" to "++t
    where
      (froms,tos) = partition (fromMaybe False . isNegativeMixedAmount . pamount) ps

-- | Generate a simplified summary of some postings' accounts.
summarisePostingAccounts :: [Posting] -> String
summarisePostingAccounts = intercalate ", " . map accountLeafName . nub . map paccount

filterTransactionPostings :: Query -> Transaction -> Transaction
filterTransactionPostings m t@Transaction{tpostings=ps} = t{tpostings=filter (m `matchesPosting`) ps}

-------------------------------------------------------------------------------

-- | Split a transactions report whose items may involve several commodities,
-- into one or more single-commodity transactions reports.
transactionsReportByCommodity :: TransactionsReport -> [TransactionsReport]
transactionsReportByCommodity tr =
  [filterTransactionsReportByCommodity c tr | c <- transactionsReportCommodities tr]
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

-- | Filter out all but the specified commodity from this amount.
filterMixedAmountByCommodity :: Commodity -> MixedAmount -> MixedAmount
filterMixedAmountByCommodity c (Mixed as) = Mixed $ filter ((==c). acommodity) as

-------------------------------------------------------------------------------

