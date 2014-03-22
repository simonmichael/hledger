{-# LANGUAGE RecordWildCards, DeriveDataTypeable, FlexibleInstances #-}
{-|

Multi-column balance reports, used by the balance command.

-}

module Hledger.Reports.MultiBalanceReports (
  MultiBalanceReport(..),
  MultiBalanceReportRow,
  periodBalanceReport,
  cumulativeOrHistoricalBalanceReport,

  -- -- * Tests
  -- tests_Hledger_Reports_MultiBalanceReport
)
where

import Data.List
import Data.Maybe
import Data.Ord
-- import Test.HUnit

import Hledger.Data
import Hledger.Query
import Hledger.Utils
import Hledger.Reports.ReportOptions
import Hledger.Reports.BalanceReport


-- | A multi balance report is a balance report with one or more columns. It has:
--
-- 1. a list of each column's date span
--
-- 2. a list of rows, each containing a renderable account name and the amounts to show in each column
--
-- 3. a list of each column's final total
--
-- The meaning of the amounts depends on the type of balance report (see
-- 'BalanceType' and "Hledger.Cli.Balance").
newtype MultiBalanceReport = MultiBalanceReport ([DateSpan]
                                                ,[MultiBalanceReportRow]
                                                ,[MixedAmount]
                                                )

-- | A row in a multi balance report has
--
-- * An account name, with rendering hints
--
-- * A list of amounts to be shown in each of the report's columns.
type MultiBalanceReportRow = (RenderableAccountName, [MixedAmount])

instance Show MultiBalanceReport where
    -- use ppShow to break long lists onto multiple lines
    -- we have to add some bogus extra shows here to help ppShow parse the output
    -- and wrap tuples and lists properly
    show (MultiBalanceReport (spans, items, totals)) =
        "MultiBalanceReport (ignore extra quotes):\n" ++ ppShow (show spans, map show items, totals)

-- | Generate a multi balance report for the matched accounts, showing
-- their change of balance in each of the specified periods.
-- Currently has some limitations compared to the simple balance report,
-- eg always displays accounts in --flat mode.
periodBalanceReport :: ReportOpts -> Query -> Journal -> MultiBalanceReport
periodBalanceReport opts q j = MultiBalanceReport (spans, items, totals)
    where
      (q',depthq)  = (filterQuery (not . queryIsDepth) q, filterQuery queryIsDepth q)
      clip = filter (depthq `matchesAccount`)
      j' = filterJournalPostings q' $ journalSelectingAmountFromOpts opts j
      ps = journalPostings $
           filterJournalPostingAmounts (filterQuery queryIsSym q) -- remove amount parts which the query's sym: terms would exclude
           j'

      -- the requested span is the span of the query (which is
      -- based on -b/-e/-p opts and query args IIRC).
      requestedspan = queryDateSpan (date2_ opts) q

      -- the report's span will be the requested span intersected with
      -- the selected data's span; or with -E, the requested span
      -- limited by the journal's overall span.
      reportspan | empty_ opts = requestedspan `orDatesFrom` journalspan
                 | otherwise   = requestedspan `spanIntersect` matchedspan
        where
          journalspan = journalDateSpan j'
          matchedspan = postingsDateSpan ps

      -- first implementation, probably inefficient
      spans               = dbg "1 " $ splitSpan (intervalFromOpts opts) reportspan
      psPerSpan           = dbg "3"  $ [filter (isPostingInDateSpan s) ps | s <- spans]
      acctnames           = dbg "4"  $ sort $ clip $ 
                            -- expandAccountNames $ 
                            accountNamesFromPostings ps
      allAcctsZeros       = dbg "5"  $ [(a, nullmixedamt) | a <- acctnames]
      someAcctBalsPerSpan = dbg "6"  $ [[(aname a, aibalance a) | a <- drop 1 $ accountsFromPostings ps, depthq `matchesAccount` aname a, aname a `elem` acctnames] | ps <- psPerSpan]
      balsPerSpan         = dbg "7"  $ [sortBy (comparing fst) $ unionBy (\(a,_) (a',_) -> a == a') acctbals allAcctsZeros | acctbals <- someAcctBalsPerSpan]
      balsPerAcct         = dbg "8"  $ transpose balsPerSpan
      acctsAndBals        = dbg "8.5" $ zip acctnames (map (map snd) balsPerAcct)
      items               = dbg "9"  $ [((a, a, accountNameLevel a), bs) | (a,bs) <- acctsAndBals, empty_ opts || any (not . isZeroMixedAmount) bs]
      highestLevelBalsPerSpan =
                            dbg "9.5" $ [[b | (a,b) <- spanbals, not $ any (`elem` acctnames) $ init $ expandAccountName a] | spanbals <- balsPerSpan]
      totals              = dbg "10" $ map sum highestLevelBalsPerSpan

-- | Generate a multi balance report for the matched accounts, showing
-- their cumulative or (with -H) historical balance in each of the specified periods.
-- Has the same limitations as periodBalanceReport.
cumulativeOrHistoricalBalanceReport :: ReportOpts -> Query -> Journal -> MultiBalanceReport
cumulativeOrHistoricalBalanceReport opts q j = MultiBalanceReport (periodbalancespans, items, totals)
    where
      -- select/adjust basic report dates
      (reportspan, _) = reportSpans opts q j

      -- rewrite query to use adjusted dates
      dateless  = filterQuery (not . queryIsDate)
      depthless = filterQuery (not . queryIsDepth)
      q' = dateless $ depthless q
      -- reportq = And [q', Date reportspan]

      -- get starting balances and accounts from preceding txns
      precedingq = And [q', Date $ DateSpan Nothing (spanStart reportspan)]
      (startbalanceitems,_) = balanceReport opts{flat_=True,empty_=True} precedingq j
      startacctbals = dbg "startacctbals"   $ map (\((a,_,_),b) -> (a,b)) startbalanceitems
      -- acctsWithStartingBalance = map fst $ filter (not . isZeroMixedAmount . snd) startacctbals
      startingBalanceFor a | balancetype_ opts == HistoricalBalance = fromMaybe nullmixedamt $ lookup a startacctbals
                           | otherwise = nullmixedamt

      -- get balance changes by period
      MultiBalanceReport (periodbalancespans,periodbalanceitems,_) = dbg "changes" $ periodBalanceReport opts q j
      balanceChangesByAcct = map (\((a,_,_),bs) -> (a,bs)) periodbalanceitems
      acctsWithBalanceChanges = map fst $ filter ((any (not . isZeroMixedAmount)) . snd) balanceChangesByAcct
      balanceChangesFor a = fromMaybe (error $ "no data for account: a") $ -- XXX
                            lookup a balanceChangesByAcct

      -- accounts to report on
      reportaccts -- = dbg' "reportaccts" $ (dbg' "acctsWithStartingBalance" acctsWithStartingBalance) `union` (dbg' "acctsWithBalanceChanges" acctsWithBalanceChanges)
                  = acctsWithBalanceChanges

      -- sum balance changes to get ending balances for each period
      endingBalancesFor a = 
          dbg "ending balances" $ drop 1 $ scanl (+) (startingBalanceFor a) $
          dbg "balance changes" $ balanceChangesFor a

      items  = dbg "items"  $ [((a,a,0), endingBalancesFor a) | a <- reportaccts]

      -- sum highest-level account balances in each column for column totals
      totals = dbg "totals" $ map sum highestlevelbalsbycol
          where
            highestlevelbalsbycol = transpose $ map endingBalancesFor highestlevelaccts
            highestlevelaccts =
                dbg "highestlevelaccts" $
                [a | a <- reportaccts, not $ any (`elem` reportaccts) $ init $ expandAccountName a]

      -- enable to debug just this function
      -- dbg :: Show a => String -> a -> a
      -- dbg = lstrace

