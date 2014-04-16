{-# LANGUAGE RecordWildCards, DeriveDataTypeable, FlexibleInstances, ScopedTypeVariables #-}
{-|

Multi-column balance reports, used by the balance command.

-}

module Hledger.Reports.MultiBalanceReports (
  MultiBalanceReport(..),
  MultiBalanceReportRow,
  multiBalanceReport

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
-- The meaning of the amounts depends on the type of multi balance
-- report, of which there are three: periodic, cumulative and historical
-- (see 'BalanceType' and "Hledger.Cli.Balance").
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
    -- we add some bogus extra shows here to help ppShow parse the output
    -- and wrap tuples and lists properly
    show (MultiBalanceReport (spans, items, totals)) =
        "MultiBalanceReport (ignore extra quotes):\n" ++ ppShow (show spans, map show items, totals)

-- type alias just to remind us which AccountNames might be depth-clipped, below.
type ClippedAccountName = AccountName

-- | Generate a multicolumn balance report for the matched accounts,
-- showing the change of balance, accumulated balance, or historical balance
-- in each of the specified periods.
multiBalanceReport :: ReportOpts -> Query -> Journal -> MultiBalanceReport
multiBalanceReport opts q j = MultiBalanceReport (spans, items, totals)
    where
      -- dbg = const id                                   -- exclude from debug output
      dbg s = let p = "multiBalanceReport" in Hledger.Utils.dbg (p++" "++s)  -- add prefix in debug output

      symq       = dbg "symq"   $ filterQuery queryIsSym $ dbg "requested q" q
      depthq     = dbg "depthq" $ filterQuery queryIsDepth q
      depth      = queryDepth depthq
      depthless  = dbg "depthless" . filterQuery (not . queryIsDepth)
      datelessq  = dbg "datelessq"  $ filterQuery (not . queryIsDate) q
      precedingq = dbg "precedingq" $ And [datelessq, Date $ DateSpan Nothing (spanStart reportspan)]
      reportq    = dbg "reportq"    $ depthless q --  $ And [datelessq, Date reportspan] -- laziness at work -- XXX no good, works only in GHCI

      ps :: [Posting] =
          dbg "ps" $
          journalPostings $
          filterJournalPostingAmounts symq $     -- remove amount parts excluded by cur:
          filterJournalPostings reportq $        -- remove postings not matched by (adjusted) query
          journalSelectingAmountFromOpts opts j

      (reportspan, spans) = dbg "report spans" $ reportSpans opts q j ps

      psPerSpan :: [[Posting]] =
          dbg "psPerSpan" $
          [filter (isPostingInDateSpan' (whichDateFromOpts opts) s) ps | s <- spans]

      postedAcctBalChangesPerSpan :: [[(ClippedAccountName, MixedAmount)]] =
          dbg "postedAcctBalChangesPerSpan" $
          map postingAcctBals psPerSpan
          where
            postingAcctBals :: [Posting] -> [(ClippedAccountName, MixedAmount)]
            postingAcctBals ps = [(aname a, (if tree_ opts then aibalance else aebalance) a) | a <- as]
                where
                  as = depthLimit $ 
                       (if tree_ opts then id else filter ((>0).anumpostings)) $
                       drop 1 $ accountsFromPostings ps
                  depthLimit
                      | tree_ opts = filter ((depthq `matchesAccount`).aname) -- exclude deeper balances
                      | otherwise  = clipAccountsAndAggregate depth -- aggregate deeper balances at the depth limit

      postedAccts :: [AccountName] =
          dbg "postedAccts" $
          sort $ accountNamesFromPostings ps

      displayedAccts :: [ClippedAccountName] =
          dbg "displayedAccts" $
          (if tree_ opts then expandAccountNames else id) $
          nub $ map (clipAccountName depth) postedAccts

      acctBalChangesPerSpan :: [[(ClippedAccountName, MixedAmount)]] =
          dbg "acctBalChangesPerSpan" $
          [sortBy (comparing fst) $ unionBy (\(a,_) (a',_) -> a == a') postedacctbals zeroes
           | postedacctbals <- postedAcctBalChangesPerSpan]
          where zeroes = [(a, nullmixedamt) | a <- displayedAccts]

      acctBalChanges :: [(ClippedAccountName, [MixedAmount])] =
          dbg "acctBalChanges" $
          [(a, map snd abs) | abs@((a,_):_) <- transpose acctBalChangesPerSpan] -- never null, or used when null...

      -- starting balances and accounts from transactions before the report start date
      startacctbals = dbg "startacctbals" $ map (\((a,_,_),b) -> (a,b)) $ startbalanceitems
          where
            (startbalanceitems,_) = dbg "starting balance report" $ balanceReport opts' precedingq j
                                    where
                                      opts' | tree_ opts = opts{no_elide_=True}
                                            | otherwise  = opts{flat_=True}
      startingBalanceFor a = fromMaybe nullmixedamt $ lookup a startacctbals

      items :: [MultiBalanceReportRow] =
          dbg "items" $
          [((a, accountLeafName a, accountNameLevel a), displayedBals)
           | (a,changes) <- acctBalChanges
           , let displayedBals = case balancetype_ opts of
                                  HistoricalBalance -> drop 1 $ scanl (+) (startingBalanceFor a) changes
                                  CumulativeBalance -> drop 1 $ scanl (+) nullmixedamt changes
                                  _                 -> changes
           , empty_ opts || any (not . isZeroMixedAmount) displayedBals
           ]

      totals :: [MixedAmount] =
          dbg "totals" $
          map sum balsbycol
          where
            balsbycol = transpose [bs | ((a,_,_),bs) <- items, not (tree_ opts) || a `elem` highestlevelaccts]
            highestlevelaccts     =
                dbg "highestlevelaccts" $
                [a | a <- displayedAccts, not $ any (`elem` displayedAccts) $ init $ expandAccountName a]
