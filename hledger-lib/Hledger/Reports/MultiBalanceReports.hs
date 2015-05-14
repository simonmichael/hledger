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
import Safe
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
                                                ,MultiBalanceTotalsRow
                                                )

-- | A row in a multi balance report has
--
-- * An account name, with rendering hints
--
-- * A list of amounts to be shown in each of the report's columns.
--
-- * The total of the row amounts.
--
-- * The average of the row amounts.
type MultiBalanceReportRow = (RenderableAccountName, [MixedAmount], MixedAmount, MixedAmount)

type MultiBalanceTotalsRow = ([MixedAmount], MixedAmount, MixedAmount)

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
multiBalanceReport opts q j = MultiBalanceReport (displayspans, items, totalsrow)
    where
      symq       = dbg1 "symq"   $ filterQuery queryIsSym $ dbg1 "requested q" q
      depthq     = dbg1 "depthq" $ filterQuery queryIsDepth q
      depth      = queryDepth depthq
      depthless  = dbg1 "depthless" . filterQuery (not . queryIsDepth)
      datelessq  = dbg1 "datelessq"  $ filterQuery (not . queryIsDateOrDate2) q
      dateqcons  = if date2_ opts then Date2 else Date
      precedingq = dbg1 "precedingq" $ And [datelessq, dateqcons $ DateSpan Nothing (spanStart reportspan)]
      requestedspan  = dbg1 "requestedspan"  $ queryDateSpan (date2_ opts) q                              -- span specified by -b/-e/-p options and query args
      requestedspan' = dbg1 "requestedspan'" $ requestedspan `spanDefaultsFrom` journalDateSpan (date2_ opts) j  -- if open-ended, close it using the journal's end dates
      intervalspans  = dbg1 "intervalspans"  $ splitSpan (intervalFromOpts opts) requestedspan'           -- interval spans enclosing it
      reportspan     = dbg1 "reportspan"     $ DateSpan (maybe Nothing spanStart $ headMay intervalspans) -- the requested span enlarged to a whole number of intervals
                                                       (maybe Nothing spanEnd   $ lastMay intervalspans)
      newdatesq = dbg1 "newdateq" $ dateqcons reportspan
      reportq  = dbg1 "reportq" $ depthless $ And [datelessq, newdatesq] -- user's query enlarged to whole intervals and with no depth limit

      ps :: [Posting] =
          dbg1 "ps" $
          journalPostings $
          filterJournalAmounts symq $     -- remove amount parts excluded by cur:
          filterJournalPostings reportq $        -- remove postings not matched by (adjusted) query
          journalSelectingAmountFromOpts opts j

      displayspans = dbg1 "displayspans" $ splitSpan (intervalFromOpts opts) displayspan
        where
          displayspan
            | empty_ opts = dbg1 "displayspan (-E)" $ reportspan                                -- all the requested intervals
            | otherwise   = dbg1 "displayspan"      $ requestedspan `spanIntersect` matchedspan -- exclude leading/trailing empty intervals
          matchedspan = dbg1 "matchedspan" $ postingsDateSpan' (whichDateFromOpts opts) ps

      psPerSpan :: [[Posting]] =
          dbg1 "psPerSpan" $
          [filter (isPostingInDateSpan' (whichDateFromOpts opts) s) ps | s <- displayspans]

      postedAcctBalChangesPerSpan :: [[(ClippedAccountName, MixedAmount)]] =
          dbg1 "postedAcctBalChangesPerSpan" $
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

      postedAccts :: [AccountName] = dbg1 "postedAccts" $ sort $ accountNamesFromPostings ps

      -- starting balances and accounts from transactions before the report start date
      startacctbals = dbg1 "startacctbals" $ map (\((a,_,_),b) -> (a,b)) startbalanceitems
          where
            (startbalanceitems,_) = dbg1 "starting balance report" $ balanceReport opts' precedingq j
                                    where
                                      opts' | tree_ opts = opts{no_elide_=True}
                                            | otherwise  = opts{accountlistmode_=ALFlat}
      startingBalanceFor a = fromMaybe nullmixedamt $ lookup a startacctbals
      startAccts = dbg1 "startAccts" $ map fst startacctbals

      displayedAccts :: [ClippedAccountName] =
          dbg1 "displayedAccts" $
          (if tree_ opts then expandAccountNames else id) $
          nub $ map (clipOrEllipsifyAccountName depth) $
          if empty_ opts then nub $ sort $ startAccts ++ postedAccts else postedAccts

      acctBalChangesPerSpan :: [[(ClippedAccountName, MixedAmount)]] =
          dbg1 "acctBalChangesPerSpan" $
          [sortBy (comparing fst) $ unionBy (\(a,_) (a',_) -> a == a') postedacctbals zeroes
           | postedacctbals <- postedAcctBalChangesPerSpan]
          where zeroes = [(a, nullmixedamt) | a <- displayedAccts]

      acctBalChanges :: [(ClippedAccountName, [MixedAmount])] =
          dbg1 "acctBalChanges" $
          [(a, map snd abs) | abs@((a,_):_) <- transpose acctBalChangesPerSpan] -- never null, or used when null...

      items :: [MultiBalanceReportRow] =
          dbg1 "items" $
          [((a, accountLeafName a, accountNameLevel a), displayedBals, rowtot, rowavg)
           | (a,changes) <- acctBalChanges
           , let displayedBals = case balancetype_ opts of
                                  HistoricalBalance -> drop 1 $ scanl (+) (startingBalanceFor a) changes
                                  CumulativeBalance -> drop 1 $ scanl (+) nullmixedamt changes
                                  _                 -> changes
           , let rowtot = sum displayedBals
           , let rowavg = averageMixedAmounts displayedBals
           , empty_ opts || depth == 0 || any (not . isZeroMixedAmount) displayedBals
           ]

      totals :: [MixedAmount] =
          -- dbg1 "totals" $
          map sum balsbycol
          where
            balsbycol = transpose [bs | ((a,_,_),bs,_,_) <- items, not (tree_ opts) || a `elem` highestlevelaccts]
            highestlevelaccts     =
                dbg1 "highestlevelaccts" $
                [a | a <- displayedAccts, not $ any (`elem` displayedAccts) $ init $ expandAccountName a]

      totalsrow :: MultiBalanceTotalsRow =
          dbg1 "totalsrow" $
          (totals, sum totals, averageMixedAmounts totals)

      dbg1 s = let p = "multiBalanceReport" in Hledger.Utils.dbg1 (p++" "++s)  -- add prefix in this function's debug output
      -- dbg1 = const id  -- exclude this function from debug output

