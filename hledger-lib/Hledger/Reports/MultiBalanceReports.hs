{-# LANGUAGE RecordWildCards, DeriveDataTypeable, FlexibleInstances, ScopedTypeVariables #-}
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
    -- we add some bogus extra shows here to help ppShow parse the output
    -- and wrap tuples and lists properly
    show (MultiBalanceReport (spans, items, totals)) =
        "MultiBalanceReport (ignore extra quotes):\n" ++ ppShow (show spans, map show items, totals)

-- type alias just to remind us which AccountNames might be depth-clipped, below.
type ClippedAccountName = AccountName

-- | Generate a multi balance report for the matched accounts, showing
-- their change of balance in each of the specified periods.
-- Currently has some limitations compared to the simple balance report,
-- eg always displays accounts in --flat mode.
periodBalanceReport :: ReportOpts -> Query -> Journal -> MultiBalanceReport
periodBalanceReport opts q j = MultiBalanceReport (spans, items, totals)
    where
      -- dbg = const id                                   -- exclude from debug output
      dbg s = let p = "periodBalanceReport" in Hledger.Utils.dbg (p++" "++s)  -- add prefix in debug output

      -- Example data below is from
      -- hledger -f data/balance-multicol.journal balance -p 'monthly2013' assets: --depth=1 --debug=1
      -- with flatShowsExclusiveBalance=True. To see more, run other commands from
      -- tests/balance-multicol.test with --debug=1.

      nodepthq = dbg "nodepthq" $ filterQuery (not . queryIsDepth) q
                 -- And ([Date (DateSpan (Just 2013-01-01) (Just 2014-01-01)),Acct "assets:"])
      depthq   = dbg "depthq"   $ filterQuery queryIsDepth q
                 -- Any
      depth    = queryDepth depthq
                 -- Depth 1
      symq     = dbg "symq"     $ filterQuery queryIsSym q
                 -- Any

      ps :: [Posting] =
           dbg "ps" $
           journalPostings $
           filterJournalPostingAmounts symq $     -- exclude amount parts excluded by cur:
           filterJournalPostings nodepthq $       -- exclude unmatched postings, but include all depths
           journalSelectingAmountFromOpts opts j
           -- 
           -- [(assets:checking)                 1
           -- ,(assets:checking)                -1
           -- ,(assets:cash)                     1
           -- ,(assets:checking)                 1
           -- ]

      -- the report's span will be the requested span intersected with
      -- the selected data's span; or with -E, the requested span
      -- limited by the journal's overall span.
      reportspan | empty_ opts = requestedspan `orDatesFrom` journalspan
                 | otherwise   = requestedspan `spanIntersect` matchedspan
        where
          requestedspan = queryDateSpan (date2_ opts) q -- based on -b/-e/-p opts and query args IIRC
          journalspan   = journalDateSpan j
          matchedspan   = postingsDateSpan ps

      spans :: [DateSpan] =
          dbg "spans" $
          splitSpan (intervalFromOpts opts) reportspan
          -- [DateSpan (Just 2013-01-01) (Just 2013-02-01)
          -- ,DateSpan (Just 2013-02-01) (Just 2013-03-01)
          -- ,DateSpan (Just 2013-03-01) (Just 2013-04-01)
          -- ]

      psBySpan :: [[Posting]] =
          dbg "psBySpan" $
          [filter (isPostingInDateSpan s) ps | s <- spans]
          -- [[(assets:checking)                 1, (assets:checking)                -1]
          -- ,[(assets:cash)                     1]
          -- ,[(assets:checking)                 1]

      postedAccts :: [AccountName] =
          dbg "postedAccts" $
          sort $ accountNamesFromPostings ps
          -- [ "assets:cash" , "assets:checking" ]

      displayedAccts :: [ClippedAccountName] =
          dbg "displayedAccts" $
          nub $ map (clipAccountName depth) postedAccts
          -- [ "assets" ]

      zeroes :: [(ClippedAccountName, MixedAmount)] =
          dbg "zeroes" $
          [(a, nullmixedamt) | a <- displayedAccts]
          -- [ ( "assets" , 0 ) ]

      postedAcctBalsBySpan :: [[(ClippedAccountName, MixedAmount)]] =
          dbg "postedAcctBalsBySpan" $
          [postingAcctBals ps | ps <- psBySpan]
          where
            postingAcctBals :: [Posting] -> [(ClippedAccountName, MixedAmount)]
            postingAcctBals ps = [(aname a, (if flatShowsExclusiveBalance then aebalance else aibalance) a) | a <- as]
                where
                  as = depthLimit $ 
                       filter ((>0).anumpostings) $
                       drop 1 $ accountsFromPostings ps
                  depthLimit
                      | flatShowsExclusiveBalance = clipAccountsAndAggregate depth -- aggregate deeper balances at the depth limit
                      | otherwise                 = filter ((depthq `matchesAccount`).aname) -- exclude deeper balances
          -- [ [ ( "assets" , 0 ) ]
          -- , [ ( "assets" , 1 ) ]
          -- , [ ( "assets" , 1 ) ]
          -- ]

      displayedBalsBySpan :: [[(ClippedAccountName, MixedAmount)]] =
          dbg "displayedBalsBySpan" $
          [sortBy (comparing fst) $ unionBy (\(a,_) (a',_) -> a == a') postedacctbals zeroes
           | postedacctbals <- postedAcctBalsBySpan]
          --
          -- [ [ ( "assets" , 0 ) ]
          -- , [ ( "assets" , 1 ) ]
          -- , [ ( "assets" , 1 ) ]
          -- ]

      displayedBalsByAcct :: [[(ClippedAccountName, MixedAmount)]] =
          dbg "displayedBalsByAcct" $
          transpose displayedBalsBySpan
          -- [ [ ( "assets" , 0 ) , ( "assets" , 1 ) , ( "assets" , 1 ) ] ]

      acctBalsAlist :: [(ClippedAccountName, [MixedAmount])] =
          dbg "acctBalsAlist" $
          zip displayedAccts (map (map snd) [bs | bs <- displayedBalsByAcct
                                             -- , maybe False ((`elem` postedAccts).fst) $ headMay bs
                                            ])
          -- [ ( "assets" , [ 0 , 1 , 1 ] ) ]

      items :: [MultiBalanceReportRow] =
          dbg "items" $
          [((a, a, accountNameLevel a), bs) | (a,bs) <- acctBalsAlist, empty_ opts || any (not . isZeroMixedAmount) bs]
          -- [ ( ( "assets" , "assets" , 1 ) , [ 0 , 1 , 1 ] ) ]

      -- highestLevelBalsBySpan :: [[MixedAmount]] =
      --     dbg "highestLevelBalsBySpan" $
      --     [[b | (a,b) <- spanbals, not $ any (`elem` postedAccts) $ init $ expandAccountName a] | spanbals <- displayedBalsBySpan]

      totals :: [MixedAmount] =
          dbg "totals" $
          if flatShowsExclusiveBalance
          then map (sum . map snd) displayedBalsBySpan
          else map (sum . map pamount) psBySpan
          -- else map sum highestLevelBalsBySpan
          -- [ 0 , 1 , 1 ]

-- | Generate a multi balance report for the matched accounts, showing
-- their cumulative or (with -H) historical balance in each of the specified periods.
-- Has the same limitations as periodBalanceReport.
cumulativeOrHistoricalBalanceReport :: ReportOpts -> Query -> Journal -> MultiBalanceReport
cumulativeOrHistoricalBalanceReport opts q j = MultiBalanceReport (periodbalancespans, items, totals)
    where
      -- dbg = const id                                   -- exclude from debug output
      dbg s = let p = "cumulativeOrHistoricalBalanceReport" in Hledger.Utils.dbg (p++" "++s)  -- add prefix in debug output

      -- select/adjust basic report dates
      (reportspan, _) = dbg "report spans" $ reportSpans opts q j

      -- starting balances and accounts from transactions before the report start date
      startacctbals = dbg "startacctbals" $ map (\((a,_,_),b) -> (a,b)) $ startbalanceitems
          where
            dateless              = filterQuery (not . queryIsDate)
            precedingq            = dbg "precedingq" $ And [dateless q, Date $ DateSpan Nothing (spanStart reportspan)]
            (startbalanceitems,_) = dbg "starting balance report" $ balanceReport opts{flat_=True,empty_=True} precedingq j
      -- acctsWithStartingBalance = map fst $ filter (not . isZeroMixedAmount . snd) startacctbals
      startingBalanceFor a
          | balancetype_ opts == HistoricalBalance = fromMaybe nullmixedamt $ lookup a startacctbals
          | otherwise                              = nullmixedamt

      -- balance changes in each period for each account
      MultiBalanceReport (periodbalancespans,periodbalanceitems,_) = dbg "balance changes report" $ periodBalanceReport opts q j
      balanceChangesByAcct    = map (\((a,_,_),bs) -> (a,bs)) periodbalanceitems
      acctsWithBalanceChanges = map fst $ filter ((any (not . isZeroMixedAmount)) . snd) balanceChangesByAcct
      balanceChangesFor a     = fromMaybe (error $ "no data for account: a") $ -- XXX
                                lookup a balanceChangesByAcct

      -- accounts to report on
      reportaccts = dbg "reportaccts"
                    acctsWithBalanceChanges
                    -- (dbg' "acctsWithStartingBalance" acctsWithStartingBalance) `union` (dbg' "acctsWithBalanceChanges" acctsWithBalanceChanges)

      -- ending balances in each period (starting balance plus balance changes) for an account
      endingBalancesFor a = 
          dbg "ending balances" $ drop 1 $ scanl (+) (startingBalanceFor a) $
          dbg ("balance changes for "++a) $ balanceChangesFor a

      items  = dbg "items"  $ [((a,a,0), endingBalancesFor a) | a <- reportaccts]

      totals = dbg "totals" $ 
          if flatShowsExclusiveBalance
          then map sum balsbycol
          else map sum highestlevelbalsbycol
          where
            balsbycol             = transpose $ map endingBalancesFor reportaccts
            highestlevelbalsbycol = transpose $ map endingBalancesFor highestlevelaccts
            highestlevelaccts     =
                dbg "highestlevelaccts" $
                [a | a <- reportaccts, not $ any (`elem` reportaccts) $ init $ expandAccountName a]

