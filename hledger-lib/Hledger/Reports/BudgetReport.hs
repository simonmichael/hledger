{- |
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Hledger.Reports.BudgetReport
where

import Data.Decimal
import Data.List
import Data.Maybe
#if !(MIN_VERSION_base(4,11,0))
import Data.Monoid ((<>))
#endif
import Data.Ord
import Data.Time.Calendar
--import Safe
import Test.HUnit
--import Data.List
--import Data.Maybe
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Text as T
--import qualified Data.Text.Lazy as TL
--import System.Console.CmdArgs.Explicit as C
--import Lucid as L
--import Text.CSV
--import Test.HUnit
import Text.Printf (printf)
import Text.Tabular as T
--import Text.Tabular.AsciiWide

import Hledger.Data
--import Hledger.Query
import Hledger.Utils
--import Hledger.Read (mamountp')
import Hledger.Reports.ReportOptions
import Hledger.Reports.ReportTypes
import Hledger.Reports.MultiBalanceReports


--type MultiBalanceReportRow    = (AccountName, AccountName, Int, [MixedAmount], MixedAmount, MixedAmount)
--type MultiBalanceReportTotals = ([MixedAmount], MixedAmount, MixedAmount) -- (Totals list, sum of totals, average of totals)

--type PeriodicReportRow a =
--  ( AccountName  -- ^ A full account name.
--  , [a]          -- ^ The data value for each subperiod.
--  , a            -- ^ The total of this row's values.
--  , a            -- ^ The average of this row's values.
--  )

type BudgetGoal    = Change
type BudgetTotal   = Total
type BudgetAverage = Average

-- | A budget report tracks expected and actual changes per account and subperiod.
type BudgetReport = PeriodicReport (Maybe Change, Maybe BudgetGoal)

-- | Calculate budget goals from periodic transactions with the specified report interval,
-- calculate actual inflows/outflows from the regular transactions (adjusted to match the
-- budget goals' account tree), and return both as a 'BudgetReport'.
budgetReport :: ReportOpts -> Bool -> Bool -> DateSpan -> Day -> Journal -> BudgetReport
budgetReport ropts assrt showunbudgeted reportspan d j =
  let
    budgetj          = budgetJournal assrt ropts reportspan j
    actualj          = budgetRollUp showunbudgeted budgetj j
    q                = queryFromOpts d ropts
    budgetgoalreport = dbg1 "budgetgoalreport" $ multiBalanceReport ropts q budgetj
    actualreport     = dbg1 "actualreport"     $ multiBalanceReport ropts q actualj
  in
    dbg1 "budgetreport" $ 
    combineBudgetAndActual budgetgoalreport actualreport

-- | Use all periodic transactions in the journal to generate 
-- budget transactions in the specified report period.
-- Budget transactions are similar to forecast transactions except
-- their purpose is to set goal amounts (of change) per account and period.
budgetJournal :: Bool -> ReportOpts -> DateSpan -> Journal -> Journal
budgetJournal assrt _ropts reportspan j =
  either error' id $ journalBalanceTransactions assrt j{ jtxns = budgetts }
  where
    budgetspan = dbg2 "budgetspan" $ reportspan
    budgetts =
      dbg1 "budgetts" $
      [makeBudgetTxn t
      | pt <- jperiodictxns j
      , t <- runPeriodicTransaction pt budgetspan
      ]
    makeBudgetTxn t = txnTieKnot $ t { tdescription = T.pack "Budget transaction" }

-- | Re-map account names to closest parent with periodic transaction from budget.
-- Accounts that don't have suitable parent are either remapped to "<unbudgeted>:topAccount"
-- or left as-is if --show-unbudgeted is provided.
budgetRollUp :: Bool -> Journal -> Journal -> Journal
budgetRollUp showunbudgeted budget j = j { jtxns = remapTxn <$> jtxns j }
    where
        budgetAccounts = nub $ concatMap (map paccount . ptpostings) $ jperiodictxns budget
        remapAccount origAcctName = remapAccount' origAcctName
          where
            remapAccount' acctName
              | acctName `elem` budgetAccounts = acctName
              | otherwise =
                case parentAccountName acctName of
                  "" | showunbudgeted -> origAcctName
                     | otherwise      -> unbudgetedAccount <> acctsep <> acctName
                  parent -> remapAccount' parent
        remapPosting p = p { paccount = remapAccount $ paccount p, porigin = Just . fromMaybe p $ porigin p }
        remapTxn = mapPostings (map remapPosting)
        mapPostings f t = txnTieKnot $ t { tpostings = f $ tpostings t }

-- | Combine a per-account-and-subperiod report of budget goals, and one
-- of actual change amounts, into a budget performance report.
-- The two reports should have the same report interval, but need not
-- have exactly the same account rows or date columns.
-- (Cells in the combined budget report can be missing a budget goal,
-- an actual amount, or both.) The combined report will include:
--
-- - consecutive subperiods at the same interval as the two reports,
--   spanning the period of both reports
--
-- - all accounts mentioned in either report, sorted by account code or
--   account name or amount as appropriate.
--
combineBudgetAndActual :: MultiBalanceReport -> MultiBalanceReport -> BudgetReport
combineBudgetAndActual
  (MultiBalanceReport (budgetperiods, budgetrows, (budgettots, budgetgrandtot, budgetgrandavg)))
  (MultiBalanceReport (actualperiods, actualrows, (actualtots, actualgrandtot, actualgrandavg))) =
  let
    periods = nub $ sort $ filter (/= nulldatespan) $ budgetperiods ++ actualperiods

    -- first, combine any corresponding budget goals with actual changes
    rows1 =
      [ (acct, treeacct, treeindent, amtandgoals, totamtandgoal, avgamtandgoal)
      | (acct, treeacct, treeindent, actualamts, actualtot, actualavg) <- actualrows
      , let mbudgetgoals       = Map.lookup acct budgetGoalsByAcct :: Maybe ([BudgetGoal], BudgetTotal, BudgetAverage)
      , let budgetmamts        = maybe (replicate (length periods) Nothing) (map Just . first3) mbudgetgoals :: [Maybe BudgetGoal]
      , let mbudgettot         = maybe Nothing (Just . second3) mbudgetgoals :: Maybe BudgetTotal
      , let mbudgetavg         = maybe Nothing (Just . third3)  mbudgetgoals :: Maybe BudgetAverage
      , let acctBudgetByPeriod = Map.fromList [ (p,budgetamt) | (p, Just budgetamt) <- zip budgetperiods budgetmamts ] :: Map DateSpan BudgetGoal
      , let acctActualByPeriod = Map.fromList [ (p,actualamt) | (p, Just actualamt) <- zip actualperiods (map Just actualamts) ] :: Map DateSpan Change
      , let amtandgoals        = [ (Map.lookup p acctActualByPeriod, Map.lookup p acctBudgetByPeriod) | p <- periods ] :: [(Maybe Change, Maybe BudgetGoal)]
      , let totamtandgoal      = (Just actualtot, mbudgettot)
      , let avgamtandgoal      = (Just actualavg, mbudgetavg)
      ]
      where
        budgetGoalsByAcct :: Map AccountName ([BudgetGoal], BudgetTotal, BudgetAverage) =
          Map.fromList [ (acct, (amts, tot, avg)) | (acct, _, _, amts, tot, avg) <- budgetrows ]

    -- next, make rows for budget goals with no actual changes
    rows2 =
      [ (acct, treeacct, treeindent, amtandgoals, totamtandgoal, avgamtandgoal)
      | (acct, treeacct, treeindent, budgetgoals, budgettot, budgetavg) <- budgetrows
      , not $ acct `elem` acctsdone
      , let acctBudgetByPeriod = Map.fromList $ zip budgetperiods budgetgoals :: Map DateSpan BudgetGoal
      , let amtandgoals        = [ (Nothing, Map.lookup p acctBudgetByPeriod) | p <- periods ] :: [(Maybe Change, Maybe BudgetGoal)]
      , let totamtandgoal      = (Nothing, Just budgettot)
      , let avgamtandgoal      = (Nothing, Just budgetavg)
      ]
      where
        acctsdone = map first6 rows1

    -- combine and re-sort rows
    -- TODO: respect hierarchy in tree mode
    -- TODO: respect --sort-amount
    -- TODO: add --sort-budget
    rows :: [PeriodicReportRow (Maybe Change, Maybe BudgetGoal)] =
      sortBy (comparing first6) $ rows1 ++ rows2
-- massive duplication from multiBalanceReport to handle tree mode sorting ?
--      dbg1 "sorteditems" $
--      sortitems items
--      where
--        sortitems
--          | sort_amount_ opts && accountlistmode_ opts == ALTree       = sortTreeMultiBalanceReportRowsByAmount
--          | sort_amount_ opts                                          = sortFlatMultiBalanceReportRowsByAmount
--          | not (sort_amount_ opts) && accountlistmode_ opts == ALTree = sortTreeMultiBalanceReportRowsByAccountCodeAndName
--          | otherwise                                                  = sortFlatMultiBalanceReportRowsByAccountCodeAndName
--          where
--            -- Sort the report rows, representing a flat account list, by row total.
--            sortFlatMultiBalanceReportRowsByAmount = sortBy (maybeflip $ comparing fifth6)
--              where
--                maybeflip = if normalbalance_ opts == Just NormallyNegative then id else flip
--
--            -- Sort the report rows, representing a tree of accounts, by row total at each level.
--            -- To do this we recreate an Account tree with the row totals as balances,
--            -- so we can do a hierarchical sort, flatten again, and then reorder the
--            -- report rows similarly. Yes this is pretty long winded.
--            sortTreeMultiBalanceReportRowsByAmount rows = sortedrows
--              where
--                anamesandrows = [(first6 r, r) | r <- rows]
--                anames = map fst anamesandrows
--                atotals = [(a,tot) | (a,_,_,_,tot,_) <- rows]
--                nametree = treeFromPaths $ map expandAccountName anames
--                accounttree = nameTreeToAccount "root" nametree
--                accounttreewithbals = mapAccounts setibalance accounttree
--                  where
--                    -- this error should not happen, but it's ugly TODO
--                    setibalance a = a{aibalance=fromMaybe (error "sortTreeMultiBalanceReportRowsByAmount 1") $ lookup (aname a) atotals}
--                sortedaccounttree = sortAccountTreeByAmount (fromMaybe NormallyPositive $ normalbalance_ opts) accounttreewithbals
--                sortedaccounts = drop 1 $ flattenAccounts sortedaccounttree
--                -- dropped the root account, also ignore any parent accounts not in rows
--                sortedrows = concatMap (\a -> maybe [] (:[]) $ lookup (aname a) anamesandrows) sortedaccounts
--
--            -- Sort the report rows by account code if any, with the empty account code coming last, then account name.
--            sortFlatMultiBalanceReportRowsByAccountCodeAndName = sortBy (comparing acodeandname)
--              where
--                acodeandname r = (acode', aname)
--                  where
--                    aname = first6 r
--                    macode = fromMaybe Nothing $ lookup aname $ jaccounts j
--                    acode' = fromMaybe maxBound macode
--
--            -- Sort the report rows, representing a tree of accounts, by account code and then account name at each level.
--            -- Convert a tree of account names, look up the account codes, sort and flatten the tree, reorder the rows.
--            sortTreeMultiBalanceReportRowsByAccountCodeAndName rows = sortedrows
--              where
--                anamesandrows = [(first6 r, r) | r <- rows]
--                anames = map fst anamesandrows
--                nametree = treeFromPaths $ map expandAccountName anames
--                accounttree = nameTreeToAccount "root" nametree
--                accounttreewithcodes = mapAccounts (accountSetCodeFrom j) accounttree
--                sortedaccounttree = sortAccountTreeByAccountCodeAndName accounttreewithcodes
--                sortedaccounts = drop 1 $ flattenAccounts sortedaccounttree
--                -- dropped the root account, also ignore any parent accounts not in rows
--                sortedrows = concatMap (\a -> maybe [] (:[]) $ lookup (aname a) anamesandrows) sortedaccounts
--

    -- TODO: grand total & average shows 0% when there are no actual amounts, inconsistent with other cells
    totalrow =
      ( ""
      , ""
      , 0
      , [ (Map.lookup p totActualByPeriod, Map.lookup p totBudgetByPeriod) | p <- periods ] :: [(Maybe Total, Maybe BudgetTotal)]
      , ( Just actualgrandtot, Just budgetgrandtot ) :: (Maybe Total, Maybe BudgetTotal)
      , ( Just actualgrandavg, Just budgetgrandavg ) :: (Maybe Total, Maybe BudgetTotal)
      )
      where
        totBudgetByPeriod = Map.fromList $ zip budgetperiods budgettots :: Map DateSpan BudgetTotal
        totActualByPeriod = Map.fromList $ zip actualperiods actualtots :: Map DateSpan Change

  in
    PeriodicReport
      ( periods
      , rows
      , totalrow
      )

-- | Figure out the overall period of a BudgetReport.
budgetReportSpan :: BudgetReport -> DateSpan
budgetReportSpan (PeriodicReport ([], _, _))    = DateSpan Nothing Nothing
budgetReportSpan (PeriodicReport (spans, _, _)) = DateSpan (spanStart $ head spans) (spanEnd $ last spans)

-- | Render a budget report as plain text suitable for console output.
budgetReportAsText :: ReportOpts -> BudgetReport -> String
budgetReportAsText ropts budgetr =
  printf "Budget performance in %s:\n\n" (showDateSpan $ budgetReportSpan budgetr)
  ++ 
  tableAsText ropts showcell (budgetReportAsTable ropts budgetr)
  where
    showcell :: (Maybe Change, Maybe BudgetGoal) -> String
    showcell (mactual, mbudget) = actualstr ++ " " ++ budgetstr
      where
        actualwidth  = 7
        percentwidth = 4
        budgetwidth  = 5
        actualstr = printf ("%"++show actualwidth++"s") (maybe "0" showamt mactual)
        budgetstr = case (mactual, mbudget) of
          (_,       Nothing)     -> replicate (percentwidth + 7 + budgetwidth) ' '
          (mactual, Just budget) ->
            case percentage mactual budget of
              Just pct ->
                printf ("[%"++show percentwidth++"s%% of %"++show budgetwidth++"s]")
                       (show $ roundTo 0 pct) (showbudgetamt budget)
              Nothing ->
                printf ("["++replicate (percentwidth+5) ' '++"%"++show budgetwidth++"s]")
                       (showbudgetamt budget)

    percentage :: Maybe Change -> BudgetGoal -> Maybe Percentage
    percentage Nothing _ = Nothing
    percentage (Just actual) budget =
      -- percentage of budget consumed is always computed in the cost basis
      case (toCost actual, toCost budget) of
        (Mixed [a1], Mixed [a2])
          | isReallyZeroAmount a1 -> Just 0 -- if there are no postings, we consumed 0% of budget
          | acommodity a1 == acommodity a2 && aquantity a2 /= 0 ->
            Just $ 100 * aquantity a1 / aquantity a2
        _ -> Nothing
        where
          toCost = normaliseMixedAmount . costOfMixedAmount

    showamt :: MixedAmount -> String
    showamt | color_ ropts  = cshowMixedAmountOneLineWithoutPrice
            | otherwise     = showMixedAmountOneLineWithoutPrice

    -- don't show the budget amount in color, it messes up alignment
    showbudgetamt = showMixedAmountOneLineWithoutPrice

-- | Build a 'Table' from a multi-column balance report.
budgetReportAsTable :: ReportOpts -> BudgetReport -> Table String String (Maybe MixedAmount, Maybe MixedAmount)
budgetReportAsTable 
  ropts 
  (PeriodicReport
    ( periods
    , rows
    , (_, _, _, coltots, grandtot, grandavg)
    )) =
    addtotalrow $ 
    Table
      (T.Group NoLine $ map Header accts)
      (T.Group NoLine $ map Header colheadings)
      (map rowvals rows)
  where
    colheadings = map showDateSpanMonthAbbrev periods
                  ++ (if row_total_ ropts then ["  Total"] else [])
                  ++ (if average_   ropts then ["Average"] else [])
    accts = map renderacct rows
    renderacct (a,a',i,_,_,_)
      | tree_ ropts = replicate ((i-1)*2) ' ' ++ T.unpack a'
      | otherwise   = T.unpack $ maybeAccountNameDrop ropts a
    rowvals (_,_,_,as,rowtot,rowavg) = as
                                       ++ (if row_total_ ropts then [rowtot] else [])
                                       ++ (if average_   ropts then [rowavg] else [])
    addtotalrow | no_total_ ropts = id
                | otherwise       = (+----+ (row "" $
                                     coltots
                                     ++ (if row_total_ ropts && not (null coltots) then [grandtot] else [])
                                     ++ (if average_   ropts && not (null coltots) then [grandavg] else [])
                                     ))

-- XXX here for now
-- | Drop leading components of accounts names as specified by --drop, but only in --flat mode.
maybeAccountNameDrop :: ReportOpts -> AccountName -> AccountName
maybeAccountNameDrop opts a | tree_ opts = a
                            | otherwise  = accountNameDrop (drop_ opts) a

tests_Hledger_Reports_BudgetReport :: Test
tests_Hledger_Reports_BudgetReport = TestList [
  ]
