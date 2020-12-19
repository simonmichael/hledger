{- |
-}

{-# LANGUAGE CPP                 #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module Hledger.Reports.BudgetReport (
  BudgetGoal,
  BudgetTotal,
  BudgetAverage,
  BudgetCell,
  BudgetReportRow,
  BudgetReport,
  budgetReport,
  budgetReportAsTable,
  budgetReportAsText,
  budgetReportAsCsv,
  -- * Helpers
  reportPeriodName,
  -- * Tests
  tests_BudgetReport
)
where

import Data.Decimal
import Data.Default (def)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.List
import Data.List.Extra (nubSort)
import Data.Maybe
#if !(MIN_VERSION_base(4,11,0))
import Data.Monoid ((<>))
#endif
import Safe
--import Data.List
--import Data.Maybe
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Text as T
--import qualified Data.Text.Lazy as TL
--import System.Console.CmdArgs.Explicit as C
--import Lucid as L

import Text.Printf (printf)
import Text.Tabular as T
import Text.Tabular.AsciiWide as T

import Hledger.Data
import Hledger.Utils
import Hledger.Read.CsvReader (CSV)
import Hledger.Reports.ReportOptions
import Hledger.Reports.ReportTypes
import Hledger.Reports.MultiBalanceReport


type BudgetGoal    = Change
type BudgetTotal   = Total
type BudgetAverage = Average

-- | A budget report tracks expected and actual changes per account and subperiod.
type BudgetCell = (Maybe Change, Maybe BudgetGoal)
type BudgetReportRow = PeriodicReportRow DisplayName BudgetCell
type BudgetReport    = PeriodicReport    DisplayName BudgetCell

type BudgetDisplayCell = ((String, Int), Maybe ((String, Int), Maybe (String, Int)))

-- | Calculate per-account, per-period budget (balance change) goals
-- from all periodic transactions, calculate actual balance changes 
-- from the regular transactions, and compare these to get a 'BudgetReport'.
-- Unbudgeted accounts may be hidden or renamed (see journalWithBudgetAccountNames).
budgetReport :: ReportSpec -> Bool -> DateSpan -> Journal -> BudgetReport
budgetReport rspec assrt reportspan j = dbg4 "sortedbudgetreport" budgetreport
  where
    -- Budget report demands ALTree mode to ensure subaccounts and subaccount budgets are properly handled
    -- and that reports with and without --empty make sense when compared side by side
    ropts = (rsOpts rspec){ accountlistmode_ = ALTree }
    showunbudgeted = empty_ ropts
    budgetedaccts =
      dbg3 "budgetedacctsinperiod" $
      nub $
      concatMap expandAccountName $
      accountNamesFromPostings $
      concatMap tpostings $
      concatMap (`runPeriodicTransaction` reportspan) $
      jperiodictxns j
    actualj = journalWithBudgetAccountNames budgetedaccts showunbudgeted j
    budgetj = journalAddBudgetGoalTransactions assrt ropts reportspan j
    actualreport@(PeriodicReport actualspans _ _) =
        dbg5 "actualreport" $ multiBalanceReport rspec{rsOpts=ropts{empty_=True}} actualj
    budgetgoalreport@(PeriodicReport _ budgetgoalitems budgetgoaltotals) =
        dbg5 "budgetgoalreport" $ multiBalanceReport rspec{rsOpts=ropts{empty_=True}} budgetj
    budgetgoalreport'
      -- If no interval is specified:
      -- budgetgoalreport's span might be shorter actualreport's due to periodic txns;
      -- it should be safe to replace it with the latter, so they combine well.
      | interval_ ropts == NoInterval = PeriodicReport actualspans budgetgoalitems budgetgoaltotals
      | otherwise = budgetgoalreport
    budgetreport = combineBudgetAndActual ropts j budgetgoalreport' actualreport

-- | Use all periodic transactions in the journal to generate
-- budget goal transactions in the specified date span.
-- Budget goal transactions are similar to forecast transactions except
-- their purpose and effect is to define balance change goals, per account and period,
-- for BudgetReport.
journalAddBudgetGoalTransactions :: Bool -> ReportOpts -> DateSpan -> Journal -> Journal
journalAddBudgetGoalTransactions assrt _ropts reportspan j =
  either error' id $ journalBalanceTransactions assrt j{ jtxns = budgetts }  -- PARTIAL:
  where
    budgetspan = dbg3 "budget span" $ reportspan
    budgetts =
      dbg5 "budget goal txns" $
      [makeBudgetTxn t
      | pt <- jperiodictxns j
      , t <- runPeriodicTransaction pt budgetspan
      ]
    makeBudgetTxn t = txnTieKnot $ t { tdescription = T.pack "Budget transaction" }

-- | Adjust a journal's account names for budget reporting, in two ways:
--
-- 1. accounts with no budget goal anywhere in their ancestry are moved
--    under the "unbudgeted" top level account.
--
-- 2. subaccounts with no budget goal are merged with their closest parent account
--    with a budget goal, so that only budgeted accounts are shown.
--    This can be disabled by -E/--empty.
--
journalWithBudgetAccountNames :: [AccountName] -> Bool -> Journal -> Journal
journalWithBudgetAccountNames budgetedaccts showunbudgeted j = 
  dbg5With (("budget account names: "++).pshow.journalAccountNamesUsed) $ 
  j { jtxns = remapTxn <$> jtxns j }
  where
    remapTxn = mapPostings (map remapPosting)
      where
        mapPostings f t = txnTieKnot $ t { tpostings = f $ tpostings t }
        remapPosting p = p { paccount = remapAccount $ paccount p, poriginal = Just . fromMaybe p $ poriginal p }
          where
            remapAccount a
              | hasbudget         = a
              | hasbudgetedparent = if showunbudgeted then a else budgetedparent
              | otherwise         = if showunbudgeted then u <> acctsep <> a else u
              where
                hasbudget = a `elem` budgetedaccts
                hasbudgetedparent = not $ T.null budgetedparent
                budgetedparent = headDef "" $ filter (`elem` budgetedaccts) $ parentAccountNames a
                u = unbudgetedAccountName

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
combineBudgetAndActual :: ReportOpts -> Journal -> MultiBalanceReport -> MultiBalanceReport -> BudgetReport
combineBudgetAndActual ropts j
      (PeriodicReport budgetperiods budgetrows (PeriodicReportRow _ budgettots budgetgrandtot budgetgrandavg))
      (PeriodicReport actualperiods actualrows (PeriodicReportRow _ actualtots actualgrandtot actualgrandavg)) =
    PeriodicReport periods sortedrows totalrow
  where
    periods = nubSort . filter (/= nulldatespan) $ budgetperiods ++ actualperiods

    -- first, combine any corresponding budget goals with actual changes
    rows1 =
      [ PeriodicReportRow acct amtandgoals totamtandgoal avgamtandgoal
      | PeriodicReportRow acct actualamts actualtot actualavg <- actualrows
      , let mbudgetgoals       = HM.lookup (displayFull acct) budgetGoalsByAcct :: Maybe ([BudgetGoal], BudgetTotal, BudgetAverage)
      , let budgetmamts        = maybe (Nothing <$ periods) (map Just . first3) mbudgetgoals :: [Maybe BudgetGoal]
      , let mbudgettot         = second3 <$> mbudgetgoals :: Maybe BudgetTotal
      , let mbudgetavg         = third3 <$> mbudgetgoals  :: Maybe BudgetAverage
      , let acctBudgetByPeriod = Map.fromList [ (p,budgetamt) | (p, Just budgetamt) <- zip budgetperiods budgetmamts ] :: Map DateSpan BudgetGoal
      , let acctActualByPeriod = Map.fromList [ (p,actualamt) | (p, Just actualamt) <- zip actualperiods (map Just actualamts) ] :: Map DateSpan Change
      , let amtandgoals        = [ (Map.lookup p acctActualByPeriod, Map.lookup p acctBudgetByPeriod) | p <- periods ] :: [BudgetCell]
      , let totamtandgoal      = (Just actualtot, mbudgettot)
      , let avgamtandgoal      = (Just actualavg, mbudgetavg)
      ]
      where
        budgetGoalsByAcct :: HashMap AccountName ([BudgetGoal], BudgetTotal, BudgetAverage) =
          HM.fromList [ (displayFull acct, (amts, tot, avg))
                         | PeriodicReportRow acct amts tot avg <- budgetrows ]

    -- next, make rows for budget goals with no actual changes
    rows2 =
      [ PeriodicReportRow acct amtandgoals totamtandgoal avgamtandgoal
      | PeriodicReportRow acct budgetgoals budgettot budgetavg <- budgetrows
      , displayFull acct `notElem` map prrFullName rows1
      , let acctBudgetByPeriod = Map.fromList $ zip budgetperiods budgetgoals :: Map DateSpan BudgetGoal
      , let amtandgoals        = [ (Nothing, Map.lookup p acctBudgetByPeriod) | p <- periods ] :: [BudgetCell]
      , let totamtandgoal      = (Nothing, Just budgettot)
      , let avgamtandgoal      = (Nothing, Just budgetavg)
      ]

    -- combine and re-sort rows
    -- TODO: add --sort-budget to sort by budget goal amount
    sortedrows :: [BudgetReportRow] = sortRowsLike (mbrsorted unbudgetedrows ++ mbrsorted rows') rows
      where
        (unbudgetedrows, rows') = partition ((==unbudgetedAccountName) . prrFullName) rows
        mbrsorted = map prrFullName . sortRows ropts j . map (fmap $ fromMaybe 0 . fst)
        rows = rows1 ++ rows2

    -- TODO: grand total & average shows 0% when there are no actual amounts, inconsistent with other cells
    totalrow = PeriodicReportRow ()
        [ (Map.lookup p totActualByPeriod, Map.lookup p totBudgetByPeriod) | p <- periods ]
        ( Just actualgrandtot, Just budgetgrandtot )
        ( Just actualgrandavg, Just budgetgrandavg )
      where
        totBudgetByPeriod = Map.fromList $ zip budgetperiods budgettots :: Map DateSpan BudgetTotal
        totActualByPeriod = Map.fromList $ zip actualperiods actualtots :: Map DateSpan Change

-- | Render a budget report as plain text suitable for console output.
budgetReportAsText :: ReportOpts -> BudgetReport -> String
budgetReportAsText ropts@ReportOpts{..} budgetr =
    title ++ "\n\n" ++
    renderTable def{tableBorders=False,prettyTable=pretty_tables_}
        (alignCell TopLeft) (alignCell TopRight) (uncurry showcell) displayTableWithWidths
  where
    title = printf "Budget performance in %s%s:"
      (showDateSpan $ periodicReportSpan budgetr)
      (case value_ of
        Just (AtCost _mc)   -> ", valued at cost"
        Just (AtThen _mc)   -> error' unsupportedValueThenError  -- PARTIAL:
        Just (AtEnd _mc)    -> ", valued at period ends"
        Just (AtNow _mc)    -> ", current value"
        Just (AtDate d _mc) -> ", valued at "++showDate d
        Nothing             -> "")

    displayTableWithWidths :: Table String String ((Int, Int, Int), BudgetDisplayCell)
    displayTableWithWidths = Table rh ch $ map (zipWith (,) widths) displaycells
    Table rh ch displaycells = case budgetReportAsTable ropts budgetr of
        Table rh' ch' vals -> maybetranspose . Table rh' ch' $ map (map displayCell) vals

    displayCell (actual, budget) = (showamt actual', budgetAndPerc <$> budget)
      where
        actual' = fromMaybe 0 actual
        budgetAndPerc b = (showamt b, showper <$> percentage actual' b)
        showamt = showMixedOneLine showAmountWithoutPrice Nothing (Just 32) color_
        showper p = let str = show (roundTo 0 p) in (str, length str)
    cellWidth ((_,wa), Nothing)                    = (wa,  0,  0)
    cellWidth ((_,wa), Just ((_,wb), Nothing))     = (wa, wb,  0)
    cellWidth ((_,wa), Just ((_,wb), Just (_,wp))) = (wa, wb, wp)

    widths = zip3 actualwidths budgetwidths percentwidths
    actualwidths  = map (maximum' . map (first3  . cellWidth)) cols
    budgetwidths  = map (maximum' . map (second3 . cellWidth)) cols
    percentwidths = map (maximum' . map (third3  . cellWidth)) cols
    cols = transpose displaycells

    -- XXX lay out actual, percentage and/or goal in the single table cell for now, should probably use separate cells
    showcell :: (Int, Int, Int) -> BudgetDisplayCell -> Cell
    showcell (actualwidth, budgetwidth, percentwidth) ((actual,wa), mbudget) =
        Cell TopRight [(replicate (actualwidth - wa) ' ' ++ actual ++ budgetstr, actualwidth + totalbudgetwidth)]
      where
        totalpercentwidth = if percentwidth == 0 then 0 else percentwidth + 5
        totalbudgetwidth  = if budgetwidth == 0 then 0 else budgetwidth + totalpercentwidth + 3
        budgetstr = case mbudget of
          Nothing                             -> replicate totalbudgetwidth ' '
          Just ((budget, wb), Nothing)        -> " [" ++ replicate totalpercentwidth ' ' ++ replicate (budgetwidth - wb) ' ' ++ budget ++ "]"
          Just ((budget, wb), Just (pct, wp)) -> " [" ++ replicate (percentwidth - wp) ' ' ++ pct ++ "% of " ++ replicate (budgetwidth - wb) ' ' ++ budget ++ "]"

    -- | Calculate the percentage of actual change to budget goal to show, if any.
    -- If valuing at cost, both amounts are converted to cost before comparing.
    -- A percentage will not be shown if:
    -- - actual or goal are not the same, single, commodity
    -- - the goal is zero
    percentage :: Change -> BudgetGoal -> Maybe Percentage
    percentage actual budget =
      case (maybecost $ normaliseMixedAmount actual, maybecost $ normaliseMixedAmount budget) of
        (Mixed [a], Mixed [b]) | (acommodity a == acommodity b || amountLooksZero a) && not (amountLooksZero b)
            -> Just $ 100 * aquantity a / aquantity b
        _   -> -- trace (pshow $ (maybecost actual, maybecost budget))  -- debug missing percentage
               Nothing
      where
        maybecost = case value_ of
            Just (AtCost _) -> mixedAmountCost
            _               -> id

    maybetranspose | transpose_ = \(Table rh ch vals) -> Table ch rh (transpose vals)
                   | otherwise  = id

-- | Build a 'Table' from a multi-column balance report.
budgetReportAsTable :: ReportOpts -> BudgetReport -> Table String String (Maybe MixedAmount, Maybe MixedAmount)
budgetReportAsTable
  ropts@ReportOpts{balancetype_}
  (PeriodicReport spans rows (PeriodicReportRow _ coltots grandtot grandavg)) =
    addtotalrow $
    Table
      (T.Group NoLine $ map Header accts)
      (T.Group NoLine $ map Header colheadings)
      (map rowvals rows)
  where
    colheadings = map (reportPeriodName balancetype_ spans) spans
                  ++ ["  Total" | row_total_ ropts]
                  ++ ["Average" | average_ ropts]

    accts = map renderacct rows
    -- FIXME. Have to check explicitly for which to render here, since
    -- budgetReport sets accountlistmode to ALTree. Find a principled way to do
    -- this.
    renderacct row = case accountlistmode_ ropts of
        ALTree -> replicate ((prrDepth row - 1)*2) ' ' ++ T.unpack (prrDisplayName row)
        ALFlat -> T.unpack . accountNameDrop (drop_ ropts) $ prrFullName row
    rowvals (PeriodicReportRow _ as rowtot rowavg) =
        as ++ [rowtot | row_total_ ropts] ++ [rowavg | average_ ropts]
    addtotalrow
      | no_total_ ropts = id
      | otherwise = (+----+ (row "" $
                       coltots ++ [grandtot | row_total_ ropts && not (null coltots)]
                               ++ [grandavg | average_ ropts && not (null coltots)]
                    ))

-- | Make a name for the given period in a multiperiod report, given
-- the type of balance being reported and the full set of report
-- periods. This will be used as a column heading (or row heading, in
-- a register summary report). We try to pick a useful name as follows:
--
-- - ending-balance reports: the period's end date
--
-- - balance change reports where the periods are months and all in the same year:
--   the short month name in the current locale
--
-- - all other balance change reports: a description of the datespan,
--   abbreviated to compact form if possible (see showDateSpan).
--
reportPeriodName :: BalanceType -> [DateSpan] -> DateSpan -> String
reportPeriodName balancetype spans =
  case balancetype of
    PeriodChange -> if multiyear then showDateSpan else showDateSpanMonthAbbrev
      where
        multiyear = (>1) $ length $ nubSort $ map spanStartYear spans
    _ -> maybe "" (showDate . prevday) . spanEnd

-- XXX generalise this with multiBalanceReportAsCsv ?
-- | Render a budget report as CSV. Like multiBalanceReportAsCsv,
-- but includes alternating actual and budget amount columns.
budgetReportAsCsv :: ReportOpts -> BudgetReport -> CSV
budgetReportAsCsv 
  ReportOpts{average_, row_total_, no_total_, transpose_}
  (PeriodicReport colspans items (PeriodicReportRow _ abtotals (magrandtot,mbgrandtot) (magrandavg,mbgrandavg)))
  = (if transpose_ then transpose else id) $

  -- heading row
  ("Account" : 
   concatMap (\span -> [showDateSpan span, "budget"]) colspans
   ++ concat [["Total"  ,"budget"] | row_total_]
   ++ concat [["Average","budget"] | average_]
  ) :

  -- account rows
  [T.unpack (displayFull a) :
   map showmamt (flattentuples abamts)
   ++ concat [[showmamt mactualrowtot, showmamt mbudgetrowtot] | row_total_]
   ++ concat [[showmamt mactualrowavg, showmamt mbudgetrowavg] | average_]
  | PeriodicReportRow a abamts (mactualrowtot,mbudgetrowtot) (mactualrowavg,mbudgetrowavg) <- items
  ]

  -- totals row
  ++ concat [
    [
    "Total:" :
    map showmamt (flattentuples abtotals)
    ++ concat [[showmamt magrandtot,showmamt mbgrandtot] | row_total_] 
    ++ concat [[showmamt magrandavg,showmamt mbgrandavg] | average_]
    ]
  | not no_total_
  ]

  where
    flattentuples abs = concat [[a,b] | (a,b) <- abs]
    showmamt = maybe "" (showMixedAmountOneLineWithoutPrice False)

-- tests

tests_BudgetReport = tests "BudgetReport" [
 ]
