{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
  combineBudgetAndActual,
  -- * Tests
  tests_BudgetReport
)
where

import Control.Applicative ((<|>))
import Control.Arrow ((***))
import Data.Decimal (roundTo)
import Data.Function (on)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.List (find, partition, transpose, foldl')
import Data.List.Extra (nubSort)
import Data.Maybe (fromMaybe, catMaybes)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TB
--import System.Console.CmdArgs.Explicit as C
--import Lucid as L
import Text.Tabular.AsciiWide as Tab

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

type BudgetDisplayCell = (WideBuilder, Maybe (WideBuilder, Maybe WideBuilder))
type BudgetDisplayRow  = [BudgetDisplayCell]
type BudgetShowMixed   = MixedAmount -> [WideBuilder]
type BudgetPercBudget  = Change -> BudgetGoal -> [Maybe Percentage]

-- | Calculate per-account, per-period budget (balance change) goals
-- from all periodic transactions, calculate actual balance changes
-- from the regular transactions, and compare these to get a 'BudgetReport'.
-- Unbudgeted accounts may be hidden or renamed (see journalWithBudgetAccountNames).
budgetReport :: ReportSpec -> BalancingOpts -> DateSpan -> Journal -> BudgetReport
budgetReport rspec bopts reportspan j = dbg4 "sortedbudgetreport" budgetreport
  where
    -- Budget report demands ALTree mode to ensure subaccounts and subaccount budgets are properly handled
    -- and that reports with and without --empty make sense when compared side by side
    ropts = (_rsReportOpts rspec){ accountlistmode_ = ALTree }
    showunbudgeted = empty_ ropts
    budgetedaccts =
      dbg3 "budgetedacctsinperiod" $
      S.fromList $
      expandAccountNames $
      accountNamesFromPostings $
      concatMap tpostings $
      concatMap (`runPeriodicTransaction` reportspan) $
      jperiodictxns j
    actualj = journalWithBudgetAccountNames budgetedaccts showunbudgeted j
    budgetj = journalAddBudgetGoalTransactions bopts ropts reportspan j
    actualreport@(PeriodicReport actualspans _ _) =
        dbg5 "actualreport" $ multiBalanceReport rspec{_rsReportOpts=ropts{empty_=True}} actualj
    budgetgoalreport@(PeriodicReport _ budgetgoalitems budgetgoaltotals) =
        dbg5 "budgetgoalreport" $ multiBalanceReport rspec{_rsReportOpts=ropts{empty_=True}} budgetj
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
journalAddBudgetGoalTransactions :: BalancingOpts -> ReportOpts -> DateSpan -> Journal -> Journal
journalAddBudgetGoalTransactions bopts ropts reportspan j =
  either error' id $ journalBalanceTransactions bopts j{ jtxns = budgetts }  -- PARTIAL:
  where
    budgetspan = dbg3 "budget span" $ reportspan
    pat = fromMaybe "" $ dbg3 "budget pattern" $ T.toLower <$> budgetpat_ ropts
    -- select periodic transactions matching a pattern
    -- (the argument of the (final) --budget option).
    -- XXX two limitations/wishes, requiring more extensive type changes:
    -- - give an error if pat is non-null and matches no periodic txns
    -- - allow a regexp or a full hledger query, not just a substring
    budgetts =
      dbg5 "budget goal txns" $
      [makeBudgetTxn t
      | pt <- jperiodictxns j
      , pat `T.isInfixOf` T.toLower (ptdescription pt)
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
journalWithBudgetAccountNames :: S.Set AccountName -> Bool -> Journal -> Journal
journalWithBudgetAccountNames budgetedaccts showunbudgeted j =
  dbg5With (("budget account names: "++).pshow.journalAccountNamesUsed) $
  j { jtxns = remapTxn <$> jtxns j }
  where
    remapTxn = txnTieKnot . transactionTransformPostings remapPosting
    remapPosting p = p { paccount = remapAccount $ paccount p, poriginal = poriginal p <|> Just p }
    remapAccount a
      | a `S.member` budgetedaccts = a
      | Just p <- budgetedparent   = if showunbudgeted then a else p
      | otherwise                  = if showunbudgeted then u <> acctsep <> a else u
      where
        budgetedparent = find (`S.member` budgetedaccts) $ parentAccountNames a
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
        mbrsorted = map prrFullName . sortRows ropts j . map (fmap $ fromMaybe nullmixedamt . fst)
        rows = rows1 ++ rows2

    totalrow = PeriodicReportRow ()
        [ (Map.lookup p totActualByPeriod, Map.lookup p totBudgetByPeriod) | p <- periods ]
        ( Just actualgrandtot, budget budgetgrandtot )
        ( Just actualgrandavg, budget budgetgrandavg )
      where
        totBudgetByPeriod = Map.fromList $ zip budgetperiods budgettots :: Map DateSpan BudgetTotal
        totActualByPeriod = Map.fromList $ zip actualperiods actualtots :: Map DateSpan Change
        budget b = if mixedAmountLooksZero b then Nothing else Just b

-- | Render a budget report as plain text suitable for console output.
budgetReportAsText :: ReportOpts -> BudgetReport -> TL.Text
budgetReportAsText ropts@ReportOpts{..} budgetr = TB.toLazyText $
    TB.fromText title <> TB.fromText "\n\n" <>
      balanceReportTableAsText ropts (budgetReportAsTable ropts budgetr)
  where
    title = "Budget performance in " <> showDateSpan (periodicReportSpan budgetr)
           <> (case cost_ of
                 Cost   -> ", converted to cost"
                 NoCost -> "")
           <> (case value_ of
                 Just (AtThen _mc)   -> ", valued at posting date"
                 Just (AtEnd _mc)    -> ", valued at period ends"
                 Just (AtNow _mc)    -> ", current value"
                 Just (AtDate d _mc) -> ", valued at " <> showDate d
                 Nothing             -> "")
           <> ":"

-- | Build a 'Table' from a multi-column balance report.
budgetReportAsTable :: ReportOpts -> BudgetReport -> Table Text Text WideBuilder
budgetReportAsTable
  ReportOpts{..}
  (PeriodicReport spans items tr) =
    maybetransposetable $
    addtotalrow $
    Table
      (Tab.Group NoLine $ map Header accts)
      (Tab.Group NoLine $ map Header colheadings)
      rows
  where
    colheadings = ["Commodity" | commodity_column_]
                  ++ map (reportPeriodName balanceaccum_ spans) spans
                  ++ ["  Total" | row_total_]
                  ++ ["Average" | average_]

    -- FIXME. Have to check explicitly for which to render here, since
    -- budgetReport sets accountlistmode to ALTree. Find a principled way to do
    -- this.
    renderacct row = case accountlistmode_ of
        ALTree -> T.replicate ((prrDepth row - 1)*2) " " <> prrDisplayName row
        ALFlat -> accountNameDrop (drop_) $ prrFullName row

    addtotalrow
      | no_total_ = id
      | otherwise = let rh = Tab.Group NoLine . replicate (length totalrows) $ Header ""
                        ch = Header [] -- ignored
                     in (flip (concatTables SingleLine) $ Table rh ch totalrows)

    maybetranspose
      | transpose_ = transpose
      | otherwise  = id

    maybetransposetable
      | transpose_ = \(Table rh ch vals) -> Table ch rh (transpose vals)
      | otherwise  = id

    (accts, rows, totalrows) = (accts, prependcs itemscs (padcells texts), prependcs trcs (padtr trtexts))
      where
        shownitems :: [[(AccountName, WideBuilder, BudgetDisplayRow)]]
        shownitems = (fmap (\i -> fmap (\(cs, cvals) -> (renderacct i, cs, cvals)) . showrow $ rowToBudgetCells i) items)
        (accts, itemscs, texts) = unzip3 $ concat shownitems

        showntr    :: [[(WideBuilder, BudgetDisplayRow)]]
        showntr    = [showrow $ rowToBudgetCells tr]
        (trcs, trtexts)         = unzip  $ concat showntr
        trwidths
          | transpose_ = drop (length texts) widths
          | otherwise = widths

        padcells = maybetranspose . fmap (fmap (uncurry paddisplaycell) . zip widths)   . maybetranspose
        padtr    = maybetranspose . fmap (fmap (uncurry paddisplaycell) . zip trwidths) . maybetranspose

        -- commodities are shown with the amounts without `commodity-column`
        prependcs cs
          | commodity_column_ = zipWith (:) cs
          | otherwise = id

    rowToBudgetCells (PeriodicReportRow _ as rowtot rowavg) = as
        ++ [rowtot | row_total_ && not (null as)]
        ++ [rowavg | average_   && not (null as)]

    -- functions for displaying budget cells depending on `commodity-column` flag
    rowfuncs :: [CommoditySymbol] -> (BudgetShowMixed, BudgetPercBudget)
    rowfuncs cs
      | not commodity_column_ =
          ( pure . showMixedAmountB oneLine{displayColour=color_, displayMaxWidth=Just 32}
          , \a -> pure . percentage a)
      | otherwise =
          ( showMixedAmountLinesB noPrice{displayOrder=Just cs, displayMinWidth=Nothing, displayColour=color_}
          , \a b -> fmap (percentage' a b) cs)

    showrow :: [BudgetCell] -> [(WideBuilder, BudgetDisplayRow)]
    showrow row =
      let cs = budgetCellsCommodities row
          (showmixed, percbudget) = rowfuncs cs
       in   zip (fmap wbFromText cs)
          . transpose
          . fmap (showcell showmixed percbudget)
          $ row

    budgetCellsCommodities = S.toList . foldl' S.union mempty . fmap budgetCellCommodities
    budgetCellCommodities :: BudgetCell -> S.Set CommoditySymbol
    budgetCellCommodities (am, bm) = f am `S.union` f bm
      where f = maybe mempty maCommodities

    cellswidth :: [BudgetCell] -> [[(Int, Int, Int)]]
    cellswidth row =
      let cs = budgetCellsCommodities row
          (showmixed, percbudget) = rowfuncs cs
          disp = showcell showmixed percbudget
          budgetpercwidth = wbWidth *** maybe 0 wbWidth
          cellwidth (am, bm) = let (bw, pw) = maybe (0, 0) budgetpercwidth bm in (wbWidth am, bw, pw)
       in fmap (fmap cellwidth . disp) row

    -- build a list of widths for each column. In the case of transposed budget
    -- reports, the total 'row' must be included in this list
    widths = zip3 actualwidths budgetwidths percentwidths
      where
        actualwidths  = map (maximum' . map first3 ) $ cols
        budgetwidths  = map (maximum' . map second3) $ cols
        percentwidths = map (maximum' . map third3 ) $ cols
        catcolumnwidths = foldl' (zipWith (++)) $ repeat []
        cols = maybetranspose $ catcolumnwidths $ map (cellswidth . rowToBudgetCells) items ++ [cellswidth $ rowToBudgetCells tr]

    -- split a BudgetCell into BudgetDisplayCell's (one per commodity when applicable)
    showcell :: BudgetShowMixed -> BudgetPercBudget -> BudgetCell -> BudgetDisplayRow
    showcell showmixed percbudget (actual, mbudget) = zip (showmixed actual') full
      where
        actual' = fromMaybe nullmixedamt actual

        budgetAndPerc b = uncurry zip
          ( showmixed b
          , fmap (fmap (wbFromText . T.pack . show . roundTo 0)) $ percbudget actual' b
          )

        full
          | Just b <- mbudget = fmap Just $ budgetAndPerc b
          | otherwise         = repeat Nothing

    paddisplaycell :: (Int, Int, Int) -> BudgetDisplayCell -> WideBuilder
    paddisplaycell (actualwidth, budgetwidth, percentwidth) (actual, mbudget) = full
      where
        toPadded (WideBuilder b w) =
            (TB.fromText . flip T.replicate " " $ actualwidth - w) <> b

        (totalpercentwidth, totalbudgetwidth) =
          let totalpercentwidth = if percentwidth == 0 then 0 else percentwidth + 5
           in ( totalpercentwidth
              , if budgetwidth == 0 then 0 else budgetwidth + totalpercentwidth + 3
              )

        -- | Display a padded budget string
        budgetb (budget, perc) =
          let perct = case perc of
                Nothing  -> T.replicate totalpercentwidth " "
                Just pct -> T.replicate (percentwidth - wbWidth pct) " " <> wbToText pct <> "% of "
           in TB.fromText $ " [" <> perct <> T.replicate (budgetwidth - wbWidth budget) " " <> wbToText budget <> "]"

        emptyBudget = TB.fromText $ T.replicate totalbudgetwidth " "

        full = flip WideBuilder (actualwidth + totalbudgetwidth) $
            toPadded actual <> maybe emptyBudget budgetb mbudget

    -- | Calculate the percentage of actual change to budget goal to show, if any.
    -- If valuing at cost, both amounts are converted to cost before comparing.
    -- A percentage will not be shown if:
    -- - actual or goal are not the same, single, commodity
    -- - the goal is zero
    percentage :: Change -> BudgetGoal -> Maybe Percentage
    percentage actual budget =
      case (costedAmounts actual, costedAmounts budget) of
        ([a], [b]) | (acommodity a == acommodity b || amountLooksZero a) && not (amountLooksZero b)
            -> Just $ 100 * aquantity a / aquantity b
        _   -> -- trace (pshow $ (maybecost actual, maybecost budget))  -- debug missing percentage
               Nothing
      where
        costedAmounts = case cost_ of
            Cost   -> amounts . mixedAmountCost
            NoCost -> amounts

    -- | Calculate the percentage of actual change to budget goal for a particular commodity
    percentage' :: Change -> BudgetGoal -> CommoditySymbol -> Maybe Percentage
    percentage' am bm c = case ((,) `on` find ((==) c . acommodity) . amounts) am bm of
        (Just a, Just b) -> percentage (mixedAmount a) (mixedAmount b)
        _                -> Nothing

-- XXX generalise this with multiBalanceReportAsCsv ?
-- | Render a budget report as CSV. Like multiBalanceReportAsCsv,
-- but includes alternating actual and budget amount columns.
budgetReportAsCsv :: ReportOpts -> BudgetReport -> CSV
budgetReportAsCsv
  ReportOpts{..}
  (PeriodicReport colspans items tr)
  = (if transpose_ then transpose else id) $

  -- heading row
  ("Account" :
  ["Commodity" | commodity_column_ ]
   ++ concatMap (\span -> [showDateSpan span, "budget"]) colspans
   ++ concat [["Total"  ,"budget"] | row_total_]
   ++ concat [["Average","budget"] | average_]
  ) :

  -- account rows
  concatMap (rowAsTexts prrFullName) items

  -- totals row
  ++ concat [ rowAsTexts (const "Total:") tr | not no_total_ ]

  where
    flattentuples abs = concat [[a,b] | (a,b) <- abs]
    showNorm = maybe "" (wbToText . showMixedAmountB oneLine)

    rowAsTexts :: (PeriodicReportRow a BudgetCell -> Text)
               -> PeriodicReportRow a BudgetCell
               -> [[Text]]
    rowAsTexts render row@(PeriodicReportRow _ as (rowtot,budgettot) (rowavg, budgetavg))
      | not commodity_column_ = [render row : fmap showNorm all]
      | otherwise =
            joinNames . zipWith (:) cs  -- add symbols and names
          . transpose                   -- each row becomes a list of Text quantities
          . fmap (fmap wbToText . showMixedAmountLinesB oneLine{displayOrder=Just cs, displayMinWidth=Nothing})
          . fmap (fromMaybe nullmixedamt)
          $ all
      where
        cs = S.toList . foldl' S.union mempty . fmap maCommodities $ catMaybes all
        all = flattentuples as
            ++ concat [[rowtot, budgettot] | row_total_]
            ++ concat [[rowavg, budgetavg] | average_]

        joinNames = fmap ((:) (render row))

-- tests

tests_BudgetReport = tests "BudgetReport" [
 ]
