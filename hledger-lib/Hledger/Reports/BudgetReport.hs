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
import Data.List (find, partition, transpose, foldl', maximumBy, intercalate)
import Data.List.Extra (nubSort)
import Data.Maybe (fromMaybe, catMaybes, isJust)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TB
import Safe (minimumDef)
--import System.Console.CmdArgs.Explicit as C
--import Lucid as L
import qualified Text.Tabular.AsciiWide as Tab

import Hledger.Data
import Hledger.Utils
import Hledger.Reports.ReportOptions
import Hledger.Reports.ReportTypes
import Hledger.Reports.MultiBalanceReport
import Data.Ord (comparing)
import Control.Monad ((>=>))

-- All MixedAmounts:
type BudgetGoal    = Change
type BudgetTotal   = Total
type BudgetAverage = Average

-- | A budget report tracks expected and actual changes per account and subperiod.
-- Each table cell has an actual change amount and/or a budget goal amount.
type BudgetCell = (Maybe Change, Maybe BudgetGoal)
-- | A row in a budget report table - account name and data cells.
type BudgetReportRow = PeriodicReportRow DisplayName BudgetCell
-- | A full budget report table.
type BudgetReport    = PeriodicReport    DisplayName BudgetCell

-- A BudgetCell's data values rendered for display - the actual change amount,
-- the budget goal amount if any, and the corresponding goal percentage if possible.
type BudgetDisplayCell = (WideBuilder, Maybe (WideBuilder, Maybe WideBuilder))
-- | A row of rendered budget data cells.
type BudgetDisplayRow  = [BudgetDisplayCell]

-- | An amount render helper for the budget report. Renders each commodity separately.
type BudgetShowAmountsFn   = MixedAmount -> [WideBuilder]
-- | A goal percentage calculating helper for the budget report.
type BudgetCalcPercentagesFn  = Change -> BudgetGoal -> [Maybe Percentage]

_brrShowDebug :: BudgetReportRow -> String
_brrShowDebug (PeriodicReportRow dname budgetpairs _tot _avg) =
  unwords [
    T.unpack $ displayFull dname,
    "",
    intercalate " | "
      [ maybe "-" showMixedAmount mactual <> " [" <> maybe "-" showMixedAmount mgoal <> "]"
      | (mactual,mgoal) <- budgetpairs ]
    ]

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
      concatMap (\pt -> runPeriodicTransaction False pt reportspan) $
      jperiodictxns j
    actualj = journalWithBudgetAccountNames budgetedaccts showunbudgeted j
    budgetj = journalAddBudgetGoalTransactions bopts ropts reportspan j
    priceoracle = journalPriceOracle (infer_prices_ ropts) j
    budgetgoalreport@(PeriodicReport _ budgetgoalitems budgetgoaltotals) =
        dbg5 "budgetgoalreport" $ multiBalanceReportWith rspec{_rsReportOpts=ropts{empty_=True}} budgetj priceoracle mempty
    budgetedacctsseen = S.fromList $ map prrFullName budgetgoalitems
    actualreport@(PeriodicReport actualspans _ _) =
        dbg5 "actualreport"     $ multiBalanceReportWith rspec{_rsReportOpts=ropts{empty_=True}} actualj priceoracle budgetedacctsseen
    budgetgoalreport'
      -- If no interval is specified:
      -- budgetgoalreport's span might be shorter actualreport's due to periodic txns;
      -- it should be safe to replace it with the latter, so they combine well.
      | interval_ ropts == NoInterval = PeriodicReport actualspans budgetgoalitems budgetgoaltotals
      | otherwise = budgetgoalreport
    budgetreport = combineBudgetAndActual ropts j budgetgoalreport' actualreport

-- | Use all (or all matched by --budget's argument) periodic transactions in the journal 
-- to generate budget goal transactions in the specified date span (and before, to support
-- --historical. The precise start date is the natural start date of the largest interval
-- of the active periodic transaction rules that is on or before the earlier of journal start date,
-- report start date.)
-- Budget goal transactions are similar to forecast transactions except their purpose 
-- and effect is to define balance change goals, per account and period, for BudgetReport.
--
journalAddBudgetGoalTransactions :: BalancingOpts -> ReportOpts -> DateSpan -> Journal -> Journal
journalAddBudgetGoalTransactions bopts ropts reportspan j =
  either error' id $  -- PARTIAL:
    (journalStyleAmounts >=> journalBalanceTransactions bopts) j{ jtxns = budgetts }
  where
    budgetspan = dbg3 "budget span" $ DateSpan (Exact <$> mbudgetgoalsstartdate) (Exact <$> spanEnd reportspan)
      where
        mbudgetgoalsstartdate =
          -- We want to also generate budget goal txns before the report start date, in case -H is used.
          -- What should the actual starting date for goal txns be ? This gets tricky. 
          -- Consider a journal with a "~ monthly" periodic transaction rule, where the first transaction is on 1/5.
          -- Users will certainly expect a budget goal for january, but "~ monthly" generates transactions
          -- on the first of month, and starting from 1/5 would exclude 1/1.
          -- Secondly, consider a rule like "~ every february 2nd from 2020/01"; we should not start that
          -- before 2020-02-02.
          -- Hopefully the following algorithm produces intuitive behaviour in general:
          -- from the earlier of the journal start date and the report start date,
          -- move backward to the nearest natural start date of the largest period seen among the
          -- active periodic transactions, unless that is disallowed by a start date in the periodic rule.
          -- (Do we need to pay attention to an end date in the rule ? Don't think so.)
          -- (So with "~ monthly", the journal start date 1/5 is adjusted to 1/1.)
          case minimumDef Nothing $ filter isJust [journalStartDate False j, spanStart reportspan] of
            Nothing -> Nothing
            Just d  -> Just d'
              where
                -- the interval and any date span of the periodic transaction with longest period
                (intervl, spn) =
                  case budgetpts of
                    []  -> (Days 1, nulldatespan)
                    pts -> (ptinterval pt, ptspan pt)
                      where pt = maximumBy (comparing ptinterval) pts  -- PARTIAL: maximumBy won't fail
                -- the natural start of this interval on or before the journal/report start
                intervalstart = intervalBoundaryBefore intervl d
                -- the natural interval start before the journal/report start,
                -- or the rule-specified start if later,
                -- but no later than the journal/report start.
                d' = min d $ maybe intervalstart (max intervalstart) $ spanStart spn

    -- select periodic transactions matching a pattern
    -- (the argument of the (final) --budget option).
    -- XXX two limitations/wishes, requiring more extensive type changes:
    -- - give an error if pat is non-null and matches no periodic txns
    -- - allow a regexp or a full hledger query, not just a substring
    pat = fromMaybe "" $ dbg3 "budget pattern" $ T.toLower <$> budgetpat_ ropts
    budgetpts = [pt | pt <- jperiodictxns j, pat `T.isInfixOf` T.toLower (ptdescription pt)]
    budgetts =
      dbg5 "budget goal txns" $
      [makeBudgetTxn t
      | pt <- budgetpts
      , t <- runPeriodicTransaction False pt budgetspan
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
    PeriodicReport periods combinedrows totalrow
  where
    periods = nubSort . filter (/= nulldatespan) $ budgetperiods ++ actualperiods

    -- first, combine any corresponding budget goals with actual changes
    actualsplusgoals = [
        -- dbg0With (("actualsplusgoals: "<>)._brrShowDebug) $
        PeriodicReportRow acct amtandgoals totamtandgoal avgamtandgoal
      | PeriodicReportRow acct actualamts actualtot actualavg <- actualrows

      , let mbudgetgoals       = HM.lookup (displayFull acct) budgetGoalsByAcct :: Maybe ([BudgetGoal], BudgetTotal, BudgetAverage)
      , let budgetmamts        = maybe (Nothing <$ periods) (map Just . first3) mbudgetgoals :: [Maybe BudgetGoal]
      , let mbudgettot         = second3 <$> mbudgetgoals :: Maybe BudgetTotal
      , let mbudgetavg         = third3 <$> mbudgetgoals  :: Maybe BudgetAverage
      , let acctGoalByPeriod   = Map.fromList [ (p,budgetamt) | (p, Just budgetamt) <- zip budgetperiods budgetmamts ] :: Map DateSpan BudgetGoal
      , let acctActualByPeriod = Map.fromList [ (p,actualamt) | (p, Just actualamt) <- zip actualperiods (map Just actualamts) ] :: Map DateSpan Change
      , let amtandgoals        = [ (Map.lookup p acctActualByPeriod, Map.lookup p acctGoalByPeriod) | p <- periods ] :: [BudgetCell]
      , let totamtandgoal      = (Just actualtot, mbudgettot)
      , let avgamtandgoal      = (Just actualavg, mbudgetavg)
      ]
      where
        budgetGoalsByAcct :: HashMap AccountName ([BudgetGoal], BudgetTotal, BudgetAverage) =
          HM.fromList [ (displayFull acct, (amts, tot, avg))
                      | PeriodicReportRow acct amts tot avg <-
                          -- dbg0With (unlines.map (("budgetgoals: "<>).prrShowDebug)) $
                          budgetrows
                      ]

    -- next, make rows for budget goals with no actual changes
    othergoals = [
        -- dbg0With (("othergoals: "<>)._brrShowDebug) $
        PeriodicReportRow acct amtandgoals totamtandgoal avgamtandgoal
      | PeriodicReportRow acct budgetgoals budgettot budgetavg <- budgetrows
      , displayFull acct `notElem` map prrFullName actualsplusgoals
      , let acctGoalByPeriod   = Map.fromList $ zip budgetperiods budgetgoals :: Map DateSpan BudgetGoal
      , let amtandgoals        = [ (Just 0, Map.lookup p acctGoalByPeriod) | p <- periods ] :: [BudgetCell]
      , let totamtandgoal      = (Just 0, Just budgettot)
      , let avgamtandgoal      = (Just 0, Just budgetavg)
      ]

    -- combine and re-sort rows
    -- TODO: add --sort-budget to sort by budget goal amount
    combinedrows :: [BudgetReportRow] =
      -- map (dbg0With (("combinedrows: "<>)._brrShowDebug)) $
      sortRowsLike (mbrsorted unbudgetedrows ++ mbrsorted rows') rows
      where
        (unbudgetedrows, rows') = partition ((==unbudgetedAccountName) . prrFullName) rows
        mbrsorted = map prrFullName . sortRows ropts j . map (fmap $ fromMaybe nullmixedamt . fst)
        rows = actualsplusgoals ++ othergoals

    totalrow = PeriodicReportRow ()
        [ (Map.lookup p totActualByPeriod, Map.lookup p totGoalByPeriod) | p <- periods ]
        ( Just actualgrandtot, budget budgetgrandtot )
        ( Just actualgrandavg, budget budgetgrandavg )
      where
        totGoalByPeriod = Map.fromList $ zip budgetperiods budgettots :: Map DateSpan BudgetTotal
        totActualByPeriod = Map.fromList $ zip actualperiods actualtots :: Map DateSpan Change
        budget b = if mixedAmountLooksZero b then Nothing else Just b

-- | Render a budget report as plain text suitable for console output.
budgetReportAsText :: ReportOpts -> BudgetReport -> TL.Text
budgetReportAsText ropts@ReportOpts{..} budgetr = TB.toLazyText $
    TB.fromText title <> TB.fromText "\n\n" <>
      balanceReportTableAsText ropts (budgetReportAsTable ropts budgetr)
  where
    title = "Budget performance in " <> showDateSpan (periodicReportSpan budgetr)
           <> (case conversionop_ of
                 Just ToCost -> ", converted to cost"
                 _           -> "")
           <> (case value_ of
                 Just (AtThen _mc)   -> ", valued at posting date"
                 Just (AtEnd _mc)    -> ", valued at period ends"
                 Just (AtNow _mc)    -> ", current value"
                 Just (AtDate d _mc) -> ", valued at " <> showDate d
                 Nothing             -> "")
           <> ":"

-- | Build a 'Table' from a multi-column balance report.
budgetReportAsTable :: ReportOpts -> BudgetReport -> Tab.Table Text Text WideBuilder
budgetReportAsTable ReportOpts{..} (PeriodicReport spans items totrow) =
  maybetransposetable $
  addtotalrow $
    Tab.Table
      (Tab.Group Tab.NoLine $ map Tab.Header accts)
      (Tab.Group Tab.NoLine $ map Tab.Header colheadings)
      rows
  where
    maybetransposetable
      | transpose_ = \(Tab.Table rh ch vals) -> Tab.Table ch rh (transpose vals)
      | otherwise  = id

    addtotalrow
      | no_total_ = id
      | otherwise = let rh = Tab.Group Tab.NoLine . replicate (length totalrows) $ Tab.Header ""
                        ch = Tab.Header [] -- ignored
                     in (flip (Tab.concatTables Tab.SingleLine) $ Tab.Table rh ch totalrows)

    colheadings = ["Commodity" | layout_ == LayoutBare]
                  ++ map (reportPeriodName balanceaccum_ spans) spans
                  ++ ["  Total" | row_total_]
                  ++ ["Average" | average_]

    (accts, rows, totalrows) =
      (accts'
      ,maybecommcol itemscs  $ showcells  texts
      ,maybecommcol totrowcs $ showtotrow totrowtexts)
      where
        -- If --layout=bare, prepend a commodities column.
        maybecommcol :: [WideBuilder] -> [[WideBuilder]] -> [[WideBuilder]]
        maybecommcol cs
          | layout_ == LayoutBare = zipWith (:) cs
          | otherwise             = id

        showcells, showtotrow :: [[BudgetDisplayCell]] -> [[WideBuilder]]
        (showcells, showtotrow) =
          (maybetranspose . map (zipWith showBudgetDisplayCell widths)       . maybetranspose
          ,maybetranspose . map (zipWith showBudgetDisplayCell totrowwidths) . maybetranspose)
          where
            -- | Combine a BudgetDisplayCell's rendered values into a "[PERCENT of GOAL]" rendering,
            -- respecting the given widths.
            showBudgetDisplayCell :: (Int, Int, Int) -> BudgetDisplayCell -> WideBuilder
            showBudgetDisplayCell (actualwidth, budgetwidth, percentwidth) (actual, mbudget) =
              flip WideBuilder (actualwidth + totalbudgetwidth) $
                toPadded actual <> maybe emptycell showBudgetGoalAndPercentage mbudget

              where
                toPadded (WideBuilder b w) = (TB.fromText . flip T.replicate " " $ actualwidth - w) <> b

                (totalpercentwidth, totalbudgetwidth) =
                  let totalpercentwidth' = if percentwidth == 0 then 0 else percentwidth + 5
                   in ( totalpercentwidth'
                      , if budgetwidth == 0 then 0 else budgetwidth + totalpercentwidth' + 3
                      )

                emptycell :: TB.Builder
                emptycell = TB.fromText $ T.replicate totalbudgetwidth " "

                showBudgetGoalAndPercentage :: (WideBuilder, Maybe WideBuilder) -> TB.Builder
                showBudgetGoalAndPercentage (goal, perc) =
                  let perct = case perc of
                        Nothing  -> T.replicate totalpercentwidth " "
                        Just pct -> T.replicate (percentwidth - wbWidth pct) " " <> wbToText pct <> "% of "
                   in TB.fromText $ " [" <> perct <> T.replicate (budgetwidth - wbWidth goal) " " <> wbToText goal <> "]"

            -- | Build a list of widths for each column.
            -- When --transpose is used, the totals row must be included in this list.
            widths :: [(Int, Int, Int)]
            widths = zip3 actualwidths budgetwidths percentwidths
              where
                actualwidths  = map (maximum' . map first3 ) $ cols
                budgetwidths  = map (maximum' . map second3) $ cols
                percentwidths = map (maximum' . map third3 ) $ cols
                catcolumnwidths = foldl' (zipWith (++)) $ repeat []
                cols = maybetranspose $ catcolumnwidths $ map (cellswidth . rowToBudgetCells) items ++ [cellswidth $ rowToBudgetCells totrow]

                cellswidth :: [BudgetCell] -> [[(Int, Int, Int)]]
                cellswidth row =
                  let cs = budgetCellsCommodities row
                      (showmixed, percbudget) = mkBudgetDisplayFns cs
                      disp = showcell showmixed percbudget
                      budgetpercwidth = wbWidth *** maybe 0 wbWidth
                      cellwidth (am, bm) = let (bw, pw) = maybe (0, 0) budgetpercwidth bm in (wbWidth am, bw, pw)
                   in map (map cellwidth . disp) row

            totrowwidths :: [(Int, Int, Int)]
            totrowwidths
              | transpose_ = drop (length texts) widths
              | otherwise = widths

            maybetranspose
              | transpose_ = transpose
              | otherwise  = id

        (accts', itemscs, texts) = unzip3 $ concat shownitems
          where
            shownitems :: [[(AccountName, WideBuilder, BudgetDisplayRow)]]
            shownitems =
              map (\i ->
                let
                  addacctcolumn = map (\(cs, cvals) -> (renderacct i, cs, cvals))
                  isunbudgetedrow = displayFull (prrName i) == unbudgetedAccountName
                in addacctcolumn $ showrow isunbudgetedrow $ rowToBudgetCells i)
              items
              where
                -- FIXME. Have to check explicitly for which to render here, since
                -- budgetReport sets accountlistmode to ALTree. Find a principled way to do
                -- this.
                renderacct row = case accountlistmode_ of
                  ALTree -> T.replicate ((prrDepth row - 1)*2) " " <> prrDisplayName row
                  ALFlat -> accountNameDrop (drop_) $ prrFullName row

        (totrowcs, totrowtexts)  = unzip  $ concat showntotrow
          where
            showntotrow :: [[(WideBuilder, BudgetDisplayRow)]]
            showntotrow = [showrow False $ rowToBudgetCells totrow]

        -- | Get the data cells from a row or totals row, maybe adding 
        -- the row total and/or row average depending on options.
        rowToBudgetCells :: PeriodicReportRow a BudgetCell -> [BudgetCell]
        rowToBudgetCells (PeriodicReportRow _ as rowtot rowavg) = as
            ++ [rowtot | row_total_ && not (null as)]
            ++ [rowavg | average_   && not (null as)]

        -- | Render a row's data cells as "BudgetDisplayCell"s, and a rendered list of commodity symbols.
        -- Also requires a flag indicating whether this is the special <unbudgeted> row.
        -- (The types make that hard to check here.)
        showrow :: Bool -> [BudgetCell] -> [(WideBuilder, BudgetDisplayRow)]
        showrow isunbudgetedrow cells =
          let
            cs = budgetCellsCommodities cells
            -- #2071 If there are no commodities - because there are no actual or goal amounts -
            -- the zipped list would be empty, causing this row not to be shown.
            -- But rows like this sometimes need to be shown to preserve the account tree structure.
            -- So, ensure 0 will be shown as actual amount(s).
            -- Unfortunately this disables boring parent eliding, as if --no-elide had been used.
            -- (Just turning on --no-elide higher up doesn't work right.)
            -- Note, no goal amount will be shown for these rows,
            -- whereas --no-elide is likely to show a goal amount aggregated from children.
            cs1 = if null cs && not isunbudgetedrow then [""] else cs
            (showmixed, percbudget) = mkBudgetDisplayFns cs1
          in
            zip (map wbFromText cs1) $
            transpose $
            map (showcell showmixed percbudget)
            cells

        budgetCellsCommodities :: [BudgetCell] -> [CommoditySymbol]
        budgetCellsCommodities = S.toList . foldl' S.union mempty . map budgetCellCommodities
          where
            budgetCellCommodities :: BudgetCell -> S.Set CommoditySymbol
            budgetCellCommodities (am, bm) = f am `S.union` f bm
              where f = maybe mempty maCommodities

        -- | Render a "BudgetCell"'s amounts as "BudgetDisplayCell"s (one per commodity).
        showcell :: BudgetShowAmountsFn -> BudgetCalcPercentagesFn -> BudgetCell -> BudgetDisplayRow
        showcell showCommodityAmounts calcCommodityPercentages (mactual, mbudget) =
          zip actualamts budgetinfos
          where
            actual = fromMaybe nullmixedamt mactual
            actualamts = showCommodityAmounts actual
            budgetinfos =
              case mbudget of
                Nothing   -> repeat Nothing
                Just goal -> map Just $ showGoalAmountsAndPercentages goal
                where
                  showGoalAmountsAndPercentages :: MixedAmount -> [(WideBuilder, Maybe WideBuilder)]
                  showGoalAmountsAndPercentages goal = zip amts mpcts
                    where
                      amts  = showCommodityAmounts goal
                      mpcts = map (showrounded <$>) $ calcCommodityPercentages actual goal
                        where showrounded = wbFromText . T.pack . show . roundTo 0

        -- | Make budget info display helpers that adapt to --layout=wide.
        mkBudgetDisplayFns :: [CommoditySymbol] -> (BudgetShowAmountsFn, BudgetCalcPercentagesFn)
        mkBudgetDisplayFns cs = case layout_ of
          LayoutWide width ->
               ( pure . showMixedAmountB oneLineNoCostFmt{displayMaxWidth=width, displayColour=color_}
               , \a -> pure . percentage a)
          _ -> ( showMixedAmountLinesB noCostFmt{displayCommodity=layout_/=LayoutBare, displayCommodityOrder=Just cs, displayMinWidth=Nothing, displayColour=color_}
               , \a b -> map (percentage' a b) cs)
          where
            -- | Calculate the percentage of actual change to budget goal to show, if any.
            -- If valuing at cost, both amounts are converted to cost before comparing.
            -- A percentage will not be shown if:
            --
            -- - actual or goal are not the same, single, commodity
            --
            -- - the goal is zero
            --
            percentage :: Change -> BudgetGoal -> Maybe Percentage
            percentage actual budget =
              case (costedAmounts actual, costedAmounts budget) of
                ([a], [b]) | (acommodity a == acommodity b || amountLooksZero a) && not (amountLooksZero b)
                    -> Just $ 100 * aquantity a / aquantity b
                _   -> Nothing
              where
                costedAmounts = case conversionop_ of
                    Just ToCost -> amounts . mixedAmountCost
                    _           -> amounts

            -- | Like percentage, but accept multicommodity actual and budget amounts,
            -- and extract the specified commodity from both.
            percentage' :: Change -> BudgetGoal -> CommoditySymbol -> Maybe Percentage
            percentage' am bm c = case ((,) `on` find ((==) c . acommodity) . amounts) am bm of
                (Just a, Just b) -> percentage (mixedAmount a) (mixedAmount b)
                _                -> Nothing

-- XXX generalise this with multiBalanceReportAsCsv ?
-- | Render a budget report as CSV. Like multiBalanceReportAsCsv,
-- but includes alternating actual and budget amount columns.
budgetReportAsCsv :: ReportOpts -> BudgetReport -> [[Text]]
budgetReportAsCsv
  ReportOpts{..}
  (PeriodicReport colspans items totrow)
  = (if transpose_ then transpose else id) $

  -- heading row
  ("Account" :
  ["Commodity" | layout_ == LayoutBare ]
   ++ concatMap (\spn -> [showDateSpan spn, "budget"]) colspans
   ++ concat [["Total"  ,"budget"] | row_total_]
   ++ concat [["Average","budget"] | average_]
  ) :

  -- account rows
  concatMap (rowAsTexts prrFullName) items

  -- totals row
  ++ concat [ rowAsTexts (const "Total:") totrow | not no_total_ ]

  where
    flattentuples tups = concat [[a,b] | (a,b) <- tups]
    showNorm = maybe "" (wbToText . showMixedAmountB oneLineNoCostFmt)

    rowAsTexts :: (PeriodicReportRow a BudgetCell -> Text)
               -> PeriodicReportRow a BudgetCell
               -> [[Text]]
    rowAsTexts render row@(PeriodicReportRow _ as (rowtot,budgettot) (rowavg, budgetavg))
      | layout_ /= LayoutBare = [render row : map showNorm vals]
      | otherwise =
            joinNames . zipWith (:) cs  -- add symbols and names
          . transpose                   -- each row becomes a list of Text quantities
          . map (map wbToText . showMixedAmountLinesB dopts . fromMaybe nullmixedamt)
          $ vals
      where
        cs = S.toList . foldl' S.union mempty . map maCommodities $ catMaybes vals
        dopts = oneLineNoCostFmt{displayCommodity=layout_ /= LayoutBare, displayCommodityOrder=Just cs, displayMinWidth=Nothing}
        vals = flattentuples as
            ++ concat [[rowtot, budgettot] | row_total_]
            ++ concat [[rowavg, budgetavg] | average_]

        joinNames = map (render row :)

-- tests

tests_BudgetReport = testGroup "BudgetReport" [
 ]
