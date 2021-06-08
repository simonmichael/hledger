{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-|

Multi-column balance reports, used by the balance command.

-}

module Hledger.Reports.MultiBalanceReport (
  MultiBalanceReport,
  MultiBalanceReportRow,

  multiBalanceReport,
  multiBalanceReportWith,

  compoundBalanceReport,
  compoundBalanceReportWith,

  sortRows,
  sortRowsLike,

  -- * Helper functions
  makeReportQuery,
  getPostingsByColumn,
  getPostings,
  startingBalances,
  generateMultiBalanceReport,

  -- -- * Tests
  tests_MultiBalanceReport
)
where

import Control.Monad (guard)
import Data.Foldable (toList)
import Data.List (sortOn, transpose)
import Data.List.NonEmpty (NonEmpty(..))
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromMaybe, isJust, mapMaybe)
import Data.Ord (Down(..))
import Data.Semigroup (sconcat)
import Data.Time.Calendar (Day, fromGregorian)
import Safe (lastDef, minimumMay)

import Hledger.Data
import Hledger.Query
import Hledger.Utils hiding (dbg3,dbg4,dbg5)
import qualified Hledger.Utils
import Hledger.Read (mamountp')
import Hledger.Reports.ReportOptions
import Hledger.Reports.ReportTypes


-- add a prefix to this function's debug output
dbg3 s = let p = "multiBalanceReport" in Hledger.Utils.dbg3 (p++" "++s)
dbg4 s = let p = "multiBalanceReport" in Hledger.Utils.dbg4 (p++" "++s)
dbg5 s = let p = "multiBalanceReport" in Hledger.Utils.dbg5 (p++" "++s)


-- | A multi balance report is a kind of periodic report, where the amounts
-- correspond to balance changes or ending balances in a given period. It has:
--
-- 1. a list of each column's period (date span)
--
-- 2. a list of rows, each containing:
--
--   * the full account name, display name, and display depth
--
--   * A list of amounts, one for each column.
--
--   * the total of the row's amounts for a periodic report
--
--   * the average of the row's amounts
--
-- 3. the column totals, and the overall grand total (or zero for
-- cumulative/historical reports) and grand average.

type MultiBalanceReport    = PeriodicReport    DisplayName MixedAmount
type MultiBalanceReportRow = PeriodicReportRow DisplayName MixedAmount

-- type alias just to remind us which AccountNames might be depth-clipped, below.
type ClippedAccountName = AccountName


-- | Generate a multicolumn balance report for the matched accounts,
-- showing the change of balance, accumulated balance, or historical balance
-- in each of the specified periods. If the normalbalance_ option is set, it
-- adjusts the sorting and sign of amounts (see ReportOpts and
-- CompoundBalanceCommand). hledger's most powerful and useful report, used
-- by the balance command (in multiperiod mode) and (via compoundBalanceReport)
-- by the bs/cf/is commands.
multiBalanceReport :: ReportSpec -> Journal -> MultiBalanceReport
multiBalanceReport rspec j = multiBalanceReportWith rspec j (journalPriceOracle infer j)
  where infer = infer_value_ $ rsOpts rspec

-- | A helper for multiBalanceReport. This one takes an extra argument,
-- a PriceOracle to be used for looking up market prices. Commands which
-- run multiple reports (bs etc.) can generate the price oracle just
-- once for efficiency, passing it to each report by calling this
-- function directly.
multiBalanceReportWith :: ReportSpec -> Journal -> PriceOracle -> MultiBalanceReport
multiBalanceReportWith rspec' j priceoracle = report
  where
    -- Queries, report/column dates.
    reportspan = dbg3 "reportspan" $ reportSpan j rspec'
    rspec      = dbg3 "reportopts" $ makeReportQuery rspec' reportspan

    -- Group postings into their columns.
    colps = dbg5 "colps" $ getPostingsByColumn rspec j priceoracle reportspan

    -- The matched accounts with a starting balance. All of these should appear
    -- in the report, even if they have no postings during the report period.
    startbals = dbg5 "startbals" $ startingBalances rspec j priceoracle reportspan

    -- Generate and postprocess the report, negating balances and taking percentages if needed
    report = dbg4 "multiBalanceReportWith" $
      generateMultiBalanceReport rspec j priceoracle colps startbals

-- | Generate a compound balance report from a list of CBCSubreportSpec. This
-- shares postings between the subreports.
compoundBalanceReport :: ReportSpec -> Journal -> [CBCSubreportSpec a]
                      -> CompoundPeriodicReport a MixedAmount
compoundBalanceReport rspec j = compoundBalanceReportWith rspec j (journalPriceOracle infer j)
  where infer = infer_value_ $ rsOpts rspec

-- | A helper for compoundBalanceReport, similar to multiBalanceReportWith.
compoundBalanceReportWith :: ReportSpec -> Journal -> PriceOracle
                          -> [CBCSubreportSpec a]
                          -> CompoundPeriodicReport a MixedAmount
compoundBalanceReportWith rspec' j priceoracle subreportspecs = cbr
  where
    -- Queries, report/column dates.
    reportspan = dbg3 "reportspan" $ reportSpan j rspec'
    rspec      = dbg3 "reportopts" $ makeReportQuery rspec' reportspan

    -- Group postings into their columns.
    colps = dbg5 "colps" $ getPostingsByColumn rspec j priceoracle reportspan

    -- The matched accounts with a starting balance. All of these should appear
    -- in the report, even if they have no postings during the report period.
    startbals = dbg5 "startbals" $ startingBalances rspec j priceoracle reportspan

    subreports = map generateSubreport subreportspecs
      where
        generateSubreport CBCSubreportSpec{..} =
            ( cbcsubreporttitle
            -- Postprocess the report, negating balances and taking percentages if needed
            , cbcsubreporttransform $
                generateMultiBalanceReport rspec{rsOpts=ropts} j priceoracle colps' startbals'
            , cbcsubreportincreasestotal
            )
          where
            -- Filter the column postings according to each subreport
            colps'     = filter (matchesPosting q) <$> colps
            startbals' = HM.filterWithKey (\k _ -> matchesAccount q k) startbals
            ropts      = cbcsubreportoptions $ rsOpts rspec
            q          = cbcsubreportquery j

    -- Sum the subreport totals by column. Handle these cases:
    -- - no subreports
    -- - empty subreports, having no subtotals (#588)
    -- - subreports with a shorter subtotals row than the others
    overalltotals = case subreports of
        []     -> PeriodicReportRow () [] nullmixedamt nullmixedamt
        (r:rs) -> sconcat $ fmap subreportTotal (r:|rs)
      where
        subreportTotal (_, sr, increasestotal) =
            (if increasestotal then id else fmap maNegate) $ prTotals sr

    cbr = CompoundPeriodicReport "" (M.keys colps) subreports overalltotals


-- | Calculate starting balances, if needed for -H
--
-- Balances at report start date, from all earlier postings which otherwise match the query.
-- These balances are unvalued.
-- TODO: Do we want to check whether to bother calculating these? isHistorical
-- and startDate is not nothing, otherwise mempty? This currently gives a
-- failure with some totals which are supposed to be 0 being blank.
startingBalances :: ReportSpec -> Journal -> PriceOracle -> DateSpan -> HashMap AccountName Account
startingBalances rspec@ReportSpec{rsQuery=query,rsOpts=ropts} j priceoracle reportspan =
    fmap (M.findWithDefault nullacct precedingspan) acctmap
  where
    acctmap = calculateReportMatrix rspec' j priceoracle mempty
            . M.singleton precedingspan . map fst $ getPostings rspec' j priceoracle

    rspec' = rspec{rsQuery=startbalq,rsOpts=ropts'}
    -- If we're re-valuing every period, we need to have the unvalued start
    -- balance, so we can do it ourselves later.
    ropts' = case value_ ropts of
        Just (AtEnd _) -> ropts''{value_=Nothing}
        _              -> ropts''
      where ropts'' = ropts{period_=precedingperiod, no_elide_=accountlistmode_ ropts == ALTree}

    -- q projected back before the report start date.
    -- When there's no report start date, in case there are future txns (the hledger-ui case above),
    -- we use emptydatespan to make sure they aren't counted as starting balance.
    startbalq = dbg3 "startbalq" $ And [datelessq, precedingspanq]
    datelessq = dbg3 "datelessq" $ filterQuery (not . queryIsDateOrDate2) query

    precedingperiod = dateSpanAsPeriod . spanIntersect precedingspan .
                         periodAsDateSpan $ period_ ropts
    precedingspan = DateSpan Nothing $ spanStart reportspan
    precedingspanq = (if date2_ ropts then Date2 else Date) $ case precedingspan of
        DateSpan Nothing Nothing -> emptydatespan
        a -> a

-- | Remove any date queries and insert queries from the report span.
-- The user's query expanded to the report span
-- if there is one (otherwise any date queries are left as-is, which
-- handles the hledger-ui+future txns case above).
makeReportQuery :: ReportSpec -> DateSpan -> ReportSpec
makeReportQuery rspec reportspan
    | reportspan == nulldatespan = rspec
    | otherwise = rspec{rsQuery=query}
  where
    query            = simplifyQuery $ And [dateless $ rsQuery rspec, reportspandatesq]
    reportspandatesq = dbg3 "reportspandatesq" $ dateqcons reportspan
    dateless         = dbg3 "dateless" . filterQuery (not . queryIsDateOrDate2)
    dateqcons        = if date2_ (rsOpts rspec) then Date2 else Date

-- | Group postings, grouped by their column
getPostingsByColumn :: ReportSpec -> Journal -> PriceOracle -> DateSpan -> Map DateSpan [Posting]
getPostingsByColumn rspec j priceoracle reportspan = columns
  where
    -- Postings matching the query within the report period.
    ps :: [(Posting, Day)] = dbg5 "getPostingsByColumn" $ getPostings rspec j priceoracle

    -- The date spans to be included as report columns.
    colspans = dbg3 "colspans" $ splitSpan (interval_ $ rsOpts rspec) reportspan
    addPosting (p, d) = maybe id (M.adjust (p:)) $ latestSpanContaining colspans d
    emptyMap = M.fromList . zip colspans $ repeat []

    -- Group postings into their columns
    columns = foldr addPosting emptyMap ps

-- | Gather postings matching the query within the report period.
getPostings :: ReportSpec -> Journal -> PriceOracle -> [(Posting, Day)]
getPostings rspec@ReportSpec{rsQuery=query,rsOpts=ropts} j priceoracle =
    map (\p -> (p, date p)) .
    journalPostings .
    filterJournalAmounts symq .      -- remove amount parts excluded by cur:
    filterJournalPostings reportq $  -- remove postings not matched by (adjusted) query
    valuedJournal
  where
    symq = dbg3 "symq" . filterQuery queryIsSym $ dbg3 "requested q" query
    -- The user's query with no depth limit, and expanded to the report span
    -- if there is one (otherwise any date queries are left as-is, which
    -- handles the hledger-ui+future txns case above).
    reportq = dbg3 "reportq" $ depthless query
    depthless = dbg3 "depthless" . filterQuery (not . queryIsDepth)
    valuedJournal | isJust (valuationAfterSum ropts) = j
                  | otherwise = journalApplyValuationFromOptsWith rspec j priceoracle

    date = case whichDateFromOpts ropts of
        PrimaryDate   -> postingDate
        SecondaryDate -> postingDate2


-- | Given a set of postings, eg for a single report column, gather
-- the accounts that have postings and calculate the change amount for
-- each. Accounts and amounts will be depth-clipped appropriately if
-- a depth limit is in effect.
acctChangesFromPostings :: ReportSpec -> [Posting] -> HashMap ClippedAccountName Account
acctChangesFromPostings ReportSpec{rsQuery=query,rsOpts=ropts} ps =
    HM.fromList [(aname a, a) | a <- as]
  where
    as = filterAccounts . drop 1 $ accountsFromPostings ps
    filterAccounts = case accountlistmode_ ropts of
        ALTree -> filter ((depthq `matchesAccount`) . aname)      -- exclude deeper balances
        ALFlat -> clipAccountsAndAggregate (queryDepth depthq) .  -- aggregate deeper balances at the depth limit.
                      filter ((0<) . anumpostings)
    depthq = dbg3 "depthq" $ filterQuery queryIsDepth query

-- | Gather the account balance changes into a regular matrix, then
-- accumulate and value amounts, as specified by the report options.
--
-- Makes sure all report columns have an entry.
calculateReportMatrix :: ReportSpec -> Journal -> PriceOracle
                      -> HashMap ClippedAccountName Account
                      -> Map DateSpan [Posting]
                      -> HashMap ClippedAccountName (Map DateSpan Account)
calculateReportMatrix rspec@ReportSpec{rsOpts=ropts} j priceoracle startbals colps =  -- PARTIAL:
    -- Ensure all columns have entries, including those with starting balances
    HM.mapWithKey rowbals allchanges
  where
    -- The valued row amounts to be displayed: per-period changes,
    -- zero-based cumulative totals, or
    -- starting-balance-based historical balances.
    rowbals name changes = dbg5 "rowbals" $ case balancetype_ ropts of
        PeriodChange      -> changeamts
        CumulativeChange  -> cumulative
        HistoricalBalance -> historical
      where
        -- changes to report on: usually just the changes itself, but use the
        -- differences in the historical amount for ValueChangeReports.
        changeamts = case reporttype_ ropts of
            ChangeReport      -> M.mapWithKey avalue changes
            BudgetReport      -> M.mapWithKey avalue changes
            ValueChangeReport -> periodChanges valuedStart historical
        cumulative = cumulativeSum avalue nullacct changeamts
        historical = cumulativeSum avalue startingBalance changes
        startingBalance = HM.lookupDefault nullacct name startbals
        valuedStart = avalue (DateSpan Nothing historicalDate) startingBalance

    -- Transpose to get each account's balance changes across all columns, then
    -- pad with zeros
    allchanges     = ((<>zeros) <$> acctchanges) <> (zeros <$ startbals)
    acctchanges    = dbg5 "acctchanges" . addElided $ transposeMap colacctchanges
    colacctchanges = dbg5 "colacctchanges" $ fmap (acctChangesFromPostings rspec) colps

    avalue = acctApplyBoth . mixedAmountApplyValuationAfterSumFromOptsWith ropts j priceoracle
    acctApplyBoth f a = a{aibalance = f $ aibalance a, aebalance = f $ aebalance a}
    addElided = if queryDepth (rsQuery rspec) == Just 0 then HM.insert "..." zeros else id
    historicalDate = minimumMay $ mapMaybe spanStart colspans
    zeros = M.fromList [(span, nullacct) | span <- colspans]
    colspans = M.keys colps


-- | Lay out a set of postings grouped by date span into a regular matrix with rows
-- given by AccountName and columns by DateSpan, then generate a MultiBalanceReport
-- from the columns.
generateMultiBalanceReport :: ReportSpec -> Journal -> PriceOracle
                           -> Map DateSpan [Posting] -> HashMap AccountName Account
                           -> MultiBalanceReport
generateMultiBalanceReport rspec@ReportSpec{rsOpts=ropts} j priceoracle colps startbals =
    report
  where
    -- Process changes into normal, cumulative, or historical amounts, plus value them
    matrix = calculateReportMatrix rspec j priceoracle startbals colps

    -- All account names that will be displayed, possibly depth-clipped.
    displaynames = dbg5 "displaynames" $ displayedAccounts rspec matrix

    -- All the rows of the report.
    rows = dbg5 "rows" . (if invert_ ropts then map (fmap maNegate) else id)  -- Negate amounts if applicable
             $ buildReportRows ropts displaynames matrix

    -- Calculate column totals
    totalsrow = dbg5 "totalsrow" $ calculateTotalsRow ropts rows

    -- Sorted report rows.
    sortedrows = dbg5 "sortedrows" $ sortRows ropts j rows

    -- Take percentages if needed
    report = reportPercent ropts $ PeriodicReport (M.keys colps) sortedrows totalsrow

-- | Build the report rows.
-- One row per account, with account name info, row amounts, row total and row average.
-- Rows are unsorted.
buildReportRows :: ReportOpts
                -> HashMap AccountName DisplayName
                -> HashMap AccountName (Map DateSpan Account)
                -> [MultiBalanceReportRow]
buildReportRows ropts displaynames =
  toList . HM.mapMaybeWithKey mkRow  -- toList of HashMap's Foldable instance - does not sort consistently
  where
    mkRow name accts = do
        displayname <- HM.lookup name displaynames
        return $ PeriodicReportRow displayname rowbals rowtot rowavg
      where
        rowbals = map balance $ toList accts  -- toList of Map's Foldable instance - does sort by key
        -- The total and average for the row.
        -- These are always simply the sum/average of the displayed row amounts.
        -- Total for a cumulative/historical report is always the last column.
        rowtot = case balancetype_ ropts of
            PeriodChange -> maSum rowbals
            _            -> lastDef nullmixedamt rowbals
        rowavg = averageMixedAmounts rowbals
    balance = case accountlistmode_ ropts of ALTree -> aibalance; ALFlat -> aebalance

-- | Calculate accounts which are to be displayed in the report, as well as
-- their name and depth
displayedAccounts :: ReportSpec -> HashMap AccountName (Map DateSpan Account)
                  -> HashMap AccountName DisplayName
displayedAccounts ReportSpec{rsQuery=query,rsOpts=ropts} valuedaccts
    | depth == 0 = HM.singleton "..." $ DisplayName "..." "..." 1
    | otherwise  = HM.mapWithKey (\a _ -> displayedName a) displayedAccts
  where
    -- Accounts which are to be displayed
    displayedAccts = (if depth == 0 then id else HM.filterWithKey keep) valuedaccts
      where
        keep name amts = isInteresting name amts || name `HM.member` interestingParents

    displayedName name = case accountlistmode_ ropts of
        ALTree -> DisplayName name leaf . max 0 $ level - boringParents
        ALFlat -> DisplayName name droppedName 1
      where
        droppedName = accountNameDrop (drop_ ropts) name
        leaf = accountNameFromComponents . reverse . map accountLeafName $
            droppedName : takeWhile notDisplayed parents

        level = max 0 $ accountNameLevel name - drop_ ropts
        parents = take (level - 1) $ parentAccountNames name
        boringParents = if no_elide_ ropts then 0 else length $ filter notDisplayed parents
        notDisplayed = not . (`HM.member` displayedAccts)

    -- Accounts interesting for their own sake
    isInteresting name amts =
        d <= depth                                     -- Throw out anything too deep
        && ((empty_ ropts && all (null . asubs) amts)  -- Keep all leaves when using empty_
           || not (isZeroRow balance amts))            -- Throw out anything with zero balance
      where
        d = accountNameLevel name
        balance | ALTree <- accountlistmode_ ropts, d == depth = aibalance
                | otherwise = aebalance

    -- Accounts interesting because they are a fork for interesting subaccounts
    interestingParents = dbg5 "interestingParents" $ case accountlistmode_ ropts of
        ALTree -> HM.filterWithKey hasEnoughSubs numSubs
        ALFlat -> mempty
      where
        hasEnoughSubs name nsubs = nsubs >= minSubs && accountNameLevel name > drop_ ropts
        minSubs = if no_elide_ ropts then 1 else 2

    isZeroRow balance = all (mixedAmountLooksZero . balance)
    depth = fromMaybe maxBound $  queryDepth query
    numSubs = subaccountTallies . HM.keys $ HM.filterWithKey isInteresting valuedaccts

-- | Sort the rows by amount or by account declaration order.
sortRows :: ReportOpts -> Journal -> [MultiBalanceReportRow] -> [MultiBalanceReportRow]
sortRows ropts j
    | sort_amount_ ropts, ALTree <- accountlistmode_ ropts = sortTreeMBRByAmount
    | sort_amount_ ropts, ALFlat <- accountlistmode_ ropts = sortFlatMBRByAmount
    | otherwise                                            = sortMBRByAccountDeclaration
  where
    -- Sort the report rows, representing a tree of accounts, by row total at each level.
    -- Similar to sortMBRByAccountDeclaration/sortAccountNamesByDeclaration.
    sortTreeMBRByAmount :: [MultiBalanceReportRow] -> [MultiBalanceReportRow]
    sortTreeMBRByAmount rows = mapMaybe (`HM.lookup` rowMap) sortedanames
      where
        accounttree = accountTree "root" $ map prrFullName rows
        rowMap = HM.fromList $ map (\row -> (prrFullName row, row)) rows
        -- Set the inclusive balance of an account from the rows, or sum the
        -- subaccounts if it's not present
        accounttreewithbals = mapAccounts setibalance accounttree
        setibalance a = a{aibalance = maybe (maSum . map aibalance $ asubs a) prrTotal $
                                          HM.lookup (aname a) rowMap}
        sortedaccounttree = sortAccountTreeByAmount (fromMaybe NormallyPositive $ normalbalance_ ropts) accounttreewithbals
        sortedanames = map aname $ drop 1 $ flattenAccounts sortedaccounttree

    -- Sort the report rows, representing a flat account list, by row total (and then account name).
    sortFlatMBRByAmount :: [MultiBalanceReportRow] -> [MultiBalanceReportRow]
    sortFlatMBRByAmount = case fromMaybe NormallyPositive $ normalbalance_ ropts of
        NormallyPositive -> sortOn (\r -> (Down $ amt r, prrFullName r))
        NormallyNegative -> sortOn (\r -> (amt r, prrFullName r))
      where amt = mixedAmountStripPrices . prrTotal

    -- Sort the report rows by account declaration order then account name.
    sortMBRByAccountDeclaration :: [MultiBalanceReportRow] -> [MultiBalanceReportRow]
    sortMBRByAccountDeclaration rows = sortRowsLike sortedanames rows
      where
        sortedanames = sortAccountNamesByDeclaration j (tree_ ropts) $ map prrFullName rows

-- | Build the report totals row.
--
-- Calculate the column totals. These are always the sum of column amounts.
calculateTotalsRow :: ReportOpts -> [MultiBalanceReportRow] -> PeriodicReportRow () MixedAmount
calculateTotalsRow ropts rows =
    PeriodicReportRow () coltotals grandtotal grandaverage
  where
    isTopRow row = flat_ ropts || not (any (`HM.member` rowMap) parents)
      where parents = init . expandAccountName $ prrFullName row
    rowMap = HM.fromList $ map (\row -> (prrFullName row, row)) rows

    colamts = transpose . map prrAmounts $ filter isTopRow rows

    coltotals :: [MixedAmount] = dbg5 "coltotals" $ map maSum colamts

    -- Calculate the grand total and average. These are always the sum/average
    -- of the column totals.
    -- Total for a cumulative/historical report is always the last column.
    grandtotal = case balancetype_ ropts of
        PeriodChange -> maSum coltotals
        _            -> lastDef nullmixedamt coltotals
    grandaverage = averageMixedAmounts coltotals

-- | Map the report rows to percentages if needed
reportPercent :: ReportOpts -> MultiBalanceReport -> MultiBalanceReport
reportPercent ropts report@(PeriodicReport spans rows totalrow)
  | percent_ ropts = PeriodicReport spans (map percentRow rows) (percentRow totalrow)
  | otherwise      = report
  where
    percentRow (PeriodicReportRow name rowvals rowtotal rowavg) =
      PeriodicReportRow name
        (zipWith perdivide rowvals $ prrAmounts totalrow)
        (perdivide rowtotal $ prrTotal totalrow)
        (perdivide rowavg $ prrAverage totalrow)


-- | Transpose a Map of HashMaps to a HashMap of Maps.
--
-- Makes sure that all DateSpans are present in all rows.
transposeMap :: Map DateSpan (HashMap AccountName a)
             -> HashMap AccountName (Map DateSpan a)
transposeMap = M.foldrWithKey addSpan mempty
  where
    addSpan span acctmap seen = HM.foldrWithKey (addAcctSpan span) seen acctmap

    addAcctSpan span acct a = HM.alter f acct
      where f = Just . M.insert span a . fromMaybe mempty

-- | A sorting helper: sort a list of things (eg report rows) keyed by account name
-- to match the provided ordering of those same account names.
sortRowsLike :: [AccountName] -> [PeriodicReportRow DisplayName b] -> [PeriodicReportRow DisplayName b]
sortRowsLike sortedas rows = mapMaybe (`HM.lookup` rowMap) sortedas
  where rowMap = HM.fromList $ map (\row -> (prrFullName row, row)) rows

-- | Given a list of account names, find all forking parent accounts, i.e.
-- those which fork between different branches
subaccountTallies :: [AccountName] -> HashMap AccountName Int
subaccountTallies = foldr incrementParent mempty . expandAccountNames
  where incrementParent a = HM.insertWith (+) (parentAccountName a) 1

-- | A helper: what percentage is the second mixed amount of the first ?
-- Keeps the sign of the first amount.
-- Uses unifyMixedAmount to unify each argument and then divides them.
-- Both amounts should be in the same, single commodity.
-- This can call error if the arguments are not right.
perdivide :: MixedAmount -> MixedAmount -> MixedAmount
perdivide a b = fromMaybe (error' errmsg) $ do  -- PARTIAL:
    a' <- unifyMixedAmount a
    b' <- unifyMixedAmount b
    guard $ amountIsZero a' || amountIsZero b' || acommodity a' == acommodity b'
    return $ mixed [per $ if aquantity b' == 0 then 0 else aquantity a' / abs (aquantity b') * 100]
  where errmsg = "Cannot calculate percentages if accounts have different commodities (Hint: Try --cost, -V or similar flags.)"

-- Add the values of two accounts. Should be right-biased, since it's used
-- in scanl, so other properties (such as anumpostings) stay in the right place
sumAcct :: Account -> Account -> Account
sumAcct Account{aibalance=i1,aebalance=e1} a@Account{aibalance=i2,aebalance=e2} =
    a{aibalance = i1 `maPlus` i2, aebalance = e1 `maPlus` e2}

-- Subtract the values in one account from another. Should be left-biased.
subtractAcct :: Account -> Account -> Account
subtractAcct a@Account{aibalance=i1,aebalance=e1} Account{aibalance=i2,aebalance=e2} =
    a{aibalance = i1 `maMinus` i2, aebalance = e1 `maMinus` e2}

-- | Extract period changes from a cumulative list
periodChanges :: Account -> Map k Account -> Map k Account
periodChanges start amtmap =
    M.fromDistinctAscList . zip dates $ zipWith subtractAcct amts (start:amts)
  where (dates, amts) = unzip $ M.toAscList amtmap

-- | Calculate a cumulative sum from a list of period changes and a valuation
-- function.
cumulativeSum :: (DateSpan -> Account -> Account) -> Account -> Map DateSpan Account -> Map DateSpan Account
cumulativeSum value start = snd . M.mapAccumWithKey accumValued start
  where accumValued startAmt date newAmt = let s = sumAcct startAmt newAmt in (s, value date s)

-- tests

tests_MultiBalanceReport = tests "MultiBalanceReport" [

  let
    amt0 = Amount {acommodity="$", aquantity=0, aprice=Nothing, astyle=AmountStyle {ascommodityside = L, ascommodityspaced = False, asprecision = Precision 2, asdecimalpoint = Just '.', asdigitgroups = Nothing}, aismultiplier=False}
    (rspec,journal) `gives` r = do
      let rspec' = rspec{rsQuery=And [queryFromFlags $ rsOpts rspec, rsQuery rspec]}
          (eitems, etotal) = r
          (PeriodicReport _ aitems atotal) = multiBalanceReport rspec' journal
          showw (PeriodicReportRow a lAmt amt amt')
              = (displayFull a, displayName a, displayDepth a, map showMixedAmountDebug lAmt, showMixedAmountDebug amt, showMixedAmountDebug amt')
      (map showw aitems) @?= (map showw eitems)
      showMixedAmountDebug (prrTotal atotal) @?= showMixedAmountDebug etotal -- we only check the sum of the totals
  in
   tests "multiBalanceReport" [
      test "null journal"  $
      (defreportspec, nulljournal) `gives` ([], nullmixedamt)

     ,test "with -H on a populated period"  $
      (defreportspec{rsOpts=defreportopts{period_= PeriodBetween (fromGregorian 2008 1 1) (fromGregorian 2008 1 2), balancetype_=HistoricalBalance}}, samplejournal) `gives`
       (
        [ PeriodicReportRow (flatDisplayName "assets:bank:checking") [mamountp' "$1.00"]  (mamountp' "$1.00")  (mixedAmount amt0{aquantity=1})
        , PeriodicReportRow (flatDisplayName "income:salary")        [mamountp' "$-1.00"] (mamountp' "$-1.00") (mixedAmount amt0{aquantity=(-1)})
        ],
        mamountp' "$0.00")

     -- ,test "a valid history on an empty period"  $
     --  (defreportopts{period_= PeriodBetween (fromGregorian 2008 1 2) (fromGregorian 2008 1 3), balancetype_=HistoricalBalance}, samplejournal) `gives`
     --   (
     --    [
     --     ("assets:bank:checking","checking",3, [mamountp' "$1.00"], mamountp' "$1.00",mixedAmount amt0 {aquantity=1})
     --    ,("income:salary","salary",2, [mamountp' "$-1.00"], mamountp' "$-1.00",mixedAmount amt0 {aquantity=(-1)})
     --    ],
     --    mixedAmount usd0)

     -- ,test "a valid history on an empty period (more complex)"  $
     --  (defreportopts{period_= PeriodBetween (fromGregorian 2009 1 1) (fromGregorian 2009 1 2), balancetype_=HistoricalBalance}, samplejournal) `gives`
     --   (
     --    [
     --    ("assets:bank:checking","checking",3, [mamountp' "$1.00"], mamountp' "$1.00",mixedAmount amt0 {aquantity=1})
     --    ,("assets:bank:saving","saving",3, [mamountp' "$1.00"], mamountp' "$1.00",mixedAmount amt0 {aquantity=1})
     --    ,("assets:cash","cash",2, [mamountp' "$-2.00"], mamountp' "$-2.00",mixedAmount amt0 {aquantity=(-2)})
     --    ,("expenses:food","food",2, [mamountp' "$1.00"], mamountp' "$1.00",mixedAmount amt0 {aquantity=(1)})
     --    ,("expenses:supplies","supplies",2, [mamountp' "$1.00"], mamountp' "$1.00",mixedAmount amt0 {aquantity=(1)})
     --    ,("income:gifts","gifts",2, [mamountp' "$-1.00"], mamountp' "$-1.00",mixedAmount amt0 {aquantity=(-1)})
     --    ,("income:salary","salary",2, [mamountp' "$-1.00"], mamountp' "$-1.00",mixedAmount amt0 {aquantity=(-1)})
     --    ],
     --    mixedAmount usd0)
    ]
 ]
