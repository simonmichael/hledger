{-# LANGUAGE CPP                 #-}
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

  CompoundBalanceReport,
  compoundBalanceReport,
  compoundBalanceReportWith,

  tableAsText,

  sortRows,
  sortRowsLike,

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
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Ord (Down(..))
#if !(MIN_VERSION_base(4,11,0))
import Data.Semigroup ((<>))
#endif
import Data.Semigroup (sconcat)
import Data.Time.Calendar (Day, addDays, fromGregorian)
import Safe (headMay, lastDef, lastMay)
import Text.Tabular as T
import Text.Tabular.AsciiWide (render)

import Hledger.Data
import Hledger.Query
import Hledger.Utils
import Hledger.Read (mamountp')
import Hledger.Reports.ReportOptions
import Hledger.Reports.ReportTypes


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
type CompoundBalanceReport = CompoundPeriodicReport DisplayName MixedAmount

-- type alias just to remind us which AccountNames might be depth-clipped, below.
type ClippedAccountName = AccountName



-- | Generate a multicolumn balance report for the matched accounts,
-- showing the change of balance, accumulated balance, or historical balance
-- in each of the specified periods. If the normalbalance_ option is set, it
-- adjusts the sorting and sign of amounts (see ReportOpts and
-- CompoundBalanceCommand). hledger's most powerful and useful report, used
-- by the balance command (in multiperiod mode) and (via compoundBalanceReport)
-- by the bs/cf/is commands.
multiBalanceReport :: Day -> ReportOpts -> Journal -> MultiBalanceReport
multiBalanceReport today ropts j =
    multiBalanceReportWith ropts q j (journalPriceOracle infer j)
  where
    q = queryFromOpts today ropts
    infer = infer_value_ ropts

-- | A helper for multiBalanceReport. This one takes an explicit Query
-- instead of deriving one from ReportOpts, and an extra argument, a
-- PriceOracle to be used for looking up market prices. Commands which
-- run multiple reports (bs etc.) can generate the price oracle just
-- once for efficiency, passing it to each report by calling this
-- function directly.
multiBalanceReportWith :: ReportOpts -> Query -> Journal -> PriceOracle -> MultiBalanceReport
multiBalanceReportWith ropts q j priceoracle = report
  where
    -- Queries, report/column dates.
    reportspan = dbg "reportspan" $ calculateReportSpan ropts q j
    reportq    = dbg "reportq"    $ makeReportQuery ropts reportspan q

    -- Group postings into their columns.
    colps    = dbg'' "colps"  $ getPostingsByColumn ropts reportq j reportspan
    colspans = dbg "colspans" $ M.keys colps

    -- Generate and postprocess the report, negating balances and taking percentages if needed
    report = dbg' "report" $ generateMultiBalanceReport ropts reportq j priceoracle reportspan colspans colps

-- | Generate a compound balance report from a list of CBCSubreportSpec. This
-- shares postings between the subreports.
compoundBalanceReport :: Day -> ReportOpts -> Journal -> [CBCSubreportSpec]
                      -> CompoundBalanceReport
compoundBalanceReport today ropts j =
    compoundBalanceReportWith ropts q j (journalPriceOracle infer j)
  where
    q = queryFromOpts today ropts
    infer = infer_value_ ropts

-- | A helper for compoundBalanceReport, similar to multiBalanceReportWith.
compoundBalanceReportWith :: ReportOpts -> Query -> Journal -> PriceOracle
                          -> [CBCSubreportSpec] -> CompoundBalanceReport
compoundBalanceReportWith ropts q j priceoracle subreportspecs = cbr
  where
    -- Queries, report/column dates.
    reportspan = dbg "reportspan" $ calculateReportSpan ropts q j
    reportq    = dbg "reportq"    $ makeReportQuery ropts reportspan q

    -- Group postings into their columns.
    colps    = dbg'' "colps"  $ getPostingsByColumn ropts{empty_=True} reportq j reportspan
    colspans = dbg "colspans" $ M.keys colps

    -- Filter the column postings according to each subreport
    subreportcolps = map filterSubreport subreportspecs
      where filterSubreport sr = filter (matchesPosting $ cbcsubreportquery sr j) <$> colps

    subreports = zipWith generateSubreport subreportspecs subreportcolps
      where
        generateSubreport CBCSubreportSpec{..} colps' =
            ( cbcsubreporttitle
            -- Postprocess the report, negating balances and taking percentages if needed
            , prNormaliseSign cbcsubreportnormalsign $
                generateMultiBalanceReport ropts' reportq j priceoracle reportspan colspans colps'
            , cbcsubreportincreasestotal
            )
          where
            ropts' = ropts{normalbalance_=Just cbcsubreportnormalsign}

    -- Sum the subreport totals by column. Handle these cases:
    -- - no subreports
    -- - empty subreports, having no subtotals (#588)
    -- - subreports with a shorter subtotals row than the others
    overalltotals = case subreports of
        []     -> PeriodicReportRow () [] nullmixedamt nullmixedamt
        (r:rs) -> sconcat $ fmap subreportTotal (r:|rs)
      where
        subreportTotal (_, sr, increasestotal) =
            (if increasestotal then id else fmap negate) $ prTotals sr

    cbr = CompoundPeriodicReport "" colspans subreports overalltotals


-- | Calculate starting balances, if needed for -H
--
-- Balances at report start date, from all earlier postings which otherwise match the query.
-- These balances are unvalued.
-- TODO: Do we want to check whether to bother calculating these? isHistorical
-- and startDate is not nothing, otherwise mempty? This currently gives a
-- failure with some totals which are supposed to be 0 being blank.
startingBalances :: ReportOpts -> Query -> Journal -> DateSpan -> HashMap AccountName Account
startingBalances ropts q j reportspan = acctchanges
  where
    acctchanges = acctChangesFromPostings ropts' startbalq . map fst $
        getPostings ropts' startbalq j

    -- q projected back before the report start date.
    -- When there's no report start date, in case there are future txns (the hledger-ui case above),
    -- we use emptydatespan to make sure they aren't counted as starting balance.
    startbalq = dbg'' "startbalq" $ And [datelessq, precedingspanq]
    datelessq = dbg "datelessq" $ filterQuery (not . queryIsDateOrDate2) q

    ropts' = case accountlistmode_ ropts of
        ALTree -> ropts{no_elide_=True, period_=precedingperiod}
        ALFlat -> ropts{period_=precedingperiod}

    precedingperiod = dateSpanAsPeriod . spanIntersect precedingspan .
                         periodAsDateSpan $ period_ ropts
    precedingspan = DateSpan Nothing $ spanStart reportspan
    precedingspanq = (if date2_ ropts then Date2 else Date) $ case precedingspan of
        DateSpan Nothing Nothing -> emptydatespan
        a -> a

-- | Calculate the span of the report to be generated.
calculateReportSpan :: ReportOpts -> Query -> Journal -> DateSpan
calculateReportSpan ropts q j = reportspan
  where
    -- The date span specified by -b/-e/-p options and query args if any.
    requestedspan  = dbg "requestedspan" $ queryDateSpan (date2_ ropts) q
    -- If the requested span is open-ended, close it using the journal's end dates.
    -- This can still be the null (open) span if the journal is empty.
    requestedspan' = dbg "requestedspan'" $
        requestedspan `spanDefaultsFrom` journalDateSpan (date2_ ropts) j
    -- The list of interval spans enclosing the requested span.
    -- This list can be empty if the journal was empty,
    -- or if hledger-ui has added its special date:-tomorrow to the query
    -- and all txns are in the future.
    intervalspans  = dbg "intervalspans" $ splitSpan (interval_ ropts) requestedspan'
    -- The requested span enlarged to enclose a whole number of intervals.
    -- This can be the null span if there were no intervals.
    reportspan = DateSpan (spanStart =<< headMay intervalspans)
                          (spanEnd =<< lastMay intervalspans)

-- | Remove any date queries and insert queries from the report span.
-- The user's query expanded to the report span
-- if there is one (otherwise any date queries are left as-is, which
-- handles the hledger-ui+future txns case above).
makeReportQuery :: ReportOpts -> DateSpan -> Query -> Query
makeReportQuery ropts reportspan q
    | reportspan == nulldatespan = q
    | otherwise = And [dateless q, reportspandatesq]
  where
    reportspandatesq = dbg "reportspandatesq" $ dateqcons reportspan
    dateless   = dbg "dateless" . filterQuery (not . queryIsDateOrDate2)
    dateqcons  = if date2_ ropts then Date2 else Date

-- | Group postings, grouped by their column
getPostingsByColumn :: ReportOpts -> Query -> Journal -> DateSpan -> Map DateSpan [Posting]
getPostingsByColumn ropts q j reportspan = columns
  where
    -- Postings matching the query within the report period.
    ps :: [(Posting, Day)] = dbg'' "ps" $ getPostings ropts q j
    days = map snd ps

    -- The date spans to be included as report columns.
    colspans = calculateColSpans ropts reportspan days
    addPosting (p, d) = maybe id (M.adjust (p:)) $ latestSpanContaining colspans d
    emptyMap = M.fromList . zip colspans $ repeat []

    -- Group postings into their columns
    columns = foldr addPosting emptyMap ps

-- | Gather postings matching the query within the report period.
getPostings :: ReportOpts -> Query -> Journal -> [(Posting, Day)]
getPostings ropts q =
    map (\p -> (p, date p)) .
    journalPostings .
    filterJournalAmounts symq .    -- remove amount parts excluded by cur:
    filterJournalPostings reportq  -- remove postings not matched by (adjusted) query
  where
    symq = dbg "symq" . filterQuery queryIsSym $ dbg "requested q" q
    -- The user's query with no depth limit, and expanded to the report span
    -- if there is one (otherwise any date queries are left as-is, which
    -- handles the hledger-ui+future txns case above).
    reportq = dbg "reportq" $ depthless q
    depthless = dbg "depthless" . filterQuery (not . queryIsDepth)

    date = case whichDateFromOpts ropts of
        PrimaryDate   -> postingDate
        SecondaryDate -> postingDate2

-- | Calculate the DateSpans to be used for the columns of the report.
calculateColSpans :: ReportOpts -> DateSpan -> [Day] -> [DateSpan]
calculateColSpans ropts reportspan days =
    splitSpan (interval_ ropts) displayspan
  where
    displayspan
      | empty_ ropts = dbg "displayspan (-E)" reportspan                        -- all the requested intervals
      | otherwise = dbg "displayspan" $ reportspan `spanIntersect` matchedspan  -- exclude leading/trailing empty intervals
    matchedspan = dbg "matchedspan" $ daysSpan days


-- | Gather the account balance changes into a regular matrix
-- including the accounts from all columns.
calculateAccountChanges :: ReportOpts -> Query -> [DateSpan]
                        -> Map DateSpan [Posting]
                        -> HashMap ClippedAccountName (Map DateSpan Account)
calculateAccountChanges ropts q colspans colps
    | queryDepth q == Just 0 = acctchanges <> elided
    | otherwise              = acctchanges
  where
    -- Transpose to get each account's balance changes across all columns.
    acctchanges = transposeMap colacctchanges

    colacctchanges :: Map DateSpan (HashMap ClippedAccountName Account) =
      dbg'' "colacctchanges" $ fmap (acctChangesFromPostings ropts q) colps

    elided = HM.singleton "..." $ M.fromList [(span, nullacct) | span <- colspans]

-- | Given a set of postings, eg for a single report column, gather
-- the accounts that have postings and calculate the change amount for
-- each. Accounts and amounts will be depth-clipped appropriately if
-- a depth limit is in effect.
acctChangesFromPostings :: ReportOpts -> Query -> [Posting] -> HashMap ClippedAccountName Account
acctChangesFromPostings ropts q ps = HM.fromList [(aname a, a) | a <- as]
  where
    as = filterAccounts . drop 1 $ accountsFromPostings ps
    filterAccounts = case accountlistmode_ ropts of
        ALTree -> filter ((depthq `matchesAccount`) . aname)      -- exclude deeper balances
        ALFlat -> clipAccountsAndAggregate (queryDepth depthq) .  -- aggregate deeper balances at the depth limit.
                      filter ((0<) . anumpostings)
    depthq = dbg "depthq" $ filterQuery queryIsDepth q

-- | Accumulate and value amounts, as specified by the report options.
--
-- Makes sure all report columns have an entry.
accumValueAmounts :: ReportOpts -> Journal -> PriceOracle -> [DateSpan]
                  -> HashMap ClippedAccountName Account
                  -> HashMap ClippedAccountName (Map DateSpan Account)
                  -> HashMap ClippedAccountName (Map DateSpan Account)
accumValueAmounts ropts j priceoracle colspans startbals acctchanges =  -- PARTIAL:
    HM.mapWithKey processRow $ acctchanges <> (mempty <$ startbals)
  where
    -- Must accumulate before valuing, since valuation can change without any
    -- postings. Make sure every column has an entry.
    processRow name changes = M.mapWithKey valueAcct . rowbals name $ changes <> zeros

    -- The row amounts to be displayed: per-period changes,
    -- zero-based cumulative totals, or
    -- starting-balance-based historical balances.
    rowbals name changes = dbg'' "rowbals" $ case balancetype_ ropts of
        PeriodChange      -> changes
        CumulativeChange  -> snd $ M.mapAccum f nullacct                  changes
        HistoricalBalance -> snd $ M.mapAccum f (startingBalanceFor name) changes
      where f a b = let s = sumAcct a b in (s, s)

    -- Add the values of two accounts. Should be right-biased, since it's used
    -- in scanl, so other properties (such as anumpostings) stay in the right place
    sumAcct Account{aibalance=i1,aebalance=e1} a@Account{aibalance=i2,aebalance=e2} =
        a{aibalance = i1 + i2, aebalance = e1 + e2}

    -- We may be converting amounts to value, per hledger_options.m4.md "Effect of --value on reports".
    valueAcct (DateSpan _ (Just end)) acct =
        acct{aibalance = value (aibalance acct), aebalance = value (aebalance acct)}
      where value = avalue (addDays (-1) end)
    valueAcct _ _ = error' "multiBalanceReport: expected all spans to have an end date"  -- XXX should not happen

    avalue periodlast = maybe id
        (mixedAmountApplyValuation priceoracle styles periodlast mreportlast today multiperiod) $
        value_ ropts
      where
        -- Some things needed if doing valuation.
        styles = journalCommodityStyles j
        mreportlast = reportPeriodLastDay ropts
        today = fromMaybe (error' "multiBalanceReport: could not pick a valuation date, ReportOpts today_ is unset") $ today_ ropts  -- XXX shouldn't happen
        multiperiod = interval_ ropts /= NoInterval

    startingBalanceFor a = HM.lookupDefault nullacct a startbals
    zeros = M.fromList [(span, nullacct) | span <- colspans]


-- | Lay out a set of postings grouped by date span into a regular matrix with rows
-- given by AccountName and columns by DateSpan, then generate a MultiBalanceReport
-- from the columns.
generateMultiBalanceReport :: ReportOpts -> Query -> Journal -> PriceOracle
                           -> DateSpan -> [DateSpan]
                           -> Map DateSpan [Posting]
                           -> MultiBalanceReport
generateMultiBalanceReport ropts q j priceoracle reportspan colspans colps = report
  where
    -- Each account's balance changes across all columns.
    acctchanges = dbg'' "acctchanges" $ calculateAccountChanges ropts q colspans colps

    -- The matched accounts with a starting balance. All of these should appear
    -- in the report, even if they have no postings during the report period.
    startbals = dbg' "startbals" $ startingBalances ropts q j reportspan

    -- Process changes into normal, cumulative, or historical amounts, plus value them
    accumvalued = accumValueAmounts ropts j priceoracle colspans startbals acctchanges

    -- All account names that will be displayed, possibly depth-clipped.
    displaynames = dbg'' "displaynames" $ displayedAccounts ropts q accumvalued

    -- All the rows of the report.
    rows = dbg'' "rows" $ buildReportRows ropts displaynames accumvalued

    -- Calculate column totals
    totalsrow = dbg' "totalsrow" $ calculateTotalsRow ropts rows

    -- Sorted report rows.
    sortedrows = dbg' "sortedrows" $ sortRows ropts j rows

    -- Postprocess the report, negating balances and taking percentages if needed
    report = postprocessReport ropts $ PeriodicReport colspans sortedrows totalsrow

-- | Build the report rows.
--
-- One row per account, with account name info, row amounts, row total and row average.
buildReportRows :: ReportOpts
                -> HashMap AccountName DisplayName
                -> HashMap AccountName (Map DateSpan Account)
                -> [MultiBalanceReportRow]
buildReportRows ropts displaynames = toList . HM.mapMaybeWithKey mkRow
  where
    mkRow name accts = do
        displayname <- HM.lookup name displaynames
        return $ PeriodicReportRow displayname rowbals rowtot rowavg
      where
        rowbals = map balance $ toList accts
        -- The total and average for the row.
        -- These are always simply the sum/average of the displayed row amounts.
        -- Total for a cumulative/historical report is always the last column.
        rowtot = case balancetype_ ropts of
            PeriodChange -> sum rowbals
            _            -> lastDef 0 rowbals
        rowavg = averageMixedAmounts rowbals
    balance = case accountlistmode_ ropts of ALTree -> aibalance; ALFlat -> aebalance

-- | Calculate accounts which are to be displayed in the report, as well as
-- their name and depth
displayedAccounts :: ReportOpts -> Query
                  -> HashMap AccountName (Map DateSpan Account)
                  -> HashMap AccountName DisplayName
displayedAccounts ropts q valuedaccts
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
    interestingParents = dbg'' "interestingParents" $ case accountlistmode_ ropts of
        ALTree -> HM.filterWithKey hasEnoughSubs numSubs
        ALFlat -> mempty
      where
        hasEnoughSubs name nsubs = nsubs >= minSubs && accountNameLevel name > drop_ ropts
        minSubs = if no_elide_ ropts then 1 else 2

    isZeroRow balance = all (mixedAmountLooksZero . balance)
    depth = fromMaybe maxBound $ queryDepth q
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
        setibalance a = a{aibalance = maybe (sum . map aibalance $ asubs a) prrTotal $
                                          HM.lookup (aname a) rowMap}
        sortedaccounttree = sortAccountTreeByAmount (fromMaybe NormallyPositive $ normalbalance_ ropts) accounttreewithbals
        sortedanames = map aname $ drop 1 $ flattenAccounts sortedaccounttree

    -- Sort the report rows, representing a flat account list, by row total.
    sortFlatMBRByAmount :: [MultiBalanceReportRow] -> [MultiBalanceReportRow]
    sortFlatMBRByAmount = case normalbalance_ ropts of
        Just NormallyNegative -> sortOn amt
        _                     -> sortOn (Down . amt)
      where amt = normaliseMixedAmountSquashPricesForDisplay . prrTotal

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

    coltotals :: [MixedAmount] = dbg'' "coltotals" $ map sum colamts

    -- Calculate the grand total and average. These are always the sum/average
    -- of the column totals.
    -- Total for a cumulative/historical report is always the last column.
    grandtotal = case balancetype_ ropts of
        PeriodChange -> sum coltotals
        _            -> lastDef 0 coltotals
    grandaverage = averageMixedAmounts coltotals

-- | Map the report rows to percentages and negate if needed
postprocessReport :: ReportOpts -> MultiBalanceReport -> MultiBalanceReport
postprocessReport ropts = maybePercent . maybeInvert
  where
    maybeInvert  = if invert_  ropts then fmap negate else id
    maybePercent = if percent_ ropts then prPercent   else id

    prPercent (PeriodicReport spans rows totalrow) =
        PeriodicReport spans (map percentRow rows) (percentRow totalrow)
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
transposeMap xs = M.foldrWithKey addSpan mempty xs
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
  where
    incrementParent a = HM.insertWith (+) (parentAccountName a) 1

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

-- Local debug helper
-- add a prefix to this function's debug output
dbg   s = let p = "multiBalanceReport" in Hledger.Utils.dbg3 (p++" "++s)
dbg'  s = let p = "multiBalanceReport" in Hledger.Utils.dbg4 (p++" "++s)
dbg'' s = let p = "multiBalanceReport" in Hledger.Utils.dbg5 (p++" "++s)
-- dbg = const id  -- exclude this function from debug output

-- common rendering helper, XXX here for now
tableAsText :: ReportOpts -> (a -> String) -> Table String String a -> String
tableAsText (ReportOpts{pretty_tables_ = pretty}) showcell =
  unlines
  . trimborder
  . lines
  . render pretty id id showcell
  . align
  where
    trimborder = drop 1 . init . map (drop 1 . init)
    align (Table l t d) = Table l' t d
      where
        acctswidth = maximum' $ map strWidth (headerContents l)
        l'         = padRightWide acctswidth <$> l

-- tests

tests_MultiBalanceReport = tests "MultiBalanceReport" [

  let
    amt0 = Amount {acommodity="$", aquantity=0, aprice=Nothing, astyle=AmountStyle {ascommodityside = L, ascommodityspaced = False, asprecision = Precision 2, asdecimalpoint = Just '.', asdigitgroups = Nothing}, aismultiplier=False}
    (opts,journal) `gives` r = do
      let (eitems, etotal) = r
          (PeriodicReport _ aitems atotal) = multiBalanceReport nulldate opts journal
          showw (PeriodicReportRow a lAmt amt amt')
              = (displayFull a, displayName a, displayDepth a, map showMixedAmountDebug lAmt, showMixedAmountDebug amt, showMixedAmountDebug amt')
      (map showw aitems) @?= (map showw eitems)
      showMixedAmountDebug (prrTotal atotal) @?= showMixedAmountDebug etotal -- we only check the sum of the totals
  in
   tests "multiBalanceReport" [
      test "null journal"  $
      (defreportopts, nulljournal) `gives` ([], Mixed [nullamt])

     ,test "with -H on a populated period"  $
      (defreportopts{period_= PeriodBetween (fromGregorian 2008 1 1) (fromGregorian 2008 1 2), balancetype_=HistoricalBalance}, samplejournal) `gives`
       (
        [ PeriodicReportRow (flatDisplayName "assets:bank:checking") [mamountp' "$1.00"]  (mamountp' "$1.00")  (Mixed [amt0 {aquantity=1}])
        , PeriodicReportRow (flatDisplayName "income:salary")        [mamountp' "$-1.00"] (mamountp' "$-1.00") (Mixed [amt0 {aquantity=(-1)}])
        ],
        mamountp' "$0.00")

     -- ,test "a valid history on an empty period"  $
     --  (defreportopts{period_= PeriodBetween (fromGregorian 2008 1 2) (fromGregorian 2008 1 3), balancetype_=HistoricalBalance}, samplejournal) `gives`
     --   (
     --    [
     --     ("assets:bank:checking","checking",3, [mamountp' "$1.00"], mamountp' "$1.00",Mixed [amt0 {aquantity=1}])
     --    ,("income:salary","salary",2, [mamountp' "$-1.00"], mamountp' "$-1.00",Mixed [amt0 {aquantity=(-1)}])
     --    ],
     --    Mixed [usd0])

     -- ,test "a valid history on an empty period (more complex)"  $
     --  (defreportopts{period_= PeriodBetween (fromGregorian 2009 1 1) (fromGregorian 2009 1 2), balancetype_=HistoricalBalance}, samplejournal) `gives`
     --   (
     --    [
     --    ("assets:bank:checking","checking",3, [mamountp' "$1.00"], mamountp' "$1.00",Mixed [amt0 {aquantity=1}])
     --    ,("assets:bank:saving","saving",3, [mamountp' "$1.00"], mamountp' "$1.00",Mixed [amt0 {aquantity=1}])
     --    ,("assets:cash","cash",2, [mamountp' "$-2.00"], mamountp' "$-2.00",Mixed [amt0 {aquantity=(-2)}])
     --    ,("expenses:food","food",2, [mamountp' "$1.00"], mamountp' "$1.00",Mixed [amt0 {aquantity=(1)}])
     --    ,("expenses:supplies","supplies",2, [mamountp' "$1.00"], mamountp' "$1.00",Mixed [amt0 {aquantity=(1)}])
     --    ,("income:gifts","gifts",2, [mamountp' "$-1.00"], mamountp' "$-1.00",Mixed [amt0 {aquantity=(-1)}])
     --    ,("income:salary","salary",2, [mamountp' "$-1.00"], mamountp' "$-1.00",Mixed [amt0 {aquantity=(-1)}])
     --    ],
     --    Mixed [usd0])
    ]
 ]
