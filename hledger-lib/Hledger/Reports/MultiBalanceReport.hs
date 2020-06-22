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
  balanceReportFromMultiBalanceReport,
  tableAsText,

  sortAccountItemsLike,

  -- -- * Tests
  tests_MultiBalanceReport
)
where

import Data.List
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Ord
#if !(MIN_VERSION_base(4,11,0))
import Data.Semigroup ((<>))
#endif
import Data.Time.Calendar
import Safe
import Text.Tabular as T
import Text.Tabular.AsciiWide

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

-- type alias just to remind us which AccountNames might be depth-clipped, below.
type ClippedAccountName = AccountName

-- | Generate a multicolumn balance report for the matched accounts,
-- showing the change of balance, accumulated balance, or historical balance
-- in each of the specified periods. Does not support tree-mode boring parent eliding.
-- If the normalbalance_ option is set, it adjusts the sorting and sign of amounts
-- (see ReportOpts and CompoundBalanceCommand).
-- hledger's most powerful and useful report, used by the balance
-- command (in multiperiod mode) and (via multiBalanceReport') by the bs/cf/is commands.
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
    ropts'     = dbg "ropts'"     $ setDefaultAccountListMode ALFlat ropts
    reportspan = dbg "reportspan" $ calculateReportSpan ropts' q j
    reportq    = dbg "reportq"    $ makeReportQuery ropts' reportspan q

    -- The matched accounts with a starting balance. All of these should appear
    -- in the report, even if they have no postings during the report period.
    startbals = dbg' "startbals" $ startingBalances ropts' reportq j reportspan

    -- Postings matching the query within the report period.
    ps :: [(Posting, Day)] = dbg'' "ps" $ getPostings ropts' reportq j
    days = map snd ps

    -- The date spans to be included as report columns.
    colspans = dbg "colspans" $ calculateColSpans ropts' reportspan days

    -- Group postings into their columns.
    colps = dbg'' "colps" $ calculateColumns colspans ps

    -- Each account's balance changes across all columns.
    acctchanges = dbg'' "acctchanges" $ calculateAccountChanges ropts' q colspans startbals colps

    -- Process changes into normal, cumulative, or historical amounts, plus value them
    accumvalued = dbg'' "accumvalued" $ accumValueAmounts ropts' j priceoracle colspans startbals acctchanges

    -- All account names that will be displayed, possibly depth-clipped.
    displayaccts = dbg'' "displayaccts" $ displayedAccounts ropts' q accumvalued

    -- All the rows of the report.
    rows = dbg'' "rows" $ buildReportRows ropts' accumvalued

    -- Sorted report rows.
    sortedrows = dbg' "sortedrows" $ sortRows ropts' j rows

    -- Calculate column totals
    totalsrow = dbg' "totalsrow" $ calculateTotalsRow ropts' displayaccts sortedrows

    -- Postprocess the report, negating balances and taking percentages if needed
    report = dbg' "report" . postprocessReport ropts' displayaccts $
        PeriodicReport colspans sortedrows totalsrow


-- | Calculate the span of the report to be generated.
setDefaultAccountListMode :: AccountListMode -> ReportOpts -> ReportOpts
setDefaultAccountListMode def ropts = ropts{accountlistmode_=mode}
  where
    mode = case accountlistmode_ ropts of
        ALDefault -> def
        a         -> a

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

    ropts' | tree_ ropts = ropts{no_elide_=True, period_=precedingperiod}
           | otherwise   = ropts{accountlistmode_=ALFlat, period_=precedingperiod}

    precedingperiod = dateSpanAsPeriod . spanIntersect precedingspan .
                         periodAsDateSpan $ period_ ropts
    precedingspan = DateSpan Nothing $ spanStart reportspan
    precedingspanq = (if date2_ ropts then Date2 else Date) $ case precedingspan of
        DateSpan Nothing Nothing -> emptydatespan
        a -> a

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

-- | Group postings into their columns.
calculateColumns :: [DateSpan] -> [(Posting, Day)] -> Map DateSpan [Posting]
calculateColumns colspans = foldr addPosting emptyMap
  where
    addPosting (p, d) = maybe id (M.adjust (p:)) $ latestSpanContaining colspans d
    emptyMap = M.fromList . zip colspans $ repeat []

-- | Calculate account balance changes in each column.
--
-- In each column, gather the accounts that have postings and their change amount.
acctChangesFromPostings :: ReportOpts -> Query -> [Posting] -> HashMap ClippedAccountName Account
acctChangesFromPostings ropts q ps = HM.fromList [(aname a, a) | a <- as]
  where
    as = filterAccounts . drop 1 $ accountsFromPostings ps
    filterAccounts
      | tree_ ropts = filter ((depthq `matchesAccount`) . aname)      -- exclude deeper balances
      | otherwise   = clipAccountsAndAggregate (queryDepth depthq) .  -- aggregate deeper balances at the depth limit.
                      filter ((0<) . anumpostings)
    depthq = dbg "depthq" $ filterQuery queryIsDepth q

-- | Gather the account balance changes into a regular matrix including the accounts
-- from all columns
calculateAccountChanges :: ReportOpts -> Query -> [DateSpan]
                        -> HashMap ClippedAccountName Account
                        -> Map DateSpan [Posting]
                        -> HashMap ClippedAccountName (Map DateSpan Account)
calculateAccountChanges ropts q colspans startbals colps
    | queryDepth q == 0 = acctchanges <> elided
    | otherwise         = acctchanges
  where
    -- Transpose to get each account's balance changes across all columns.
    acctchanges = transposeMap colacctchanges <> (mempty <$ startbals)

    colacctchanges :: Map DateSpan (HashMap ClippedAccountName Account) =
      dbg'' "colacctchanges" $ fmap (acctChangesFromPostings ropts q) colps

    elided = HM.singleton "..." $ M.fromList [(span, nullacct) | span <- colspans]

-- | Accumulate and value amounts, as specified by the report options.
--
-- Makes sure all report columns have an entry.
accumValueAmounts :: ReportOpts -> Journal -> PriceOracle -> [DateSpan]
                  -> HashMap ClippedAccountName Account
                  -> HashMap ClippedAccountName (Map DateSpan Account)
                  -> HashMap ClippedAccountName [Account]
accumValueAmounts ropts j priceoracle colspans startbals = HM.mapWithKey processRow
  where
    -- Must accumulate before valuing, since valuation can change without any
    -- postings
    processRow name col = zipWith valueAcct spans $ rowbals name amts
      where (spans, amts) = unzip . M.toList $ col <> zeros

    -- The row amounts to be displayed: per-period changes,
    -- zero-based cumulative totals, or
    -- starting-balance-based historical balances.
    rowbals name changes = dbg'' "rowbals" $ case balancetype_ ropts of
        PeriodChange      -> changes
        CumulativeChange  -> drop 1 $ scanl sumAcct nullacct                  changes
        HistoricalBalance -> drop 1 $ scanl sumAcct (startingBalanceFor name) changes

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

-- | Build the report rows.
--
-- One row per account, with account name info, row amounts, row total and row average.
buildReportRows :: ReportOpts -> HashMap AccountName [Account] -> [MultiBalanceReportRow]
buildReportRows ropts acctvalues =
    [ PeriodicReportRow (flatDisplayName a) rowbals rowtot rowavg
    | (a,accts) <- HM.toList acctvalues
    , let rowbals = map balance accts
    -- The total and average for the row.
    -- These are always simply the sum/average of the displayed row amounts.
    -- Total for a cumulative/historical report is always zero.
    , let rowtot = if balancetype_ ropts == PeriodChange then sum rowbals else 0
    , let rowavg = averageMixedAmounts rowbals
    ]
  where balance = if tree_ ropts then aibalance else aebalance

-- | Calculate accounts which are to be displayed in the report, as well as
-- their name and depth
displayedAccounts :: ReportOpts -> Query
                  -> HashMap AccountName [Account]
                  -> HashMap AccountName DisplayName
displayedAccounts ropts q valuedaccts
    | depth == 0 = HM.singleton "..." $ DisplayName "..." "..." 0
    | otherwise  = HM.mapWithKey (\a _ -> displayedName a) displayedAccts
  where
    -- Accounts which are to be displayed
    displayedAccts = HM.filterWithKey keep valuedaccts
      where
        keep name amts = isInteresting name amts || isInterestingParent name

    isDisplayed = (`HM.member` displayedAccts)

    displayedName name
        | flat_ ropts = DisplayName name droppedName 0
        | otherwise   = DisplayName name leaf d
      where
        leaf = accountNameFromComponents . reverse . map accountLeafName $
            droppedName : takeWhile (not . isDisplayed) parents
        d | no_elide_ ropts = accountNameLevel droppedName
          | otherwise       = accountNameLevel droppedName - length boringParents
        boringParents = filter (not . isDisplayed) parents
        parents = parentAccountNames droppedName
        droppedName = accountNameDrop (drop_ ropts) name

    -- Accounts interesting for their own sake
    interestingAccounts = dbg'' "interestingAccounts" $
        HM.filterWithKey isInteresting valuedaccts

    isInteresting name amts =
        d <= depth                                      -- Throw out anything too deep
        && (keepEmpty || not (isZeroRow balance amts))  -- Boring because has only zero entries
      where
        d = accountNameLevel name
        balance = if tree_ ropts && d == depth then aibalance else aebalance

    -- Accounts interesting because they are a fork for interesting subaccounts
    interestingParents = dbg'' "interestingParents" $
        HM.filterWithKey (\name i -> i > 1 && accountNameLevel name > drop_ ropts) .
        subaccountTallies $ HM.keys interestingAccounts

    isInterestingParent
        | flat_ ropts                     = const False
        | empty_ ropts || no_elide_ ropts = const True
        | otherwise                       = (`HM.member` interestingParents)

    isZeroRow balance = all (mixedAmountLooksZero . balance)
    keepEmpty = empty_ ropts || depth == 0
    depth = queryDepth q

-- | Sort the rows by amount or by account declaration order. This is a bit tricky.
-- TODO: is it always ok to sort report rows after report has been generated, as a separate step ?
sortRows :: ReportOpts -> Journal -> [MultiBalanceReportRow] -> [MultiBalanceReportRow]
sortRows ropts j
    | sort_amount_ ropts && accountlistmode_ ropts == ALTree = sortTreeMBRByAmount
    | sort_amount_ ropts                                     = sortFlatMBRByAmount
    | otherwise                                              = sortMBRByAccountDeclaration
  where
    -- Sort the report rows, representing a tree of accounts, by row total at each level.
    -- Similar to sortMBRByAccountDeclaration/sortAccountNamesByDeclaration.
    sortTreeMBRByAmount :: [MultiBalanceReportRow] -> [MultiBalanceReportRow]
    sortTreeMBRByAmount rows = sortedrows
      where
        anamesandrows = [(prrFullName r, r) | r <- rows]
        anames = map fst anamesandrows
        atotals = [(prrFullName r, prrTotal r) | r <- rows]
        accounttree = accountTree "root" anames
        accounttreewithbals = mapAccounts setibalance accounttree
          where
            -- should not happen, but it's dangerous; TODO
            setibalance a = a{aibalance=fromMaybe (error "sortTreeMBRByAmount 1") $ lookup (aname a) atotals}
        sortedaccounttree = sortAccountTreeByAmount (fromMaybe NormallyPositive $ normalbalance_ ropts) accounttreewithbals
        sortedanames = map aname $ drop 1 $ flattenAccounts sortedaccounttree
        sortedrows = sortAccountItemsLike sortedanames anamesandrows

    -- Sort the report rows, representing a flat account list, by row total.
    sortFlatMBRByAmount = sortBy (maybeflip $ comparing (normaliseMixedAmountSquashPricesForDisplay . prrTotal))
      where
        maybeflip = if normalbalance_ ropts == Just NormallyNegative then id else flip

    -- Sort the report rows by account declaration order then account name.
    sortMBRByAccountDeclaration rows = sortedrows
      where
        anamesandrows = [(prrFullName r, r) | r <- rows]
        anames = map fst anamesandrows
        sortedanames = sortAccountNamesByDeclaration j (tree_ ropts) anames
        sortedrows = sortAccountItemsLike sortedanames anamesandrows

-- | Build the report totals row.
--
-- Calculate the column totals. These are always the sum of column amounts.
calculateTotalsRow :: ReportOpts -> HashMap ClippedAccountName DisplayName
                   -> [MultiBalanceReportRow] -> PeriodicReportRow () MixedAmount
calculateTotalsRow ropts displayaccts rows =
    PeriodicReportRow () coltotals grandtotal grandaverage
  where
    highestlevelaccts = HM.filterWithKey (\a _ -> isHighest a) displayaccts
      where isHighest = not . any (`HM.member` displayaccts) . init . expandAccountName

    colamts = transpose . map prrAmounts $ filter isHighest rows
      where isHighest row = not (tree_ ropts) || prrFullName row `HM.member` highestlevelaccts

    -- TODO: If colamts is null, then this is empty. Do we want it to be a full
    -- column of zeros?
    coltotals :: [MixedAmount] = dbg'' "coltotals" $ map sum colamts

    -- Calculate the grand total and average. These are always the sum/average
    -- of the column totals.
    grandtotal  = if balancetype_ ropts == PeriodChange then sum coltotals else 0
    grandaverage = averageMixedAmounts coltotals

-- | Map the report rows to percentages and negate if needed
postprocessReport :: ReportOpts -> HashMap AccountName DisplayName
                  -> MultiBalanceReport -> MultiBalanceReport
postprocessReport ropts displaynames =
    maybeInvert . maybePercent . setNames
  where
    setNames = prMapMaybeName $ (`HM.lookup` displaynames) . displayFull

    maybeInvert  = if invert_  ropts then prNegate  else id
    maybePercent = if percent_ ropts then prPercent else id

    prPercent (PeriodicReport spans rows totalrow) =
        PeriodicReport spans (map percentRow rows) (percentRow totalrow)
      where
        percentRow (PeriodicReportRow name rowvals rowtotal rowavg) =
            PeriodicReportRow name
                (zipWith perdivide rowvals $ prrAmounts totalrow)
                (perdivide rowtotal $ prrTotal totalrow)
                (perdivide rowavg $ prrAverage totalrow)


-- | Generates a simple non-columnar BalanceReport, but using multiBalanceReport,
-- in order to support --historical. Does not support tree-mode boring parent eliding.
-- If the normalbalance_ option is set, it adjusts the sorting and sign of amounts
-- (see ReportOpts and CompoundBalanceCommand).
balanceReportFromMultiBalanceReport :: ReportOpts -> Query -> Journal
    -> ([(AccountName, AccountName, Int, MixedAmount)], MixedAmount)
balanceReportFromMultiBalanceReport ropts q j = (rows', total)
  where
    PeriodicReport _ rows (PeriodicReportRow _ totals _ _) =
        multiBalanceReportWith ropts' q j (journalPriceOracle (infer_value_ ropts) j)
    rows' = [( displayFull a
             , displayName a
             , if tree_ ropts' then displayDepth a - 1 else 0  -- BalanceReport uses 0-based account depths
             , headDef nullmixedamt amts     -- 0 columns is illegal, should not happen, return zeroes if it does
             ) | PeriodicReportRow a amts _ _ <- rows]
    total = headDef nullmixedamt totals
    ropts' = setDefaultAccountListMode ALTree ropts


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
sortAccountItemsLike :: [AccountName] -> [(AccountName, b)] -> [b]
sortAccountItemsLike sortedas items = mapMaybe (`lookup` items) sortedas

-- | Given a list of account names, find all forking parent accounts, i.e.
-- those which fork between different branches
subaccountTallies :: [AccountName] -> HashMap AccountName Int
subaccountTallies as = foldr incrementParent mempty allaccts
  where
    allaccts = expandAccountNames as
    incrementParent a = HM.insertWith (+) (parentAccountName a) 1

-- | Helper to unify a MixedAmount to a single commodity value.
-- Like normaliseMixedAmount, this consolidates amounts of the same commodity
-- and discards zero amounts; but this one insists on simplifying to
-- a single commodity, and will throw a program-terminating error if
-- this is not possible.
unifyMixedAmount :: MixedAmount -> Amount
unifyMixedAmount mixedAmount = foldl combine (num 0) (amounts mixedAmount)
  where
    combine amount result =
      if amountIsZero amount
      then result
      else if amountIsZero result
        then amount
        else if acommodity amount == acommodity result
          then amount + result
          else error' "Cannot calculate percentages for accounts with multiple commodities. (Hint: Try --cost, -V or similar flags.)"

-- | Helper to calculate the percentage from two mixed. Keeps the sign of the first argument.
-- Uses unifyMixedAmount to unify each argument and then divides them.
perdivide :: MixedAmount -> MixedAmount -> MixedAmount
perdivide a b =
  let a' = unifyMixedAmount a
      b' = unifyMixedAmount b
  in if amountIsZero a' || amountIsZero b' || acommodity a' == acommodity b'
    then mixed [per $ if aquantity b' == 0 then 0 else (aquantity a' / abs (aquantity b') * 100)]
    else error' "Cannot calculate percentages if accounts have different commodities. (Hint: Try --cost, -V or similar flags.)"

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
    amt0 = Amount {acommodity="$", aquantity=0, aprice=Nothing, astyle=AmountStyle {ascommodityside = L, ascommodityspaced = False, asprecision = 2, asdecimalpoint = Just '.', asdigitgroups = Nothing}, aismultiplier=False}
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
        [ PeriodicReportRow (flatDisplayName "assets:bank:checking") [mamountp' "$1.00"]  (Mixed [nullamt]) (Mixed [amt0 {aquantity=1}])
        , PeriodicReportRow (flatDisplayName "income:salary")        [mamountp' "$-1.00"] (Mixed [nullamt]) (Mixed [amt0 {aquantity=(-1)}])
        ],
        Mixed [nullamt])

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
