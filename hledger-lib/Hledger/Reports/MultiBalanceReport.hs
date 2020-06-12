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

  -- -- * Tests
  tests_MultiBalanceReport
)
where

import Data.List
import Data.List.Extra (nubSort)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Ord
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
--   * the full account name
--
--   * the account's depth
--
--   * A list of amounts, one for each column.
--
--   * the total of the row's amounts for a periodic report
--
--   * the average of the row's amounts
--
-- 3. the column totals, and the overall grand total (or zero for
-- cumulative/historical reports) and grand average.

type MultiBalanceReport    = PeriodicReport AccountName MixedAmount
type MultiBalanceReportRow = PeriodicReportRow AccountName MixedAmount

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
multiBalanceReportWith ropts@ReportOpts{..} q j priceoracle =
    (if invert_ then prNegate else id) $
    PeriodicReport colspans mappedsortedrows mappedtotalsrow
  where
    ----------------------------------------------------------------------
    -- 1. Queries, report/column dates.

    depthq     = dbg "depthq" $ filterQuery queryIsDepth q
    depth      = queryDepth depthq
    -- The date span specified by -b/-e/-p options and query args if any.
    requestedspan  = dbg "requestedspan"  $ queryDateSpan date2_ q
    -- If the requested span is open-ended, close it using the journal's end dates.
    -- This can still be the null (open) span if the journal is empty.
    requestedspan' = dbg "requestedspan'" $ requestedspan `spanDefaultsFrom` journalDateSpan date2_ j
    -- The list of interval spans enclosing the requested span.
    -- This list can be empty if the journal was empty,
    -- or if hledger-ui has added its special date:-tomorrow to the query
    -- and all txns are in the future.
    intervalspans  = dbg "intervalspans"  $ splitSpan interval_ requestedspan'
    -- The requested span enlarged to enclose a whole number of intervals.
    -- This can be the null span if there were no intervals.
    reportspan     = dbg "reportspan"     $ DateSpan (maybe Nothing spanStart $ headMay intervalspans)
                                                      (maybe Nothing spanEnd   $ lastMay intervalspans)
    -- The user's query with no depth limit, and expanded to the report span
    -- if there is one (otherwise any date queries are left as-is, which
    -- handles the hledger-ui+future txns case above).
    reportq = dbg "reportq" $ makeReportQuery ropts reportspan q

    -- The matched accounts with a starting balance. All of these shold appear
    -- in the report, even if they have no postings during the report period.
    startbals = dbg' "startbals" $ startingBalances ropts reportq j reportspan
    -- The matched accounts with a starting balance. All of these should appear
    -- in the report even if they have no postings during the report period.
    startaccts = dbg'' "startaccts" $ HM.keys startbals

    -- Postings matching the query within the report period.
    ps :: [(Posting, Day)] = dbg'' "ps" $ getPostings ropts reportq j
    days = map snd ps

    -- The date spans to be included as report columns.
    colspans = dbg "colspans" $ calculateColSpans ropts reportspan days

    -- Group postings into their columns.
    colps = dbg'' "colps" $ calculateColumns colspans ps

    -- Each account's balance changes across all columns.
    acctchanges = dbg'' "acctchanges" $ calculateAccountChanges ropts q startbals colps

    -- Process changes into normal, cumulative, or historical amounts, plus value them
    accumvalued = dbg'' "accumvalued" $ accumValueAmounts ropts j priceoracle startbals acctchanges

    -- All account names that will be displayed, possibly depth-clipped.
    displayaccts :: [ClippedAccountName] =
        dbg'' "displayaccts" $
        (if tree_ ropts then expandAccountNames else id) $
        nub $ map (clipOrEllipsifyAccountName depth) $
        if empty_ || balancetype_ == HistoricalBalance
        then nubSort $ startaccts ++ allpostedaccts
        else allpostedaccts
      where
        allpostedaccts :: [AccountName] =
          dbg'' "allpostedaccts" . sort . accountNamesFromPostings $ map fst ps

    -- All the rows of the report.
    rows = dbg'' "rows" $ buildReportRows ropts reportq accumvalued

    -- Sorted report rows.
    sortedrows = dbg' "sortedrows" $ sortRows ropts j rows

    ----------------------------------------------------------------------
    -- 8. Build the report totals row.

    -- Calculate the column totals. These are always the sum of column amounts.
    highestlevelaccts = [a | a <- displayaccts, not $ any (`elem` displayaccts) $ init $ expandAccountName a]
    colamts = transpose . map prrAmounts $ filter isHighest rows
      where isHighest row = not (tree_ ropts) || prrName row `elem` highestlevelaccts
    coltotals :: [MixedAmount] =
      dbg'' "coltotals" $ map sum colamts
    -- Calculate the grand total and average. These are always the sum/average
    -- of the column totals.
    [grandtotal,grandaverage] =
      let amts = map ($ map sum colamts)
            [if balancetype_==PeriodChange then sum else const 0
            ,averageMixedAmounts
            ]
      in amts
    -- Totals row.
    totalsrow :: PeriodicReportRow () MixedAmount =
      dbg' "totalsrow" $ PeriodicReportRow () 0 coltotals grandtotal grandaverage

    ----------------------------------------------------------------------
    -- 9. Map the report rows to percentages if needed
    -- It is not correct to do this before step 6 due to the total and average columns.
    -- This is not done in step 6, since the report totals are calculated in 8.
    -- Perform the divisions to obtain percentages
    mappedsortedrows :: [MultiBalanceReportRow] =
      if not percent_ then sortedrows
      else dbg'' "mappedsortedrows"
        [ PeriodicReportRow aname alevel
            (zipWith perdivide rowvals coltotals)
            (rowtotal `perdivide` grandtotal)
            (rowavg `perdivide` grandaverage)
         | PeriodicReportRow aname alevel rowvals rowtotal rowavg <- sortedrows
        ]
    mappedtotalsrow :: PeriodicReportRow () MixedAmount
      | percent_  = dbg'' "mappedtotalsrow" $ PeriodicReportRow () 0
           (map (\t -> perdivide t t) coltotals)
           (perdivide grandtotal grandtotal)
           (perdivide grandaverage grandaverage)
      | otherwise = totalsrow


-- | Calculate starting balances, if needed for -H
--
-- Balances at report start date, from all earlier postings which otherwise match the query.
-- These balances are unvalued except maybe converted to cost.
startingBalances :: ReportOpts -> Query -> Journal -> DateSpan -> HashMap AccountName MixedAmount
startingBalances ropts q j reportspan = acctchanges
  where
    acctchanges = acctChangesFromPostings ropts'' startbalq . map fst $
        getPostings ropts'' startbalq j

    -- q projected back before the report start date.
    -- When there's no report start date, in case there are future txns (the hledger-ui case above),
    -- we use emptydatespan to make sure they aren't counted as starting balance.
    startbalq = dbg'' "startbalq" $ And [datelessq, precedingspanq]
    datelessq = dbg "datelessq" $ filterQuery (not . queryIsDateOrDate2) q

    ropts' | tree_ ropts = ropts{no_elide_=True}
           | otherwise   = ropts{accountlistmode_=ALFlat}
    ropts'' = ropts'{period_ = precedingperiod}

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

-- | Remove any date queries and insert queries from the report span.
makeReportQuery :: ReportOpts -> DateSpan -> Query -> Query
makeReportQuery ropts reportspan q
    | reportspan == nulldatespan = q
    | otherwise = And [dateless q, reportspandatesq]
  where
    reportspandatesq = dbg "reportspandatesq" $ dateqcons reportspan
    dateless   = dbg "dateless" . filterQuery (not . queryIsDateOrDate2)
    dateqcons  = if date2_ ropts then Date2 else Date

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
acctChangesFromPostings :: ReportOpts -> Query -> [Posting] -> HashMap ClippedAccountName MixedAmount
acctChangesFromPostings ropts q ps =
    HM.fromList [(aname a, (if tree_ ropts then aibalance else aebalance) a) | a <- as]
  where
    as = depthLimit $
         (if tree_ ropts then id else filter ((>0).anumpostings)) $
         drop 1 $ accountsFromPostings ps
    depthLimit
      | tree_ ropts = filter ((depthq `matchesAccount`) . aname) -- exclude deeper balances
      | otherwise   = clipAccountsAndAggregate $ queryDepth depthq -- aggregate deeper balances at the depth limit
    depthq = dbg "depthq" $ filterQuery queryIsDepth q

-- | Gather the account balance changes into a regular matrix including the accounts
-- from all columns
calculateAccountChanges :: ReportOpts -> Query
                        -> HashMap ClippedAccountName MixedAmount
                        -> Map DateSpan [Posting]
                        -> HashMap ClippedAccountName (Map DateSpan MixedAmount)
calculateAccountChanges ropts q startbals colps = acctchanges
  where
    -- Transpose to get each account's balance changes across all columns.
    acctchanges = transposeMap colacctchanges <> (zeros <$ startbals)

    colacctchanges :: Map DateSpan (HashMap ClippedAccountName MixedAmount) =
      dbg'' "colacctchanges" $ fmap (acctChangesFromPostings ropts q) colps

    zeros = nullmixedamt <$ colacctchanges

-- | Accumulate and value amounts, as specified by the report options.
accumValueAmounts :: ReportOpts -> Journal -> PriceOracle
                  -> HashMap ClippedAccountName MixedAmount
                  -> HashMap ClippedAccountName (Map DateSpan MixedAmount)
                  -> HashMap ClippedAccountName [MixedAmount]
accumValueAmounts ropts j priceoracle startbals = HM.mapWithKey processRow
  where
    processRow name col = zipWith valueAcct spans $ rowbals name amts
      where (spans, amts) = unzip $ M.toList col

    -- The row amounts to be displayed: per-period changes,
    -- zero-based cumulative totals, or
    -- starting-balance-based historical balances.
    rowbals name changes = dbg'' "rowbals" $ case balancetype_ ropts of
        PeriodChange      -> changes
        CumulativeChange  -> drop 1 $ scanl (+) 0                         changes
        HistoricalBalance -> drop 1 $ scanl (+) (startingBalanceFor name) changes

    -- We may be converting amounts to value, per hledger_options.m4.md "Effect of --value on reports".
    valueAcct (DateSpan _ (Just end)) = avalue periodlast
      where periodlast = addDays (-1) end
    valueAcct _ = error' "multiBalanceReport: expected all spans to have an end date"  -- XXX should not happen

    avalue periodlast = maybe id
        (mixedAmountApplyValuation priceoracle styles periodlast mreportlast today multiperiod) $
        value_ ropts
      where
        -- Some things needed if doing valuation.
        styles = journalCommodityStyles j
        mreportlast = reportPeriodLastDay ropts
        today = fromMaybe (error' "multiBalanceReport: could not pick a valuation date, ReportOpts today_ is unset") $ today_ ropts  -- XXX shouldn't happen
        multiperiod = interval_ ropts /= NoInterval

    startingBalanceFor a = HM.lookupDefault nullmixedamt a startbals

-- | Build the report rows.
--
-- One row per account, with account name info, row amounts, row total and row average.
buildReportRows :: ReportOpts -> Query -> HashMap AccountName [MixedAmount] -> [MultiBalanceReportRow]
buildReportRows ropts q acctvalues =
    [ PeriodicReportRow a (accountNameLevel a) rowbals rowtot rowavg
    | (a,rowbals) <- HM.toList acctvalues
    -- The total and average for the row.
    -- These are always simply the sum/average of the displayed row amounts.
    -- Total for a cumulative/historical report is always zero.
    , let rowtot = if balancetype_ ropts == PeriodChange then sum rowbals else 0
    , let rowavg = averageMixedAmounts rowbals
    , empty_ ropts || queryDepth q == 0 || any (not . mixedAmountLooksZero) rowbals  -- TODO: Remove this eventually, to be handled elswhere
    ]

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
        anamesandrows = [(prrName r, r) | r <- rows]
        anames = map fst anamesandrows
        atotals = [(prrName r, prrTotal r) | r <- rows]
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
        anamesandrows = [(prrName r, r) | r <- rows]
        anames = map fst anamesandrows
        sortedanames = sortAccountNamesByDeclaration j (tree_ ropts) anames
        sortedrows = sortAccountItemsLike sortedanames anamesandrows


-- | Generates a simple non-columnar BalanceReport, but using multiBalanceReport,
-- in order to support --historical. Does not support tree-mode boring parent eliding.
-- If the normalbalance_ option is set, it adjusts the sorting and sign of amounts
-- (see ReportOpts and CompoundBalanceCommand).
balanceReportFromMultiBalanceReport :: ReportOpts -> Query -> Journal
    -> ([(AccountName, AccountName, Int, MixedAmount)], MixedAmount)
balanceReportFromMultiBalanceReport opts q j = (rows', total)
  where
    PeriodicReport _ rows (PeriodicReportRow _ _ totals _ _) =
      multiBalanceReportWith opts q j (journalPriceOracle (infer_value_ opts) j)
    rows' = [( a
             , if flat_ opts then a else accountLeafName a   -- BalanceReport expects full account name here with --flat
             , if tree_ opts then d-1 else 0  -- BalanceReport uses 0-based account depths
             , headDef nullmixedamt amts     -- 0 columns is illegal, should not happen, return zeroes if it does
             ) | PeriodicReportRow a d amts _ _ <- rows]
    total = headDef nullmixedamt totals


-- | Transpose a Map of HashMaps to a HashMap of Maps.
transposeMap :: Map DateSpan (HashMap AccountName MixedAmount)
             -> HashMap AccountName (Map DateSpan MixedAmount)
transposeMap xs = M.foldrWithKey addSpan mempty xs
  where
    addSpan span acctmap seen = HM.foldrWithKey (addAcctSpan span) seen acctmap

    addAcctSpan span acct a = HM.alter f acct
      where f = Just . M.insert span a . fromMaybe emptySpanMap

    emptySpanMap = nullmixedamt <$ xs

-- | A sorting helper: sort a list of things (eg report rows) keyed by account name
-- to match the provided ordering of those same account names.
sortAccountItemsLike :: [AccountName] -> [(AccountName, b)] -> [b]
sortAccountItemsLike sortedas items =
  concatMap (\a -> maybe [] (:[]) $ lookup a items) sortedas

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
          showw (PeriodicReportRow acct indent lAmt amt amt')
              = (acct, accountLeafName acct, indent, map showMixedAmountDebug lAmt, showMixedAmountDebug amt, showMixedAmountDebug amt')
      (map showw aitems) @?= (map showw eitems)
      showMixedAmountDebug (prrTotal atotal) @?= showMixedAmountDebug etotal -- we only check the sum of the totals
  in
   tests "multiBalanceReport" [
      test "null journal"  $
      (defreportopts, nulljournal) `gives` ([], Mixed [nullamt])

     ,test "with -H on a populated period"  $
      (defreportopts{period_= PeriodBetween (fromGregorian 2008 1 1) (fromGregorian 2008 1 2), balancetype_=HistoricalBalance}, samplejournal) `gives`
       (
        [ PeriodicReportRow "assets:bank:checking" 3 [mamountp' "$1.00"]  (Mixed [nullamt]) (Mixed [amt0 {aquantity=1}])
        , PeriodicReportRow "income:salary"        2 [mamountp' "$-1.00"] (Mixed [nullamt]) (Mixed [amt0 {aquantity=(-1)}])
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
