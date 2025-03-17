{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
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
  getPostings,
  generateMultiBalanceAccount,
  generateMultiBalanceReport,

  -- -- * Tests
  tests_MultiBalanceReport
)
where

import Control.Monad (guard)
import Data.Foldable (toList)
import Data.List (sortOn)
import Data.List.NonEmpty (NonEmpty((:|)))
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.Maybe (fromMaybe, isJust, mapMaybe)
import Data.Ord (Down(..))
import Data.Semigroup (sconcat)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.These (these)
import Data.Time.Calendar (Day, addDays, fromGregorian)
import Data.Traversable (mapAccumL)
import Safe (lastDef)

import Hledger.Data
import Hledger.Query
import Hledger.Utils hiding (dbg3,dbg4,dbg5)
import qualified Hledger.Utils
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


-- | Generate a multicolumn balance report for the matched accounts,
-- showing the change of balance, accumulated balance, or historical balance
-- in each of the specified periods. If the normalbalance_ option is set, it
-- adjusts the sorting and sign of amounts (see ReportOpts and
-- CompoundBalanceCommand). hledger's most powerful and useful report, used
-- by the balance command (in multiperiod mode) and (via compoundBalanceReport)
-- by the bs/cf/is commands.
multiBalanceReport :: ReportSpec -> Journal -> MultiBalanceReport
multiBalanceReport rspec j = multiBalanceReportWith rspec j (journalPriceOracle infer j) mempty
  where infer = infer_prices_ $ _rsReportOpts rspec

-- | A helper for multiBalanceReport. This one takes some extra arguments,
-- a 'PriceOracle' to be used for looking up market prices, and a set of
-- 'AccountName's which should not be elided. Commands which run multiple
-- reports (bs etc.) can generate the price oracle just once for efficiency,
-- passing it to each report by calling this function directly.
multiBalanceReportWith :: ReportSpec -> Journal -> PriceOracle -> Set AccountName -> MultiBalanceReport
multiBalanceReportWith rspec' j priceoracle unelidableaccts = report
  where
    -- Queries, report/column dates.
    (reportspan, colspans) = dbg5 "reportSpan" $ reportSpan j rspec'
    rspec = dbg3 "reportopts" $ makeReportQuery rspec' reportspan
    -- force evaluation order to show price lookup after date spans in debug output (XXX not working)
    -- priceoracle = reportspan `seq` priceoracle0

    -- Get postings
    ps = dbg5 "ps" $ getPostings rspec j priceoracle reportspan

    -- Process changes into normal, cumulative, or historical amounts, plus value them and mark which are uninteresting
    acct = dbg5 "acct" $ generateMultiBalanceAccount rspec j priceoracle unelidableaccts colspans ps

    -- Generate and postprocess the report, negating balances and taking percentages if needed
    report = dbg4 "multiBalanceReportWith" $ generateMultiBalanceReport (_rsReportOpts rspec) colspans acct

-- | Generate a compound balance report from a list of CBCSubreportSpec. This
-- shares postings between the subreports.
compoundBalanceReport :: ReportSpec -> Journal -> [CBCSubreportSpec a]
                      -> CompoundPeriodicReport a MixedAmount
compoundBalanceReport rspec j = compoundBalanceReportWith rspec j (journalPriceOracle infer j)
  where infer = infer_prices_ $ _rsReportOpts rspec

-- | A helper for compoundBalanceReport, similar to multiBalanceReportWith.
compoundBalanceReportWith :: ReportSpec -> Journal -> PriceOracle
                          -> [CBCSubreportSpec a]
                          -> CompoundPeriodicReport a MixedAmount
compoundBalanceReportWith rspec' j priceoracle subreportspecs = cbr
  where
    -- Queries, report/column dates.
    (reportspan, colspans) = dbg5 "reportSpan" $ reportSpan j rspec'
    rspec = dbg3 "reportopts" $ makeReportQuery rspec' reportspan

    -- Get postings
    ps = dbg5 "ps" $ getPostings rspec j priceoracle reportspan

    subreports = map generateSubreport subreportspecs
      where
        generateSubreport CBCSubreportSpec{..} =
            ( cbcsubreporttitle
            -- Postprocess the report, negating balances and taking percentages if needed
            , cbcsubreporttransform $ generateMultiBalanceReport ropts colspans acct
            , cbcsubreportincreasestotal
            )
          where
            ropts = cbcsubreportoptions $ _rsReportOpts rspec
            -- Add a restriction to this subreport to the report query.
            -- XXX in non-thorough way, consider updateReportSpec ?
            rspecsub = rspec{_rsReportOpts=ropts, _rsQuery=And [cbcsubreportquery, _rsQuery rspec]}
            -- Account representing this subreport
            acct = generateMultiBalanceAccount rspecsub j priceoracle mempty colspans $
                     filter (matchesPostingExtra (journalAccountType j) cbcsubreportquery) ps

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

    cbr = CompoundPeriodicReport "" colspans subreports overalltotals


-- | Remove any date queries and insert queries from the report span.
-- The user's query expanded to the report span
-- if there is one (otherwise any date queries are left as-is, which
-- handles the hledger-ui+future txns case above).
makeReportQuery :: ReportSpec -> DateSpan -> ReportSpec
makeReportQuery rspec reportspan
    | reportspan == nulldatespan = rspec
    | otherwise = rspec{_rsQuery=query}
  where
    query            = simplifyQuery $ And [dateless $ _rsQuery rspec, reportspandatesq]
    reportspandatesq = dbg3 "reportspandatesq" $ dateqcons reportspan
    dateless         = dbg3 "dateless" . filterQuery (not . queryIsDateOrDate2)
    dateqcons        = if date2_ (_rsReportOpts rspec) then Date2 else Date

-- | Gather postings matching the query within the report period.
getPostings :: ReportSpec -> Journal -> PriceOracle -> DateSpan -> [Posting]
getPostings rspec@ReportSpec{_rsQuery=query, _rsReportOpts=ropts} j priceoracle reportspan =
    map clipPosting
    . setPostingsCount
    . journalPostings
    $ journalValueAndFilterPostingsWith rspec' j priceoracle
  where
    -- Clip posting names to the requested depth
    clipPosting p = p{paccount = clipOrEllipsifyAccountName depthSpec $ paccount p}

    -- If doing --count, set all posting amounts to "1".
    setPostingsCount = case balancecalc_ ropts of
        CalcPostingsCount -> map (postingTransformAmount (const $ mixed [num 1]))
        _                 -> id

    rspec' = rspec{_rsQuery=fullreportq,_rsReportOpts=ropts'}
    -- If we're re-valuing every period, we need to have the unvalued start
    -- balance, so we can do it ourselves later.
    ropts' = if isJust (valuationAfterSum ropts)
        then ropts{period_=dateSpanAsPeriod fullreportspan, value_=Nothing, conversionop_=Just NoConversionOp}  -- If we're valuing after the sum, don't do it now
        else ropts{period_=dateSpanAsPeriod fullreportspan}

    -- q projected back before the report start date.
    -- When there's no report start date, in case there are future txns (the hledger-ui case above),
    -- we use emptydatespan to make sure they aren't counted as starting balance.
    fullreportq = dbg3 "fullreportq" $ And [datelessq, fullreportspanq]
    datelessq   = dbg3 "datelessq" $ filterQuery (not . queryIsDateOrDate2) depthlessq

    -- The user's query with no depth limit, and expanded to the report span
    -- if there is one (otherwise any date queries are left as-is, which
    -- handles the hledger-ui+future txns case above).
    depthlessq = dbg3 "depthlessq" $ filterQuery (not . queryIsDepth) query

    depthSpec  = dbg3 "depthSpec" . queryDepth $ filterQuery queryIsDepth query

    fullreportspan  = if requiresHistorical ropts then DateSpan Nothing (Exact <$> spanEnd reportspan) else reportspan
    fullreportspanq = (if date2_ ropts then Date2 else Date) $ case fullreportspan of
        DateSpan Nothing Nothing -> emptydatespan
        a -> a

-- | Generate the 'Account' for the requested multi-balance report from a list
-- of 'Posting's.
generateMultiBalanceAccount :: ReportSpec -> Journal -> PriceOracle -> Set AccountName -> [DateSpan] -> [Posting] -> Account
generateMultiBalanceAccount rspec@ReportSpec{_rsReportOpts=ropts} j priceoracle unelidableaccts colspans =
    -- Add declared accounts if called with --declared and --empty
    (if (declared_ ropts && empty_ ropts) then addDeclaredAccounts rspec j else id)
    -- Negate amounts if applicable
    . (if invert_ ropts then fmap (applyAccountBalance maNegate) else id)
    -- Mark which accounts are boring and which are interesting
    . markAccountBoring rspec unelidableaccts
    -- Set account declaration info (for sorting purposes)
    . mapAccounts (accountSetDeclarationInfo j)
    -- Process changes into normal, cumulative, or historical amounts, plus value them
    . calculateReportAccount rspec j priceoracle colspans

-- | Add declared accounts to the account tree.
addDeclaredAccounts :: Monoid a => ReportSpec -> Journal -> Account' a -> Account' a
addDeclaredAccounts rspec j acct =
    these id id const <$> mergeAccounts acct declaredTree
  where
    declaredTree =
        mapAccounts (\a -> a{aboring = not $ aname a `HS.member` HS.fromList declaredAccounts}) $
          accountTreeFromBalanceAndNames "root" (mempty <$ abalances acct) declaredAccounts

    -- With --declared, add the query-matching declared accounts (as dummy postings
    -- so they are processed like the rest).
    declaredAccounts =
      map (clipOrEllipsifyAccountName depthSpec) .
      filter (matchesAccountExtra (journalAccountType j) (journalAccountTags j) accttypetagsq) $
      journalAccountNamesDeclared j

    accttypetagsq  = dbg3 "accttypetagsq" .
      filterQueryOrNotQuery (\q -> queryIsAcct q || queryIsType q || queryIsTag q) $
      _rsQuery rspec

    depthSpec = queryDepth . filterQuery queryIsDepth $ _rsQuery rspec


-- | Gather the account balance changes into a regular matrix, then
-- accumulate and value amounts, as specified by the report options.
-- Makes sure all report columns have an entry.
calculateReportAccount :: ReportSpec -> Journal -> PriceOracle -> [DateSpan] -> [Posting] -> Account
calculateReportAccount rspec@ReportSpec{_rsReportOpts=ropts} j priceoracle colspans ps =  -- PARTIAL:
    -- Ensure all columns have entries, including those with starting balances
    mapAccounts (\a -> a{abalances = rowbals $ abalances a}) changesAcct
  where
    -- The valued row amounts to be displayed: per-period changes,
    -- zero-based cumulative totals, or
    -- starting-balance-based historical balances.
    rowbals :: AccountBalances AccountBalance -> AccountBalances AccountBalance
    rowbals unvaluedChanges = case balanceaccum_ ropts of
        PerPeriod  -> changes
        Cumulative -> cumulative
        Historical -> historical
      where
        -- changes to report on: usually just the valued changes themselves, but use the
        -- differences in the valued historical amount for CalcValueChange and CalcGain.
        changes = case balancecalc_ ropts of
            CalcChange        -> avalue unvaluedChanges
            CalcBudget        -> avalue unvaluedChanges
            CalcValueChange   -> periodChanges historical
            CalcGain          -> periodChanges historical
            CalcPostingsCount -> avalue unvaluedChanges
        -- the historical balance is the valued cumulative sum of all unvalued changes
        historical = avalue $ cumulativeSum unvaluedChanges
        -- since this is a cumulative sum of valued amounts, it should not be valued again
        cumulative = cumulativeSum changes{abhistorical = mempty}
        avalue = accountBalancesValuation ropts j priceoracle colspans

    changesAcct = dbg5With (\x -> "multiBalanceReport changesAcct\n" ++ showAccounts x) $
        accountFromPostings getDate intervalStarts ps

    getDate = postingDateOrDate2 (whichDate (_rsReportOpts rspec))
    intervalStarts = case mapMaybe spanStart colspans of
      [] -> [nulldate]  -- Deal with the case of the empty journal
      xs -> xs

-- | The valuation function to use for the chosen report options.
accountBalancesValuation :: ReportOpts -> Journal -> PriceOracle -> [DateSpan]
                         -> AccountBalances AccountBalance -> AccountBalances AccountBalance
accountBalancesValuation ropts j priceoracle colspans =
    opAccountBalances valueAccountBalance accountBalancePeriodEnds
  where
    valueAccountBalance :: Day -> AccountBalance -> AccountBalance
    valueAccountBalance d = applyAccountBalance (valueMixedAmount d)

    valueMixedAmount :: Day -> MixedAmount -> MixedAmount
    valueMixedAmount = mixedAmountApplyValuationAfterSumFromOptsWith ropts j priceoracle

    accountBalancePeriodEnds :: AccountBalances Day
    accountBalancePeriodEnds = dbg5 "accountBalancePeriodEnds" $ case colspans of  -- FIXME: Change colspans to nonempty list
        [DateSpan Nothing Nothing] -> accountBalancesFromList nulldate [(nulldate, nulldate)]  -- Empty journal
        h:ds                       -> accountBalancesFromList (makeJustFst $ boundaries h) $ map (makeJust . boundaries) (h:ds)
        []                         -> error "accountBalancePeriodEnds: Shouldn't have empty colspans"  -- PARTIAL: Shouldn't occur
      where
        boundaries spn = (spanStart spn, spanEnd spn)

        makeJust (Just x, Just y) = (x, addDays (-1) y)
        makeJust _    = error "calculateReportAccount: expected all non-initial spans to have start and end dates"
        makeJustFst (Just x, _) = addDays (-1) x
        makeJustFst _ = error "calculateReportAccount: expected initial span to have an end date"

-- | Mark which nodes of an 'Account' are boring, and so should be omitted from reports.
markAccountBoring :: ReportSpec -> Set AccountName -> Account -> Account
markAccountBoring ReportSpec{_rsQuery=query,_rsReportOpts=ropts} unelidableaccts
    -- If depth 0, only the top-level account is interesting
    | qdepthIsZero = markBoring False . mapAccounts (markBoring True)
    -- Otherwise, an account is interesting if it is interesting on its own, or
    -- is a fork for interesting subaccounts (but the top-level root account is
    -- never interesting)
    | otherwise    = markBoring True . mapAccounts (markBoringBy (\a -> not $ isInteresting a || isInterestingFork a))
  where
    -- Accounts interesting for their own sake
    isInteresting :: Account -> Bool
    isInteresting acct =
        d <= qdepth                                 -- Throw out anything too deep
        && ( name `Set.member` unelidableaccts      -- Unelidable accounts should be kept unless too deep
           || (empty_ ropts && keepWhenEmpty acct)  -- Keep empty accounts when called with --empty
           || not (isZeroRow balance amts)          -- Keep everything with a non-zero balance in the row
           )
      where
        name = aname acct
        amts = abdatemap $ abalances acct
        d = accountNameLevel name

        qdepth = fromMaybe maxBound $ getAccountNameClippedDepth depthspec name
        balance = maybeStripPrices . case accountlistmode_ ropts of
            ALTree | d == qdepth -> abibalance
            _                    -> abebalance

    -- Accounts interesting because they are a fork for interesting subaccounts
    isInterestingFork :: Account -> Bool
    isInterestingFork acct = case accountlistmode_ ropts of
        ALTree -> hasEnoughSubs
        ALFlat -> False
      where
        hasEnoughSubs = length interestingSubs >= minimumSubs && accountNameLevel (aname acct) > drop_ ropts
        interestingSubs = filter (anyAccounts (not . aboring)) $ asubs acct
        minimumSubs = if no_elide_ ropts then 1 else 2

    isZeroRow balance = all (mixedAmountLooksZero . balance)
    keepWhenEmpty = case accountlistmode_ ropts of
        ALFlat -> any ((0<) . abnumpostings) . abdatemap . abalances  -- Keep all accounts that have postings in flat mode
        ALTree -> null . asubs  -- Keep only empty leaves in tree mode
    maybeStripPrices = if conversionop_ ropts == Just NoConversionOp then id else mixedAmountStripCosts

    qdepthIsZero = depthspec == DepthSpec (Just 0) []
    depthspec = queryDepth query

    markBoring   v a = a{aboring = v}
    markBoringBy f a = a{aboring = f a}


-- | Lay out a set of postings grouped by date span into a regular matrix with rows
-- given by AccountName and columns by DateSpan, then generate a MultiBalanceReport
-- from the columns.
generateMultiBalanceReport :: ReportOpts -> [DateSpan] -> Account -> MultiBalanceReport
generateMultiBalanceReport ropts colspans acct =
    reportPercent ropts $ PeriodicReport colspans (buildAndSort acct) totalsrow
  where
    -- Build report rows and sort them
    buildAndSort = dbg5 "sortedrows" . case accountlistmode_ ropts of
      ALTree | sort_amount_ ropts -> buildReportRows ropts . sortTreeByAmount
      ALFlat | sort_amount_ ropts -> sortFlatByAmount . buildReportRows ropts
      _                           -> buildReportRows ropts . sortAccountTreeByDeclaration

    -- Calculate column totals
    totalsrow = dbg5 "totalsrow" $ calculateTotalsRow ropts acct

    sortTreeByAmount = sortAccountTreeByAmount (fromMaybe NormallyPositive $ normalbalance_ ropts)
    sortFlatByAmount = case fromMaybe NormallyPositive $ normalbalance_ ropts of
        NormallyPositive -> sortOn (\r -> (Down $ amt r, prrFullName r))
        NormallyNegative -> sortOn (\r -> (amt r, prrFullName r))
      where amt = mixedAmountStripCosts . prrTotal

-- | Build the report rows.
-- One row per account, with account name info, row amounts, row total and row average.
-- Rows are sorted according to the order in the 'Account' tree.
buildReportRows :: ReportOpts -> Account -> [MultiBalanceReportRow]
buildReportRows ropts = mkRows True (-drop_ ropts) 0
  where
    -- Build the row for an account at a given depth with some number of boring parents
    mkRows :: Bool -> Int -> Int -> Account -> [MultiBalanceReportRow]
    mkRows isRoot d boringParents acct
        -- Account is a boring root account, and should be bypassed entirely
        | aboring acct && isRoot         = buildSubrows d 0
        -- Account is boring and has been dropped, so should be skipped and move up the hierarchy
        | aboring acct && d < 0          = buildSubrows (d + 1) 0
        -- Account is boring, and we can omit boring parents, so we should omit but keep track
        | aboring acct && canOmitParents = buildSubrows d (boringParents + 1)
        -- Account is not boring or otherwise should be displayed.
        | otherwise = PeriodicReportRow displayname rowbals rowtot rowavg : buildSubrows (d + 1) 0
      where
        displayname = displayedName d boringParents $ aname acct
        rowbals = map balance . toList . abdatemap $ abalances acct
        buildSubrows i b = concatMap (mkRows False i b) $ asubs acct

        -- The total and average for the row.
        -- These are always simply the sum/average of the displayed row amounts.
        -- Total for a cumulative/historical report is always the last column.
        rowtot = case balanceaccum_ ropts of
            PerPeriod -> maSum rowbals
            _         -> lastDef nullmixedamt rowbals
        rowavg = averageMixedAmounts rowbals

    canOmitParents = flat_ ropts || not (no_elide_ ropts)
    balance = case accountlistmode_ ropts of
        ALTree -> abibalance
        ALFlat -> abebalance

    displayedName d boringParents name
        | d == 0 && name == "root" = DisplayName "..." "..." 0
        | otherwise = case accountlistmode_ ropts of
            ALTree -> DisplayName name leaf $ max 0 d
            ALFlat -> DisplayName name droppedName 0
      where
        leaf = accountNameFromComponents
               . reverse . take (boringParents + 1) . reverse
               $ accountNameComponents droppedName
        droppedName = accountNameDrop (drop_ ropts) name

-- | Build the report totals row.
--
-- Calculate the column totals. These are always the sum of column amounts.
calculateTotalsRow :: ReportOpts -> Account -> PeriodicReportRow () MixedAmount
calculateTotalsRow ropts acct =
    PeriodicReportRow () coltotals grandtotal grandaverage
  where
    -- Totals come from the inclusive balances of the root account.
    coltotals = map abibalance . toList . abdatemap $ abalances acct

    -- Calculate the grand total and average. These are always the sum/average
    -- of the column totals.
    -- Total for a cumulative/historical report is always the last column.
    grandtotal = case balanceaccum_ ropts of
        PerPeriod -> maSum coltotals
        _         -> lastDef nullmixedamt coltotals
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
        setibalance a = a{abalances = (abalances a){abhistorical = hist}}
          where
            hist = (abhistorical $ abalances a){abibalance = maybe (maSum . map (abibalance . abhistorical . abalances) $ asubs a) prrTotal $ HM.lookup (aname a) rowMap}
        sortedaccounttree = sortAccountTreeByAmount (fromMaybe NormallyPositive $ normalbalance_ ropts) accounttreewithbals
        sortedanames = map aname $ drop 1 $ flattenAccounts sortedaccounttree

    -- Sort the report rows, representing a flat account list, by row total (and then account name).
    sortFlatMBRByAmount :: [MultiBalanceReportRow] -> [MultiBalanceReportRow]
    sortFlatMBRByAmount = case fromMaybe NormallyPositive $ normalbalance_ ropts of
        NormallyPositive -> sortOn (\r -> (Down $ amt r, prrFullName r))
        NormallyNegative -> sortOn (\r -> (amt r, prrFullName r))
      where amt = mixedAmountStripCosts . prrTotal

    -- Sort the report rows by account declaration order then account name.
    sortMBRByAccountDeclaration :: [MultiBalanceReportRow] -> [MultiBalanceReportRow]
    sortMBRByAccountDeclaration rows = sortRowsLike sortedanames rows
      where
        sortedanames = sortAccountNamesByDeclaration j (tree_ ropts) $ map prrFullName rows

-- | A sorting helper: sort a list of things (eg report rows) keyed by account name
-- to match the provided ordering of those same account names.
sortRowsLike :: [AccountName] -> [PeriodicReportRow DisplayName b] -> [PeriodicReportRow DisplayName b]
sortRowsLike sortedas rows = mapMaybe (`HM.lookup` rowMap) sortedas
  where rowMap = HM.fromList $ map (\row -> (prrFullName row, row)) rows

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

-- | Calculate a cumulative sum from a list of period changes.
cumulativeSum :: AccountBalances AccountBalance -> AccountBalances AccountBalance
cumulativeSum = snd . mapAccumL (\prev new -> let z = prev <> new in (z, z)) mempty

-- | Extract period changes from a cumulative list.
periodChanges :: AccountBalances AccountBalance -> AccountBalances AccountBalance
periodChanges = snd . mapAccumL (\prev new -> (new, opAccountBalance maMinus new prev)) mempty

-- tests

tests_MultiBalanceReport = testGroup "MultiBalanceReport" [

  let
    amt0 = Amount {acommodity="$", aquantity=0, acost=Nothing,
      astyle=AmountStyle {ascommodityside = L, ascommodityspaced = False, asdigitgroups = Nothing,
      asdecimalmark = Just '.', asprecision = Precision 2, asrounding = NoRounding}}
    (rspec,journal) `gives` r = do
      let rspec' = rspec{_rsQuery=And [queryFromFlags $ _rsReportOpts rspec, _rsQuery rspec]}
          (eitems, etotal) = r
          (PeriodicReport _ aitems atotal) = multiBalanceReport rspec' journal
          showw (PeriodicReportRow a lAmt amt amt')
              = (displayFull a, displayName a, displayIndent a, map showMixedAmountDebug lAmt, showMixedAmountDebug amt, showMixedAmountDebug amt')
      (map showw aitems) @?= (map showw eitems)
      showMixedAmountDebug (prrTotal atotal) @?= showMixedAmountDebug etotal -- we only check the sum of the totals
  in
   testGroup "multiBalanceReport" [
      testCase "null journal"  $
      (defreportspec, nulljournal) `gives` ([], nullmixedamt)

     ,testCase "with -H on a populated period"  $
      (defreportspec{_rsReportOpts=defreportopts{period_= PeriodBetween (fromGregorian 2008 1 1) (fromGregorian 2008 1 2), balanceaccum_=Historical}}, samplejournal) `gives`
       (
        [ PeriodicReportRow (flatDisplayName "assets:bank:checking") [mixedAmount $ usd 1]    (mixedAmount $ usd 1)    (mixedAmount amt0{aquantity=1})
        , PeriodicReportRow (flatDisplayName "income:salary")        [mixedAmount $ usd (-1)] (mixedAmount $ usd (-1)) (mixedAmount amt0{aquantity=(-1)})
        ],
        mixedAmount $ usd 0)

     -- ,testCase "a valid history on an empty period"  $
     --  (defreportopts{period_= PeriodBetween (fromGregorian 2008 1 2) (fromGregorian 2008 1 3), balanceaccum_=Historical}, samplejournal) `gives`
     --   (
     --    [
     --     ("assets:bank:checking","checking",3, [mamountp' "$1.00"], mamountp' "$1.00",mixedAmount amt0 {aquantity=1})
     --    ,("income:salary","salary",2, [mamountp' "$-1.00"], mamountp' "$-1.00",mixedAmount amt0 {aquantity=(-1)})
     --    ],
     --    mixedAmount usd0)

     -- ,testCase "a valid history on an empty period (more complex)"  $
     --  (defreportopts{period_= PeriodBetween (fromGregorian 2009 1 1) (fromGregorian 2009 1 2), balanceaccum_=Historical}, samplejournal) `gives`
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
