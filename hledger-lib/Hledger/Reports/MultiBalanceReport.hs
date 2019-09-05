{-# LANGUAGE FlexibleInstances, RecordWildCards, ScopedTypeVariables, OverloadedStrings, DeriveGeneric #-}
{-|

Multi-column balance reports, used by the balance command.

-}

module Hledger.Reports.MultiBalanceReport (
  MultiBalanceReport(..),
  MultiBalanceReportRow,
  multiBalanceReport,
  multiBalanceReportWith,
  balanceReportFromMultiBalanceReport,
  mbrNegate,
  mbrNormaliseSign,
  multiBalanceReportSpan,
  tableAsText,

  -- -- * Tests
  tests_MultiBalanceReport
)
where

import GHC.Generics (Generic)
import Control.DeepSeq (NFData)
import Data.List
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
import Hledger.Reports.BalanceReport


-- | A multi balance report is a balance report with multiple columns,
-- corresponding to consecutive subperiods within the overall report
-- period. It has:
--
-- 1. a list of each column's period (date span)
--
-- 2. a list of rows, each containing:
--
--   * the full account name
--
--   * the leaf account name
--
--   * the account's depth
--
--   * A list of amounts, one for each column. The meaning of the
--     amounts depends on the type of multi balance report, of which
--     there are three: periodic, cumulative and historical (see
--     'BalanceType' and "Hledger.Cli.Commands.Balance").
--
--   * the total of the row's amounts for a periodic report,
--     or zero for cumulative/historical reports (since summing
--     end balances generally doesn't make sense).
--
--   * the average of the row's amounts
--
-- 3. the column totals, and the overall grand total (or zero for
-- cumulative/historical reports) and grand average.
--
newtype MultiBalanceReport =
  MultiBalanceReport ([DateSpan]
                     ,[MultiBalanceReportRow]
                     ,MultiBalanceReportTotals
                     )
  deriving (Generic)

type MultiBalanceReportRow    = (AccountName, AccountName, Int, [MixedAmount], MixedAmount, MixedAmount)
type MultiBalanceReportTotals = ([MixedAmount], MixedAmount, MixedAmount) -- (Totals list, sum of totals, average of totals)

instance NFData MultiBalanceReport

instance Show MultiBalanceReport where
    -- use pshow (pretty-show's ppShow) to break long lists onto multiple lines
    -- we add some bogus extra shows here to help it parse the output
    -- and wrap tuples and lists properly
    show (MultiBalanceReport (spans, items, totals)) =
        "MultiBalanceReport (ignore extra quotes):\n" ++ pshow (show spans, map show items, totals)

-- type alias just to remind us which AccountNames might be depth-clipped, below.
type ClippedAccountName = AccountName

-- | Generate a multicolumn balance report for the matched accounts,
-- showing the change of balance, accumulated balance, or historical balance
-- in each of the specified periods. Does not support tree-mode boring parent eliding.
-- If the normalbalance_ option is set, it adjusts the sorting and sign of amounts
-- (see ReportOpts and CompoundBalanceCommand).
-- hledger's most powerful and useful report, used by the balance
-- command (in multiperiod mode) and (via multiBalanceReport') by the bs/cf/is commands.
multiBalanceReport :: ReportOpts -> Query -> Journal -> MultiBalanceReport
multiBalanceReport ropts q j = multiBalanceReportWith ropts q j (journalPriceOracle j)

-- | A helper for multiBalanceReport. This one takes an extra argument, a
-- PriceOracle to be used for looking up market prices. Commands which
-- run multiple reports (bs etc.) can generate the price oracle just once
-- for efficiency, passing it to each report by calling this function directly.
multiBalanceReportWith :: ReportOpts -> Query -> Journal -> PriceOracle -> MultiBalanceReport
multiBalanceReportWith ropts@ReportOpts{..} q j@Journal{..} priceoracle =
  (if invert_ then mbrNegate else id) $
  MultiBalanceReport (colspans, sortedrows, totalsrow)
    where
      dbg1 s = let p = "multiBalanceReport" in Hledger.Utils.dbg1 (p++" "++s)  -- add prefix in this function's debug output
      -- dbg1 = const id  -- exclude this function from debug output

      ----------------------------------------------------------------------
      -- 1. Queries, report/column dates.

      symq       = dbg1 "symq"   $ filterQuery queryIsSym $ dbg1 "requested q" q
      depthq     = dbg1 "depthq" $ filterQuery queryIsDepth q
      depth      = queryDepth depthq
      depthless  = dbg1 "depthless" . filterQuery (not . queryIsDepth)
      datelessq  = dbg1 "datelessq"  $ filterQuery (not . queryIsDateOrDate2) q
      dateqcons  = if date2_ then Date2 else Date
      -- The date span specified by -b/-e/-p options and query args if any.
      requestedspan  = dbg1 "requestedspan"  $ queryDateSpan date2_ q
      -- If the requested span is open-ended, close it using the journal's end dates.
      -- This can still be the null (open) span if the journal is empty.
      requestedspan' = dbg1 "requestedspan'" $ requestedspan `spanDefaultsFrom` journalDateSpan date2_ j
      -- The list of interval spans enclosing the requested span.
      -- This list can be empty if the journal was empty,
      -- or if hledger-ui has added its special date:-tomorrow to the query
      -- and all txns are in the future.
      intervalspans  = dbg1 "intervalspans"  $ splitSpan interval_ requestedspan'
      -- The requested span enlarged to enclose a whole number of intervals.
      -- This can be the null span if there were no intervals.
      reportspan     = dbg1 "reportspan"     $ DateSpan (maybe Nothing spanStart $ headMay intervalspans)
                                                        (maybe Nothing spanEnd   $ lastMay intervalspans)
      mreportstart = spanStart reportspan
      -- The user's query with no depth limit, and expanded to the report span
      -- if there is one (otherwise any date queries are left as-is, which
      -- handles the hledger-ui+future txns case above).
      reportq   = dbg1 "reportq" $ depthless $
        if reportspan == nulldatespan
        then q
        else And [datelessq, reportspandatesq]
          where
            reportspandatesq = dbg1 "reportspandatesq" $ dateqcons reportspan
      -- The date spans to be included as report columns.
      colspans :: [DateSpan] = dbg1 "colspans" $ splitSpan interval_ displayspan
        where
          displayspan
            | empty_    = dbg1 "displayspan (-E)" reportspan                              -- all the requested intervals
            | otherwise = dbg1 "displayspan" $ requestedspan `spanIntersect` matchedspan  -- exclude leading/trailing empty intervals
          matchedspan = dbg1 "matchedspan" $ postingsDateSpan' (whichDateFromOpts ropts) ps

      -- If doing cost valuation, convert amounts to cost.
      j' = journalSelectingAmountFromOpts ropts j

      ----------------------------------------------------------------------
      -- 2. Calculate starting balances, if needed for -H

      -- Balances at report start date, from all earlier postings which otherwise match the query.
      -- These balances are unvalued except maybe converted to cost.
      startbals :: [(AccountName, MixedAmount)] = dbg1 "startbals" $ map (\(a,_,_,b) -> (a,b)) startbalanceitems
        where
          (startbalanceitems,_) = dbg1 "starting balance report" $ balanceReport ropts''{value_=Nothing} startbalq j'
            where
              ropts' | tree_ ropts = ropts{no_elide_=True}
                     | otherwise   = ropts{accountlistmode_=ALFlat}
              ropts'' = ropts'{period_ = precedingperiod}
                where
                  precedingperiod = dateSpanAsPeriod $ spanIntersect (DateSpan Nothing mreportstart) $ periodAsDateSpan period_
              -- q projected back before the report start date.
              -- When there's no report start date, in case there are future txns (the hledger-ui case above),
              -- we use emptydatespan to make sure they aren't counted as starting balance.
              startbalq = dbg1 "startbalq" $ And [datelessq, dateqcons precedingspan]
                where
                  precedingspan = case mreportstart of
                                  Just d  -> DateSpan Nothing (Just d)
                                  Nothing -> emptydatespan
      -- The matched accounts with a starting balance. All of these should appear
      -- in the report even if they have no postings during the report period.
      startaccts = dbg1 "startaccts" $ map fst startbals
      -- Helpers to look up an account's starting balance.
      startingBalanceFor a = fromMaybe nullmixedamt $ lookup a startbals

      ----------------------------------------------------------------------
      -- 3. Gather postings for each column.

      -- Postings matching the query within the report period.
      ps :: [Posting] =
          dbg1 "ps" $
          journalPostings $
          filterJournalAmounts symq $      -- remove amount parts excluded by cur:
          filterJournalPostings reportq $  -- remove postings not matched by (adjusted) query
          j'

      -- Group postings into their columns, with the column end dates.
      colps :: [([Posting], Maybe Day)] =
          dbg1 "colps"
          [(filter (isPostingInDateSpan' (whichDateFromOpts ropts) s) ps, spanEnd s) | s <- colspans]

      ----------------------------------------------------------------------
      -- 4. Calculate account balance changes in each column.

      -- In each column, gather the accounts that have postings and their change amount.
      acctChangesFromPostings :: [Posting] -> [(ClippedAccountName, MixedAmount)]
      acctChangesFromPostings ps = [(aname a, (if tree_ ropts then aibalance else aebalance) a) | a <- as]
          where
            as = depthLimit $
                 (if tree_ ropts then id else filter ((>0).anumpostings)) $
                 drop 1 $ accountsFromPostings ps
            depthLimit
                | tree_ ropts = filter ((depthq `matchesAccount`).aname) -- exclude deeper balances
                | otherwise   = clipAccountsAndAggregate depth -- aggregate deeper balances at the depth limit
      colacctchanges :: [[(ClippedAccountName, MixedAmount)]] =
          dbg1 "colacctchanges" $ map (acctChangesFromPostings . fst) colps

      ----------------------------------------------------------------------
      -- 5. Gather the account balance changes into a regular matrix including the accounts
      -- from all columns (and with -H, accounts with starting balances), adding zeroes where needed.

      -- All account names that will be displayed, possibly depth-clipped.
      displayaccts :: [ClippedAccountName] =
          dbg1 "displayaccts" $
          (if tree_ ropts then expandAccountNames else id) $
          nub $ map (clipOrEllipsifyAccountName depth) $
          if empty_ || balancetype_ == HistoricalBalance
          then nub $ sort $ startaccts ++ allpostedaccts
          else allpostedaccts
        where
          allpostedaccts :: [AccountName] = dbg1 "allpostedaccts" $ sort $ accountNamesFromPostings ps
      -- Each column's balance changes for each account, adding zeroes where needed.
      colallacctchanges :: [[(ClippedAccountName, MixedAmount)]] =
          dbg1 "colallacctchanges"
          [sortBy (comparing fst) $
           unionBy (\(a,_) (a',_) -> a == a') postedacctchanges zeroes
           | postedacctchanges <- colacctchanges]
          where zeroes = [(a, nullmixedamt) | a <- displayaccts]
      -- Transpose to get each account's balance changes across all columns.
      acctchanges :: [(ClippedAccountName, [MixedAmount])] =
          dbg1 "acctchanges"
          [(a, map snd abs) | abs@((a,_):_) <- transpose colallacctchanges] -- never null, or used when null...

      ----------------------------------------------------------------------
      -- 6. Build the report rows.

      -- One row per account, with account name info, row amounts, row total and row average.
      rows :: [MultiBalanceReportRow] =
          dbg1 "rows" $
          [(a, accountLeafName a, accountNameLevel a, valuedrowbals, rowtot, rowavg)
           | (a,changes) <- dbg1 "acctchanges" acctchanges
             -- The row amounts to be displayed: per-period changes,
             -- zero-based cumulative totals, or
             -- starting-balance-based historical balances.
           , let rowbals = dbg1 "rowbals" $ case balancetype_ of
                   PeriodChange      -> changes
                   CumulativeChange  -> drop 1 $ scanl (+) 0                      changes
                   HistoricalBalance -> drop 1 $ scanl (+) (startingBalanceFor a) changes
             -- We may be converting amounts to value, per hledger_options.m4.md "Effect of --value on reports".
           , let valuedrowbals = dbg1 "valuedrowbals" $ [avalue periodlastday amt | (amt,periodlastday) <- zip rowbals lastdays]
             -- The total and average for the row.
             -- These are always simply the sum/average of the displayed row amounts.
             -- Total for a cumulative/historical report is always zero.
           , let rowtot = if balancetype_==PeriodChange then sum valuedrowbals else 0
           , let rowavg = averageMixedAmounts valuedrowbals
           , empty_ || depth == 0 || any (not . isZeroMixedAmount) valuedrowbals
           ]
        where
          avalue periodlast =
            maybe id (mixedAmountApplyValuation priceoracle styles periodlast mreportlast today multiperiod) value_
            where
              -- Some things needed if doing valuation.
              styles = journalCommodityStyles j
              mreportlast = reportPeriodLastDay ropts
              today = fromMaybe (error' "multiBalanceReport: could not pick a valuation date, ReportOpts today_ is unset") today_  -- XXX shouldn't happen
              multiperiod = interval_ /= NoInterval
          -- The last day of each column's subperiod.
          lastdays =
            map ((maybe
                  (error' "multiBalanceReport: expected all spans to have an end date")  -- XXX should not happen
                  (addDays (-1)))
                . spanEnd) colspans

      ----------------------------------------------------------------------
      -- 7. Sort the report rows.

      -- Sort the rows by amount or by account declaration order. This is a bit tricky.
      -- TODO: is it always ok to sort report rows after report has been generated, as a separate step ?
      sortedrows :: [MultiBalanceReportRow] =
        dbg1 "sortedrows" $
        sortrows rows
        where
          sortrows
            | sort_amount_ && accountlistmode_ == ALTree = sortTreeMBRByAmount
            | sort_amount_                               = sortFlatMBRByAmount
            | otherwise                                  = sortMBRByAccountDeclaration
            where
              -- Sort the report rows, representing a tree of accounts, by row total at each level.
              -- Similar to sortMBRByAccountDeclaration/sortAccountNamesByDeclaration.
              sortTreeMBRByAmount rows = sortedrows
                where
                  anamesandrows = [(first6 r, r) | r <- rows]
                  anames = map fst anamesandrows
                  atotals = [(a,tot) | (a,_,_,_,tot,_) <- rows]
                  accounttree = accountTree "root" anames
                  accounttreewithbals = mapAccounts setibalance accounttree
                    where
                      -- should not happen, but it's dangerous; TODO
                      setibalance a = a{aibalance=fromMaybe (error "sortTreeMBRByAmount 1") $ lookup (aname a) atotals}
                  sortedaccounttree = sortAccountTreeByAmount (fromMaybe NormallyPositive normalbalance_) accounttreewithbals
                  sortedanames = map aname $ drop 1 $ flattenAccounts sortedaccounttree
                  sortedrows = sortAccountItemsLike sortedanames anamesandrows

              -- Sort the report rows, representing a flat account list, by row total.
              sortFlatMBRByAmount = sortBy (maybeflip $ comparing (normaliseMixedAmountSquashPricesForDisplay . fifth6))
                where
                  maybeflip = if normalbalance_ == Just NormallyNegative then id else flip

              -- Sort the report rows by account declaration order then account name.
              sortMBRByAccountDeclaration rows = sortedrows
                where
                  anamesandrows = [(first6 r, r) | r <- rows]
                  anames = map fst anamesandrows
                  sortedanames = sortAccountNamesByDeclaration j (tree_ ropts) anames
                  sortedrows = sortAccountItemsLike sortedanames anamesandrows

      ----------------------------------------------------------------------
      -- 8. Build the report totals row.

      -- Calculate the column totals. These are always the sum of column amounts.
      highestlevelaccts = [a | a <- displayaccts, not $ any (`elem` displayaccts) $ init $ expandAccountName a]
      colamts           = transpose [bs | (a,_,_,bs,_,_) <- rows, not (tree_ ropts) || a `elem` highestlevelaccts]
      coltotals :: [MixedAmount] =
        dbg1 "coltotals" $ map sum colamts
      -- Calculate the grand total and average. These are always the sum/average
      -- of the column totals.
      [grandtotal,grandaverage] =
        let amts = map ($ map sum colamts)
              [if balancetype_==PeriodChange then sum else const 0
              ,averageMixedAmounts
              ]
        in amts
      -- Totals row.
      totalsrow :: MultiBalanceReportTotals =
        dbg1 "totalsrow" (coltotals, grandtotal, grandaverage)

-- | Given a MultiBalanceReport and its normal balance sign,
-- if it is known to be normally negative, convert it to normally positive.
mbrNormaliseSign :: NormalSign -> MultiBalanceReport -> MultiBalanceReport
mbrNormaliseSign NormallyNegative = mbrNegate
mbrNormaliseSign _ = id

-- | Flip the sign of all amounts in a MultiBalanceReport.
mbrNegate (MultiBalanceReport (colspans, rows, totalsrow)) =
  MultiBalanceReport (colspans, map mbrRowNegate rows, mbrTotalsRowNegate totalsrow)
  where
    mbrRowNegate (acct,shortacct,indent,amts,tot,avg) = (acct,shortacct,indent,map negate amts,-tot,-avg)
    mbrTotalsRowNegate (amts,tot,avg) = (map negate amts,-tot,-avg)

-- | Figure out the overall date span of a multicolumn balance report.
multiBalanceReportSpan :: MultiBalanceReport -> DateSpan
multiBalanceReportSpan (MultiBalanceReport ([], _, _))       = DateSpan Nothing Nothing
multiBalanceReportSpan (MultiBalanceReport (colspans, _, _)) = DateSpan (spanStart $ head colspans) (spanEnd $ last colspans)

-- | Generates a simple non-columnar BalanceReport, but using multiBalanceReport,
-- in order to support --historical. Does not support tree-mode boring parent eliding.
-- If the normalbalance_ option is set, it adjusts the sorting and sign of amounts
-- (see ReportOpts and CompoundBalanceCommand).
balanceReportFromMultiBalanceReport :: ReportOpts -> Query -> Journal -> BalanceReport
balanceReportFromMultiBalanceReport opts q j = (rows', total)
  where
    MultiBalanceReport (_, rows, (totals, _, _)) = multiBalanceReport opts q j
    rows' = [(a
             ,if flat_ opts then a else a'   -- BalanceReport expects full account name here with --flat
             ,if tree_ opts then d-1 else 0  -- BalanceReport uses 0-based account depths
             , headDef nullmixedamt amts     -- 0 columns is illegal, should not happen, return zeroes if it does
             ) | (a,a',d, amts, _, _) <- rows]
    total = headDef nullmixedamt totals


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
    (opts,journal) `gives` r = do
      let (eitems, etotal) = r
          (MultiBalanceReport (_, aitems, atotal)) = multiBalanceReport opts (queryFromOpts nulldate opts) journal
          showw (acct,acct',indent,lAmt,amt,amt') = (acct, acct', indent, map showMixedAmountDebug lAmt, showMixedAmountDebug amt, showMixedAmountDebug amt')
      (map showw aitems) `is` (map showw eitems)
      ((\(_, b, _) -> showMixedAmountDebug b) atotal) `is` (showMixedAmountDebug etotal) -- we only check the sum of the totals
    usd0 = usd 0
    amount0 = Amount {acommodity="$", aquantity=0, aprice=Nothing, astyle=AmountStyle {ascommodityside = L, ascommodityspaced = False, asprecision = 2, asdecimalpoint = Just '.', asdigitgroups = Nothing}, aismultiplier=False}
  in
   tests "multiBalanceReport" [
      test "null journal"  $
      (defreportopts, nulljournal) `gives` ([], Mixed [nullamt])

     ,test "with -H on a populated period"  $
      (defreportopts{period_= PeriodBetween (fromGregorian 2008 1 1) (fromGregorian 2008 1 2), balancetype_=HistoricalBalance}, samplejournal) `gives`
       (
        [
         ("assets:bank:checking", "checking", 3, [mamountp' "$1.00"] , Mixed [nullamt], Mixed [amount0 {aquantity=1}])
        ,("income:salary"       ,"salary"   , 2, [mamountp' "$-1.00"], Mixed [nullamt], Mixed [amount0 {aquantity=(-1)}])
        ],
        Mixed [nullamt])

     ,_test "a valid history on an empty period"  $
      (defreportopts{period_= PeriodBetween (fromGregorian 2008 1 2) (fromGregorian 2008 1 3), balancetype_=HistoricalBalance}, samplejournal) `gives`
       (
        [
         ("assets:bank:checking","checking",3, [mamountp' "$1.00"], mamountp' "$1.00",Mixed [amount0 {aquantity=1}])
        ,("income:salary","salary",2, [mamountp' "$-1.00"], mamountp' "$-1.00",Mixed [amount0 {aquantity=(-1)}])
        ],
        Mixed [usd0])

     ,_test "a valid history on an empty period (more complex)"  $
      (defreportopts{period_= PeriodBetween (fromGregorian 2009 1 1) (fromGregorian 2009 1 2), balancetype_=HistoricalBalance}, samplejournal) `gives`
       (
        [
        ("assets:bank:checking","checking",3, [mamountp' "$1.00"], mamountp' "$1.00",Mixed [amount0 {aquantity=1}])
        ,("assets:bank:saving","saving",3, [mamountp' "$1.00"], mamountp' "$1.00",Mixed [amount0 {aquantity=1}])
        ,("assets:cash","cash",2, [mamountp' "$-2.00"], mamountp' "$-2.00",Mixed [amount0 {aquantity=(-2)}])
        ,("expenses:food","food",2, [mamountp' "$1.00"], mamountp' "$1.00",Mixed [amount0 {aquantity=(1)}])
        ,("expenses:supplies","supplies",2, [mamountp' "$1.00"], mamountp' "$1.00",Mixed [amount0 {aquantity=(1)}])
        ,("income:gifts","gifts",2, [mamountp' "$-1.00"], mamountp' "$-1.00",Mixed [amount0 {aquantity=(-1)}])
        ,("income:salary","salary",2, [mamountp' "$-1.00"], mamountp' "$-1.00",Mixed [amount0 {aquantity=(-1)}])
        ],
        Mixed [usd0])
    ]
 ]
