{-# LANGUAGE FlexibleInstances, ScopedTypeVariables, OverloadedStrings #-}
{-|

Multi-column balance reports, used by the balance command.

-}

module Hledger.Reports.MultiBalanceReports (
  MultiBalanceReport(..),
  MultiBalanceReportRow,
  multiBalanceReport,
  balanceReportFromMultiBalanceReport,
  mbrNegate,
  mbrNormaliseSign,
  multiBalanceReportSpan,
  tableAsText,

  -- -- * Tests
  tests_MultiBalanceReports
)
where

import Control.Applicative ((<|>))
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


-- | A multi balance report is a balance report with one or more columns. It has:
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
--   * a list of amounts, one for each column
--
--   * the total of the row's amounts
--
--   * the average of the row's amounts
--
-- 3. the column totals and the overall total and average
--
-- The meaning of the amounts depends on the type of multi balance
-- report, of which there are three: periodic, cumulative and historical
-- (see 'BalanceType' and "Hledger.Cli.Commands.Balance").
newtype MultiBalanceReport =
  MultiBalanceReport ([DateSpan]
                     ,[MultiBalanceReportRow]
                     ,MultiBalanceReportTotals
                     )
type MultiBalanceReportRow    = (AccountName, AccountName, Int, [MixedAmount], MixedAmount, MixedAmount)
type MultiBalanceReportTotals = ([MixedAmount], MixedAmount, MixedAmount) -- (Totals list, sum of totals, average of totals)

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
multiBalanceReport :: ReportOpts -> Query -> Journal -> MultiBalanceReport
multiBalanceReport opts q j =
  (if invert_ opts then mbrNegate else id) $ 
  (if value_ opts then mbrValue opts j else id) $
  MultiBalanceReport (displayspans, sorteditems, totalsrow)
    where
      symq       = dbg1 "symq"   $ filterQuery queryIsSym $ dbg1 "requested q" q
      depthq     = dbg1 "depthq" $ filterQuery queryIsDepth q
      depth      = queryDepth depthq
      depthless  = dbg1 "depthless" . filterQuery (not . queryIsDepth)
      datelessq  = dbg1 "datelessq"  $ filterQuery (not . queryIsDateOrDate2) q
      dateqcons  = if date2_ opts then Date2 else Date
      -- The date span specified by -b/-e/-p options and query args if any.
      requestedspan  = dbg1 "requestedspan"  $ queryDateSpan (date2_ opts) q
      -- If the requested span is open-ended, close it using the journal's end dates.
      -- This can still be the null (open) span if the journal is empty.
      requestedspan' = dbg1 "requestedspan'" $ requestedspan `spanDefaultsFrom` journalDateSpan (date2_ opts) j
      -- The list of interval spans enclosing the requested span.
      -- This list can be empty if the journal was empty,
      -- or if hledger-ui has added its special date:-tomorrow to the query
      -- and all txns are in the future.
      intervalspans  = dbg1 "intervalspans"  $ splitSpan (interval_ opts) requestedspan'           
      -- The requested span enlarged to enclose a whole number of intervals.
      -- This can be the null span if there were no intervals. 
      reportspan     = dbg1 "reportspan"     $ DateSpan (maybe Nothing spanStart $ headMay intervalspans)
                                                        (maybe Nothing spanEnd   $ lastMay intervalspans)
      -- The user's query with no depth limit, and expanded to the report span
      -- if there is one (otherwise any date queries are left as-is, which
      -- handles the hledger-ui+future txns case above).
      reportq   = dbg1 "reportq" $ depthless $ 
        if reportspan == nulldatespan 
        then q 
        else And [datelessq, reportspandatesq]
          where
            reportspandatesq = dbg1 "reportspandatesq" $ dateqcons reportspan
      -- q projected back before the report start date, to calculate starting balances.
      -- When there's no report start date, in case there are future txns (the hledger-ui case above),
      -- we use emptydatespan to make sure they aren't counted as starting balance.  
      startbalq = dbg1 "startbalq" $ And [datelessq, dateqcons precedingspan]
        where
          precedingspan = case spanStart reportspan of
                            Just d  -> DateSpan Nothing (Just d)
                            Nothing -> emptydatespan 
      ps :: [Posting] =
          dbg1 "ps" $
          journalPostings $
          filterJournalAmounts symq $     -- remove amount parts excluded by cur:
          filterJournalPostings reportq $        -- remove postings not matched by (adjusted) query
          journalSelectingAmountFromOpts opts j

      displayspans = dbg1 "displayspans" $ splitSpan (interval_ opts) displayspan
        where
          displayspan
            | empty_ opts = dbg1 "displayspan (-E)" reportspan                                -- all the requested intervals
            | otherwise   = dbg1 "displayspan"      $ requestedspan `spanIntersect` matchedspan -- exclude leading/trailing empty intervals
          matchedspan = dbg1 "matchedspan" $ postingsDateSpan' (whichDateFromOpts opts) ps

      psPerSpan :: [[Posting]] =
          dbg1 "psPerSpan"
          [filter (isPostingInDateSpan' (whichDateFromOpts opts) s) ps | s <- displayspans]

      postedAcctBalChangesPerSpan :: [[(ClippedAccountName, MixedAmount)]] =
          dbg1 "postedAcctBalChangesPerSpan" $
          map postingAcctBals psPerSpan
          where
            postingAcctBals :: [Posting] -> [(ClippedAccountName, MixedAmount)]
            postingAcctBals ps = [(aname a, (if tree_ opts then aibalance else aebalance) a) | a <- as]
                where
                  as = depthLimit $
                       (if tree_ opts then id else filter ((>0).anumpostings)) $
                       drop 1 $ accountsFromPostings ps
                  depthLimit
                      | tree_ opts = filter ((depthq `matchesAccount`).aname) -- exclude deeper balances
                      | otherwise  = clipAccountsAndAggregate depth -- aggregate deeper balances at the depth limit

      postedAccts :: [AccountName] = dbg1 "postedAccts" $ sort $ accountNamesFromPostings ps

      -- starting balances and accounts from transactions before the report start date
      startacctbals = dbg1 "startacctbals" $ map (\(a,_,_,b) -> (a,b)) startbalanceitems
          where
            (startbalanceitems,_) = dbg1 "starting balance report" $ balanceReport opts' startbalq j
                                    where
                                      opts' | tree_ opts = opts{no_elide_=True}
                                            | otherwise  = opts{accountlistmode_=ALFlat}
      startingBalanceFor a = fromMaybe nullmixedamt $ lookup a startacctbals
      startAccts = dbg1 "startAccts" $ map fst startacctbals

      displayedAccts :: [ClippedAccountName] =
          dbg1 "displayedAccts" $
          (if tree_ opts then expandAccountNames else id) $
          nub $ map (clipOrEllipsifyAccountName depth) $
          if empty_ opts || (balancetype_ opts) == HistoricalBalance then nub $ sort $ startAccts ++ postedAccts else postedAccts

      acctBalChangesPerSpan :: [[(ClippedAccountName, MixedAmount)]] =
          dbg1 "acctBalChangesPerSpan"
          [sortBy (comparing fst) $ unionBy (\(a,_) (a',_) -> a == a') postedacctbals zeroes
           | postedacctbals <- postedAcctBalChangesPerSpan]
          where zeroes = [(a, nullmixedamt) | a <- displayedAccts]

      acctBalChanges :: [(ClippedAccountName, [MixedAmount])] =
          dbg1 "acctBalChanges"
          [(a, map snd abs) | abs@((a,_):_) <- transpose acctBalChangesPerSpan] -- never null, or used when null...

      items :: [MultiBalanceReportRow] =
          dbg1 "items" $
          [(a, accountLeafName a, accountNameLevel a, displayedBals, rowtot, rowavg)
           | (a,changes) <- acctBalChanges
           , let displayedBals = case balancetype_ opts of
                                  HistoricalBalance -> drop 1 $ scanl (+) (startingBalanceFor a) changes
                                  CumulativeChange -> drop 1 $ scanl (+) nullmixedamt changes
                                  _                 -> changes
           , let rowtot = sum displayedBals
           , let rowavg = averageMixedAmounts displayedBals
           , empty_ opts || depth == 0 || any (not . isZeroMixedAmount) displayedBals
           ]

      -- TODO TBD: is it always ok to sort report rows after report has been generated ?
      -- Or does sorting sometimes need to be done as part of the report generation ?  
      sorteditems :: [MultiBalanceReportRow] =
        dbg1 "sorteditems" $
        sortitems items
        where
          sortitems
            | sort_amount_ opts && accountlistmode_ opts == ALTree       = sortTreeMBRByAmount
            | sort_amount_ opts                                          = sortFlatMBRByAmount
            | otherwise                                                  = sortMBRByAccountDeclaration
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
                  sortedaccounttree = sortAccountTreeByAmount (fromMaybe NormallyPositive $ normalbalance_ opts) accounttreewithbals
                  sortedanames = map aname $ drop 1 $ flattenAccounts sortedaccounttree
                  sortedrows = sortAccountItemsLike sortedanames anamesandrows 

              -- Sort the report rows, representing a flat account list, by row total. 
              sortFlatMBRByAmount = sortBy (maybeflip $ comparing (normaliseMixedAmountSquashPricesForDisplay . fifth6))
                where
                  maybeflip = if normalbalance_ opts == Just NormallyNegative then id else flip

              -- Sort the report rows by account declaration order then account name. 
              sortMBRByAccountDeclaration rows = sortedrows
                where 
                  anamesandrows = [(first6 r, r) | r <- rows]
                  anames = map fst anamesandrows
                  sortedanames = sortAccountNamesByDeclaration j (tree_ opts) anames
                  sortedrows = sortAccountItemsLike sortedanames anamesandrows 

      totals :: [MixedAmount] =
          -- dbg1 "totals" $
          map sum balsbycol
          where
            balsbycol = transpose [bs | (a,_,_,bs,_,_) <- sorteditems, not (tree_ opts) || a `elem` highestlevelaccts]
            highestlevelaccts     =
                dbg1 "highestlevelaccts"
                [a | a <- displayedAccts, not $ any (`elem` displayedAccts) $ init $ expandAccountName a]

      totalsrow :: MultiBalanceReportTotals =
          dbg1 "totalsrow"
          (totals, sum totals, averageMixedAmounts totals)

      dbg1 s = let p = "multiBalanceReport" in Hledger.Utils.dbg1 (p++" "++s)  -- add prefix in this function's debug output
      -- dbg1 = const id  -- exclude this function from debug output

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

-- | Convert all the posting amounts in a MultiBalanceReport to their
-- default valuation commodities. This means using the Journal's most
-- recent applicable market prices before the valuation date.
-- The valuation date is the specified report end date if any,
-- otherwise the current date, otherwise the journal's end date.
mbrValue :: ReportOpts -> Journal -> MultiBalanceReport -> MultiBalanceReport
mbrValue ropts j r =
  let mvaluationdate = periodEnd (period_ ropts) <|> today_ ropts <|> journalEndDate False j
  in case mvaluationdate of
    Nothing -> r
    Just d  -> r'
      where
        -- prices are in parse order - sort into date then parse order,
        -- & reversed for quick lookup of the latest price.
        prices = reverse $ sortOn mpdate $ jmarketprices j

        MultiBalanceReport (spans, rows, (coltotals, rowtotaltotal, rowavgtotal)) = r
        r' = MultiBalanceReport
             (spans,
              [(acct, acct', depth, map convert rowamts, convert rowtotal, convert rowavg) | (acct, acct', depth, rowamts, rowtotal, rowavg) <- rows],
              (map convert coltotals, convert rowtotaltotal, convert rowavgtotal))
        convert = mixedAmountValue prices d

    -- -- convert to value ?
    -- -- first get period end date(s) XXX duplicated from multiBalanceReport
    -- -- The date span specified by -b/-e/-p options and query args if any.
    -- requestedspan  = dbg1 "requestedspan"  $ queryDateSpan (date2_ ropts) userq  --  XXX userq ok ?
    -- -- If the requested span is open-ended, close it using the journal's end dates.
    -- -- This can still be the null (open) span if the journal is empty.
    -- requestedspan' = dbg1 "requestedspan'" $ requestedspan `spanDefaultsFrom` journalDateSpan (date2_ ropts) j
    -- -- The list of interval spans enclosing the requested span.
    -- -- This list can be empty if the journal was empty,
    -- -- or if hledger-ui has added its special date:-tomorrow to the query
    -- -- and all txns are in the future.
    -- -- intervalspans  = dbg1 "intervalspans"  $ splitSpan (interval_ ropts) requestedspan'


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

tests_MultiBalanceReports = tests "MultiBalanceReports" [
  let
    (opts,journal) `gives` r = do
      let (eitems, etotal) = r
          (MultiBalanceReport (_, aitems, atotal)) = multiBalanceReport opts (queryFromOpts nulldate opts) journal
          showw (acct,acct',indent,lAmt,amt,amt') = (acct, acct', indent, map showMixedAmountDebug lAmt, showMixedAmountDebug amt, showMixedAmountDebug amt')
      (map showw aitems) `is` (map showw eitems)
      ((\(_, b, _) -> showMixedAmountDebug b) atotal) `is` (showMixedAmountDebug etotal) -- we only check the sum of the totals
    usd0 = usd 0
    amount0 = Amount {acommodity="$", aquantity=0, aprice=NoPrice, astyle=AmountStyle {ascommodityside = L, ascommodityspaced = False, asprecision = 2, asdecimalpoint = Just '.', asdigitgroups = Nothing}, aismultiplier=False}
  in 
   tests "multiBalanceReport" [
      test "null journal"  $
      (defreportopts, nulljournal) `gives` ([], Mixed [nullamt])
  
     ,test "with -H on a populated period"  $
      (defreportopts{period_= PeriodBetween (fromGregorian 2008 1 1) (fromGregorian 2008 1 2), balancetype_=HistoricalBalance}, samplejournal) `gives`
       (
        [
         ("assets:bank:checking","checking",3, [mamountp' "$1.00"], mamountp' "$1.00",Mixed [amount0 {aquantity=1}])
        ,("income:salary","salary",2, [mamountp' "$-1.00"], mamountp' "$-1.00",Mixed [amount0 {aquantity=(-1)}])
        ],
        Mixed [usd0])
  
     ,test "a valid history on an empty period"  $
      (defreportopts{period_= PeriodBetween (fromGregorian 2008 1 2) (fromGregorian 2008 1 3), balancetype_=HistoricalBalance}, samplejournal) `gives`
       (
        [
         ("assets:bank:checking","checking",3, [mamountp' "$1.00"], mamountp' "$1.00",Mixed [amount0 {aquantity=1}])
        ,("income:salary","salary",2, [mamountp' "$-1.00"], mamountp' "$-1.00",Mixed [amount0 {aquantity=(-1)}])
        ],
        Mixed [usd0])
  
     ,test "a valid history on an empty period (more complex)"  $
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
