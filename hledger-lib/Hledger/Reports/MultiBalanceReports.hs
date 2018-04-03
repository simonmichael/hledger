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
  tests_Hledger_Reports_MultiBalanceReport
)
where

import Data.List
import Data.Maybe
import Data.Ord
import Data.Time.Calendar
import Safe
import Test.HUnit
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
    -- use ppShow to break long lists onto multiple lines
    -- we add some bogus extra shows here to help ppShow parse the output
    -- and wrap tuples and lists properly
    show (MultiBalanceReport (spans, items, totals)) =
        "MultiBalanceReport (ignore extra quotes):\n" ++ ppShow (show spans, map show items, totals)

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
  MultiBalanceReport (displayspans, sorteditems, totalsrow)
    where
      symq       = dbg1 "symq"   $ filterQuery queryIsSym $ dbg1 "requested q" q
      depthq     = dbg1 "depthq" $ filterQuery queryIsDepth q
      depth      = queryDepth depthq
      depthless  = dbg1 "depthless" . filterQuery (not . queryIsDepth)
      datelessq  = dbg1 "datelessq"  $ filterQuery (not . queryIsDateOrDate2) q
      dateqcons  = if date2_ opts then Date2 else Date
      precedingq = dbg1 "precedingq" $ And [datelessq, dateqcons $ DateSpan Nothing (spanStart reportspan)]
      requestedspan  = dbg1 "requestedspan"  $ queryDateSpan (date2_ opts) q                              -- span specified by -b/-e/-p options and query args
      requestedspan' = dbg1 "requestedspan'" $ requestedspan `spanDefaultsFrom` journalDateSpan (date2_ opts) j  -- if open-ended, close it using the journal's end dates
      intervalspans  = dbg1 "intervalspans"  $ splitSpan (interval_ opts) requestedspan'           -- interval spans enclosing it
      reportspan     = dbg1 "reportspan"     $ DateSpan (maybe Nothing spanStart $ headMay intervalspans) -- the requested span enlarged to a whole number of intervals
                                                       (maybe Nothing spanEnd   $ lastMay intervalspans)
      newdatesq = dbg1 "newdateq" $ dateqcons reportspan
      reportq  = dbg1 "reportq" $ depthless $ And [datelessq, newdatesq] -- user's query enlarged to whole intervals and with no depth limit

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
            (startbalanceitems,_) = dbg1 "starting balance report" $ balanceReport opts' precedingq j
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

      sorteditems :: [MultiBalanceReportRow] =
        dbg1 "sorteditems" $
        sortitems items
        where
          sortitems
            | sort_amount_ opts && accountlistmode_ opts == ALTree       = sortTreeMultiBalanceReportRowsByAmount
            | sort_amount_ opts                                          = sortFlatMultiBalanceReportRowsByAmount
            | not (sort_amount_ opts) && accountlistmode_ opts == ALTree = sortTreeMultiBalanceReportRowsByAccountCodeAndName
            | otherwise                                                  = sortFlatMultiBalanceReportRowsByAccountCodeAndName
            where
              -- Sort the report rows, representing a flat account list, by row total. 
              sortFlatMultiBalanceReportRowsByAmount = sortBy (maybeflip $ comparing fifth6)
                where
                  maybeflip = if normalbalance_ opts == Just NormallyNegative then id else flip

              -- Sort the report rows, representing a tree of accounts, by row total at each level.
              -- To do this we recreate an Account tree with the row totals as balances, 
              -- so we can do a hierarchical sort, flatten again, and then reorder the  
              -- report rows similarly. Yes this is pretty long winded. 
              sortTreeMultiBalanceReportRowsByAmount rows = sortedrows
                where
                  anamesandrows = [(first6 r, r) | r <- rows]
                  anames = map fst anamesandrows
                  atotals = [(a,tot) | (a,_,_,_,tot,_) <- rows]
                  nametree = treeFromPaths $ map expandAccountName anames
                  accounttree = nameTreeToAccount "root" nametree
                  accounttreewithbals = mapAccounts setibalance accounttree
                    where
                      -- this error should not happen, but it's ugly TODO 
                      setibalance a = a{aibalance=fromMaybe (error "sortTreeMultiBalanceReportRowsByAmount 1") $ lookup (aname a) atotals}
                  sortedaccounttree = sortAccountTreeByAmount (fromMaybe NormallyPositive $ normalbalance_ opts) accounttreewithbals
                  sortedaccounts = drop 1 $ flattenAccounts sortedaccounttree
                  -- dropped the root account, also ignore any parent accounts not in rows
                  sortedrows = concatMap (\a -> maybe [] (:[]) $ lookup (aname a) anamesandrows) sortedaccounts 

              -- Sort the report rows by account code if any, with the empty account code coming last, then account name. 
              sortFlatMultiBalanceReportRowsByAccountCodeAndName = sortBy (comparing acodeandname)
                where
                  acodeandname r = (acode', aname)
                    where
                      aname = first6 r
                      macode = fromMaybe Nothing $ lookup aname $ jaccounts j
                      acode' = fromMaybe maxBound macode 

              -- Sort the report rows, representing a tree of accounts, by account code and then account name at each level.
              -- Convert a tree of account names, look up the account codes, sort and flatten the tree, reorder the rows.
              sortTreeMultiBalanceReportRowsByAccountCodeAndName rows = sortedrows
                where
                  anamesandrows = [(first6 r, r) | r <- rows]
                  anames = map fst anamesandrows
                  nametree = treeFromPaths $ map expandAccountName anames
                  accounttree = nameTreeToAccount "root" nametree
                  accounttreewithcodes = mapAccounts (accountSetCodeFrom j) accounttree
                  sortedaccounttree = sortAccountTreeByAccountCodeAndName accounttreewithcodes
                  sortedaccounts = drop 1 $ flattenAccounts sortedaccounttree
                  -- dropped the root account, also ignore any parent accounts not in rows
                  sortedrows = concatMap (\a -> maybe [] (:[]) $ lookup (aname a) anamesandrows) sortedaccounts 

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


tests_multiBalanceReport =
  let
    (opts,journal) `gives` r = do
      let (eitems, etotal) = r
          (MultiBalanceReport (_, aitems, atotal)) = multiBalanceReport opts (queryFromOpts nulldate opts) journal
          showw (acct,acct',indent,lAmt,amt,amt') = (acct, acct', indent, map showMixedAmountDebug lAmt, showMixedAmountDebug amt, showMixedAmountDebug amt')
      assertEqual "items" (map showw eitems) (map showw aitems)
      assertEqual "total" (showMixedAmountDebug etotal) ((\(_, b, _) -> showMixedAmountDebug b) atotal) -- we only check the sum of the totals
    usd0 = usd 0
    amount0 = Amount {acommodity="$", aquantity=0, aprice=NoPrice, astyle=AmountStyle {ascommodityside = L, ascommodityspaced = False, asprecision = 2, asdecimalpoint = Just '.', asdigitgroups = Nothing}, amultiplier=False}
  in [
   "multiBalanceReport with no args on null journal" ~: do
   (defreportopts, nulljournal) `gives` ([], Mixed [nullamt])

   ,"multiBalanceReport with -H on a populated period" ~: do
    (defreportopts{period_= PeriodBetween (fromGregorian 2008 1 1) (fromGregorian 2008 1 2), balancetype_=HistoricalBalance}, samplejournal) `gives`
     (
      [
       ("assets:bank:checking","checking",3, [mamountp' "$1.00"], mamountp' "$1.00",Mixed [amount0 {aquantity=1}])
      ,("income:salary","salary",2, [mamountp' "$-1.00"], mamountp' "$-1.00",Mixed [amount0 {aquantity=(-1)}])
      ],
      Mixed [usd0])

   ,"multiBalanceReport tests the ability to have a valid history on an empty period" ~: do
    (defreportopts{period_= PeriodBetween (fromGregorian 2008 1 2) (fromGregorian 2008 1 3), balancetype_=HistoricalBalance}, samplejournal) `gives`
     (
      [
       ("assets:bank:checking","checking",3, [mamountp' "$1.00"], mamountp' "$1.00",Mixed [amount0 {aquantity=1}])
      ,("income:salary","salary",2, [mamountp' "$-1.00"], mamountp' "$-1.00",Mixed [amount0 {aquantity=(-1)}])
      ],
      Mixed [usd0])

   ,"multiBalanceReport tests the ability to have a valid history on an empty period (More complex)" ~: do
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

tests_Hledger_Reports_MultiBalanceReport :: Test
tests_Hledger_Reports_MultiBalanceReport = TestList
  tests_multiBalanceReport
