{-|

Balance report, used by the balance command.

-}

{-# LANGUAGE FlexibleInstances, RecordWildCards, ScopedTypeVariables, OverloadedStrings #-}

module Hledger.Reports.BalanceReport (
  BalanceReport,
  BalanceReportItem,
  balanceReport,
  flatShowsExclusiveBalance,
  sortAccountItemsLike,

  -- * Tests
  tests_BalanceReport
)
where

import Data.List
import Data.Ord
import Data.Maybe
import Data.Time.Calendar

import Hledger.Data
import Hledger.Read (mamountp')
import Hledger.Query
import Hledger.Utils
import Hledger.Reports.ReportOptions


-- | A simple balance report. It has:
--
-- 1. a list of items, one per account, each containing:
--
--   * the full account name
--
--   * the Ledger-style elided short account name
--     (the leaf account name, prefixed by any boring parents immediately above);
--     or with --flat, the full account name again
--
--   * the number of indentation steps for rendering a Ledger-style account tree,
--     taking into account elided boring parents, --no-elide and --flat
--
--   * an amount
--
-- 2. the total of all amounts
--
type BalanceReport = ([BalanceReportItem], MixedAmount)
type BalanceReportItem = (AccountName, AccountName, Int, MixedAmount)

-- | When true (the default), this makes balance --flat reports and their implementation clearer.
-- Single/multi-col balance reports currently aren't all correct if this is false.
flatShowsExclusiveBalance    = True

-- | Enabling this makes balance --flat --empty also show parent accounts without postings,
-- in addition to those with postings and a zero balance. Disabling it shows only the latter.
-- No longer supported, but leave this here for a bit.
-- flatShowsPostinglessAccounts = True

-- | Generate a simple balance report, containing the matched accounts and
-- their balances (change of balance) during the specified period.
-- This is like PeriodChangeReport with a single column (but more mature,
-- eg this can do hierarchical display).
balanceReport :: ReportOpts -> Query -> Journal -> BalanceReport
balanceReport ropts@ReportOpts{..} q j@Journal{..} =
  (if invert_ then brNegate  else id) $
  (sorteditems, total)
    where
      -- dbg1 = const id -- exclude from debug output
      dbg1 s = let p = "balanceReport" in Hledger.Utils.dbg1 (p++" "++s)  -- add prefix in debug output

      -- Get all the summed accounts & balances, according to the query, as an account tree.
      -- If doing cost valuation, amounts will be converted to cost first.
      accttree = ledgerRootAccount $ ledgerFromJournal q $ journalSelectingAmountFromOpts ropts j

      -- For other kinds of valuation, convert the summed amounts to value,
      -- per hledger_options.m4.md "Effect of --value on reports".
      valuedaccttree = mapAccounts avalue accttree
        where
          avalue a@Account{..} = a{aebalance=bvalue aebalance, aibalance=bvalue aibalance}
            where
              bvalue = maybe id (mixedAmountApplyValuation (journalPriceOracle j) (journalCommodityStyles j) periodlast mreportlast today multiperiod) value_
                where
                  periodlast =
                    fromMaybe (error' "balanceReport: expected a non-empty journal") $ -- XXX shouldn't happen
                    reportPeriodOrJournalLastDay ropts j
                  mreportlast = reportPeriodLastDay ropts
                  today = fromMaybe (error' "balanceReport: could not pick a valuation date, ReportOpts today_ is unset") today_
                  multiperiod = interval_ /= NoInterval

      -- Modify this tree for display - depth limit, boring parents, zeroes - and convert to a list.
      displayaccts :: [Account]
          | queryDepth q == 0 =
                         dbg1 "displayaccts" $
                         take 1 $ clipAccountsAndAggregate (queryDepth q) $ flattenAccounts valuedaccttree
          | flat_ ropts = dbg1 "displayaccts" $
                         filterzeros $
                         filterempty $
                         drop 1 $ clipAccountsAndAggregate (queryDepth q) $ flattenAccounts valuedaccttree
          | otherwise  = dbg1 "displayaccts" $
                         filter (not.aboring) $
                         drop 1 $ flattenAccounts $
                         markboring $
                         prunezeros $
                         sortAccountTreeByAmount (fromMaybe NormallyPositive normalbalance_) $
                         clipAccounts (queryDepth q) valuedaccttree
          where
            balance     = if flat_ ropts then aebalance else aibalance
            filterzeros = if empty_ then id else filter (not . isZeroMixedAmount . balance)
            filterempty = filter (\a -> anumpostings a > 0 || not (isZeroMixedAmount (balance a)))
            prunezeros  = if empty_ then id else fromMaybe nullacct . pruneAccounts (isZeroMixedAmount . balance)
            markboring  = if no_elide_ then id else markBoringParentAccounts

      -- Make a report row for each account.
      items = dbg1 "items" $ map (balanceReportItem ropts q) displayaccts

      -- Sort report rows (except sorting by amount in tree mode, which was done above).
      sorteditems
        | sort_amount_ && tree_ ropts = items
        | sort_amount_                = sortFlatBRByAmount items
        | otherwise                   = sortBRByAccountDeclaration items
        where
          -- Sort the report rows, representing a flat account list, by row total.
          sortFlatBRByAmount :: [BalanceReportItem] -> [BalanceReportItem]
          sortFlatBRByAmount = sortBy (maybeflip $ comparing (normaliseMixedAmountSquashPricesForDisplay . fourth4))
            where
              maybeflip = if normalbalance_ == Just NormallyNegative then id else flip
          -- Sort the report rows by account declaration order then account name.
          sortBRByAccountDeclaration :: [BalanceReportItem] -> [BalanceReportItem]
          sortBRByAccountDeclaration rows = sortedrows
            where
              anamesandrows = [(first4 r, r) | r <- rows]
              anames = map fst anamesandrows
              sortedanames = sortAccountNamesByDeclaration j (tree_ ropts) anames
              sortedrows = sortAccountItemsLike sortedanames anamesandrows

      -- Calculate the grand total.
      total | not (flat_ ropts) = dbg1 "total" $ sum [amt | (_,_,indent,amt) <- items, indent == 0]
            | otherwise         = dbg1 "total" $
                                  if flatShowsExclusiveBalance
                                  then sum $ map fourth4 items
                                  else sum $ map aebalance $ clipAccountsAndAggregate 1 displayaccts

-- | A sorting helper: sort a list of things (eg report rows) keyed by account name
-- to match the provided ordering of those same account names.
sortAccountItemsLike :: [AccountName] -> [(AccountName, b)] -> [b]
sortAccountItemsLike sortedas items =
  concatMap (\a -> maybe [] (:[]) $ lookup a items) sortedas

-- | In an account tree with zero-balance leaves removed, mark the
-- elidable parent accounts (those with one subaccount and no balance
-- of their own).
markBoringParentAccounts :: Account -> Account
markBoringParentAccounts = tieAccountParents . mapAccounts mark
  where
    mark a | length (asubs a) == 1 && isZeroMixedAmount (aebalance a) = a{aboring=True}
           | otherwise = a

balanceReportItem :: ReportOpts -> Query -> Account -> BalanceReportItem
balanceReportItem opts q a
  | flat_ opts = (name, name,       0,      (if flatShowsExclusiveBalance then aebalance else aibalance) a)
  | otherwise  = (name, elidedname, indent, aibalance a)
  where
    name | queryDepth q > 0 = aname a
         | otherwise        = "..."
    elidedname = accountNameFromComponents (adjacentboringparentnames ++ [accountLeafName name])
    adjacentboringparentnames = reverse $ map (accountLeafName.aname) $ takeWhile aboring parents
    indent = length $ filter (not.aboring) parents
    -- parents exclude the tree's root node
    parents = case parentAccounts a of [] -> []
                                       as -> init as

-- -- the above using the newer multi balance report code:
-- balanceReport' opts q j = (items, total)
--   where
--     MultiBalanceReport (_,mbrrows,mbrtotals) = PeriodChangeReport opts q j
--     items = [(a,a',n, headDef 0 bs) | ((a,a',n), bs) <- mbrrows]
--     total = headDef 0 mbrtotals

-- | Flip the sign of all amounts in a BalanceReport.
brNegate :: BalanceReport -> BalanceReport
brNegate (is, tot) = (map brItemNegate is, -tot)
  where
    brItemNegate (a, a', d, amt) = (a, a', d, -amt)


-- tests

Right samplejournal2 =
  journalBalanceTransactions False
    nulljournal{
      jtxns = [
        txnTieKnot Transaction{
          tindex=0,
          tsourcepos=nullsourcepos,
          tdate=parsedate "2008/01/01",
          tdate2=Just $ parsedate "2009/01/01",
          tstatus=Unmarked,
          tcode="",
          tdescription="income",
          tcomment="",
          ttags=[],
          tpostings=
            [posting {paccount="assets:bank:checking", pamount=Mixed [usd 1]}
            ,posting {paccount="income:salary", pamount=missingmixedamt}
            ],
          tprecedingcomment=""
        }
      ]
    }

tests_BalanceReport = tests "BalanceReport" [
  tests "balanceReport" $
    let
      (opts,journal) `gives` r = do
        let (eitems, etotal) = r
            (aitems, atotal) = balanceReport opts (queryFromOpts nulldate opts) journal
            showw (acct,acct',indent,amt) = (acct, acct', indent, showMixedAmountDebug amt)
        (map showw eitems) `is` (map showw aitems)
        (showMixedAmountDebug etotal) `is` (showMixedAmountDebug atotal)
      usd0 = usd 0
    in [

     test "balanceReport with no args on null journal" $
     (defreportopts, nulljournal) `gives` ([], Mixed [nullamt])

    ,test "balanceReport with no args on sample journal" $
     (defreportopts, samplejournal) `gives`
      ([
        ("assets","assets",0, mamountp' "$0.00")
       ,("assets:bank","bank",1, mamountp' "$2.00")
       ,("assets:bank:checking","checking",2, mamountp' "$1.00")
       ,("assets:bank:saving","saving",2, mamountp' "$1.00")
       ,("assets:cash","cash",1, mamountp' "$-2.00")
       ,("expenses","expenses",0, mamountp' "$2.00")
       ,("expenses:food","food",1, mamountp' "$1.00")
       ,("expenses:supplies","supplies",1, mamountp' "$1.00")
       ,("income","income",0, mamountp' "$-2.00")
       ,("income:gifts","gifts",1, mamountp' "$-1.00")
       ,("income:salary","salary",1, mamountp' "$-1.00")
       ],
       Mixed [usd0])

    ,test "balanceReport with --depth=N" $
     (defreportopts{depth_=Just 1}, samplejournal) `gives`
      ([
       ("expenses",    "expenses",    0, mamountp'  "$2.00")
       ,("income",      "income",      0, mamountp' "$-2.00")
       ],
       Mixed [usd0])

    ,test "balanceReport with depth:N" $
     (defreportopts{query_="depth:1"}, samplejournal) `gives`
      ([
       ("expenses",    "expenses",    0, mamountp'  "$2.00")
       ,("income",      "income",      0, mamountp' "$-2.00")
       ],
       Mixed [usd0])

    ,tests "balanceReport with a date or secondary date span" [
     (defreportopts{query_="date:'in 2009'"}, samplejournal2) `gives`
      ([],
       Mixed [nullamt])
     ,(defreportopts{query_="date2:'in 2009'"}, samplejournal2) `gives`
      ([
        ("assets:bank:checking","assets:bank:checking",0,mamountp' "$1.00")
       ,("income:salary","income:salary",0,mamountp' "$-1.00")
       ],
       Mixed [usd0])
     ]

    ,test "balanceReport with desc:" $
     (defreportopts{query_="desc:income"}, samplejournal) `gives`
      ([
        ("assets:bank:checking","assets:bank:checking",0,mamountp' "$1.00")
       ,("income:salary","income:salary",0, mamountp' "$-1.00")
       ],
       Mixed [usd0])

    ,test "balanceReport with not:desc:" $
     (defreportopts{query_="not:desc:income"}, samplejournal) `gives`
      ([
        ("assets","assets",0, mamountp' "$-1.00")
       ,("assets:bank:saving","bank:saving",1, mamountp' "$1.00")
       ,("assets:cash","cash",1, mamountp' "$-2.00")
       ,("expenses","expenses",0, mamountp' "$2.00")
       ,("expenses:food","food",1, mamountp' "$1.00")
       ,("expenses:supplies","supplies",1, mamountp' "$1.00")
       ,("income:gifts","income:gifts",0, mamountp' "$-1.00")
       ],
       Mixed [usd0])

    ,test "balanceReport with period on a populated period" $
      (defreportopts{period_= PeriodBetween (fromGregorian 2008 1 1) (fromGregorian 2008 1 2)}, samplejournal) `gives`
       (
        [
         ("assets:bank:checking","assets:bank:checking",0, mamountp' "$1.00")
        ,("income:salary","income:salary",0, mamountp' "$-1.00")
        ],
        Mixed [usd0])

     ,test "balanceReport with period on an unpopulated period" $
      (defreportopts{period_= PeriodBetween (fromGregorian 2008 1 2) (fromGregorian 2008 1 3)}, samplejournal) `gives`
       ([],Mixed [nullamt])



  {-
      ,test "accounts report with account pattern o" ~:
       defreportopts{patterns_=["o"]} `gives`
       ["                  $1  expenses:food"
       ,"                 $-2  income"
       ,"                 $-1    gifts"
       ,"                 $-1    salary"
       ,"--------------------"
       ,"                 $-1"
       ]

      ,test "accounts report with account pattern o and --depth 1" ~:
       defreportopts{patterns_=["o"],depth_=Just 1} `gives`
       ["                  $1  expenses"
       ,"                 $-2  income"
       ,"--------------------"
       ,"                 $-1"
       ]

      ,test "accounts report with account pattern a" ~:
       defreportopts{patterns_=["a"]} `gives`
       ["                 $-1  assets"
       ,"                  $1    bank:saving"
       ,"                 $-2    cash"
       ,"                 $-1  income:salary"
       ,"                  $1  liabilities:debts"
       ,"--------------------"
       ,"                 $-1"
       ]

      ,test "accounts report with account pattern e" ~:
       defreportopts{patterns_=["e"]} `gives`
       ["                 $-1  assets"
       ,"                  $1    bank:saving"
       ,"                 $-2    cash"
       ,"                  $2  expenses"
       ,"                  $1    food"
       ,"                  $1    supplies"
       ,"                 $-2  income"
       ,"                 $-1    gifts"
       ,"                 $-1    salary"
       ,"                  $1  liabilities:debts"
       ,"--------------------"
       ,"                   0"
       ]

      ,test "accounts report with unmatched parent of two matched subaccounts" ~:
       defreportopts{patterns_=["cash","saving"]} `gives`
       ["                 $-1  assets"
       ,"                  $1    bank:saving"
       ,"                 $-2    cash"
       ,"--------------------"
       ,"                 $-1"
       ]

      ,test "accounts report with multi-part account name" ~:
       defreportopts{patterns_=["expenses:food"]} `gives`
       ["                  $1  expenses:food"
       ,"--------------------"
       ,"                  $1"
       ]

      ,test "accounts report with negative account pattern" ~:
       defreportopts{patterns_=["not:assets"]} `gives`
       ["                  $2  expenses"
       ,"                  $1    food"
       ,"                  $1    supplies"
       ,"                 $-2  income"
       ,"                 $-1    gifts"
       ,"                 $-1    salary"
       ,"                  $1  liabilities:debts"
       ,"--------------------"
       ,"                  $1"
       ]

      ,test "accounts report negative account pattern always matches full name" ~:
       defreportopts{patterns_=["not:e"]} `gives`
       ["--------------------"
       ,"                   0"
       ]

      ,test "accounts report negative patterns affect totals" ~:
       defreportopts{patterns_=["expenses","not:food"]} `gives`
       ["                  $1  expenses:supplies"
       ,"--------------------"
       ,"                  $1"
       ]

      ,test "accounts report with -E shows zero-balance accounts" ~:
       defreportopts{patterns_=["assets"],empty_=True} `gives`
       ["                 $-1  assets"
       ,"                  $1    bank"
       ,"                   0      checking"
       ,"                  $1      saving"
       ,"                 $-2    cash"
       ,"--------------------"
       ,"                 $-1"
       ]

      ,test "accounts report with cost basis" $
         j <- (readJournal def Nothing $ unlines
                [""
                ,"2008/1/1 test           "
                ,"  a:b          10h @ $50"
                ,"  c:d                   "
                ]) >>= either error' return
         let j' = journalCanonicaliseAmounts $ journalToCost j -- enable cost basis adjustment
         balanceReportAsText defreportopts (balanceReport defreportopts Any j') `is`
           ["                $500  a:b"
           ,"               $-500  c:d"
           ,"--------------------"
           ,"                   0"
           ]
  -}
   ]

 ]

