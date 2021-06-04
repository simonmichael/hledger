{-|

Balance report, used by the balance command.

-}

{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Hledger.Reports.BalanceReport (
  BalanceReport,
  BalanceReportItem,
  balanceReport,
  flatShowsExclusiveBalance,

  -- * Tests
  tests_BalanceReport
)
where

import Data.Time.Calendar

import Hledger.Data
import Hledger.Read (mamountp')
import Hledger.Query
import Hledger.Utils
import Hledger.Reports.MultiBalanceReport (multiBalanceReport)
import Hledger.Reports.ReportOptions
import Hledger.Reports.ReportTypes


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
-- If the normalbalance_ option is set, it adjusts the sorting and sign of
-- amounts (see ReportOpts and CompoundBalanceCommand).
balanceReport :: ReportSpec -> Journal -> BalanceReport
balanceReport rspec j = (rows, total)
  where
    report = multiBalanceReport rspec j
    rows = [( prrFullName row
            , prrDisplayName row
            , prrDepth row - 1  -- BalanceReport uses 0-based account depths
            , prrTotal row
            ) | row <- prRows report]
    total = prrTotal $ prTotals report


-- tests

Right samplejournal2 =
  journalBalanceTransactions balancingOpts
    nulljournal{
      jtxns = [
        txnTieKnot Transaction{
          tindex=0,
          tsourcepos=nullsourcepos,
          tdate=fromGregorian 2008 01 01,
          tdate2=Just $ fromGregorian 2009 01 01,
          tstatus=Unmarked,
          tcode="",
          tdescription="income",
          tcomment="",
          ttags=[],
          tpostings=
            [posting {paccount="assets:bank:checking", pamount=mixedAmount (usd 1)}
            ,posting {paccount="income:salary", pamount=missingmixedamt}
            ],
          tprecedingcomment=""
        }
      ]
    }

tests_BalanceReport = tests "BalanceReport" [

  let
    (rspec,journal) `gives` r = do
      let opts' = rspec{rsQuery=And [queryFromFlags $ rsOpts rspec, rsQuery rspec]}
          (eitems, etotal) = r
          (aitems, atotal) = balanceReport opts' journal
          showw (acct,acct',indent,amt) = (acct, acct', indent, showMixedAmountDebug amt)
      (map showw aitems) @?= (map showw eitems)
      (showMixedAmountDebug atotal) @?= (showMixedAmountDebug etotal)
  in
    tests "balanceReport" [

     test "no args, null journal" $
     (defreportspec, nulljournal) `gives` ([], nullmixedamt)

    ,test "no args, sample journal" $
     (defreportspec, samplejournal) `gives`
      ([
        ("assets:bank:checking","assets:bank:checking",0, mamountp' "$1.00")
       ,("assets:bank:saving","assets:bank:saving",0, mamountp' "$1.00")
       ,("assets:cash","assets:cash",0, mamountp' "$-2.00")
       ,("expenses:food","expenses:food",0, mamountp' "$1.00")
       ,("expenses:supplies","expenses:supplies",0, mamountp' "$1.00")
       ,("income:gifts","income:gifts",0, mamountp' "$-1.00")
       ,("income:salary","income:salary",0, mamountp' "$-1.00")
       ],
       mixedAmount (usd 0))

    ,test "with --tree" $
     (defreportspec{rsOpts=defreportopts{accountlistmode_=ALTree}}, samplejournal) `gives`
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
       mixedAmount (usd 0))

    ,test "with --depth=N" $
     (defreportspec{rsOpts=defreportopts{depth_=Just 1}}, samplejournal) `gives`
      ([
       ("expenses",    "expenses",    0, mamountp'  "$2.00")
       ,("income",      "income",      0, mamountp' "$-2.00")
       ],
       mixedAmount (usd 0))

    ,test "with depth:N" $
     (defreportspec{rsQuery=Depth 1}, samplejournal) `gives`
      ([
       ("expenses",    "expenses",    0, mamountp'  "$2.00")
       ,("income",      "income",      0, mamountp' "$-2.00")
       ],
       mixedAmount (usd 0))

    ,test "with date:" $
     (defreportspec{rsQuery=Date $ DateSpan (Just $ fromGregorian 2009 01 01) (Just $ fromGregorian 2010 01 01)}, samplejournal2) `gives`
      ([], nullmixedamt)

    ,test "with date2:" $
     (defreportspec{rsQuery=Date2 $ DateSpan (Just $ fromGregorian 2009 01 01) (Just $ fromGregorian 2010 01 01)}, samplejournal2) `gives`
      ([
        ("assets:bank:checking","assets:bank:checking",0,mamountp' "$1.00")
       ,("income:salary","income:salary",0,mamountp' "$-1.00")
       ],
       mixedAmount (usd 0))

    ,test "with desc:" $
     (defreportspec{rsQuery=Desc $ toRegexCI' "income"}, samplejournal) `gives`
      ([
        ("assets:bank:checking","assets:bank:checking",0,mamountp' "$1.00")
       ,("income:salary","income:salary",0, mamountp' "$-1.00")
       ],
       mixedAmount (usd 0))

    ,test "with not:desc:" $
     (defreportspec{rsQuery=Not . Desc $ toRegexCI' "income"}, samplejournal) `gives`
      ([
        ("assets:bank:saving","assets:bank:saving",0, mamountp' "$1.00")
       ,("assets:cash","assets:cash",0, mamountp' "$-2.00")
       ,("expenses:food","expenses:food",0, mamountp' "$1.00")
       ,("expenses:supplies","expenses:supplies",0, mamountp' "$1.00")
       ,("income:gifts","income:gifts",0, mamountp' "$-1.00")
       ],
       mixedAmount (usd 0))

    ,test "with period on a populated period" $
      (defreportspec{rsOpts=defreportopts{period_= PeriodBetween (fromGregorian 2008 1 1) (fromGregorian 2008 1 2)}}, samplejournal) `gives`
       (
        [
         ("assets:bank:checking","assets:bank:checking",0, mamountp' "$1.00")
        ,("income:salary","income:salary",0, mamountp' "$-1.00")
        ],
        mixedAmount (usd 0))

     ,test "with period on an unpopulated period" $
      (defreportspec{rsOpts=defreportopts{period_= PeriodBetween (fromGregorian 2008 1 2) (fromGregorian 2008 1 3)}}, samplejournal) `gives`
       ([], nullmixedamt)



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

