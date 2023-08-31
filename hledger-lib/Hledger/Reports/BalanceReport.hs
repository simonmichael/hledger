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

instance HasAmounts BalanceReportItem where
  styleAmounts styles (a,b,c,d) = (a,b,c,styleAmounts styles d)

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
  journalBalanceTransactions defbalancingopts
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

tests_BalanceReport = testGroup "BalanceReport" [

  let
    (rspec,journal) `gives` r = do
      let opts' = rspec{_rsQuery=And [queryFromFlags $ _rsReportOpts rspec, _rsQuery rspec]}
          (eitems, etotal) = r
          (aitems, atotal) = balanceReport opts' journal
          showw (acct,acct',indent,amt) = (acct, acct', indent, showMixedAmountDebug amt)
      (map showw aitems) @?= (map showw eitems)
      (showMixedAmountDebug atotal) @?= (showMixedAmountDebug etotal)
  in
    testGroup "balanceReport" [

     testCase "no args, null journal" $
     (defreportspec, nulljournal) `gives` ([], nullmixedamt)

    ,testCase "no args, sample journal" $
     (defreportspec, samplejournal) `gives`
      ([
        ("assets:bank:checking","assets:bank:checking",0, mixedAmount (usd 1))
       ,("assets:bank:saving","assets:bank:saving",0, mixedAmount (usd 1))
       ,("assets:cash","assets:cash",0, mixedAmount (usd (-2)))
       ,("expenses:food","expenses:food",0, mixedAmount (usd 1))
       ,("expenses:supplies","expenses:supplies",0, mixedAmount (usd 1))
       ,("income:gifts","income:gifts",0, mixedAmount (usd (-1)))
       ,("income:salary","income:salary",0, mixedAmount (usd (-1)))
       ],
       mixedAmount (usd 0))

    ,testCase "with --tree" $
     (defreportspec{_rsReportOpts=defreportopts{accountlistmode_=ALTree}}, samplejournal) `gives`
      ([
        ("assets","assets",0, mixedAmount (usd 0))
       ,("assets:bank","bank",1, mixedAmount (usd 2))
       ,("assets:bank:checking","checking",2, mixedAmount (usd 1))
       ,("assets:bank:saving","saving",2, mixedAmount (usd 1))
       ,("assets:cash","cash",1, mixedAmount (usd (-2)))
       ,("expenses","expenses",0, mixedAmount (usd 2))
       ,("expenses:food","food",1, mixedAmount (usd 1))
       ,("expenses:supplies","supplies",1, mixedAmount (usd 1))
       ,("income","income",0, mixedAmount (usd (-2)))
       ,("income:gifts","gifts",1, mixedAmount (usd (-1)))
       ,("income:salary","salary",1, mixedAmount (usd (-1)))
       ],
       mixedAmount (usd 0))

    ,testCase "with --depth=N" $
     (defreportspec{_rsReportOpts=defreportopts{depth_=Just 1}}, samplejournal) `gives`
      ([
       ("expenses",    "expenses",     0, mixedAmount (usd 2))
       ,("income",      "income",      0, mixedAmount (usd (-2)))
       ],
       mixedAmount (usd 0))

    ,testCase "with depth:N" $
     (defreportspec{_rsQuery=Depth 1}, samplejournal) `gives`
      ([
       ("expenses",    "expenses",     0, mixedAmount (usd 2))
       ,("income",      "income",      0, mixedAmount (usd (-2)))
       ],
       mixedAmount (usd 0))

    ,testCase "with date:" $
     (defreportspec{_rsQuery=Date $ DateSpan (Just $ Exact $ fromGregorian 2009 01 01) (Just $ Exact $ fromGregorian 2010 01 01)}, samplejournal2) `gives`
      ([], nullmixedamt)

    ,testCase "with date2:" $
     (defreportspec{_rsQuery=Date2 $ DateSpan (Just $ Exact $ fromGregorian 2009 01 01) (Just $ Exact $ fromGregorian 2010 01 01)}, samplejournal2) `gives`
      ([
        ("assets:bank:checking","assets:bank:checking",0,mixedAmount (usd 1))
       ,("income:salary","income:salary",0,mixedAmount (usd (-1)))
       ],
       mixedAmount (usd 0))

    ,testCase "with desc:" $
     (defreportspec{_rsQuery=Desc $ toRegexCI' "income"}, samplejournal) `gives`
      ([
        ("assets:bank:checking","assets:bank:checking",0,mixedAmount (usd 1))
       ,("income:salary","income:salary",0, mixedAmount (usd (-1)))
       ],
       mixedAmount (usd 0))

    ,testCase "with not:desc:" $
     (defreportspec{_rsQuery=Not . Desc $ toRegexCI' "income"}, samplejournal) `gives`
      ([
        ("assets:bank:saving","assets:bank:saving",0, mixedAmount (usd 1))
       ,("assets:cash","assets:cash",0, mixedAmount (usd (-2)))
       ,("expenses:food","expenses:food",0, mixedAmount (usd 1))
       ,("expenses:supplies","expenses:supplies",0, mixedAmount (usd 1))
       ,("income:gifts","income:gifts",0, mixedAmount (usd (-1)))
       ],
       mixedAmount (usd 0))

    ,testCase "with period on a populated period" $
      (defreportspec{_rsReportOpts=defreportopts{period_= PeriodBetween (fromGregorian 2008 1 1) (fromGregorian 2008 1 2)}}, samplejournal) `gives`
       (
        [
         ("assets:bank:checking","assets:bank:checking",0, mixedAmount (usd 1))
        ,("income:salary","income:salary",0, mixedAmount (usd (-1)))
        ],
        mixedAmount (usd 0))

     ,testCase "with period on an unpopulated period" $
      (defreportspec{_rsReportOpts=defreportopts{period_= PeriodBetween (fromGregorian 2008 1 2) (fromGregorian 2008 1 3)}}, samplejournal) `gives`
       ([], nullmixedamt)



  {-
      ,testCase "accounts report with account pattern o" ~:
       defreportopts{patterns_=["o"]} `gives`
       ["                  $1  expenses:food"
       ,"                 $-2  income"
       ,"                 $-1    gifts"
       ,"                 $-1    salary"
       ,"--------------------"
       ,"                 $-1"
       ]

      ,testCase "accounts report with account pattern o and --depth 1" ~:
       defreportopts{patterns_=["o"],depth_=Just 1} `gives`
       ["                  $1  expenses"
       ,"                 $-2  income"
       ,"--------------------"
       ,"                 $-1"
       ]

      ,testCase "accounts report with account pattern a" ~:
       defreportopts{patterns_=["a"]} `gives`
       ["                 $-1  assets"
       ,"                  $1    bank:saving"
       ,"                 $-2    cash"
       ,"                 $-1  income:salary"
       ,"                  $1  liabilities:debts"
       ,"--------------------"
       ,"                 $-1"
       ]

      ,testCase "accounts report with account pattern e" ~:
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

      ,testCase "accounts report with unmatched parent of two matched subaccounts" ~:
       defreportopts{patterns_=["cash","saving"]} `gives`
       ["                 $-1  assets"
       ,"                  $1    bank:saving"
       ,"                 $-2    cash"
       ,"--------------------"
       ,"                 $-1"
       ]

      ,testCase "accounts report with multi-part account name" ~:
       defreportopts{patterns_=["expenses:food"]} `gives`
       ["                  $1  expenses:food"
       ,"--------------------"
       ,"                  $1"
       ]

      ,testCase "accounts report with negative account pattern" ~:
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

      ,testCase "accounts report negative account pattern always matches full name" ~:
       defreportopts{patterns_=["not:e"]} `gives`
       ["--------------------"
       ,"                   0"
       ]

      ,testCase "accounts report negative patterns affect totals" ~:
       defreportopts{patterns_=["expenses","not:food"]} `gives`
       ["                  $1  expenses:supplies"
       ,"--------------------"
       ,"                  $1"
       ]

      ,testCase "accounts report with -E shows zero-balance accounts" ~:
       defreportopts{patterns_=["assets"],empty_=True} `gives`
       ["                 $-1  assets"
       ,"                  $1    bank"
       ,"                   0      checking"
       ,"                  $1      saving"
       ,"                 $-2    cash"
       ,"--------------------"
       ,"                 $-1"
       ]

      ,testCase "accounts report with cost basis" $
         j <- (readJournal def Nothing $ unlines
                [""
                ,"2008/1/1 test           "
                ,"  a:b          10h @ $50"
                ,"  c:d                   "
                ]) >>= either error' return
         let j' = journalCanonicaliseAmounts $ journalToCost ToCost j -- enable cost basis adjustment
         balanceReportAsText defreportopts (balanceReport defreportopts Any j') `is`
           ["                $500  a:b"
           ,"               $-500  c:d"
           ,"--------------------"
           ,"                   0"
           ]
  -}
     ]

 ]

