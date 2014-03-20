{-# LANGUAGE RecordWildCards, DeriveDataTypeable, FlexibleInstances #-}
{-|

Balance report, used by the balance command.

-}

module Hledger.Reports.BalanceReport (
  BalanceReport,
  BalanceReportItem,
  RenderableAccountName,
  balanceReport,

  -- * Tests
  tests_Hledger_Reports_BalanceReport
)
where

import Data.Maybe
import Test.HUnit

import Hledger.Data
import Hledger.Read (mamountp')
import Hledger.Query
import Hledger.Utils
import Hledger.Reports.ReportOptions


-- | A simple single-column balance report. It has:
--
-- 1. a list of rows, each containing a renderable account name and a corresponding amount
--
-- 2. the final total of the amounts
type BalanceReport = ([BalanceReportItem], MixedAmount)
type BalanceReportItem = (RenderableAccountName, MixedAmount)

-- | A renderable account name includes some additional hints for rendering accounts in a balance report.
-- It has:
--
-- * The full account name
-- 
-- * The ledger-style short elided account name (the leaf name, prefixed by any boring parents immediately above)
-- 
-- * The number of indentation steps to use when rendering a ledger-style account tree
--   (normally the 0-based depth of this account excluding boring parents, or 0 with --flat).
type RenderableAccountName = (AccountName, AccountName, Int)

-- | Generate a simple balance report, containing the matched accounts and
-- their balances (change of balance) during the specified period.
-- This is like periodBalanceReport with a single column (but more mature,
-- eg this can do hierarchical display).
balanceReport :: ReportOpts -> Query -> Journal -> BalanceReport
balanceReport opts q j = (items, total)
    where
      l =  ledgerFromJournal q $ journalSelectingAmountFromOpts opts j
      accts =
          dbg "accts1" $ 
          clipAccounts (queryDepth q) $ -- exclude accounts deeper than specified depth
          ledgerRootAccount l
      accts'
          | flat_ opts = filterzeros $ tail $ flattenAccounts accts
          | otherwise  = filter (not.aboring) $ tail $ flattenAccounts $ markboring $ prunezeros accts
          where
            filterzeros | empty_ opts = id
                        | otherwise = filter (not . isZeroMixedAmount . aebalance)
            prunezeros | empty_ opts = id
                       | otherwise   = fromMaybe nullacct . pruneAccounts (isZeroMixedAmount.aibalance)
            markboring | no_elide_ opts = id
                       | otherwise      = markBoringParentAccounts
      items = map (balanceReportItem opts) accts'
      total = sum [amt | ((a,_,indent),amt) <- items, if flat_ opts then accountNameLevel a == 1 else indent == 0]
              -- XXX check account level == 1 is valid when top-level accounts excluded

-- | In an account tree with zero-balance leaves removed, mark the
-- elidable parent accounts (those with one subaccount and no balance
-- of their own).
markBoringParentAccounts :: Account -> Account
markBoringParentAccounts = tieAccountParents . mapAccounts mark
  where
    mark a | length (asubs a) == 1 && isZeroMixedAmount (aebalance a) = a{aboring=True}
           | otherwise = a

balanceReportItem :: ReportOpts -> Account -> BalanceReportItem
balanceReportItem opts a@Account{aname=name, aibalance=ibal}
  | flat_ opts = ((name, name,       0),      ibal)
  | otherwise  = ((name, elidedname, indent), ibal)
  where
    elidedname = accountNameFromComponents (adjacentboringparentnames ++ [accountLeafName name])
    adjacentboringparentnames = reverse $ map (accountLeafName.aname) $ takeWhile aboring $ parents
    indent = length $ filter (not.aboring) parents
    parents = init $ parentAccounts a

-- -- the above using the newer multi balance report code:
-- balanceReport' opts q j = (items, total)
--   where
--     MultiBalanceReport (_,mbrrows,mbrtotals) = periodBalanceReport opts q j
--     items = [(a,a',n, headDef 0 bs) | ((a,a',n), bs) <- mbrrows]
--     total = headDef 0 mbrtotals

tests_balanceReport =
  let (opts,journal) `gives` r = do
         let (eitems, etotal) = r
             (aitems, atotal) = balanceReport opts (queryFromOpts nulldate opts) journal
         assertEqual "items" eitems aitems
         -- assertEqual "" (length eitems) (length aitems)
         -- mapM (\(e,a) -> assertEqual "" e a) $ zip eitems aitems
         assertEqual "total" etotal atotal
  in [

   "balanceReport with no args on null journal" ~: do
   (defreportopts, nulljournal) `gives` ([], Mixed [nullamt])

  ,"balanceReport with no args on sample journal" ~: do
   (defreportopts, samplejournal) `gives`
    ([
      (("assets","assets",0), mamountp' "$-1.00")
     ,(("assets:bank:saving","bank:saving",1), mamountp' "$1.00")
     ,(("assets:cash","cash",1), mamountp' "$-2.00")
     ,(("expenses","expenses",0), mamountp' "$2.00")
     ,(("expenses:food","food",1), mamountp' "$1.00")
     ,(("expenses:supplies","supplies",1), mamountp' "$1.00")
     ,(("income","income",0), mamountp' "$-2.00")
     ,(("income:gifts","gifts",1), mamountp' "$-1.00")
     ,(("income:salary","salary",1), mamountp' "$-1.00")
     ,(("liabilities:debts","liabilities:debts",0), mamountp' "$1.00")
     ],
     Mixed [nullamt])

  ,"balanceReport with --depth=N" ~: do
   (defreportopts{depth_=Just 1}, samplejournal) `gives`
    ([
      (("assets",      "assets",      0), mamountp' "$-1.00")
     ,(("expenses",    "expenses",    0), mamountp'  "$2.00")
     ,(("income",      "income",      0), mamountp' "$-2.00")
     ,(("liabilities", "liabilities", 0), mamountp'  "$1.00")
     ],
     Mixed [nullamt])

  ,"balanceReport with depth:N" ~: do
   (defreportopts{query_="depth:1"}, samplejournal) `gives`
    ([
      (("assets",      "assets",      0), mamountp' "$-1.00")
     ,(("expenses",    "expenses",    0), mamountp'  "$2.00")
     ,(("income",      "income",      0), mamountp' "$-2.00")
     ,(("liabilities", "liabilities", 0), mamountp'  "$1.00")
     ],
     Mixed [nullamt])

  ,"balanceReport with a date or secondary date span" ~: do
   (defreportopts{query_="date:'in 2009'"}, samplejournal2) `gives`
    ([],
     Mixed [nullamt])
   (defreportopts{query_="edate:'in 2009'"}, samplejournal2) `gives`
    ([
      (("assets:bank:checking","assets:bank:checking",0),mamountp' "$1.00")
     ,(("income:salary","income:salary",0),mamountp' "$-1.00")
     ],
     Mixed [nullamt])

  ,"balanceReport with desc:" ~: do
   (defreportopts{query_="desc:income"}, samplejournal) `gives`
    ([
      (("assets:bank:checking","assets:bank:checking",0),mamountp' "$1.00")
     ,(("income:salary","income:salary",0), mamountp' "$-1.00")
     ],
     Mixed [nullamt])

  ,"balanceReport with not:desc:" ~: do
   (defreportopts{query_="not:desc:income"}, samplejournal) `gives`
    ([
      (("assets","assets",0), mamountp' "$-2.00")
     ,(("assets:bank","bank",1), Mixed [nullamt])
     ,(("assets:bank:checking","checking",2),mamountp' "$-1.00")
     ,(("assets:bank:saving","saving",2), mamountp' "$1.00")
     ,(("assets:cash","cash",1), mamountp' "$-2.00")
     ,(("expenses","expenses",0), mamountp' "$2.00")
     ,(("expenses:food","food",1), mamountp' "$1.00")
     ,(("expenses:supplies","supplies",1), mamountp' "$1.00")
     ,(("income:gifts","income:gifts",0), mamountp' "$-1.00")
     ,(("liabilities:debts","liabilities:debts",0), mamountp' "$1.00")
     ],
     Mixed [nullamt])


{-
    ,"accounts report with account pattern o" ~:
     defreportopts{patterns_=["o"]} `gives`
     ["                  $1  expenses:food"
     ,"                 $-2  income"
     ,"                 $-1    gifts"
     ,"                 $-1    salary"
     ,"--------------------"
     ,"                 $-1"
     ]

    ,"accounts report with account pattern o and --depth 1" ~:
     defreportopts{patterns_=["o"],depth_=Just 1} `gives`
     ["                  $1  expenses"
     ,"                 $-2  income"
     ,"--------------------"
     ,"                 $-1"
     ]

    ,"accounts report with account pattern a" ~:
     defreportopts{patterns_=["a"]} `gives`
     ["                 $-1  assets"
     ,"                  $1    bank:saving"
     ,"                 $-2    cash"
     ,"                 $-1  income:salary"
     ,"                  $1  liabilities:debts"
     ,"--------------------"
     ,"                 $-1"
     ]

    ,"accounts report with account pattern e" ~:
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

    ,"accounts report with unmatched parent of two matched subaccounts" ~: 
     defreportopts{patterns_=["cash","saving"]} `gives`
     ["                 $-1  assets"
     ,"                  $1    bank:saving"
     ,"                 $-2    cash"
     ,"--------------------"
     ,"                 $-1"
     ]

    ,"accounts report with multi-part account name" ~: 
     defreportopts{patterns_=["expenses:food"]} `gives`
     ["                  $1  expenses:food"
     ,"--------------------"
     ,"                  $1"
     ]

    ,"accounts report with negative account pattern" ~:
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

    ,"accounts report negative account pattern always matches full name" ~: 
     defreportopts{patterns_=["not:e"]} `gives`
     ["--------------------"
     ,"                   0"
     ]

    ,"accounts report negative patterns affect totals" ~: 
     defreportopts{patterns_=["expenses","not:food"]} `gives`
     ["                  $1  expenses:supplies"
     ,"--------------------"
     ,"                  $1"
     ]

    ,"accounts report with -E shows zero-balance accounts" ~:
     defreportopts{patterns_=["assets"],empty_=True} `gives`
     ["                 $-1  assets"
     ,"                  $1    bank"
     ,"                   0      checking"
     ,"                  $1      saving"
     ,"                 $-2    cash"
     ,"--------------------"
     ,"                 $-1"
     ]

    ,"accounts report with cost basis" ~: do
       j <- (readJournal Nothing Nothing Nothing $ unlines
              [""
              ,"2008/1/1 test           "
              ,"  a:b          10h @ $50"
              ,"  c:d                   "
              ]) >>= either error' return
       let j' = journalCanonicaliseAmounts $ journalConvertAmountsToCost j -- enable cost basis adjustment
       balanceReportAsText defreportopts (balanceReport defreportopts Any j') `is`
         ["                $500  a:b"
         ,"               $-500  c:d"
         ,"--------------------"
         ,"                   0"
         ]
-}
 ]

Right samplejournal2 = journalBalanceTransactions $ 
         nulljournal
         {jtxns = [
           txnTieKnot $ Transaction {
             tdate=parsedate "2008/01/01",
             tdate2=Just $ parsedate "2009/01/01",
             tstatus=False,
             tcode="",
             tdescription="income",
             tcomment="",
             ttags=[],
             tpostings=
                 [posting {paccount="assets:bank:checking", pamount=Mixed [usd 1]}
                 ,posting {paccount="income:salary", pamount=missingmixedamt}
                 ],
             tpreceding_comment_lines=""
           }
          ]
         }
         
-- tests_isInterestingIndented = [
--   "isInterestingIndented" ~: do 
--    let (opts, journal, acctname) `gives` r = isInterestingIndented opts l acctname `is` r
--           where l = ledgerFromJournal (queryFromOpts nulldate opts) journal
     
--    (defreportopts, samplejournal, "expenses") `gives` True
--  ]

tests_Hledger_Reports_BalanceReport :: Test
tests_Hledger_Reports_BalanceReport = TestList $
    tests_balanceReport
