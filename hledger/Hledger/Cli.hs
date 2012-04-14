{-| 
Hledger.Cli re-exports the options, utilities and commands provided by the
hledger command-line program.
-}

module Hledger.Cli (
                     module Hledger.Cli.Add,
                     module Hledger.Cli.Balance,
                     module Hledger.Cli.Balancesheet,
                     module Hledger.Cli.Histogram,
                     module Hledger.Cli.Incomestatement,
                     module Hledger.Cli.Print,
                     module Hledger.Cli.Register,
                     module Hledger.Cli.Stats,
                     module Hledger.Cli.Options,
                     module Hledger.Cli.Utils,
                     module Hledger.Cli.Version,
                     tests_Hledger_Cli
              )
where
import Control.Monad
import qualified Data.Map as Map
import Data.Time.Calendar
import System.Time (ClockTime(TOD))
import Test.HUnit

import Hledger
import Hledger.Cli.Add
import Hledger.Cli.Balance
import Hledger.Cli.Balancesheet
import Hledger.Cli.Histogram
import Hledger.Cli.Incomestatement
import Hledger.Cli.Print
import Hledger.Cli.Register
import Hledger.Cli.Stats
import Hledger.Cli.Options
import Hledger.Cli.Utils
import Hledger.Cli.Version

-- | hledger and hledger-lib's unit tests aggregated from all modules
-- plus some more which are easier to define here for now.
tests_Hledger_Cli :: Test
tests_Hledger_Cli = TestList
 [
    tests_Hledger_Data
   ,tests_Hledger_Read
   -- ,tests_Hledger_Cli_Add
   -- ,tests_Hledger_Cli_Balance
   ,tests_Hledger_Cli_Balancesheet
   -- ,tests_Hledger_Cli_Histogram
   ,tests_Hledger_Cli_Incomestatement
   ,tests_Hledger_Cli_Options
   -- ,tests_Hledger_Cli_Print
   ,tests_Hledger_Cli_Register
   -- ,tests_Hledger_Cli_Stats


   ,"account directive" ~:
   let sameParse str1 str2 = do j1 <- readJournal Nothing Nothing Nothing str1 >>= either error' return
                                j2 <- readJournal Nothing Nothing Nothing str2 >>= either error' return
                                j1 `is` j2{filereadtime=filereadtime j1, files=files j1, jContext=jContext j1}
   in TestList
   [
    "account directive 1" ~: sameParse 
                          "2008/12/07 One\n  test:from  $-1\n  test:to  $1\n"
                          "!account test\n2008/12/07 One\n  from  $-1\n  to  $1\n"

   ,"account directive 2" ~: sameParse 
                           "2008/12/07 One\n  test:foo:from  $-1\n  test:foo:to  $1\n"
                           "!account test\n!account foo\n2008/12/07 One\n  from  $-1\n  to  $1\n"

   ,"account directive 3" ~: sameParse 
                           "2008/12/07 One\n  test:from  $-1\n  test:to  $1\n"
                           "!account test\n!account foo\n!end\n2008/12/07 One\n  from  $-1\n  to  $1\n"

   ,"account directive 4" ~: sameParse 
                           ("2008/12/07 One\n  alpha  $-1\n  beta  $1\n" ++
                            "!account outer\n2008/12/07 Two\n  aigh  $-2\n  bee  $2\n" ++
                            "!account inner\n2008/12/07 Three\n  gamma  $-3\n  delta  $3\n" ++
                            "!end\n2008/12/07 Four\n  why  $-4\n  zed  $4\n" ++
                            "!end\n2008/12/07 Five\n  foo  $-5\n  bar  $5\n"
                           )
                           ("2008/12/07 One\n  alpha  $-1\n  beta  $1\n" ++
                            "2008/12/07 Two\n  outer:aigh  $-2\n  outer:bee  $2\n" ++
                            "2008/12/07 Three\n  outer:inner:gamma  $-3\n  outer:inner:delta  $3\n" ++
                            "2008/12/07 Four\n  outer:why  $-4\n  outer:zed  $4\n" ++
                            "2008/12/07 Five\n  foo  $-5\n  bar  $5\n"
                           )

   ,"account directive should preserve \"virtual\" posting type" ~: do
      j <- readJournal Nothing Nothing Nothing "!account test\n2008/12/07 One\n  (from)  $-1\n  (to)  $1\n" >>= either error' return
      let p = head $ tpostings $ head $ jtxns j
      assertBool "" $ (paccount p) == "test:from"
      assertBool "" $ (ptype p) == VirtualPosting

   ]

   ,"account aliases" ~: do
      Right j <- readJournal Nothing Nothing Nothing "!alias expenses = equity:draw:personal\n1/1\n (expenses:food)  1\n"
      let p = head $ tpostings $ head $ jtxns j
      assertBool "" $ paccount p == "equity:draw:personal:food"

  ,"ledgerAccountNames" ~:
    ledgerAccountNames ledger7 `is`
     ["assets","assets:cash","assets:checking","assets:saving","equity","equity:opening balances",
      "expenses","expenses:food","expenses:food:dining","expenses:phone","expenses:vacation",
      "liabilities","liabilities:credit cards","liabilities:credit cards:discover"]

  ,"balance report tests" ~:
   let opts `gives` es = do
        j <- samplejournal
        d <- getCurrentDay
        accountsReportAsText opts (accountsReport opts (optsToFilterSpec opts d) j) `is` es
   in TestList
   [
    "balance report with no args" ~:
    defreportopts `gives`
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

   ,"balance report can be limited with --depth" ~:
    defreportopts{depth_=Just 1} `gives`
    ["                 $-1  assets"
    ,"                  $2  expenses"
    ,"                 $-2  income"
    ,"                  $1  liabilities"
    ,"--------------------"
    ,"                   0"
    ]
    
   ,"balance report with account pattern o" ~:
    defreportopts{patterns_=["o"]} `gives`
    ["                  $1  expenses:food"
    ,"                 $-2  income"
    ,"                 $-1    gifts"
    ,"                 $-1    salary"
    ,"--------------------"
    ,"                 $-1"
    ]

   ,"balance report with account pattern o and --depth 1" ~:
    defreportopts{patterns_=["o"],depth_=Just 1} `gives`
    ["                  $1  expenses"
    ,"                 $-2  income"
    ,"--------------------"
    ,"                 $-1"
    ]

   ,"balance report with account pattern a" ~:
    defreportopts{patterns_=["a"]} `gives`
    ["                 $-1  assets"
    ,"                  $1    bank:saving"
    ,"                 $-2    cash"
    ,"                 $-1  income:salary"
    ,"                  $1  liabilities:debts"
    ,"--------------------"
    ,"                 $-1"
    ]

   ,"balance report with account pattern e" ~:
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

   ,"balance report with unmatched parent of two matched subaccounts" ~: 
    defreportopts{patterns_=["cash","saving"]} `gives`
    ["                 $-1  assets"
    ,"                  $1    bank:saving"
    ,"                 $-2    cash"
    ,"--------------------"
    ,"                 $-1"
    ]

   ,"balance report with multi-part account name" ~: 
    defreportopts{patterns_=["expenses:food"]} `gives`
    ["                  $1  expenses:food"
    ,"--------------------"
    ,"                  $1"
    ]

   ,"balance report with negative account pattern" ~:
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

   ,"balance report negative account pattern always matches full name" ~: 
    defreportopts{patterns_=["not:e"]} `gives`
    ["--------------------"
    ,"                   0"
    ]

   ,"balance report negative patterns affect totals" ~: 
    defreportopts{patterns_=["expenses","not:food"]} `gives`
    ["                  $1  expenses:supplies"
    ,"--------------------"
    ,"                  $1"
    ]

   ,"balance report with -E shows zero-balance accounts" ~:
    defreportopts{patterns_=["assets"],empty_=True} `gives`
    ["                 $-1  assets"
    ,"                  $1    bank"
    ,"                   0      checking"
    ,"                  $1      saving"
    ,"                 $-2    cash"
    ,"--------------------"
    ,"                 $-1"
    ]

   ,"balance report with cost basis" ~: do
      j <- (readJournal Nothing Nothing Nothing $ unlines
             [""
             ,"2008/1/1 test           "
             ,"  a:b          10h @ $50"
             ,"  c:d                   "
             ]) >>= either error' return
      let j' = journalCanonicaliseAmounts $ journalConvertAmountsToCost j -- enable cost basis adjustment
      accountsReportAsText defreportopts (accountsReport defreportopts nullfilterspec j') `is`
        ["                $500  a:b"
        ,"               $-500  c:d"
        ,"--------------------"
        ,"                   0"
        ]

   ]

  ,"journalCanonicaliseAmounts" ~:
   "use the greatest precision" ~:
    (map precision $ journalAmountAndPriceCommodities $ journalCanonicaliseAmounts $ journalWithAmounts ["1","2.00"]) `is` [2,2]

  ,"commodities" ~:
    Map.elems (commodities ledger7) `is` [Commodity {symbol="$", side=L, spaced=False, decimalpoint='.', precision=2, separator=',', separatorpositions=[]}]

  -- don't know what this should do
  -- ,"elideAccountName" ~: do
  --    (elideAccountName 50 "aaaaaaaaaaaaaaaaaaaa:aaaaaaaaaaaaaaaaaaaa:aaaaaaaaaaaaaaaaaaaa"
  --     `is` "aa:aaaaaaaaaaaaaaaaaaaa:aaaaaaaaaaaaaaaaaaaa")
  --    (elideAccountName 20 "aaaaaaaaaaaaaaaaaaaa:aaaaaaaaaaaaaaaaaaaa:aaaaaaaaaaaaaaaaaaaa"
  --     `is` "aa:aa:aaaaaaaaaaaaaa")

  ,"default year" ~: do
    j <- readJournal Nothing Nothing Nothing defaultyear_journal_str >>= either error' return
    tdate (head $ jtxns j) `is` fromGregorian 2009 1 1
    return ()

  ,"print report tests" ~: TestList
  [

   "print expenses" ~:
   do 
    let opts = defreportopts{patterns_=["expenses"]}
    j <- samplejournal
    d <- getCurrentDay
    showTransactions opts (optsToFilterSpec opts d) j `is` unlines
     ["2008/06/03 * eat & shop"
     ,"    expenses:food                $1"
     ,"    expenses:supplies            $1"
     ,"    assets:cash                 $-2"
     ,""
     ]

  , "print report with depth arg" ~:
   do 
    let opts = defreportopts{depth_=Just 2}
    j <- samplejournal
    d <- getCurrentDay
    showTransactions opts (optsToFilterSpec opts d) j `is` unlines
      ["2008/01/01 income"
      ,"    income:salary           $-1"
      ,""
      ,"2008/06/01 gift"
      ,"    income:gifts           $-1"
      ,""
      ,"2008/06/03 * eat & shop"
      ,"    expenses:food                $1"
      ,"    expenses:supplies            $1"
      ,"    assets:cash                 $-2"
      ,""
      ,"2008/12/31 * pay off"
      ,"    liabilities:debts            $1"
      ,""
      ]

  ]

  ,"register report tests" ~:
  let registerdates = filter (not . null) .  map (strip . take 10) . lines
  in
  TestList
  [

   "register report with no args" ~:
   do 
    j <- samplejournal
    let opts = defreportopts
    (postingsReportAsText opts $ postingsReport opts (optsToFilterSpec opts date1) j) `is` unlines
     ["2008/01/01 income               assets:bank:checking             $1           $1"
     ,"                                income:salary                   $-1            0"
     ,"2008/06/01 gift                 assets:bank:checking             $1           $1"
     ,"                                income:gifts                    $-1            0"
     ,"2008/06/02 save                 assets:bank:saving               $1           $1"
     ,"                                assets:bank:checking            $-1            0"
     ,"2008/06/03 eat & shop           expenses:food                    $1           $1"
     ,"                                expenses:supplies                $1           $2"
     ,"                                assets:cash                     $-2            0"
     ,"2008/12/31 pay off              liabilities:debts                $1           $1"
     ,"                                assets:bank:checking            $-1            0"
     ]

  ,"register report with cleared option" ~:
   do 
    let opts = defreportopts{cleared_=True}
    j <- readJournal' sample_journal_str
    (postingsReportAsText opts $ postingsReport opts (optsToFilterSpec opts date1) j) `is` unlines
     ["2008/06/03 eat & shop           expenses:food                    $1           $1"
     ,"                                expenses:supplies                $1           $2"
     ,"                                assets:cash                     $-2            0"
     ,"2008/12/31 pay off              liabilities:debts                $1           $1"
     ,"                                assets:bank:checking            $-1            0"
     ]

  ,"register report with uncleared option" ~:
   do 
    let opts = defreportopts{uncleared_=True}
    j <- readJournal' sample_journal_str
    (postingsReportAsText opts $ postingsReport opts (optsToFilterSpec opts date1) j) `is` unlines
     ["2008/01/01 income               assets:bank:checking             $1           $1"
     ,"                                income:salary                   $-1            0"
     ,"2008/06/01 gift                 assets:bank:checking             $1           $1"
     ,"                                income:gifts                    $-1            0"
     ,"2008/06/02 save                 assets:bank:saving               $1           $1"
     ,"                                assets:bank:checking            $-1            0"
     ]

  ,"register report sorts by date" ~:
   do 
    j <- readJournal' $ unlines
        ["2008/02/02 a"
        ,"  b  1"
        ,"  c"
        ,""
        ,"2008/01/01 d"
        ,"  e  1"
        ,"  f"
        ]
    let opts = defreportopts
    registerdates (postingsReportAsText opts $ postingsReport opts (optsToFilterSpec opts date1) j) `is` ["2008/01/01","2008/02/02"]

  ,"register report with account pattern" ~:
   do
    j <- samplejournal
    let opts = defreportopts{patterns_=["cash"]}
    (postingsReportAsText opts $ postingsReport opts (optsToFilterSpec opts date1) j) `is` unlines
     ["2008/06/03 eat & shop           assets:cash                     $-2          $-2"
     ]

  ,"register report with account pattern, case insensitive" ~:
   do 
    j <- samplejournal
    let opts = defreportopts{patterns_=["cAsH"]}
    (postingsReportAsText opts $ postingsReport opts (optsToFilterSpec opts date1) j) `is` unlines
     ["2008/06/03 eat & shop           assets:cash                     $-2          $-2"
     ]

  ,"register report with display expression" ~:
   do 
    j <- samplejournal
    let gives displayexpr = 
            (registerdates (postingsReportAsText opts $ postingsReport opts (optsToFilterSpec opts date1) j) `is`)
                where opts = defreportopts{display_=Just displayexpr}
    "d<[2008/6/2]"  `gives` ["2008/01/01","2008/06/01"]
    "d<=[2008/6/2]" `gives` ["2008/01/01","2008/06/01","2008/06/02"]
    "d=[2008/6/2]"  `gives` ["2008/06/02"]
    "d>=[2008/6/2]" `gives` ["2008/06/02","2008/06/03","2008/12/31"]
    "d>[2008/6/2]"  `gives` ["2008/06/03","2008/12/31"]

  ,"register report with period expression" ~:
   do 
    j <- samplejournal
    let periodexpr `gives` dates = do
          j' <- samplejournal
          registerdates (postingsReportAsText opts $ postingsReport opts (optsToFilterSpec opts date1) j') `is` dates
              where opts = defreportopts{period_=maybePeriod date1 periodexpr}
    ""     `gives` ["2008/01/01","2008/06/01","2008/06/02","2008/06/03","2008/12/31"]
    "2008" `gives` ["2008/01/01","2008/06/01","2008/06/02","2008/06/03","2008/12/31"]
    "2007" `gives` []
    "june" `gives` ["2008/06/01","2008/06/02","2008/06/03"]
    "monthly" `gives` ["2008/01/01","2008/06/01","2008/12/01"]
    "quarterly" `gives` ["2008/01/01","2008/04/01","2008/10/01"]
    let opts = defreportopts{period_=maybePeriod date1 "yearly"}
    (postingsReportAsText opts $ postingsReport opts (optsToFilterSpec opts date1) j) `is` unlines
     ["2008/01/01 - 2008/12/31         assets:bank:saving               $1           $1"
     ,"                                assets:cash                     $-2          $-1"
     ,"                                expenses:food                    $1            0"
     ,"                                expenses:supplies                $1           $1"
     ,"                                income:gifts                    $-1            0"
     ,"                                income:salary                   $-1          $-1"
     ,"                                liabilities:debts                $1            0"
     ]
    let opts = defreportopts{period_=maybePeriod date1 "quarterly"}
    registerdates (postingsReportAsText opts $ postingsReport opts (optsToFilterSpec opts date1) j) `is` ["2008/01/01","2008/04/01","2008/10/01"]
    let opts = defreportopts{period_=maybePeriod date1 "quarterly",empty_=True}
    registerdates (postingsReportAsText opts $ postingsReport opts (optsToFilterSpec opts date1) j) `is` ["2008/01/01","2008/04/01","2008/07/01","2008/10/01"]

  ]

  , "register report with depth arg" ~:
   do 
    j <- samplejournal
    let opts = defreportopts{depth_=Just 2}
    (postingsReportAsText opts $ postingsReport opts (optsToFilterSpec opts date1) j) `is` unlines
     ["2008/01/01 income               assets:bank                      $1           $1"
     ,"                                income:salary                   $-1            0"
     ,"2008/06/01 gift                 assets:bank                      $1           $1"
     ,"                                income:gifts                    $-1            0"
     ,"2008/06/02 save                 assets:bank                      $1           $1"
     ,"                                assets:bank                     $-1            0"
     ,"2008/06/03 eat & shop           expenses:food                    $1           $1"
     ,"                                expenses:supplies                $1           $2"
     ,"                                assets:cash                     $-2            0"
     ,"2008/12/31 pay off              liabilities:debts                $1           $1"
     ,"                                assets:bank                     $-1            0"
     ]

  ,"show dollars" ~: show (dollars 1) ~?= "$1.00"

  ,"show hours" ~: show (hours 1) ~?= "1.0h"

  ,"unicode in balance layout" ~: do
    j <- readJournal'
      "2009/01/01 * медвежья шкура\n  расходы:покупки  100\n  актив:наличные\n"
    let opts = defreportopts
    accountsReportAsText opts (accountsReport opts (optsToFilterSpec opts date1) j) `is`
      ["                -100  актив:наличные"
      ,"                 100  расходы:покупки"
      ,"--------------------"
      ,"                   0"
      ]

  ,"unicode in register layout" ~: do
    j <- readJournal'
      "2009/01/01 * медвежья шкура\n  расходы:покупки  100\n  актив:наличные\n"
    let opts = defreportopts
    (postingsReportAsText opts $ postingsReport opts (optsToFilterSpec opts date1) j) `is` unlines
      ["2009/01/01 медвежья шкура       расходы:покупки                 100          100"
      ,"                                актив:наличные                 -100            0"]

  ,"subAccounts" ~: do
    l <- liftM (journalToLedger nullfilterspec) samplejournal
    let a = ledgerAccount l "assets"
    map aname (ledgerSubAccounts l a) `is` ["assets:bank","assets:cash"]

 ]

  
-- fixtures/test data

date1 = parsedate "2008/11/26"
-- t1 = LocalTime date1 midday

samplejournal = readJournal' sample_journal_str

sample_journal_str = unlines
 ["; A sample journal file."
 ,";"
 ,"; Sets up this account tree:"
 ,"; assets"
 ,";   bank"
 ,";     checking"
 ,";     saving"
 ,";   cash"
 ,"; expenses"
 ,";   food"
 ,";   supplies"
 ,"; income"
 ,";   gifts"
 ,";   salary"
 ,"; liabilities"
 ,";   debts"
 ,""
 ,"2008/01/01 income"
 ,"    assets:bank:checking  $1"
 ,"    income:salary"
 ,""
 ,"2008/06/01 gift"
 ,"    assets:bank:checking  $1"
 ,"    income:gifts"
 ,""
 ,"2008/06/02 save"
 ,"    assets:bank:saving  $1"
 ,"    assets:bank:checking"
 ,""
 ,"2008/06/03 * eat & shop"
 ,"    expenses:food      $1"
 ,"    expenses:supplies  $1"
 ,"    assets:cash"
 ,""
 ,"2008/12/31 * pay off"
 ,"    liabilities:debts  $1"
 ,"    assets:bank:checking"
 ,""
 ,""
 ,";final comment"
 ]

defaultyear_journal_str = unlines
 ["Y2009"
 ,""
 ,"01/01 A"
 ,"    a  $1"
 ,"    b"
 ]

-- write_sample_journal = writeFile "sample.journal" sample_journal_str

-- entry2_str = unlines
--  ["2007/01/27 * joes diner"
--  ,"    expenses:food:dining                      $10.00"
--  ,"    expenses:gifts                            $10.00"
--  ,"    assets:checking                          $-20.00"
--  ,""
--  ]

-- entry3_str = unlines
--  ["2007/01/01 * opening balance"
--  ,"    assets:cash                                $4.82"
--  ,"    equity:opening balances"
--  ,""
--  ,"2007/01/01 * opening balance"
--  ,"    assets:cash                                $4.82"
--  ,"    equity:opening balances"
--  ,""
--  ,"2007/01/28 coopportunity"
--  ,"  expenses:food:groceries                 $47.18"
--  ,"  assets:checking"
--  ,""
--  ]

-- periodic_entry1_str = unlines
--  ["~ monthly from 2007/2/2"
--  ,"  assets:saving            $200.00"
--  ,"  assets:checking"
--  ,""
--  ]

-- periodic_entry2_str = unlines
--  ["~ monthly from 2007/2/2"
--  ,"  assets:saving            $200.00         ;auto savings"
--  ,"  assets:checking"
--  ,""
--  ]

-- periodic_entry3_str = unlines
--  ["~ monthly from 2007/01/01"
--  ,"    assets:cash                                $4.82"
--  ,"    equity:opening balances"
--  ,""
--  ,"~ monthly from 2007/01/01"
--  ,"    assets:cash                                $4.82"
--  ,"    equity:opening balances"
--  ,""
--  ]

-- journal1_str = unlines
--  [""
--  ,"2007/01/27 * joes diner"
--  ,"  expenses:food:dining                    $10.00"
--  ,"  expenses:gifts                          $10.00"
--  ,"  assets:checking                        $-20.00"
--  ,""
--  ,""
--  ,"2007/01/28 coopportunity"
--  ,"  expenses:food:groceries                 $47.18"
--  ,"  assets:checking                        $-47.18"
--  ,""
--  ,""
--  ]

-- journal2_str = unlines
--  [";comment"
--  ,"2007/01/27 * joes diner"
--  ,"  expenses:food:dining                    $10.00"
--  ,"  assets:checking                        $-47.18"
--  ,""
--  ]

-- journal3_str = unlines
--  ["2007/01/27 * joes diner"
--  ,"  expenses:food:dining                    $10.00"
--  ,";intra-entry comment"
--  ,"  assets:checking                        $-47.18"
--  ,""
--  ]

-- journal4_str = unlines
--  ["!include \"somefile\""
--  ,"2007/01/27 * joes diner"
--  ,"  expenses:food:dining                    $10.00"
--  ,"  assets:checking                        $-47.18"
--  ,""
--  ]

-- journal5_str = ""

-- journal6_str = unlines
--  ["~ monthly from 2007/1/21"
--  ,"    expenses:entertainment  $16.23        ;netflix"
--  ,"    assets:checking"
--  ,""
--  ,"; 2007/01/01 * opening balance"
--  ,";     assets:saving                            $200.04"
--  ,";     equity:opening balances                         "
--  ,""
--  ]

-- journal7_str = unlines
--  ["2007/01/01 * opening balance"
--  ,"    assets:cash                                $4.82"
--  ,"    equity:opening balances                         "
--  ,""
--  ,"2007/01/01 * opening balance"
--  ,"    income:interest                                $-4.82"
--  ,"    equity:opening balances                         "
--  ,""
--  ,"2007/01/02 * ayres suites"
--  ,"    expenses:vacation                        $179.92"
--  ,"    assets:checking                                 "
--  ,""
--  ,"2007/01/02 * auto transfer to savings"
--  ,"    assets:saving                            $200.00"
--  ,"    assets:checking                                 "
--  ,""
--  ,"2007/01/03 * poquito mas"
--  ,"    expenses:food:dining                       $4.82"
--  ,"    assets:cash                                     "
--  ,""
--  ,"2007/01/03 * verizon"
--  ,"    expenses:phone                            $95.11"
--  ,"    assets:checking                                 "
--  ,""
--  ,"2007/01/03 * discover"
--  ,"    liabilities:credit cards:discover         $80.00"
--  ,"    assets:checking                                 "
--  ,""
--  ,"2007/01/04 * blue cross"
--  ,"    expenses:health:insurance                 $90.00"
--  ,"    assets:checking                                 "
--  ,""
--  ,"2007/01/05 * village market liquor"
--  ,"    expenses:food:dining                       $6.48"
--  ,"    assets:checking                                 "
--  ,""
--  ]

journal7 = Journal
          [] 
          [] 
          [
           txnTieKnot $ Transaction {
             tdate=parsedate "2007/01/01",
             teffectivedate=Nothing,
             tstatus=False,
             tcode="*",
             tdescription="opening balance",
             tcomment="",
             tmetadata=[],
             tpostings=[
              Posting {
                pstatus=False,
                paccount="assets:cash",
                pamount=(Mixed [dollars 4.82]),
                pcomment="",
                ptype=RegularPosting,
                pmetadata=[],
                ptransaction=Nothing
              },
              Posting {
                pstatus=False,
                paccount="equity:opening balances",
                pamount=(Mixed [dollars (-4.82)]),
                pcomment="",
                ptype=RegularPosting,
                pmetadata=[],
                ptransaction=Nothing
              }
             ],
             tpreceding_comment_lines=""
           }
          ,
           txnTieKnot $ Transaction {
             tdate=parsedate "2007/02/01",
             teffectivedate=Nothing,
             tstatus=False,
             tcode="*",
             tdescription="ayres suites",
             tcomment="",
             tmetadata=[],
             tpostings=[
              Posting {
                pstatus=False,
                paccount="expenses:vacation",
                pamount=(Mixed [dollars 179.92]),
                pcomment="",
                ptype=RegularPosting,
                pmetadata=[],
                ptransaction=Nothing
              },
              Posting {
                pstatus=False,
                paccount="assets:checking",
                pamount=(Mixed [dollars (-179.92)]),
                pcomment="",
                ptype=RegularPosting,
                pmetadata=[],
                ptransaction=Nothing
              }
             ],
             tpreceding_comment_lines=""
           }
          ,
           txnTieKnot $ Transaction {
             tdate=parsedate "2007/01/02",
             teffectivedate=Nothing,
             tstatus=False,
             tcode="*",
             tdescription="auto transfer to savings",
             tcomment="",
             tmetadata=[],
             tpostings=[
              Posting {
                pstatus=False,
                paccount="assets:saving",
                pamount=(Mixed [dollars 200]),
                pcomment="",
                ptype=RegularPosting,
                pmetadata=[],
                ptransaction=Nothing
              },
              Posting {
                pstatus=False,
                paccount="assets:checking",
                pamount=(Mixed [dollars (-200)]),
                pcomment="",
                ptype=RegularPosting,
                pmetadata=[],
                ptransaction=Nothing
              }
             ],
             tpreceding_comment_lines=""
           }
          ,
           txnTieKnot $ Transaction {
             tdate=parsedate "2007/01/03",
             teffectivedate=Nothing,
             tstatus=False,
             tcode="*",
             tdescription="poquito mas",
             tcomment="",
             tmetadata=[],
             tpostings=[
              Posting {
                pstatus=False,
                paccount="expenses:food:dining",
                pamount=(Mixed [dollars 4.82]),
                pcomment="",
                ptype=RegularPosting,
                pmetadata=[],
                ptransaction=Nothing
              },
              Posting {
                pstatus=False,
                paccount="assets:cash",
                pamount=(Mixed [dollars (-4.82)]),
                pcomment="",
                ptype=RegularPosting,
                pmetadata=[],
                ptransaction=Nothing
              }
             ],
             tpreceding_comment_lines=""
           }
          ,
           txnTieKnot $ Transaction {
             tdate=parsedate "2007/01/03",
             teffectivedate=Nothing,
             tstatus=False,
             tcode="*",
             tdescription="verizon",
             tcomment="",
             tmetadata=[],
             tpostings=[
              Posting {
                pstatus=False,
                paccount="expenses:phone",
                pamount=(Mixed [dollars 95.11]),
                pcomment="",
                ptype=RegularPosting,
                pmetadata=[],
                ptransaction=Nothing
              },
              Posting {
                pstatus=False,
                paccount="assets:checking",
                pamount=(Mixed [dollars (-95.11)]),
                pcomment="",
                ptype=RegularPosting,
                pmetadata=[],
                ptransaction=Nothing
              }
             ],
             tpreceding_comment_lines=""
           }
          ,
           txnTieKnot $ Transaction {
             tdate=parsedate "2007/01/03",
             teffectivedate=Nothing,
             tstatus=False,
             tcode="*",
             tdescription="discover",
             tcomment="",
             tmetadata=[],
             tpostings=[
              Posting {
                pstatus=False,
                paccount="liabilities:credit cards:discover",
                pamount=(Mixed [dollars 80]),
                pcomment="",
                ptype=RegularPosting,
                pmetadata=[],
                ptransaction=Nothing
              },
              Posting {
                pstatus=False,
                paccount="assets:checking",
                pamount=(Mixed [dollars (-80)]),
                pcomment="",
                ptype=RegularPosting,
                pmetadata=[],
                ptransaction=Nothing
              }
             ],
             tpreceding_comment_lines=""
           }
          ]
          []
          []
          ""
          nullctx
          []
          (TOD 0 0)

ledger7 = journalToLedger nullfilterspec journal7

-- journal8_str = unlines
--  ["2008/1/1 test           "
--  ,"  a:b          10h @ $40"
--  ,"  c:d                   "
--  ,""
--  ]

-- timelogentry1_str  = "i 2007/03/11 16:19:00 hledger\n"
-- timelogentry1 = TimeLogEntry In (parsedatetime "2007/03/11 16:19:00") "hledger"

-- timelogentry2_str  = "o 2007/03/11 16:30:00\n"
-- timelogentry2 = TimeLogEntry Out (parsedatetime "2007/03/11 16:30:00") ""

-- a1 = Mixed [(hours 1){price=Just $ Mixed [Amount (comm "$") 10 Nothing]}]
-- a2 = Mixed [(hours 2){price=Just $ Mixed [Amount (comm "EUR") 10 Nothing]}]
-- a3 = Mixed $ amounts a1 ++ amounts a2

journalWithAmounts :: [String] -> Journal
journalWithAmounts as =
        Journal
        []
        []
        [t | a <- as, let t = nulltransaction{tdescription=a,tpostings=[nullposting{pamount=parse a,ptransaction=Just t}]}]
        []
        []
        ""
        nullctx
        []
        (TOD 0 0)
    where parse = fromparse . parseWithCtx nullctx someamount
