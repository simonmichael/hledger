{- |

This module contains hledger's unit tests. These are built in to hledger,
and can be run at any time by doing @hledger test@ (or, with a few more
options, by doing @make unittest@ in the hledger source tree.)

Other kinds of tests:

hledger's functional tests are a set of shell/command-line tests defined
by .test files in the tests\/ subdirectory. These can be run by doing
@make functest@ in the hledger source tree.

hledger's doctests are shell tests defined in literal blocks in haddock
documentation in the source, run by doing @make doctest@ in the hledger
source tree. They hardly used, but here is an example:

@
$ hledger -f data/sample.journal balance o
                  $1  expenses:food
                 $-2  income
                 $-1    gifts
                 $-1    salary
--------------------
                 $-1
@

-}

module Hledger.Cli.Tests
where
import qualified Data.Map as Map
import System.Exit (exitFailure, exitWith, ExitCode(ExitSuccess)) -- base 3 compatible
import System.Time (ClockTime(TOD))

import Hledger.Cli.Commands.All
import Hledger.Data  -- including testing utils in Hledger.Data.Utils
import Hledger.Read.Common (emptyCtx)
import Hledger.Read (readJournal)
import Hledger.Read.Journal (someamount)
import Hledger.Cli.Options
import Hledger.Cli.Utils


-- | Run unit tests.
runtests :: [Opt] -> [String] -> IO ()
runtests _ args = do
  (counts,_) <- liftM (flip (,) 0) $ runTestTT ts
  if errors counts > 0 || (failures counts > 0)
   then exitFailure
   else exitWith ExitSuccess
    where
      ts = TestList $ filter matchname $ tflatten tests  -- show flat test names
      -- ts = tfilter matchname $ TestList tests -- show hierarchical test names
      matchname = matchpats args . tname

-- | unit tests, augmenting the ones defined in each module. Where that is
-- inconvenient due to import cycles or whatever, we define them here.
tests :: Test
tests = TestList [
   tests_Hledger_Data,
   tests_Hledger_Commands,

   "account directive" ~:
   let sameParse str1 str2 = do j1 <- readJournal Nothing str1 >>= either error return
                                j2 <- readJournal Nothing str2 >>= either error return
                                j1 `is` j2{filereadtime=filereadtime j1, jtext=jtext j1}
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
   ]

  ,"accountnames" ~:
    accountnames ledger7 `is`
     ["assets","assets:cash","assets:checking","assets:saving","equity","equity:opening balances",
      "expenses","expenses:food","expenses:food:dining","expenses:phone","expenses:vacation",
      "liabilities","liabilities:credit cards","liabilities:credit cards:discover"]

  ,"accountNameTreeFrom" ~: do
    accountNameTreeFrom ["a"]       `is` Node "top" [Node "a" []]
    accountNameTreeFrom ["a","b"]   `is` Node "top" [Node "a" [], Node "b" []]
    accountNameTreeFrom ["a","a:b"] `is` Node "top" [Node "a" [Node "a:b" []]]
    accountNameTreeFrom ["a:b:c"]   `is` Node "top" [Node "a" [Node "a:b" [Node "a:b:c" []]]]

  ,"balance report tests" ~:
   let (opts,args) `gives` es = do 
        l <- samplejournalwithopts opts args
        t <- getCurrentLocalTime
        balanceReportAsText opts (balanceReport opts (optsToFilterSpec opts args t) l) `is` unlines es
   in TestList
   [

    "balance report with no args" ~:
    ([], []) `gives`
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
    ,"                  $0"
    ]

   ,"balance report can be limited with --depth" ~:
    ([Depth "1"], []) `gives`
    ["                 $-1  assets"
    ,"                  $2  expenses"
    ,"                 $-2  income"
    ,"                  $1  liabilities"
    ,"--------------------"
    ,"                  $0"
    ]
    
   ,"balance report with account pattern o" ~:
    ([SubTotal], ["o"]) `gives`
    ["                  $1  expenses:food"
    ,"                 $-2  income"
    ,"                 $-1    gifts"
    ,"                 $-1    salary"
    ,"--------------------"
    ,"                 $-1"
    ]

   ,"balance report with account pattern o and --depth 1" ~:
    ([Depth "1"], ["o"]) `gives`
    ["                  $1  expenses"
    ,"                 $-2  income"
    ,"--------------------"
    ,"                 $-1"
    ]

   ,"balance report with account pattern a" ~:
    ([], ["a"]) `gives`
    ["                 $-1  assets"
    ,"                  $1    bank:saving"
    ,"                 $-2    cash"
    ,"                 $-1  income:salary"
    ,"                  $1  liabilities:debts"
    ,"--------------------"
    ,"                 $-1"
    ]

   ,"balance report with account pattern e" ~:
    ([], ["e"]) `gives`
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
    ,"                  $0"
    ]

   ,"balance report with unmatched parent of two matched subaccounts" ~: 
    ([], ["cash","saving"]) `gives`
    ["                 $-1  assets"
    ,"                  $1    bank:saving"
    ,"                 $-2    cash"
    ,"--------------------"
    ,"                 $-1"
    ]

   ,"balance report with multi-part account name" ~: 
    ([], ["expenses:food"]) `gives`
    ["                  $1  expenses:food"
    ,"--------------------"
    ,"                  $1"
    ]

   ,"balance report with negative account pattern" ~:
    ([], ["not:assets"]) `gives`
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
    ([], ["not:e"]) `gives`
    ["--------------------"
    ,"                   0"
    ]

   ,"balance report negative patterns affect totals" ~: 
    ([], ["expenses","not:food"]) `gives`
    ["                  $1  expenses:supplies"
    ,"--------------------"
    ,"                  $1"
    ]

   ,"balance report with -E shows zero-balance accounts" ~:
    ([SubTotal,Empty], ["assets"]) `gives`
    ["                 $-1  assets"
    ,"                  $1    bank"
    ,"                  $0      checking"
    ,"                  $1      saving"
    ,"                 $-2    cash"
    ,"--------------------"
    ,"                 $-1"
    ]

   ,"balance report with cost basis" ~: do
      j <- (readJournal Nothing $ unlines
             [""
             ,"2008/1/1 test           "
             ,"  a:b          10h @ $50"
             ,"  c:d                   "
             ]) >>= either error return
      let j' = journalCanonicaliseAmounts $ journalConvertAmountsToCost j -- enable cost basis adjustment
      balanceReportAsText [] (balanceReport [] nullfilterspec j') `is`
       unlines
        ["                $500  a:b"
        ,"               $-500  c:d"
        ,"--------------------"
        ,"                  $0"
        ]

   ,"balance report elides zero-balance root account(s)" ~: do
      l <- readJournalWithOpts []
             (unlines
              ["2008/1/1 one"
              ,"  test:a  1"
              ,"  test:b"
              ])
      balanceReportAsText [] (balanceReport [] nullfilterspec l) `is`
       unlines
        ["                   1  test:a"
        ,"                  -1  test:b"
        ,"--------------------"
        ,"                   0"
        ]

   ]

  ,"balanceTransaction" ~: do
     assertBool "detect unbalanced entry, sign error"
                    (isLeft $ balanceTransaction
                           (Transaction (parsedate "2007/01/28") Nothing False "" "test" ""
                            [Posting False "a" (Mixed [dollars 1]) "" RegularPosting Nothing, 
                             Posting False "b" (Mixed [dollars 1]) "" RegularPosting Nothing
                            ] ""))
     assertBool "detect unbalanced entry, multiple missing amounts"
                    (isLeft $ balanceTransaction
                           (Transaction (parsedate "2007/01/28") Nothing False "" "test" ""
                            [Posting False "a" missingamt "" RegularPosting Nothing, 
                             Posting False "b" missingamt "" RegularPosting Nothing
                            ] ""))
     let e = balanceTransaction (Transaction (parsedate "2007/01/28") Nothing False "" "test" ""
                           [Posting False "a" (Mixed [dollars 1]) "" RegularPosting Nothing, 
                            Posting False "b" missingamt "" RegularPosting Nothing
                           ] "")
     assertBool "one missing amount should be ok" (isRight e)
     assertEqual "balancing amount is added" 
                     (Mixed [dollars (-1)])
                     (case e of
                        Right e' -> (pamount $ last $ tpostings e')
                        Left _ -> error "should not happen")

  ,"journalCanonicaliseAmounts" ~:
   "use the greatest precision" ~:
    (map precision $ journalAmountAndPriceCommodities $ journalCanonicaliseAmounts $ journalWithAmounts ["1","2.00"]) `is` [2,2]

  ,"commodities" ~:
    Map.elems (commodities ledger7) `is` [Commodity {symbol="$", side=L, spaced=False, comma=False, precision=2}]

  ,"dateSpanFromOpts" ~: do
    let todaysdate = parsedate "2008/11/26"
    let gives = is . show . dateSpanFromOpts todaysdate
    [] `gives` "DateSpan Nothing Nothing"
    [Begin "2008", End "2009"] `gives` "DateSpan (Just 2008-01-01) (Just 2009-01-01)"
    [Period "in 2008"] `gives` "DateSpan (Just 2008-01-01) (Just 2009-01-01)"
    [Begin "2005", End "2007",Period "in 2008"] `gives` "DateSpan (Just 2008-01-01) (Just 2009-01-01)"

  -- don't know what this should do
  -- ,"elideAccountName" ~: do
  --    (elideAccountName 50 "aaaaaaaaaaaaaaaaaaaa:aaaaaaaaaaaaaaaaaaaa:aaaaaaaaaaaaaaaaaaaa"
  --     `is` "aa:aaaaaaaaaaaaaaaaaaaa:aaaaaaaaaaaaaaaaaaaa")
  --    (elideAccountName 20 "aaaaaaaaaaaaaaaaaaaa:aaaaaaaaaaaaaaaaaaaa:aaaaaaaaaaaaaaaaaaaa"
  --     `is` "aa:aa:aaaaaaaaaaaaaa")

  ,"expandAccountNames" ~:
    expandAccountNames ["assets:cash","assets:checking","expenses:vacation"] `is`
     ["assets","assets:cash","assets:checking","expenses","expenses:vacation"]

  ,"intervalFromOpts" ~: do
    let gives = is . intervalFromOpts
    [] `gives` NoInterval
    [DailyOpt] `gives` Daily
    [WeeklyOpt] `gives` Weekly
    [MonthlyOpt] `gives` Monthly
    [QuarterlyOpt] `gives` Quarterly
    [YearlyOpt] `gives` Yearly
    [Period "weekly"] `gives` Weekly
    [Period "monthly"] `gives` Monthly
    [Period "quarterly"] `gives` Quarterly
    [WeeklyOpt, Period "yearly"] `gives` Yearly

  ,"isAccountNamePrefixOf" ~: do
    "assets" `isAccountNamePrefixOf` "assets" `is` False
    "assets" `isAccountNamePrefixOf` "assets:bank" `is` True
    "assets" `isAccountNamePrefixOf` "assets:bank:checking" `is` True
    "my assets" `isAccountNamePrefixOf` "assets:bank" `is` False

  ,"isTransactionBalanced" ~: do
     let t = Transaction (parsedate "2009/01/01") Nothing False "" "a" ""
             [Posting False "b" (Mixed [dollars 1.00]) "" RegularPosting (Just t)
             ,Posting False "c" (Mixed [dollars (-1.00)]) "" RegularPosting (Just t)
             ] ""
     assertBool "detect balanced" (isTransactionBalanced t)
     let t = Transaction (parsedate "2009/01/01") Nothing False "" "a" ""
             [Posting False "b" (Mixed [dollars 1.00]) "" RegularPosting (Just t)
             ,Posting False "c" (Mixed [dollars (-1.01)]) "" RegularPosting (Just t)
             ] ""
     assertBool "detect unbalanced" (not $ isTransactionBalanced t)
     let t = Transaction (parsedate "2009/01/01") Nothing False "" "a" ""
             [Posting False "b" (Mixed [dollars 1.00]) "" RegularPosting (Just t)
             ] ""
     assertBool "detect unbalanced, one posting" (not $ isTransactionBalanced t)
     let t = Transaction (parsedate "2009/01/01") Nothing False "" "a" ""
             [Posting False "b" (Mixed [dollars 0]) "" RegularPosting (Just t)
             ] ""
     assertBool "one zero posting is considered balanced for now" (isTransactionBalanced t)
     let t = Transaction (parsedate "2009/01/01") Nothing False "" "a" ""
             [Posting False "b" (Mixed [dollars 1.00]) "" RegularPosting (Just t)
             ,Posting False "c" (Mixed [dollars (-1.00)]) "" RegularPosting (Just t)
             ,Posting False "d" (Mixed [dollars 100]) "" VirtualPosting (Just t)
             ] ""
     assertBool "virtual postings don't need to balance" (isTransactionBalanced t)
     let t = Transaction (parsedate "2009/01/01") Nothing False "" "a" ""
             [Posting False "b" (Mixed [dollars 1.00]) "" RegularPosting (Just t)
             ,Posting False "c" (Mixed [dollars (-1.00)]) "" RegularPosting (Just t)
             ,Posting False "d" (Mixed [dollars 100]) "" BalancedVirtualPosting (Just t)
             ] ""
     assertBool "balanced virtual postings need to balance among themselves" (not $ isTransactionBalanced t)
     let t = Transaction (parsedate "2009/01/01") Nothing False "" "a" ""
             [Posting False "b" (Mixed [dollars 1.00]) "" RegularPosting (Just t)
             ,Posting False "c" (Mixed [dollars (-1.00)]) "" RegularPosting (Just t)
             ,Posting False "d" (Mixed [dollars 100]) "" BalancedVirtualPosting (Just t)
             ,Posting False "e" (Mixed [dollars (-100)]) "" BalancedVirtualPosting (Just t)
             ] ""
     assertBool "balanced virtual postings need to balance among themselves (2)" (isTransactionBalanced t)

  ,"isSubAccountNameOf" ~: do
    "assets" `isSubAccountNameOf` "assets" `is` False
    "assets:bank" `isSubAccountNameOf` "assets" `is` True
    "assets:bank:checking" `isSubAccountNameOf` "assets" `is` False
    "assets:bank" `isSubAccountNameOf` "my assets" `is` False

  ,"default year" ~: do
    rl <- readJournal Nothing defaultyear_journal_str >>= either error return
    tdate (head $ jtxns rl) `is` fromGregorian 2009 1 1
    return ()

  ,"normaliseMixedAmount" ~: do
     normaliseMixedAmount (Mixed []) ~?= Mixed [nullamt]

  ,"parsedate" ~: do
    parsedate "2008/02/03" `is` parsetimewith "%Y/%m/%d" "2008/02/03" date1
    parsedate "2008-02-03" `is` parsetimewith "%Y/%m/%d" "2008/02/03" date1

  ,"period expressions" ~: do
    let todaysdate = parsedate "2008/11/26"
    let str `gives` result = show (parsewith (periodexpr todaysdate) str) `is` ("Right " ++ result)
    "from aug to oct"           `gives` "(NoInterval,DateSpan (Just 2008-08-01) (Just 2008-10-01))"
    "aug to oct"                `gives` "(NoInterval,DateSpan (Just 2008-08-01) (Just 2008-10-01))"
    "every day from aug to oct" `gives` "(Daily,DateSpan (Just 2008-08-01) (Just 2008-10-01))"
    "daily from aug"            `gives` "(Daily,DateSpan (Just 2008-08-01) Nothing)"
    "every week to 2009"        `gives` "(Weekly,DateSpan Nothing (Just 2009-01-01))"

  ,"print report tests" ~: TestList
  [

   "print expenses" ~:
   do 
    let args = ["expenses"]
    l <- samplejournalwithopts [] args
    t <- getCurrentLocalTime
    showTransactions (optsToFilterSpec [] args t) l `is` unlines
     ["2008/06/03 * eat & shop"
     ,"    expenses:food                $1"
     ,"    expenses:supplies            $1"
     ,"    assets:cash                 $-2"
     ,""
     ]

  , "print report with depth arg" ~:
   do 
    l <- samplejournal
    t <- getCurrentLocalTime
    showTransactions (optsToFilterSpec [Depth "2"] [] t) l `is` unlines
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

  ,"punctuatethousands 1" ~: punctuatethousands "" `is` ""

  ,"punctuatethousands 2" ~: punctuatethousands "1234567.8901" `is` "1,234,567.8901"

  ,"punctuatethousands 3" ~: punctuatethousands "-100" `is` "-100"

  ,"register report tests" ~:
  let registerdates = filter (not . null) .  map (strip . take 10) . lines
  in
  TestList
  [

   "register report with no args" ~:
   do 
    l <- samplejournal
    (registerReportAsText [] $ registerReport [] (optsToFilterSpec [] [] t1) l) `is` unlines
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
    let opts = [Cleared]
    l <- readJournalWithOpts opts sample_journal_str
    (registerReportAsText opts $ registerReport opts (optsToFilterSpec opts [] t1) l) `is` unlines
     ["2008/06/03 eat & shop           expenses:food                    $1           $1"
     ,"                                expenses:supplies                $1           $2"
     ,"                                assets:cash                     $-2            0"
     ,"2008/12/31 pay off              liabilities:debts                $1           $1"
     ,"                                assets:bank:checking            $-1            0"
     ]

  ,"register report with uncleared option" ~:
   do 
    let opts = [UnCleared]
    l <- readJournalWithOpts opts sample_journal_str
    (registerReportAsText opts $ registerReport opts (optsToFilterSpec opts [] t1) l) `is` unlines
     ["2008/01/01 income               assets:bank:checking             $1           $1"
     ,"                                income:salary                   $-1            0"
     ,"2008/06/01 gift                 assets:bank:checking             $1           $1"
     ,"                                income:gifts                    $-1            0"
     ,"2008/06/02 save                 assets:bank:saving               $1           $1"
     ,"                                assets:bank:checking            $-1            0"
     ]

  ,"register report sorts by date" ~:
   do 
    l <- readJournalWithOpts [] $ unlines
        ["2008/02/02 a"
        ,"  b  1"
        ,"  c"
        ,""
        ,"2008/01/01 d"
        ,"  e  1"
        ,"  f"
        ]
    registerdates (registerReportAsText [] $ registerReport [] (optsToFilterSpec [] [] t1) l) `is` ["2008/01/01","2008/02/02"]

  ,"register report with account pattern" ~:
   do
    l <- samplejournal
    (registerReportAsText [] $ registerReport [] (optsToFilterSpec [] ["cash"] t1) l) `is` unlines
     ["2008/06/03 eat & shop           assets:cash                     $-2          $-2"
     ]

  ,"register report with account pattern, case insensitive" ~:
   do 
    l <- samplejournal
    (registerReportAsText [] $ registerReport [] (optsToFilterSpec [] ["cAsH"] t1) l) `is` unlines
     ["2008/06/03 eat & shop           assets:cash                     $-2          $-2"
     ]

  ,"register report with display expression" ~:
   do 
    l <- samplejournal
    let gives displayexpr = 
            (registerdates (registerReportAsText opts $ registerReport opts (optsToFilterSpec opts [] t1) l) `is`)
                where opts = [Display displayexpr]
    "d<[2008/6/2]"  `gives` ["2008/01/01","2008/06/01"]
    "d<=[2008/6/2]" `gives` ["2008/01/01","2008/06/01","2008/06/02"]
    "d=[2008/6/2]"  `gives` ["2008/06/02"]
    "d>=[2008/6/2]" `gives` ["2008/06/02","2008/06/03","2008/12/31"]
    "d>[2008/6/2]"  `gives` ["2008/06/03","2008/12/31"]

  ,"register report with period expression" ~:
   do 
    l <- samplejournal
    let periodexpr `gives` dates = do
          l' <- samplejournalwithopts opts []
          registerdates (registerReportAsText opts $ registerReport opts (optsToFilterSpec opts [] t1) l') `is` dates
              where opts = [Period periodexpr]
    ""     `gives` ["2008/01/01","2008/06/01","2008/06/02","2008/06/03","2008/12/31"]
    "2008" `gives` ["2008/01/01","2008/06/01","2008/06/02","2008/06/03","2008/12/31"]
    "2007" `gives` []
    "june" `gives` ["2008/06/01","2008/06/02","2008/06/03"]
    "monthly" `gives` ["2008/01/01","2008/06/01","2008/12/01"]
    "quarterly" `gives` ["2008/01/01","2008/04/01","2008/10/01"]
    let opts = [Period "yearly"]
    (registerReportAsText opts $ registerReport opts (optsToFilterSpec opts [] t1) l) `is` unlines
     ["2008/01/01 - 2008/12/31         assets:bank:saving               $1           $1"
     ,"                                assets:cash                     $-2          $-1"
     ,"                                expenses:food                    $1            0"
     ,"                                expenses:supplies                $1           $1"
     ,"                                income:gifts                    $-1            0"
     ,"                                income:salary                   $-1          $-1"
     ,"                                liabilities:debts                $1            0"
     ]
    let opts = [Period "quarterly"]
    registerdates (registerReportAsText opts $ registerReport opts (optsToFilterSpec opts [] t1) l) `is` ["2008/01/01","2008/04/01","2008/10/01"]
    let opts = [Period "quarterly",Empty]
    registerdates (registerReportAsText opts $ registerReport opts (optsToFilterSpec opts [] t1) l) `is` ["2008/01/01","2008/04/01","2008/07/01","2008/10/01"]

  ]

  , "register report with depth arg" ~:
   do 
    l <- samplejournal
    let opts = [Depth "2"]
    (registerReportAsText opts $ registerReport opts (optsToFilterSpec opts [] t1) l) `is` unlines
     ["2008/01/01 income               income:salary                   $-1          $-1"
     ,"2008/06/01 gift                 income:gifts                    $-1          $-2"
     ,"2008/06/03 eat & shop           expenses:food                    $1          $-1"
     ,"                                expenses:supplies                $1            0"
     ,"                                assets:cash                     $-2          $-2"
     ,"2008/12/31 pay off              liabilities:debts                $1          $-1"
     ]

  ,"show dollars" ~: show (dollars 1) ~?= "$1.00"

  ,"show hours" ~: show (hours 1) ~?= "1.0h"

  ,"unicode in balance layout" ~: do
    l <- readJournalWithOpts []
      "2009/01/01 * медвежья шкура\n  расходы:покупки  100\n  актив:наличные\n"
    balanceReportAsText [] (balanceReport [] (optsToFilterSpec [] [] t1) l) `is` unlines
      ["                -100  актив:наличные"
      ,"                 100  расходы:покупки"
      ,"--------------------"
      ,"                   0"
      ]

  ,"unicode in register layout" ~: do
    l <- readJournalWithOpts []
      "2009/01/01 * медвежья шкура\n  расходы:покупки  100\n  актив:наличные\n"
    (registerReportAsText [] $ registerReport [] (optsToFilterSpec [] [] t1) l) `is` unlines
      ["2009/01/01 медвежья шкура       расходы:покупки                 100          100"
      ,"                                актив:наличные                 -100            0"]

  ,"fixSmartDateStr" ~: do
    let gives = is . fixSmartDateStr (parsedate "2008/11/26")
    "1999-12-02"   `gives` "1999/12/02"
    "1999.12.02"   `gives` "1999/12/02"
    "1999/3/2"     `gives` "1999/03/02"
    "19990302"     `gives` "1999/03/02"
    "2008/2"       `gives` "2008/02/01"
    "0020/2"       `gives` "0020/02/01"
    "1000"         `gives` "1000/01/01"
    "4/2"          `gives` "2008/04/02"
    "2"            `gives` "2008/11/02"
    "January"      `gives` "2008/01/01"
    "feb"          `gives` "2008/02/01"
    "today"        `gives` "2008/11/26"
    "yesterday"    `gives` "2008/11/25"
    "tomorrow"     `gives` "2008/11/27"
    "this day"     `gives` "2008/11/26"
    "last day"     `gives` "2008/11/25"
    "next day"     `gives` "2008/11/27"
    "this week"    `gives` "2008/11/24" -- last monday
    "last week"    `gives` "2008/11/17" -- previous monday
    "next week"    `gives` "2008/12/01" -- next monday
    "this month"   `gives` "2008/11/01"
    "last month"   `gives` "2008/10/01"
    "next month"   `gives` "2008/12/01"
    "this quarter" `gives` "2008/10/01"
    "last quarter" `gives` "2008/07/01"
    "next quarter" `gives` "2009/01/01"
    "this year"    `gives` "2008/01/01"
    "last year"    `gives` "2007/01/01"
    "next year"    `gives` "2009/01/01"
--     "last wed"     `gives` "2008/11/19"
--     "next friday"  `gives` "2008/11/28"
--     "next january" `gives` "2009/01/01"

  ,"subAccounts" ~: do
    l <- liftM (journalToLedger nullfilterspec) samplejournal
    let a = ledgerAccount l "assets"
    map aname (ledgerSubAccounts l a) `is` ["assets:bank","assets:cash"]

  -- ,"summarisePostingsInDateSpan" ~: do
  --   let gives (b,e,depth,showempty,ps) =
  --           (summarisePostingsInDateSpan (mkdatespan b e) depth showempty ps `is`)
  --   let ps =
  --           [
  --            nullposting{lpdescription="desc",lpaccount="expenses:food:groceries",lpamount=Mixed [dollars 1]}
  --           ,nullposting{lpdescription="desc",lpaccount="expenses:food:dining",   lpamount=Mixed [dollars 2]}
  --           ,nullposting{lpdescription="desc",lpaccount="expenses:food",          lpamount=Mixed [dollars 4]}
  --           ,nullposting{lpdescription="desc",lpaccount="expenses:food:dining",   lpamount=Mixed [dollars 8]}
  --           ]
  --   ("2008/01/01","2009/01/01",0,9999,False,[]) `gives` 
  --    []
  --   ("2008/01/01","2009/01/01",0,9999,True,[]) `gives` 
  --    [
  --     nullposting{lpdate=parsedate "2008/01/01",lpdescription="- 2008/12/31"}
  --    ]
  --   ("2008/01/01","2009/01/01",0,9999,False,ts) `gives` 
  --    [
  --     nullposting{lpdate=parsedate "2008/01/01",lpdescription="- 2008/12/31",lpaccount="expenses:food",          lpamount=Mixed [dollars 4]}
  --    ,nullposting{lpdate=parsedate "2008/01/01",lpdescription="- 2008/12/31",lpaccount="expenses:food:dining",   lpamount=Mixed [dollars 10]}
  --    ,nullposting{lpdate=parsedate "2008/01/01",lpdescription="- 2008/12/31",lpaccount="expenses:food:groceries",lpamount=Mixed [dollars 1]}
  --    ]
  --   ("2008/01/01","2009/01/01",0,2,False,ts) `gives` 
  --    [
  --     nullposting{lpdate=parsedate "2008/01/01",lpdescription="- 2008/12/31",lpaccount="expenses:food",lpamount=Mixed [dollars 15]}
  --    ]
  --   ("2008/01/01","2009/01/01",0,1,False,ts) `gives` 
  --    [
  --     nullposting{lpdate=parsedate "2008/01/01",lpdescription="- 2008/12/31",lpaccount="expenses",lpamount=Mixed [dollars 15]}
  --    ]
  --   ("2008/01/01","2009/01/01",0,0,False,ts) `gives` 
  --    [
  --     nullposting{lpdate=parsedate "2008/01/01",lpdescription="- 2008/12/31",lpaccount="",lpamount=Mixed [dollars 15]}
  --    ]

  ]

  
-- fixtures/test data

date1 = parsedate "2008/11/26"
t1 = LocalTime date1 midday

samplejournal = readJournalWithOpts [] sample_journal_str
samplejournalwithopts opts _ = readJournalWithOpts opts sample_journal_str

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

write_sample_journal = writeFile "sample.journal" sample_journal_str

entry2_str = unlines
 ["2007/01/27 * joes diner"
 ,"    expenses:food:dining                      $10.00"
 ,"    expenses:gifts                            $10.00"
 ,"    assets:checking                          $-20.00"
 ,""
 ]

entry3_str = unlines
 ["2007/01/01 * opening balance"
 ,"    assets:cash                                $4.82"
 ,"    equity:opening balances"
 ,""
 ,"2007/01/01 * opening balance"
 ,"    assets:cash                                $4.82"
 ,"    equity:opening balances"
 ,""
 ,"2007/01/28 coopportunity"
 ,"  expenses:food:groceries                 $47.18"
 ,"  assets:checking"
 ,""
 ]

periodic_entry1_str = unlines
 ["~ monthly from 2007/2/2"
 ,"  assets:saving            $200.00"
 ,"  assets:checking"
 ,""
 ]

periodic_entry2_str = unlines
 ["~ monthly from 2007/2/2"
 ,"  assets:saving            $200.00         ;auto savings"
 ,"  assets:checking"
 ,""
 ]

periodic_entry3_str = unlines
 ["~ monthly from 2007/01/01"
 ,"    assets:cash                                $4.82"
 ,"    equity:opening balances"
 ,""
 ,"~ monthly from 2007/01/01"
 ,"    assets:cash                                $4.82"
 ,"    equity:opening balances"
 ,""
 ]

journal1_str = unlines
 [""
 ,"2007/01/27 * joes diner"
 ,"  expenses:food:dining                    $10.00"
 ,"  expenses:gifts                          $10.00"
 ,"  assets:checking                        $-20.00"
 ,""
 ,""
 ,"2007/01/28 coopportunity"
 ,"  expenses:food:groceries                 $47.18"
 ,"  assets:checking                        $-47.18"
 ,""
 ,""
 ]

journal2_str = unlines
 [";comment"
 ,"2007/01/27 * joes diner"
 ,"  expenses:food:dining                    $10.00"
 ,"  assets:checking                        $-47.18"
 ,""
 ]

journal3_str = unlines
 ["2007/01/27 * joes diner"
 ,"  expenses:food:dining                    $10.00"
 ,";intra-entry comment"
 ,"  assets:checking                        $-47.18"
 ,""
 ]

journal4_str = unlines
 ["!include \"somefile\""
 ,"2007/01/27 * joes diner"
 ,"  expenses:food:dining                    $10.00"
 ,"  assets:checking                        $-47.18"
 ,""
 ]

journal5_str = ""

journal6_str = unlines
 ["~ monthly from 2007/1/21"
 ,"    expenses:entertainment  $16.23        ;netflix"
 ,"    assets:checking"
 ,""
 ,"; 2007/01/01 * opening balance"
 ,";     assets:saving                            $200.04"
 ,";     equity:opening balances                         "
 ,""
 ]

journal7_str = unlines
 ["2007/01/01 * opening balance"
 ,"    assets:cash                                $4.82"
 ,"    equity:opening balances                         "
 ,""
 ,"2007/01/01 * opening balance"
 ,"    income:interest                                $-4.82"
 ,"    equity:opening balances                         "
 ,""
 ,"2007/01/02 * ayres suites"
 ,"    expenses:vacation                        $179.92"
 ,"    assets:checking                                 "
 ,""
 ,"2007/01/02 * auto transfer to savings"
 ,"    assets:saving                            $200.00"
 ,"    assets:checking                                 "
 ,""
 ,"2007/01/03 * poquito mas"
 ,"    expenses:food:dining                       $4.82"
 ,"    assets:cash                                     "
 ,""
 ,"2007/01/03 * verizon"
 ,"    expenses:phone                            $95.11"
 ,"    assets:checking                                 "
 ,""
 ,"2007/01/03 * discover"
 ,"    liabilities:credit cards:discover         $80.00"
 ,"    assets:checking                                 "
 ,""
 ,"2007/01/04 * blue cross"
 ,"    expenses:health:insurance                 $90.00"
 ,"    assets:checking                                 "
 ,""
 ,"2007/01/05 * village market liquor"
 ,"    expenses:food:dining                       $6.48"
 ,"    assets:checking                                 "
 ,""
 ]

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
             tpostings=[
              Posting {
                pstatus=False,
                paccount="assets:cash",
                pamount=(Mixed [dollars 4.82]),
                pcomment="",
                ptype=RegularPosting,
                ptransaction=Nothing
              },
              Posting {
                pstatus=False,
                paccount="equity:opening balances",
                pamount=(Mixed [dollars (-4.82)]),
                pcomment="",
                ptype=RegularPosting,
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
             tpostings=[
              Posting {
                pstatus=False,
                paccount="expenses:vacation",
                pamount=(Mixed [dollars 179.92]),
                pcomment="",
                ptype=RegularPosting,
                ptransaction=Nothing
              },
              Posting {
                pstatus=False,
                paccount="assets:checking",
                pamount=(Mixed [dollars (-179.92)]),
                pcomment="",
                ptype=RegularPosting,
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
             tpostings=[
              Posting {
                pstatus=False,
                paccount="assets:saving",
                pamount=(Mixed [dollars 200]),
                pcomment="",
                ptype=RegularPosting,
                ptransaction=Nothing
              },
              Posting {
                pstatus=False,
                paccount="assets:checking",
                pamount=(Mixed [dollars (-200)]),
                pcomment="",
                ptype=RegularPosting,
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
             tpostings=[
              Posting {
                pstatus=False,
                paccount="expenses:food:dining",
                pamount=(Mixed [dollars 4.82]),
                pcomment="",
                ptype=RegularPosting,
                ptransaction=Nothing
              },
              Posting {
                pstatus=False,
                paccount="assets:cash",
                pamount=(Mixed [dollars (-4.82)]),
                pcomment="",
                ptype=RegularPosting,
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
             tpostings=[
              Posting {
                pstatus=False,
                paccount="expenses:phone",
                pamount=(Mixed [dollars 95.11]),
                pcomment="",
                ptype=RegularPosting,
                ptransaction=Nothing
              },
              Posting {
                pstatus=False,
                paccount="assets:checking",
                pamount=(Mixed [dollars (-95.11)]),
                pcomment="",
                ptype=RegularPosting,
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
             tpostings=[
              Posting {
                pstatus=False,
                paccount="liabilities:credit cards:discover",
                pamount=(Mixed [dollars 80]),
                pcomment="",
                ptype=RegularPosting,
                ptransaction=Nothing
              },
              Posting {
                pstatus=False,
                paccount="assets:checking",
                pamount=(Mixed [dollars (-80)]),
                pcomment="",
                ptype=RegularPosting,
                ptransaction=Nothing
              }
             ],
             tpreceding_comment_lines=""
           }
          ]
          []
          []
          ""
          ""
          (TOD 0 0)
          ""

ledger7 = journalToLedger nullfilterspec journal7

journal8_str = unlines
 ["2008/1/1 test           "
 ,"  a:b          10h @ $40"
 ,"  c:d                   "
 ,""
 ]

timelogentry1_str  = "i 2007/03/11 16:19:00 hledger\n"
timelogentry1 = TimeLogEntry In (parsedatetime "2007/03/11 16:19:00") "hledger"

timelogentry2_str  = "o 2007/03/11 16:30:00\n"
timelogentry2 = TimeLogEntry Out (parsedatetime "2007/03/11 16:30:00") ""

a1 = Mixed [(hours 1){price=Just $ Mixed [Amount (comm "$") 10 Nothing]}]
a2 = Mixed [(hours 2){price=Just $ Mixed [Amount (comm "EUR") 10 Nothing]}]
a3 = Mixed $ amounts a1 ++ amounts a2

journalWithAmounts :: [String] -> Journal
journalWithAmounts as =
        Journal
        []
        []
        [t | a <- as, let t = nulltransaction{tdescription=a,tpostings=[nullposting{pamount=parse a,ptransaction=Just t}]}]
        []
        []
        ""
        ""
        (TOD 0 0)
        ""
    where parse = fromparse . parseWithCtx emptyCtx someamount

