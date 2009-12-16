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
source tree. They are no longer used, but here is an example:

@
$ hledger -f sample.ledger balance o
                  $1  expenses:food
                 $-2  income
                 $-1    gifts
                 $-1    salary
--------------------
                 $-1
@

-}

module Tests
where
import qualified Data.Map as Map
import Data.Time.Format
import Locale (defaultTimeLocale)
import Text.ParserCombinators.Parsec
import Test.HUnit.Tools (runVerboseTests)
import System.Exit (exitFailure, exitWith, ExitCode(ExitSuccess)) -- base 3 compatible
import System.Time (ClockTime(TOD))

import Commands.All
import Ledger
import Options
import Utils


runtests opts args = do
  (counts,_) <- runner ts
  if errors counts > 0 || (failures counts > 0)
   then exitFailure
   else exitWith ExitSuccess
    where
      runner | Verbose `elem` opts = runVerboseTests
             | otherwise = liftM (flip (,) 0) . runTestTT
      ts = TestList $ filter matchname $ concatMap tflatten tests
      --ts = tfilter matchname $ TestList tests -- unflattened
      matchname = matchpats args . tname

-- | Get a Test's label, or the empty string.
tname :: Test -> String
tname (TestLabel n _) = n
tname _ = ""

-- | Flatten a Test containing TestLists into a list of single tests.
tflatten :: Test -> [Test]
tflatten (TestLabel _ t@(TestList _)) = tflatten t
tflatten (TestList ts) = concatMap tflatten ts
tflatten t = [t]

-- | Filter TestLists in a Test, recursively, preserving the structure.
tfilter :: (Test -> Bool) -> Test -> Test
tfilter p (TestLabel l ts) = TestLabel l (tfilter p ts)
tfilter p (TestList ts) = TestList $ filter (any p . tflatten) $ map (tfilter p) ts
tfilter _ t = t

-- | Simple way to assert something is some expected value, with no label.
is :: (Eq a, Show a) => a -> a -> Assertion
a `is` e = assertEqual "" e a

-- | Assert a parse result is some expected value, or print a parse error.
parseis :: (Show a, Eq a) => (Either ParseError a) -> a -> Assertion
parse `parseis` expected = either printParseError (`is` expected) parse

------------------------------------------------------------------------------
-- | Tests for any function or topic. Mostly ordered by test name.
tests :: [Test]
tests = [

   "account directive" ~: 
   let sameParse str1 str2 = do l1 <- journalFromString str1
                                l2 <- journalFromString str2
                                l1 `is` l2
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

  ,"amount arithmetic" ~: do
    let a1 = dollars 1.23
    let a2 = Amount (comm "$") (-1.23) Nothing
    let a3 = Amount (comm "$") (-1.23) Nothing
    (a1 + a2) `is` Amount (comm "$") 0 Nothing
    (a1 + a3) `is` Amount (comm "$") 0 Nothing
    (a2 + a3) `is` Amount (comm "$") (-2.46) Nothing
    (a3 + a3) `is` Amount (comm "$") (-2.46) Nothing
    sum [a2,a3] `is` Amount (comm "$") (-2.46) Nothing
    sum [a3,a3] `is` Amount (comm "$") (-2.46) Nothing
    sum [a1,a2,a3,-a3] `is` Amount (comm "$") 0 Nothing
    let dollar0 = dollar{precision=0}
    (sum [Amount dollar 1.25 Nothing, Amount dollar0 (-1) Nothing, Amount dollar (-0.25) Nothing])
      `is` (Amount dollar 0 Nothing)

  ,"mixed amount arithmetic" ~: do
    let dollar0 = dollar{precision=0}
    (sum $ map (Mixed . (\a -> [a]))
             [Amount dollar 1.25 Nothing,
              Amount dollar0 (-1) Nothing,
              Amount dollar (-0.25) Nothing])
      `is` Mixed [Amount dollar 0 Nothing]

  ,"balance report tests" ~:
   let (opts,args) `gives` es = do 
        l <- sampleledgerwithopts opts args
        showBalanceReport opts args l `is` unlines es
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
    ]

   ,"balance report can be limited with --depth" ~:
    ([Depth "1"], []) `gives`
    ["                 $-1  assets"
    ,"                  $2  expenses"
    ,"                 $-2  income"
    ,"                  $1  liabilities"
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
    ([], ["not:e"]) `gives` []

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
      rl <- journalFromString $ unlines
             [""
             ,"2008/1/1 test           "
             ,"  a:b          10h @ $50"
             ,"  c:d                   "
             ,""
             ]
      let l = cacheLedger [] $ 
              filterJournal (DateSpan Nothing Nothing) [] Nothing False $ 
              canonicaliseAmounts True rl -- enable cost basis adjustment            
      showBalanceReport [] [] l `is` 
       unlines
        ["                $500  a:b"
        ,"               $-500  c:d"
        ]

   ,"balance report elides zero-balance root account(s)" ~: do
      l <- ledgerFromStringWithOpts [] [] sampletime
             (unlines
              ["2008/1/1 one"
              ,"  test:a  1"
              ,"  test:b"
              ])
      showBalanceReport [] [] l `is`
       unlines
        ["                   1  test:a"
        ,"                  -1  test:b"
        ]

   ]

  ,"balanceLedgerTransaction" ~: do
     assertBool "detect unbalanced entry, sign error"
                    (isLeft $ balanceLedgerTransaction
                           (LedgerTransaction (parsedate "2007/01/28") Nothing False "" "test" ""
                            [Posting False "a" (Mixed [dollars 1]) "" RegularPosting, 
                             Posting False "b" (Mixed [dollars 1]) "" RegularPosting
                            ] ""))
     assertBool "detect unbalanced entry, multiple missing amounts"
                    (isLeft $ balanceLedgerTransaction
                           (LedgerTransaction (parsedate "2007/01/28") Nothing False "" "test" ""
                            [Posting False "a" missingamt "" RegularPosting, 
                             Posting False "b" missingamt "" RegularPosting
                            ] ""))
     let e = balanceLedgerTransaction (LedgerTransaction (parsedate "2007/01/28") Nothing False "" "test" ""
                           [Posting False "a" (Mixed [dollars 1]) "" RegularPosting, 
                            Posting False "b" missingamt "" RegularPosting
                           ] "")
     assertBool "one missing amount should be ok" (isRight e)
     assertEqual "balancing amount is added" 
                     (Mixed [dollars (-1)])
                     (case e of
                        Right e' -> (pamount $ last $ ltpostings e')
                        Left _ -> error "should not happen")

  ,"cacheLedger" ~:
    length (Map.keys $ accountmap $ cacheLedger [] journal7) `is` 15

  ,"canonicaliseAmounts" ~:
   "use the greatest precision" ~:
    journalPrecisions (canonicaliseAmounts False $ journalWithAmounts ["1","2.00"]) `is` [2,2]

  ,"commodities" ~:
    commodities ledger7 `is` [Commodity {symbol="$", side=L, spaced=False, comma=False, precision=2}]

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

  ,"entriesFromTimeLogEntries" ~: do
     today <- getCurrentDay
     now' <- getCurrentTime
     tz <- getCurrentTimeZone
     let now = utcToLocalTime tz now'
         nowstr = showtime now
         yesterday = prevday today
         clockin = TimeLogEntry In
         mktime d = LocalTime d . fromMaybe midnight . parseTime defaultTimeLocale "%H:%M:%S"
         showtime = formatTime defaultTimeLocale "%H:%M"
         assertEntriesGiveStrings name es ss = assertEqual name ss (map ltdescription $ entriesFromTimeLogEntries now es)

     assertEntriesGiveStrings "started yesterday, split session at midnight"
                                  [clockin (mktime yesterday "23:00:00") ""]
                                  ["23:00-23:59","00:00-"++nowstr]
     assertEntriesGiveStrings "split multi-day sessions at each midnight"
                                  [clockin (mktime (addDays (-2) today) "23:00:00") ""]
                                  ["23:00-23:59","00:00-23:59","00:00-"++nowstr]
     assertEntriesGiveStrings "auto-clock-out if needed" 
                                  [clockin (mktime today "00:00:00") ""] 
                                  ["00:00-"++nowstr]
     let future = utcToLocalTime tz $ addUTCTime 100 now'
         futurestr = showtime future
     assertEntriesGiveStrings "use the clockin time for auto-clockout if it's in the future"
                                  [clockin future ""]
                                  [printf "%s-%s" futurestr futurestr]

  ,"expandAccountNames" ~:
    expandAccountNames ["assets:cash","assets:checking","expenses:vacation"] `is`
     ["assets","assets:cash","assets:checking","expenses","expenses:vacation"]

  ,"intervalFromOpts" ~: do
    let gives = is . intervalFromOpts
    [] `gives` NoInterval
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

  ,"isLedgerTransactionBalanced" ~: do
     assertBool "detect balanced"
        (isLedgerTransactionBalanced
        (LedgerTransaction (parsedate "2009/01/01") Nothing False "" "a" ""
         [Posting False "b" (Mixed [dollars 1.00]) "" RegularPosting
         ,Posting False "c" (Mixed [dollars (-1.00)]) "" RegularPosting
         ] ""))
     assertBool "detect unbalanced"
        (not $ isLedgerTransactionBalanced
        (LedgerTransaction (parsedate "2009/01/01") Nothing False "" "a" ""
         [Posting False "b" (Mixed [dollars 1.00]) "" RegularPosting
         ,Posting False "c" (Mixed [dollars (-1.01)]) "" RegularPosting
         ] ""))
     assertBool "detect unbalanced, one posting"
        (not $ isLedgerTransactionBalanced
        (LedgerTransaction (parsedate "2009/01/01") Nothing False "" "a" ""
         [Posting False "b" (Mixed [dollars 1.00]) "" RegularPosting
         ] ""))
     assertBool "one zero posting is considered balanced for now"
        (isLedgerTransactionBalanced
        (LedgerTransaction (parsedate "2009/01/01") Nothing False "" "a" ""
         [Posting False "b" (Mixed [dollars 0]) "" RegularPosting
         ] ""))
     assertBool "virtual postings don't need to balance"
        (isLedgerTransactionBalanced
        (LedgerTransaction (parsedate "2009/01/01") Nothing False "" "a" ""
         [Posting False "b" (Mixed [dollars 1.00]) "" RegularPosting
         ,Posting False "c" (Mixed [dollars (-1.00)]) "" RegularPosting
         ,Posting False "d" (Mixed [dollars 100]) "" VirtualPosting
         ] ""))
     assertBool "balanced virtual postings need to balance among themselves"
        (not $ isLedgerTransactionBalanced
        (LedgerTransaction (parsedate "2009/01/01") Nothing False "" "a" ""
         [Posting False "b" (Mixed [dollars 1.00]) "" RegularPosting
         ,Posting False "c" (Mixed [dollars (-1.00)]) "" RegularPosting
         ,Posting False "d" (Mixed [dollars 100]) "" BalancedVirtualPosting
         ] ""))
     assertBool "balanced virtual postings need to balance among themselves (2)"
        (isLedgerTransactionBalanced
        (LedgerTransaction (parsedate "2009/01/01") Nothing False "" "a" ""
         [Posting False "b" (Mixed [dollars 1.00]) "" RegularPosting
         ,Posting False "c" (Mixed [dollars (-1.00)]) "" RegularPosting
         ,Posting False "d" (Mixed [dollars 100]) "" BalancedVirtualPosting
         ,Posting False "e" (Mixed [dollars (-100)]) "" BalancedVirtualPosting
         ] ""))

  ,"isSubAccountNameOf" ~: do
    "assets" `isSubAccountNameOf` "assets" `is` False
    "assets:bank" `isSubAccountNameOf` "assets" `is` True
    "assets:bank:checking" `isSubAccountNameOf` "assets" `is` False
    "assets:bank" `isSubAccountNameOf` "my assets" `is` False

  ,"default year" ~: do
    rl <- journalFromString defaultyear_ledger_str
    ltdate (head $ ledger_txns rl) `is` fromGregorian 2009 1 1
    return ()

  ,"ledgerFile" ~: do
    assertBool "ledgerFile should parse an empty file" (isRight $ parseWithCtx emptyCtx ledgerFile "")
    r <- journalFromString "" -- don't know how to get it from ledgerFile
    assertBool "ledgerFile parsing an empty file should give an empty ledger" $ null $ ledger_txns r

  ,"ledgerHistoricalPrice" ~:
    parseWithCtx emptyCtx ledgerHistoricalPrice price1_str `parseis` price1

  ,"ledgerTransaction" ~: do
    parseWithCtx emptyCtx ledgerTransaction entry1_str `parseis` entry1
    assertBool "ledgerTransaction should not parse just a date"
                   $ isLeft $ parseWithCtx emptyCtx ledgerTransaction "2009/1/1\n"
    assertBool "ledgerTransaction should require some postings"
                   $ isLeft $ parseWithCtx emptyCtx ledgerTransaction "2009/1/1 a\n"
    let t = parseWithCtx emptyCtx ledgerTransaction "2009/1/1 a ;comment\n b 1\n"
    assertBool "ledgerTransaction should not include a comment in the description"
                   $ either (const False) ((== "a") . ltdescription) t

  ,"ledgeraccountname" ~: do
    assertBool "ledgeraccountname parses a normal accountname" (isRight $ parsewith ledgeraccountname "a:b:c")
    assertBool "ledgeraccountname rejects an empty inner component" (isLeft $ parsewith ledgeraccountname "a::c")
    assertBool "ledgeraccountname rejects an empty leading component" (isLeft $ parsewith ledgeraccountname ":b:c")
    assertBool "ledgeraccountname rejects an empty trailing component" (isLeft $ parsewith ledgeraccountname "a:b:")

  ,"ledgerposting" ~:
    parseWithCtx emptyCtx ledgerposting rawposting1_str `parseis` rawposting1

  ,"parsedate" ~: do
    parsedate "2008/02/03" `is` parsetimewith "%Y/%m/%d" "2008/02/03" sampledate
    parsedate "2008-02-03" `is` parsetimewith "%Y/%m/%d" "2008/02/03" sampledate

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
    l <- sampleledgerwithopts [] args
    showLedgerTransactions [] args l `is` unlines 
     ["2008/06/03 * eat & shop"
     ,"    expenses:food                $1"
     ,"    expenses:supplies            $1"
     ,"    assets:cash                 $-2"
     ,""
     ]

  , "print report with depth arg" ~:
   do 
    l <- sampleledger
    showLedgerTransactions [Depth "2"] [] l `is` unlines
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
    l <- sampleledger
    showRegisterReport [] [] l `is` unlines
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

  ,"register report with cleared arg" ~:
   do 
    l <- ledgerFromStringWithOpts [Cleared] [] sampletime sample_ledger_str
    showRegisterReport [Cleared] [] l `is` unlines
     ["2008/06/03 eat & shop           expenses:food                    $1           $1"
     ,"                                expenses:supplies                $1           $2"
     ,"                                assets:cash                     $-2            0"
     ,"2008/12/31 pay off              liabilities:debts                $1           $1"
     ,"                                assets:bank:checking            $-1            0"
     ]

  ,"register report with uncleared arg" ~:
   do 
    l <- ledgerFromStringWithOpts [UnCleared] [] sampletime sample_ledger_str
    showRegisterReport [UnCleared] [] l `is` unlines
     ["2008/01/01 income               assets:bank:checking             $1           $1"
     ,"                                income:salary                   $-1            0"
     ,"2008/06/01 gift                 assets:bank:checking             $1           $1"
     ,"                                income:gifts                    $-1            0"
     ,"2008/06/02 save                 assets:bank:saving               $1           $1"
     ,"                                assets:bank:checking            $-1            0"
     ]

  ,"register report sorts by date" ~:
   do 
    l <- ledgerFromStringWithOpts [] [] sampletime $ unlines
        ["2008/02/02 a"
        ,"  b  1"
        ,"  c"
        ,""
        ,"2008/01/01 d"
        ,"  e  1"
        ,"  f"
        ]
    registerdates (showRegisterReport [] [] l) `is` ["2008/01/01","2008/02/02"]

  ,"register report with account pattern" ~:
   do
    l <- sampleledger
    showRegisterReport [] ["cash"] l `is` unlines
     ["2008/06/03 eat & shop           assets:cash                     $-2          $-2"
     ]

  ,"register report with account pattern, case insensitive" ~:
   do 
    l <- sampleledger
    showRegisterReport [] ["cAsH"] l `is` unlines
     ["2008/06/03 eat & shop           assets:cash                     $-2          $-2"
     ]

  ,"register report with display expression" ~:
   do 
    l <- sampleledger
    let gives displayexpr = 
            (registerdates (showRegisterReport [Display displayexpr] [] l) `is`)
    "d<[2008/6/2]"  `gives` ["2008/01/01","2008/06/01"]
    "d<=[2008/6/2]" `gives` ["2008/01/01","2008/06/01","2008/06/02"]
    "d=[2008/6/2]"  `gives` ["2008/06/02"]
    "d>=[2008/6/2]" `gives` ["2008/06/02","2008/06/03","2008/12/31"]
    "d>[2008/6/2]"  `gives` ["2008/06/03","2008/12/31"]

  ,"register report with period expression" ~:
   do 
    l <- sampleledger    
    let periodexpr `gives` dates = do
          lopts <- sampleledgerwithopts [Period periodexpr] []
          registerdates (showRegisterReport [Period periodexpr] [] lopts) `is` dates
    ""     `gives` ["2008/01/01","2008/06/01","2008/06/02","2008/06/03","2008/12/31"]
    "2008" `gives` ["2008/01/01","2008/06/01","2008/06/02","2008/06/03","2008/12/31"]
    "2007" `gives` []
    "june" `gives` ["2008/06/01","2008/06/02","2008/06/03"]
    "monthly" `gives` ["2008/01/01","2008/06/01","2008/12/01"]
    "quarterly" `gives` ["2008/01/01","2008/04/01","2008/10/01"]
    showRegisterReport [Period "yearly"] [] l `is` unlines
     ["2008/01/01 - 2008/12/31         assets:bank:saving               $1           $1"
     ,"                                assets:cash                     $-2          $-1"
     ,"                                expenses:food                    $1            0"
     ,"                                expenses:supplies                $1           $1"
     ,"                                income:gifts                    $-1            0"
     ,"                                income:salary                   $-1          $-1"
     ,"                                liabilities:debts                $1            0"
     ]
    registerdates (showRegisterReport [Period "quarterly"] [] l) `is` ["2008/01/01","2008/04/01","2008/10/01"]
    registerdates (showRegisterReport [Period "quarterly",Empty] [] l) `is` ["2008/01/01","2008/04/01","2008/07/01","2008/10/01"]

  ]

  , "register report with depth arg" ~:
   do 
    l <- sampleledger
    showRegisterReport [Depth "2"] [] l `is` unlines
     ["2008/01/01 income               income:salary                   $-1          $-1"
     ,"2008/06/01 gift                 income:gifts                    $-1          $-2"
     ,"2008/06/03 eat & shop           expenses:food                    $1          $-1"
     ,"                                expenses:supplies                $1            0"
     ,"                                assets:cash                     $-2          $-2"
     ,"2008/12/31 pay off              liabilities:debts                $1          $-1"
     ]

  ,"show dollars" ~: show (dollars 1) ~?= "$1.00"

  ,"show hours" ~: show (hours 1) ~?= "1.0h"

  ,"showLedgerTransaction" ~: do
     assertEqual "show a balanced transaction, eliding last amount"
       (unlines
        ["2007/01/28 coopportunity"
        ,"    expenses:food:groceries        $47.18"
        ,"    assets:checking"
        ,""
        ])
       (showLedgerTransaction
        (LedgerTransaction (parsedate "2007/01/28") Nothing False "" "coopportunity" ""
         [Posting False "expenses:food:groceries" (Mixed [dollars 47.18]) "" RegularPosting
         ,Posting False "assets:checking" (Mixed [dollars (-47.18)]) "" RegularPosting
         ] ""))
     assertEqual "show a balanced transaction, no eliding"
       (unlines
        ["2007/01/28 coopportunity"
        ,"    expenses:food:groceries        $47.18"
        ,"    assets:checking               $-47.18"
        ,""
        ])
       (showLedgerTransactionUnelided
        (LedgerTransaction (parsedate "2007/01/28") Nothing False "" "coopportunity" ""
         [Posting False "expenses:food:groceries" (Mixed [dollars 47.18]) "" RegularPosting
         ,Posting False "assets:checking" (Mixed [dollars (-47.18)]) "" RegularPosting
         ] ""))
     -- document some cases that arise in debug/testing:
     assertEqual "show an unbalanced transaction, should not elide"
       (unlines
        ["2007/01/28 coopportunity"
        ,"    expenses:food:groceries        $47.18"
        ,"    assets:checking               $-47.19"
        ,""
        ])
       (showLedgerTransaction
        (LedgerTransaction (parsedate "2007/01/28") Nothing False "" "coopportunity" ""
         [Posting False "expenses:food:groceries" (Mixed [dollars 47.18]) "" RegularPosting
         ,Posting False "assets:checking" (Mixed [dollars (-47.19)]) "" RegularPosting
         ] ""))
     assertEqual "show an unbalanced transaction with one posting, should not elide"
       (unlines
        ["2007/01/28 coopportunity"
        ,"    expenses:food:groceries        $47.18"
        ,""
        ])
       (showLedgerTransaction
        (LedgerTransaction (parsedate "2007/01/28") Nothing False "" "coopportunity" ""
         [Posting False "expenses:food:groceries" (Mixed [dollars 47.18]) "" RegularPosting
         ] ""))
     assertEqual "show a transaction with one posting and a missing amount"
       (unlines
        ["2007/01/28 coopportunity"
        ,"    expenses:food:groceries              "
        ,""
        ])
       (showLedgerTransaction
        (LedgerTransaction (parsedate "2007/01/28") Nothing False "" "coopportunity" ""
         [Posting False "expenses:food:groceries" missingamt "" RegularPosting
         ] ""))

  ,"unicode in balance layout" ~: do
    l <- ledgerFromStringWithOpts [] [] sampletime
      "2009/01/01 * медвежья шкура\n  расходы:покупки  100\n  актив:наличные\n"
    showBalanceReport [] [] l `is` unlines
      ["                -100  актив:наличные"
      ,"                 100  расходы:покупки"]

  ,"unicode in register layout" ~: do
    l <- ledgerFromStringWithOpts [] [] sampletime
      "2009/01/01 * медвежья шкура\n  расходы:покупки  100\n  актив:наличные\n"
    showRegisterReport [] [] l `is` unlines
      ["2009/01/01 медвежья шкура       расходы:покупки                 100          100"
      ,"                                актив:наличные                 -100            0"]

  ,"smart dates" ~: do
    let gives = is . fixSmartDateStr (parsedate "2008/11/26")
    "1999-12-02"   `gives` "1999/12/02"
    "1999.12.02"   `gives` "1999/12/02"
    "1999/3/2"     `gives` "1999/03/02"
    "19990302"     `gives` "1999/03/02"
    "2008/2"       `gives` "2008/02/01"
    "20/2"         `gives` "0020/02/01"
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

  ,"splitSpan" ~: do
    let gives (interval, span) = (splitSpan interval span `is`)
    (NoInterval,mkdatespan "2008/01/01" "2009/01/01") `gives`
     [mkdatespan "2008/01/01" "2009/01/01"]
    (Quarterly,mkdatespan "2008/01/01" "2009/01/01") `gives`
     [mkdatespan "2008/01/01" "2008/04/01"
     ,mkdatespan "2008/04/01" "2008/07/01"
     ,mkdatespan "2008/07/01" "2008/10/01"
     ,mkdatespan "2008/10/01" "2009/01/01"
     ]
    (Quarterly,nulldatespan) `gives`
     [nulldatespan]
    (Daily,mkdatespan "2008/01/01" "2008/01/01") `gives`
     [mkdatespan "2008/01/01" "2008/01/01"]
    (Quarterly,mkdatespan "2008/01/01" "2008/01/01") `gives`
     [mkdatespan "2008/01/01" "2008/01/01"]

  ,"subAccounts" ~: do
    l <- sampleledger
    let a = ledgerAccount l "assets"
    map aname (ledgerSubAccounts l a) `is` ["assets:bank","assets:cash"]

  ,"summariseTransactionsInDateSpan" ~: do
    let gives (b,e,tnum,depth,showempty,ts) = 
            (summariseTransactionsInDateSpan (mkdatespan b e) tnum depth showempty ts `is`)
    let ts =
            [
             nulltxn{tdescription="desc",taccount="expenses:food:groceries",tamount=Mixed [dollars 1]}
            ,nulltxn{tdescription="desc",taccount="expenses:food:dining",   tamount=Mixed [dollars 2]}
            ,nulltxn{tdescription="desc",taccount="expenses:food",          tamount=Mixed [dollars 4]}
            ,nulltxn{tdescription="desc",taccount="expenses:food:dining",   tamount=Mixed [dollars 8]}
            ]
    ("2008/01/01","2009/01/01",0,9999,False,[]) `gives` 
     []
    ("2008/01/01","2009/01/01",0,9999,True,[]) `gives` 
     [
      nulltxn{tdate=parsedate "2008/01/01",tdescription="- 2008/12/31"}
     ]
    ("2008/01/01","2009/01/01",0,9999,False,ts) `gives` 
     [
      nulltxn{tdate=parsedate "2008/01/01",tdescription="- 2008/12/31",taccount="expenses:food",          tamount=Mixed [dollars 4]}
     ,nulltxn{tdate=parsedate "2008/01/01",tdescription="- 2008/12/31",taccount="expenses:food:dining",   tamount=Mixed [dollars 10]}
     ,nulltxn{tdate=parsedate "2008/01/01",tdescription="- 2008/12/31",taccount="expenses:food:groceries",tamount=Mixed [dollars 1]}
     ]
    ("2008/01/01","2009/01/01",0,2,False,ts) `gives` 
     [
      nulltxn{tdate=parsedate "2008/01/01",tdescription="- 2008/12/31",taccount="expenses:food",tamount=Mixed [dollars 15]}
     ]
    ("2008/01/01","2009/01/01",0,1,False,ts) `gives` 
     [
      nulltxn{tdate=parsedate "2008/01/01",tdescription="- 2008/12/31",taccount="expenses",tamount=Mixed [dollars 15]}
     ]
    ("2008/01/01","2009/01/01",0,0,False,ts) `gives` 
     [
      nulltxn{tdate=parsedate "2008/01/01",tdescription="- 2008/12/31",taccount="",tamount=Mixed [dollars 15]}
     ]

  ,"postingamount" ~: do
    parseWithCtx emptyCtx postingamount " $47.18" `parseis` Mixed [dollars 47.18]
    parseWithCtx emptyCtx postingamount " $1." `parseis` 
     Mixed [Amount Commodity {symbol="$",side=L,spaced=False,comma=False,precision=0} 1 Nothing]

  ]

  
------------------------------------------------------------------------------
-- test data

sampledate = parsedate "2008/11/26"
sampletime = LocalTime sampledate midday
sampleledger = ledgerFromStringWithOpts [] [] sampletime sample_ledger_str
sampleledgerwithopts opts args = ledgerFromStringWithOpts opts args sampletime sample_ledger_str

sample_ledger_str = unlines
 ["; A sample ledger file."
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

defaultyear_ledger_str = unlines
 ["Y2009"
 ,""
 ,"01/01 A"
 ,"    a  $1"
 ,"    b"
 ]

write_sample_ledger = writeFile "sample.ledger" sample_ledger_str

rawposting1_str  = "  expenses:food:dining  $10.00\n"

rawposting1 = Posting False "expenses:food:dining" (Mixed [dollars 10]) "" RegularPosting

entry1_str = unlines
 ["2007/01/28 coopportunity"
 ,"    expenses:food:groceries                   $47.18"
 ,"    assets:checking"
 ,""
 ]

entry1 =
    LedgerTransaction (parsedate "2007/01/28") Nothing False "" "coopportunity" ""
     [Posting False "expenses:food:groceries" (Mixed [dollars 47.18]) "" RegularPosting, 
      Posting False "assets:checking" (Mixed [dollars (-47.18)]) "" RegularPosting] ""


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

ledger1_str = unlines
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

ledger2_str = unlines
 [";comment"
 ,"2007/01/27 * joes diner"
 ,"  expenses:food:dining                    $10.00"
 ,"  assets:checking                        $-47.18"
 ,""
 ]

ledger3_str = unlines
 ["2007/01/27 * joes diner"
 ,"  expenses:food:dining                    $10.00"
 ,";intra-entry comment"
 ,"  assets:checking                        $-47.18"
 ,""
 ]

ledger4_str = unlines
 ["!include \"somefile\""
 ,"2007/01/27 * joes diner"
 ,"  expenses:food:dining                    $10.00"
 ,"  assets:checking                        $-47.18"
 ,""
 ]

ledger5_str = ""

ledger6_str = unlines
 ["~ monthly from 2007/1/21"
 ,"    expenses:entertainment  $16.23        ;netflix"
 ,"    assets:checking"
 ,""
 ,"; 2007/01/01 * opening balance"
 ,";     assets:saving                            $200.04"
 ,";     equity:opening balances                         "
 ,""
 ]

ledger7_str = unlines
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
           LedgerTransaction {
             ltdate=parsedate "2007/01/01", 
             lteffectivedate=Nothing,
             ltstatus=False, 
             ltcode="*", 
             ltdescription="opening balance", 
             ltcomment="",
             ltpostings=[
              Posting {
                pstatus=False,
                paccount="assets:cash", 
                pamount=(Mixed [dollars 4.82]),
                pcomment="",
                ptype=RegularPosting
              },
              Posting {
                pstatus=False,
                paccount="equity:opening balances", 
                pamount=(Mixed [dollars (-4.82)]),
                pcomment="",
                ptype=RegularPosting
              }
             ],
             ltpreceding_comment_lines=""
           }
          ,
           LedgerTransaction {
             ltdate=parsedate "2007/02/01", 
             lteffectivedate=Nothing,
             ltstatus=False, 
             ltcode="*", 
             ltdescription="ayres suites", 
             ltcomment="",
             ltpostings=[
              Posting {
                pstatus=False,
                paccount="expenses:vacation", 
                pamount=(Mixed [dollars 179.92]),
                pcomment="",
                ptype=RegularPosting
              },
              Posting {
                pstatus=False,
                paccount="assets:checking", 
                pamount=(Mixed [dollars (-179.92)]),
                pcomment="",
                ptype=RegularPosting
              }
             ],
             ltpreceding_comment_lines=""
           }
          ,
           LedgerTransaction {
             ltdate=parsedate "2007/01/02", 
             lteffectivedate=Nothing,
             ltstatus=False, 
             ltcode="*", 
             ltdescription="auto transfer to savings", 
             ltcomment="",
             ltpostings=[
              Posting {
                pstatus=False,
                paccount="assets:saving", 
                pamount=(Mixed [dollars 200]),
                pcomment="",
                ptype=RegularPosting
              },
              Posting {
                pstatus=False,
                paccount="assets:checking", 
                pamount=(Mixed [dollars (-200)]),
                pcomment="",
                ptype=RegularPosting
              }
             ],
             ltpreceding_comment_lines=""
           }
          ,
           LedgerTransaction {
             ltdate=parsedate "2007/01/03", 
             lteffectivedate=Nothing,
             ltstatus=False, 
             ltcode="*", 
             ltdescription="poquito mas", 
             ltcomment="",
             ltpostings=[
              Posting {
                pstatus=False,
                paccount="expenses:food:dining", 
                pamount=(Mixed [dollars 4.82]),
                pcomment="",
                ptype=RegularPosting
              },
              Posting {
                pstatus=False,
                paccount="assets:cash", 
                pamount=(Mixed [dollars (-4.82)]),
                pcomment="",
                ptype=RegularPosting
              }
             ],
             ltpreceding_comment_lines=""
           }
          ,
           LedgerTransaction {
             ltdate=parsedate "2007/01/03", 
             lteffectivedate=Nothing,
             ltstatus=False, 
             ltcode="*", 
             ltdescription="verizon", 
             ltcomment="",
             ltpostings=[
              Posting {
                pstatus=False,
                paccount="expenses:phone", 
                pamount=(Mixed [dollars 95.11]),
                pcomment="",
                ptype=RegularPosting
              },
              Posting {
                pstatus=False,
                paccount="assets:checking", 
                pamount=(Mixed [dollars (-95.11)]),
                pcomment="",
                ptype=RegularPosting
              }
             ],
             ltpreceding_comment_lines=""
           }
          ,
           LedgerTransaction {
             ltdate=parsedate "2007/01/03", 
             lteffectivedate=Nothing,
             ltstatus=False, 
             ltcode="*", 
             ltdescription="discover", 
             ltcomment="",
             ltpostings=[
              Posting {
                pstatus=False,
                paccount="liabilities:credit cards:discover", 
                pamount=(Mixed [dollars 80]),
                pcomment="",
                ptype=RegularPosting
              },
              Posting {
                pstatus=False,
                paccount="assets:checking", 
                pamount=(Mixed [dollars (-80)]),
                pcomment="",
                ptype=RegularPosting
              }
             ],
             ltpreceding_comment_lines=""
           }
          ] 
          []
          []
          ""
          ""
          (TOD 0 0)

ledger7 = cacheLedger [] journal7 

ledger8_str = unlines
 ["2008/1/1 test           "
 ,"  a:b          10h @ $40"
 ,"  c:d                   "
 ,""
 ]

timelogentry1_str  = "i 2007/03/11 16:19:00 hledger\n"
timelogentry1 = TimeLogEntry In (parsedatetime "2007/03/11 16:19:00") "hledger"

timelogentry2_str  = "o 2007/03/11 16:30:00\n"
timelogentry2 = TimeLogEntry Out (parsedatetime "2007/03/11 16:30:00") ""

price1_str = "P 2004/05/01 XYZ $55.00\n"
price1 = HistoricalPrice (parsedate "2004/05/01") "XYZ" $ Mixed [dollars 55]

a1 = Mixed [(hours 1){price=Just $ Mixed [Amount (comm "$") 10 Nothing]}]
a2 = Mixed [(hours 2){price=Just $ Mixed [Amount (comm "EUR") 10 Nothing]}]
a3 = Mixed $ amounts a1 ++ amounts a2

journalWithAmounts :: [String] -> Journal
journalWithAmounts as = 
        Journal 
        [] 
        [] 
        [nullledgertxn{ltdescription=a,ltpostings=[nullrawposting{pamount=parse a}]} | a <- as]
        []
        []
        ""
        ""
        (TOD 0 0)
    where parse = fromparse . parseWithCtx emptyCtx postingamount . (" "++)

