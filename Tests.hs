-- see also
-- http://hackage.haskell.org/cgi-bin/hackage-scripts/package/test-framework
-- http://hackage.haskell.org/cgi-bin/hackage-scripts/package/HTF

module Tests
where
import qualified Data.Map as Map
import Data.Time.Format
import System.Locale (defaultTimeLocale)
import Text.ParserCombinators.Parsec
import Test.HUnit
import Test.HUnit.Tools (assertRaises, runVerboseTests)
import Ledger
import Utils
import Options
import BalanceCommand
import PrintCommand
import RegisterCommand


runtests opts args = runner flattests
    where
      runner | (Verbose `elem` opts) = runVerboseTests
             | otherwise = \t -> runTestTT t >>= return . (flip (,) 0)
      flattests = TestList $ filter matchname $ concatMap tflatten tests
      deeptests = tfilter matchname $ TestList tests
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
a `is` e = assertEqual "" a e

-- | Assert a parse result is some expected value, or print a parse error.
parseis :: (Show a, Eq a) => (Either ParseError a) -> a -> Assertion
parse `parseis` expected = either printParseError (`is` expected) parse

------------------------------------------------------------------------------
-- | Tests for any function or topic. Mostly ordered by test name.
tests :: [Test]
tests = [

   "account directive" ~: 
   let sameParse str1 str2 = do l1 <- rawledgerfromstring str1
                                l2 <- rawledgerfromstring str2
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

  ,"accountnames" ~: do
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
    (sum [a2,a3]) `is` Amount (comm "$") (-2.46) Nothing
    (sum [a3,a3]) `is` Amount (comm "$") (-2.46) Nothing
    (sum [a1,a2,a3,-a3]) `is` Amount (comm "$") 0 Nothing

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

   ,"balance report level can also be limited with --depth" ~:
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
    ["                  $1  expenses:food"
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
    ,"                  $1  liabilities"
    ,"--------------------"
    ,"                 $-1"
    ]

   ,"balance report with account pattern e" ~:
    ([], ["e"]) `gives`
    ["                 $-1  assets"
    ,"                  $2  expenses"
    ,"                  $1    supplies"
    ,"                 $-2  income"
    ,"                  $1  liabilities:debts"
    ]

   ,"balance report with unmatched parent of two matched subaccounts" ~: 
    ([], ["cash","saving"]) `gives`
    ["                  $1  assets:bank:saving"
    ,"                 $-2  assets:cash"
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
    ,"                 $-2  income"
    ,"                  $1  liabilities"
    ,"--------------------"
    ,"                  $1"
    ]

   ,"balance report negative account pattern always matches full name" ~: 
    ([], ["not:e"]) `gives` []

   ,"balance report negative patterns affect totals" ~: 
    ([], ["expenses","not:food"]) `gives`
    ["                  $1  expenses"
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
      rl <- rawledgerfromstring $ unlines
             [""
             ,"2008/1/1 test           "
             ,"  a:b          10h @ $50"
             ,"  c:d                   "
             ,""
             ]
      let l = cacheLedger [] $ 
              filterRawLedger (DateSpan Nothing Nothing) [] False False $ 
              canonicaliseAmounts True rl -- enable cost basis adjustment            
      showBalanceReport [] [] l `is` 
       unlines
        ["                $500  a"
        ,"               $-500  c"
        ]

   ,"balance report elides zero-balance root account(s)" ~: do
      l <- ledgerfromstringwithopts [] [] sampletime
             (unlines
              ["2008/1/1 one"
              ,"  test:a  1"
              ,"  test:b"
              ])
      showBalanceReport [] [] l `is` ""
      showBalanceReport [SubTotal] [] l `is`
       unlines
        ["                1  test:a"
        ,"               -1  test:b"
        ]

   ]

  ,"balanceEntry" ~: do
     let fromeither (Left err) = error err
         fromeither (Right e) = e
     (tamount $ last $ etransactions $ fromeither $ balanceEntry entry1) `is` Mixed [dollars (-47.18)]
     assertBool "detect unbalanced entry, sign error"
                    (isLeft $ balanceEntry
                           (Entry (parsedate "2007/01/28") False "" "test" ""
                            [RawTransaction False "a" (Mixed [dollars 1]) "" RegularTransaction, 
                             RawTransaction False "b" (Mixed [dollars 1]) "" RegularTransaction
                            ] ""))
     assertBool "detect unbalanced entry, multiple missing amounts"
                    (isLeft $ balanceEntry
                           (Entry (parsedate "2007/01/28") False "" "test" ""
                            [RawTransaction False "a" missingamt "" RegularTransaction, 
                             RawTransaction False "b" missingamt "" RegularTransaction
                            ] ""))
     assertBool "one missing amount should be ok"
                    (isRight $ balanceEntry
                           (Entry (parsedate "2007/01/28") False "" "test" ""
                            [RawTransaction False "a" (Mixed [dollars 1]) "" RegularTransaction, 
                             RawTransaction False "b" missingamt "" RegularTransaction
                            ] ""))

  ,"balancereportacctnames" ~: 
   let gives (opt,pats) e = do 
         l <- sampleledger
         let t = pruneZeroBalanceLeaves $ ledgerAccountTree 999 l
         balancereportacctnames l (opt=="-s") pats t `is` e
   in TestList
   [
    "balancereportacctnames 0" ~: ("-s",[])              `gives` ["assets","assets:bank","assets:bank:checking","assets:bank:saving",
                                                                  "assets:cash","expenses","expenses:food","expenses:supplies","income",
                                                                  "income:gifts","income:salary","liabilities","liabilities:debts"]
   ,"balancereportacctnames 1" ~: ("",  [])              `gives` ["assets","expenses","income","liabilities"]
   ,"balancereportacctnames 2" ~: ("",  ["assets"])      `gives` ["assets"]
   ,"balancereportacctnames 3" ~: ("",  ["as"])          `gives` ["assets","assets:cash"]
   ,"balancereportacctnames 4" ~: ("",  ["assets:cash"]) `gives` ["assets:cash"]
   ,"balancereportacctnames 5" ~: ("",  ["^assets"])     `gives` ["expenses","income","liabilities"]
   ,"balancereportacctnames 6" ~: ("",  ["^e"])          `gives` []
   ,"balancereportacctnames 7" ~: ("-s",["assets"])      `gives` ["assets","assets:bank","assets:bank:checking","assets:bank:saving","assets:cash"]
   ,"balancereportacctnames 8" ~: ("-s",["^e"])          `gives` []
   ]

  ,"cacheLedger" ~: do
    (length $ Map.keys $ accountmap $ cacheLedger [] rawledger7) `is` 15

  ,"canonicaliseAmounts" ~:
   "use the greatest precision" ~: do
    (rawLedgerPrecisions $ canonicaliseAmounts False $ rawLedgerWithAmounts ["1","2.00"]) `is` [2,2]

  ,"dateSpanFromOpts" ~: do
    let todaysdate = parsedate "2008/11/26"
    let opts `gives` spans = show (dateSpanFromOpts todaysdate opts) `is` spans
    [] `gives` "DateSpan Nothing Nothing"
    [Begin "2008", End "2009"] `gives` "DateSpan (Just 2008-01-01) (Just 2009-01-01)"
    [Period "in 2008"] `gives` "DateSpan (Just 2008-01-01) (Just 2009-01-01)"
    [Begin "2005", End "2007",Period "in 2008"] `gives` "DateSpan (Just 2008-01-01) (Just 2009-01-01)"

  ,"entriesFromTimeLogEntries" ~: do
     today <- getCurrentDay
     let
         clockin t a = TimeLogEntry 'i' t a
         clockout t = TimeLogEntry 'o' t ""
         yesterday = prevday today
         mktime d s = LocalTime d $ fromMaybe midnight $ parseTime defaultTimeLocale "%H:%M:%S" s
         noon = LocalTime today midday
         ts `gives` ss = (map edescription $ entriesFromTimeLogEntries noon ts) `is` ss
     [] `gives` []
     [clockin (mktime today "00:00:00") ""] `gives` ["00:00-12:00"]
     [clockin (mktime yesterday "23:00:00") ""] `gives` ["23:00-23:59","00:00-12:00"]
     [clockin (mktime (addDays (-2) today) "23:00:00") ""] `gives` ["23:00-23:59","00:00-23:59","00:00-12:00"]

  ,"expandAccountNames" ~: do
    expandAccountNames ["assets:cash","assets:checking","expenses:vacation"] `is`
     ["assets","assets:cash","assets:checking","expenses","expenses:vacation"]

  ,"intervalFromOpts" ~: do
    let opts `gives` interval = intervalFromOpts opts `is` interval
    [] `gives` NoInterval
    [WeeklyOpt] `gives` Weekly
    [MonthlyOpt] `gives` Monthly
    [YearlyOpt] `gives` Yearly
    [Period "weekly"] `gives` Weekly
    [Period "monthly"] `gives` Monthly
    [WeeklyOpt, Period "yearly"] `gives` Yearly

  ,"isAccountNamePrefixOf" ~: do
    "assets" `isAccountNamePrefixOf` "assets:bank" `is` True
    "assets" `isAccountNamePrefixOf` "assets:bank:checking" `is` True
    "my assets" `isAccountNamePrefixOf` "assets:bank" `is` False

  ,"isSubAccountNameOf" ~: do
    "assets:bank" `isSubAccountNameOf` "assets" `is` True
    "assets:bank:checking" `isSubAccountNameOf` "assets" `is` False
    "assets:bank" `isSubAccountNameOf` "my assets" `is` False

  ,"default year" ~: do
    rl <- rawledgerfromstring defaultyear_ledger_str
    (edate $ head $ entries rl) `is` fromGregorian 2009 1 1
    return ()

  ,"ledgerEntry" ~: do
    parseWithCtx ledgerEntry entry1_str `parseis` entry1

  ,"ledgerHistoricalPrice" ~: do
    parseWithCtx ledgerHistoricalPrice price1_str `parseis` price1

  ,"ledgertransaction" ~: do
    parseWithCtx ledgertransaction rawtransaction1_str `parseis` rawtransaction1

  ,"parsedate" ~: do
    parsedate "2008/02/03" `is` parsetimewith "%Y/%m/%d" "2008/02/03" sampledate
    parsedate "2008-02-03" `is` parsetimewith "%Y/%m/%d" "2008/02/03" sampledate

  ,"period expressions" ~: do
    let todaysdate = parsedate "2008/11/26"
    let str `gives` result = (show $ parsewith (periodexpr todaysdate) str) `is` ("Right "++result)
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
    showEntries [] args l `is` unlines 
     ["2008/06/03 * eat & shop"
     ,"    expenses:food                                 $1"
     ,"    expenses:supplies                             $1"
     ,"    assets:cash                                  $-2"
     ,""
     ]

  , "print report with depth arg" ~:
   do 
    l <- sampleledger
    showEntries [Depth "2"] [] l `is` unlines
      ["2008/01/01 income"
      ,"    income:salary                                $-1"
      ,""
      ,"2008/06/01 gift"
      ,"    income:gifts                                 $-1"
      ,""
      ,"2008/06/03 * eat & shop"
      ,"    expenses:food                                 $1"
      ,"    expenses:supplies                             $1"
      ,"    assets:cash                                  $-2"
      ,""
      ,"2008/12/31 * pay off"
      ,"    liabilities:debts                             $1"
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

  ,"register report sorts by date" ~:
   do 
    l <- ledgerfromstringwithopts [] [] sampletime $ unlines
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
    let displayexpr `gives` dates = 
            registerdates (showRegisterReport [Display displayexpr] [] l) `is` dates
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

  ,"smart dates" ~: do
    let str `gives` datestr = fixSmartDateStr (parsedate "2008/11/26") str `is` datestr
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
    let (interval,span) `gives` spans = splitSpan interval span `is` spans
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
    (map aname $ subAccounts l a) `is` ["assets:bank","assets:cash"]

  ,"summariseTransactionsInDateSpan" ~: do
    let (b,e,entryno,depth,showempty,ts) `gives` summaryts = 
            summariseTransactionsInDateSpan (mkdatespan b e) entryno depth showempty ts `is` summaryts
    let ts =
            [
             nulltxn{description="desc",account="expenses:food:groceries",amount=Mixed [dollars 1]}
            ,nulltxn{description="desc",account="expenses:food:dining",   amount=Mixed [dollars 2]}
            ,nulltxn{description="desc",account="expenses:food",          amount=Mixed [dollars 4]}
            ,nulltxn{description="desc",account="expenses:food:dining",   amount=Mixed [dollars 8]}
            ]
    ("2008/01/01","2009/01/01",0,Nothing,False,[]) `gives` 
     []
    ("2008/01/01","2009/01/01",0,Nothing,True,[]) `gives` 
     [
      nulltxn{date=parsedate "2008/01/01",description="- 2008/12/31"}
     ]
    ("2008/01/01","2009/01/01",0,Nothing,False,ts) `gives` 
     [
      nulltxn{date=parsedate "2008/01/01",description="- 2008/12/31",account="expenses:food",          amount=Mixed [dollars 4]}
     ,nulltxn{date=parsedate "2008/01/01",description="- 2008/12/31",account="expenses:food:dining",   amount=Mixed [dollars 10]}
     ,nulltxn{date=parsedate "2008/01/01",description="- 2008/12/31",account="expenses:food:groceries",amount=Mixed [dollars 1]}
     ]
    ("2008/01/01","2009/01/01",0,Just 2,False,ts) `gives` 
     [
      nulltxn{date=parsedate "2008/01/01",description="- 2008/12/31",account="expenses:food",amount=Mixed [dollars 15]}
     ]
    ("2008/01/01","2009/01/01",0,Just 1,False,ts) `gives` 
     [
      nulltxn{date=parsedate "2008/01/01",description="- 2008/12/31",account="expenses",amount=Mixed [dollars 15]}
     ]
    ("2008/01/01","2009/01/01",0,Just 0,False,ts) `gives` 
     [
      nulltxn{date=parsedate "2008/01/01",description="- 2008/12/31",account="",amount=Mixed [dollars 15]}
     ]

  ,"timelog" ~: do
    parseWithCtx timelog timelog1_str `parseis` timelog1

  ,"transactionamount" ~: do
    parseWithCtx transactionamount " $47.18" `parseis` Mixed [dollars 47.18]
    parseWithCtx transactionamount " $1." `parseis` 
     Mixed [Amount (Commodity {symbol="$",side=L,spaced=False,comma=False,precision=0}) 1 Nothing]

  ]

  
------------------------------------------------------------------------------
-- test data

sampledate = parsedate "2008/11/26"
sampletime = LocalTime sampledate midday
sampleledger = ledgerfromstringwithopts [] [] sampletime sample_ledger_str
sampleledgerwithopts opts args = ledgerfromstringwithopts opts args sampletime sample_ledger_str

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

rawtransaction1_str  = "  expenses:food:dining  $10.00\n"

rawtransaction1 = RawTransaction "expenses:food:dining" (Mixed [dollars 10]) "" RegularTransaction

entry1_str = unlines
 ["2007/01/28 coopportunity"
 ,"  expenses:food:groceries                 $47.18"
 ,"  assets:checking"
 ,""
 ]

entry1 =
    (Entry (parsedate "2007/01/28") False "" "coopportunity" ""
     [RawTransaction "expenses:food:groceries" (Mixed [dollars 47.18]) "" RegularTransaction, 
      RawTransaction "assets:checking" (Mixed [dollars (-47.18)]) "" RegularTransaction] "")


entry2_str = unlines
 ["2007/01/27 * joes diner"
 ,"  expenses:food:dining                    $10.00"
 ,"  expenses:gifts                          $10.00"
 ,"  assets:checking                        $-20.00"
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

rawledger7 = RawLedger
          [] 
          [] 
          [
           Entry {
             edate= parsedate "2007/01/01", 
             estatus=False, 
             ecode="*", 
             edescription="opening balance", 
             ecomment="",
             etransactions=[
              RawTransaction {
                taccount="assets:cash", 
                tamount=(Mixed [dollars 4.82]),
                tcomment="",
                rttype=RegularTransaction
              },
              RawTransaction {
                taccount="equity:opening balances", 
                tamount=(Mixed [dollars (-4.82)]),
                tcomment="",
                rttype=RegularTransaction
              }
             ],
             epreceding_comment_lines=""
           }
          ,
           Entry {
             edate= parsedate "2007/02/01", 
             estatus=False, 
             ecode="*", 
             edescription="ayres suites", 
             ecomment="",
             etransactions=[
              RawTransaction {
                taccount="expenses:vacation", 
                tamount=(Mixed [dollars 179.92]),
                tcomment="",
                rttype=RegularTransaction
              },
              RawTransaction {
                taccount="assets:checking", 
                tamount=(Mixed [dollars (-179.92)]),
                tcomment="",
                rttype=RegularTransaction
              }
             ],
             epreceding_comment_lines=""
           }
          ,
           Entry {
             edate=parsedate "2007/01/02", 
             estatus=False, 
             ecode="*", 
             edescription="auto transfer to savings", 
             ecomment="",
             etransactions=[
              RawTransaction {
                taccount="assets:saving", 
                tamount=(Mixed [dollars 200]),
                tcomment="",
                rttype=RegularTransaction
              },
              RawTransaction {
                taccount="assets:checking", 
                tamount=(Mixed [dollars (-200)]),
                tcomment="",
                rttype=RegularTransaction
              }
             ],
             epreceding_comment_lines=""
           }
          ,
           Entry {
             edate=parsedate "2007/01/03", 
             estatus=False, 
             ecode="*", 
             edescription="poquito mas", 
             ecomment="",
             etransactions=[
              RawTransaction {
                taccount="expenses:food:dining", 
                tamount=(Mixed [dollars 4.82]),
                tcomment="",
                rttype=RegularTransaction
              },
              RawTransaction {
                taccount="assets:cash", 
                tamount=(Mixed [dollars (-4.82)]),
                tcomment="",
                rttype=RegularTransaction
              }
             ],
             epreceding_comment_lines=""
           }
          ,
           Entry {
             edate=parsedate "2007/01/03", 
             estatus=False, 
             ecode="*", 
             edescription="verizon", 
             ecomment="",
             etransactions=[
              RawTransaction {
                taccount="expenses:phone", 
                tamount=(Mixed [dollars 95.11]),
                tcomment="",
                rttype=RegularTransaction
              },
              RawTransaction {
                taccount="assets:checking", 
                tamount=(Mixed [dollars (-95.11)]),
                tcomment="",
                rttype=RegularTransaction
              }
             ],
             epreceding_comment_lines=""
           }
          ,
           Entry {
             edate=parsedate "2007/01/03", 
             estatus=False, 
             ecode="*", 
             edescription="discover", 
             ecomment="",
             etransactions=[
              RawTransaction {
                taccount="liabilities:credit cards:discover", 
                tamount=(Mixed [dollars 80]),
                tcomment="",
                rttype=RegularTransaction
              },
              RawTransaction {
                taccount="assets:checking", 
                tamount=(Mixed [dollars (-80)]),
                tcomment="",
                rttype=RegularTransaction
              }
             ],
             epreceding_comment_lines=""
           }
          ] 
          []
          []
          ""

ledger7 = cacheLedger [] rawledger7 

ledger8_str = unlines
 ["2008/1/1 test           "
 ,"  a:b          10h @ $40"
 ,"  c:d                   "
 ,""
 ]

timelogentry1_str  = "i 2007/03/11 16:19:00 hledger\n"
timelogentry1 = TimeLogEntry 'i' (parsedatetime "2007/03/11 16:19:00") "hledger"

timelogentry2_str  = "o 2007/03/11 16:30:00\n"
timelogentry2 = TimeLogEntry 'o' (parsedatetime "2007/03/11 16:30:00") ""

timelog1_str = concat [
                timelogentry1_str,
                timelogentry2_str
               ]
timelog1 = TimeLog [
            timelogentry1,
            timelogentry2
           ]

price1_str = "P 2004/05/01 XYZ $55\n"
price1 = HistoricalPrice (parsedate "2004/05/01") "XYZ" "$" 55

a1 = Mixed [(hours 1){price=Just $ Mixed [Amount (comm "$") 10 Nothing]}]
a2 = Mixed [(hours 2){price=Just $ Mixed [Amount (comm "EUR") 10 Nothing]}]
a3 = Mixed $ (amounts a1) ++ (amounts a2)

rawLedgerWithAmounts as = 
        RawLedger 
        [] 
        [] 
        [nullentry{edescription=a,etransactions=[nullrawtxn{tamount=parse a}]} | a <- as]
        []
        []
        ""
    where parse = fromparse . parseWithCtx transactionamount . (" "++)

