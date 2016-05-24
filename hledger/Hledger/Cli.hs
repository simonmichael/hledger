{-|

Hledger.Cli re-exports the options, utilities and commands provided by
the hledger command-line program. This module also aggregates the
built-in unit tests defined throughout hledger and hledger-lib, and
adds some more which are easier to define here.

-}

{-# LANGUAGE OverloadedStrings #-}

module Hledger.Cli (
                     module Hledger.Cli.Accounts,
                     module Hledger.Cli.Add,
                     module Hledger.Cli.Balance,
                     module Hledger.Cli.Balancesheet,
                     module Hledger.Cli.Cashflow,
                     module Hledger.Cli.Help,
                     module Hledger.Cli.Histogram,
                     module Hledger.Cli.Incomestatement,
                     module Hledger.Cli.Info,
                     module Hledger.Cli.Man,
                     module Hledger.Cli.Print,
                     module Hledger.Cli.Register,
                     module Hledger.Cli.Stats,
                     module Hledger.Cli.CliOptions,
                     module Hledger.Cli.DocFiles,
                     module Hledger.Cli.Utils,
                     module Hledger.Cli.Version,
                     tests_Hledger_Cli,
                     module Hledger,
                     module System.Console.CmdArgs.Explicit
              )
where
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Calendar
import System.Console.CmdArgs.Explicit
import Test.HUnit

import Hledger
import Hledger.Cli.Accounts
import Hledger.Cli.Add
import Hledger.Cli.Balance
import Hledger.Cli.Balancesheet
import Hledger.Cli.Cashflow
import Hledger.Cli.Histogram
import Hledger.Cli.Help
import Hledger.Cli.Incomestatement
import Hledger.Cli.Info
import Hledger.Cli.Man
import Hledger.Cli.Print
import Hledger.Cli.Register
import Hledger.Cli.Stats
import Hledger.Cli.CliOptions
import Hledger.Cli.DocFiles
import Hledger.Cli.Utils
import Hledger.Cli.Version


tests_Hledger_Cli :: Test
tests_Hledger_Cli = TestList
 [
    tests_Hledger
   -- ,tests_Hledger_Cli_Add
   ,tests_Hledger_Cli_Balance
   ,tests_Hledger_Cli_Balancesheet
   ,tests_Hledger_Cli_Cashflow
   -- ,tests_Hledger_Cli_Histogram
   ,tests_Hledger_Cli_Incomestatement
   ,tests_Hledger_Cli_CliOptions
   -- ,tests_Hledger_Cli_Print
   ,tests_Hledger_Cli_Register
   -- ,tests_Hledger_Cli_Stats


   ,"apply account directive" ~:
   let ignoresourcepos j = j{jtxns=map (\t -> t{tsourcepos=nullsourcepos}) (jtxns j)} in
   let sameParse str1 str2 = do j1 <- readJournal Nothing Nothing True Nothing str1 >>= either error' (return . ignoresourcepos)
                                j2 <- readJournal Nothing Nothing True Nothing str2 >>= either error' (return . ignoresourcepos)
                                j1 `is` j2{jlastreadtime=jlastreadtime j1, jfiles=jfiles j1} --, jparsestate=jparsestate j1}
   in TestList
   [
    "apply account directive 1" ~: sameParse
                           ("2008/12/07 One\n  alpha  $-1\n  beta  $1\n" <>
                            "apply account outer\n2008/12/07 Two\n  aigh  $-2\n  bee  $2\n" <>
                            "apply account inner\n2008/12/07 Three\n  gamma  $-3\n  delta  $3\n" <>
                            "end apply account\n2008/12/07 Four\n  why  $-4\n  zed  $4\n" <>
                            "end apply account\n2008/12/07 Five\n  foo  $-5\n  bar  $5\n"
                           )
                           ("2008/12/07 One\n  alpha  $-1\n  beta  $1\n" <>
                            "2008/12/07 Two\n  outer:aigh  $-2\n  outer:bee  $2\n" <>
                            "2008/12/07 Three\n  outer:inner:gamma  $-3\n  outer:inner:delta  $3\n" <>
                            "2008/12/07 Four\n  outer:why  $-4\n  outer:zed  $4\n" <>
                            "2008/12/07 Five\n  foo  $-5\n  bar  $5\n"
                           )

   ,"apply account directive should preserve \"virtual\" posting type" ~: do
      j <- readJournal Nothing Nothing True Nothing "apply account test\n2008/12/07 One\n  (from)  $-1\n  (to)  $1\n" >>= either error' return
      let p = head $ tpostings $ head $ jtxns j
      assertBool "" $ paccount p == "test:from"
      assertBool "" $ ptype p == VirtualPosting

   ]

   ,"account aliases" ~: do
      j <- readJournal Nothing Nothing True Nothing "!alias expenses = equity:draw:personal\n1/1\n (expenses:food)  1\n" >>= either error' return
      let p = head $ tpostings $ head $ jtxns j
      assertBool "" $ paccount p == "equity:draw:personal:food"

  ,"ledgerAccountNames" ~:
    ledgerAccountNames ledger7 `is`
     ["assets","assets:cash","assets:checking","assets:saving","equity","equity:opening balances",
      "expenses","expenses:food","expenses:food:dining","expenses:phone","expenses:vacation",
      "liabilities","liabilities:credit cards","liabilities:credit cards:discover"]

  -- ,"journalCanonicaliseAmounts" ~:
  --  "use the greatest precision" ~:
  --   (map asprecision $ journalAmountAndPriceCommodities $ journalCanonicaliseAmounts $ journalWithAmounts ["1","2.00"]) `is` [2,2]

  -- don't know what this should do
  -- ,"elideAccountName" ~: do
  --    (elideAccountName 50 "aaaaaaaaaaaaaaaaaaaa:aaaaaaaaaaaaaaaaaaaa:aaaaaaaaaaaaaaaaaaaa"
  --     `is` "aa:aaaaaaaaaaaaaaaaaaaa:aaaaaaaaaaaaaaaaaaaa")
  --    (elideAccountName 20 "aaaaaaaaaaaaaaaaaaaa:aaaaaaaaaaaaaaaaaaaa:aaaaaaaaaaaaaaaaaaaa"
  --     `is` "aa:aa:aaaaaaaaaaaaaa")

  ,"default year" ~: do
    j <- readJournal Nothing Nothing True Nothing defaultyear_journal_txt >>= either error' return
    tdate (head $ jtxns j) `is` fromGregorian 2009 1 1
    return ()

  ,"show dollars" ~: showAmount (usd 1) ~?= "$1.00"

  ,"show hours" ~: showAmount (hrs 1) ~?= "1.00h"

 ]


-- fixtures/test data

-- date1 = parsedate "2008/11/26"
-- t1 = LocalTime date1 midday

{-
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
-}

defaultyear_journal_txt :: Text
defaultyear_journal_txt = T.unlines
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

journal7 :: Journal
journal7 = nulljournal {jtxns =
          [
           txnTieKnot Transaction {
             tindex=0,
             tsourcepos=nullsourcepos,
             tdate=parsedate "2007/01/01",
             tdate2=Nothing,
             tstatus=Uncleared,
             tcode="*",
             tdescription="opening balance",
             tcomment="",
             ttags=[],
             tpostings=
                 ["assets:cash" `post` usd 4.82
                 ,"equity:opening balances" `post` usd (-4.82)
                 ],
             tpreceding_comment_lines=""
           }
          ,
           txnTieKnot Transaction {
             tindex=0,
             tsourcepos=nullsourcepos,
             tdate=parsedate "2007/02/01",
             tdate2=Nothing,
             tstatus=Uncleared,
             tcode="*",
             tdescription="ayres suites",
             tcomment="",
             ttags=[],
             tpostings=
                 ["expenses:vacation" `post` usd 179.92
                 ,"assets:checking" `post` usd (-179.92)
                 ],
             tpreceding_comment_lines=""
           }
          ,
           txnTieKnot Transaction {
             tindex=0,
             tsourcepos=nullsourcepos,
             tdate=parsedate "2007/01/02",
             tdate2=Nothing,
             tstatus=Uncleared,
             tcode="*",
             tdescription="auto transfer to savings",
             tcomment="",
             ttags=[],
             tpostings=
                 ["assets:saving" `post` usd 200
                 ,"assets:checking" `post` usd (-200)
                 ],
             tpreceding_comment_lines=""
           }
          ,
           txnTieKnot Transaction {
             tindex=0,
             tsourcepos=nullsourcepos,
             tdate=parsedate "2007/01/03",
             tdate2=Nothing,
             tstatus=Uncleared,
             tcode="*",
             tdescription="poquito mas",
             tcomment="",
             ttags=[],
             tpostings=
                 ["expenses:food:dining" `post` usd 4.82
                 ,"assets:cash" `post` usd (-4.82)
                 ],
             tpreceding_comment_lines=""
           }
          ,
           txnTieKnot Transaction {
             tindex=0,
             tsourcepos=nullsourcepos,
             tdate=parsedate "2007/01/03",
             tdate2=Nothing,
             tstatus=Uncleared,
             tcode="*",
             tdescription="verizon",
             tcomment="",
             ttags=[],
             tpostings=
                 ["expenses:phone" `post` usd 95.11
                 ,"assets:checking" `post` usd (-95.11)
                 ],
             tpreceding_comment_lines=""
           }
          ,
           txnTieKnot Transaction {
             tindex=0,
             tsourcepos=nullsourcepos,
             tdate=parsedate "2007/01/03",
             tdate2=Nothing,
             tstatus=Uncleared,
             tcode="*",
             tdescription="discover",
             tcomment="",
             ttags=[],
             tpostings=
                 ["liabilities:credit cards:discover" `post` usd 80
                 ,"assets:checking" `post` usd (-80)
                 ],
             tpreceding_comment_lines=""
           }
          ]
         }

ledger7 :: Ledger
ledger7 = ledgerFromJournal Any journal7
