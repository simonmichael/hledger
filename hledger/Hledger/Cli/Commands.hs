{-|
hledger's built-in commands, and helpers for printing the commands list.
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Hledger.Cli.Commands (
   findCommand
  ,builtinCommands
  ,builtinCommandNames
  ,printCommandsList
  ,tests_Hledger_Cli_Commands
  ,module Hledger.Cli.Commands.Accounts
  ,module Hledger.Cli.Commands.Activity
  ,module Hledger.Cli.Commands.Add
  ,module Hledger.Cli.Commands.Balance
  ,module Hledger.Cli.Commands.Balancesheet
  ,module Hledger.Cli.Commands.Balancesheetequity
  ,module Hledger.Cli.Commands.Cashflow
  ,module Hledger.Cli.Commands.Checkdates
  ,module Hledger.Cli.Commands.Checkdupes
  ,module Hledger.Cli.Commands.Equity
  ,module Hledger.Cli.Commands.Help
  ,module Hledger.Cli.Commands.Incomestatement
  ,module Hledger.Cli.Commands.Prices
  ,module Hledger.Cli.Commands.Print
  ,module Hledger.Cli.Commands.Printunique
  ,module Hledger.Cli.Commands.Register
  ,module Hledger.Cli.Commands.Registermatch
  ,module Hledger.Cli.Commands.Stats
) 
where

import Control.Monad
import Data.List
import Data.List.Split (splitOn)
import Data.Monoid ((<>))
import Data.String.Here
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Calendar
import System.Console.CmdArgs.Explicit as C
import System.Exit
import Test.HUnit

import Hledger
import Hledger.Cli.CliOptions
import Hledger.Cli.Commands.Accounts
import Hledger.Cli.Commands.Activity
import Hledger.Cli.Commands.Add
import Hledger.Cli.Commands.Balance
import Hledger.Cli.Commands.Balancesheet
import Hledger.Cli.Commands.Balancesheetequity
import Hledger.Cli.Commands.Cashflow
import Hledger.Cli.Commands.Checkdates
import Hledger.Cli.Commands.Checkdupes
import Hledger.Cli.Commands.Equity
import Hledger.Cli.Commands.Help
import Hledger.Cli.Commands.Incomestatement
import Hledger.Cli.Commands.Prices
import Hledger.Cli.Commands.Print
import Hledger.Cli.Commands.Printunique
import Hledger.Cli.Commands.Register
import Hledger.Cli.Commands.Registermatch
import Hledger.Cli.Commands.Stats


-- | The cmdargs subcommand mode and IO action for each builtin command.
-- Command actions take parsed CLI options and a (lazy) finalised journal.
builtinCommands :: [(Mode RawOpts, CliOpts -> Journal -> IO ())]
builtinCommands = [
   (accountsmode           , accounts) 
  ,(activitymode           , activity) 
  ,(addmode                , add) 
  ,(balancemode            , balance) 
  ,(balancesheetmode       , balancesheet) 
  ,(balancesheetequitymode , balancesheetequity) 
  ,(cashflowmode           , cashflow) 
  ,(checkdatesmode         , checkdates) 
  ,(helpmode               , help') 
  ,(incomestatementmode    , incomestatement) 
  ,(printmode              , print') 
  ,(registermode           , register) 
  ,(statsmode              , stats) 
  ,(testmode               , testcmd) 
  ]

-- | All names and aliases of builtin commands.
builtinCommandNames :: [String]
builtinCommandNames = concatMap (modeNames . fst) builtinCommands

-- | Look up a builtin command's mode and action by exact command name or alias. 
findCommand :: String -> Maybe (Mode RawOpts, CliOpts -> Journal -> IO ()) 
findCommand cmdname = find (elem cmdname . modeNames . fst) builtinCommands 

-- | A template for the commands list, containing entries (indented lines)
-- for all currently known builtin and addon commands.
-- These will be filtered based on the commands found at runtime,  
-- except those beginning with "hledger", which are not filtered. 
-- OTHERCMDS is replaced with an entry for each unknown addon command found. 
-- COUNT is replaced with the number of commands found.
--  
-- The command descriptions here should be kept synced with 
-- each command's builtin help and with hledger manual's command list.
--
commandsListTemplate :: String
commandsListTemplate = [here|Commands available (COUNT):

Standard reports:
 accounts             show chart of accounts
 balancesheet (bs)    show a balance sheet
 balancesheetequity (bse)    show a balance sheet with equity
 cashflow (cf)        show a cashflow statement
 incomestatement (is) show an income statement
 transactions (txns)  show transactions in some account

General reporting:
 activity             show a bar chart of posting counts per interval
 balance (bal)        show accounts and balances
 budget               add automated postings/txns/bucket accts (experimental)
 chart                generate simple balance pie charts (experimental)
 check                check more powerful balance assertions
 check-dates          check transactions are ordered by date
 check-dupes          check for accounts with the same leaf name
 irr                  calculate internal rate of return of an investment
 prices               show market price records
 print                show transaction journal entries
 print-unique         show only transactions with unique descriptions
 register (reg)       show postings and running total
 register-match       show best matching transaction for a description
 stats                show some journal statistics

Interfaces:
 add                  console ui for adding transactions
 api                  web api server
 iadd                 curses ui for adding transactions
 ui                   curses ui
 web                  web ui

Misc:
 autosync             download/deduplicate/convert OFX data
 equity               generate transactions to zero & restore account balances
 interest             generate interest transactions
 rewrite              add automated postings to certain transactions
 test                 run some self tests
OTHERCMDS
Help:
 help                 show any of the hledger manuals in various formats
 hledger CMD -h       show command usage
 hledger -h           show general usage
|]

-- | Print the commands list, modifying the template above based on
-- the currently available addons. Missing addons will be removed, and
-- extra addons will be added under Misc.
printCommandsList :: [String] -> IO ()
printCommandsList addonsFound = putStr commandsList
  where
    commandsFound = builtinCommandNames ++ addonsFound
    unknownCommandsFound = addonsFound \\ knownCommands

    adjustline l | " hledger " `isPrefixOf` l = [l]
    adjustline (' ':l) | not $ w `elem` commandsFound = []
      where w = takeWhile (not . (`elem` ['|',' '])) l
    adjustline l = [l]

    commandsList1 =
      regexReplace "OTHERCMDS" (unlines [' ':w | w <- unknownCommandsFound]) $
      unlines $ concatMap adjustline $ lines commandsListTemplate

    commandsList =
      regexReplace "COUNT" (show $ length $ commandsFromCommandsList commandsList1)
      commandsList1

knownCommands :: [String]
knownCommands = sort $ commandsFromCommandsList commandsListTemplate

-- | Extract the command names from a commands list like the above:
-- the first word (or words separated by |) of lines beginning with a space.
commandsFromCommandsList :: String -> [String]
commandsFromCommandsList s = concatMap (splitOn "|") [w | ' ':l <- lines s, let w:_ = words l]



-- The test command, defined here so it can access other commands' tests.

testmode = (defCommandMode ["test"]) {
  modeHelp = "run built-in self-tests"
 ,modeArgs = ([], Just $ argsFlag "[REGEXPS]")
 ,modeGroupFlags = Group {
     groupUnnamed = []
    ,groupHidden = [
        flagNone ["tree"] (\opts -> setboolopt "tree" opts) "show tests hierarchically"
       ,flagNone ["flat"] (\opts -> setboolopt "flat" opts) "show tests as a flat list"
      ]
    ,groupNamed = [generalflagsgroup3]
    }
 }

-- | Run some or all hledger-lib and hledger unit tests, and exit with success or failure.
testcmd :: CliOpts -> Journal -> IO ()
testcmd opts _ = do
  let ts = 
        (if tree_ $ reportopts_ opts then matchedTestsTree else matchedTestsFlat) 
          opts tests_Hledger_Cli_Commands
  results <- liftM (fst . flip (,) 0) $ runTestTT ts
  if errors results > 0 || failures results > 0
    then exitFailure
    else exitWith ExitSuccess

-- | All or pattern-matched tests, as a flat list to show simple names.
matchedTestsFlat opts = TestList . 
  filter (matchesAccount (queryFromOpts nulldate $ reportopts_ opts) . T.pack . testName) . 
  flattenTests

-- | All or pattern-matched tests, in the original suites to show hierarchical names.
matchedTestsTree opts = 
  filterTests (matchesAccount (queryFromOpts nulldate $ reportopts_ opts) . T.pack . testName) 


-- collected hledger-lib + hledger unit tests

tests_Hledger_Cli_Commands :: Test
tests_Hledger_Cli_Commands = TestList [
   tests_Hledger
  ,tests_Hledger_Cli_CliOptions
  -- ,tests_Hledger_Cli_Commands_Activity
  -- ,tests_Hledger_Cli_Commands_Add
  ,tests_Hledger_Cli_Commands_Balance
  ,tests_Hledger_Cli_Commands_Balancesheet
  ,tests_Hledger_Cli_Commands_Cashflow
  ,tests_Hledger_Cli_Commands_Incomestatement
  ,tests_Hledger_Cli_Commands_Print
  ,tests_Hledger_Cli_Commands_Register
  -- ,tests_Hledger_Cli_Commands_Stats

  -- some more tests easiest to define here:
  
  ,"apply account directive" ~: 
    let ignoresourcepos j = j{jtxns=map (\t -> t{tsourcepos=nullsourcepos}) (jtxns j)} in
    let sameParse str1 str2 = do j1 <- readJournal Nothing Nothing True Nothing str1 >>= either error' (return . ignoresourcepos)
                                 j2 <- readJournal Nothing Nothing True Nothing str2 >>= either error' (return . ignoresourcepos)
                                 j1 `is` j2{jlastreadtime=jlastreadtime j1, jfiles=jfiles j1} --, jparsestate=jparsestate j1}
    in sameParse
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


-- test data

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
             tstatus=Unmarked,
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
             tstatus=Unmarked,
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
             tstatus=Unmarked,
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
             tstatus=Unmarked,
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
             tstatus=Unmarked,
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
             tstatus=Unmarked,
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
