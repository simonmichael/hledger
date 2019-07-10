{-|
hledger's built-in commands, and helpers for printing the commands list.

New built-in commands should be added in four places below:
the export list, the import list, builtinCommands, commandsList.
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Hledger.Cli.Commands (
   findCommand
  ,testcmd
  ,builtinCommands
  ,builtinCommandNames
  ,printCommandsList
  ,tests_Commands
  ,module Hledger.Cli.Commands.Accounts
  ,module Hledger.Cli.Commands.Activity
  ,module Hledger.Cli.Commands.Add
  ,module Hledger.Cli.Commands.Balance
  ,module Hledger.Cli.Commands.Balancesheet
  ,module Hledger.Cli.Commands.Balancesheetequity
  ,module Hledger.Cli.Commands.Cashflow
  ,module Hledger.Cli.Commands.Checkdates
  ,module Hledger.Cli.Commands.Checkdupes
  ,module Hledger.Cli.Commands.Close
  ,module Hledger.Cli.Commands.Commodities
  ,module Hledger.Cli.Commands.Descriptions
  ,module Hledger.Cli.Commands.Diff
  ,module Hledger.Cli.Commands.Help
  ,module Hledger.Cli.Commands.Import
  ,module Hledger.Cli.Commands.Incomestatement
  ,module Hledger.Cli.Commands.Notes
  ,module Hledger.Cli.Commands.Payees
  ,module Hledger.Cli.Commands.Prices
  ,module Hledger.Cli.Commands.Print
  ,module Hledger.Cli.Commands.Printunique
  ,module Hledger.Cli.Commands.Register
  ,module Hledger.Cli.Commands.Registermatch
  ,module Hledger.Cli.Commands.Rewrite
  ,module Hledger.Cli.Commands.Stats
  ,module Hledger.Cli.Commands.Tags
) 
where

import Data.Char (isSpace)
import Data.Default
import Data.List
#if !(MIN_VERSION_base(4,11,0))
import Data.Monoid ((<>))
#endif
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Calendar
import qualified EasyTest
import System.Console.CmdArgs.Explicit as C
import System.Exit

import Hledger 
import Hledger.Cli.CliOptions
import Hledger.Cli.Version
import Hledger.Cli.Commands.Accounts
import Hledger.Cli.Commands.Activity
import Hledger.Cli.Commands.Add
import Hledger.Cli.Commands.Balance
import Hledger.Cli.Commands.Balancesheet
import Hledger.Cli.Commands.Balancesheetequity
import Hledger.Cli.Commands.Cashflow
import Hledger.Cli.Commands.Checkdates
import Hledger.Cli.Commands.Checkdupes
import Hledger.Cli.Commands.Close
import Hledger.Cli.Commands.Commodities
import Hledger.Cli.Commands.Descriptions
import Hledger.Cli.Commands.Diff
import Hledger.Cli.Commands.Files
import Hledger.Cli.Commands.Help
import Hledger.Cli.Commands.Import
import Hledger.Cli.Commands.Incomestatement
import Hledger.Cli.Commands.Notes
import Hledger.Cli.Commands.Payees
import Hledger.Cli.Commands.Prices
import Hledger.Cli.Commands.Print
import Hledger.Cli.Commands.Printunique
import Hledger.Cli.Commands.Register
import Hledger.Cli.Commands.Registermatch
import Hledger.Cli.Commands.Rewrite
import Hledger.Cli.Commands.Roi
import Hledger.Cli.Commands.Stats
import Hledger.Cli.Commands.Tags
import Hledger.Cli.Utils (tests_Cli_Utils)

-- | The cmdargs subcommand mode (for command-line parsing)
-- and IO action (for doing the command's work) for each builtin command.
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
  ,(checkdupesmode         , checkdupes)
  ,(closemode              , close)
  ,(commoditiesmode        , commodities)
  ,(descriptionsmode        , descriptions)
  ,(helpmode               , help')
  ,(importmode             , importcmd)
  ,(filesmode              , files)
  ,(diffmode               , diff)
  ,(incomestatementmode    , incomestatement)
  ,(notesmode              , notes)
  ,(payeesmode             , payees)
  ,(pricesmode             , prices)
  ,(printmode              , print')
  ,(printuniquemode        , printunique)
  ,(registermode           , register)
  ,(registermatchmode      , registermatch)
  ,(rewritemode            , rewrite)
  ,(roimode                , roi)
  ,(statsmode              , stats)
  ,(tagsmode               , tags)
  ,(testmode               , testcmd)
  ]

-- | The commands list, showing command names, standard aliases,
-- and short descriptions. This is modified at runtime, as follows:
--
-- PROGVERSION is replaced with the program name and version.
--
-- Lines beginning with a space represent builtin commands, with format:
--  COMMAND (ALIASES) DESCRIPTION
-- These should be kept synced with builtinCommands above, and
-- their docs (Commands/\*.md).
--
-- Lines beginning with + represent known addon commands. These lines
-- will be suppressed if hledger-CMD is not found in $PATH at runtime.
--
-- OTHER is replaced with additional command lines (without descriptions)
-- for any unknown addon commands found in $PATH at runtime.
--
-- TODO: generate more of this automatically.
-- 
commandsList :: String
commandsList = unlines [
   "-------------------------------------------------------------------------------"
  ,"PROGVERSION"
  ,"Usage: hledger COMMAND [OPTIONS] [-- ADDONCMDOPTIONS]"
  ,"Commands (+ addons found in $PATH):"
  ,""
  ,"Data entry (these commands modify the journal file):"
  ," add                      add transactions using guided prompts"
  ,"+iadd                     add transactions using curses ui"
  ," import                   add any new transactions from other files (eg csv)"
  ,""
  ,"Data management:"
  ,"+autosync                 download/deduplicate/convert OFX data"
  ,"+check                    check more powerful balance assertions"
  ," check-dates              check transactions are ordered by date"
  ," check-dupes              check for accounts with the same leaf name"
  ," close (equity)           generate balance-resetting transactions"
  ," diff                     compare account transactions in two journal files"
  ,"+interest                 generate interest transactions"
  ," rewrite                  generate automated postings/diffs (old, use --auto)"
  ,""
  ,"Financial reports:"
  ," balancesheet (bs)        show assets, liabilities and net worth"
  ," balancesheetequity (bse) show assets, liabilities and equity"
  ," cashflow (cf)            show changes in liquid assets"
  ," incomestatement (is)     show revenues and expenses"
  ,"+irr                      calculate internal rate of return (old, use roi)"
  ," roi                      show return on investments"
  ,""
  ,"Low-level reports:"
  ," accounts (a)             show account names"
  ," activity                 show postings-per-interval bar charts"
  ," balance (b, bal)         show balance changes/end balances/budgets in accounts"
  ," commodities              show commodity/currency symbols"
  ," descriptions             show unique transaction descriptions"
  ," files                    show input file paths"
  ," notes                    show unique note segments of transaction descriptions"
  ," payees                   show unique payee segments of transaction descriptions"
  ," prices                   show market price records"
  ," print (p, txns)          show transactions (journal entries)"
  ," print-unique             show only transactions with unique descriptions"
  ," register (r, reg)        show postings in one or more accounts & running total"
  ," register-match           show a recent posting that best matches a description"
  ," stats                    show journal statistics"
  ," tags                     show tag names"
  ," test                     run self tests"
  ,""
  ,"Alternate user interfaces:"
  ,"+ui                       run curses ui"
  ,"+web                      run web ui"
  ,"+api                      run http api server"
  ,""
  ,"Other:"
  ,"OTHER"
  ,"Help:"
  ," (no arguments)           show this commands list"
  ," -h                       show general flags"
  ," COMMAND -h               show flags & docs for COMMAND"
  ," help [MANUAL]            show hledger manuals in various formats"
  ,""
  ]
-- commands                 show brief commands list
-- edit                     open a text editor on some part of the journal
-- aregister (ar, areg)     show transactions in a single account


-- | All names and aliases of builtin commands.
builtinCommandNames :: [String]
builtinCommandNames = concatMap (modeNames . fst) builtinCommands

-- | Look up a builtin command's mode and action by exact command name or alias. 
findCommand :: String -> Maybe (Mode RawOpts, CliOpts -> Journal -> IO ()) 
findCommand cmdname = find (elem cmdname . modeNames . fst) builtinCommands 

-- | Extract the command names from commandsList: the first word
-- of lines beginning with a space or + sign.
commandsFromCommandsList :: String -> [String]
commandsFromCommandsList s =
  [w | c:l <- lines s, c `elem` [' ','+'], let w:_ = words l]

knownCommands :: [String]
knownCommands = sort $ commandsFromCommandsList commandsList

-- | Print the commands list, modifying the template above based on
-- the currently available addons. Missing addons will be removed, and
-- extra addons will be added under Misc.
printCommandsList :: [String] -> IO ()
printCommandsList addonsFound =
  putStr $
  regexReplace "PROGVERSION" (prognameandversion) $
  regexReplace "OTHER" (unlines $ (map ('+':) unknownCommandsFound)) $
  unlines $ concatMap adjustline $ lines $
  cmdlist
  where
    cmdlist = commandsList
    commandsFound = map (' ':) builtinCommandNames ++ map ('+':) addonsFound
    unknownCommandsFound = addonsFound \\ knownCommands

    adjustline l         | " hledger " `isPrefixOf` l     = [l]
    adjustline l@('+':_) | cmd `notElem` commandsFound = []
      where
        cmd = takeWhile (not . isSpace) l
    adjustline l = [l]


-- The test command is defined here for easy access to other modules' tests.

testmode = hledgerCommandMode
  $(embedFileRelative "Hledger/Cli/Commands/Test.txt")
  []
  [generalflagsgroup3]
  []
  ([], Just $ argsFlag "[TESTPATTERN] [SEED]")

-- | The test command.
-- Unlike most hledger commands, this one does not read the user's journal.
-- A 'Journal' argument remains in the type signature, but it should
-- not be used (and would raise an error).
testcmd :: CliOpts -> Journal -> IO ()
testcmd opts _undefined = do 
  let args = words' $ query_ $ reportopts_ opts
  -- workaround for https://github.com/joelburget/easytest/issues/11 
--  import System.IO (hSetEncoding, stdout, stderr, utf8)
--  hSetEncoding stdout utf8
--  hSetEncoding stderr utf8
  e <- runEasytests args $ EasyTest.tests [
     tests_Hledger
    ,tests "Hledger.Cli" [
       tests_Cli_Utils
      ,tests_Commands
      ]
    ]
  if e then exitFailure else exitSuccess


tests_Commands = tests "Commands" [
   tests_Balance
  ,tests_Register

  -- some more tests easiest to define here:
  
  ,test "apply account directive" $ do 
    let ignoresourcepos j = j{jtxns=map (\t -> t{tsourcepos=nullsourcepos}) (jtxns j)}
    let sameParse str1 str2 = do j1 <- io $ readJournal def Nothing str1 >>= either error' (return . ignoresourcepos)
                                 j2 <- io $ readJournal def Nothing str2 >>= either error' (return . ignoresourcepos)
                                 j1 `is` j2{jlastreadtime=jlastreadtime j1, jfiles=jfiles j1} --, jparsestate=jparsestate j1}
    sameParse
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

  ,test "apply account directive should preserve \"virtual\" posting type" $ do
    j <- io $ readJournal def Nothing "apply account test\n2008/12/07 One\n  (from)  $-1\n  (to)  $1\n" >>= either error' return
    let p = head $ tpostings $ head $ jtxns j
    paccount p `is` "test:from"
    ptype p `is` VirtualPosting
  
  ,test "account aliases" $ do
    j <- io $ readJournal def Nothing "!alias expenses = equity:draw:personal\n1/1\n (expenses:food)  1\n" >>= either error' return
    let p = head $ tpostings $ head $ jtxns j
    paccount p `is` "equity:draw:personal:food"

  ,test "ledgerAccountNames" $
    ledgerAccountNames ledger7 `is`
     ["assets","assets:cash","assets:checking","assets:saving","equity","equity:opening balances",
      "expenses","expenses:food","expenses:food:dining","expenses:phone","expenses:vacation",
      "liabilities","liabilities:credit cards","liabilities:credit cards:discover"]

  -- ,test "journalCanonicaliseAmounts" ~:
  --  "use the greatest precision" ~:
  --   (map asprecision $ journalAmountAndPriceCommodities $ journalCanonicaliseAmounts $ journalWithAmounts ["1","2.00"]) `is` [2,2]

  -- don't know what this should do
  -- ,test "elideAccountName" ~: do
  --    (elideAccountName 50 "aaaaaaaaaaaaaaaaaaaa:aaaaaaaaaaaaaaaaaaaa:aaaaaaaaaaaaaaaaaaaa"
  --     `is` "aa:aaaaaaaaaaaaaaaaaaaa:aaaaaaaaaaaaaaaaaaaa")
  --    (elideAccountName 20 "aaaaaaaaaaaaaaaaaaaa:aaaaaaaaaaaaaaaaaaaa:aaaaaaaaaaaaaaaaaaaa"
  --     `is` "aa:aa:aaaaaaaaaaaaaa")

  ,test "default year" $ do
    j <- io $ readJournal def Nothing defaultyear_journal_txt >>= either error' return
    tdate (head $ jtxns j) `is` fromGregorian 2009 1 1

  ,test "show dollars" $ showAmount (usd 1) `is` "$1.00"

  ,test "show hours" $ showAmount (hrs 1) `is` "1.00h"

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
             tprecedingcomment=""
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
             tprecedingcomment=""
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
             tprecedingcomment=""
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
             tprecedingcomment=""
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
             tprecedingcomment=""
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
             tprecedingcomment=""
           }
          ]
         }

ledger7 :: Ledger
ledger7 = ledgerFromJournal Any journal7
