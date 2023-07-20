{-|
hledger's built-in commands, and helpers for printing the commands list.

New built-in commands should be added in four places below:
the export list, the import list, builtinCommands, commandsList.

-}

-- Note: commands list rendering is intensely sensitive to change,
-- very easy to break in ways that tests currently do not catch.

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Hledger.Cli.Commands (
   testcmd
  ,builtinCommands
  ,builtinCommandNames
  ,findBuiltinCommand
  ,knownAddonCommands
  ,knownCommands
  ,printCommandsList
  ,tests_Hledger_Cli
  ,module Hledger.Cli.Commands.Accounts
  ,module Hledger.Cli.Commands.Activity
  ,module Hledger.Cli.Commands.Add
  ,module Hledger.Cli.Commands.Aregister
  ,module Hledger.Cli.Commands.Balance
  ,module Hledger.Cli.Commands.Balancesheet
  ,module Hledger.Cli.Commands.Balancesheetequity
  ,module Hledger.Cli.Commands.Cashflow
  ,module Hledger.Cli.Commands.Close
  ,module Hledger.Cli.Commands.Codes
  ,module Hledger.Cli.Commands.Commodities
  ,module Hledger.Cli.Commands.Demo
  ,module Hledger.Cli.Commands.Descriptions
  ,module Hledger.Cli.Commands.Diff
  ,module Hledger.Cli.Commands.Help
  ,module Hledger.Cli.Commands.Import
  ,module Hledger.Cli.Commands.Incomestatement
  ,module Hledger.Cli.Commands.Notes
  ,module Hledger.Cli.Commands.Payees
  ,module Hledger.Cli.Commands.Prices
  ,module Hledger.Cli.Commands.Print
  ,module Hledger.Cli.Commands.Register
  ,module Hledger.Cli.Commands.Rewrite
  ,module Hledger.Cli.Commands.Stats
  ,module Hledger.Cli.Commands.Tags
) 
where

import Data.Char (isAlphaNum, isSpace)
import Data.List
import Data.List.Extra (nubSort)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Calendar
import String.ANSI
import System.Environment (withArgs)
import System.Console.CmdArgs.Explicit as C
import Test.Tasty (defaultMain)

import Hledger
import Hledger.Cli.CliOptions
import Hledger.Cli.Version
import Hledger.Cli.Commands.Accounts
import Hledger.Cli.Commands.Activity
import Hledger.Cli.Commands.Add
import Hledger.Cli.Commands.Aregister
import Hledger.Cli.Commands.Balance
import Hledger.Cli.Commands.Balancesheet
import Hledger.Cli.Commands.Balancesheetequity
import Hledger.Cli.Commands.Cashflow
import Hledger.Cli.Commands.Check
import Hledger.Cli.Commands.Close
import Hledger.Cli.Commands.Codes
import Hledger.Cli.Commands.Commodities
import Hledger.Cli.Commands.Demo
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
import Hledger.Cli.Commands.Register
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
  ,(aregistermode          , aregister)
  ,(balancemode            , balance)
  ,(balancesheetequitymode , balancesheetequity)
  ,(balancesheetmode       , balancesheet)
  ,(cashflowmode           , cashflow)
  ,(checkmode              , check)
  ,(closemode              , close)
  ,(codesmode              , codes)
  ,(commoditiesmode        , commodities)
  ,(demomode               , demo)
  ,(descriptionsmode       , descriptions)
  ,(diffmode               , diff)
  ,(filesmode              , files)
  ,(helpmode               , help')
  ,(importmode             , importcmd)
  ,(incomestatementmode    , incomestatement)
  ,(notesmode              , notes)
  ,(payeesmode             , payees)
  ,(pricesmode             , prices)
  ,(printmode              , print')
  ,(registermode           , register)
  ,(rewritemode            , rewrite)
  ,(roimode                , roi)
  ,(statsmode              , stats)
  ,(tagsmode               , tags)
  ,(testmode               , testcmd)
  ]

-- figlet -f FONTNAME hledger, then escape backslashes
_banner_slant = drop 1 [""
    -----------------------------------------80-------------------------------------
  ,"    __    __         __               "
  ,"   / /_  / /__  ____/ /___ ____  _____"
  ,"  / __ \\/ / _ \\/ __  / __ `/ _ \\/ ___/"
  ," / / / / /  __/ /_/ / /_/ /  __/ /    "
  ,"/_/ /_/_/\\___/\\__,_/\\__, /\\___/_/     "
  ,"                   /____/             "
  ]

_banner_smslant = drop 1 [""
  ,"   __   __       __            "
  ,"  / /  / /__ ___/ /__ ____ ____"
  ," / _ \\/ / -_) _  / _ `/ -_) __/"
  ,"/_//_/_/\\__/\\_,_/\\_, /\\__/_/   "
  ,"                /___/          "
  ]

_banner_speed = drop 1 [""
  ,"______ ______    _________                    "
  ,"___  /____  /__________  /______ _____________"
  ,"__  __ \\_  /_  _ \\  __  /__  __ `/  _ \\_  ___/"
  ,"_  / / /  / /  __/ /_/ / _  /_/ //  __/  /    "
  ,"/_/ /_//_/  \\___/\\__,_/  _\\__, / \\___//_/     "
  ,"                         /____/               "
  ]

-- | Choose and apply an accent color for hledger output, if possible
-- picking one that will contrast with the current terminal background colour.
accent :: String -> String
accent
  | not useColorOnStdout          = id
  | terminalIsLight == Just False = brightWhite
  | terminalIsLight == Just True  = brightBlack
  | otherwise                     = id

-- | The commands list, showing command names, standard aliases,
-- and short descriptions. This is modified at runtime, as follows:
--
-- progversion is the program name and version.
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
commandsList :: String -> [String] -> [String]
commandsList progversion othercmds =
  map (bold'.accent) _banner_smslant ++ 
  [
  -- Keep the following synced with:
  --  commands.m4
  --  hledger.m4.md -> Commands
  --  commandsFromCommandsList. Only commands should begin with space or plus.
   "-------------------------------------------------------------------------------"
  ,progversion
  ,"Usage: hledger CMD [OPTS] [-- ADDONCMDOPTS]"
  ,"Commands (builtins + addons):"
  ,""
  ,bold' "ENTERING DATA (add or edit transactions, updating the journal file)"
  ," add                      add transactions using terminal prompts"
  ,"+edit                     edit a subset of transactions"
  ,"+iadd                     add transactions using a TUI"
  ," import                   add new transactions from other files, eg CSV files"
  ,""
    -----------------------------------------80-------------------------------------
  ,bold' "GENERATING DATA (generate entries to be added to the journal file)"
  ,"+autosync                 download/deduplicate/convert OFX data"
  ," close                    generate balance-zeroing/restoring transactions"
  ,"+interest                 generate interest transactions"
  ,"+lots sell                generate a lot-selling transaction"
  ," rewrite                  generate auto postings, like print --auto"
  ,"+stockquotes              download market prices from AlphaVantage"
  ,""
    -----------------------------------------80-------------------------------------
  ,bold' "MANAGING DATA (error checking, file management, version control..)"
  ," check                    check for various kinds of error in the data"
  ,"+check-fancyassertions    check more powerful balance assertions"
  ,"+check-tagfiles           check file paths in tag values exist"
  ," diff                     compare account transactions in two journal files"
  ," files                    show data files in use"
  ,"+git                      simple version management with git"
  ,"+pijul                    simple version management with pijul"
  ,""
    -----------------------------------------80-------------------------------------
  ,bold' "FINANCIAL REPORTS (standard financial statements)"
  ," aregister (areg)         show transactions in a particular account"
  ," balancesheet (bs)        show assets, liabilities and net worth"
  ," balancesheetequity (bse) show assets, liabilities and equity"
  ," cashflow (cf)            show changes in liquid assets"
  ," incomestatement (is)     show revenues and expenses"
  ,""
    -----------------------------------------80-------------------------------------
  ,bold' "VERSATILE REPORTS (more complex/versatile reporting commands)"
  ," activity                 show a simple bar chart of posting counts per period"
  ," balance (bal)            show balance changes, end balances, budgets, gains.."
  ,"+bar                      show a balance report as a simple bar chart"
  ,"+lots                     show a commodity's lots"
  ,"+plot                     create charts from balance reports, in terminal or GUI"
  ," print                    show transactions or export journal data"
  ," register (reg)           show postings in one or more accounts & running total"
  ," roi                      show return on investments"
  ,""
    -----------------------------------------80-------------------------------------
  ,bold' "BASIC REPORTS (lists and stats)"
  ," accounts                 show account names"
  ," codes                    show transaction codes"
  ," commodities              show commodity/currency symbols"
  ," descriptions             show full transaction descriptions (payee and note)"
  ," notes                    show note part of transaction descriptions"
  ," payees                   show payee names"
  ," prices                   show historical market prices"
  ," stats                    show journal statistics"
  ," tags                     show tag names"
  ," test                     run self tests"
  ,""
    -----------------------------------------80-------------------------------------
  ,bold' "UIS (other user interfaces)"
  ,"+ui                       run terminal UI"
  ,"+web                      run web UI"
  ,""
    -----------------------------------------80-------------------------------------
  ,bold' "HELP (show help and docs)"
  ," hledger                          show this commands list"
  ," hledger -h                       show hledger's command-line help"
  ," hledger CMD -h                   show CMD's command-line help and manual"
  ," hledger help [-i|-m|-p] [TOPIC]  show hledger's manual with info, man, or pager"
  ," hledger demo [DEMO] -- [ASCOPTS] show brief demos on various topics"
  ," https://hledger.org              html manuals, tutorials, support.."
  ,""
    -----------------------------------------80-------------------------------------
  ,bold' "OTHER (more hledger-* addon commands found in PATH)"
  ]
  ++ map (' ':) (lines $ multicol 79 othercmds)
  ++ [""]

-- | Extract just the command names from the default commands list above,
-- (the first word of lines between "Usage:" and "HELP" beginning with a space or plus sign),
-- in the order they occur. With a true first argument, extracts only the addon command names.
-- Needs to be kept synced with commandsList.
commandsListExtractCommands :: Bool -> [String] -> [String]
commandsListExtractCommands addonsonly l =
  [ w | c:ws@(d:_) <- takeWhile (not . isInfixOf "HELP") $ dropWhile (not . isInfixOf "Usage:") l
  , c `elem` '+':[' '|not addonsonly]
  , isAlphaNum d
  , not $ "://" `isInfixOf` ws
  , let w:_ = words ws
  ]

-- | Canonical names of all commands which have a slot in the commands list, in alphabetical order.
-- These include the builtin commands and the known addon commands.
knownCommands :: [String]
knownCommands = nubSort . commandsListExtractCommands False $ commandsList progname []

-- | Canonical names of the known addon commands which have a slot in the commands list,
-- in alphabetical order.
knownAddonCommands :: [String]
knownAddonCommands = nubSort . commandsListExtractCommands True $ commandsList progname []

-- | All names and aliases of the builtin commands.
builtinCommandNames :: [String]
builtinCommandNames = concatMap (modeNames . fst) builtinCommands

-- | Look up a builtin command's mode and action by exact command name or alias. 
findBuiltinCommand :: String -> Maybe (Mode RawOpts, CliOpts -> Journal -> IO ()) 
findBuiltinCommand cmdname = find (elem cmdname . modeNames . fst) builtinCommands 

-- | Print the commands list, with a pager if appropriate, customising the
-- commandsList template above with the given version string and the installed addons.
-- Uninstalled known addons will be removed from the list,
-- installed known addons will have the + prefix removed,
-- and installed unknown addons will be added under Misc.
printCommandsList :: String -> [String] -> IO ()
printCommandsList progversion installedaddons =
  seq (length $ dbg8 "uninstalledknownaddons" uninstalledknownaddons) $  -- for debug output
  seq (length $ dbg8 "installedknownaddons"   installedknownaddons)   $
  seq (length $ dbg8 "installedunknownaddons" installedunknownaddons) $
  pager $ unlines $ map unplus $ filter (not.isuninstalledaddon) $
  commandsList progversion installedunknownaddons
  where
    knownaddons = knownAddonCommands
    uninstalledknownaddons  = knownaddons \\ installedaddons
    installedknownaddons    = knownaddons `intersect` installedaddons
    installedunknownaddons  = installedaddons \\ knownaddons
    unplus ('+':cs) = ' ':cs
    unplus s = s
    isuninstalledaddon =
      \case
        ('+':l) | cmd `notElem` installedaddons ->
                  dbg9With (const $ "hiding uninstalled addon: "<>cmd) $
                  True where cmd = takeWhile (not . isSpace) l
        _ -> False

-- The test command is defined here for easy access to other modules' tests.

testmode = hledgerCommandMode
  $(embedFileRelative "Hledger/Cli/Commands/Test.txt")
  []
  [generalflagsgroup3]
  []
  ([], Just $ argsFlag "[-- TASTYOPTS]")

-- | The test command, which runs the hledger and hledger-lib
-- packages' unit tests. This command also accepts tasty test runner
-- options, written after a -- (double hyphen).
--
-- Unlike most hledger commands, this one does not read the user's journal.
-- A 'Journal' argument remains in the type signature, but it should
-- not be used (and would raise an error).
--
testcmd :: CliOpts -> Journal -> IO ()
testcmd opts _undefined = do
  withArgs (listofstringopt "args" $ rawopts_ opts) $
    Test.Tasty.defaultMain $ testGroup "hledger" [
       tests_Hledger
      ,tests_Hledger_Cli
      ]

-- All unit tests for Hledger.Cli, defined here rather than
-- Hledger.Cli so testcmd can use them.
tests_Hledger_Cli = testGroup "Hledger.Cli" [
   tests_Cli_Utils
  ,tests_Commands
  ]

tests_Commands = testGroup "Commands" [
   tests_Balance
  ,tests_Register
  ,tests_Aregister

  -- some more tests easiest to define here:

  ,testGroup "apply account directive" [
     testCase "works" $ do
        let
          ignoresourcepos j = j{jtxns=map (\t -> t{tsourcepos=nullsourcepos}) (jtxns j)}
          sameParse str1 str2 = do
            j1 <- ignoresourcepos <$> readJournal' str1  -- PARTIAL:
            j2 <- ignoresourcepos <$> readJournal' str2  -- PARTIAL:
            j1 @?= j2{jlastreadtime=jlastreadtime j1, jfiles=jfiles j1} --, jparsestate=jparsestate j1}
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

    ,testCase "preserves \"virtual\" posting type" $ do
      j <- readJournal' "apply account test\n2008/12/07 One\n  (from)  $-1\n  (to)  $1\n"  -- PARTIAL:
      let p = head $ tpostings $ head $ jtxns j
      paccount p @?= "test:from"
      ptype p @?= VirtualPosting
    ]

  ,testCase "alias directive" $ do
    j <- readJournal' "!alias expenses = equity:draw:personal\n1/1\n (expenses:food)  1\n"  -- PARTIAL:
    let p = head $ tpostings $ head $ jtxns j
    paccount p @?= "equity:draw:personal:food"

  ,testCase "Y default year directive" $ do
    j <- readJournal' defaultyear_journal_txt  -- PARTIAL:
    tdate (head $ jtxns j) @?= fromGregorian 2009 1 1

  ,testCase "ledgerAccountNames" $
    (ledgerAccountNames ledger7)
    @?=
    ["assets","assets:cash","assets:checking","assets:saving","equity","equity:opening balances",
     "expenses","expenses:food","expenses:food:dining","expenses:phone","expenses:vacation",
     "liabilities","liabilities:credit cards","liabilities:credit cards:discover"]

  -- ,testCase "journalCanonicaliseAmounts" ~:
  --  "use the greatest precision" ~:
  --   (map asprecision $ journalAmountAndPriceCommodities $ journalCanonicaliseAmounts $ journalWithAmounts ["1","2.00"]) @?= [2,2]

  -- don't know what this should do
  -- ,testCase "elideAccountName" ~: do
  --    (elideAccountName 50 "aaaaaaaaaaaaaaaaaaaa:aaaaaaaaaaaaaaaaaaaa:aaaaaaaaaaaaaaaaaaaa"
  --     @?= "aa:aaaaaaaaaaaaaaaaaaaa:aaaaaaaaaaaaaaaaaaaa")
  --    (elideAccountName 20 "aaaaaaaaaaaaaaaaaaaa:aaaaaaaaaaaaaaaaaaaa:aaaaaaaaaaaaaaaaaaaa"
  --     @?= "aa:aa:aaaaaaaaaaaaaa")

  ,testCase "show dollars" $ showAmount (usd 1) @?= "$1.00"

  ,testCase "show hours" $ showAmount (hrs 1) @?= "1.00h"

  ]

-- test data

-- date1 = fromGregorian 2008 11 26
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
--  ["include \"somefile\""
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
             tdate=fromGregorian 2007 01 01,
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
             tdate=fromGregorian 2007 02 01,
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
             tdate=fromGregorian 2007 01 02,
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
             tdate=fromGregorian 2007 01 03,
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
             tdate=fromGregorian 2007 01 03,
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
             tdate=fromGregorian 2007 01 03,
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
