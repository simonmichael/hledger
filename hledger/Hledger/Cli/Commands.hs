{-|
hledger's built-in commands, and helpers for printing the commands list.
-}

{-# LANGUAGE QuasiQuotes #-}

module Hledger.Cli.Commands (
   findCommand
  ,builtinCommands
  ,builtinCommandNames
  ,printCommandsList
  ,tests_Hledger_Cli_Commands
) 
where

import Data.String.Here
import Data.List
import Data.List.Split (splitOn)
import System.Console.CmdArgs.Explicit as C
import Test.HUnit

import Hledger.Cli.Accounts
import Hledger.Cli.Activity
import Hledger.Cli.Add
import Hledger.Cli.Balance
import Hledger.Cli.Balancesheet
import Hledger.Cli.Balancesheetequity
import Hledger.Cli.Cashflow
import Hledger.Cli.Help
import Hledger.Cli.Incomestatement
import Hledger.Cli.Print
import Hledger.Cli.Register
import Hledger.Cli.Stats
import Hledger.Cli.CliOptions
import Hledger.Data
import Hledger.Utils (regexReplace)


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
  ,(helpmode               , help') 
  ,(incomestatementmode    , incomestatement) 
  ,(printmode              , print') 
  ,(registermode           , register) 
  ,(statsmode              , stats) 
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
      where w = takeWhile (not . (`elem` "| ")) l
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

tests_Hledger_Cli_Commands :: Test
tests_Hledger_Cli_Commands = TestList [
  -- ,tests_Hledger_Cli_Add
   tests_Hledger_Cli_Balance
  ,tests_Hledger_Cli_Balancesheet
  ,tests_Hledger_Cli_Cashflow
  -- ,tests_Hledger_Cli_Histogram
  ,tests_Hledger_Cli_Incomestatement
  -- ,tests_Hledger_Cli_Print
  ,tests_Hledger_Cli_Register
  -- ,tests_Hledger_Cli_Stats
  ]