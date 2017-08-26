{-|
hledger - a ledger-compatible accounting tool.
Copyright (c) 2007-2011 Simon Michael <simon@joyful.com>
Released under GPL version 3 or later.

hledger is a partial haskell clone of John Wiegley's "ledger".  It
generates ledger-compatible register & balance reports from a plain text
journal, and demonstrates a functional implementation of ledger.
For more information, see http:\/\/hledger.org .

This module provides the main function for the hledger command-line
executable. It is exposed here so that it can be imported by eg benchmark
scripts.

You can use the command line:

> $ hledger --help

or ghci:

> $ ghci hledger
> > j <- readJournalFile Nothing Nothing True "examples/sample.journal"
> > register [] ["income","expenses"] j
> 2008/01/01 income               income:salary                   $-1          $-1
> 2008/06/01 gift                 income:gifts                    $-1          $-2
> 2008/06/03 eat & shop           expenses:food                    $1          $-1
>                                 expenses:supplies                $1            0
> > balance [Depth "1"] [] l
>                  $-1  assets
>                   $2  expenses
>                  $-2  income
>                   $1  liabilities
> > l <- myLedger

See "Hledger.Data.Ledger" for more examples.

-}

{-# LANGUAGE QuasiQuotes #-}

module Hledger.Cli.Main where

-- import Control.Monad
import Data.Char (isDigit)
import Data.String.Here
import Data.List
import Data.List.Split (splitOn)
import Safe
import System.Console.CmdArgs.Explicit as C
import System.Environment
import System.Exit
import System.FilePath
import System.Process
import Text.Printf

import Hledger (ensureJournalFileExists)
import Hledger.Cli.Add
import Hledger.Cli.Accounts
import Hledger.Cli.Balance
import Hledger.Cli.Help
import Hledger.Cli.Histogram
import Hledger.Cli.CompoundBalanceCommand
import Hledger.Cli.Info
import Hledger.Cli.Man
import Hledger.Cli.Print
import Hledger.Cli.Register
import Hledger.Cli.Stats
import Hledger.Cli.CliOptions
import Hledger.Cli.Tests
import Hledger.Cli.Utils
import Hledger.Cli.Version
import Hledger.Data.Dates (getCurrentDay)
import Hledger.Data.RawOptions (RawOpts)
import Hledger.Reports.ReportOptions (period_, interval_, queryFromOpts)
import Hledger.Utils


-- | The overall cmdargs mode describing command-line options for hledger.
mainmode addons = defMode {
  modeNames = [progname ++ " [CMD]"]
 ,modeArgs = ([], Just $ argsFlag "[ARGS]")
 ,modeHelp = unlines ["hledger's main command line interface. Runs builtin commands and other hledger executables. Type \"hledger\" to list available commands."]
 ,modeGroupModes = Group {
    -- subcommands in the unnamed group, shown first:
    groupUnnamed = [
     ]
    -- subcommands in named groups:
   ,groupNamed = [
     ]
    -- subcommands handled but not shown in the help:
   ,groupHidden = [
      oldconvertmode
     ,accountsmode
     ,activitymode
     ,addmode
     ,balancemode
     ,balancesheetmode
     ,cashflowmode
     ,compoundreportmode
     ,helpmode
     ,incomestatementmode
     ,infomode
     ,manmode
     ,printmode
     ,registermode
     ,statsmode
     ,testmode
     ] ++ map quickAddonCommandMode addons
   }
 ,modeGroupFlags = Group {
     -- flags in named groups:
     groupNamed = [
        (  "General input flags",     inputflags)
       ,("\nGeneral reporting flags", reportflags)
       ,("\nGeneral help flags",      helpflags)
       ]
     -- flags in the unnamed group, shown last:
    ,groupUnnamed = []
     -- flags handled but not shown in the help:
    ,groupHidden =
        [detailedversionflag]
        -- ++ inputflags -- included here so they'll not raise a confusing error if present with no COMMAND
    }
 ,modeHelpSuffix = lines $ regexReplace "PROGNAME" progname [here|Examples:
PROGNAME                         list commands
PROGNAME CMD [--] [OPTS] [ARGS]  run a command (use -- with addon commands)
PROGNAME-CMD [OPTS] [ARGS]       or run addon commands directly
PROGNAME -h                      show general usage
PROGNAME CMD -h                  show command usage
PROGNAME help [MANUAL]           show any of the hledger manuals in various formats
|]
 }

oldconvertmode = (defCommandMode ["convert"]) {
  modeValue = [("command","convert")]
 ,modeHelp = "convert is no longer needed, just use -f FILE.csv"
 ,modeArgs = ([], Just $ argsFlag "[CSVFILE]")
 ,modeGroupFlags = Group {
     groupUnnamed = []
    ,groupHidden = helpflags
    ,groupNamed = []
    }
 }

builtinCommands :: [Mode RawOpts]
builtinCommands =
  let gs = modeGroupModes $ mainmode []
  in concatMap snd (groupNamed gs) ++ groupUnnamed gs ++ groupHidden gs

builtinCommandNames :: [String]
builtinCommandNames = concatMap modeNames builtinCommands

-- | Parse hledger CLI options from these command line arguments and
-- add-on command names, or raise any error.
argsToCliOpts :: [String] -> [String] -> IO CliOpts
argsToCliOpts args addons = do
  let
    args'        = moveFlagsAfterCommand args
    cmdargsopts  = either usageError id $ process (mainmode addons) args'
    cmdargsopts' = decodeRawOpts cmdargsopts
  rawOptsToCliOpts cmdargsopts'

-- | A hacky workaround for cmdargs not accepting flags before the
-- subcommand name: try to detect and move such flags after the
-- command.  This allows the user to put them in either position.
-- The order of options is not preserved, but this should be ok.
--
-- Since we're not parsing flags as precisely as cmdargs here, this is
-- imperfect. We make a decent effort to:
-- - move all no-argument help/input/report flags
-- - move all required-argument help/input/report flags along with their values, space-separated or not
-- - not confuse things further or cause misleading errors.
moveFlagsAfterCommand :: [String] -> [String]
moveFlagsAfterCommand args = moveArgs $ ensureDebugHasArg args
  where
    -- quickly! make sure --debug has a numeric argument, or this all goes to hell
    ensureDebugHasArg as =
      case break (=="--debug") as of
       (bs,"--debug":c:cs) | null c || not (all isDigit c) -> bs++"--debug=1":c:cs
       (bs,"--debug":[])                                   -> bs++"--debug=1":[]
       _                                                   -> as

    -- -h ..., --version ...
    moveArgs (f:a:as)   | isMovableNoArgFlag f                   = (moveArgs $ a:as) ++ [f]
    -- -f FILE ..., --alias ALIAS ...
    moveArgs (f:v:a:as) | isMovableReqArgFlag f, isValue v       = (moveArgs $ a:as) ++ [f,v]
    -- -fFILE ..., --alias=ALIAS ...
    moveArgs (fv:a:as)  | isMovableReqArgFlagAndValue fv         = (moveArgs $ a:as) ++ [fv]
    -- -f(missing arg)
    moveArgs (f:a:as)   | isMovableReqArgFlag f, not (isValue a) = (moveArgs $ a:as) ++ [f]
    -- anything else
    moveArgs as = as

isMovableNoArgFlag a  = "-" `isPrefixOf` a && dropWhile (=='-') a `elem` noargflagstomove

isMovableReqArgFlag a = "-" `isPrefixOf` a && dropWhile (=='-') a `elem` reqargflagstomove

isMovableReqArgFlagAndValue ('-':'-':a:as) = case break (== '=') (a:as) of (f:fs,_:_) -> (f:fs) `elem` reqargflagstomove
                                                                           _          -> False
isMovableReqArgFlagAndValue ('-':shortflag:_:_) = [shortflag] `elem` reqargflagstomove
isMovableReqArgFlagAndValue _ = False

isValue "-"     = True
isValue ('-':_) = False
isValue _       = True

flagstomove = inputflags ++ reportflags ++ helpflags
noargflagstomove  = concatMap flagNames $ filter ((==FlagNone).flagInfo) flagstomove
reqargflagstomove = -- filter (/= "debug") $
                    concatMap flagNames $ filter ((==FlagReq ).flagInfo) flagstomove

-- | Template for the commands list. 
-- Includes an entry for all known or hypothetical builtin and addon commands; 
-- these will be filtered based on the commands found at runtime.  
-- Commands beginning with "hledger" are not filtered ("hledger -h" etc.) 
-- COUNT is replaced with the number of commands found.  
-- OTHERCMDS is replaced with an entry for each unknown addon command found. 
-- The command descriptions here should be synced with each command's builtin help 
-- and with hledger manual's command list.
commandsListTemplate :: String
commandsListTemplate = [here|Commands available (COUNT):

Standard reports:
 accounts             show chart of accounts
 balancesheet (bs)    show a balance sheet
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

knownCommands :: [String]
knownCommands = sort $ commandsFromCommandsList commandsListTemplate

-- | Extract the command names from a commands list like the above:
-- the first word (or words separated by |) of lines beginning with a space.
commandsFromCommandsList :: String -> [String]
commandsFromCommandsList s = concatMap (splitOn "|") [w | ' ':l <- lines s, let w:_ = words l]

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


-- | Let's go.
main :: IO ()
main = do

  -- Choose and run the appropriate internal or external command based
  -- on the raw command-line arguments, cmdarg's interpretation of
  -- same, and hledger-* executables in the user's PATH. A somewhat
  -- complex mishmash of cmdargs and custom processing, hence all the
  -- debugging support and tests. See also Hledger.Cli.CliOptions and
  -- command-line.test.

  -- some preliminary (imperfect) argument parsing to supplement cmdargs
  args <- getArgs
  let
    args'                = moveFlagsAfterCommand args
    isFlag               = ("-" `isPrefixOf`)
    isNonEmptyNonFlag s  = not (isFlag s) && not (null s)
    rawcmd               = headDef "" $ takeWhile isNonEmptyNonFlag args'
    isNullCommand        = null rawcmd
    (argsbeforecmd, argsaftercmd') = break (==rawcmd) args
    argsaftercmd         = drop 1 argsaftercmd'
    dbgIO :: Show a => String -> a -> IO ()
    dbgIO = tracePrettyAtIO 2

  dbgIO "running" prognameandversion
  dbgIO "raw args" args
  dbgIO "raw args rearranged for cmdargs" args'
  dbgIO "raw command is probably" rawcmd
  dbgIO "raw args before command" argsbeforecmd
  dbgIO "raw args after command" argsaftercmd

  -- Search PATH for add-ons, excluding any that match built-in command names
  addons' <- hledgerAddons
  let addons = filter (not . (`elem` builtinCommandNames) . dropExtension) addons'

  -- parse arguments with cmdargs
  opts <- argsToCliOpts args addons

  -- select an action and run it.
  let
    cmd                  = command_ opts -- the full matched internal or external command name, if any
    isInternalCommand    = cmd `elem` builtinCommandNames -- not (null cmd) && not (cmd `elem` addons)
    isExternalCommand    = not (null cmd) && cmd `elem` addons -- probably
    isBadCommand         = not (null rawcmd) && null cmd
    hasVersion           = ("--version" `elem`)
    hasDetailedVersion   = ("--version+" `elem`)
    printUsage           = putStr $ showModeUsage $ mainmode addons
    badCommandError      = error' ("command "++rawcmd++" is not recognized, run with no command to see a list") >> exitFailure
    hasHelpFlag args     = any (`elem` args) ["-h","--help"]
    f `orShowHelp` mode
      | hasHelpFlag args = putStr $ showModeUsage mode
      | otherwise        = f
  dbgIO "processed opts" opts
  dbgIO "command matched" cmd
  dbgIO "isNullCommand" isNullCommand
  dbgIO "isInternalCommand" isInternalCommand
  dbgIO "isExternalCommand" isExternalCommand
  dbgIO "isBadCommand" isBadCommand
  d <- getCurrentDay
  dbgIO "period from opts" (period_ $ reportopts_ opts)
  dbgIO "interval from opts" (interval_ $ reportopts_ opts)
  dbgIO "query from opts & args" (queryFromOpts d $ reportopts_ opts)
  let
    runHledgerCommand
      -- high priority flags and situations. -h, then --help, then --info are highest priority.
      | hasHelpFlag argsbeforecmd = dbgIO "" "-h before command, showing general usage" >> printUsage
      | not (hasHelpFlag argsaftercmd) && (hasVersion argsbeforecmd || (hasVersion argsaftercmd && isInternalCommand))
                                 = putStrLn prognameandversion
      | not (hasHelpFlag argsaftercmd) && (hasDetailedVersion argsbeforecmd || (hasDetailedVersion argsaftercmd && isInternalCommand))
                                 = putStrLn prognameanddetailedversion
      -- \| (null externalcmd) && "binary-filename" `inRawOpts` rawopts = putStrLn $ binaryfilename progname
      -- \| "--browse-args" `elem` args     = System.Console.CmdArgs.Helper.execute "cmdargs-browser" mainmode' args >>= (putStr . show)
      | isNullCommand            = dbgIO "" "no command, showing commands list" >> printCommandsList addons
      | isBadCommand             = badCommandError

      -- internal commands
      | cmd == "activity"        = withJournalDo opts histogram       `orShowHelp` activitymode
      | cmd == "add"             = (journalFilePathFromOpts opts >>= (ensureJournalFileExists . head) >> withJournalDo opts add) `orShowHelp` addmode
      | cmd == "accounts"        = withJournalDo opts accounts        `orShowHelp` accountsmode
      | cmd == "balance"         = withJournalDo opts balance         `orShowHelp` balancemode
      | cmd == "balancesheet"    = withJournalDo opts balancesheet    `orShowHelp` balancesheetmode
      | cmd == "cashflow"        = withJournalDo opts cashflow        `orShowHelp` cashflowmode
      | cmd == "incomestatement" = withJournalDo opts incomestatement `orShowHelp` incomestatementmode
      | cmd == "report"          = withJournalDo opts compoundreport  `orShowHelp` compoundreportmode
      | cmd == "print"           = withJournalDo opts print'          `orShowHelp` printmode
      | cmd == "register"        = withJournalDo opts register        `orShowHelp` registermode
      | cmd == "stats"           = withJournalDo opts stats           `orShowHelp` statsmode
      | cmd == "test"            = test' opts                         `orShowHelp` testmode
      | cmd == "help"            = help' opts                         `orShowHelp` helpmode
      | cmd == "man"             = man opts                           `orShowHelp` manmode
      | cmd == "info"            = info' opts                         `orShowHelp` infomode

      -- an external command
      | isExternalCommand = do
          let externalargs = argsbeforecmd ++ filter (not.(=="--")) argsaftercmd
          let shellcmd = printf "%s-%s %s" progname cmd (unwords' externalargs) :: String
          dbgIO "external command selected" cmd
          dbgIO "external command arguments" (map quoteIfNeeded externalargs)
          dbgIO "running shell command" shellcmd
          system shellcmd >>= exitWith

      -- deprecated commands
      | cmd == "convert"         = error' (modeHelp oldconvertmode) >> exitFailure

      -- shouldn't reach here
      | otherwise                = usageError ("could not understand the arguments "++show args) >> exitFailure

  runHledgerCommand


-- tests_runHledgerCommand = [
--   -- "runHledgerCommand" ~: do
--   --   let opts = defreportopts{query_="expenses"}
--   --   d <- getCurrentDay
--   --   runHledgerCommand addons opts@CliOpts{command_=cmd} args

--  ]


