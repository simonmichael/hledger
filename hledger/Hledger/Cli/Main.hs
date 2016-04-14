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
> > j <- readJournalFile Nothing Nothing "data/sample.journal"
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

module Hledger.Cli.Main where

-- import Control.Monad
import Data.Char (isDigit)
import Data.List
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
import Hledger.Cli.Balancesheet
import Hledger.Cli.Cashflow
import Hledger.Cli.Histogram
import Hledger.Cli.Incomestatement
import Hledger.Cli.Print
import Hledger.Cli.Register
import Hledger.Cli.Stats
import Hledger.Cli.CliOptions
import Hledger.Cli.Tests
import Hledger.Cli.Utils
import Hledger.Cli.Version
import Hledger.Data.Dates (getCurrentDay)
import Hledger.Data.RawOptions (RawOpts, optserror)
import Hledger.Reports.ReportOptions (dateSpanFromOpts, intervalFromOpts, queryFromOpts)
import Hledger.Utils


-- | The overall cmdargs mode describing command-line options for hledger.
mainmode addons = defMode {
  modeNames = [progname]
 ,modeHelp = unlines []
 ,modeHelpSuffix = [""]
 ,modeArgs = ([], Just $ argsFlag "[ARGS]")
 ,modeGroupModes = Group {
    -- modes (commands) in named groups:
    groupNamed = [
      ("Data entry commands", [
        addmode
       ])
     ,("\nReporting commands", [
        printmode
       ,accountsmode
       ,balancemode
       ,registermode
       ,incomestatementmode
       ,balancesheetmode
       ,cashflowmode
       ,activitymode
       ,statsmode
       ])
     ]
     ++ case addons of [] -> []
                       cs -> [("\nAdd-on commands", map defAddonCommandMode cs)]
    -- modes in the unnamed group, shown first without a heading:
   ,groupUnnamed = [
     ]
    -- modes handled but not shown
   ,groupHidden = [
        testmode
       ,oldconvertmode
     ]
   }
 ,modeGroupFlags = Group {
     -- flags in named groups:
     groupNamed = [generalflagsgroup3]
     -- flags in the unnamed group, shown last without a heading:
    ,groupUnnamed = []
     -- flags accepted but not shown in the help:
    ,groupHidden =
        detailedversionflag :
        inputflags -- included here so they'll not raise a confusing error if present with no COMMAND
    }
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
    cmdargsopts  = processValue (mainmode addons) args'
    cmdargsopts' = decodeRawOpts cmdargsopts
  rawOptsToCliOpts cmdargsopts'

-- | A hacky workaround for cmdargs not accepting flags before the
-- subcommand name: try to detect and move such flags after the
-- command.  This allows the user to put them in either position.
-- The order of options is not preserved, but this should be ok.
--
-- Since we're not parsing flags as precisely as cmdargs here, this is
-- imperfect. We make a decent effort to:
-- - move all no-argument help and input flags
-- - move all required-argument help and input flags along with their values, space-separated or not
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

flagstomove = inputflags ++ helpflags
noargflagstomove  = concatMap flagNames $ filter ((==FlagNone).flagInfo) flagstomove
reqargflagstomove = -- filter (/= "debug") $
                    concatMap flagNames $ filter ((==FlagReq ).flagInfo) flagstomove

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

  -- Search PATH for add-ons, excluding any that match built-in names.
  -- The precise addon names (including file extension) are used for command
  -- parsing, and the display names are used for displaying the commands list.
  (addonPreciseNames', addonDisplayNames') <- hledgerAddons
  let addonPreciseNames = filter (not . (`elem` builtinCommandNames) . dropExtension) addonPreciseNames'
  let addonDisplayNames = filter (not . (`elem` builtinCommandNames)) addonDisplayNames'

  -- parse arguments with cmdargs
  opts <- argsToCliOpts args addonPreciseNames

  -- select an action and run it.
  let
    cmd                  = command_ opts -- the full matched internal or external command name, if any
    isInternalCommand    = cmd `elem` builtinCommandNames -- not (null cmd) && not (cmd `elem` addons)
    isExternalCommand    = not (null cmd) && cmd `elem` addonPreciseNames -- probably
    isBadCommand         = not (null rawcmd) && null cmd
    hasVersion           = ("--version" `elem`)
    hasDetailedVersion   = ("--version+" `elem`)
    generalUsage         = putStr $ showModeUsage $ mainmode addonDisplayNames
    generalHelp          = putStr $ showModeHelp $ mainmode addonDisplayNames
    badCommandError      = error' ("command "++rawcmd++" is not recognized, run with no command to see a list") >> exitFailure
    hasShortHelp args    = any (`elem` args) ["-h"]
    hasLongHelp args     = any (`elem` args) ["--help"]
    hasHelp args         = hasShortHelp args || hasLongHelp args
    f `orShowUsage` mode = if hasShortHelp args then putStr (showModeUsage mode) else f
    f `orShowHelp` mode  = if hasLongHelp  args then putStr (showModeHelp mode) else f
  dbgIO "processed opts" opts
  dbgIO "command matched" cmd
  dbgIO "isNullCommand" isNullCommand
  dbgIO "isInternalCommand" isInternalCommand
  dbgIO "isExternalCommand" isExternalCommand
  dbgIO "isBadCommand" isBadCommand
  d <- getCurrentDay
  dbgIO "date span from opts" (dateSpanFromOpts d $ reportopts_ opts)
  dbgIO "interval from opts" (intervalFromOpts $ reportopts_ opts)
  dbgIO "query from opts & args" (queryFromOpts d $ reportopts_ opts)
  let
    runHledgerCommand
      -- high priority flags and situations. --help should be highest priority.
      | hasShortHelp argsbeforecmd = dbgIO "" "-h before command, showing general usage" >> generalUsage
      | hasLongHelp argsbeforecmd = dbgIO "" "--help before command, showing general help" >> generalHelp
      | not (hasHelp argsaftercmd) && (hasVersion argsbeforecmd || (hasVersion argsaftercmd && isInternalCommand))
                                 = putStrLn prognameandversion
      | not (hasHelp argsaftercmd) && (hasDetailedVersion argsbeforecmd || (hasDetailedVersion argsaftercmd && isInternalCommand))
                                 = putStrLn prognameanddetailedversion
      -- \| (null externalcmd) && "binary-filename" `inRawOpts` rawopts = putStrLn $ binaryfilename progname
      -- \| "--browse-args" `elem` args     = System.Console.CmdArgs.Helper.execute "cmdargs-browser" mainmode' args >>= (putStr . show)
      | isNullCommand            = dbgIO "" "no command, showing general help" >> generalUsage
      | isBadCommand             = badCommandError

      -- internal commands
      | cmd == "activity"        = withJournalDo opts histogram       `orShowUsage` activitymode `orShowHelp` activitymode
      | cmd == "add"             = (journalFilePathFromOpts opts >>= (ensureJournalFileExists . head) >> withJournalDo opts add) `orShowUsage` addmode `orShowHelp` addmode
      | cmd == "accounts"        = withJournalDo opts accounts        `orShowUsage` accountsmode `orShowHelp` accountsmode
      | cmd == "balance"         = withJournalDo opts balance         `orShowUsage` balancemode `orShowHelp` balancemode
      | cmd == "balancesheet"    = withJournalDo opts balancesheet    `orShowUsage` balancesheetmode `orShowHelp` balancesheetmode
      | cmd == "cashflow"        = withJournalDo opts cashflow        `orShowUsage` cashflowmode `orShowHelp` cashflowmode
      | cmd == "incomestatement" = withJournalDo opts incomestatement `orShowUsage` incomestatementmode `orShowHelp` incomestatementmode
      | cmd == "print"           = withJournalDo opts print'          `orShowUsage` printmode `orShowHelp` printmode
      | cmd == "register"        = withJournalDo opts register        `orShowUsage` registermode `orShowHelp` registermode
      | cmd == "stats"           = withJournalDo opts stats           `orShowUsage` statsmode `orShowHelp` statsmode
      | cmd == "test"            = test' opts                         `orShowUsage` testmode `orShowHelp` testmode

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
      | otherwise                = optserror ("could not understand the arguments "++show args) >> exitFailure

  runHledgerCommand


-- tests_runHledgerCommand = [
--   -- "runHledgerCommand" ~: do
--   --   let opts = defreportopts{query_="expenses"}
--   --   d <- getCurrentDay
--   --   runHledgerCommand addons opts@CliOpts{command_=cmd} args

--  ]


