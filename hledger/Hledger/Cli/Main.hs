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
import Hledger.Cli.Options
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
    ,groupHidden = inputflags -- included here so they'll not raise a confusing error if present with no COMMAND
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
  rawOptsToCliOpts cmdargsopts' >>= checkCliOpts

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
moveFlagsAfterCommand args = move args
  where
    move (f:a:as)           | isMovableNoArgFlag f           = (move $ a:as) ++ [f]
    move (f:v:a:as)         | isMovableReqArgFlag f          = (move $ a:as) ++ [f,v]
    move (fv:a:as)          | isMovableReqArgFlagAndValue fv = (move $ a:as) ++ [fv]
    move ("--debug":v:a:as) | not (null v) && all isDigit v  = (move $ a:as) ++ ["--debug",v]
    move ("--debug":a:as)                                    = (move $ a:as) ++ ["--debug"]
    move (fv@('-':'-':'d':'e':'b':'u':'g':'=':_):a:as)       = (move $ a:as) ++ [fv]
    move as = as

    isMovableNoArgFlag a  = "-" `isPrefixOf` a && dropWhile (=='-') a `elem` noargflagstomove
    isMovableReqArgFlag a = "-" `isPrefixOf` a && dropWhile (=='-') a `elem` reqargflagstomove
    isMovableReqArgFlagAndValue ('-':'-':a:as) = case break (== '=') (a:as) of (f:fs,_) -> (f:fs) `elem` reqargflagstomove
                                                                               _        -> False
    isMovableReqArgFlagAndValue ('-':f:_:_) = [f] `elem` reqargflagstomove
    isMovableReqArgFlagAndValue _ = False

    noargflagstomove  = concatMap flagNames $ filter ((==FlagNone).flagInfo) flagstomove
    reqargflagstomove = concatMap flagNames $ filter ((==FlagReq ).flagInfo) flagstomove
    flagstomove = inputflags ++ helpflags

-- | Let's go.
main :: IO ()
main = do

  -- Choose and run the appropriate internal or external command based
  -- on the raw command-line arguments, cmdarg's interpretation of
  -- same, and hledger-* executables in the user's PATH. A somewhat
  -- complex mishmash of cmdargs and custom processing, hence all the
  -- debugging support and tests. See also Hledger.Cli.Options and
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
    dbgM :: Show a => String -> a -> IO ()
    dbgM = dbgAtM 2

  dbgM "running" prognameandversion
  dbgM "raw args" args
  dbgM "raw args rearranged for cmdargs" args'
  dbgM "raw command is probably" rawcmd
  dbgM "raw args before command" argsbeforecmd
  dbgM "raw args after command" argsaftercmd

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
    hasHelp args         = any (`elem` args) ["--help","-h","-?"]
    hasVersion           = ("--version" `elem`)
    generalHelp          = putStr $ showModeHelp $ mainmode addonDisplayNames
    version              = putStrLn prognameandversion
    badCommandError      = error' ("command "++rawcmd++" is not recognized, run with no command to see a list") >> exitFailure
    f `orShowHelp` mode  = if hasHelp args then putStr (showModeHelp mode) else f
  dbgM "processed opts" opts
  dbgM "command matched" cmd
  dbgM "isNullCommand" isNullCommand
  dbgM "isInternalCommand" isInternalCommand
  dbgM "isExternalCommand" isExternalCommand
  dbgM "isBadCommand" isBadCommand
  d <- getCurrentDay
  dbgM "date span from opts" (dateSpanFromOpts d $ reportopts_ opts)
  dbgM "interval from opts" (intervalFromOpts $ reportopts_ opts)
  dbgM "query from opts & args" (queryFromOpts d $ reportopts_ opts)
  let
    runHledgerCommand
      -- high priority flags and situations. --help should be highest priority.
      | hasHelp argsbeforecmd    = dbgM "" "--help before command, showing general help" >> generalHelp
      | not (hasHelp argsaftercmd) && (hasVersion argsbeforecmd || (hasVersion argsaftercmd && isInternalCommand))
                                 = version
      -- \| (null externalcmd) && "binary-filename" `inRawOpts` rawopts = putStrLn $ binaryfilename progname
      -- \| "--browse-args" `elem` args     = System.Console.CmdArgs.Helper.execute "cmdargs-browser" mainmode' args >>= (putStr . show)
      | isNullCommand            = dbgM "" "no command, showing general help" >> generalHelp
      | isBadCommand             = badCommandError

      -- internal commands
      | cmd == "activity"        = withJournalDo opts histogram       `orShowHelp` activitymode
      | cmd == "add"             = (journalFilePathFromOpts opts >>= ensureJournalFileExists >> withJournalDo opts add) `orShowHelp` addmode
      | cmd == "accounts"        = withJournalDo opts accounts        `orShowHelp` accountsmode
      | cmd == "balance"         = withJournalDo opts balance         `orShowHelp` balancemode
      | cmd == "balancesheet"    = withJournalDo opts balancesheet    `orShowHelp` balancesheetmode
      | cmd == "cashflow"        = withJournalDo opts cashflow        `orShowHelp` cashflowmode
      | cmd == "incomestatement" = withJournalDo opts incomestatement `orShowHelp` incomestatementmode
      | cmd == "print"           = withJournalDo opts print'          `orShowHelp` printmode
      | cmd == "register"        = withJournalDo opts register        `orShowHelp` registermode
      | cmd == "stats"           = withJournalDo opts stats           `orShowHelp` statsmode
      | cmd == "test"            = test' opts                         `orShowHelp` testmode

      -- an external command
      | isExternalCommand = do
          let externalargs = filter (not.(=="--")) argsaftercmd
          let shellcmd = printf "%s-%s %s" progname cmd (unwords' externalargs) :: String
          dbgM "external command selected" cmd
          dbgM "external command arguments" (map quoteIfNeeded externalargs)
          dbgM "running shell command" shellcmd
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


