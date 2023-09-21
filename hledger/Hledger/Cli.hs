{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Unused LANGUAGE pragma" #-}

{-|

This is the root module of the @hledger@ package,
providing hledger's command-line interface.
The main function,
commands,
command-line options,
and utilities useful to other hledger command-line programs
are exported.
It also re-exports hledger-lib:Hledger
and cmdargs:System.Concole.CmdArgs.Explicit

See also:

- hledger-lib:Hledger
- [The README files](https://github.com/search?q=repo%3Asimonmichael%2Fhledger+path%3A**%2FREADME*&type=code&ref=advsearch)
- [The high-level developer docs](https://hledger.org/dev.html)

== About

hledger - a fast, reliable, user-friendly plain text accounting tool.
Copyright (c) 2007-2023 Simon Michael <simon@joyful.com> and contributors
Released under GPL version 3 or later.

hledger is a Haskell rewrite of John Wiegley's "ledger".  
It generates financial reports from a plain text general journal.
You can use the command line:

> $ hledger

or ghci:

> $ make ghci
> ghci> Right j <- runExceptT $ readJournalFile definputopts "examples/sample.journal"  -- or: j <- defaultJournal
> ghci> :t j
> j :: Journal
> ghci> stats defcliopts j
> Main file                : examples/sample.journal
> Included files           : 
> Transactions span        : 2008-01-01 to 2009-01-01 (366 days)
> Last transaction         : 2008-12-31 (733772 days from now)
> Transactions             : 5 (0.0 per day)
> Transactions last 30 days: 0 (0.0 per day)
> Transactions last 7 days : 0 (0.0 per day)
> Payees/descriptions      : 5
> Accounts                 : 8 (depth 3)
> Commodities              : 1 ($)
> Market prices            : 0 ()
> 
> Run time (throughput)    : 1695276900.00s (0 txns/s)
> ghci> balance defcliopts j
>                   $1  assets:bank:saving
>                  $-2  assets:cash
>                   $1  expenses:food
>                   $1  expenses:supplies
>                  $-1  income:gifts
>                  $-1  income:salary
>                   $1  liabilities:debts
> --------------------
>                    0  
> ghci> 

etc.

-}

module Hledger.Cli (
  prognameandversion,
  versionString,
  main,
  mainmode,
  argsToCliOpts,
  -- * Re-exports
  module Hledger.Cli.CliOptions,
  module Hledger.Cli.Commands,
  module Hledger.Cli.DocFiles,
  module Hledger.Cli.Utils,
  module Hledger.Cli.Version,
  module Hledger,
  -- ** System.Console.CmdArgs.Explicit
  module System.Console.CmdArgs.Explicit,
)
where

import Control.Monad (when)
import Data.List
import Safe
import qualified System.Console.CmdArgs.Explicit as C
import System.Environment
import System.Exit
import System.FilePath
import System.Process
import Text.Printf

import Data.Time.Clock.POSIX (getPOSIXTime)


import GitHash (tGitInfoCwdTry)
import System.Console.CmdArgs.Explicit hiding (Name) -- don't clash with hledger-ui

import Hledger
import Hledger.Cli.CliOptions
import Hledger.Cli.Commands
import Hledger.Cli.DocFiles
import Hledger.Cli.Utils
import Hledger.Cli.Version


-- | The program name and version string for this build of the hledger tool,
-- including any git info available at build time.
prognameandversion :: String
prognameandversion = versionString progname packageversion

-- | A helper to generate the best version string we can from the given 
-- program name and package version strings, current os and architecture,
-- and any git info available at build time (commit hash, commit date, branch
-- name, patchlevel since latest release tag for that program's package).
-- Typically called for programs "hledger", "hledger-ui", or "hledger-web".
--
-- The git info changes whenever any file in the repository changes. 
-- Keeping this template haskell call here and not down in Hledger.Cli.Version
-- helps reduce the number of modules recompiled.
versionString :: ProgramName -> PackageVersion -> String
versionString = versionStringWith $$tGitInfoCwdTry


-- | The overall cmdargs mode describing hledger's command-line options and subcommands.
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
   ,groupHidden = map fst builtinCommands ++ map addonCommandMode addons
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
 ,modeHelpSuffix = "Examples:" :
    map (progname ++) [
     "                         list commands"
    ," CMD [--] [OPTS] [ARGS]  run a command (use -- with addon commands)"
    ,"-CMD [OPTS] [ARGS]       or run addon commands directly"
    ," -h                      show general usage"
    ," CMD -h                  show command usage"
    ," help [MANUAL]           show any of the hledger manuals in various formats"
    ]
 }

-- | Let's go!
main :: IO ()
main = do
  starttime <- getPOSIXTime
  -- try to encourage user's $PAGER to properly display ANSI
  when useColorOnStdout setupPager

  -- Choose and run the appropriate internal or external command based
  -- on the raw command-line arguments, cmdarg's interpretation of
  -- same, and hledger-* executables in the user's PATH. A somewhat
  -- complex mishmash of cmdargs and custom processing, hence all the
  -- debugging support and tests. See also Hledger.Cli.CliOptions and
  -- command-line.test.

  -- some preliminary (imperfect) argument parsing to supplement cmdargs
  args <- getArgs >>= expandArgsAt
  let
    args'                = moveFlagsAfterCommand $ replaceNumericFlags args
    isFlag               = ("-" `isPrefixOf`)
    isNonEmptyNonFlag s  = not (isFlag s) && not (null s)
    rawcmd               = headDef "" $ takeWhile isNonEmptyNonFlag args'
    isNullCommand        = null rawcmd
    (argsbeforecmd, argsaftercmd') = break (==rawcmd) args
    argsaftercmd         = drop 1 argsaftercmd'
    dbgIO :: Show a => String -> a -> IO ()
    dbgIO = ptraceAtIO 8

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
  opts' <- argsToCliOpts args addons
  let opts = opts'{progstarttime_=starttime}

  -- select an action and run it.
  let
    cmd                  = command_ opts -- the full matched internal or external command name, if any
    isInternalCommand    = cmd `elem` builtinCommandNames -- not (null cmd) && not (cmd `elem` addons)
    isExternalCommand    = not (null cmd) && cmd `elem` addons -- probably
    isBadCommand         = not (null rawcmd) && null cmd
    hasVersion           = ("--version" `elem`)
    printUsage           = pager $ showModeUsage $ mainmode addons
    badCommandError      = error' ("command "++rawcmd++" is not recognized, run with no command to see a list") >> exitFailure  -- PARTIAL:
    hasHelpFlag args1     = any (`elem` args1) ["-h","--help"]
    hasManFlag args1      = (`elem` args1) "--man"
    hasInfoFlag args1     = (`elem` args1) "--info"
    f `orShowHelp` mode1
      | hasHelpFlag args = pager $ showModeUsage mode1
      | hasInfoFlag args = runInfoForTopic "hledger" (headMay $ modeNames mode1)
      | hasManFlag args  = runManForTopic "hledger" (headMay $ modeNames mode1)
      | otherwise        = f
      -- where
      --   lastdocflag
  dbgIO "processed opts" opts
  dbgIO "command matched" cmd
  dbgIO "isNullCommand" isNullCommand
  dbgIO "isInternalCommand" isInternalCommand
  dbgIO "isExternalCommand" isExternalCommand
  dbgIO "isBadCommand" isBadCommand
  dbgIO "period from opts" (period_ . _rsReportOpts $ reportspec_ opts)
  dbgIO "interval from opts" (interval_ . _rsReportOpts $ reportspec_ opts)
  dbgIO "query from opts & args" (_rsQuery $ reportspec_ opts)
  let
    journallesserror = error $ cmd++" tried to read the journal but is not supposed to"
    runHledgerCommand
      -- high priority flags and situations. -h, then --help, then --info are highest priority.
      | isNullCommand && hasHelpFlag args = dbgIO "" "-h/--help with no command, showing general help" >> printUsage
      | isNullCommand && hasInfoFlag args = dbgIO "" "--info with no command, showing general info manual" >> runInfoForTopic "hledger" Nothing
      | isNullCommand && hasManFlag args  = dbgIO "" "--man with no command, showing general man page" >> runManForTopic "hledger" Nothing
      | not (isExternalCommand || hasHelpFlag args || hasInfoFlag args || hasManFlag args)
        && (hasVersion args) --  || (hasVersion argsaftercmd && isInternalCommand))
                                 = putStrLn prognameandversion
      -- \| (null externalcmd) && boolopt "binary-filename" rawopts = putStrLn $ binaryfilename progname
      -- \| "--browse-args" `elem` args     = System.Console.CmdArgs.Helper.execute "cmdargs-browser" mainmode' args >>= (putStr . show)
      | isNullCommand            = dbgIO "" "no command, showing commands list" >> printCommandsList prognameandversion addons
      | isBadCommand             = badCommandError

      -- builtin commands
      | Just (cmdmode, cmdaction) <- findBuiltinCommand cmd =
        (case True of
           -- these commands should not require or read the journal
          _ | cmd `elem` ["demo","help","test"] -> cmdaction opts journallesserror
          -- these commands should create the journal if missing
          _ | cmd `elem` ["add","import"] -> do
            ensureJournalFileExists . head =<< journalFilePathFromOpts opts
            withJournalDo opts (cmdaction opts)
          -- other commands read the journal and should fail if it's missing
          _ -> withJournalDo opts (cmdaction opts)
        )
        `orShowHelp` cmdmode

      -- addon commands
      | isExternalCommand = do
          let externalargs = argsbeforecmd ++ filter (/="--") argsaftercmd
          let shellcmd = printf "%s-%s %s" progname cmd (unwords' externalargs) :: String
          dbgIO "external command selected" cmd
          dbgIO "external command arguments" (map quoteIfNeeded externalargs)
          dbgIO "running shell command" shellcmd
          system shellcmd >>= exitWith

      -- deprecated commands
      -- cmd == "convert"         = error' (modeHelp oldconvertmode) >> exitFailure

      -- shouldn't reach here
      | otherwise                = usageError ("could not understand the arguments "++show args) >> exitFailure

  runHledgerCommand

-- | Parse hledger CLI options from these command line arguments and
-- add-on command names, or raise any error.
argsToCliOpts :: [String] -> [String] -> IO CliOpts
argsToCliOpts args addons = do
  let
    args'        = moveFlagsAfterCommand $ replaceNumericFlags args
    cmdargsopts  = either usageError id $ C.process (mainmode addons) args'
  rawOptsToCliOpts cmdargsopts

-- | A hacky workaround for cmdargs not accepting flags before the
-- subcommand name: try to detect and move such flags after the
-- command.  This allows the user to put them in either position.
-- The order of options is not preserved, but that should be ok.
--
-- Since we're not parsing flags as precisely as cmdargs here, this is
-- imperfect. We make a decent effort to:
-- - move all no-argument help/input/report flags
-- - move all required-argument help/input/report flags along with their values, space-separated or not
-- - ensure --debug has an argument (because.. "or this all goes to hell")
-- - not confuse things further or cause misleading errors.
moveFlagsAfterCommand :: [String] -> [String]
moveFlagsAfterCommand args = moveArgs $ ensureDebugHasArg args
  where
    moveArgs args1 = insertFlagsAfterCommand $ moveArgs' (args1, [])
      where
        -- -f FILE ..., --alias ALIAS ...
        moveArgs' ((f:v:a:as), flags) | isMovableReqArgFlag f, isValue v       = moveArgs' (a:as, flags ++ [f,v])
        -- -fFILE ..., --alias=ALIAS ...
        moveArgs' ((fv:a:as), flags)  | isMovableArgFlagAndValue fv            = moveArgs' (a:as, flags ++ [fv])
        -- -f(missing arg)
        moveArgs' ((f:a:as), flags)   | isMovableReqArgFlag f, not (isValue a) = moveArgs' (a:as, flags ++ [f])
        -- -h ..., --version ...
        moveArgs' ((f:a:as), flags)   | isMovableNoArgFlag f                   = moveArgs' (a:as, flags ++ [f])
        -- anything else
        moveArgs' (as, flags) = (as, flags)

        insertFlagsAfterCommand ([],           flags) = flags
        insertFlagsAfterCommand (command1:args2, flags) = [command1] ++ flags ++ args2

isMovableNoArgFlag a  = "-" `isPrefixOf` a && dropWhile (=='-') a `elem` optargflagstomove ++ noargflagstomove

isMovableReqArgFlag a = "-" `isPrefixOf` a && dropWhile (=='-') a `elem` reqargflagstomove

isMovableArgFlagAndValue ('-':'-':a:as) = case break (== '=') (a:as) of
    (f:fs,_:_) -> (f:fs) `elem` optargflagstomove ++ reqargflagstomove
    _          -> False
isMovableArgFlagAndValue ('-':shortflag:_:_) = [shortflag] `elem` reqargflagstomove
isMovableArgFlagAndValue _ = False

isValue "-"     = True
isValue ('-':_) = False
isValue _       = True

flagstomove = inputflags ++ reportflags ++ helpflags
noargflagstomove  = concatMap flagNames $ filter ((==FlagNone).flagInfo) flagstomove
reqargflagstomove = -- filter (/= "debug") $
                    concatMap flagNames $ filter ((==FlagReq ).flagInfo) flagstomove
optargflagstomove = concatMap flagNames $ filter (isFlagOpt   .flagInfo) flagstomove
  where
    isFlagOpt = \case
      FlagOpt     _ -> True
      FlagOptRare _ -> True
      _             -> False


-- unit tests (tests_Hledger_Cli) are defined in Hledger.Cli.Commands
