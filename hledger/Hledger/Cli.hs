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


SPDX-License-Identifier: GPL-3.0-or-later
Copyright (c) 2007-2025 (each year in this range) Simon Michael <simon@joyful.com> and contributors.

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
See the GNU General Public License for more details.
You should have received a copy of the GNU General Public License along with this program.
If not, see <https://www.gnu.org/licenses/>.

-}

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Unused LANGUAGE pragma" #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Hledger.Cli (
  main,
  mainmode,
  argsToCliOpts,
  -- * Re-exports
  module Hledger.Cli.CliOptions,
  module Hledger.Cli.Conf,
  module Hledger.Cli.Commands,
  module Hledger.Cli.DocFiles,
  module Hledger.Cli.Utils,
  module Hledger.Cli.Version,
  module Hledger,
  -- ** System.Console.CmdArgs.Explicit
  module CmdArgsWithoutName
)
where

#if MIN_VERSION_base(4,20,0)
import Control.Exception.Backtrace (setBacktraceMechanismState, BacktraceMechanism(..))
#endif
import Control.Monad (when, unless)
import Data.Bifunctor (second)
import Data.Char (isDigit)
import Data.Either (isRight)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List
import qualified Data.List.NonEmpty as NE
import Data.Maybe (isJust, fromMaybe, fromJust)
import Data.Text (pack, Text)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Safe
import System.Console.CmdArgs.Explicit
import System.Console.CmdArgs.Explicit as CmdArgsWithoutName hiding (Name)
import System.Environment
import System.Exit
import System.Process
import Text.Megaparsec (optional, takeWhile1P, eof)
import Text.Megaparsec.Char (char)
import Text.Printf

import Hledger
import Hledger.Cli.CliOptions
import Hledger.Cli.Conf
import Hledger.Cli.Commands
import Hledger.Cli.Commands.Run
import Hledger.Cli.DocFiles
import Hledger.Cli.Utils
import Hledger.Cli.Version


verboseDebugLevel = 8

-- | The overall cmdargs mode describing hledger's command-line options and subcommands.
-- The names of known addons are provided so they too can be recognised as commands.
mainmode addons = defMode {
  modeNames = [progname ++ " [COMMAND]"]
 ,modeArgs = ([], Just $ argsFlag "[ARGS]")
 ,modeHelp = unlines ["hledger's main command line interface. Run with no ARGS to list commands."]
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
     -- flags in named groups: (keep synced with Hledger.Cli.CliOptions.highlightHelp)
     groupNamed = cligeneralflagsgroups1
     -- flags in the unnamed group, shown last: (keep synced with dropUnsupportedOpts)
    ,groupUnnamed = confflags
     -- other flags handled but not shown in help:
    ,groupHidden = hiddenflagsformainmode
    }
 ,modeHelpSuffix = []
    -- "Examples:" :
    -- map (progname ++) [
    --  "                         list commands"
    -- ," CMD [--] [OPTS] [ARGS]  run a command (use -- with addon commands)"
    -- ,"-CMD [OPTS] [ARGS]       or run addon commands directly"
    -- ," -h                      show general usage"
    -- ," CMD -h                  show command usage"
    -- ," help [MANUAL]           show any of the hledger manuals in various formats"
    -- ]
 }
-- A dummy mode just for parsing --conf/--no-conf flags.
confflagsmode = defMode{
   modeGroupFlags=Group [] confflags []
  ,modeArgs = ([], Just $ argsFlag "")
  }

------------------------------------------------------------------------------
-- | hledger CLI's main procedure.
--
-- Here we will parse the command line, read any config file,
-- and search for hledger-* addon executables in the user's PATH,
-- then choose the appropriate builtin operation or addon operation to run,
-- then run it in the right way, usually reading input data (eg a journal) first.
--
-- When making a CLI usable and robust with main command, builtin subcommands,
-- various kinds of addon commands, and config files that add general and
-- command-specific options, while balancing circular dependencies, environment,
-- idioms, legacy, and libraries with their own requirements and limitations:
-- things get crazy, and there is a tradeoff against complexity and bug risk.
-- We try to provide the most intuitive, expressive and robust CLI that's feasible
-- while keeping the CLI processing below sufficiently comprehensible, troubleshootable,
-- and tested. It's an ongoing quest.
-- See also: Hledger.Cli.CliOptions, cli.test, addons.test, --debug and --debug=8.
--
-- Probably the biggest source of complexity here is that cmdargs can't parse
-- a command line containing undeclared flags, but this arises often with our
-- addon commands and builtin/custom commands which haven't implemented all options,
-- so we have to work hard to work around this.
-- https://github.com/ndmitchell/cmdargs/issues/36 is the wishlist issue;
-- implementing that would simplify hledger's CLI processing a lot.
--
main :: IO ()
main = handleExit $ withGhcDebug' $ do

#if MIN_VERSION_base(4,20,0)
  -- Control ghc 9.10+'s stack traces.
  -- CostCentreBacktrace   - collect cost-centre stack backtraces (only available when built with profiling)
  -- HasCallStackBacktrace - collect HasCallStack backtraces
  -- ExecutionBacktrace    - collect backtraces from native execution stack unwinding
  -- IPEBacktrace          - collect backtraces from Info Table Provenance Entries
#ifdef DEBUG
  setBacktraceMechanismState HasCallStackBacktrace True
#else
  setBacktraceMechanismState HasCallStackBacktrace False
#endif
#endif

  -- 0. let's go!

  let
    dbgio :: Show a => String -> a -> IO ()
    dbgio  = dbgIO verboseDebugLevel

  dbgio "running" prognameandversion
  starttime <- getPOSIXTime
  -- give ghc-debug a chance to take control
  when (ghcDebugMode == GDPauseAtStart) $ ghcDebugPause'
  -- try to encourage user's $PAGER to display ANSI when supported
  usecolor <- useColorOnStdout
  when usecolor setupPager
  -- Search PATH for addon commands. Exclude any that match builtin command names.
  addons <- addonCommandNames

  ---------------------------------------------------------------
  dbgio "\n1. Preliminary command line parsing" ()

  -- Naming notes:
  -- "arg" often has the most general meaning, including things like: -f, --flag, flagvalue, arg, >file, &, etc.
  -- confcmdarg, clicmdarg = the first non-flag argument, from config file or cli = the subcommand name
  -- cmdname = the full unabbreviated command name, or ""
  -- confcmdargs = arguments for the subcommand, from config file

  -- Do some argument preprocessing to help cmdargs
  cliargs <- getArgs
    >>= expandArgsAt         -- interpolate @ARGFILEs
    <&> replaceNumericFlags  -- convert -NUM to --depth=NUM
    <&> argsAddDoubleDash    -- repeat the first -- arg, as a cmdargs workaround
  let
    (clicmdarg, cliargswithoutcmd, cliargswithcmdfirst) = moveFlagsAfterCommand cliargs
    cliargswithcmdfirstwithoutclispecific = dropCliSpecificOpts cliargswithcmdfirst
    (cliargsbeforecmd, cliargsaftercmd) = second (drop 1) $ break (==clicmdarg) cliargs
  dbgio  "cli args with preprocessing" cliargs
  dbg1IO "cli args with preprocessing and options moved after command" cliargswithcmdfirst
  dbgio "cli command argument found" clicmdarg
  dbgio "cli args before command"    cliargsbeforecmd
  dbgio "cli args after command"     cliargsaftercmd
  -- dbgio "cli args without command"   cliargswithoutcmd

  ---------------------------------------------------------------
  dbgio "\n2. Read the config file if any" ()

  -- Identify any --conf/--no-conf options.
  -- Run cmdargs on just the args that look conf-related.
  let
    cliconfargs = dropUnsupportedOpts confflagsmode cliargswithoutcmd
    cliconfrawopts = cmdargsParse "for conf options" confflagsmode cliconfargs

  -- Read extra general and command-specific args/opts from the config file, if any.
  (conf, mconffile) <-
    seq cliconfrawopts $  -- order debug output
    if clicmdarg=="setup"  -- the setup command checks config files, but never uses one itself
      then return (nullconf,Nothing)
      else getConf' cliconfrawopts

  ---------------------------------------------------------------
  dbgio "\n3. Identify a command name if possible; handle version/help flags" ()

  -- Try to identify the subcommand name,
  -- from the first non-flag general argument in the config file,
  -- or if there is none, from the first non-flag argument on the command line.

  let
    confallgenargs = confLookup "general" conf & replaceNumericFlags
    -- we don't try to move flags/values preceding a command argument here;
    -- if a command name is written in the config file, it must be first
    (confcmdarg, confothergenargs) = case confallgenargs of
      a:as | not $ isFlagArg a -> (a,as)
      as                       -> ("",as)
    cmdarg = if not $ null confcmdarg then confcmdarg else clicmdarg
    nocmdprovided = null cmdarg

    -- The argument may be an abbreviated command name, which we need to expand.

    -- Run cmdargs on conf + cli args to get the full command name.
    -- If no command argument was provided, or if cmdargs fails because 
    -- the command line contains a bad flag or wrongly present/missing flag value,
    -- cmdname will be "".
    args = [confcmdarg | not $ null confcmdarg] <> cliargswithcmdfirstwithoutclispecific
    -- Actually, only scan the first non-flag argument, to avoid flag errors at this stage.
    possiblecmdarg = take 1 $ dropWhile isFlagArg args
    cmdname = stringopt "command" $ cmdargsParse "for command name" (mainmode addons) possiblecmdarg

    badcmdprovided = null cmdname && not nocmdprovided
    isaddoncmd     = not (null cmdname) && cmdname `elem` addons

    -- If it's a builtin command, get its mode and action.
    mbuiltincmdaction = findBuiltinCommand cmdname
    effectivemode = maybe (mainmode []) fst mbuiltincmdaction

  when (isJust mconffile) $ do
    unless (null confcmdarg) $
      dbg1IO "using command name argument from config file" confcmdarg
  dbgio "cli args with command first and no cli-specific opts" cliargswithcmdfirstwithoutclispecific
  dbg1IO "command found" cmdname
  dbgio "no command provided" nocmdprovided
  dbgio "bad command provided" badcmdprovided
  dbgio "is addon command" isaddoncmd

  -- If a bad command was provided, show that error now, before the full cmdargsParse attempt.
  when badcmdprovided $
    error' $ "command "++clicmdarg++" is not recognized. Run with no command to see a list."

  ---------------------------------------------------------------
  dbgio "\n4. Get applicable options/arguments from config file" ()

  -- Ignore any general opts or cli-specific opts not known to be supported by the command.
  let
    addoncmdssupportinggenopts = ["ui", "web"]  -- addons known to support hledger general options
    supportedgenargsfromconf
      | cmdname `elem` addoncmdssupportinggenopts =
          [a | a <- confothergenargs, not $ any (`isPrefixOf` a) addoncmdssupportinggenopts]
      | isaddoncmd = []
      | otherwise  = dropUnsupportedOpts effectivemode confothergenargs
    excludedgenargsfromconf = confothergenargs \\ supportedgenargsfromconf
    confcmdargs
      | null cmdname = []
      | otherwise =
          confLookup cmdname conf
          & replaceNumericFlags
          & if isaddoncmd then ("--":) else id

  when (isJust mconffile) $ do
    dbg1IO "using general args from config file" confothergenargs
    unless (null excludedgenargsfromconf) $
      dbg1IO "excluded general args from config file, not supported by this command" excludedgenargsfromconf
    dbg1IO "using subcommand args from config file" confcmdargs

  ---------------------------------------------------------------
  dbgio "\n5. Combine config file and command line args" ()

  let
    finalargs =
      [cmdarg | not $ null cmdarg]
        <> supportedgenargsfromconf
        <> confcmdargs
        <> [clicmdarg | not $ null confcmdarg]
        <> cliargswithoutcmd
      & replaceNumericFlags                -- convert any -NUM opts from the config file

  -- finalargs' <- expandArgsAt finalargs  -- expand @ARGFILEs in the config file ? don't bother
  dbg1IO "final args" finalargs

  -- Run cmdargs on command name + supported conf general args + conf subcommand args + cli args to get the final options.
  -- A bad flag or flag argument will cause the program to exit with an error here.
  let rawopts = cmdargsParse "final command line" (mainmode addons) finalargs

  ---------------------------------------------------------------
  seq rawopts $  -- order debug output
    dbgio "\n6. Select an action and run it" ()

  -- We check for the help/doc/version flags first, since they are a high priority.
  -- (A perfectionist might think they should be so high priority that adding -h
  -- to an invalid command line would show help. But cmdargs tends to fail first,
  -- preventing this, and trying to detect them without cmdargs, and always do the
  -- right thing with builtin commands and addon commands, gets much too complicated.)
  let
    helpFlag    = boolopt "help"    rawopts
    tldrFlag    = boolopt "tldr"    rawopts
    infoFlag    = boolopt "info"    rawopts
    manFlag     = boolopt "man"     rawopts
    versionFlag = boolopt "version" rawopts
    -- ignoredopts    cmd = error' $ cmd ++ " tried to read options but is not supposed to"
    ignoredjournal cmd = error' $ cmd ++ " tried to read the journal but is not supposed to"

  -- validate opts/args more and convert to CliOpts
  opts <- rawOptsToCliOpts rawopts >>= \opts0 -> return opts0{progstarttime_=starttime}
  dbg2IO "processed opts" opts
  dbgio "period from opts" (period_ . _rsReportOpts $ reportspec_ opts)
  dbgio "interval from opts" (interval_ . _rsReportOpts $ reportspec_ opts)
  dbgio "query from opts & args" (_rsQuery $ reportspec_ opts)

  -- Ensure that anything calling getArgs later will see all args, including config file args.
  -- Some things (--color, --debug, some checks in journalFinalise) are detected by unsafePerformIO,
  -- eg in Hledger.Utils.IO.progArgs, which means they aren't be seen in a config file
  -- (because many things before this point have forced the one-time evaluation of progArgs).
  withArgs (progname:finalargs) $
   if
    -- 6.1. no command and a help/doc flag found - show general help/docs
    | nocmdprovided && helpFlag -> runPager $ showModeUsage (mainmode []) ++ "\n"
    | nocmdprovided && tldrFlag -> runTldrForPage  "hledger"
    | nocmdprovided && infoFlag -> runInfoForTopic "hledger" Nothing
    | nocmdprovided && manFlag  -> runManForTopic  "hledger" Nothing

    -- 6.2. --version flag found and none of these other conditions - show version
    | versionFlag && not (isaddoncmd || helpFlag || tldrFlag || infoFlag || manFlag) -> putStrLn prognameandversion

    -- 6.3. no command found, nothing else to do - show the commands list
    | nocmdprovided -> do
        dbg1IO "no command, showing commands list" ()
        commands opts (ignoredjournal "commands")

    -- 6.4. builtin command found
    | Just (cmdmode, cmdaction) <- mbuiltincmdaction -> do
      let mmodecmdname = headMay $ modeNames cmdmode
      dbg1IO "running builtin command mode" $ fromMaybe "" mmodecmdname

      -- run the builtin command according to its type
      if
        -- 6.4.1. help/doc flag - show command help/docs
        | helpFlag  -> runPager $ showModeUsage cmdmode ++ "\n"
        | tldrFlag  -> runTldrForPage $ maybe "hledger" (("hledger-"<>)) mmodecmdname
        | infoFlag  -> runInfoForTopic "hledger" mmodecmdname
        | manFlag   -> runManForTopic "hledger"  mmodecmdname

        -- 6.4.2. builtin command which should not require or read the journal - run it
        | cmdname `elem` ["commands","demo","help","setup","test"] ->
          cmdaction opts (ignoredjournal cmdname)

        -- 6.4.3. builtin command which should create the journal if missing - do that and run it
        | cmdname `elem` ["add","import"] -> do
          ensureJournalFileExists . NE.head =<< journalFilePathFromOpts opts
          withJournalDo opts (cmdaction opts)

        -- 6.4.4. "run" and "repl" need findBuiltinCommands passed to it to avoid circular dependency in the code
        | cmdname == "run"  -> Hledger.Cli.Commands.Run.run Nothing findBuiltinCommand addons opts
        | cmdname == "repl" -> Hledger.Cli.Commands.Run.repl findBuiltinCommand addons opts

        -- 6.4.5. all other builtin commands - read the journal and if successful run the command with it
        | otherwise -> withJournalDo opts $ cmdaction opts

    -- 6.5. external addon command found - run it,
    -- passing any cli arguments written after the command name
    -- and any command-specific opts from the config file.
    -- Any "--" arguments, which sometimes must be used in the command line
    -- to hide addon-specific opts from hledger's cmdargs parsing,
    -- (and are also accepted in the config file, though not required there),
    -- will be removed.
    -- (hledger does not preserve -- arguments)
    -- Arguments written before the command name, and general opts from the config file,
    -- are not passed since we can't be sure they're supported.
    | isaddoncmd -> do
        let
          addonargs0 = filter (/="--") $ supportedgenargsfromconf <> confcmdargs <> cliargswithoutcmd
          addonargs = dropCliSpecificOpts addonargs0
          shellcmd = printf "%s-%s %s" progname cmdname (unwords' addonargs) :: String
        dbgio "addon command selected" cmdname
        dbgio "addon command arguments after removing cli-specific opts" (map quoteIfNeeded addonargs)
        dbg1IO "running addon" shellcmd
        system shellcmd >>= exitWith

    -- deprecated command found
    -- cmdname == "convert" = error' (modeHelp oldconvertmode)

    -- 6.6. something else (shouldn't happen) - show an error
    | otherwise -> usageError $
        "could not understand the arguments "++show finalargs
        <> if null confothergenargs then "" else "\ngeneral arguments added from config file: "++show confothergenargs
        <> if null confcmdargs then "" else "\ncommand arguments added from config file: "++show confcmdargs

  -- 7. And we're done.
  -- Give ghc-debug a final chance to take control.
  when (ghcDebugMode == GDPauseAtEnd) $ ghcDebugPause'

------------------------------------------------------------------------------


-- | A helper for addons/scripts: this parses hledger CliOpts from these
-- command line arguments and add-on command names, roughly how hledger main does.
-- If option parsing/validating fails, it exits the program with usageError.
-- Unlike main, this does not read extra args from a config file
-- or search for addons; to do those things, mimic the code in main for now.
argsToCliOpts :: [String] -> [String] -> IO CliOpts
argsToCliOpts args addons = do
  let
    (_, _, args0) = moveFlagsAfterCommand args
    args1 = replaceNumericFlags args0
    rawopts = cmdargsParse "for options" (mainmode addons) args1
  rawOptsToCliOpts rawopts

-- | Parse the given command line arguments/options with the given cmdargs mode,
-- after adding values to any valueless --debug flags,
-- with debug logging showing the given description of this parsing pass
-- (useful when cmdargsParse is called more than once).
-- If parsing fails, exit the program with an informative error message.
cmdargsParse :: String -> Mode RawOpts -> [String] -> RawOpts
cmdargsParse desc m args0 = process m (ensureDebugFlagHasVal args0)
  & either
    (\e -> error' $ e <> "\n* while parsing the following args, " <> desc <> ":\n*  " <> unwords (map quoteIfNeeded args0))
    (dbgMsg verboseDebugLevel ("cmdargs: parsing " <> desc <> ": " <> show args0))
  -- XXX better error message when cmdargs fails (eg spaced/quoted/malformed flag values) ?

-- | cmdargs does not allow options to appear before the subcommand argument.
-- We prefer to hide this restriction from the user, providing a more forgiving CLI.
-- So this helper tries to move any pre-command flags/options, and their values if any, after the command argument.
-- If there is a "--"" argument, only the preceding args are rearranged.
-- To be precise: pre-command options will be moved to the end of the part of the command line preceding the first -- argument.
-- The pre-command options' relative order will be preserved, but since they may be moved after post-command options,
-- the overall order of options may change.
-- XXX moving them right after the command would probably be better.
--
-- For convenience of the caller, this currently returns a triple:
-- (
--  the command, if one was found (or ""),
--  the rearranged args without the command,
--  the command followed by the rearranged args
-- )
--
-- Notes:
--
-- Detecting the command argument is tricky because of the flexibility of traditional flag syntax.
-- Short flags can be joined together, some flags can have a value or no value,
-- flags and values can be separated by =, a space, or nothing, etc.
--
-- In this context, a "flag" is an argument beginning with - or --, followed by one or more non-space characters.
-- We decide if a flag, and possibly its subsequent value argument, are movable
-- by checking these cases in order:
--
-- - it exactly matches a known short or long no-value flag; move it
-- - it exactly matches a short or long requires-value flag; move it and the following argument
-- - it exactly matches a short optional-value flag; assume these don't exist or we don't have any
-- - it exactly matches a long optional-value flag; assume there's no value, move it
-- - it begins with a short requires-value flag; the value is joined to it, move it
-- - it begins with a long requires-value flag followed by =; likewise
-- - it begins with a long optional-value flag followed by =; likewise
--
-- This hackery increases the risk of misleading errors, bugs, and confusion.
-- It should be fairly robust now, being aware of all builtin flags.
-- The main tests are in hledger/test/cli/cli.test, but they are not exhaustive.
--
-- All general and builtin command flags (and their values) will be moved. It's clearer to
-- write command flags after the command, but if not we'll handle it (for greater robustness).
--
-- Long flags should be spelled in full; abbreviated long flags might not get moved.
--
-- Unknown flags (from addons) are assumed to be valueless or have a joined value,
-- and will be moved - but later rejected by cmdargs.
-- Instead these should be written to the right of a "--" argument, which hides them.
--
moveFlagsAfterCommand :: [String] -> (String, [String], [String])
moveFlagsAfterCommand args =
  case moveFlagAndVal (as1, []) of
    ([],as1')                    -> ("", as, as) where as = as1' <> as2
    (unmoved@(('-':_):_), moved) -> ("", as, as) where as = unmoved <> moved <> as2
    (cmdarg:unmoved, moved)      -> (cmdarg, as, cmdarg:as) where as = unmoved <> moved <> as2
  where
    (as1, as2) = break (== "--") args
    -- Move the next argument to the end if it is a movable flag, along with its subsequent value argument if any.
    moveFlagAndVal :: ([String], [String]) -> ([String], [String])
    moveFlagAndVal ((a:b:cs), moved) =
      case isMovableFlagArg a (Just b) of
        2 -> dbgMsg lvl ("moving 2: "<>a<>" "<>b) $ moveFlagAndVal (cs, moved++[a,b])
        1 -> dbgMsg lvl ("moving 1: "<>a) $ moveFlagAndVal (b:cs, moved++[a])
        _ -> (a:b:cs, moved)
    moveFlagAndVal ([a], moved) =
      case isMovableFlagArg a Nothing of
        1 -> dbgMsg lvl ("moving 1: "<>a) ([], moved++[a])
        _ -> ([a], moved)
    moveFlagAndVal ([], moved) = ([], moved)
    lvl = 8

-- Is this a short or long flag argument that should be moved,
-- and is its following argument a value that also should be moved ?
-- Returns:
-- 0 (not a flag; don't move this argument)
-- 1 (a valueless flag, or a long flag with joined argument, or multiple joined valueless short flags; move this argument)
-- 2 (a short or long flag with a value in the next argument; move this and next argument).
isMovableFlagArg :: String -> Maybe String -> Int
isMovableFlagArg a1 ma2
  | a1 `elem` noValFlagArgs  = 1  -- short or long no-val flag
  | a1 == "--debug" && isJust ma2 && not (isDebugValue $ fromJust ma2) = 1  --debug without a value
  | a1 `elem` reqValFlagArgs = 2  -- short or long req-val flag (or --debug) with a separate value
  | a1 `elem` optValFlagArgs = 1  -- long (or short ?) opt-val flag, assume no value
  | any (`isPrefixOf` a1) shortReqValFlagArgs = 1  -- short req-val flag with a joined value
        -- or possibly multiple joined valueless short flags, we won't move those correctly
  | any (`isPrefixOf` a1) longReqValFlagArgs_ = 1  -- long req-val flag (or --debug) with a joined value
  | any (`isPrefixOf` a1) longOptValFlagArgs_ = 1  -- long opt-val flag with a joined value
  -- | isLongFlagArg a1 && any (takeWhile (/='=') `isPrefixOf`) longReqValFlagArgs_ ... -- try to move abbreviated long flags ?
  | isFlagArg a1 = 1    -- an addon flag (or mistyped flag) we don't know, assume no value or value is joined
  | otherwise = 0    -- not a flag

-- Is this string a valid --debug value ?
isDebugValue s = isRight $ parsewith isdebugvalp $ pack s
  where isdebugvalp = optional (char '-') >> takeWhile1P Nothing isDigit <* eof :: TextParser m Text

-- Flag arguments are command line arguments beginning with - or --
-- (followed by a short of long flag name, and possibly joined short flags or a joined value).
isFlagArg, isShortFlagArg, isLongFlagArg :: String -> Bool
isFlagArg a = isShortFlagArg a || isLongFlagArg a

isShortFlagArg ('-':c:_) = c /= '-'
isShortFlagArg _         = False

isLongFlagArg ('-':'-':_:_) = True
isLongFlagArg _             = False

-- | Add the leading hyphen(s) to a short or long flag name.
toFlagArg :: Name -> String
toFlagArg f = if length f == 1 then "-"++f else "--"++f

-- | Flatten a possibly multi-named Flag to (name, FlagInfo) pairs.
toFlagInfos :: Flag RawOpts -> [(Name, FlagInfo)]
toFlagInfos f = [(n,i) | let i = flagInfo f, n <- flagNames f]

-- | Is this flag's value optional ?
isOptVal :: FlagInfo -> Bool
isOptVal = \case
  FlagOpt _     -> True
  FlagOptRare _ -> True
  _             -> False

-- | All the general flags defined in hledger's main mode.
generalFlags :: [Flag RawOpts]
generalFlags = concatMap snd groupNamed <> groupHidden <> groupUnnamed
  where Group{..} = modeGroupFlags $ mainmode []  

-- | All the general flag names.
generalFlagNames :: [Name]
generalFlagNames = concatMap flagNames generalFlags

-- | All hledger's builtin subcommand-specific flags.
commandFlags :: [Flag RawOpts]
commandFlags = concatMap (groupUnnamed.modeGroupFlags) commandModes
  where
    commandModes = concatMap snd groupNamed <> groupUnnamed <> groupHidden
      where Group{..} = modeGroupModes $ mainmode []

-- | The names of general options flags, grouped by whether they expect a value.
-- There may be some overlaps with command flag names.
noValGeneralFlagNames, reqValGeneralFlagNames, optValGeneralFlagNames :: [Name]
noValGeneralFlagNames  = [f | (f,i) <- concatMap toFlagInfos generalFlags, i == FlagNone]
reqValGeneralFlagNames = [f | (f,i) <- concatMap toFlagInfos generalFlags, i == FlagReq]
optValGeneralFlagNames = [f | (f,i) <- concatMap toFlagInfos generalFlags, isOptVal i]

-- | The names of builtin subcommand flags, grouped by whether they expect a value.
-- There may be some overlaps with general flag names.
noValCommandFlagNames, reqValCommandFlagNames, optValCommandFlagNames :: [Name]
noValCommandFlagNames  = [f | (f,i) <- concatMap toFlagInfos commandFlags, i == FlagNone]
reqValCommandFlagNames = [f | (f,i) <- concatMap toFlagInfos commandFlags, i == FlagReq]
optValCommandFlagNames = [f | (f,i) <- concatMap toFlagInfos commandFlags, isOptVal i]

-- All flag arguments understood by hledger cli and builtin commands, grouped by whether they expect a value.
-- Any command flags which have the same name as a general flag are excluded.
noValFlagArgs  = map toFlagArg $ noValGeneralFlagNames  `union` (noValCommandFlagNames  \\ generalFlagNames)
reqValFlagArgs = map toFlagArg $ reqValGeneralFlagNames `union` (reqValCommandFlagNames \\ generalFlagNames)
optValFlagArgs = map toFlagArg $ optValGeneralFlagNames `union` (optValCommandFlagNames \\ generalFlagNames)

-- Short flag args that expect a required value.
shortReqValFlagArgs = filter isShortFlagArg reqValFlagArgs

-- Long flag args that expect a required value, with = appended.
longReqValFlagArgs_ = map (++"=") $ filter isLongFlagArg reqValFlagArgs

-- Long flag args that expect an optional value, with = appended.
longOptValFlagArgs_ = map (++"=") $ filter isLongFlagArg optValFlagArgs ++ ["--debug"]

-- Drop any arguments which look like cli-specific options (--no-conf, --conf CONFFILE, etc.)
-- Keep synced with mainmode's groupUnnamed.
dropCliSpecificOpts :: [String] -> [String]
dropCliSpecificOpts = \case
  "--conf":_:as                   -> dropCliSpecificOpts as
  a:as | "--conf=" `isPrefixOf` a -> dropCliSpecificOpts as
  "--no-conf":as                  -> dropCliSpecificOpts as
  "-n":as                         -> dropCliSpecificOpts as
  a:as                            -> a:dropCliSpecificOpts as
  []                              -> []

-- | Given a hledger cmdargs mode and a list of command line arguments, try to drop any of the
-- arguments which seem to be flags not supported by this mode. Also drop their values if any.
--
-- >>> dropUnsupportedOpts confflagsmode ["--debug","1","-f","file"]
-- []
-- >>> dropUnsupportedOpts confflagsmode ["--debug","-f","file"]
-- []
dropUnsupportedOpts :: Mode RawOpts -> [String] -> [String]
dropUnsupportedOpts m = \case
  []   -> []
  "--debug":a:as | not (m `supportsFlag` "debug") ->
    go $ if isDebugValue a then as else a:as
  a:as -> if
    | isLongFlagArg a,
      let f = takeWhile (/='=') a,
      let as' = if isReqValFlagArg f && '=' `notElem` a then drop 1 as else as
      -> if m `supportsFlag` f then a : go as else go as'
    | isShortFlagArg a,
      let f = take 2 a,
      let as' = if isReqValFlagArg f && length a == 2 then drop 1 as else as
      -> if m `supportsFlag` f then a : go as else go as'
    | otherwise -> a : dropUnsupportedOpts m as
  where
    go = dropUnsupportedOpts m
    isReqValFlagArg = (`elem` reqValFlagArgs)
    supportsFlag m1 flagarg = elem flagarg $ map toFlagArg $ concatMap flagNames $ modeAndSubmodeFlags m1

-- | Get all the flags defined in a mode or its immediate subcommands,
-- whether in named, unnamed or hidden groups.
-- Does not recurse into subsubcommands,
-- and does not deduplicate (general flags are repeated on all hledger subcommands).
modeAndSubmodeFlags :: Mode a -> [Flag a]
modeAndSubmodeFlags m@Mode{modeGroupModes=Group{..}} =
  modeFlags m <> concatMap modeFlags (concatMap snd groupNamed <> groupUnnamed <> groupHidden)

-- unit tests (tests_Hledger_Cli) are defined in Hledger.Cli.Commands
