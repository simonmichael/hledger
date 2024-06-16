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

{-# LANGUAGE LambdaCase #-}

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Unused LANGUAGE pragma" #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}

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
  module System.Console.CmdArgs.Explicit,
)
where

import Control.Monad (when)
import Data.List
import qualified Data.List.NonEmpty as NE
import Data.Time.Clock.POSIX (getPOSIXTime)
import Safe
import System.Console.CmdArgs.Explicit hiding (Name) -- don't clash with hledger-ui
import qualified System.Console.CmdArgs.Explicit as CmdArgs
import System.Environment
import System.Exit
import System.FilePath
import System.Process
import Text.Printf

import Hledger
import Hledger.Cli.CliOptions
import Hledger.Cli.Conf
import Hledger.Cli.Commands
import Hledger.Cli.DocFiles
import Hledger.Cli.Utils
import Hledger.Cli.Version
import Data.Bifunctor (second)
import Data.Function ((&))
import Data.Functor ((<&>))
import Control.Monad.Extra (unless)
import Data.List.Extra (nubSort)


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
     -- flags in the unnamed group, shown last:
    ,groupUnnamed = []
     -- flags handled but not shown in the help:
    ,groupHidden =
        [detailedversionflag]
        -- ++ inputflags -- included here so they'll not raise a confusing error if present with no COMMAND
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

------------------------------------------------------------------------------
-- | hledger CLI's main procedure.
--
-- Here we will parse the command line, read any config file,
-- and search for hledger-* addon executables in the user's PATH,
-- then choose the appropriate builtin operation or addon operation to run,
-- then run it in the right way, usually reading input data (eg a journal) first.
--
-- When making a CLI usable and robust with main command, builtin subcommands,
-- and various kinds of addon commands, while balancing UX, environment, idioms,
-- legacy, and language and libraries and workarounds with their own requirements
-- and limitations, things get complicated and bugs can easily creep in.
-- So try to keep the processing below reasonably manageable, testable and clear.
-- See also: Hledger.Cli.CliOptions, cli.test, and --debug=8.
--
main :: IO ()
main = withGhcDebug' $ do

  -- let's go!
  let
    dbgIO :: Show a => String -> a -> IO ()  -- this signature is needed
    dbgIO = ptraceAtIO 8
  dbgIO "running" prognameandversion

  starttime <- getPOSIXTime

  -- give ghc-debug a chance to take control
  when (ghcDebugMode == GDPauseAtStart) $ ghcDebugPause'

  -- try to encourage user's $PAGER to display ANSI when supported
  when useColorOnStdout setupPager

  -- do some preliminary argument parsing to help cmdargs
  cliargs <- getArgs
    >>= expandArgsAt         -- interpolate @ARGFILEs
    <&> replaceNumericFlags  -- convert -NUM to --depth=NUM
  let
    cliargswithcmdfirst  = cliargs & moveFlagsAfterCommand
    isNonEmptyNonFlag s  = not $ null s || "-" `isPrefixOf` s
    clicmdarg            = headDef "" $ takeWhile isNonEmptyNonFlag cliargswithcmdfirst
    nocmdprovided        = null clicmdarg
    (cliargsbeforecmd, cliargsaftercmd) = second (drop 1) $ break (==clicmdarg) cliargs

  dbgIO "cli args" cliargs
  dbgIO "cli args with command argument first, if any" cliargswithcmdfirst
  dbgIO "command argument found"  clicmdarg
  dbgIO "cli args before command" cliargsbeforecmd
  dbgIO "cli args after command"  cliargsaftercmd

  -- Search PATH for addon commands. Exclude any that match builtin command names.
  addons <- hledgerAddons <&> filter (not . (`elem` builtinCommandNames) . dropExtension) 

  -- Now, so we can look for command-specific options in config files,
  -- try to identify the command's full name (clicmdarg may be an abbreviation).
  -- For this we do a preliminary parse of the user's arguments with cmdargs.
  -- If no command was provided, or if the command line contains a bad flag
  -- or a wrongly present/missing flag argument, cmd will be "".
  let
    cmd = parseArgsWithCmdargs cliargswithcmdfirst addons & either (const "") (stringopt "command")
    badcmdprovided = null cmd && not nocmdprovided
    isaddoncmd = not (null cmd) && cmd `elem` addons
    -- isbuiltincmd = cmd `elem` builtinCommandNames

  dbgIO "nocmdprovided" nocmdprovided    
  dbgIO "badcmdprovided" badcmdprovided
  dbgIO "cmd found" cmd
  dbgIO "isaddoncmd" isaddoncmd

  -- Read any extra general args/opts, and any command-specific ones, from a config file.
  -- And insert them before the user's args, with adjustments, to get the final args.
  conf <- getConf
  let
    genargsfromconf = confArgsFor "general" conf
    cmdargsfromconf = if null cmd then [] else confArgsFor cmd conf
    argsfromcli = drop 1 cliargswithcmdfirst
    finalargs =  -- (avoid breaking vs code haskell highlighting..)
      (if null clicmdarg then [] else [clicmdarg]) <> genargsfromconf <> cmdargsfromconf <> argsfromcli
      & replaceNumericFlags                -- convert any -NUM opts from the config file
  -- finalargs' <- expandArgsAt finalargs  -- expand any @ARGFILEs from the config file ? don't bother

  unless (null genargsfromconf) $ dbgIO ("extra general args from config file")   genargsfromconf
  unless (null cmdargsfromconf) $ dbgIO ("extra "<>cmd<>" args from config file") cmdargsfromconf
  dbgIO "final args" finalargs

  -- Now parse these in full, first to RawOpts with cmdargs, then to hledger CliOpts.
  -- At this point a bad flag or flag argument will cause the program to exit with an error.
  let rawopts = either usageError id $ parseArgsWithCmdargs finalargs addons
  opts0 <- rawOptsToCliOpts rawopts
  let opts = opts0{progstarttime_=starttime}

  dbgIO "processed opts" opts
  dbgIO "period from opts" (period_ . _rsReportOpts $ reportspec_ opts)
  dbgIO "interval from opts" (interval_ . _rsReportOpts $ reportspec_ opts)
  dbgIO "query from opts & args" (_rsQuery $ reportspec_ opts)

  -- Finally, select an action and run it.

  -- We check for the help/doc/version flags first, since they are a high priority.
  -- (A perfectionist might think they should be so high priority that adding -h
  -- to an invalid command line would show help. But cmdargs tends to fail first,
  -- preventing this, and trying to detect them without cmdargs, and always do the
  -- right thing with builtin commands and addon commands, gets much too complicated.)
  let
    helpFlag    = boolopt "help"    $ rawopts_ opts
    tldrFlag    = boolopt "tldr"    $ rawopts_ opts
    infoFlag    = boolopt "info"    $ rawopts_ opts
    manFlag     = boolopt "man"     $ rawopts_ opts
    versionFlag = boolopt "version" $ rawopts_ opts

  if
    -- no command and a help/doc flag found - show general help/docs
    | nocmdprovided && helpFlag -> pager $ showModeUsage (mainmode []) ++ "\n"
    | nocmdprovided && tldrFlag -> runTldrForPage  "hledger"
    | nocmdprovided && infoFlag -> runInfoForTopic "hledger" Nothing
    | nocmdprovided && manFlag  -> runManForTopic  "hledger" Nothing

    -- --version flag found and none of these other conditions - show version
    | versionFlag && not (isaddoncmd || helpFlag || tldrFlag || infoFlag || manFlag) -> putStrLn prognameandversion

    -- there's a command argument, but it's bad - show error
    | badcmdprovided -> error' $ "command "++clicmdarg++" is not recognized, run with no command to see a list"

    -- no command found, nothing else to do - show the commands list
    | nocmdprovided -> dbgIO "" "no command, showing commands list" >> printCommandsList prognameandversion addons

    -- builtin command found
    | Just (cmdmode, cmdaction) <- findBuiltinCommand cmd,
      let mcmdname = headMay $ modeNames cmdmode,
      let tldrpagename = maybe "hledger" (("hledger-"<>)) mcmdname ->
      if
        -- help/doc flag - show command help/docs
        | helpFlag  -> pager $ showModeUsage cmdmode ++ "\n"
        | tldrFlag  -> runTldrForPage tldrpagename
        | infoFlag  -> runInfoForTopic "hledger" mcmdname
        | manFlag   -> runManForTopic "hledger"  mcmdname

        -- builtin command which should not require or read the journal - run it
        | cmd `elem` ["demo","help","test"] ->
          cmdaction opts $ error' $ cmd++" tried to read the journal but is not supposed to"

        -- builtin command which should create the journal if missing - do that and run it
        | cmd `elem` ["add","import"] -> do
          ensureJournalFileExists . NE.head =<< journalFilePathFromOpts opts
          withJournalDo opts (cmdaction opts)

        -- all other builtin commands - read the journal and if successful run the command with it
        | otherwise -> withJournalDo opts $ cmdaction opts

    -- addon command found - run it, passing along all arguments except the command name.
    -- It will process args and read the journal itself as needed.
    | isaddoncmd -> do
        let addonargs = cliargsbeforecmd ++ filter (/="--") cliargsaftercmd
        let shellcmd = printf "%s-%s %s" progname cmd (unwords' addonargs) :: String
        dbgIO "addon command selected" cmd
        dbgIO "addon command arguments" (map quoteIfNeeded addonargs)
        dbgIO "running shell command" shellcmd
        system shellcmd >>= exitWith

    -- deprecated command found
    -- cmd == "convert" = error' (modeHelp oldconvertmode) >> exitFailure

    -- something else (shouldn't happen) - show an error 
    | otherwise -> usageError $
        "could not understand the arguments "++show finalargs
        <> if null genargsfromconf then "" else "\ngeneral arguments added from config file: "++show genargsfromconf
        <> if null cmdargsfromconf then "" else "\ncommand "<>cmd<>" arguments added from config file: "++show cmdargsfromconf

  -- And we're done.
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
  let args' = args & moveFlagsAfterCommand & replaceNumericFlags
  let rawopts = either usageError id $ parseArgsWithCmdargs args' addons
  rawOptsToCliOpts rawopts

-- | Parse these command line arguments/options with cmdargs using mainmode.
-- The names of known addon commands are provided so they too can be recognised.
-- If it fails, exit the program with usageError.
parseArgsWithCmdargs :: [String] -> [String] -> Either String RawOpts
parseArgsWithCmdargs args addons = CmdArgs.process (mainmode addons) args

-- | cmdargs does not allow flags (options) to appear before the subcommand name.
-- We prefer to hide this restriction from the user, making the CLI more forgiving.
-- So this tries to move flags, and their values if any, after the command name.
-- This is tricky because of the flexibility of traditional flag syntax.
-- Short flags can be joined together, some flags can have a value or no value,
-- flags and values can be separated by =, a space, or nothing, etc.
--
-- We make a best-effort attempt like so:
-- whether a flag argument (- or -- followed by a non-space character and zero or more others),
-- and its following argument, are movable, falls into these cases, to be checked in this order:
--
-- - it exactly matches a known short or long no-value flag; move it
-- - it exactly matches a short or long requires-value flag; move it and the following argument
-- - it exactly matches a short optional-value flag; assume these don't exist or we don't have any
-- - it exactly matches a long optional-value flag; assume there's no value, move it
-- - it begins with a short requires-value flag; the value is joined to it, move it
-- - it begins with a long requires-value flag followed by =; likewise
-- - it begins with a long optional-value flag followed by =; likewise
--
-- Notes:
--
-- - This hackery increases the risk of causing misleading errors, bugs, or confusion.
--   But it should be fairly robust now, being aware of all builtin flags.
--
-- - All general and builtin command flags (and their values) will be moved. It's clearer to
--   write command flags after the command, but if not we'll handle it (for greater robustness).
--
-- - Unknown flags (from addons) are assumed to be valueless or have a joined value,
--   and will be moved - but later rejected by cmdargs.
--   Instead these should be written to the right of a "--" argument, which hides them.
--   (We could perhaps automate this, but currently don't.)
--
moveFlagsAfterCommand :: [String] -> [String]
moveFlagsAfterCommand args = insertFlagsAfterCommand $ moveFlagArgs (args, [])
  where
    allbuiltinflags = modeAndSubmodeFlags $ mainmode []
    flagsToArgs flags = nubSort [ if length f == 1 then "-"++f else "--"++f | f <- nubSort $ concatMap flagNames flags]
    novalflagargs  = flagsToArgs $ filter ((==FlagNone).flagInfo) allbuiltinflags
    reqvalflagargs = flagsToArgs $ filter ((==FlagReq).flagInfo)  allbuiltinflags
    optvalflagargs = flagsToArgs $ filter isOptValFlag allbuiltinflags
    isOptValFlag f = case flagInfo f of
      FlagOpt     _ -> True
      FlagOptRare _ -> True
      _             -> False
    isshort ('-':c:_) = c /= '-'
    isshort _         = False
    islong ('-':'-':_:_) = True
    islong _             = False
    isflag a = isshort a || islong a
    shortreqvalflagargs = filter isshort reqvalflagargs
    longreqvalflagargs_ = map (++"=") $ filter islong reqvalflagargs
    longoptvalflagargs_ = map (++"=") $ filter islong optvalflagargs ++ ["--debug"]

    -- Is this a short or long flag argument that should be moved,
    -- and is its following argument a value that also should be moved ?
    -- Returns:
    -- 0 (not a flag)
    -- 1 (single flag, maybe with joined argument; or multiple joined short flags)
    -- 2 (flag with value in the next argument).
    isMovableFlagArg :: String -> Int
    isMovableFlagArg a
      | a `elem` novalflagargs  = 1  -- short or long no-val flag
      | a `elem` reqvalflagargs = 2  -- short or long req-val flag, value is in next argument
      | a `elem` optvalflagargs = 1  -- long (or short ?) opt-val flag, assume no value
      | any (`isPrefixOf` a) shortreqvalflagargs = 1  -- short req-val flag, value is joined
      | any (`isPrefixOf` a) longreqvalflagargs_ = 1  -- long req-val flag, value is joined with =
      | any (`isPrefixOf` a) longoptvalflagargs_ = 1  -- long opt-val flag, value is joined with =
      | isflag a = 1    -- an addon flag (or mistyped flag) we don't know, assume no value or value is joined
      | otherwise = 0   -- not a flag

    moveFlagArgs ((a:b:cs), moved)
      | isMovableFlagArg a == 2 = moveFlagArgs (cs, moved++[a,b])
      | isMovableFlagArg a == 1 = moveFlagArgs (b:cs, moved++[a])
      | otherwise               = (a:b:cs, moved)
    moveFlagArgs (as, moved)       = (as, moved)

    insertFlagsAfterCommand ([],             flags) = flags
    insertFlagsAfterCommand (command1:args2, flags) = [command1] ++ flags ++ args2

-- | Get all the flags defined in a mode or its immediate subcommands,
-- whether in named, unnamed or hidden groups (does not recurse into subsubcommands).
modeAndSubmodeFlags :: Mode a -> [Flag a]
modeAndSubmodeFlags m@Mode{modeGroupModes=Group{..}} =
  modeFlags m <> concatMap modeFlags (concatMap snd groupNamed <> groupUnnamed <> groupHidden)


-- unit tests (tests_Hledger_Cli) are defined in Hledger.Cli.Commands
