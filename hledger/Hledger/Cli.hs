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

-- | Let's go!
main :: IO ()
main = withGhcDebug' $ do
  when (ghcDebugMode == GDPauseAtStart) $ ghcDebugPause'

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
  rawcliargs <- getArgs >>= expandArgsAt
  let
    cliargswithcmdfirst  = rawcliargs & replaceNumericFlags & moveFlagsAfterCommand
    isNonEmptyNonFlag s  = not $ null s || "-" `isPrefixOf` s
    clicmdarg            = headDef "" $ takeWhile isNonEmptyNonFlag cliargswithcmdfirst
    isNullCommand        = null clicmdarg
    (rawcliargsbeforecmd, rawcliargsaftercmd) = second (drop 1) $ break (==clicmdarg) rawcliargs
    dbgIO :: Show a => String -> a -> IO ()  -- type signature needed
    dbgIO = ptraceAtIO 8

  dbgIO "running" prognameandversion
  dbgIO "raw cli args" rawcliargs
  dbgIO "raw args before command" rawcliargsbeforecmd
  dbgIO "raw args after command"  rawcliargsaftercmd
  dbgIO "raw cli args rearranged for cmdargs" cliargswithcmdfirst
  dbgIO "command argument is probably" clicmdarg

  -- search PATH for addon commands, excluding any that match builtin command names
  addons <- hledgerAddons <&> filter (not . (`elem` builtinCommandNames) . dropExtension) 

  -- do a preliminary parse with cmdargs to identify the full command name
  let cmd = stringopt "command" $ parseArgsWithCmdargs cliargswithcmdfirst addons

  -- get any extra args/opts declared in a config file, both general and command-specific
  conf <- getConf
  let
    genargsfromconf = confArgsFor "general" conf
    cmdargsfromconf = confArgsFor cmd       conf
  dbgIO ("extra general args from config file")   genargsfromconf
  dbgIO ("extra "<>cmd<>" args from config file") cmdargsfromconf

  -- insert the config file args (before the others) and parse the lot with cmdargs
  let
    (clicmdarg',cliotherargs) = splitAt 1 cliargswithcmdfirst
    allargswithcmdfirst = clicmdarg' <> genargsfromconf <> cmdargsfromconf <> cliotherargs & replaceNumericFlags
  dbgIO "allargswithcmdfirst" allargswithcmdfirst
  opts' <- argsToCliOpts allargswithcmdfirst addons
  -- and save the start time
  let opts = opts'{progstarttime_=starttime}

  -- select an action and prepare to run it
  let
    isInternalCommand = cmd `elem` builtinCommandNames -- not (null cmd) && not (cmd `elem` addons)
    isExternalCommand = not (null cmd) && cmd `elem` addons -- probably
    isBadCommand      = not (null clicmdarg) && null cmd
    printUsage        = pager $ showModeUsage (mainmode addons) ++ "\n"
    badCommandError   = error' ("command "++clicmdarg++" is not recognized, run with no command to see a list") >> exitFailure  -- PARTIAL:
    helpFlag       = boolopt "help"    $ rawopts_ opts
    tldrFlag       = boolopt "tldr"    $ rawopts_ opts
    infoFlag       = boolopt "info"    $ rawopts_ opts
    manFlag        = boolopt "man"     $ rawopts_ opts
    versionFlag    = boolopt "version" $ rawopts_ opts
    f `orShowHelp` mode1
      | helpFlag = pager $ showModeUsage mode1 ++ "\n"
      | tldrFlag = runTldrForPage $ maybe "hledger" (("hledger-"<>)) $ headMay $ modeNames mode1
      | infoFlag = runInfoForTopic "hledger" (headMay $ modeNames mode1)
      | manFlag  = runManForTopic "hledger" (headMay $ modeNames mode1)
      | otherwise   = f
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
    runHledgerCommand
      -- high priority flags and situations. -h, then --help, then --tldr, then --info, then --man are highest priority.
      | isNullCommand && helpFlag = dbgIO "" "-h/--help with no command, showing general help" >> printUsage
      | isNullCommand && tldrFlag = dbgIO "" "--tldr with no command, showing general tldr page" >> runTldrForPage "hledger"
      | isNullCommand && infoFlag = dbgIO "" "--info with no command, showing general info manual" >> runInfoForTopic "hledger" Nothing
      | isNullCommand && manFlag  = dbgIO "" "--man with no command, showing general man page" >> runManForTopic "hledger" Nothing
      | versionFlag && not (isExternalCommand || helpFlag || tldrFlag || infoFlag || manFlag) = putStrLn prognameandversion
      | isNullCommand             = dbgIO "" "no command, showing commands list" >> printCommandsList prognameandversion addons
      | isBadCommand              = badCommandError

      -- builtin commands
      | Just (cmdmode, cmdaction) <- findBuiltinCommand cmd =
        (case True of
           -- these commands should not require or read the journal
          _ | cmd `elem` ["demo","help","test"] ->
              cmdaction opts $ error' $ cmd++" tried to read the journal but is not supposed to"
          -- these commands should create the journal if missing
          _ | cmd `elem` ["add","import"] -> do
              ensureJournalFileExists . NE.head =<< journalFilePathFromOpts opts
              withJournalDo opts (cmdaction opts)
          -- other commands read the journal and should fail if it's missing
          _ -> withJournalDo opts (cmdaction opts)
        )
        `orShowHelp` cmdmode

      -- addon commands
      | isExternalCommand = do
          let externalargs = rawcliargsbeforecmd ++ filter (/="--") rawcliargsaftercmd
          let shellcmd = printf "%s-%s %s" progname cmd (unwords' externalargs) :: String
          dbgIO "external command selected" cmd
          dbgIO "external command arguments" (map quoteIfNeeded externalargs)
          dbgIO "running shell command" shellcmd
          system shellcmd >>= exitWith

      -- deprecated commands
      -- cmd == "convert" = error' (modeHelp oldconvertmode) >> exitFailure

      -- XXX shouldn't/doesn't reach here, but this output might be helpful
      | otherwise = usageError $
          "could not understand the arguments "++show allargswithcmdfirst
          <> if null genargsfromconf then "" else "\ngeneral arguments added from config file: "++show genargsfromconf
          <> if null cmdargsfromconf then "" else "\ncommand "<>cmd<>" arguments added from config file: "++show cmdargsfromconf

  -- do it
  runHledgerCommand

  when (ghcDebugMode == GDPauseAtEnd) $ ghcDebugPause'

-- | Parse hledger CLI options from these command line arguments and add-on command names.
-- Or if it fails, exit the program with usageError.
argsToCliOpts :: [String] -> [String] -> IO CliOpts
argsToCliOpts rawargs addons = do
  -- Try to ensure the command argument is first, and rewrite -NUM flags
  -- which cmdargs doesn't support. This is already done in main but
  -- perhaps there are other users of this function.
  let args = moveFlagsAfterCommand $ replaceNumericFlags rawargs
  rawOptsToCliOpts $ parseArgsWithCmdargs args addons

-- | Parse these command line arguments/options with cmdargs using mainmode.
-- The names of known addon commands are provided so they too can be recognised.
-- If it fails, exit the program with usageError.
parseArgsWithCmdargs :: [String] -> [String] -> RawOpts
parseArgsWithCmdargs args addons =
  either usageError id $ CmdArgs.process (mainmode addons) args

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
noargflagstomove  = concatMap flagNames (filter ((==FlagNone).flagInfo) flagstomove)
  -- silly special case: if someone is abbreviating --tldr, make sure it works right when written before COMMAND
  -- (not needed for --info, --man, --version since their abbreviations are ambiguous)
  ++ ["tl", "tld"]
reqargflagstomove = -- filter (/= "debug") $
                    concatMap flagNames $ filter ((==FlagReq ).flagInfo) flagstomove
optargflagstomove = concatMap flagNames $ filter (isFlagOpt   .flagInfo) flagstomove
  where
    isFlagOpt = \case
      FlagOpt     _ -> True
      FlagOptRare _ -> True
      _             -> False


-- unit tests (tests_Hledger_Cli) are defined in Hledger.Cli.Commands
