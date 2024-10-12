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
Copyright (c) 2007-2024 Simon Michael <simon@joyful.com> and contributors
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

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Unused LANGUAGE pragma" #-}
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

import Control.Monad (when, unless)
import Data.Bifunctor (second)
import Data.Char (isDigit)
import Data.Either (isRight)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List
import qualified Data.List.NonEmpty as NE
import Data.Maybe (isJust)
import Data.Text (pack, Text)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Safe
import System.Console.CmdArgs.Explicit
import System.Console.CmdArgs.Explicit as CmdArgsWithoutName hiding (Name)
import System.Environment
import System.Exit
import System.FilePath
import System.Process
import Text.Megaparsec (optional, takeWhile1P, eof)
import Text.Megaparsec.Char (char)
import Text.Printf

import Hledger
import Hledger.Cli.CliOptions
import Hledger.Cli.Conf
import Hledger.Cli.Commands
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
main = withGhcDebug' $ do

  -- 0. let's go!
  let
    -- Trace helpers. These always trace to stderr, even when running `hledger ui`;
    -- that's ok as conf is a hledger cli feature for now.
    dbgIO, dbgIO1, dbgIO2 :: Show a => String -> a -> IO ()  -- this signature is needed
    dbgIO  = ptraceAtIO verboseDebugLevel
    dbgIO1 = ptraceAtIO 1
    dbgIO2 = ptraceAtIO 2

  dbgIO "running" prognameandversion

  starttime <- getPOSIXTime

  -- give ghc-debug a chance to take control
  when (ghcDebugMode == GDPauseAtStart) $ ghcDebugPause'

  -- try to encourage user's $PAGER to display ANSI when supported
  when useColorOnStdout setupPager

  -- Search PATH for addon commands. Exclude any that match builtin command names.
  addons <- hledgerAddons <&> filter (not . (`elem` builtinCommandNames) . dropExtension)

  ---------------------------------------------------------------
  -- 1. Preliminary command line parsing.

  dbgIO "\n1. Preliminary command line parsing" ()
  -- Do some argument preprocessing to help cmdargs
  cliargs <- getArgs
    >>= expandArgsAt         -- interpolate @ARGFILEs
    <&> replaceNumericFlags  -- convert -NUM to --depth=NUM
  let
    (clicmdarg, cliargswithoutcmd, cliargswithcmdfirst) = moveFlagsAfterCommand cliargs
    cliargswithcmdfirstwithoutclispecific = dropCliSpecificOpts cliargswithcmdfirst
    (cliargsbeforecmd, cliargsaftercmd) = second (drop 1) $ break (==clicmdarg) cliargs
  dbgIO "cli args" cliargs
  dbg1IO "cli args with command first, if any" cliargswithcmdfirst
  dbgIO "command argument found"   clicmdarg
  dbgIO "cli args before command"  cliargsbeforecmd
  dbgIO "cli args after command"   cliargsaftercmd

  -- Now try to identify the full subcommand name, so we can look for
  -- command-specific options in config files (clicmdarg may be only an abbreviation).
  -- For this do a preliminary cmdargs parse of the arguments with cli-specific options removed.
  -- If no command was provided, or if the command line contains a bad flag
  -- or a wrongly present/missing flag argument, cmd will be "".
  let
    rawopts1 = cmdargsParse
      "for command name"
      (mainmode addons)
      cliargswithcmdfirstwithoutclispecific
    cmd = stringopt "command" rawopts1
      -- XXX better error message when cmdargs fails (eg spaced/quoted/malformed flag values) ?
    nocmdprovided  = null clicmdarg
    badcmdprovided = null cmd && not nocmdprovided
    isaddoncmd = not (null cmd) && cmd `elem` addons
    -- isbuiltincmd = cmd `elem` builtinCommandNames
    mcmdmodeaction = findBuiltinCommand cmd
    effectivemode = maybe (mainmode []) fst mcmdmodeaction
  dbgIO "cli args with command first and no cli-specific opts" cliargswithcmdfirstwithoutclispecific
  dbgIO1 "command found" cmd
  dbgIO "no command provided" nocmdprovided
  dbgIO "bad command provided" badcmdprovided
  dbgIO "is addon command" isaddoncmd

  ---------------------------------------------------------------
  -- 2. Read extra options from a config file.

  dbgIO "\n2. Read options from a config file" ()
  -- Identify any --conf/--no-conf options.
  -- For this parse with cmdargs a second time, this time with just the args that look conf-related.
  let cliconfargs = dropUnsupportedOpts confflagsmode cliargswithoutcmd
  dbgIO "cli args without command" cliargswithoutcmd
  -- dbgIO "cli conf args" cliconfargs
  let rawopts2 = cmdargsParse "for conf options" confflagsmode cliconfargs

  -- Read extra general and command-specific args/opts from the config file if found.
  -- Ignore any general opts or cli-specific opts not known to be supported by the command.
  (conf, mconffile) <- getConf rawopts2
  let
    genargsfromconf = confLookup "general" conf
    addoncmdssupportinggenopts = ["ui", "web"]  -- addons known to support hledger general options
    supportedgenargsfromconf
      | cmd `elem` addoncmdssupportinggenopts =
          [a | a <- genargsfromconf, not $ any (`isPrefixOf` a) addoncmdssupportinggenopts]
      | isaddoncmd = []
      | otherwise  = dropUnsupportedOpts effectivemode genargsfromconf
    excludedgenargsfromconf = genargsfromconf \\ supportedgenargsfromconf
    cmdargsfromconf
      | null cmd  = []
      | otherwise = confLookup cmd conf & if isaddoncmd then ("--":) else id
  when (isJust mconffile) $ do
    dbgIO1 "using extra general args from config file" genargsfromconf
    unless (null excludedgenargsfromconf) $
      dbgIO1 "excluded general args from config file, not supported by this command" excludedgenargsfromconf
    dbgIO1 "using extra command args from config file" cmdargsfromconf

  ---------------------------------------------------------------
  -- 3. Combine cli and config file args and parse with cmdargs a third time.
  -- A bad flag or flag argument will cause the program to exit with an error here.

  dbgIO "\n3. Combine command line and config file args" ()
  let
    finalargs =
      (if null clicmdarg then [] else [clicmdarg]) <> supportedgenargsfromconf <> cmdargsfromconf <> cliargswithoutcmd
      & replaceNumericFlags                -- convert any -NUM opts from the config file
  -- finalargs' <- expandArgsAt finalargs  -- expand @ARGFILEs in the config file ? don't bother
  let rawopts3 = cmdargsParse "for all options" (mainmode addons) finalargs

  ---------------------------------------------------------------
  -- 4. Finally, select an action and run it.

  dbgIO "\n4. Select an action" ()
  -- We check for the help/doc/version flags first, since they are a high priority.
  -- (A perfectionist might think they should be so high priority that adding -h
  -- to an invalid command line would show help. But cmdargs tends to fail first,
  -- preventing this, and trying to detect them without cmdargs, and always do the
  -- right thing with builtin commands and addon commands, gets much too complicated.)
  let
    helpFlag    = boolopt "help"    rawopts3
    tldrFlag    = boolopt "tldr"    rawopts3
    infoFlag    = boolopt "info"    rawopts3
    manFlag     = boolopt "man"     rawopts3
    versionFlag = boolopt "version" rawopts3

  if
    -- 4.1. no command and a help/doc flag found - show general help/docs
    | nocmdprovided && helpFlag -> pager $ showModeUsage (mainmode []) ++ "\n"
    | nocmdprovided && tldrFlag -> runTldrForPage  "hledger"
    | nocmdprovided && infoFlag -> runInfoForTopic "hledger" Nothing
    | nocmdprovided && manFlag  -> runManForTopic  "hledger" Nothing

    -- 4.2. --version flag found and none of these other conditions - show version
    | versionFlag && not (isaddoncmd || helpFlag || tldrFlag || infoFlag || manFlag) -> putStrLn prognameandversion

    -- 4.3. there's a command argument, but it's bad - show error
    | badcmdprovided -> error' $ "command "++clicmdarg++" is not recognized, run with no command to see a list"

    -- 4.4. no command found, nothing else to do - show the commands list
    | nocmdprovided -> dbgIO "" "no command, showing commands list" >> printCommandsList prognameandversion addons

    -- 4.5. builtin command found
    | Just (cmdmode, cmdaction) <- mcmdmodeaction -> do

      -- validate opts/args more and convert to CliOpts
      opts <- rawOptsToCliOpts rawopts3 >>= \opts0 -> return opts0{progstarttime_=starttime}
      dbgIO2 "processed opts" opts
      dbgIO "period from opts" (period_ . _rsReportOpts $ reportspec_ opts)
      dbgIO "interval from opts" (interval_ . _rsReportOpts $ reportspec_ opts)
      dbgIO "query from opts & args" (_rsQuery $ reportspec_ opts)
      let
        mcmdname = headMay $ modeNames cmdmode
        tldrpagename = maybe "hledger" (("hledger-"<>)) mcmdname

      -- run the builtin command according to its type
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

    -- 4.6. external addon command found - run it,
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
          addonargs0 = filter (/="--") $ supportedgenargsfromconf <> cmdargsfromconf <> cliargswithoutcmd
          addonargs = dropCliSpecificOpts addonargs0
          shellcmd = printf "%s-%s %s" progname cmd (unwords' addonargs) :: String
        dbgIO "addon command selected" cmd
        dbgIO "addon command arguments after removing cli-specific opts" (map quoteIfNeeded addonargs)
        dbgIO1 "running addon" shellcmd
        system shellcmd >>= exitWith

    -- deprecated command found
    -- cmd == "convert" = error' (modeHelp oldconvertmode) >> exitFailure

    -- 4.7. something else (shouldn't happen) - show an error
    | otherwise -> usageError $
        "could not understand the arguments "++show finalargs
        <> if null genargsfromconf then "" else "\ngeneral arguments added from config file: "++show genargsfromconf
        <> if null cmdargsfromconf then "" else "\ncommand arguments added from config file: "++show cmdargsfromconf

  -- 5. And we're done.
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
    rawopts3 = cmdargsParse "for options" (mainmode addons) args1
  rawOptsToCliOpts rawopts3

-- | Parse the given command line arguments/options with the given cmdargs mode,
-- after adding values to any valueless --debug flags,
-- with debug logging showing the given description of this parsing pass
-- (useful when cmdargsParse is called more than once).
-- If parsing fails, exit the program with an informative error message.
cmdargsParse :: String -> Mode RawOpts -> [String] -> RawOpts
cmdargsParse desc m args0 = process m (ensureDebugFlagHasVal args0)
  & either
    (\e -> error' $ e <> " while parsing these args " <> desc <> ": " <> unwords (map quoteIfNeeded args0))
    (traceOrLogAt verboseDebugLevel ("cmdargs: parsing " <> desc <> ": " <> show args0))

-- | cmdargs does not allow flags (options) to appear before the subcommand argument.
-- We prefer to hide this restriction from the user, making the CLI more forgiving.
-- So this tries to move flags, and their values if any, after the command argument.
-- It also returns the (possibly empty) command argument and the other arguments,
-- separately for convenience.
--
-- Detecting the command argument is tricky because of the flexibility of traditional flag syntax.
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
-- - This hackery increases the risk of misleading errors, bugs, and confusion.
--   It should be fairly robust now, being aware of all builtin flags.
--   The main tests are in hledger/test/cli/cli.test, but they are not exhaustive.
--
-- - All general and builtin command flags (and their values) will be moved. It's clearer to
--   write command flags after the command, but if not we'll handle it (for greater robustness).
--
-- - Long flags should be spelled in full; abbreviated long flags might not get moved.
--
-- - Unknown flags (from addons) are assumed to be valueless or have a joined value,
--   and will be moved - but later rejected by cmdargs.
--   Instead these should be written to the right of a "--" argument, which hides them.
--
moveFlagsAfterCommand :: [String] -> (String, [String], [String])
moveFlagsAfterCommand args =
  case moveFlagArgs (args, []) of
    ([],as)                      -> ("", as, as)
    (unmoved@(('-':_):_), moved) -> ("", as, as) where as = unmoved<>moved
    (cmdarg:unmoved, moved)      -> (cmdarg, as, cmdarg:as) where as = unmoved<>moved
  where
    moveFlagArgs :: ([String], [String]) -> ([String], [String])
    moveFlagArgs ((a:b:cs), moved)
      | isMovableFlagArg a b == 2 = moveFlagArgs (cs, moved++[a,b])
      | isMovableFlagArg a b == 1 = moveFlagArgs (b:cs, moved++[a])
      | otherwise                 = (a:b:cs, moved)
      where
        -- Is this a short or long flag argument that should be moved,
        -- and is its following argument a value that also should be moved ?
        -- Returns:
        -- 0 (not a flag; don't move this argument)
        -- 1 (a valueless flag, or a long flag with joined argument, or multiple joined valueless short flags; move this argument)
        -- 2 (a short or long flag with a value in the next argument; move this and next argument).
        isMovableFlagArg :: String -> String -> Int
        isMovableFlagArg a1 a2
          | a1 `elem` noValFlagArgs  = 1  -- short or long no-val flag
          | a1 == "--debug" && not (isDebugValue a2) = 1  --debug without a value
          | a1 `elem` reqValFlagArgs = 2  -- short or long req-val flag (or --debug) with a separate value
          | a1 `elem` optValFlagArgs = 1  -- long (or short ?) opt-val flag, assume no value
          | any (`isPrefixOf` a1) shortReqValFlagArgs = 1  -- short req-val flag with a joined value
               -- or possibly multiple joined valueless short flags, we won't move those correctly
          | any (`isPrefixOf` a1) longReqValFlagArgs_ = 1  -- long req-val flag (or --debug) with a joined value
          | any (`isPrefixOf` a1) longOptValFlagArgs_ = 1  -- long opt-val flag with a joined value
          -- | isLongFlagArg a1 && any (takeWhile (/='=') `isPrefixOf`) longReqValFlagArgs_ ... -- try to move abbreviated long flags ?
          | isFlagArg a1 = 1    -- an addon flag (or mistyped flag) we don't know, assume no value or value is joined
          | otherwise = 0    -- not a flag
    moveFlagArgs (as, moved)       = (as, moved)

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
