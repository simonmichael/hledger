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
> > Right j <- readJournalFile definputopts "examples/sample.journal"
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
> > j <- defaultJournal

etc.

-}

module Hledger.Cli.Main where

import Data.Char (isDigit)
import Data.List
import Safe
import qualified System.Console.CmdArgs.Explicit as C
import System.Environment
import System.Exit
import System.FilePath
import System.Process
import Text.Printf

import Hledger.Cli


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
  opts <- argsToCliOpts args addons

  -- select an action and run it.
  let
    cmd                  = command_ opts -- the full matched internal or external command name, if any
    isInternalCommand    = cmd `elem` builtinCommandNames -- not (null cmd) && not (cmd `elem` addons)
    isExternalCommand    = not (null cmd) && cmd `elem` addons -- probably
    isBadCommand         = not (null rawcmd) && null cmd
    hasVersion           = ("--version" `elem`)
    printUsage           = putStr $ showModeUsage $ mainmode addons
    badCommandError      = error' ("command "++rawcmd++" is not recognized, run with no command to see a list") >> exitFailure  -- PARTIAL:
    hasHelpFlag args     = any (`elem` args) ["-h","--help"]
    hasManFlag args      = any (`elem` args) ["--man"]
    hasInfoFlag args     = any (`elem` args) ["--info"]
    f `orShowHelp` mode
      | hasHelpFlag args = putStr $ showModeUsage mode
      | hasInfoFlag args = runInfoForTopic "hledger" (headMay $ modeNames mode)
      | hasManFlag args  = runManForTopic "hledger" (headMay $ modeNames mode)
      | otherwise        = f
      -- where
      --   lastdocflag
  dbgIO "processed opts" opts
  dbgIO "command matched" cmd
  dbgIO "isNullCommand" isNullCommand
  dbgIO "isInternalCommand" isInternalCommand
  dbgIO "isExternalCommand" isExternalCommand
  dbgIO "isBadCommand" isBadCommand
  dbgIO "period from opts" (period_ . rsOpts $ reportspec_ opts)
  dbgIO "interval from opts" (interval_ . rsOpts $ reportspec_ opts)
  dbgIO "query from opts & args" (rsQuery $ reportspec_ opts)
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
      -- \| (null externalcmd) && "binary-filename" `inRawOpts` rawopts = putStrLn $ binaryfilename progname
      -- \| "--browse-args" `elem` args     = System.Console.CmdArgs.Helper.execute "cmdargs-browser" mainmode' args >>= (putStr . show)
      | isNullCommand            = dbgIO "" "no command, showing commands list" >> printCommandsList addons
      | isBadCommand             = badCommandError

      -- builtin commands
      | Just (cmdmode, cmdaction) <- findCommand cmd =
        (case True of
           -- these commands should not require or read the journal
          _ | cmd `elem` ["test","help"] -> cmdaction opts journallesserror
          -- these commands should create the journal if missing
          _ | cmd `elem` ["add","import"] -> do
            (ensureJournalFileExists =<< (head <$> journalFilePathFromOpts opts))
            withJournalDo opts (cmdaction opts)
          -- other commands read the journal and should fail if it's missing
          _ -> withJournalDo opts (cmdaction opts)
        )
        `orShowHelp` cmdmode

      -- addon commands
      | isExternalCommand = do
          let externalargs = argsbeforecmd ++ filter (not.(=="--")) argsaftercmd
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

    moveArgs args = insertFlagsAfterCommand $ moveArgs' (args, [])
      where
        -- -h ..., --version ...
        moveArgs' ((f:a:as), flags)   | isMovableNoArgFlag f                   = moveArgs' (a:as, flags ++ [f])
        -- -f FILE ..., --alias ALIAS ...
        moveArgs' ((f:v:a:as), flags) | isMovableReqArgFlag f, isValue v       = moveArgs' (a:as, flags ++ [f,v])
        -- -fFILE ..., --alias=ALIAS ...
        moveArgs' ((fv:a:as), flags)  | isMovableReqArgFlagAndValue fv         = moveArgs' (a:as, flags ++ [fv])
        -- -f(missing arg)
        moveArgs' ((f:a:as), flags)   | isMovableReqArgFlag f, not (isValue a) = moveArgs' (a:as, flags ++ [f])
        -- anything else
        moveArgs' (as, flags) = (as, flags)

        insertFlagsAfterCommand ([],           flags) = flags
        insertFlagsAfterCommand (command:args, flags) = [command] ++ flags ++ args

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

