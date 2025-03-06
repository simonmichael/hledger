{-|

The @run@ command allows you to run multiple commands via REPL or from the supplied file(s).

-}

{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Hledger.Cli.Commands.Run (
  runmode
 ,run
 ,replmode
 ,repl
 ,runOrReplStub
) where

import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import Data.Semigroup (sconcat)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Console.CmdArgs.Explicit as C ( Mode )
import Hledger
import Hledger.Cli.CliOptions

import Control.Exception
import Control.Concurrent.MVar
import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Extra (concatMapM)

import System.Exit (ExitCode)
import System.IO (stdin, hIsTerminalDevice, hIsOpen)
import System.IO.Unsafe (unsafePerformIO)
import System.Console.Haskeline

import Safe (headMay)
import Hledger.Cli.Utils (journalTransform)

-- | Command line options for this command.
runmode = hledgerCommandMode
  $(embedFileRelative "Hledger/Cli/Commands/Run.txt")
  (
  []
  )
  cligeneralflagsgroups1
  hiddenflags
  ([], Just $ argsFlag "[COMMANDS_FILE1 COMMANDS_FILE2 ...] OR [-- command1 args... -- command2 args... -- command3 args...]")

replmode = hledgerCommandMode
  $(embedFileRelative "Hledger/Cli/Commands/Repl.txt")
  (
  []
  )
  cligeneralflagsgroups1
  hiddenflags
  ([], Nothing)

-- | The fake run/repl command introduced to break circular dependency.
-- This module needs access to `findBuiltinCommand`, which is defined in Hledger.Cli.Commands
-- However, Hledger.Cli.Commands imports this module, which creates circular dependency.
-- We expose this do-nothing function so that it could be included in the list of all commands inside
-- Hledger.Cli.Commands and ensure that "run" is recognized as a valid command by the Hledger.Cli top-level
-- command line parser. That parser, however, would not call run'. It has a special case for "run", and
-- will call "run" (see below), passing it `findBuiltinCommand`, thus breaking circular dependency.
runOrReplStub :: CliOpts -> Journal -> IO ()
runOrReplStub _opts _j = return ()

-- | Default input files that would be used by commands if
--   there is no explicit alternative given
newtype DefaultRunJournal = DefaultRunJournal (NE.NonEmpty String) deriving (Show)

-- | The actual run command.
run :: Maybe DefaultRunJournal -> (String -> Maybe (Mode RawOpts, CliOpts -> Journal -> IO ())) -> CliOpts -> IO ()
run defaultJournalOverride findBuiltinCommand cliopts@CliOpts{rawopts_=rawopts} = do
  withJournalCached defaultJournalOverride cliopts $ \(_,key) -> do
    let args = dbg1 "args" $ listofstringopt "args" rawopts
    isTerminal <- isStdinTerminal
    if args == [] && not isTerminal
      then do
        inputFiles <- journalFilePathFromOpts cliopts
        let journalFromStdin = any (== "-") $ map (snd . splitReaderPrefix) $ NE.toList inputFiles
        if journalFromStdin
        then error' "'run' can't read commands from stdin, as one of the input files was stdin as well"
        else runREPL key findBuiltinCommand
      else do
        -- Check if arguments start with "--".
        -- If not, assume that they are files with commands
          case args of
            "--":_ -> runFromArgs  key findBuiltinCommand args
            _ -> runFromFiles key findBuiltinCommand args

-- | The actual repl command.
repl :: (String -> Maybe (Mode RawOpts, CliOpts -> Journal -> IO ())) -> CliOpts -> IO ()
repl findBuiltinCommand cliopts = do
  withJournalCached Nothing cliopts $ \(_,key) -> do
    runREPL key findBuiltinCommand

-- | Run commands from files given to "run".
runFromFiles :: DefaultRunJournal -> (String -> Maybe (Mode RawOpts, CliOpts -> Journal -> IO ())) -> [String] -> IO ()
runFromFiles defaultJournalOverride findBuiltinCommand inputfiles = do
  dbg1IO "inputfiles" inputfiles
  -- read commands from all the inputfiles
  commands <- (flip concatMapM) inputfiles $ \f -> do
    dbg1IO "reading commands" f
    lines . T.unpack <$> T.readFile f

  forM_ commands (runCommand defaultJournalOverride findBuiltinCommand . parseCommand)

-- | Run commands from command line arguments given to "run".
runFromArgs :: DefaultRunJournal -> (String -> Maybe (Mode RawOpts, CliOpts -> Journal -> IO ())) -> [String] -> IO ()
runFromArgs defaultJournalOverride findBuiltinCommand args = do
  -- read commands from all the inputfiles
  let commands = dbg1 "commands from args" $ splitAtElement "--" args
  forM_ commands (runCommand defaultJournalOverride findBuiltinCommand)

-- When commands are passed on the command line, shell will parse them for us
-- When commands are read from file, we need to split the line into command and arguments
parseCommand :: String -> [String]
parseCommand line =
  -- # begins a comment, ignore everything after #
  takeWhile (not. ((Just '#')==) . headMay) $  words' (strip line)

-- | Take a single command line (from file, or REPL, or "--"-surrounded block of the args), and run it.
runCommand :: DefaultRunJournal -> (String -> Maybe (Mode RawOpts, CliOpts -> Journal -> IO ())) -> [String] -> IO ()
runCommand defaultJournalOverride findBuiltinCommand cmdline = do
  dbg1IO "runCommand for" cmdline
  case cmdline of
    "echo":args -> putStrLn $ unwords $ args
    cmdname:args ->
      case findBuiltinCommand cmdname of
        Nothing -> error' $ "Unrecognized command: " ++ unwords (cmdname:args)
        Just (cmdmode,cmdaction) -> do
          -- Even though expandArgsAt is done by the Cli.hs, it stops at the first '--', so we need
          -- to do it here as well to make sure that each command can use @ARGFILEs
          args' <- replaceNumericFlags <$> expandArgsAt args
          dbg1IO "runCommand final args" (cmdname,args')
          opts <- getHledgerCliOpts' cmdmode args'
          withJournalCached (Just defaultJournalOverride) opts $ \(j,key) -> do
            if cmdname == "run" -- allow "run" to call "run"
              then run (Just key) findBuiltinCommand opts
              else cmdaction opts j
    [] -> return ()

-- | Run an interactive REPL.
runREPL :: DefaultRunJournal -> (String -> Maybe (Mode RawOpts, CliOpts -> Journal -> IO ())) -> IO ()
runREPL defaultJournalOverride findBuiltinCommand = do
  isTerminal <- isStdinTerminal
  if not isTerminal
    then runInputT defaultSettings (loop "")
    else do
      putStrLn "Enter hledger commands. To exit, enter 'quit' or 'exit', or send EOF."
      runInputT defaultSettings (loop "% ")
  where
  loop :: String -> InputT IO ()
  loop prompt = do
    minput <- getInputLine prompt
    case minput of
      Nothing -> return ()
      Just "quit" -> return ()
      Just "exit" -> return ()
      Just input -> do
        liftIO $ (runCommand defaultJournalOverride findBuiltinCommand $ parseCommand input)
                  `catches`
                  [Handler (\(e::ErrorCall) -> putStr $ show e)
                  ,Handler (\(_::ExitCode) -> return ())
                  ,Handler (\UserInterrupt -> return ())
                  ]
        loop prompt

isStdinTerminal = do
  op <- hIsOpen stdin
  if op then hIsTerminalDevice stdin else return False

-- | Cache of all journals that have been read by commands given to "run",
-- keyed by the fully-expanded filename.
journalCache :: MVar (Map.Map (InputOpts,PrefixedFilePath) Journal)
journalCache = unsafePerformIO $ newMVar Map.empty
{-# NOINLINE journalCache #-}

-- | Cache of stdin contents, so that we can re-read it if InputOptions change
stdinCache :: MVar (Maybe T.Text)
stdinCache = unsafePerformIO $ newMVar Nothing
{-# NOINLINE stdinCache #-}

-- | Similar to `withJournal`, but uses caches all the journals it reads.
-- When reading from stdin, caches the stdin contents so that we could reprocess
-- it if a read with different InputOptions is requested.
withJournalCached :: Maybe DefaultRunJournal -> CliOpts -> ((Journal, DefaultRunJournal) -> IO ()) -> IO ()
withJournalCached defaultJournalOverride cliopts cmd = do
  (j,key) <- case defaultJournalOverride of
    Nothing -> journalFilePathFromOpts cliopts >>= readFiles
    Just (DefaultRunJournal defaultFiles) -> do
      mbjournalpaths <- journalFilePathFromOptsNoDefault cliopts
      case mbjournalpaths of
        Nothing -> readFiles defaultFiles -- use the journal(s) given to the "run" itself
        Just journalpaths -> readFiles journalpaths
  cmd (j,key)
  where
    readFiles journalpaths = do
      j <- journalTransform cliopts . sconcat <$> mapM (readAndCacheJournalFile (inputopts_ cliopts)) journalpaths
      return (j, DefaultRunJournal journalpaths)
    -- | Read a journal file, caching it (and InputOptions used to read it) if it has not been seen before.
    -- If the same file is requested with different InputOptions, we read it anew and cache
    -- it separately.
    readAndCacheJournalFile :: InputOpts -> PrefixedFilePath -> IO Journal
    readAndCacheJournalFile iopts fp = do
      dbg1IO "readAndCacheJournalFile" fp
      modifyMVar journalCache $ \cache ->
        case Map.lookup (ioptsWithoutReportSpan,fp) cache of
          Just journal -> do
            dbg1IO "readAndCacheJournalFile using cache" (fp, iopts)
            return (cache, journal)
          Nothing -> do
            dbg1IO "readAndCacheJournalFile reading and caching journals" (fp, iopts)
            journal <- runExceptT $ if snd (splitReaderPrefix fp) == "-" then readStdin else readJournalFile iopts fp
            either error' (\j -> return (Map.insert (ioptsWithoutReportSpan,fp) j cache, j)) journal
      where
        -- InputOptions contain reportspan_ that is used to calculare forecast period,
        -- that is used by journalFinalise to insert forecast transactions.addHeaderBorders
        -- For the purposes of caching, we want to ignore this, as it is only used for forecast
        -- and it is sufficient to include forecast_ in the InputOptions that we use as a key.
        ioptsWithoutReportSpan = iopts { reportspan_ = emptydatespan }
        -- Read stdin, or if we read it alread, use a cache
        -- readStdin :: InputOpts -> ExceptT String IO Journal
        readStdin = do
          stdinContent <- liftIO $ modifyMVar stdinCache $ \cache ->
            case cache of
              Just cached -> do
                dbg1IO "readStdin using cached stdin" "-"
                return (cache, cached)
              Nothing -> do
                dbg1IO "readStdin reading and caching stdin" "-"
                stdinContent <- readFileOrStdinPortably "-"
                return (Just stdinContent, stdinContent)
          hndl <- liftIO $ inputToHandle stdinContent
          readJournal iopts Nothing hndl
