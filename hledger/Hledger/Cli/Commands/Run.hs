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
 ,run'
) where

import qualified Data.Map.Strict as Map
import Data.Semigroup (sconcat)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Console.CmdArgs.Explicit as C ( Mode )
import Hledger
import Hledger.Cli.CliOptions

import Control.Concurrent.MVar
import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Extra (concatMapM)

import System.Console.CmdArgs.Explicit (expandArgsAt)
import System.Directory (doesFileExist)
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
  ([], Just $ argsFlag "[COMMANDS_FILE1 COMMANDS_FILE2 ...] OR [command1 args... -- command2 args... -- command3 args...]")

-- | The fake run command introduced to break circular dependency.
-- This module needs access to `findBuiltinCommand`, which is defined in Hledger.Cli.Commands
-- However, Hledger.Cli.Commands imports this module, which creates circular dependency.
-- We expose this do-nothing function so that it could be included in the list of all commands inside
-- Hledger.Cli.Commands and ensure that "run" is recognized as a valid command by the Hledger.Cli top-level
-- command line parser. That parser, however, would not call run'. It has a special case for "run", and
-- will call "run" (see below), passing it `findBuiltinCommand`, thus breaking circular dependency.
run' :: CliOpts -> Journal -> IO ()
run' _opts _j = return ()

-- | The actual run command.
run :: (String -> Maybe (Mode RawOpts, CliOpts -> Journal -> IO ())) -> CliOpts -> Journal -> IO ()
run findBuiltinCommand CliOpts{rawopts_=rawopts} j = do
  let args = dbg1 "args" $ listofstringopt "args" rawopts
  if args == []
  then runREPL j findBuiltinCommand
  else do
    -- Check if arguments could be interpreted as files.
    -- If not, assume that they are commands specified directly on the command line
    allAreFiles <- and <$> mapM (doesFileExist . snd . splitReaderPrefix) args
    case allAreFiles of
      True  -> runFromFiles j findBuiltinCommand args
      False -> runFromArgs  j findBuiltinCommand args

-- | Run commands from files given to "run".
runFromFiles :: Journal -> (String -> Maybe (Mode RawOpts, CliOpts -> Journal -> IO ())) -> [String] -> IO ()
runFromFiles defaultJrnl findBuiltinCommand inputfiles = do
  dbg1IO "inputfiles" inputfiles
  -- read commands from all the inputfiles
  commands <- (flip concatMapM) inputfiles $ \f -> do
    dbg1IO "reading commands" f
    lines . T.unpack <$> T.readFile f

  forM_ commands (runCommand defaultJrnl findBuiltinCommand . parseCommand)

-- | Run commands from command line arguments given to "run".
runFromArgs :: Journal -> (String -> Maybe (Mode RawOpts, CliOpts -> Journal -> IO ())) -> [String] -> IO ()
runFromArgs defaultJrnl findBuiltinCommand args = do
  -- read commands from all the inputfiles
  let commands = dbg1 "commands from args" $ splitAtElement "--" args
  forM_ commands (runCommand defaultJrnl findBuiltinCommand)

-- When commands are passed on the command line, shell will parse them for us
-- When commands are read from file, we need to split the line into command and arguments
parseCommand :: String -> [String]
parseCommand line =
  -- # begins a comment, ignore everything after #
  takeWhile (not. ((Just '#')==) . headMay) $  words' (strip line)

-- | Take a single command line (from file, or REPL, or "--"-surrounded block of the args), and run it.
runCommand :: Journal -> (String -> Maybe (Mode RawOpts, CliOpts -> Journal -> IO ())) -> [String] -> IO ()
runCommand defaultJrnl findBuiltinCommand cmdline = do
  dbg1IO "runCommand for" cmdline
  case cmdline of
    "echo":args -> putStrLn $ unwords $ args
    cmdname:args ->
      case findBuiltinCommand cmdname of
        Nothing -> putStrLn $ unwords (cmdname:args)
        Just (cmdmode,cmdaction) -> do
          -- Allow "run" to call "run"
          let cmdaction' = if cmdname == "run" then run findBuiltinCommand else cmdaction
          -- Even though expandArgsAt is done by the Cli.hs, it stops at the first '--', so we need
          -- to do it here as well to make sure that each command can use @ARGFILEs 
          args' <- replaceNumericFlags <$> expandArgsAt args
          dbg1IO "runCommand final args" (cmdname,args')
          opts <- getHledgerCliOpts' cmdmode args'
          withJournalCached defaultJrnl opts (cmdaction' opts)
    [] -> return ()

-- | Run an interactive REPL.
runREPL :: Journal -> (String -> Maybe (Mode RawOpts, CliOpts -> Journal -> IO ())) -> IO ()
runREPL defaultJrnl findBuiltinCommand = do
  putStrLn "Enter hledger commands. To exit, enter 'quit' or 'exit', or send EOF."
  runInputT defaultSettings loop
  where
  loop :: InputT IO ()
  loop = do
    minput <- getInputLine "% "
    case minput of
      Nothing -> return ()
      Just "quit" -> return ()
      Just "exit" -> return ()
      Just input -> do
        liftIO $ runCommand defaultJrnl findBuiltinCommand $ parseCommand input
        loop

-- | Cache of all journals that have been read by commands given to "run",
-- keyed by the fully-expanded filename.
journalCache :: MVar (Map.Map String Journal)
journalCache = unsafePerformIO $ newMVar Map.empty
{-# NOINLINE journalCache #-}

-- | Similar to `withJournal`, but uses caches all the journals it reads.
withJournalCached :: Journal -> CliOpts -> (Journal -> IO ()) -> IO ()
withJournalCached defaultJrnl cliopts cmd = do
  mbjournalpaths <- journalFilePathFromOptsNoDefault cliopts
  j <- case mbjournalpaths of
    Nothing -> return defaultJrnl -- use the journal given to the "run" itself
    Just journalpaths -> journalTransform cliopts . sconcat <$> mapM (readAndCacheJournalFile (inputopts_ cliopts)) journalpaths
  cmd j
  where
    -- | Read a journal file, caching it if it has not been read before.
    readAndCacheJournalFile :: InputOpts -> PrefixedFilePath -> IO Journal
    readAndCacheJournalFile iopts fp | snd (splitReaderPrefix fp) == "-" = do
      dbg1IO "readAndCacheJournalFile using stdin, not cached" "-"
      j <- runExceptT $ readJournalFile iopts "-"
      either error' return j
    readAndCacheJournalFile iopts fp = do
      dbg1IO "readAndCacheJournalFile" fp
      modifyMVar journalCache $ \cache ->
        case Map.lookup fp cache of
          Just journal -> do
            dbg1IO "readAndCacheJournalFile using cache" fp
            return (cache, journal)
          Nothing -> do
            dbg1IO "readAndCacheJournalFile reading and caching journals" fp
            journal <- runExceptT $ readJournalFile iopts fp
            either error' (\j -> return (Map.insert fp j cache, j)) journal
