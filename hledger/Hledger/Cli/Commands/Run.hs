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


import qualified Data.List.NonEmpty as NE
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
  ([], Just $ argsFlag "[COMMANDS_FILE1 COMMANDS_FILE2 ...]")

-- | The fake run command introduced to break circular dependency
run' :: CliOpts -> Journal -> IO ()
run' _opts _j = return ()

-- | The actual run command.
run :: (String -> Maybe (Mode RawOpts, CliOpts -> Journal -> IO ())) -> CliOpts -> Journal -> IO ()
run findBuiltinCommand CliOpts{rawopts_=rawopts} j = do
  -- Add current journal to cache
  addJournalToCache j defaultJournalKey
  let args = dbg1 "args" $ listofstringopt "args" rawopts
  case args of
    [] -> runREPL findBuiltinCommand
    maybeFile:_ -> do
      -- Check if arguments could be interpreted as files.
      -- If not, assume that they are files
      isFile <- doesFileExist maybeFile
      case isFile of
        True  -> runFromFiles findBuiltinCommand args
        False -> runFromArgs  findBuiltinCommand args

runFromFiles :: (String -> Maybe (Mode RawOpts, CliOpts -> Journal -> IO ())) -> [String] -> IO ()
runFromFiles findBuiltinCommand inputfiles = do
  dbg1IO "inputfiles" inputfiles
  -- read commands from all the inputfiles
  commands <- (flip concatMapM) inputfiles $ \f -> do
    dbg1IO "reading commands" f
    lines . T.unpack <$> T.readFile f

  forM_ commands (runCommand findBuiltinCommand . parseCommand)

runFromArgs :: (String -> Maybe (Mode RawOpts, CliOpts -> Journal -> IO ())) -> [String] -> IO ()
runFromArgs findBuiltinCommand args = do
  -- read commands from all the inputfiles
  let commands = dbg1 "commands from args" $ splitAtElement "--" args
  forM_ commands (runCommand findBuiltinCommand)

-- When commands are passed on the command line, shell will parse them for us
-- When commands are read from file, we need to split the line into command and arguments
parseCommand :: String -> [String]
parseCommand line =
  -- # begins a comment, ignore everything after #
  takeWhile (not. ((Just '#')==) . headMay) $  words' (strip line)

runCommand :: (String -> Maybe (Mode RawOpts, CliOpts -> Journal -> IO ())) -> [String] -> IO ()
runCommand findBuiltinCommand cmdline = do
  dbg1IO "running command" cmdline
  -- # begins a comment, ignore everything after #
  case cmdline of
    "echo":args -> putStrLn $ unwords $ args
    cmdname:args ->
      case findBuiltinCommand cmdname of
      Nothing -> putStrLn $ unwords (cmdname:args)
      Just (cmdmode,cmdaction) -> do
        opts <- getHledgerCliOpts' cmdmode args
        withJournalCached opts (cmdaction opts)
    [] -> return ()

runREPL :: (String -> Maybe (Mode RawOpts, CliOpts -> Journal -> IO ())) -> IO ()
runREPL findBuiltinCommand = do
  putStrLn "Enter hledger commands, or 'quit' for help."
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
        liftIO $ runCommand findBuiltinCommand $ parseCommand input
        loop

{-# NOINLINE journalCache #-}
journalCache :: MVar (Map.Map String Journal)
journalCache = unsafePerformIO $ newMVar Map.empty

-- | Key used to cache the journal given in the arguments to 'run'.
defaultJournalKey :: String
defaultJournalKey = "journal specified in args of run"

addJournalToCache :: Journal -> String -> IO ()
addJournalToCache j key = modifyMVar_ journalCache $ \cache ->
  return $ Map.insert key j cache

withJournalCached :: CliOpts -> (Journal -> IO ()) -> IO ()
withJournalCached cliopts cmd = do
  mbjournalpaths <- journalFilePathFromOptsNoDefault cliopts
  let journalpaths = case mbjournalpaths of
                      Nothing    -> NE.fromList [defaultJournalKey]
                      Just paths -> paths
  j <- journalTransform cliopts . sconcat <$> mapM (readAndCacheJournalFile (inputopts_ cliopts)) journalpaths
  cmd j
  where
    readAndCacheJournalFile :: InputOpts -> PrefixedFilePath -> IO Journal
    readAndCacheJournalFile iopts "-" = do
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
