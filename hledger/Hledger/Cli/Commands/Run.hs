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

import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Console.CmdArgs.Explicit as C ( Mode )
import Hledger
import Hledger.Cli.CliOptions

import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Extra (concatMapM)

import System.Directory (doesFileExist)
import System.Console.Haskeline

import Safe (headMay)

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
  let args = dbg1 "args" $ listofstringopt "args" rawopts
  case args of
    [] -> runREPL findBuiltinCommand j
    maybeFile:_ -> do
      -- Check if arguments could be interpreted as files.
      -- If not, assume that they are files
      isFile <- doesFileExist maybeFile
      case isFile of
        True  -> runFromFiles findBuiltinCommand args j
        False -> runFromArgs  findBuiltinCommand args j

runFromFiles :: (String -> Maybe (Mode RawOpts, CliOpts -> Journal -> IO ())) -> [String] -> Journal -> IO ()
runFromFiles findBuiltinCommand inputfiles j = do
  dbg1IO "inputfiles" inputfiles
  -- read commands from all the inputfiles
  commands <- (flip concatMapM) inputfiles $ \f -> do
    dbg1IO "reading commands" f
    lines . T.unpack <$> T.readFile f

  forM_ commands (runCommand findBuiltinCommand j . parseCommand)

runFromArgs :: (String -> Maybe (Mode RawOpts, CliOpts -> Journal -> IO ())) -> [String] -> Journal -> IO ()
runFromArgs findBuiltinCommand args j = do
  -- read commands from all the inputfiles
  let commands = dbg1 "commands from args" $ splitAtElement "--" args
  forM_ commands (runCommand findBuiltinCommand j)

-- When commands are passed on the command line, shell will parse them for us
-- When commands are read from file, we need to split the line into command and arguments
parseCommand :: String -> [String]
parseCommand line =
  -- # begins a comment, ignore everything after #
  takeWhile (not. ((Just '#')==) . headMay) $  words' (strip line)

runCommand :: (String -> Maybe (Mode RawOpts, CliOpts -> Journal -> IO ())) -> Journal -> [String] -> IO ()
runCommand findBuiltinCommand j cmdline = do
  dbg1IO "running command" cmdline
  -- # begins a comment, ignore everything after #
  case cmdline of
    "echo":args -> putStrLn $ unwords $ args
    cmdname:args ->
      case findBuiltinCommand cmdname of
      Nothing -> putStrLn $ unwords (cmdname:args)
      Just (cmdmode,cmdaction) -> do
        opts <- getHledgerCliOpts' cmdmode args
        cmdaction opts j
    [] -> return ()

runREPL :: (String -> Maybe (Mode RawOpts, CliOpts -> Journal -> IO ())) -> Journal -> IO ()
runREPL findBuiltinCommand j = do
  putStrLn "Enter hledger commands, or 'help' for help."
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
        liftIO $ runCommand findBuiltinCommand j $ parseCommand input
        loop
