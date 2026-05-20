{-|

The @gettxns@ command fetches transaction data for the journal, by
calling a @gettxns_@ shell script which downloads data (typically CSV/TSV/SSV files)
from banks, brokerages, etc. The script is invoked with the journal's
@data/@ directory as working directory, so it can save files there.

-}

{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Hledger.Cli.Commands.Gettxns (
  gettxnsmode
 ,gettxns
) where

import Control.Exception (IOException, try)
import System.Console.CmdArgs.Explicit
import System.Directory (createDirectoryIfMissing, doesFileExist, findExecutable)
import System.Exit (ExitCode(..))
import System.FilePath (takeDirectory, (</>))
import System.IO (hPutStrLn, stderr)
import System.Process (proc, cwd, readCreateProcessWithExitCode)

import Hledger
import Hledger.Cli.CliOptions


-- | Command line options for this command.
gettxnsmode = hledgerCommandMode
  $(embedFileRelative "Hledger/Cli/Commands/Gettxns.txt")
  [flagNone ["dry-run"] (setboolopt "dry-run") "just print the command that would be run"
  ]
  cligeneralflagsgroups2
  hiddenflags
  ([], Nothing)

scriptName :: FilePath
scriptName = "gettxns_"

-- | Like 'warn' but throws away the trailing-action argument; for the common
-- "log a warning, then continue" pattern.
warn_ :: String -> IO ()
warn_ msg = warn msg (return ())

-- | The gettxns command.
gettxns :: CliOpts -> Journal -> IO ()
gettxns CliOpts{rawopts_=rawopts} j = do
  let
    dryRun  = boolopt "dry-run" rawopts
    jdir    = takeDirectory (journalFilePath j)
    dataDir = jdir </> dataDirName
    localScript = dataDir </> scriptName
    notFound = error' $ unlines
      [scriptName <> " was not found in JOURNALDIR/" <> dataDirName <> "/ or in PATH."
      ,"Please install bin/" <> scriptName <> " from the hledger source tree,"
      ,"or your own script, in your " <> dataDirName <> "/ directory or in $PATH."
      ]
  createDirectoryIfMissing True dataDir
  script <- if dryRun
              then return scriptName
              else do
                localExists <- doesFileExist localScript
                if localExists
                  then return localScript
                  else findExecutable scriptName >>= maybe notFound return
  let cmdline = "cd " <> dataDir <> " && " <> scriptName
  hPutStrLn stderr cmdline
  if dryRun
    then return ()
    else do
      eres <- try (readCreateProcessWithExitCode (proc script []){cwd = Just dataDir} "")
                :: IO (Either IOException (ExitCode, String, String))
      case eres of
        Left e ->
          warn_ $ scriptName <> " failed: " <> show e
        Right (ec, out, err) -> do
          putStr out
          hPutStrLn stderr err
          case ec of
            ExitFailure n -> warn_ $ scriptName <> " exited " <> show n
            ExitSuccess   -> return ()
