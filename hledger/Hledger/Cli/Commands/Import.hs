{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Hledger.Cli.Commands.Import (
  importmode
 ,importcmd
) 
where

import Control.Monad
import Data.List
import Data.Ord
import Data.String.Here
import Hledger
import Hledger.Cli.CliOptions
import Hledger.Cli.Commands.Add (journalAddTransaction)
-- import Hledger.Cli.Commands.Print (print')
import System.Console.CmdArgs.Explicit
import Text.Printf

importmode = hledgerCommandMode
  [here| import
Read new transactions added to each FILE since last run, and add them to
the main journal file. Or with --dry-run, just print the transactions 
that would be added.

Input files are provided as arguments, or glob patterns. So eg to add new 
transactions from all CSV files to the main journal: hledger import *.csv

New transactions are detected like print --new (using .latest.FILE state files)

FLAGS
  |]
  [flagNone ["dry-run"] (\opts -> setboolopt "dry-run" opts) "just show the transactions to be imported"] 
  [generalflagsgroup1]
  []
  ([], Just $ argsFlag "FILE [...]")

importcmd opts@CliOpts{rawopts_=rawopts,inputopts_=iopts} j = do
  let
    inputfiles = listofstringopt "args" rawopts
    dryrun = boolopt "dry-run" rawopts
    iopts' = iopts{new_=True, new_save_=not dryrun}
  case inputfiles of
    [] -> error' "please provide one or more input files as arguments"
    fs -> do
      enewj <- readJournalFilesWithOpts iopts' fs
      case enewj of
        Left e     -> error' e 
        Right newj ->
          case sortBy (comparing tdate) $ jtxns newj of
            [] -> putStrLn "no new transactions"
            newts | dryrun -> do
              printf "would import %d new transactions:\n\n" (length newts)
              -- TODO how to force output here ?
              -- length (jtxns newj) `seq` print' opts{rawopts_=("explicit",""):rawopts} newj
              mapM_ (putStr . showTransactionUnelided) newts
            newts -> do
              foldM (flip journalAddTransaction opts) j newts  -- gets forced somehow.. (how ?)
              printf "imported %d new transactions\n" (length newts)