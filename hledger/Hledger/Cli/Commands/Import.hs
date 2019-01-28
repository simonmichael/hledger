{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Hledger.Cli.Commands.Import (
  importmode
 ,importcmd
) 
where

import Control.Monad
import Data.List
import Data.Ord
import Hledger
import Hledger.Cli.CliOptions
import Hledger.Cli.Commands.Add (journalAddTransaction)
-- import Hledger.Cli.Commands.Print (print')
import System.Console.CmdArgs.Explicit
import Text.Printf

importmode = hledgerCommandMode
  ($(hereFileRelative "Hledger/Cli/Commands/Import.md"))
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
      enewj <- readJournalFiles iopts' fs
      case enewj of
        Left e     -> error' e 
        Right newj ->
          case sortBy (comparing tdate) $ jtxns newj of
            [] -> return ()
            newts | dryrun -> do
              printf "; would import %d new transactions:\n\n" (length newts)
              -- TODO how to force output here ?
              -- length (jtxns newj) `seq` print' opts{rawopts_=("explicit",""):rawopts} newj
              mapM_ (putStr . showTransactionUnelided) newts
            newts -> do
              foldM (flip journalAddTransaction opts) j newts  -- gets forced somehow.. (how ?)
              printf "imported %d new transactions\n" (length newts)
