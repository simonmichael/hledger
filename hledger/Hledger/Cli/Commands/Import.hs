{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Hledger.Cli.Commands.Import (
  importmode
 ,importcmd
)
where

import Control.Monad
import Data.List
import Hledger
import Hledger.Cli.CliOptions
import Hledger.Cli.Commands.Add (journalAddTransaction)
-- import Hledger.Cli.Commands.Print (print')
import System.Console.CmdArgs.Explicit
import Text.Printf

importmode = hledgerCommandMode
  $(embedFileRelative "Hledger/Cli/Commands/Import.txt")
  [flagNone ["catchup"] (setboolopt "catchup") "just mark all transactions as already imported"
  ,flagNone ["dry-run"] (setboolopt "dry-run") "just show the transactions to be imported"
  ]
  [generalflagsgroup1]
  hiddenflags
  ([], Just $ argsFlag "FILE [...]")

importcmd opts@CliOpts{rawopts_=rawopts,inputopts_=iopts} j = do
  let
    inputfiles = listofstringopt "args" rawopts
    inputstr = intercalate ", " inputfiles
    catchup = boolopt "catchup" rawopts
    dryrun = boolopt "dry-run" rawopts
    iopts' = iopts{new_=True, new_save_=not dryrun}
  case inputfiles of
    [] -> error' "please provide one or more input files as arguments"
    fs -> do
      enewj <- readJournalFiles iopts' fs
      case enewj of
        Left e     -> error' e
        Right newj ->
          case sortOn tdate $ jtxns newj of
            [] -> do
              printf "; no new transactions found.\n\n"
            newts | dryrun -> do
              printf "; would import %d new transactions:\n\n" (length newts)
              -- TODO how to force output here ?
              -- length (jtxns newj) `seq` print' opts{rawopts_=("explicit",""):rawopts} newj
              mapM_ (putStr . showTransactionUnelided) newts
            newts | catchup -> do
              printf "marked %s as caught up, skipping %d unimported transactions\n\n" inputstr (length newts)
            newts -> do
              foldM_ (`journalAddTransaction` opts) j newts  -- gets forced somehow.. (how ?)
              printf "imported %d new transactions\n" (length newts)
