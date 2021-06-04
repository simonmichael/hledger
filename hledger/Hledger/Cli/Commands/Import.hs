{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Hledger.Cli.Commands.Import (
  importmode
 ,importcmd
)
where

import Control.Monad
import Data.List
import qualified Data.Text.IO as T
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
  -- XXX could be helpful to show the last-seen date, and number of old transactions, too
  let
    inputfiles = listofstringopt "args" rawopts
    inputstr = intercalate ", " $ map quoteIfNeeded inputfiles
    catchup = boolopt "catchup" rawopts
    dryrun = boolopt "dry-run" rawopts
    iopts' = iopts{new_=True, new_save_=not dryrun, balancingopts_=balancingOpts{commodity_styles_=Just $ journalCommodityStyles j}}
  case inputfiles of
    [] -> error' "please provide one or more input files as arguments"  -- PARTIAL:
    fs -> do
      enewj <- readJournalFiles iopts' fs
      case enewj of
        Left e     -> error' e
        Right newj ->
          case sortOn tdate $ jtxns newj of
            -- with --dry-run the output should be valid journal format, so messages have ; prepended
            [] -> do
              -- in this case, we vary the output depending on --dry-run, which is a bit awkward
              let semicolon = if dryrun then "; " else "" :: String
              printf "%sno new transactions found in %s\n\n" semicolon inputstr
            newts | dryrun -> do
              printf "; would import %d new transactions from %s:\n\n" (length newts) inputstr
              -- TODO how to force output here ?
              -- length (jtxns newj) `seq` print' opts{rawopts_=("explicit",""):rawopts} newj
              mapM_ (T.putStr . showTransaction) newts
            newts | catchup -> do
              printf "marked %s as caught up, skipping %d unimported transactions\n\n" inputstr (length newts)
            newts -> do
              -- XXX This writes unix line endings (\n), some at least,
              -- even if the file uses dos line endings (\r\n), which could leave
              -- mixed line endings in the file. See also writeFileWithBackupIfChanged.
              foldM_ (`journalAddTransaction` opts) j newts  -- gets forced somehow.. (how ?)
              printf "imported %d new transactions from %s\n" (length newts) inputstr
