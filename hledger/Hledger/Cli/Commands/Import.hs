{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}

module Hledger.Cli.Commands.Import (
  importmode
 ,importcmd
)
where

import Control.Monad
import Data.List
import qualified Data.Text.IO as T
import System.Console.CmdArgs.Explicit
import Text.Printf

import Hledger
import Hledger.Cli.CliOptions
import Hledger.Cli.Commands.Add (journalAddTransaction)
import System.IO (stderr)

importmode = hledgerCommandMode
  $(embedFileRelative "Hledger/Cli/Commands/Import.txt")
  [flagNone ["catchup"] (setboolopt "catchup") "just mark all transactions as already imported"
  ,flagNone ["dry-run"] (setboolopt "dry-run") "just show the transactions to be imported"
  ]
  cligeneralflagsgroups1
  hiddenflags
  ([], Just $ argsFlag "FILE [...]")

importcmd opts@CliOpts{rawopts_=rawopts,inputopts_=iopts} j = do
  -- XXX could be helpful to show the last-seen date, and number of old transactions, too
  let
    inputfiles = listofstringopt "args" rawopts
    inputstr = intercalate ", " $ map quoteIfNeeded inputfiles
    catchup = boolopt "catchup" rawopts
    dryrun = boolopt "dry-run" rawopts
    combinedStyles = 
      let
        maybeInputStyles = commodity_styles_ . balancingopts_ $ iopts
        inferredStyles =  journalCommodityStyles j
      in
        case maybeInputStyles of
          Nothing -> Just inferredStyles
          Just inputStyles -> Just $ inputStyles <> inferredStyles

    iopts' = iopts{
      new_=True,  -- read only new transactions since last time
      new_save_=False,  -- defer saving .latest files until the end
      strict_=False,  -- defer strict checks until the end
      balancingopts_=defbalancingopts{commodity_styles_= combinedStyles}  -- use amount styles from both when balancing txns
      }

  case inputfiles of
    [] -> error' "please provide one or more input files as arguments"  -- PARTIAL:
    fs -> do
      enewjandlatestdatesforfiles <- runExceptT $ readJournalFilesAndLatestDates iopts' fs
      case enewjandlatestdatesforfiles of
        Left err -> error' err
        Right (newj, latestdatesforfiles) ->
          case sortOn tdate $ jtxns newj of
            -- with --dry-run the output should be valid journal format, so messages have ; prepended
            [] -> do
              -- in this case, we vary the output depending on --dry-run, which is a bit awkward
              let semicolon = if dryrun then "; " else "" :: String
              hPrintf stderr "%sno new transactions found in %s\n\n" semicolon inputstr

            newts | catchup ->
              if dryrun
                then hPrintf stderr "would skip %d new transactions (dry run)\n\n" (length newts)
                else do
                  hPrintf stderr "marked %s as caught up, skipping %d transactions\n\n" inputstr (length newts)
                  saveLatestDatesForFiles latestdatesforfiles

            newts -> do
              if dryrun
              then do
                -- show txns to be imported
                hPrintf stderr "would import %d new transactions from %s:\n\n" (length newts) inputstr
                mapM_ (T.putStr . showTransaction) newts

                -- then check the whole journal with them added, if in strict mode
                when (strict_ iopts) $ strictChecks

              else do
                -- first check the whole journal with them added, if in strict mode
                when (strict_ iopts) $ strictChecks

                -- then append the transactions to the main journal file.
                -- XXX This writes unix line endings (\n), some at least,
                -- even if the file uses dos line endings (\r\n), which could leave
                -- mixed line endings in the file. See also writeFileWithBackupIfChanged.
                foldM_ (`journalAddTransaction` opts) j newts  -- gets forced somehow.. (how ?)

                hPrintf stderr "imported %d new transactions from %s to %s\n" (length newts) inputstr (journalFilePath j)

                -- and if we got this far, update each file's .latest file
                saveLatestDatesForFiles latestdatesforfiles

              where
                -- add the new transactions to the journal in memory and check the whole thing
                strictChecks = either fail pure $ journalStrictChecks j'
                  where j' = foldl' (flip addTransaction) j newts
