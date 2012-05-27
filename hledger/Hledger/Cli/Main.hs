{-|
hledger - a ledger-compatible accounting tool.
Copyright (c) 2007-2011 Simon Michael <simon@joyful.com>
Released under GPL version 3 or later.

hledger is a partial haskell clone of John Wiegley's "ledger".  It
generates ledger-compatible register & balance reports from a plain text
journal, and demonstrates a functional implementation of ledger.
For more information, see http:\/\/hledger.org .

This module provides the main function for the hledger command-line
executable. It is exposed here so that it can be imported by eg benchmark
scripts.

You can use the command line:

> $ hledger --help

or ghci:

> $ ghci hledger
> > j <- readJournalFile Nothing Nothing "data/sample.journal"
> > register [] ["income","expenses"] j
> 2008/01/01 income               income:salary                   $-1          $-1
> 2008/06/01 gift                 income:gifts                    $-1          $-2
> 2008/06/03 eat & shop           expenses:food                    $1          $-1
>                                 expenses:supplies                $1            0
> > balance [Depth "1"] [] l
>                  $-1  assets
>                   $2  expenses
>                  $-2  income
>                   $1  liabilities
> > l <- myLedger

See "Hledger.Data.Ledger" for more examples.

-}

module Hledger.Cli.Main where

import Control.Monad
import Data.List
import Safe
import System.Environment
import System.Exit
import System.Process
import Text.Printf

import Hledger (ensureJournalFileExists)
import Hledger.Cli.Add
import Hledger.Cli.Balance
import Hledger.Cli.Balancesheet
import Hledger.Cli.Histogram
import Hledger.Cli.Incomestatement
import Hledger.Cli.Print
import Hledger.Cli.Register
import Hledger.Cli.Stats
import Hledger.Cli.Options
import Hledger.Cli.Tests
import Hledger.Cli.Utils
import Hledger.Cli.Version
import Hledger.Utils
import Hledger.Reports
import Hledger.Data.Dates

main :: IO ()
main = do
  args <- getArgs
  addons <- getHledgerAddonCommands
  opts <- getHledgerCliOpts addons
  when (debug_ opts) $ do
    printf "%s\n" prognameandversion
    printf "args: %s\n" (show args)
    printf "opts: %s\n" (show opts)
    d <- getCurrentDay
    printf "query: %s\n" (show $ queryFromOpts d $ reportopts_ opts)

  run' opts addons args
    where
      run' opts@CliOpts{command_=cmd} addons args
       -- delicate, add tests before changing (eg --version, ADDONCMD --version, INTERNALCMD --version)
       | (null matchedaddon) && "version" `in_` (rawopts_ opts)         = putStrLn prognameandversion
       | (null matchedaddon) && "binary-filename" `in_` (rawopts_ opts) = putStrLn $ binaryfilename progname
       | null cmd                                        = putStr $ showModeHelp mainmode'
       | cmd `isPrefixOf` "add"                          = showModeHelpOr addmode      $ journalFilePathFromOpts opts >>= ensureJournalFileExists >> withJournalDo opts add
       | cmd `isPrefixOf` "test"                         = showModeHelpOr testmode     $ test' opts
       | any (cmd `isPrefixOf`) ["accounts","balance"]   = showModeHelpOr accountsmode $ withJournalDo opts balance
       | any (cmd `isPrefixOf`) ["entries","print"]      = showModeHelpOr entriesmode  $ withJournalDo opts print'
       | any (cmd `isPrefixOf`) ["postings","register"]  = showModeHelpOr postingsmode $ withJournalDo opts register
       | any (cmd `isPrefixOf`) ["activity","histogram"] = showModeHelpOr activitymode $ withJournalDo opts histogram
       | cmd `isPrefixOf` "incomestatement"              = showModeHelpOr activitymode $ withJournalDo opts incomestatement
       | any (cmd `isPrefixOf`) ["balancesheet","bs"]    = showModeHelpOr activitymode $ withJournalDo opts balancesheet
       | cmd `isPrefixOf` "stats"                        = showModeHelpOr statsmode    $ withJournalDo opts stats
       | not (null matchedaddon)                           = do
                                                             when (debug_ opts) $ printf "running %s\n" shellcmd
                                                             system shellcmd >>= exitWith
       | cmd == "convert"                                = optserror ("convert is no longer needed, just use -f FILE.csv") >> exitFailure
       | otherwise                                       = optserror ("command "++cmd++" is not recognized") >> exitFailure
       where
        mainmode' = mainmode addons
        showModeHelpOr mode f | "help" `in_` (rawopts_ opts) = putStr $ showModeHelp mode
                              | otherwise = f
        matchedaddon | null cmd  = ""
                     | otherwise = headDef "" $ filter (cmd `isPrefixOf`) addons
        shellcmd = printf "%s-%s %s" progname matchedaddon (unwords' subcmdargs)
        subcmdargs = args1 ++ drop 1 args2 where (args1,args2) = break (== cmd) $ filter (/="--") args

{- tests:

hledger -> main help
hledger --help -> main help
hledger --help command -> command help
hledger command --help -> command help
hledger badcommand -> unrecognized command, try --help (non-zero exit)
hledger badcommand --help -> main help
hledger --help badcommand -> main help
hledger --mainflag command -> works
hledger command --mainflag -> works
hledger command --commandflag -> works
hledger command --mainflag --commandflag -> works
XX hledger --mainflag command --commandflag -> works
XX hledger --commandflag command -> works
XX hledger --commandflag command --mainflag -> works

-}