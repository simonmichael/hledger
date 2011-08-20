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
> > j <- readJournalFile "data/sample.journal"
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
> > t <- myTimelog

See "Hledger.Data.Ledger" for more examples.

-}

module Hledger.Cli.Main where

import Control.Monad
import Data.List
import Text.Printf

import Hledger.Cli.Add
import Hledger.Cli.Balance
import Hledger.Cli.Convert
import Hledger.Cli.Histogram
import Hledger.Cli.Print
import Hledger.Cli.Register
import Hledger.Cli.Stats
import Hledger.Cli.Options
import Hledger.Cli.Tests
import Hledger.Cli.Utils
import Hledger.Cli.Version

main :: IO ()
main = do
  opts <- getHledgerOpts
  when (debug_ opts) $ printf "%s\n" progversion >> printf "opts: %s\n" (show opts)
  runWith opts

runWith :: CliOpts -> IO ()
runWith opts = run' opts
    where 
      cmd = command_ opts
      run' opts
          | null cmd                                       = printModeHelpAndExit mainmode
          | any (cmd `isPrefixOf`) ["accounts","balance"]  = showModeHelpOr accountsmode $ withJournalDo opts balance
          | any (cmd `isPrefixOf`) ["activity","histogram"] = showModeHelpOr activitymode $ withJournalDo opts histogram
          | cmd `isPrefixOf` "add"                         = showModeHelpOr addmode $ withJournalDo opts add
          | cmd `isPrefixOf` "convert"                     = showModeHelpOr convertmode $ convert opts
          | any (cmd `isPrefixOf`) ["entries","print"]     = showModeHelpOr entriesmode $ withJournalDo opts print'
          | any (cmd `isPrefixOf`) ["postings","register"] = showModeHelpOr postingsmode $ withJournalDo opts register
          | cmd `isPrefixOf` "stats"                       = showModeHelpOr statsmode $ withJournalDo opts stats
          | cmd `isPrefixOf` "test"                        = showModeHelpOr testmode $ runtests opts >> return ()
          | cmd `isPrefixOf` "binaryfilename"              = showModeHelpOr binaryfilenamemode $ putStrLn $ binaryfilename progname
          | otherwise                                      = showModeHelpOr mainmode $ optserror $ "command "++cmd++" is not recognized"
      showModeHelpOr mode f = do
        when ("help" `in_` (rawopts_ opts)) $ printModeHelpAndExit mode
        when ("version" `in_` (rawopts_ opts)) $ printVersionAndExit
        f

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