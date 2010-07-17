{-# LANGUAGE CPP #-}
{-|
hledger - a ledger-compatible accounting tool.
Copyright (c) 2007-2010 Simon Michael <simon@joyful.com>
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
#if defined(WEB) || defined(WEB610)
import System.Info (os)
#endif
#if __GLASGOW_HASKELL__ <= 610
import Prelude hiding (putStr, putStrLn)
import System.IO.UTF8
#endif

import Hledger.Cli.Commands.All
import Hledger.Data
import Hledger.Cli.Options
import Hledger.Cli.Tests
import Hledger.Cli.Utils (withJournalDo)
import Hledger.Cli.Version (versionmsg, binaryfilename)

main :: IO ()
main = do
  (opts, cmd, args) <- parseArguments
  run cmd opts args
    where
      run cmd opts args
       | Help `elem` opts             = putStr help1
       | HelpOptions `elem` opts      = putStr help2
       | HelpAll `elem` opts          = putStr $ help1 ++ "\n" ++ help2
       | Version `elem` opts          = putStrLn versionmsg
       | BinaryFilename `elem` opts   = putStrLn binaryfilename
       | null cmd                     = maybe (putStr help1) (withJournalDo opts args cmd) defaultcmd
       | cmd `isPrefixOf` "balance"   = withJournalDo opts args cmd balance
       | cmd `isPrefixOf` "convert"   = withJournalDo opts args cmd convert
       | cmd `isPrefixOf` "print"     = withJournalDo opts args cmd print'
       | cmd `isPrefixOf` "register"  = withJournalDo opts args cmd register
       | cmd `isPrefixOf` "histogram" = withJournalDo opts args cmd histogram
       | cmd `isPrefixOf` "add"       = withJournalDo opts args cmd add
       | cmd `isPrefixOf` "stats"     = withJournalDo opts args cmd stats
#ifdef VTY
       | cmd `isPrefixOf` "vty"       = withJournalDo opts args cmd vty
#endif
#if defined(WEB) || defined(WEB610)
       | cmd `isPrefixOf` "web"       = withJournalDo opts args cmd web
#endif
#ifdef CHART
       | cmd `isPrefixOf` "chart"       = withJournalDo opts args cmd chart
#endif
       | cmd `isPrefixOf` "test"      = runtests opts args >> return ()
       | otherwise                    = putStr help1

-- in a web-enabled build on windows, run the web ui by default
#if defined(WEB) || defined(WEB610)
      defaultcmd | os=="mingw32" = Just web
                 | otherwise = Nothing
#else
      defaultcmd = Nothing
#endif
