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

#if __GLASGOW_HASKELL__ <= 610
import Prelude hiding (putStr, putStrLn)
import System.IO.UTF8
#endif

import Hledger.Data
import Hledger.Cli.Commands
import Hledger.Cli.Options
import Hledger.Cli.Tests
import Hledger.Cli.Utils (withJournalDo)
import Hledger.Cli.Version (progversionstr, binaryfilename)

main :: IO ()
main = do
  (opts, args) <- parseArgumentsWith options_cli
  run opts args
    where
      run opts _
       | Help `elem` opts             = putStr usage_cli
       | Version `elem` opts          = putStrLn $ progversionstr progname_cli
       | BinaryFilename `elem` opts   = putStrLn $ binaryfilename progname_cli
      run _ []                        = argsError "a command is required."
      run opts (cmd:args)
       | cmd `isPrefixOf` "balance"   = withJournalDo opts args cmd balance
       | cmd `isPrefixOf` "convert"   = withJournalDo opts args cmd convert
       | cmd `isPrefixOf` "print"     = withJournalDo opts args cmd print'
       | cmd `isPrefixOf` "register"  = withJournalDo opts args cmd register
       | cmd `isPrefixOf` "histogram" = withJournalDo opts args cmd histogram
       | cmd `isPrefixOf` "add"       = withJournalDo opts args cmd add
       | cmd `isPrefixOf` "stats"     = withJournalDo opts args cmd stats
       | cmd `isPrefixOf` "test"      = runtests opts args >> return ()
       | otherwise                    = argsError $ "command "++cmd++" is unrecognized."
