-- #!/usr/bin/env runhaskell  <- sp doesn't like
{-# OPTIONS_GHC -cpp #-}
{-|
hledger - a ledger-compatible text-based accounting tool.

Copyright (c) 2007-2009 Simon Michael <simon@joyful.com>
Released under GPL version 3 or later.

hledger is a partial haskell clone of John Wiegley's "ledger" text-based
accounting tool.  It generates ledger-compatible register & balance
reports from a plain text journal, and demonstrates a functional
implementation of ledger.  For more information, see http:\/\/hledger.org .

You can use the command line:

> $ hledger --help

or ghci:

> $ ghci hledger
> > l <- readLedger "sample.ledger"
> > register [] ["income","expenses"] l
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

See "Ledger.Ledger" for more examples.
-}

module Main where
import Prelude hiding (putStr, putStrLn)
import System.IO.UTF8

import Commands.All
import Ledger
import Options
import Tests
import Utils (withLedgerDo)
import Version (versionmsg, binaryfilename)

main :: IO ()
main = do
  (opts, cmd, args) <- parseArguments
  run cmd opts args
    where
      run cmd opts args
       | Help `elem` opts             = putStr usage
       | Version `elem` opts          = putStrLn versionmsg
       | BinaryFilename `elem` opts   = putStrLn binaryfilename
       | cmd `isPrefixOf` "balance"   = withLedgerDo opts args cmd balance
       | cmd `isPrefixOf` "convert"   = withLedgerDo opts args cmd convert
       | cmd `isPrefixOf` "print"     = withLedgerDo opts args cmd print'
       | cmd `isPrefixOf` "register"  = withLedgerDo opts args cmd register
       | cmd `isPrefixOf` "histogram" = withLedgerDo opts args cmd histogram
       | cmd `isPrefixOf` "add"       = withLedgerDo opts args cmd add
       | cmd `isPrefixOf` "stats"     = withLedgerDo opts args cmd stats
#ifdef VTY
       | cmd `isPrefixOf` "ui"        = withLedgerDo opts args cmd ui
#endif
#ifdef HAPPS
       | cmd `isPrefixOf` "web"       = withLedgerDo opts args cmd web
#endif
       | cmd `isPrefixOf` "test"      = runtests opts args >> return ()
       | otherwise                    = putStr usage
