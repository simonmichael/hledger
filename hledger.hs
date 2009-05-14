-- sp doesn't like.. #!/usr/bin/env runhaskell
{-# OPTIONS_GHC -cpp #-}
{-|
hledger - a ledger-compatible text-based accounting tool.

Copyright (c) 2007-2009 Simon Michael <simon@joyful.com>
Released under GPL version 3 or later.

hledger is a partial haskell clone of John Wiegley's "ledger" text-based
accounting tool.  It generates ledger-compatible register & balance
reports from a plain text journal, and demonstrates a functional
implementation of ledger.  For more information, see ledger.org .

You can use the command line:

> $ hledger --help

or ghci:

> $ ghci hledger
> > l <- ledgerfromfilewithopts [] [] "sample.ledger"
> > balance [] [] l
>                  $-1  assets
>                   $2  expenses
>                  $-2  income
>                   $1  liabilities
> > register [] ["income","expenses"] l
> 2008/01/01 income               income:salary                   $-1          $-1
> 2008/06/01 gift                 income:gifts                    $-1          $-2
> 2008/06/03 eat & shop           expenses:food                    $1          $-1
>                                 expenses:supplies                $1            0

-}

module Main (
             -- for easy ghci access
             module Main,
             module Utils,
             module Options,
             module BalanceCommand,
             module ConvertCommand,
             module PrintCommand,
             module RegisterCommand,
             module HistogramCommand,
             module AddCommand,
#ifdef VTY
             module UICommand,
#endif
#ifdef HAPPS
             module WebCommand,
#endif
)
where
import Prelude hiding (putStr)
import Control.Monad.Error
import qualified Data.Map as Map (lookup)
import System.IO.UTF8
import System.IO (stderr)

import Version (versionmsg)
import Ledger
import Utils (withLedgerDo)
import Options
import Tests
import BalanceCommand
import ConvertCommand
import PrintCommand
import RegisterCommand
import HistogramCommand
import AddCommand
#ifdef VTY
import UICommand
#endif
#ifdef HAPPS
import WebCommand
#endif


main :: IO ()
main = do
  (opts, cmd, args) <- parseArguments
  run cmd opts args
    where 
      run cmd opts args
       | Help `elem` opts             = putStr $ usage
       | Version `elem` opts          = putStr versionmsg
       | cmd `isPrefixOf` "balance"   = withLedgerDo opts args balance
       | cmd `isPrefixOf` "convert"   = withLedgerDo opts args convert
       | cmd `isPrefixOf` "print"     = withLedgerDo opts args print'
       | cmd `isPrefixOf` "register"  = withLedgerDo opts args register
       | cmd `isPrefixOf` "histogram" = withLedgerDo opts args histogram
       | cmd `isPrefixOf` "add"       = withLedgerDo opts args add
#ifdef VTY
       | cmd `isPrefixOf` "ui"        = withLedgerDo opts args ui
#endif
#ifdef HAPPS
       | cmd `isPrefixOf` "web"       = withLedgerDo opts args web
#endif
       | cmd `isPrefixOf` "test"      = runtests opts args >> return ()
       | otherwise                    = putStr $ usage

