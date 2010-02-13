#!/usr/bin/env runhaskell
{-|
hledger - a ledger-compatible text-based accounting tool.

Copyright (c) 2007-2010 Simon Michael <simon@joyful.com>
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
import HledgerMain (main)
