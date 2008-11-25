#!/usr/bin/env runhaskell
{-|
hledger - a ledger-compatible text-based accounting tool.

Copyright (c) 2007-2008 Simon Michael <simon@joyful.com>
Released under GPL version 3 or later.

hledger is a minimal haskell clone of John Wiegley's "ledger" text-based
accounting tool (http://newartisans.com/software/ledger.html).  hledger
generates ledger-compatible register & balance reports from a plain text
ledger file, and demonstrates a functional implementation of ledger.  For
more information, see the hledger home page: http://joyful.com/hledger

You can use the command line:

> $ hledger --help

or ghci:

> $ ghci hledger
> > l <- ledgerfromfile [] "sample.ledger"
> > balance [] [] l
>                  $-1  assets
>                   $2  expenses
>                  $-2  income
>                   $1  liabilities:debts
> > register [] ["income","expenses"] l
> 2007/01/01 income               income:salary                   $-1          $-1
> 2007/01/01 gift                 income:gifts                    $-1          $-2
> 2007/01/01 eat & shop           expenses:food                    $1          $-1
>                                 expenses:supplies                $1            0

-}

module Main (
             module Main,
             module Utils,
             module Options,
             module BalanceCommand,
             module PrintCommand,
             module RegisterCommand,
)
where
import qualified Data.Map as Map (lookup)
import Ledger
import Utils
import Options
import BalanceCommand
import PrintCommand
import RegisterCommand
import Tests


main :: IO ()
main = do
  (opts, cmd, args) <- parseArguments
  run cmd opts args
    where 
      run cmd opts args
       | Help `elem` opts            = putStr $ usage opts
       | Version `elem` opts         = putStr version
       | cmd `isPrefixOf` "balance"  = parseLedgerAndDo opts args balance
       | cmd `isPrefixOf` "print"    = parseLedgerAndDo opts args print'
       | cmd `isPrefixOf` "register" = parseLedgerAndDo opts args register
       | cmd `isPrefixOf` "test"     = runtests opts args >> return ()
       | otherwise                   = putStr $ usage opts

-- | parse the user's specified ledger file and do some action with it
-- (or report a parse error). This function makes the whole thing go.
parseLedgerAndDo :: [Opt] -> [String] -> ([Opt] -> [String] -> Ledger -> IO ()) -> IO ()
parseLedgerAndDo opts args cmd = 
    ledgerFilePathFromOpts opts >>= parseLedgerFile >>= either printParseError runcmd
    where
      runcmd = cmd opts args . cacheLedger apats . filterRawLedger b e dpats c r . canonicaliseAmounts costbasis
      b = beginDateFromOpts opts
      e = endDateFromOpts opts
      (apats,dpats) = parseAccountDescriptionArgs opts args
      c = Cleared `elem` opts
      r = Real `elem` opts
      costbasis = CostBasis `elem` opts

