#!/usr/bin/env runhaskell
{-|
hledger - a ledger-compatible text-based accounting tool.

Copyright (c) 2007-2008 Simon Michael <simon@joyful.com>
Released under GPL version 3 or later.

This is a minimal haskell clone of John Wiegley's ledger
<http://newartisans.com/software/ledger.html>.  hledger generates
simple ledger-compatible register & balance reports from a plain text
ledger file, and demonstrates a functional implementation of ledger.

You can use the command line:

> $ hledger --help

or ghci:

> $ ghci hledger
> > l <- ledgerfromfile "sample.ledger"
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
       | Help `elem` opts            = putStr usage
       | Version `elem` opts         = putStr version
       | cmd `isPrefixOf` "balance"  = parseLedgerAndDo opts args balance
       | cmd `isPrefixOf` "print"    = parseLedgerAndDo opts args print'
       | cmd `isPrefixOf` "register" = parseLedgerAndDo opts args register
       | cmd `isPrefixOf` "test"     = runtests args >> return ()
       | otherwise                   = putStr usage

-- | parse the user's specified ledger file and do some action with it
-- (or report a parse error). This function makes the whole thing go.
parseLedgerAndDo :: [Opt] -> [String] -> ([Opt] -> [String] -> Ledger -> IO ()) -> IO ()
parseLedgerAndDo opts args cmd = 
    ledgerFilePathFromOpts opts >>= parseLedgerFile >>= either printParseError runthecommand
    where
      runthecommand = cmd opts args . cacheLedger . normaliseRawLedgerAmounts . filterRawLedger begin end descpats cleared
      begin = beginDateFromOpts opts
      end = endDateFromOpts opts
      cleared = Cleared `elem` opts
      descpats = snd $ parseAccountDescriptionArgs args

