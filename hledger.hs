#!/usr/bin/env runhaskell
{-|
hledger - a ledger-compatible text-based accounting tool.

Copyright (c) 2007-2008 Simon Michael <simon@joyful.com>
Released under GPL version 3 or later.

This is a minimal haskell clone of John Wiegley's ledger
<http://newartisans.com/software/ledger.html>.  hledger generates
simple ledger-compatible register & balance reports from a plain text
ledger file, and demonstrates a (naive) purely functional
implementation of ledger.

-}

module Main
where
import qualified Data.Map as Map (lookup)
import Options
import Ledger
import BalanceCommand
import PrintCommand
import RegisterCommand
import Tests
import Utils


main :: IO ()
main = do
  (opts, cmd, args) <- parseArguments
  run cmd opts args
    where 
      run cmd opts args
       | Help `elem` opts            = putStr usage
       | Version `elem` opts         = putStr version
       | cmd `isPrefixOf` "selftest" = runhunit >> return ()
       | cmd `isPrefixOf` "print"    = parseLedgerAndDo opts args printentries
       | cmd `isPrefixOf` "register" = parseLedgerAndDo opts args printregister
       | cmd `isPrefixOf` "balance"  = parseLedgerAndDo opts args printbalance
       | otherwise                   = putStr usage

-- | parse the user's specified ledger file and do some action with it
-- (or report a parse error). This function makes the whole thing go.
parseLedgerAndDo :: [Opt] -> [String] -> ([Opt] -> [String] -> Ledger -> IO ()) -> IO ()
parseLedgerAndDo opts args cmd = 
    ledgerFilePathFromOpts opts >>= parseLedgerFile >>= either printParseError runthecommand
    where
      runthecommand = cmd opts args . cacheLedger acctpat . filterRawLedgerEntries begin end descpat
      begin = beginDateFromOpts opts
      end = endDateFromOpts opts
      acctpat = regexFor acctpats
      descpat = regexFor descpats
      (acctpats,descpats) = parseAccountDescriptionArgs args

