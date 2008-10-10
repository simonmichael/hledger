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

This module includes some helpers for working with your ledger in ghci. Examples:

> $ rm -f hledger.o
> $ ghci hledger.hs
> *Main> l <- ledger
> Ledger with 696 entries, 132 accounts
> *Main> putStr $ drawTree $ treemap show $ accountnametree l
> ...
> *Main> putStr $ showLedgerAccountBalances l 1
> ...
> *Main> printregister l
> ...
> *Main> accounts l
> ...
> *Main> accountnamed "expenses:food:groceries"
> Account expenses:food:groceries with 60 transactions

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


main :: IO ()
main = do
  (opts, cmd, args) <- parseArguments
  run cmd opts args
    where 
      run cmd opts args
       | Help `elem` opts            = putStr usage
       | Version `elem` opts         = putStr version
       | cmd `isPrefixOf` "selftest" = hunit >> quickcheck >> return ()
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
      runthecommand = cmd opts args . cacheLedger acctpat . filterLedgerEntries begin end descpat
      begin = beginDateFromOpts opts
      end = endDateFromOpts opts
      acctpat = regexFor acctpats
      descpat = regexFor descpats
      (acctpats,descpats) = parseAccountDescriptionArgs args

-- ghci helpers

-- | get a RawLedger from the file your LEDGER environment variable points to
-- or (WARNING) an empty one if there was a problem.
myrawledger :: IO RawLedger
myrawledger = do
  parsed <- ledgerFilePathFromOpts [] >>= parseLedgerFile
  return $ either (\_ -> RawLedger [] [] [] "") id parsed

-- | as above, and convert it to a cached Ledger
myledger :: IO Ledger
myledger = do
  l <- myrawledger
  return $ cacheLedger wildcard $ filterLedgerEntries "" "" wildcard l

-- | get a Ledger from the given file path
rawledgerfromfile :: String -> IO RawLedger
rawledgerfromfile f = do
  parsed <- ledgerFilePathFromOpts [File f] >>= parseLedgerFile
  return $ either (\_ -> RawLedger [] [] [] "") id parsed

-- | get a named account from your ledger file
accountnamed :: AccountName -> IO Account
accountnamed a = myledger >>= (return . fromMaybe nullacct . Map.lookup a . accounts)

