#!/usr/bin/env runhaskell
{-|
hledger - a ledger-compatible text-based accounting tool.

Copyright (c) 2007-2008 Simon Michael <simon@joyful.com>
Released under GPL version 3 or later.

This is a minimal haskell clone of John Wiegley's ledger
<http://newartisans.com/software/ledger.html>.  hledger generates
simple ledger-compatible register & balance reports from a standard
ledger file, and demonstrates a (naive) purely functional
implementation of ledger.

Code organization: the early code defined each major type and their
associated functions in a module. This was too restrictive, so now most
types are defined in "Types" at the bottom. The original modules are still
used to group functions related to each type.  Here is the approximate
import hierarchy:

@
hledger ("Main")
 "Tests"
 "Parse"
  "Options"
  "Models"
   "TimeLog"
   "Ledger"
    "Account"
     "Transaction"
    "RawLedger"
     "LedgerEntry"
      "RawTransaction"
       "AccountName"
       "Amount"
        "Currency"
         "Types"
          "Utils"
@

This module includes some helpers for querying your ledger in ghci. Examples:

> $ rm -f hledger.o
> $ ghci hledger.hs
> *Main> l <- myledger
> Ledger with 696 entries, 132 accounts
> *Main> putStr $ drawTree $ treemap show $ accountnametree l
> ...
> *Main> putStr $ showLedgerAccounts l 1
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
import System
import Text.ParserCombinators.Parsec (ParseError)
import qualified Data.Map as Map (lookup)

import Options
import Models
import Parse (parseLedgerAndDo, parseLedgerFile)
import Tests (hunit, quickcheck)
import Utils hiding (test)


main :: IO ()
main = do
  (opts, (cmd:args)) <- getArgs >>= parseOptions
  let pats = parsePatternArgs args
  run cmd opts pats
    where run cmd opts pats
              | Help `elem` opts            = putStr usage
              | cmd `isPrefixOf` "selftest" = selftest opts pats
              | cmd `isPrefixOf` "print"    = print_   opts pats
              | cmd `isPrefixOf` "register" = register opts pats
              | cmd `isPrefixOf` "balance"  = balance  opts pats
              | otherwise                   = putStr usage

type Command = [Flag] -> (Regex,Regex) -> IO ()

selftest :: Command
selftest opts pats = do 
  hunit
  quickcheck
  return ()

print_ :: Command
print_ opts pats = parseLedgerAndDo opts pats printentries

register :: Command
register opts pats = parseLedgerAndDo opts pats printregister

balance :: Command
balance opts pats = do
  parseLedgerAndDo opts pats printbalance
    where
      printbalance l =
          putStr $ showLedgerAccounts l depth
              where 
                showsubs = (ShowSubs `elem` opts)
                depth = case (pats, showsubs) of
                          -- when there are no account patterns and no -s, show
                          -- only to depth 1. (This was clearer when we used maybe.)
                          ((wildcard,_), False) -> 1
                          otherwise  -> 9999

-- ghci helpers

-- | get a Ledger from the file your LEDGER environment variable points to
-- or (WARNING) an empty one if there was a problem.
myledger :: IO Ledger
myledger = do
  parsed <- ledgerFilePath [] >>= parseLedgerFile
  let ledgerfile = either (\_ -> RawLedger [] [] [] "") id parsed
  return $ cacheLedger ledgerfile (wildcard,wildcard)

-- | get a Ledger from the given file path
ledgerfromfile :: String -> IO Ledger
ledgerfromfile f = do
  parsed <- ledgerFilePath [File f] >>= parseLedgerFile
  let ledgerfile = either (\_ -> RawLedger [] [] [] "") id parsed
  return $ cacheLedger ledgerfile (wildcard,wildcard)

-- | get a named account from your ledger file
accountnamed :: AccountName -> IO Account
accountnamed a = myledger >>= (return . fromMaybe nullacct . Map.lookup a . accounts)

