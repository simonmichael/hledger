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
import System
import qualified Data.Map as Map (lookup)

import Options
import Tests (hunit, quickcheck)
import Ledger.Parse (parseLedgerFile, parseError)
import Ledger.Utils hiding (test)
import Ledger hiding (rawledger)


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

type Command = [Flag] -> ([String],[String]) -> IO ()

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
balance opts pats = parseLedgerAndDo opts pats printbalance
    where
      printbalance :: Ledger -> IO ()
      printbalance l = putStr $ showLedgerAccountBalances l depth
          where 
            showsubs = (ShowSubs `elem` opts)
            depth = case (pats, showsubs) of
                      -- when there is no -s or pattern args, show with depth 1
                      (([],[]), False) -> 1
                      otherwise  -> 9999

-- | parse the user's specified ledger file and do some action with it
-- (or report a parse error). This function makes the whole thing go.
parseLedgerAndDo :: [Flag] -> ([String],[String]) -> (Ledger -> IO ()) -> IO ()
parseLedgerAndDo opts (apats,dpats) cmd = do
    path <- ledgerFilePath opts
    parsed <- parseLedgerFile path
    case parsed of Left err -> parseError err
                   Right l -> cmd $ cacheLedger l (regexFor apats, regexFor dpats)

-- ghci helpers

-- | get a RawLedger from the file your LEDGER environment variable points to
-- or (WARNING) an empty one if there was a problem.
rawledger :: IO RawLedger
rawledger = do
  parsed <- ledgerFilePath [] >>= parseLedgerFile
  return $ either (\_ -> RawLedger [] [] [] "") id parsed

-- | as above, and convert it to a cached Ledger
ledger :: IO Ledger
ledger = do
  l <- rawledger
  return $ cacheLedger l nullpats

-- | get a Ledger from the given file path
rawledgerfromfile :: String -> IO RawLedger
rawledgerfromfile f = do
  parsed <- ledgerFilePath [File f] >>= parseLedgerFile
  return $ either (\_ -> RawLedger [] [] [] "") id parsed

-- | get a named account from your ledger file
accountnamed :: AccountName -> IO Account
accountnamed a = ledger >>= (return . fromMaybe nullacct . Map.lookup a . accounts)

