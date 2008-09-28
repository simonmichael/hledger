#!/usr/bin/env runhaskell
{-
hledger - ledger-compatible money management tool (& haskell study)
GPLv3, (c) Simon Michael & contributors
A port of John Wiegley's ledger at http://newartisans.com/ledger.html

See Types.hs for a code overview.
-}

module Main
where
import System
import Text.ParserCombinators.Parsec (ParseError)
import qualified Data.Map as Map (lookup)

import Options
import Models
import Parse
import Tests
import Utils hiding (test)


main :: IO ()
main = do
  (opts, (cmd:args)) <- getArgs >>= parseOptions
  let pats = parsePatternArgs args
  run cmd opts pats
  where run cmd opts pats
            | Help `elem` opts            = putStr usage
            | cmd `isPrefixOf` "test"     = test     opts pats
            | cmd `isPrefixOf` "print"    = doWithFilteredLedger opts pats printentries
            | cmd `isPrefixOf` "register" = doWithFilteredLedger opts pats printregister
            | cmd `isPrefixOf` "balance"  = balance  opts pats
            | otherwise                   = putStr usage

doWithFilteredLedger :: [Flag] -> FilterPatterns -> (Ledger -> IO ()) -> IO ()
doWithFilteredLedger opts pats cmd = do
    ledgerFilePath opts >>= parseLedgerFile >>= doWithParsed pats cmd

doWithParsed :: FilterPatterns -> (Ledger -> IO ()) -> (Either ParseError LedgerFile) -> IO ()
doWithParsed pats cmd parsed = do
  case parsed of Left e -> parseError e
                 Right l -> cmd $ cacheLedger pats l 

type Command = [Flag] -> FilterPatterns -> IO ()

test :: Command
test opts pats = do 
  Tests.hunit
  Tests.quickcheck
  return ()

printentries l = putStr $ showEntries $ setprecisions $ entries $ rawledger l
    where setprecisions = map (entrySetPrecision (lprecision l))
      
printregister l = putStr $ showTransactionsWithBalances 
                  (sortBy (comparing date) $ ledgerTransactions l)
                  nullamt{precision=lprecision l}

balance :: Command
balance opts pats = do
  doWithFilteredLedger opts pats printbalance
    where
      printbalance l =
          putStr $ showLedgerAccounts l depth
              where 
                showsubs = (ShowSubs `elem` opts)
                depth = case (pats, showsubs) of
                          ((Nothing,_), False) -> 1
                          otherwise  -> 9999

-- helpers for interacting in ghci

-- returns a Ledger parsed from the file your LEDGER environment variable
-- points to or (WARNING:) an empty one if there was a problem.
myledger :: IO Ledger
myledger = do
  parsed <- ledgerFilePath [] >>= parseLedgerFile
  let ledgerfile = either (\_ -> LedgerFile [] [] [] "") id parsed
  return $ cacheLedger (argpats [] []) ledgerfile

-- similar, but accepts a file path
ledgerfromfile :: String -> IO Ledger
ledgerfromfile f = do
  parsed <- ledgerFilePath [File f] >>= parseLedgerFile
  let ledgerfile = either (\_ -> LedgerFile [] [] [] "") id parsed
  return $ cacheLedger (argpats [] []) ledgerfile

accountnamed :: AccountName -> IO Account
accountnamed a = myledger >>= (return . fromMaybe nullacct . Map.lookup a . accounts)
  

--clearedBalanceToDate :: String -> Amount

{-
ghci examples:

$ ghci hledger.hs
GHCi, version 6.8.2: http://www.haskell.org/ghc/  :? for help
Loading package base ... linking ... done.
Ok, modules loaded: Utils, Main, Tests, Parse, Models, Ledger, LedgerFile, LedgerEntry, Amount, Currency, Types, LedgerTransaction, AccountName, Transaction, Account, TimeLog, Options.
Prelude Main> l <- myledger
<..snip..>
Ledger with 628 entries, 128 accounts
Prelude Main> 

$ ghci hledger.hs
> l <- myledger
> putStr $ drawTree $ treemap show $ accountnametree l
> putStr $ showLedgerAccounts l 1
> printregister l
> import Types
> accounts l
> accountnamed "assets"


-}
