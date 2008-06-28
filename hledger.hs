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

{-
interactive testing in ghci:

p <- ledgerFilePath [File "./test.ledger"] >>= parseLedgerFile
let r = either (\_ -> LedgerFile [] [] [] "") id p
let l = cacheLedger (argpats [] []) r
let ant = accountnametree l
let at = accounts l
putStr $ drawTree $ treemap show $ ant
putStr $ showLedgerAccounts l 1
:m +Tests
l7

-}
