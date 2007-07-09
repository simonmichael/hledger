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
  let (acctpats, descpats) = parseLedgerPatternArgs args
  run cmd opts acctpats descpats
  where run cmd opts acctpats descpats
            | Help `elem` opts            = putStr usage
            | cmd `isPrefixOf` "test"     = test     opts acctpats descpats
            | cmd `isPrefixOf` "print"    = printcmd opts acctpats descpats
            | cmd `isPrefixOf` "register" = register opts acctpats descpats
            | cmd `isPrefixOf` "balance"  = balance  opts acctpats descpats
            | otherwise                   = putStr usage

doWithFilteredLedger :: [Flag] -> [String] -> [String] -> (Ledger -> IO ()) -> IO ()
doWithFilteredLedger opts acctpats descpats cmd = do
    ledgerFilePath opts >>= parseLedgerFile >>= doWithParsed acctpats descpats cmd

doWithParsed :: [String] -> [String] -> (Ledger -> IO ()) -> (Either ParseError LedgerFile) -> IO ()
doWithParsed acctpats descpats cmd parsed = do
  case parsed of Left e -> parseError e
                 Right l -> cmd $ cacheLedger acctpats descpats l 

type Command = [Flag] -> [String] -> [String] -> IO ()

test :: Command
test opts acctpats descpats = do 
  Tests.hunit
  Tests.quickcheck
  return ()

printcmd :: Command
printcmd opts acctpats descpats = do 
  doWithFilteredLedger opts acctpats descpats printentries
    where
      printentries l = putStr $ showEntries $ setprecision $ entries $ rawledger l
          where
            setprecision = map (entrySetPrecision (lprecision l))

register :: Command
register opts acctpats descpats = do 
  doWithFilteredLedger opts acctpats descpats printregister
    where 
      printregister l = 
          putStr $ showTransactionsWithBalances 
                     (sortBy (comparing date) $ ledgerTransactions l)
                     nullamt{precision=lprecision l}

balance :: Command
balance opts acctpats descpats = do
  doWithFilteredLedger opts acctpats descpats printbalance
    where
      printbalance l =
          putStr $ showLedgerAccounts l depth
              where 
                showsubs = (ShowSubs `elem` opts)
                depth = case (acctpats, showsubs) of
                          ([],False) -> 1
                          otherwise  -> 9999

{-
interactive testing:

*Main> p <- ledgerFilePath [File "./test.dat"] >>= parseLedgerFile
*Main> let r = either (\_ -> LedgerFile [] [] []) id p
*Main> let l = cacheLedger r
*Main> let ant = accountnametree l
*Main> let at = accounts l
*Main> putStr $ drawTree $ treemap show $ ant
*Main> putStr $ showLedgerAccounts l [] False 1
*Main> :m +Tests
*Main Tests> l7

-}
