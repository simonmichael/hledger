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
            | cmd `isPrefixOf` "register" = register opts acctpats descpats
            | cmd `isPrefixOf` "balance"  = balance opts acctpats descpats
            | cmd `isPrefixOf` "print"    = printcmd opts
            | cmd `isPrefixOf` "test"     = test
            | otherwise                   = putStr usage

-- commands

test :: IO ()
test = do
  Tests.hunit
  Tests.quickcheck
  return ()

register :: [Flag] -> [String] -> [String] -> IO ()
register opts acctpats descpats = do 
  doWithLedger opts printregister
    where 
      printregister l = 
          putStr $ showTransactionsWithBalances 
                     (sortBy (comparing date) (ledgerTransactionsMatching (acctpats,descpats) l))
                     nullamt{precision=lprecision l}

printcmd :: [Flag] -> IO ()
printcmd opts = do 
  doWithLedger opts printentries
    where
      printentries l = putStr $ showEntries $ setprecision $ entries $ rawledger l
          where
            setprecision = map (entrySetPrecision (lprecision l))

balance :: [Flag] -> [String] -> [String] -> IO ()
balance opts acctpats _ = do 
  doWithLedger opts printbalance
    where
      printbalance l =
          putStr $ showLedgerAccounts l acctpats showsubs maxdepth
              where 
                showsubs = (ShowSubs `elem` opts)
                maxdepth = case (acctpats, showsubs) of
                             ([],False) -> 1
                             otherwise  -> 9999

-- utils

doWithLedger :: [Flag] -> (Ledger -> IO ()) -> IO ()
doWithLedger opts cmd = do
    ledgerFilePath opts >>= parseLedgerFile >>= doWithParsed cmd

doWithParsed :: (Ledger -> IO ()) -> (Either ParseError RawLedger) -> IO ()
doWithParsed cmd parsed = do
  case parsed of Left e -> parseError e
                 Right l -> cmd $ cacheLedger l


{-
interactive testing:

*Main> p <- ledgerFilePath [File "./test.dat"] >>= parseLedgerFile
*Main> let r = either (\_ -> RawLedger [] [] []) id p
*Main> let l = cacheLedger r
*Main> let ant = accountnametree l
*Main> let at = accounts l
*Main> putStr $ drawTree $ treemap show $ ant
*Main> putStr $ showLedgerAccounts l [] False 1
*Main> :m +Tests
*Main Tests> l7

-}
