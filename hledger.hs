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
import Utils


main :: IO ()
main = do
  (opts, (cmd:args)) <- getArgs >>= parseOptions
  let (acctpats, descpats) = parseLedgerPatternArgs args
  run cmd opts acctpats descpats
  where run cmd opts acctpats descpats
            | Help `elem` opts            = putStr usage
            | cmd `isPrefixOf` "register" = register opts acctpats descpats
            | cmd `isPrefixOf` "balance"  = balance opts acctpats descpats
            | cmd `isPrefixOf` "test"     = selftest
            | otherwise                   = putStr usage

-- commands

register :: [Flag] -> [String] -> [String] -> IO ()
register opts acctpats descpats = do 
  doWithLedger opts printRegister
    where 
      printRegister l = 
          putStr $ showTransactionsWithBalances 
                     (ledgerTransactionsMatching (acctpats,descpats) l)
                     0

balance :: [Flag] -> [String] -> [String] -> IO ()
balance opts acctpats _ = do 
  doWithLedger opts printBalance
    where
      printBalance l =
          putStr $ showLedgerAccounts l acctpats showsubs maxdepth
              where 
                showsubs = (ShowSubs `elem` opts)
                maxdepth = case (acctpats, showsubs) of
                             ([],False) -> 1
                             otherwise  -> 9999

selftest :: IO ()
selftest = do
  Tests.tests
  Tests.props
  -- Amount.tests
  return ()

-- utils

doWithLedger :: [Flag] -> (Ledger -> IO ()) -> IO ()
doWithLedger opts cmd = do
    ledgerFilePath opts >>= (trace "parsing" $ parseLedgerFile) >>= doWithParsed cmd

doWithParsed :: (Ledger -> IO ()) -> (Either ParseError RawLedger) -> IO ()
doWithParsed cmd parsed = do
  case parsed of Left e -> parseError e
                 Right l -> cmd $ cacheLedger l

-- interactive testing:
--
-- p <- ledgerFilePath [] >>= parseLedgerFile
-- let l = either (\_ -> RawLedger [] [] []) id p
-- let ant = rawLedgerAccountNameTree l
-- let at = rawLedgerAccountTreeMatching l [] True 999
-- putStr $ drawTree $ treemap show $ rawLedgerAccountTreeMatching l ["a"] False 999
