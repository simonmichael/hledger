{-
hledger - ledger-compatible money management tool (& haskell study)
GPLv3, (c) Simon Michael & contributors
inspired by John Wiegley's ledger at http://newartisans.com/ledger.html

modules/models are organized roughly like this; each layer can only
reference things below it:

hledger
 Options
 Tests
  Parse
   Models
    TimeLog
     TimeLogEntry
    Account
     Ledger
      EntryTransaction
       Entry
        Transaction
         AccountName
         Amount
         BasicTypes
          Utils

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
            | cmd `isPrefixOf` "register" = register opts acctpats descpats
            | cmd `isPrefixOf` "balance"  = balance opts acctpats descpats
            | cmd `isPrefixOf` "test"     = selftest
            | otherwise                   = putStr usage

-- commands

register :: [Flag] -> [String] -> [String] -> IO ()
register opts acctpats descpats = do 
  doWithLedger opts printRegister
    where 
      printRegister ledger = 
          putStr $ showTransactionsWithBalances 
                     (ledgerTransactionsMatching (acctpats,descpats) ledger)
                     0

balance :: [Flag] -> [String] -> [String] -> IO ()
balance opts acctpats _ = do 
  doWithLedger opts printBalance
    where
      printBalance ledger =
          putStr $ showLedgerAccounts ledger acctpats showsubs maxdepth
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
    ledgerFilePath opts >>= parseLedgerFile >>= doWithParsed cmd

doWithParsed :: Show a => (a -> IO ()) -> (Either ParseError a) -> IO ()
doWithParsed action parsed = do
  case parsed of Left e -> parseError e
                 Right l -> action l

-- interactive testing:
--
-- p <- ledgerFilePath [] >>= parseLedgerFile
-- let l = either (\_ -> Ledger [] [] []) id p
-- let ant = ledgerAccountNameTree l
-- let at = ledgerAccountTreeMatching l [] True 999
-- putStr $ drawTree $ treemap show $ ledgerAccountTreeMatching l ["a"] False 999
