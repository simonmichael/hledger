#!/usr/bin/runhaskell
-- hledger - ledger-compatible money management utilities (& haskell workout)
-- GPLv3, (c) Simon Michael & contributors, 
-- John Wiegley's ledger is at http://newartisans.com/ledger.html .

import System (getArgs)

import Options
import Types
import Parse
import Tests

-- commands

register :: IO ()
register = do 
  p <- parseLedgerFile ledgerFile
  case p of
    Left e -> do putStr "ledger parse error at "; print e
    Right l  -> putStr $ showLedger l

main :: IO ()
main = do
  (opts, args) <- getArgs >>= getOptions
  --putStr "options: "; print opts
  --putStr "arguments: "; print args
  if "reg" `elem` args
    then register
    else if "test" `elem` args 
         then test
         else return ()
