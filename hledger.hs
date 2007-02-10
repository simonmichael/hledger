#!/usr/bin/runhaskell
-- hledger - ledger-compatible money management utilities (& haskell workout)
-- GPLv3, (c) Simon Michael & contributors, 
-- John Wiegley's ledger is at http://newartisans.com/ledger.html .

import System (getArgs)
import Data.List (isPrefixOf)

import Options
import Types
import Parse
import Tests

main :: IO ()
main = do
  (opts, args) <- (getArgs >>= getOptions)
  test
  if args == []
    then register []
    else
      let (command, args) = (head args, tail args) in
      if "reg" `isPrefixOf` command then register args
      else if "bal" `isPrefixOf` command then balance args
           else error "could not recognise your command"

-- commands

register :: [String] -> IO ()
register args = do 
  p <- parseLedgerFile ledgerFilePath
  case p of
    Left e -> do putStr "ledger parse error at "; print e
    Right l  -> printRegister l

balance :: [String] -> IO ()
balance args = do 
  p <- parseLedgerFile ledgerFilePath
  case p of
    Left e -> do putStr "ledger parse error at "; print e
    Right l  -> printRegister l

