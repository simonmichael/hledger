#!/usr/bin/runhaskell
-- hledger - ledger-compatible money management utilities (& haskell study)
-- GPLv3, (c) Simon Michael & contributors, 
-- John Wiegley's ledger is at http://newartisans.com/ledger.html .

module Main where

import System (getArgs)
import Data.List (isPrefixOf)
import Test.HUnit (runTestTT)
import Text.ParserCombinators.Parsec (parseFromFile, ParseError)

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

test :: IO ()      
test = do
  runTestTT hunittests
--  quickCheck prop1
  return ()

register :: [String] -> IO ()
register args = do 
  p <- parseLedgerFile ledgerFilePath
  case p of Left e -> parseError e
            Right l -> printRegister l

balance :: [String] -> IO ()
balance args = do 
  p <- parseLedgerFile ledgerFilePath
  case p of Left e -> parseError e
            Right l -> printBalances l

-- utils

parseLedgerFile :: IO String -> IO (Either ParseError Ledger)
parseLedgerFile f = f >>= parseFromFile ledger

printRegister :: Ledger -> IO ()
printRegister l = putStr $ showRegisterEntries (entries l) 0

printBalances :: Ledger -> IO ()
printBalances l = putStr $ showRegisterEntries (entries l) 0

