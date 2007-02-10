#!/usr/bin/env runhaskell
-- hledger - ledger-compatible money management utilities (& haskell study)
-- GPLv3, (c) Simon Michael & contributors, 
-- John Wiegley's ledger is at http://newartisans.com/ledger.html .

module Main -- almost all IO is handled here
where

import System
import Data.List
import Test.HUnit (runTestTT)
import Test.QuickCheck (quickCheck)
import Text.ParserCombinators.Parsec (parseFromFile, ParseError)

import Options
import Models
import Parse
import Tests

main :: IO ()
main = do
  (opts, args) <- (getArgs >>= getOptions)
  if args == []
    then register []
    else
      let (command, args') = (head args, tail args) in
      if "reg" `isPrefixOf` command then (register args')
      else if "bal" `isPrefixOf` command then balance args'
           else if "test" `isPrefixOf` command then test
                else error "could not recognise your command"

-- commands

test :: IO ()      
test = do
  hcounts <- runTestTT tests
  qcounts <- mapM quickCheck props
  --print $ "hunit: " ++ (showHunitCounts hcounts)
  --print $ "quickcheck: " ++ (concat $ intersperse " " $ map show qcounts)
  return ()
    where showHunitCounts c =
              reverse $ tail $ reverse ("passed " ++ (unwords $ drop 5 $ words (show c)))

register :: [String] -> IO ()
register args = do 
  getLedgerFilePath >>= parseLedgerFile >>= doWithParsed (printRegister args)

balance :: [String] -> IO ()
balance args = 
    return ()

-- utils

-- doWithLedgerFile =
--     getLedgerFilePath >>= parseLedgerFile >>= doWithParsed

doWithParsed :: (a -> IO ()) -> (Either ParseError a) -> IO ()
doWithParsed a p = 
  case p of Left e -> parseError e
            Right v -> a v

printRegister :: [String] -> Ledger -> IO ()
printRegister args ledger =
    putStr $ showEntriesWithBalances (entriesMatching (head (args ++ [""])) ledger) 0
