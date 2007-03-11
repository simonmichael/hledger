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
    Account
     Ledger
      EntryTransaction
       Entry
        Transaction
         AccountName
         BasicTypes
          Utils

-}

-- application logic & most IO
module Main
where
import System
import System.Environment (withArgs) -- for testing in old hugs
import Test.HUnit (runTestTT)
import Test.QuickCheck (quickCheck)
import Text.ParserCombinators.Parsec (ParseError)
import Debug.Trace

import Options
import Models
import Parse
import Tests
import Utils

main :: IO ()
main = do
  (opts, args) <- (getArgs >>= getOptions)
  if args == []
    then register [] []
    else
      let (command, args') = (head args, tail args) in
      if "reg" `isPrefixOf` command then (register opts args')
      else if "bal" `isPrefixOf` command then balance opts args'
           else if "test" `isPrefixOf` command then test
                else putStr $ usageInfo usageHeader options

-- commands

test :: IO ()      
test = do
  hcounts <- runTestTT tests
  qcounts <- mapM quickCheck props
  return ()
    where showHunitCounts c =
              reverse $ tail $ reverse ("passed " ++ (unwords $ drop 5 $ words (show c)))

register :: [Flag] -> [String] -> IO ()
register opts args = do 
  getLedgerFilePath opts >>= parseLedgerFile >>= doWithParsed (printRegister opts args)

balance :: [Flag] -> [String] -> IO ()
balance opts args = do
  getLedgerFilePath opts >>= parseLedgerFile >>= doWithParsed (printBalance opts args)

-- utils

-- doWithLedgerFile =
--     getLedgerFilePath >>= parseLedgerFile >>= doWithParsed

doWithParsed :: Show a => (a -> IO ()) -> (Either ParseError a) -> IO ()
doWithParsed a p = do
  case p of Left e -> parseError e
            Right v -> a v

printRegister :: [Flag] -> [String] -> Ledger -> IO ()
printRegister opts args ledger = do
  putStr $ showTransactionsWithBalances 
             (ledgerTransactionsMatching (acctpats,descpats) ledger)
             0
      where (acctpats,descpats) = ledgerPatternArgs args

printBalance :: [Flag] -> [String] -> Ledger -> IO ()
printBalance opts args ledger = do
  putStr $ showLedgerAccounts ledger acctpats showsubs maxdepth
    where 
      (acctpats,_) = ledgerPatternArgs args
      showsubs = (ShowSubs `elem` opts)
      maxdepth = case (acctpats, showsubs) of
                   ([],False) -> 1
                   otherwise  -> 9999


-- interactive testing:
--
-- p <- getLedgerFilePath [] >>= parseLedgerFile
-- let l = either (\_ -> Ledger [] [] []) id p
-- let ant = ledgerAccountNameTree l
-- let at = ledgerAccountTreeMatching l [] True 999
-- putStr $ drawTree $ treemap show $ ledgerAccountTreeMatching l ["a"] False 999
