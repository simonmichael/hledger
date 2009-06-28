#!/usr/bin/env runhaskell
{-

shelltest.hs (c) 2009 Simon Michael <simon@joyful.com>

Run one or more hledger command-line tests, specified by .test files like
those used in the ledger project. A ledger-style .test file contains a
partial command line, input, expected output, expected error output, and
expected exit code separated by delimiters.

Usage: $ shelltest [OPTS] *.test

This version uses the test-framework test runner. Any command-line arguments
beginning with - are passed through to that. So avoid spaces: use -tpattern
not -t pattern. To get a speedup, try adding -j8.

Here is the .test file format:
@
--option1 arg1 arg2
<<<
lines of
input
>>>
expected
output
>>>2
expected
error output
===0
;
; Lines whose first non-whitespace character is ; are ignored.
; The first line is the command line. "hledger" is prepended, and "-f-" is
; appended unless there is a -f or <... argument, in which case any
; provided input is ignored.
; Then there is a line containing <<< and 0 or more lines of input.
; Then a line containing >>> (or >>>1 for ledger testrunner compatibility)
; and 0 or more lines of expected output.
; Then a line containing >>>2 and 0 or more lines of expected stderr output.
; Then === and the expected exit code (on the same line).
; All fields except for the command line are optional, when omitted they
; are assumed to be "", "", "", and 0 respectively.
@
-}

module Main where
--import System (getArgs)
import System.Environment (getArgs,withArgs)
import System.Exit (exitFailure, exitWith, ExitCode(..))
import System.IO (hGetContents, hPutStr, stderr {-, stdout, hFlush-})
import System.Process (runInteractiveCommand, waitForProcess)
import Text.Printf (printf)
import Text.ParserCombinators.Parsec
import Control.Monad (liftM,when)
import Data.Maybe (fromMaybe)
import Data.List (partition)

import Test.Framework (defaultMain {-, testGroup-})
import Test.Framework.Providers.HUnit (hUnitTestToTests)
--import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.HUnit hiding (Test)
import qualified Test.HUnit (Test)
--import Test.QuickCheck
--import Tests (tests)

import Debug.Trace
strace :: Show a => a -> a
strace a = trace (show a) a


exe :: String
exe = "hledger"

data ShellTest = ShellTest {
     filename         :: String
    ,command          :: String
    ,stdin            :: Maybe String
    ,stdoutExpected   :: Maybe String
    ,stderrExpected   :: Maybe String
    ,exitCodeExpected :: Maybe ExitCode
    } deriving (Show)

main :: IO ()
main = do
  args <- getArgs
  let (opts,files) = partition ((=="-").take 1) args
  shelltests <-  mapM parseShellTest files
  withArgs opts $ defaultMain $ concatMap (hUnitTestToTests.shellTestToHUnitTest) shelltests

shellTestToHUnitTest :: ShellTest -> Test.HUnit.Test
shellTestToHUnitTest t = filename t ~: do {r <- runShellTest t; assertBool "" r}

parseShellTest :: FilePath -> IO ShellTest
parseShellTest = liftM (either (error.show) id) . parseFromFile shelltest

shelltest :: Parser ShellTest
shelltest = do
  st <- getParserState
  let f = sourceName $ statePos st
  c <- commandline
  i <- optionMaybe input
  o <- optionMaybe expectedoutput
  e <- optionMaybe expectederror
  x <- optionMaybe expectedexitcode
  return ShellTest{filename=f,command=c,stdin=i,stdoutExpected=o,stderrExpected=e,exitCodeExpected=x}

commandline,input,expectedoutput,expectederror,delimiter,line :: Parser String
commandline = line
input = string "<<<\n" >> (liftM unlines) (line `manyTill` (lookAhead delimiter))
expectedoutput = try $ string ">>>" >> optional (char '1') >> char '\n' >> (liftM unlines) (line `manyTill` (lookAhead delimiter))
expectederror = string ">>>2" >> (liftM $ unlines.tail) (line `manyTill` (lookAhead delimiter)) -- why tail ?
delimiter = choice [try $ string "<<<", try $ string ">>>", try $ string "===", (eof >> return "")]
line =  do
  l <- anyChar `manyTill` newline
  if take 1 (strip l) == ";" then line else return l
expectedexitcode :: Parser ExitCode
expectedexitcode = string "===" >> liftM (toExitCode.read) line -- `catch` (\e -> fail (show e))

runShellTest :: ShellTest -> IO Bool
runShellTest ShellTest{
    filename=_,command=c,stdin=i,stdoutExpected=o,stderrExpected=e,exitCodeExpected=x} = do
  let cmd = unwords [exe,c,if (any isinputarg $ words c) then "" else "-f-"]
            where isinputarg a = take 2 a == "-f" || (take 1 a == "<")
      (i',o',e',x') = (fromMaybe "" i, fromMaybe "" o, fromMaybe "" e, fromMaybe ExitSuccess x)
  -- printf "%s .. " f; hFlush stdout
  (ih,oh,eh,ph) <- runInteractiveCommand cmd
  hPutStr ih i'
  out <- hGetContents oh
  err <- hGetContents eh
  exit <- waitForProcess ph
  let (outputok, errorok, exitok) = (out==o', err==e', exit==x')
  if outputok && errorok && exitok 
   then do
     -- putStrLn "ok"
     return True 
   else do
     -- hPutStr stderr $ printf "FAIL\n"
     when (not outputok) $ printExpectedActual "stdout" o' out
     when (not errorok)  $ printExpectedActual "stderr" e' err
     when (not exitok)   $ printExpectedActual "exit code" (show (fromExitCode x')++"\n") (show (fromExitCode exit)++"\n")
     return False

printExpectedActual :: String -> String -> String -> IO ()
printExpectedActual f e a = hPutStr stderr $ printf "**Expected %s:\n%s**Got:\n%s" f e a

toExitCode :: Int -> ExitCode
toExitCode 0 = ExitSuccess
toExitCode n = ExitFailure n

fromExitCode :: ExitCode -> Int
fromExitCode ExitSuccess     = 0
fromExitCode (ExitFailure n) = n

strip,lstrip,rstrip,dropws :: String -> String
strip = lstrip . rstrip
lstrip = dropws
rstrip = reverse . dropws . reverse
dropws = dropWhile (`elem` " \t")
