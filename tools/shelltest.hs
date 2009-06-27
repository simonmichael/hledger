#!/usr/bin/env runhaskell
{-

Run one or more hledger command-line tests, specified by .test files like
those used in the ledger project. A ledger-style .test file contains a
partial command line, input, expected output, expected error output, and
expected exit code separated by delimiters.

Usage: $ shelltest *.test

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
; appended unless there is a -f or <... argument (in which case the
; provided input is ignored.)
; Then there is a line containing <<< and 0 or more lines of input.
; Then a line containing >>> (or >>>1 for ledger testrunner compatibility)
; and 0 or more lines of expected output.
; Then a line containing >>>2 and 0 or more lines of expected stderr output.
; Then === and the expected exit code (on the same line).
@
-}

module Main where
import System (getArgs)
import System.Exit (exitFailure, exitWith, ExitCode(..))
import System.IO (hGetContents, hPutStr, hFlush, stderr, stdout)
import System.Process (runInteractiveCommand, waitForProcess)
import Text.Printf (printf)
import Text.ParserCombinators.Parsec
import Control.Monad (liftM,when)

exe :: String
exe = "hledger"

data ShellTest = ShellTest {
     command          :: String
    ,stdin            :: String
    ,stdoutExpected   :: String
    ,stderrExpected   :: String
    ,exitCodeExpected :: ExitCode
    } deriving (Show)

main :: IO ()
main = do
  files <- getArgs
  ok <-  mapM (\f -> parseShellTest f >>= runShellTest) files
  if any not ok then exitFailure else exitWith ExitSuccess

parseShellTest :: FilePath -> IO ShellTest
parseShellTest = liftM (either (error.show) id) . parseFromFile shelltest

shelltest :: Parser ShellTest
shelltest = do
  c <- line; string "<<<\n"
  i <- line `manyTill` (string ">>>" >> optional (char '1') >> char '\n')
  o <- line `manyTill` (string ">>>2\n")
  e <- line `manyTill` (string "===")
  x <- line
  let x' = read x -- `catch` (\e -> fail (show e))
  eof
  return ShellTest{command=c,stdin=unlines i,stdoutExpected=unlines o,stderrExpected=unlines e,exitCodeExpected=toExitCode x'}

line :: Parser String
line = do
  l <- anyChar `manyTill` newline
  if take 1 (strip l) == ";"
   then line
   else return l

runShellTest :: ShellTest -> IO Bool
runShellTest ShellTest{
    command=c,stdin=i,stdoutExpected=o,stderrExpected=e,exitCodeExpected=x} = do
  let cmd = unwords [exe,c,if (any isinputarg $ words c) then "" else "-f-"]
            where isinputarg a = take 2 a == "-f" || (take 1 a == "<")
  printf "Testing: %s" cmd; hFlush stdout
  (ih,oh,eh,ph) <- runInteractiveCommand cmd
  hPutStr ih i
  out <- hGetContents oh
  err <- hGetContents eh
  exit <- waitForProcess ph
  let (outputok, errorok, exitok) = (out==o, err==e, exit==x)
  if outputok && errorok && exitok 
   then do
     putStrLn " .. ok"
     return True 
   else do
     hPutStr stderr $ printf " .. FAIL\n"
     when (not outputok) $ printExpectedActual "stdout" o out
     when (not errorok)  $ printExpectedActual "stderr" e err
     when (not exitok)   $ printExpectedActual "exit code" (show (fromExitCode x)++"\n") (show (fromExitCode exit)++"\n")
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
