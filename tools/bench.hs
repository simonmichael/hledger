#!/usr/bin/env runhaskell
{- 
bench.hs (see usage string below). 

For simple benchmarking. Similar to my darcs-benchmark/bench.hs script.
Example:

$ cat - >bench.tests
-f sample.ledger -s balance
-f ~/.ledger -s balance
$ bench.hs bench.tests 2 hledger "ledger --no-cache" ledger
Running 2 tests 2 times in . with 3 executables at 2008-11-26 18:52:15.776357 UTC:
1: hledger -f sample.ledger -s balance	[0.02s]
2: hledger -f sample.ledger -s balance	[0.01s]
1: ledger --no-cache -f sample.ledger -s balance	[0.02s]
2: ledger --no-cache -f sample.ledger -s balance	[0.02s]
1: ledger -f sample.ledger -s balance	[0.02s]
2: ledger -f sample.ledger -s balance	[0.02s]
1: hledger -f ~/.ledger -s balance	[3.56s]
2: hledger -f ~/.ledger -s balance	[3.56s]
1: ledger --no-cache -f ~/.ledger -s balance	[0.10s]
2: ledger --no-cache -f ~/.ledger -s balance	[0.10s]
1: ledger -f ~/.ledger -s balance	[0.10s]
2: ledger -f ~/.ledger -s balance	[0.10s]

Summary (best iteration):

                            || hledger | ledger --no-cache | ledger
============================++=========+===================+=======
-f sample.ledger -s balance ||    0.01 |              0.02 |   0.02
    -f ~/.ledger -s balance ||    3.56 |              0.10 |   0.10

-}

import Data.Char
import Data.List
import Data.Maybe
import Numeric
import System.Environment
import System.Directory
import System.FilePath
import System.Cmd
import System.IO
import Text.Tabular
import qualified Text.Tabular.AsciiArt as TA
import qualified Text.Tabular.Html     as TH
import Text.Html ((+++), renderHtml)
import System.Exit
import Text.Printf
import Data.Time.Clock
import Data.Time.Format
import System.Locale
import Control.Monad
import Debug.Trace



usage = "bench.hs <testsfile> <num> [<executable> ...]\n" ++
        "\n" ++
        "Run some functional tests, defined as lines of arguments in\n" ++
        "testsfile, num times with each of the specified executables,\n" ++
        "printing the execution times and a summary.\n" ++
        "Tips:\n" ++
        "- comment out tests with #\n"

precision = 2

main = do
  (testsfile,iterations,dir,exes) <- getArgs >>= return . parseargs
  tests <- readFile testsfile >>= return . testlines
  now <- getCurrentTime
  putStrLn $ printf "Running %d tests %d times in %s with %d executables at %s:\n" 
               (length tests) (iterations) dir (length exes) (show now)
  let doexe t e = sequence $ map (doiteration t e dir) [1..iterations]
  let dotest t = sequence $ map (doexe t) exes
  hSetBuffering stdout NoBuffering
  results <- mapM dotest tests
  summarise tests exes results 
    where 
--       parseargs (t:n:d:[]) = parseargs (t:n:d:["darcs"])
      parseargs (t:n:es) = (t,read n,".",es)
      parseargs _ = error $ "\n" ++ usage
      testlines s = filter istest $ map clean $ lines s
      istest s = not (null s || ("#" `isPrefixOf` s))
      clean = unwords . words

doiteration :: String -> String -> String -> Int -> IO Float
doiteration test exe dir iteration = do
  let cmd = unwords [exe,test]
  putStr $ show iteration ++ ": " ++ cmd
  hFlush stdout
  t <- time cmd
  printf "\t[%ss]\n" (showtime t)
  return t

time :: String -> IO Float
time cmd = do
  t1 <- getCurrentTime
  ret <- system $ cmd ++ ">/dev/null 2>&1"
  case ret of
    ExitSuccess -> return ()
    ExitFailure f -> putStr $ printf " (error %d)" f
  t2 <- getCurrentTime
  return $ realToFrac $ diffUTCTime t2 t1

summarise tests exes results = do
  -- putStrLn ""; print results
  putStrLn "\nSummary (best iteration):\n"
  let t = maketable tests exes results
  putStrLn $ TA.render id t
  -- putStrLn $ "See " ++ prefix ++ "summary.*"
  let outname = "summary"
  writeFile (outname <.> "txt") $ TA.render id t
  writeFile (outname <.> "html") $ renderHtml $ TH.css TH.defaultCss +++ TH.render id t

maketable :: [String] -> [String] -> [[[Float]]] -> Table String
maketable rownames colnames results = Table rowhdrs colhdrs rows
 where
  rowhdrs = Group NoLine $ map Header rownames
  colhdrs = Group SingleLine $ map Header colnames
  rows = map (map (showtime . minimum)) results

showtime = printf $ "%."++(show precision)++"f"

strace a = trace (show a) a
