#!/usr/bin/env runhaskell
{- 
bench.hs - simple benchmarking of command-line programs.
Requires html and tabular.
Simon Michael 2008-2013

Example:

$ simplebench.hs --help
...
$ cat - >bench.tests
-f sample.ledger -s balance
-f ~/.ledger -s balance
$ simplebench.hs -v hledger "ledger --no-cache" ledger
Using bench.tests
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
import Text.Html ((+++), renderHtml, stringToHtml)
import System.Exit
import Text.Printf
import Data.Time.Clock
import Data.Time.Format
import System.Locale
import Control.Monad
import Debug.Trace
import System.Console.GetOpt

usagehdr = "bench [-f testsfile] [-n iterations] [-p precision] executable1 [executable2 ...]\n" ++
           "\n" ++
           "Run some functional tests with each of the specified executables,\n" ++
           "where a test is \"zero or more arguments supported by all executables\",\n" ++
           "and report the best execution times.\n"
           
options = [
  Option "f" ["testsfile"] (ReqArg File "testsfile") "file containing tests, one per line, default: bench.tests"
 ,Option "n" ["iterations"] (ReqArg Num "iterations") "number of test iterations to run, default: 2"
 ,Option "p" ["precision"] (ReqArg Prec "precision") "show times with this precision, default: 2"
 ,Option "v" ["verbose"] (NoArg Verbose) "show intermediate results"
 ,Option "h" ["help"] (NoArg Help) "show this help"
 ]             

usageftr = "\n" ++
           "Tips:\n" ++
           "- executables may have arguments if enclosed in quotes\n" ++
           "- tests can be commented out with #\n" ++
           "- results are saved in benchresults.{html,txt}\n"

usage = usageInfo usagehdr options ++ usageftr

-- an option value
data Opt = File {value::String} 
         | Num  {value::String} 
         | Prec {value::String} 
-- I don't know how optValuesForConstructor etc. can have that 
-- type signature with these, but it works..
--       | Some Int
         | Verbose
         | Help
           deriving (Eq,Show)

-- option value getters.
fileopt :: [Opt] -> String
fileopt = optValueWithDefault File "bench.tests"

precisionopt :: [Opt] -> Int
precisionopt = read . optValueWithDefault Prec "2"

numopt :: [Opt] -> Int
numopt = read . optValueWithDefault Num "2"

verboseopt :: [Opt] -> Bool
verboseopt = (Verbose `elem`)

-- options utilities
parseargs :: [String] -> ([Opt],[String])
parseargs as =
  case (getOpt Permute options as) of
    (opts,args,[]) -> (opts,args)
    (_,_,errs)     -> error (concat errs ++ usage)

optValueWithDefault :: (String -> Opt) -> String -> [Opt] -> String
optValueWithDefault optcons def opts = 
    last $ def : optValuesForConstructor optcons opts

optValuesForConstructor :: (String -> Opt) -> [Opt] -> [String]
optValuesForConstructor optcons opts = concatMap get opts
    where get o = [v | optcons v == o] where v = value o

main = do
  args <- getArgs
  let (opts,exes) = parseargs args
  when (null exes) $ error $ "at least one executable needed\n" ++ usage
  let (file, num) = (fileopt opts, numopt opts)
  tests <- liftM (filter istest . lines) (readFile file)
  now <- getCurrentTime
  putStrLn $ printf "Using %s" file
  putStrLn $ printf "Running %d tests %d times with %d executables at %s:" 
               (length tests) num (length exes) (show now)
  let doexe t e = mapM (doiteration opts t e) [1..num]
  let dotest t = mapM (doexe t) exes
  hSetBuffering stdout NoBuffering
  results <- mapM dotest tests
  summarise opts tests exes results 

istest s = not (null s' || ("#" `isPrefixOf` s')) where s' = clean s
clean = unwords . words

doiteration :: [Opt] -> String -> String -> Int -> IO Float
doiteration opts test exe iteration = do
  let cmd = unwords [exe,clean test]
  when (verboseopt opts) $ putStr $ show iteration ++ ": " ++ cmd
  hFlush stdout
  t <- time cmd
  when (verboseopt opts) $ printf "\t[%ss]\n" (showtime opts t)
  return t

time :: String -> IO Float
time cmd = do
  t1 <- getCurrentTime
  ret <- system $ cmd ++ " >/dev/null 2>&1"
  case ret of
    ExitSuccess -> return ()
    ExitFailure f -> putStr $ printf " (error %d)" f
  t2 <- getCurrentTime
  return $ realToFrac $ diffUTCTime t2 t1

summarise :: [Opt] -> [String] -> [String] -> [[[Float]]] -> IO ()
summarise opts tests exes results = do
  putStrLn "\nSummary (best iteration):\n"
  let t = maketable opts tests exes results
  putStrLn $ TA.render id id id t
  let outname = "benchresults"
  writeFile (outname <.> "txt") $ TA.render id id id t
  writeFile (outname <.> "html") $ renderHtml $ TH.css TH.defaultCss +++ TH.render stringToHtml stringToHtml stringToHtml t

maketable :: [Opt] -> [String] -> [String] -> [[[Float]]] -> Table String String String
maketable opts rownames colnames results = Table rowhdrs colhdrs rows
 where
  rowhdrs = Group NoLine $ map Header $ padright rownames
  colhdrs = Group SingleLine $ map Header colnames
  rows = map (map (showtime opts . minimum)) results
  padright ss = map (printf (printf "%%-%ds" w)) ss
      where w = maximum $ map length ss

showtime :: [Opt] -> (Float -> String)
showtime opts = printf $ "%." ++ show (precisionopt opts) ++ "f"

strace a = trace (show a) a
