-- bench
-- By default, show approximate times for some standard hledger operations on a sample journal.
-- With --criterion, show accurate times (slow).
-- With --simplebench, show approximate times for the commands in default.bench, using the first hledger executable on $PATH.

import Control.Concurrent (threadDelay)
import Criterion.Main     (defaultMainWith, defaultConfig, bench, nfIO)
-- import Criterion.Types
import SimpleBench        (defaultMain)
import System.Directory   (getCurrentDirectory)
import System.Environment (getArgs, withArgs)
import System.Info        (os)
-- import System.IO          (hFlush, stdout)
import System.Process     (readProcess)
import System.TimeIt      (timeItT)
import Text.Printf
import Hledger.Cli

-- sample journal file to use for benchmarks
inputfile = "bench/10000x1000x10.journal"

outputfile = "/dev/null" -- hide output of benchmarked commands (XXX unixism)
-- outputfile = "-" -- show output of benchmarked commands

-- a delay to avoid truncation of final output by "stack bench"
-- https://github.com/commercialhaskell/stack/issues/413
stackFinalOutputDelaySeconds = 0 -- 10

main = do
 -- withArgs ["--simplebench"] $ do
 -- withArgs ["--criterion"] $ do
  args <- getArgs
  if "--criterion" `elem` args
    then withArgs [] benchWithCriterion
    else if "--simplebench" `elem` args
         then benchWithSimplebench
         else benchWithTimeit
  -- hFlush stdout
  threadDelay (stackFinalOutputDelaySeconds * 1000000)

benchWithTimeit = do
  getCurrentDirectory >>= printf "Benchmarking hledger in %s with timeit\n"
  let opts = defcliopts{output_file_=Just outputfile}
  (t0,j) <- timeit ("read "++inputfile) $ either error id <$> readJournalFile Nothing Nothing True inputfile
  (t1,_) <- timeit ("print") $ print' opts j
  (t2,_) <- timeit ("register") $ register opts j
  (t3,_) <- timeit ("balance") $ balance  opts j
  (t4,_) <- timeit ("stats") $ stats opts j
  printf "Total: %0.2fs\n" (sum [t0,t1,t2,t3,t4])

timeit :: String -> IO a -> IO (Double, a)
timeit name action = do
  printf "%s%s" name (replicate (40 - length name) ' ')
  (t,a) <- timeItT action
  printf "[%.2fs]\n" t
  return (t,a)

benchWithCriterion = do
  getCurrentDirectory >>= printf "Benchmarking hledger in %s with criterion\n"
  let opts = defcliopts{output_file_=Just "/dev/null"}
  j <- either error id <$> readJournalFile Nothing Nothing True inputfile
  Criterion.Main.defaultMainWith defaultConfig $ [
    bench ("read "++inputfile) $ nfIO $ (either error const <$> readJournalFile Nothing Nothing True inputfile),
    bench ("print")            $ nfIO $ print'   opts j,
    bench ("register")         $ nfIO $ register opts j,
    bench ("balance")          $ nfIO $ balance  opts j,
    bench ("stats")            $ nfIO $ stats    opts j
    ]

benchWithSimplebench = do
  let whichcmd = if os == "mingw32" then "where" else "which"
  exe <- init <$> readProcess whichcmd ["hledger"] ""
  pwd <- getCurrentDirectory
  printf "Benchmarking %s in %s with simplebench and shell\n" exe pwd
  flip withArgs SimpleBench.defaultMain [
     "-fbench/default.bench"
    ,"-v"
    ,"hledger"
    ] 
