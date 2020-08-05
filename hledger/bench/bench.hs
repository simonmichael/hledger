-- bench
-- By default, show approximate times for some standard hledger operations on a sample journal.
-- With --criterion, show accurate times (slow).

import Criterion.Main     (defaultMainWith, defaultConfig, bench, nfIO)
-- import QuickBench        (defaultMain)
import Data.Default
import System.Directory   (getCurrentDirectory)
import System.Environment (getArgs, withArgs)
import System.TimeIt      (timeItT)
import Text.Printf
import Hledger.Cli

-- sample journal file to use for benchmarks
inputfile = "bench/10000x1000x10.journal"

outputfile = "/dev/null" -- hide output of benchmarked commands (XXX unixism)
-- outputfile = "-" -- show output of benchmarked commands

main = do
 -- withArgs ["--quickbench"] $ do
 -- withArgs ["--criterion"] $ do
  args <- getArgs
  if "--criterion" `elem` args
  then
    withArgs [] benchWithCriterion
  else
--     if "--quickbench" `elem` args
--     then
--       benchWithQuickbench
--     else
      benchWithTimeit

benchWithTimeit = do
  getCurrentDirectory >>= printf "Benchmarking hledger in %s with timeit\n"
  let opts = defcliopts{output_file_=Just outputfile}
  (t0,j) <- timeit ("read "++inputfile) $ either error id <$> readJournalFile def inputfile  -- PARTIAL:
  (t1,_) <- timeit ("print") $ print' opts j
  (t2,_) <- timeit ("register") $ register opts j
  (t3,_) <- timeit ("balance") $ balance  opts j
  printf "Total: %0.2fs\n" (sum [t0,t1,t2,t3])

timeit :: String -> IO a -> IO (Double, a)
timeit name action = do
  printf "%s%s" name (replicate (40 - length name) ' ')
  (t,a) <- timeItT action
  printf "[%.2fs]\n" t
  return (t,a)

benchWithCriterion = do
  getCurrentDirectory >>= printf "Benchmarking hledger in %s with criterion\n"
  let opts = defcliopts{output_file_=Just "/dev/null"}
  j <- either error id <$> readJournalFile def inputfile  -- PARTIAL:
  Criterion.Main.defaultMainWith defaultConfig $ [
    bench ("read "++inputfile) $ nfIO $ (either error const <$> readJournalFile def inputfile),  -- PARTIAL:
    bench ("print")            $ nfIO $ print'   opts j,
    bench ("register")         $ nfIO $ register opts j,
    bench ("balance")          $ nfIO $ balance  opts j,
    bench ("stats")            $ nfIO $ stats    opts j
    ]
