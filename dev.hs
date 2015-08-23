-- dev.hs, for miscellaneous profiling/benchmarking/testing.

import Hledger.Utils.Debug
-- import System.Environment (getArgs)
import Control.Monad.Except
import Criterion.Main
-- import Data.Text.Lazy as LT
-- import System.Environment
-- import Hledger
-- import qualified Hledger.Read.JournalReader as JR
import qualified Hledger.Read.TimelogReader as TR
-- import qualified Hledger.Read.TimelogReaderNoJU as TRNOJU
-- import qualified Hledger.Read.TimelogReaderPP as TRPP

inputjournal = "data/10000x1000x10.journal"
inputtimelog = "data/sample.timelog"

main = do
  --  

  -- -- read the input journal
  -- j <- either error id <$> readJournalFile Nothing Nothing True inputfile
  -- -- sanity check we parsed it all
  -- putStrLn $ show $ length $ jtxns j

  -- benchmark timelog parsing
  -- s <- readFile inputtimelog
  -- putStrLn $ show $ length s
  -- let s = unlines [
  --       "i 2009/03/27 09:00:00 projects:a",
  --       "o 2009/03/27 17:00:34",
  --       "i 2009/03/31 22:21:45 personal:reading:online",
  --       "o 2009/04/01 02:00:34",
  --       "i 2009/04/02 09:00:00 projects:b",
  --       "o 2009/04/02 17:00:34"
  --      ]
  -- -- let output = return . const -- putStrLn.show

  -- -- withArgs ["-l"] $ defaultMain [bench "timelog polyparse" $ nfIO $ runExceptT $ TRPP.parseJournalWith' TRPP.timelogFile False "" s]
  -- defaultMain [
  --   -- bench ("read "++inputtimelog++" with parsec") $ nfIO $ runExceptT (TR.parse Nothing False "" s) >>= output
  --   -- bench ("read "++inputtimelog++" with parsec, no ju") $ nfIO $ runExceptT (TRNOJU.parse Nothing False "" s) >>= output,
  --   -- bench ("read "++inputtimelog++" polyparse")   $ nfIO $ runExceptT (TRPP.parse Nothing False "" s) >>= output
  --   ]

  -- return ()

-- benchWithTimeit = do
--   getCurrentDirectory >>= printf "Benchmarking hledger in %s with timeit\n"
--   let opts = defcliopts{output_file_=Just outputfile}
--   (t0,j) <- timeit ("read "++inputfile) $ either error id <$> readJournalFile Nothing Nothing True inputfile
--   (t1,_) <- timeit ("print") $ print' opts j
--   (t2,_) <- timeit ("register") $ register opts j
--   (t3,_) <- timeit ("balance") $ balance  opts j
--   (t4,_) <- timeit ("stats") $ stats opts j
--   printf "Total: %0.2fs\n" (sum [t0,t1,t2,t3,t4])

-- timeit :: String -> IO a -> IO (Double, a)
-- timeit name action = do
--   printf "%s%s" name (replicate (40 - length name) ' ')
--   (t,a) <- timeItT action
--   printf "[%.2fs]\n" t
--   return (t,a)

