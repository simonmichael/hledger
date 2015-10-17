#!/usr/bin/env runghc
-- dev.hs, for miscellaneous profiling/benchmarking/testing.

-- {-# LANGUAGE DeriveDataTypeable, StandaloneDeriving, DeriveGeneric #-}
-- {-# LANGUAGE NoWarnUnusedImports #-}

-- import System.Environment (getArgs)
-- import Control.Monad.Except
import Criterion.Main
-- import Data.Text.Lazy as LT
-- import System.Environment
import System.TimeIt      (timeItT)
import Text.Printf

import Hledger
-- import Hledger.Utils.Regex (toRegexCI)
-- import Hledger.Utils.Debug
-- import qualified Hledger.Read.JournalReader as JR
-- import qualified Hledger.Read.TimelogReader as TR
-- import qualified Hledger.Read.TimelogReaderNoJU as TRNOJU
-- import qualified Hledger.Read.TimelogReaderPP as TRPP

-- import Control.DeepSeq (NFData)
-- import Data.Data
-- import GHC.Generics (Generic)
-- import Text.Regex.TDFA (Regex(..))
--
-- instance Generic Regex
-- instance NFData Regex
-- deriving instance Data (Regex)
-- deriving instance Typeable (Regex)
-- deriving instance Generic (Regex)
-- instance NFData Regex

journal =
  -- "data/10000x1000x10.journal"
  "data/10000x1000x10.journal"

timelog = "data/sample.timelog"

timeit :: String -> IO a -> IO (Double, a)
timeit name action = do
  printf "%s%s" name (replicate (40 - length name) ' ')
  (t,a) <- timeItT action
  printf "[%.2fs]\n" t
  return (t,a)

timeReadJournal :: String -> String -> IO (Double, Journal)
timeReadJournal msg s = timeit msg $ either error id <$> readJournal Nothing Nothing True Nothing s

main = do
  -- putStrLn $ regexReplaceCI "^aa" "xx" "aa:bb:cc:dd:ee"

  (_t0,_j) <- timeit ("read "++journal) $ either error id <$> readJournalFile Nothing Nothing True journal
  return ()
  -- printf "Total: %0.2fs\n" (sum [t0,t1,t2,t3,t4])

  -- -- read the input journal
  -- s <- readFile journal
  -- j <- either error id <$> readJournal Nothing Nothing True Nothing s
  -- -- putStrLn $ show $ length $ jtxns j -- sanity check we parsed it all
  -- let accts = map paccount $ journalPostings j

  -- Criterion.Main.defaultMainWith defaultConfig $ [
  --   --  bench ("toRegexCI") $ whnf toRegexCI "^aa"
  --   -- ,bench ("toRegexCI") $ whnfIO (return $ toRegexCI "^aa")
  --   -- ,bench ("toRegexCI x 1000") $ nfIO $ sequence_ (map (return . toRegexCI) (replicate 1000 "^aa"))
  --   --  bench ("regexReplaceCI")             $ nf (regexReplaceCI "aa" "xx") "aa:bb:cc:dd:ee:1"
  --   -- ,bench ("regexReplaceCI x 1000")      $ nf (map (regexReplaceCI "bb" "xx")) (replicate 1000 "aa:bb:cc:dd:ee;2")
  --   -- ,bench ("regexReplaceCIMemo")         $ nf (regexReplaceCIMemo "ee" "xx") "aa:bb:cc:dd:ee:5"
  --   -- ,bench ("regexReplaceCIMemo x 1000")  $ nf (map (regexReplaceCIMemo "ff" "xx")) (replicate 1000 "aa:bb:cc:dd:ee:6")
  --    bench ("apply one regex alias to one posting") $
  --      nf (map (accountNameApplyAliases [RegexAlias "^1:" "x:"])) (map paccount $ take 1 $ journalPostings j)
  --   -- ,bench ("apply one regex alias to 20000 postings") $
  --   --    nf (map (accountNameApplyAliases [RegexAlias "^1:" "x:"])) (map paccount $ journalPostings j)
  --   -- ,bench ("apply 3 regex aliases to 20000 postings") $
  --   --    nf (map (accountNameApplyAliases [
  --   --                 RegexAlias "^1:" "x:"
  --   --                ,RegexAlias "^2:" "x:"
  --   --                ,RegexAlias "^3:" "x:"
  --   --                ])) accts

  --   -- ,bench ("readJournal") $ whnfIO $
  --   --    either error id <$>
  --   --    readJournal Nothing Nothing True Nothing s
  --   -- ,bench ("readJournal with aliases") $ whnfIO $
  --   --    either error id <$>
  --   --    readJournal Nothing Nothing True Nothing (
  --   --      unlines [
  --   --         "alias /^fb:/=xx \n"
  --   --         ,"alias /^f1:/=xx \n"
  --   --         ,"alias /^e7:/=xx \n"
  --   --         ] ++ s)

  --   ]

  -- (t0,j0) <- timeReadJournal ("read "++journal) s
  -- (t0',j0') <- timeReadJournal ("read "++journal++" again") s
  -- (t1,j1) <- timeReadJournal ("read "++journal++"with 3 simple aliases")
  --            (unlines [
  --                "alias fb=xx \n"
  --                ,"alias f1=xx \n"
  --                ,"alias e7=xx \n"
  --                ] ++ s)
  -- (t1',j1') <- timeReadJournal ("read "++journal++"with 3 simple aliases again")
  --            (unlines [
  --                "alias fb=xx \n"
  --                ,"alias f1=xx \n"
  --                ,"alias e7=xx \n"
  --                ] ++ s)
  -- (t2,j2) <- timeReadJournal ("read "++journal++"with 3 regex aliases")
  --            (unlines [
  --                "alias /^fb:/=xx \n"
  --                ,"alias /^f1:/=xx \n"
  --                ,"alias /^e7:/=xx \n"
  --                ] ++ s)
  -- (t2',j2') <- timeReadJournal ("read "++journal++"with 3 regex aliases again")
  --            (unlines [
  --                "alias /^fb:/=xx \n"
  --                -- ,"alias /^f1:/=xx \n"
  --                -- ,"alias /^e7:/=xx \n"
  --                ] ++ s)
  -- putStrLn $ show (
  --   -- j0,
  --   -- j0',
  --   -- j1,
  --   -- j1',
  --   -- j2,
  --   j2'
  --   ) -- force evaluation, though it seems not to be needed

  -- return ()

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

