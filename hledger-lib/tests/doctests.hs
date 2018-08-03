{-# LANGUAGE PackageImports #-}

import Data.List
import System.Environment
import "Glob" System.FilePath.Glob
import Test.DocTest

main = do
  args <- getArgs
  fs1 <- glob "Hledger/**/*.hs"
  fs2 <- glob "Text/**/*.hs"
  --fs3 <- glob "other/ledger-parse/**/*.hs"
  let fs = filter (not . isInfixOf "/.") $ ["Hledger.hs"] ++ fs1 ++ fs2
  doctest $ 
    -- show verbose progress output
    (if "--verbose" `elem` args then ("--verbose" :) else id) $ 
    -- don't reload environment per test (opposite of doctest's --fast,
    -- https://github.com/sol/doctest#a-note-on-performance)
    (if "--slow" `elem` args then id else ("--fast" :)) $
    fs
