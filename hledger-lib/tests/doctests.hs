{-# LANGUAGE PackageImports #-}

import Data.List
import "Glob" System.FilePath.Glob
import Test.DocTest

main = do
  fs1 <- glob "Hledger/**/*.hs"
  fs2 <- glob "Text/**/*.hs"
  --fs3 <- glob "other/ledger-parse/**/*.hs"
  let fs = filter (not . isInfixOf "/.") $ ["Hledger.hs"] ++ fs1 ++ fs2
  doctest $ 
    "--fast" :  -- https://github.com/sol/doctest#a-note-on-performance 
    fs
