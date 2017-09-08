{-# LANGUAGE PackageImports #-}

import Data.List
import "Glob" System.FilePath.Glob
import Test.DocTest

main = do
  fs1 <- glob "Hledger/**/*.hs"
  fs2 <- glob "Text/**/*.hs"
  --fs3 <- glob "other/ledger-parse/**/*.hs"
  doctest $ filter (not . isInfixOf "/.") $ ["Hledger.hs"] ++ fs1 ++ fs2
