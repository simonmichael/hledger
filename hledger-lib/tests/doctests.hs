{-# LANGUAGE PackageImports #-}

import Data.List
import "Glob" System.FilePath.Glob
import Test.DocTest

main = do
  fs1 <- filter (not . isInfixOf "/.") <$> glob "Hledger/**/*.hs"
  fs2 <- filter (not . isInfixOf "/.") <$> glob "other/ledger-parse/**/*.hs"
  doctest $ ["Hledger.hs"] ++ fs1 ++ fs2
