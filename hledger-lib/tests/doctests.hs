{-# LANGUAGE PackageImports #-}

import Data.List
import "Glob" System.FilePath.Glob
import Test.DocTest

main = do
  fs <- ("Hledger.hs" :) . filter (not . isInfixOf "/.") <$> glob "Hledger/**/*.hs"
  doctest fs
