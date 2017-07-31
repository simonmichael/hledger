{-# LANGUAGE PackageImports #-}

import Data.List
import Data.Monoid
import "Glob" System.FilePath.Glob
import Test.DocTest

main =
     pure ["Hledger.hs"]
  <> glob "Hledger/**/*.hs" 
  <> glob "Text/**/*.hs"
  -- <> glob "other/ledger-parse/**/*.hs"
  >>= pure . filter (not . isInfixOf "/.") 
  >>= doctest
