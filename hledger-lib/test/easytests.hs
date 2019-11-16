{-# LANGUAGE PackageImports #-}
{-
Run hledger-lib's easytest tests using the easytest runner.
Note that we use package-qualified import to overcome
Cabal heuristic missing-home-modules.
-}
import "hledger-lib" Hledger
main = run tests_Hledger
