{-# LANGUAGE PackageImports #-}
{-
Run hledger-lib's unit tests using tasty's test runner.
Note that we use package-qualified import to overcome
Cabal heuristic missing-home-modules.
-}
import "hledger-lib" Hledger
import Test.Tasty (defaultMain)

main = defaultMain tests_Hledger
