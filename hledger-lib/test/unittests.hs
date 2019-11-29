{-
Run the hledger-lib package's unit tests using the tasty test runner.
-}

-- package-qualified import to avoid cabal missing-home-modules warning (and double-building ?)
{-# LANGUAGE PackageImports #-}
import "hledger-lib" Hledger (tests_Hledger)

import Test.Tasty (defaultMain)

main = defaultMain tests_Hledger
