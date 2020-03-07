{-
Run the hledger-lib package's unit tests using the tasty test runner.
-}

-- package-qualified import to avoid cabal missing-home-modules warning (and double-building ?)
{-# LANGUAGE PackageImports #-}
import "hledger-lib" Hledger (tests_Hledger)
import System.Environment (setEnv)
import Test.Tasty (defaultMain)

main :: IO ()
main = do
  setEnv "TASTY_HIDE_SUCCESSES" "true"
  setEnv "TASTY_ANSI_TRICKS" "false"  -- helps the above
  defaultMain tests_Hledger
