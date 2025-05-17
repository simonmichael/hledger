{-
Run the hledger-lib package's unit tests using the tasty test runner.
-}

import Hledger (tests_Hledger)
import System.Environment (setEnv)
import Test.Tasty (defaultMain)

main :: IO ()
main = do
  setEnv "TASTY_HIDE_SUCCESSES" "true"
  setEnv "TASTY_ANSI_TRICKS" "false"  -- helps the above
  defaultMain tests_Hledger
