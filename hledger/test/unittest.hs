{-
Run the hledger package's unit tests using the tasty test runner
(by running the test command limited to Hledger.Cli tests).
-}

import Hledger.Cli (tests_Hledger_Cli)
import System.Environment (setEnv)
import Test.Tasty (defaultMain)

main :: IO ()
main = do
  setEnv "TASTY_HIDE_SUCCESSES" "true"
  setEnv "TASTY_ANSI_TRICKS" "false"  -- helps the above
  defaultMain tests_Hledger_Cli
