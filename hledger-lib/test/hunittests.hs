{-
Run hledger-lib's HUnit tests using the test-framework test runner, 
passing --hide-successes and any additional command line args.
-}

import System.Environment (getArgs)
import Test.Framework.Providers.HUnit (hUnitTestToTests)
import Test.Framework.Runners.Console (defaultMainWithArgs)
import Hledger (tests_Hledger)

main = do
  args <- getArgs
  let args' = "--hide-successes" : args
  defaultMainWithArgs (hUnitTestToTests tests_Hledger) args'
