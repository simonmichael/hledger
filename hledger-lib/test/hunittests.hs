import Hledger (tests_Hledger)
import System.Environment (getArgs)
import Test.Framework.Providers.HUnit (hUnitTestToTests)
import Test.Framework.Runners.Console (defaultMainWithArgs)

main :: IO ()
main = do
  args <- getArgs
  let args' = "--hide-successes" : args
  defaultMainWithArgs (hUnitTestToTests tests_Hledger) args'
