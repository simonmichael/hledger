import Hledger.Cli (tests_Hledger_Cli)
import Test.Framework.Providers.HUnit (hUnitTestToTests)
import Test.Framework.Runners.Console (defaultMain)

main :: IO ()
main = defaultMain $ hUnitTestToTests tests_Hledger_Cli
