{-
A standalone unit test runner using test-framework. Compared to hledger's
built-in test runner, this one shows verbose ansi-colored hierarchic
results, can run tests in parallel, and may have better quickcheck support.
-}

import Test.Framework (defaultMain {-, testGroup-})
import Test.Framework.Providers.HUnit (hUnitTestToTests)
--import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.HUnit hiding (Test)
import qualified Test.HUnit (Test)
--import Test.QuickCheck
import Tests (tests)
import System.Exit (-- exitFailure, exitWith,
                               ExitCode(..))
import System.IO (hGetContents, hPutStr, hFlush, stderr, stdout)
import Text.Printf (printf)
--import Text.ParserCombinators.Parsec
import Control.Monad (liftM,when)
import Data.Maybe (fromMaybe)
import System.Process (runInteractiveCommand, waitForProcess)

main :: IO ()
main = defaultMain $ concatMap hUnitTestToTests tests
