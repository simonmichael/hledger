-- {-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE CPP #-}
{- |

A simple test runner for hledger's built-in unit tests.

-}

module Hledger.Cli.Tests (
  testmode
 ,test'
)
where

import Control.Monad
import System.Exit
import Test.HUnit

import Hledger
import Hledger.Cli

#ifdef TESTS

import Test.Framework
import {-@ HTF_TESTS @-} Hledger.Read.JournalReader

-- | Run HTF unit tests and exit with success or failure.
test' :: CliOpts -> IO ()
test' _opts = htfMain htf_importedTests

#else

-- | Run HUnit unit tests and exit with success or failure.
test' :: CliOpts -> IO ()
test' opts = do
  results <- runTests opts
  if errors results > 0 || failures results > 0
   then exitFailure
   else exitWith ExitSuccess

testmode = (defCommandMode ["test"]) {
  modeHelp = "run built-in self-tests"
 ,modeArgs = ([], Just $ argsFlag "[REGEXPS]")
 ,modeGroupFlags = Group {
     groupUnnamed = []
    ,groupHidden = []
    ,groupNamed = [generalflagsgroup3]
    }
 }

-- | Run all or just the matched unit tests and return their HUnit result counts.
runTests :: CliOpts -> IO Counts
runTests = liftM (fst . flip (,) 0) . runTestTT . flatTests

-- -- | Run all or just the matched unit tests until the first failure or
-- -- error, returning the name of the problem test if any.
-- runTestsTillFailure :: CliOpts -> IO (Maybe String)
-- runTestsTillFailure _ = undefined -- do
--   -- let ts = flatTests opts
--   --     results = liftM (fst . flip (,) 0) $ runTestTT $
--   --     firstproblem = find (\counts -> )

-- | All or pattern-matched tests, as a flat list to show simple names.
flatTests opts = TestList $ filter (matchesAccount (queryFromOpts nulldate $ reportopts_ opts) . testName) $ flattenTests tests_Hledger_Cli

-- -- | All or pattern-matched tests, in the original suites to show hierarchical names.
-- hierarchicalTests opts = filterTests (matchesAccount (queryFromOpts nulldate $ reportopts_ opts) . testName) tests_Hledger_Cli

#endif
