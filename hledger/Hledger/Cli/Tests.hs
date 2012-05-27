{- |

This module contains hledger's unit tests. These are built in to hledger,
and can be run at any time by doing @hledger test@ (or, with a few more
options, by doing @make unittest@ in the hledger source tree.)

Other kinds of tests:

hledger's functional tests are a set of shell/command-line tests defined
by .test files in the tests\/ subdirectory. These can be run by doing
@make functest@ in the hledger source tree.

hledger's doctests are shell commands with expected output in literal
blocks in the haddock documentation, run by doing @make doctest@ in the
hledger source tree. They are hardly used, but here is an example:

@
$ bin\/hledger -f data\/sample.journal balance o
                  $1  expenses:food
                 $-2  income
                 $-1    gifts
                 $-1    salary
--------------------
                 $-1
@

-}

module Hledger.Cli.Tests
where
import Control.Monad
import System.Exit
import Test.HUnit

import Hledger
import Hledger.Cli


-- | Run unit tests and exit with success or failure.
test' :: CliOpts -> IO ()
test' opts = do
  results <- runTests opts
  if errors results > 0 || failures results > 0
   then exitFailure
   else exitWith ExitSuccess

-- | Run all or just the matched unit tests and return their HUnit result counts.
runTests :: CliOpts -> IO Counts
runTests = liftM (fst . flip (,) 0) . runTestTT . flatTests

-- | Run all or just the matched unit tests until the first failure or
-- error, returning the name of the problem test if any.
runTestsTillFailure :: CliOpts -> IO (Maybe String)
runTestsTillFailure opts = undefined -- do
  -- let ts = flatTests opts
  --     results = liftM (fst . flip (,) 0) $ runTestTT $
  --     firstproblem = find (\counts -> )

-- | All or pattern-matched tests, as a flat list to show simple names.
flatTests opts = TestList $ filter (matchesAccount (queryFromOpts nulldate $ reportopts_ opts) . testName) $ flattenTests tests_Hledger_Cli

-- | All or pattern-matched tests, in the original suites to show hierarchical names.
hierarchicalTests opts = filterTests (matchesAccount (queryFromOpts nulldate $ reportopts_ opts) . testName) tests_Hledger_Cli
