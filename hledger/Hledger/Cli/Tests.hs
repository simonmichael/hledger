{- |

This module contains hledger's unit tests. These are built in to hledger,
and can be run at any time by doing @hledger test@ (or, with a few more
options, by doing @make unittest@ in the hledger source tree.)

Other kinds of tests:

hledger's functional tests are a set of shell/command-line tests defined
by .test files in the tests\/ subdirectory. These can be run by doing
@make functest@ in the hledger source tree.

hledger's doctests are shell tests defined in literal blocks in haddock
documentation in the source, run by doing @make doctest@ in the hledger
source tree. are They hardly used, but here is an example:

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
import System.Exit (exitFailure, exitWith, ExitCode(ExitSuccess)) -- base 3 compatible
import Test.HUnit

import Hledger.Cli
import Hledger.Data
import Hledger.Utils


-- | Run unit tests and exit with success or failure.
runtests :: [Opt] -> [String] -> IO ()
runtests opts args = do
  (hunitcounts,_) <- runtests' opts args
  if errors hunitcounts > 0 || (failures hunitcounts > 0)
   then exitFailure
   else exitWith ExitSuccess

-- | Run unit tests and exit on failure.
runTestsOrExit :: [Opt] -> [String] -> IO ()
runTestsOrExit opts args = do
  (hunitcounts,_) <- runtests' opts args
  when (errors hunitcounts > 0 || (failures hunitcounts > 0)) $ exitFailure

runtests' :: Num b => t -> [String] -> IO (Counts, b)
runtests' _ args = liftM (flip (,) 0) $ runTestTT ts
    where
      ts = TestList $ filter matchname $ tflatten tests_Hledger_Cli  -- show flat test names
      -- ts = tfilter matchname $ TestList tests -- show hierarchical test names
      matchname = matchpats args . tname
