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
$ bin/hledger -f data/sample.journal balance o
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
import System.Exit (exitFailure, exitWith, ExitCode(ExitSuccess)) -- base 3 compatible

import Hledger.Data  -- including testing utils in Hledger.Data.Utils
import Hledger.Cli


-- | Run unit tests.
runtests :: [Opt] -> [String] -> IO ()
runtests _ args = do
  (counts,_) <- liftM (flip (,) 0) $ runTestTT ts
  if errors counts > 0 || (failures counts > 0)
   then exitFailure
   else exitWith ExitSuccess
    where
      ts = TestList $ filter matchname $ tflatten tests_Hledger_Cli  -- show flat test names
      -- ts = tfilter matchname $ TestList tests -- show hierarchical test names
      matchname = matchpats args . tname

