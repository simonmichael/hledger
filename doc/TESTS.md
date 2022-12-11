# Tests

<div class=pagetoc>

<!-- toc -->
</div>

About testing in the hledger project, as of 201809.

## Kinds of tests

<div style="margin:1em 2em; font-style:italic;">
"Here, then, is a list of properties of tests. Not all tests need to exhibit all properties. However, no property should be given up without receiving a property of greater value in return.

- Isolated — tests should return the same results regardless of the order in which they are run.
- Composable — if tests are isolated, then I can run 1 or 10 or 100 or 1,000,000 and get the same results.
- Fast — tests should run quickly.
- Inspiring — passing the tests should inspire confidence
- Writable — tests should be cheap to write relative to the cost of the code being tested.
- Readable — tests should be comprehensible for reader, invoking the motivation for writing this particular test.
- Behavioral — tests should be sensitive to changes in the behavior of the code under test. If the behavior changes, the test result should change.
- Structure-insensitive — tests should not change their result if the structure of the code changes.
- Automated — tests should run without human intervention.
- Specific — if a test fails, the cause of the failure should be obvious.
- Deterministic — if nothing changes, the test result shouldn’t change.
- Predictive — if the tests all pass, then the code under test should be suitable for production."
--[Kent Beck](https://medium.com/@kentbeck_7670/test-desiderata-94150638a4b3)
</div>

1.  Unit tests

    Unit tests exercise small chunks of functionality. In hledger, that
    means a function. So, many of our functions have one or more unit
    tests. These are mostly in hledger-lib, with a few in hledger.

    Our unit tests use the
    [tasty](https://hackage.haskell.org/package/tasty) test runner,
    [tasty-hunit](https://hackage.haskell.org/package/tasty-hunit) HUnit-style tests,
    and some helpers from
    [Hledger.Utils.Test](https://github.com/simonmichael/hledger/blob/master/hledger-lib/Hledger/Utils/Test.hs),
    such as:
    
    - `tests` and `test` aliases for `testGroup` and `testCase`
    - `assert*` helpers for constructing various kinds of assertions

    We would like our unit tests to be:

    -   easy to read (clear, concise)
    -   easy to write (low boilerplate, low cognitive load)
    -   easy to maintain (easy to edit, easy to refactor, robust)
    -   easy to associate with the code under test (easy to view/jump
        between code & test, easy to estimate coverage)
    -   and scalable (usable for all devs, easy to run and select,
        suitable for small/large modules/packages).

    Here\'s the current pattern (let us know if you see a better way):

    ``` haskell
    module Foo (
      ...
      tests_Foo -- export this module's and submodules' tests
    )
    where
    import Hledger  -- provides Hledger.Utils.Test helpers
    import Bar      -- submodules, providing tests_Bar etc.
    import Baz

    functionA = ...
    functionB = ...
    functionC = ...
    functionD = ...

    tests_Foo = tests "Foo" [ -- define tests at the end of each module

       -- a group of several named tests for functionA
       tests "functionA" [
         test "a basic test"           $ assertBool "" SOMEBOOL
        ,test "a pretty equality test" $ SOMEEXPR @?= EXPECTEDVALUE
        ,test "a pretty parsing test"  $ assertParseEq PARSER INPUT EXPECTEDRESULT
        ,test "a multiple assertions test" $ do
          A @?= B
          doSomeIO
          C @?= D
        ]

       -- a single test containing multiple unnamed assertions for functionB
      ,test "functionB" $ do
         assertBool "" BOOL
         EXPR @?= VALUE

      ,tests_Foo            -- aggregate submodule tests
      ,tests_Bar
      ]
    ```

    Here are
    [some](https://github.com/simonmichael/hledger/blob/master/hledger-lib/Hledger/Data/Posting.hs#L296)
    real-world
    [examples](https://github.com/simonmichael/hledger/blob/master/hledger-lib/Hledger/Read/JournalReader.hs#L579).

    The unit tests are shipped as part of the hledger executable, and
    can always be run via the [test](https://hledger.org/hledger.html#test)
    command (`hledger test`).

    Here\'s the quick way to run unit tests while developing:\
    `make ghcid-test` or `make ghcid-test-Some.Module`.

2.  Doc tests

    Like unit tests, but defined inside functions\' haddock
    documentation, in the style of a GHCI transcript. These test
    functionality, provide usage examples in the API docs, and test
    those examples, all at once. They are a bit more finicky and slower
    than unit tests. See
    [doctest](https://hackage.haskell.org/package/doctest) for more.

    doctests [do not work on Mac with GHC
    8.4+](https://github.com/sol/doctest/issues/199), out of the box.
    See
    [ghc\#15105](https://ghc.haskell.org/trac/ghc/ticket/15105#comment:10)
    for current status and a workaround.

3.  Functional tests

    Functional tests test the overall functioning of the program. For
    hledger, that means running `hledger` with various inputs and
    options and checking for the expected output. This exercises
    functionality in the hledger and hledger-lib packages. We do this
    with
    [shelltestrunner](https://hackage.haskell.org/package/shelltestrunner).
    Tests are defined in files named `*.test` under
    [hledger/test/](https://github.com/simonmichael/hledger/tree/master/hledger/test),
    grouped by *component* (command or topic name).
    For more about these, see the README there.

4.  Code tests

    We have some tests aimed at testing eg code quality, generally
    defined as make rules, such as:

      --------------------- -------------------------------------------------------------------------------------
      `make haddocktest`    can haddock process all code docs without error
      `make buildtest`      does all code build warning free with the default GHC version & stackage snapshot
      `make buildtestall`   does the code build warning free with all supported GHC versions/stackage snapshots
      --------------------- -------------------------------------------------------------------------------------

    See below for examples.

5.  Package test suites

    Haskell tools like stack and cabal recognise test suites defined in
    a package\'s cabal file (or package.yaml file). These can be run via
    `stack test`, `cabal test` etc., and they are required to build and
    pass by services like Stackage. Here are the currently hledger
    package test suites:

      ------------- ------------ ---------------------------------------------------------------
      package       test suite   what it runs
      hledger-lib   doctests     doctests
      hledger-lib   easytests    unit tests
      hledger       test         builtin test command (hledger\'s + hledger-lib\'s unit tests)
      hledger-ui                 
      hledger-web                
      ------------- ------------ ---------------------------------------------------------------

## Coverage

This means how thoroughly the code is tested - both in breadth (are all
parts of the code tested at least a little ?) and in depth (are all
possible code paths, states, situations tested ?).

Our current test coverage can be summarised like so:

  ------------- ------ ----- ------------
  package       unit   doc   functional
  hledger-lib   X      X     X
  hledger       X            X
  hledger-ui                 
  hledger-web                
  ------------- ------ ----- ------------

There are ways to generate detailed coverage reports for haskell unit
tests, at least. It would be useful to set this up for hledger.

## How to run tests

Run unit tests:

``` example
$ make unittest
```

Run doctests:

``` example
$ make doctest
```

Run functional tests (and unit tests, now):

``` example
$ stack install shelltestrunner
$ make functest
```

Run the package tests (unit tests, maybe doctests, but not functional
tests) of all or selected packages.

``` example
$ stack test [PKG]
```

Run \"default tests: package plus functional tests\":

``` example
$ make test
```

Test generation of haddock docs:

``` example
$ make haddocktest
```

Thorough test for build issues with current GHC:

``` example
$ make buildtest
```

Thorough test for build issues with all supported GHC versions:

``` example
$ make buildtestall
```

Run built-in hledger/hledger-lib unit tests via hledger command:

``` example
$ hledger test  # test installed hledger
$ stack build hledger && stack exec -- hledger test  # test just-built hledger
$ hledger test --help
test [TESTPATTERN] [SEED]
  Run the unit tests built in to hledger-lib and hledger,
  printing results on stdout and exiting with success or failure.
  Tests are run in two batches: easytest-based and hunit-based tests.
  If any test fails or gives an error, the exit code will be non-zero.
  If a pattern argument (case sensitive) is provided, only easytests
  in that scope and only hunit tests whose name contains it are run.
  If a numeric second argument is provided, it will set the randomness
  seed for easytests.
```

Rebuild and rerun hledger/hledger-lib unit tests via ghcid:

``` example
$ make ghcid-test
```

Rebuild and rerun only some tests via ghcid (see hledger test --help):

``` example
$ make ghcid-test-TESTPATTERN
```

See all test-related make rules:

``` example
$ make help-test
```




