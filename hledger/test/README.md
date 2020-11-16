hledger test-related files. See also [Contributor Guide: Tests].

unittest.hs - main file for a cabal test suite in the hledger package
(run by "cabal test" or "stack test"). Runs the unit tests built in to
all hledger modules. Not used much, we usually run them via hledger's
builtin "test" command instead.

doctest.hs - main file for another cabal test suite. Runs the doctests
embedded in haddock comments in some hledger modules.

The rest of the files here are functional tests, run with [shelltestrunner].
These test the hledger CLI and (indirectly) the hledger-lib package.
They are organised roughly by [component].

Older test files are in shelltestrunner's [format 1]; 
newer ones use [format 3] (preferred).
Some tests invoke unix commands so will not run in a Windows CMD shell.

[Contributor Guide: Tests]: https://hledger.org/CONTRIBUTING.html#tests
[component]: https://hledger.org/CONTRIBUTING.html#components
[shelltestrunner]: https://github.com/simonmichael/shelltestrunner#readme
[format 1]: https://github.com/simonmichael/shelltestrunner#format-1
[format 3]: https://github.com/simonmichael/shelltestrunner#format-3


Run them all (also builds hledger):

    make functest

See how the Makefile is invoking shelltestrunner:

    $ make functest -n
    stack build --fast hledger
    (COLUMNS=80 stack exec -- shelltest --execdir -j16 --hide-successes --exclude=/_ -w `stack exec -- which hledger` tests \
            && echo functest PASSED) || (echo functest FAILED; false)

These are the most important:

- `COLUMNS=80` makes output independent of your terminal width.
- `--execdir` runs each test within its own directory.
- ``-w `stack exec -- which hledger` `` ensures you are testing the hledger executable that was just built.
- `-j16` runs tests in parallel which is much faster.

Run only the tests matching a regular expression:

    $ COLUMNS=80 shelltest --execdir -w `stack exec -- which hledger` tests -i balance-assertions.*19
    :hledger/test/journal/balance-assertions.test:19: [OK]

             Test Cases  Total      
     Passed  1           1          
     Failed  0           0          
     Total   1           1          

Run only the tests in one file:

    $ COLUMNS=80 shelltest --execdir -w `stack exec -- which hledger` hledger/test/cli/query-args.test
    :hledger/test/cli/query-args.test:1: [OK]
    :hledger/test/cli/query-args.test:2: [OK]
    :hledger/test/cli/query-args.test:3: [OK]

             Test Cases  Total      
     Passed  3           3          
     Failed  0           0          
     Total   3           3          

Run a test [repeatedly](http://eradman.com/entrproject/) as its file is changed:

    $ ls hledger/test/cli/query-args.test | entr bash -c "COLUMNS=80 shelltest --execdir -w `stack exec -- which hledger` hledger/test/cli/query-args.test -i1"
    :hledger/test/cli/query-args.test:1: [OK]

             Test Cases  Total      
     Passed  1           1          
     Failed  0           0          
     Total   1           1          
    :hledger/test/cli/query-args.test:1: [OK]

             Test Cases  Total      
     Passed  1           1          
     Failed  0           0          
     Total   1           1          
      C-c C-c

More shelltestrunner options:

    $ shelltest --help

