test\
Run built-in unit tests.

_FLAGS

This command runs the unit tests built in to hledger and hledger-lib,
printing the results on stdout. If any test fails, the exit code will
be non-zero.

This is mainly used by hledger developers, but you can also use it to
sanity-check the installed hledger executable on your platform. All
tests are expected to pass - if you ever see a failure, please report
as a bug!

This command also accepts tasty test runner options, written after a
-- (double hyphen). Eg to run only the tests in Hledger.Data.Amount,
with ANSI colour codes disabled:
```shell
$ hledger test -- -pData.Amount --color=never
```
For help on these, see https://github.com/feuerbach/tasty#options 
(`-- --help` currently doesn't show them).
