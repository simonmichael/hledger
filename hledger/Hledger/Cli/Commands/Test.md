## test

Run built-in unit tests.

```flags
Flags:
no command-specific flags
```

This command runs the unit tests built in to hledger and hledger-lib,
printing the results on stdout. If any test fails, the exit code will
be non-zero.

This is mainly used by hledger developers, but you can also use it to
sanity-check the installed hledger executable on your platform. All
tests are expected to pass - if you ever see a failure, please report
as a bug!

Any arguments before a `--` argument will be passed to the `tasty` test runner as test-selecting -p patterns,
and any arguments after `--` will be passed to tasty unchanged.

Examples:
```
$ hledger test               # run all unit tests
$ hledger test balance       # run tests with "balance" in their name
$ hledger test -- -h         # show tasty's options
```
