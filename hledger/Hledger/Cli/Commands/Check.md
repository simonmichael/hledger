check\
Check for various kinds of errors in your data. 
*experimental*

_FLAGS

hledger provides a number of built-in error checks to help
prevent problems in your data. 
Some of these are run automatically; or,
you can use this `check` command to run them on demand,
with no output and a zero exit code if all is well.
Some examples:

```shell
hledger check      # basic checks
hledger check -s   # basic + strict checks
hledger check ordereddates uniqueleafnames  # basic + specified checks
```

Here are the checks currently available:

### Basic checks

These are always run by this command and other commands:

- **parseable** - data files are well-formed and can be 
  [successfully parsed](hledger.html#input-files)

- **autobalanced** - all transactions are [balanced](journal.html#postings), 
  inferring missing amounts where necessary, and possibly converting commodities 
  using [transaction prices] or automatically-inferred transaction prices

- **assertions** - all [balance assertions] in the journal are passing. 
  (This check can be disabled with `-I`/`--ignore-assertions`.)

### Strict checks

These are always run by this and other commands when `-s`/`--strict` is used
([strict mode]):

- **accounts** - all account names used by transactions 
  [have been declared](journal.html#account-error-checking)

- **commodities** - all commodity symbols used 
  [have been declared](journal.html#commodity-error-checking)

### Other checks

These checks can be run by specifying their names as arguments to the check command:

- **ordereddates** - transactions are ordered by date (similar to the old `check-dates` command)

- **uniqueleafnames** - all account leaf names are unique (similar to the old `check-dupes` command)

This command would run all of the checks above:
```shell
$ hledger check -s ordereddates uniqueleafnames
```

### Addon checks

Some checks are not yet integrated with this command, but are available as
[addon commands] in <https://github.com/simonmichael/hledger/tree/master/bin>:

- **hledger-check-tagfiles** - all tag values containing / (a forward slash) exist as file paths

- **hledger-check-fancyassertions** - more complex balance assertions are passing

You could make your own similar scripts to perform custom checks;
Cookbook -> [Scripting](scripting.html) may be helpful.


[transaction prices]: journal.html#transaction-prices
[balance assertions]: journal.html#balance-assertions
[strict mode]: hledger.html#strict-mode
[addon]: hledger.html#addon-commands