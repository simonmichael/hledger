check\
Check for various kinds of errors in your data. 

_FLAGS

hledger provides a number of built-in error checks to help
prevent problems in your data. 
Some of these are run automatically; or,
you can use this `check` command to run them on demand,
with no output and a zero exit code if all is well.
Specify their names (or a prefix) as argument(s).

Some examples:

```shell
hledger check      # basic checks
hledger check -s   # basic + strict checks
hledger check ordereddates payees  # basic + two other checks
```

Here are the checks currently available:

### Basic checks

These checks are always run automatically, by (almost) all hledger commands,
including `check`:

- **parseable** - data files are well-formed and can be 
  [successfully parsed](hledger.html#input-files)

- **balancedwithautoconversion** - all transactions are [balanced](hledger.html#postings),
  inferring missing amounts where necessary, and possibly converting commodities 
  using [transaction prices] or automatically-inferred transaction prices

- **assertions** - all [balance assertions] in the journal are passing. 
  (This check can be disabled with `-I`/`--ignore-assertions`.)

### Strict checks

These additional checks are run when the `-s`/`--strict` ([strict mode]) flag is used.
Or, they can be run by giving their names as arguments to `check`:

- **accounts** - all account names used by transactions 
  [have been declared](hledger.html#account-error-checking)

- **commodities** - all commodity symbols used 
  [have been declared](hledger.html#commodity-error-checking)

- **balancednoautoconversion** - transactions are balanced, possibly using
  explicit transaction prices but not [inferred ones](#transaction-prices)

### Other checks

These checks can be run only by giving their names as arguments to `check`.
They are more specialised and not desirable for everyone, therefore optional:

- **ordereddates** - transactions are ordered by date within each file

- **payees** - all payees used by transactions [have been declared](#declaring-payees)

- **uniqueleafnames** - all account leaf names are unique

### Custom checks

A few more checks are are available as separate [add-on commands],
in <https://github.com/simonmichael/hledger/tree/master/bin>:

- **hledger-check-tagfiles** - all tag values containing / (a forward slash) exist as file paths

- **hledger-check-fancyassertions** - more complex balance assertions are passing

You could make similar scripts to perform your own custom checks.
See: Cookbook -> [Scripting](scripting.html).


[add-on commands]:    #add-on-commands
[balance assertions]: #balance-assertions
[strict mode]:        #strict-mode
[transaction prices]: #transaction-prices
