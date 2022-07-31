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

If you are an Emacs user, you can also configure flycheck-hledger to run these checks,
providing instant feedback as you edit the journal.

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

- **recentassertions** - all accounts with balance assertions have a
  (cleared) assertion no more than 7 days before their latest posting

- **uniqueleafnames** - all account leaf names are unique

### Custom checks

A few more checks are are available as separate [add-on commands],
in <https://github.com/simonmichael/hledger/tree/master/bin>:

- **hledger-check-tagfiles** - all tag values containing / (a forward slash) exist as file paths

- **hledger-check-fancyassertions** - more complex balance assertions are passing

You could make similar scripts to perform your own custom checks.
See: Cookbook -> [Scripting](scripting.html).

### More about specific checks

`hledger check recentassertions` will complain if any balance-asserted account does not have
a (cleared) balance assertion within 7 days before its latest posting.
This aims to prevent the situation where you are regularly updating your journal,
but forgetting to check your balances against the real world,
then one day must dig back through months of data to find an error.
It assumes that adding a balance assertion requires/reminds you to check the real-world balance.
This won't be true if you auto-generate balance assertions when importing bank data,
but hopefully your auto-generated transactions are uncleared, and before manually marking them 
cleared you will remember to check the latest assertions against the real-world balances.

[add-on commands]:    #add-on-commands
[balance assertions]: #balance-assertions
[strict mode]:        #strict-mode
[transaction prices]: #transaction-prices
