## check

Check for various kinds of errors in your data. 

_FLAGS

hledger provides a number of built-in correctness checks to help
prevent problems in your data. 
Some of these are run automatically; or,
you can use this `check` command to run them on demand,
with no output and a zero exit code if all is well.
Specify their names (or a prefix) as argument(s).

Some examples:

```cli
hledger check      # basic checks
hledger check -s   # basic + strict checks
hledger check ordereddates payees  # basic + two other checks
```

If you are an Emacs user, you can also configure flycheck-hledger to run these checks,
providing instant feedback as you edit the journal.

Here are the checks currently available.
They are performed in the order they are shown here.
(Eg, an `ordereddates` failure takes precedence over an `assertions` failure).

### Default checks

These checks are always performed, by (almost) all hledger commands:

- **parseable** - data files are in a supported [format](#data-formats),
  with no syntax errors and no invalid include directives

- **autobalanced** - all transactions are [balanced](#postings),
  after inferring missing amounts and conversion [costs] where possible,
  and then converting to cost

- **assertions** - all [balance assertions] in the journal are passing.
  (This check can be disabled with `-I`/`--ignore-assertions`.)

### Strict checks

These additional checks are run when the `-s`/`--strict` ([strict mode])
flag is used with any command; or,
when they are given as arguments to the `check` command:

- **balanced** - like `autobalanced`, but conversion costs will not be
  inferred, and must be written explicitly

- **commodities** - all commodity symbols used 
  [have been declared](#commodity-error-checking)

- **accounts** - all account names used
  [have been declared](#account-error-checking)

### Other checks

These checks can be run by giving their names as arguments to `check`:

- **ordereddates** - within each file, transactions are ordered by date

- **payees** - all payees used by transactions [have been declared](#payee-directive)

- **tags** - all tags used by transactions [have been declared](#tag-directive)

- **recentassertions** - all accounts with balance assertions have a
  balance assertion within 7 days of their latest posting

- **uniqueleafnames** - all account leaf names are unique

### Custom checks

You can build your own custom checks with [add-on command scripts].
(See also [Cookbook > Scripting](scripting.html).)
Here are some examples from [hledger/bin/](https://github.com/simonmichael/hledger/tree/master/bin):

- **hledger-check-tagfiles** - all tag values containing / (a forward slash) exist as file paths

- **hledger-check-fancyassertions** - more complex balance assertions are passing


### More about specific checks

`hledger check recentassertions` will complain if any balance-asserted account
has postings more than 7 days after its latest balance assertion.
This aims to prevent the situation where you are regularly updating your journal,
but forgetting to check your balances against the real world,
then one day must dig back through months of data to find an error.
It assumes that adding a balance assertion requires/reminds you to check the real-world balance.
(That may not be true if you auto-generate balance assertions from bank data;
in that case, I recommend to import transactions uncleared, 
and when you manually review and clear them, also check the latest assertion against the real-world balance.)

[add-on command scripts]:    #add-on-commands
[balance assertions]: #balance-assertions
[strict mode]:        #strict-mode
[costs]: #costs
