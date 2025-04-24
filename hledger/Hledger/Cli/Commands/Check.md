## check

Check for various kinds of errors in your data. 

```flags
Flags:
no command-specific flags
```

hledger provides a number of built-in correctness checks to help validate your data and prevent errors.
Some are run automatically, some when you enable `--strict` mode;
or you can run any of them on demand by providing them as arguments to the `check` command.
`check` produces no output and a zero exit code if all is well.
Eg:

```cli
hledger check                      # run basic checks
hledger check -s                   # run basic and strict checks
hledger check ordereddates payees  # run basic checks and two others
```

If you are an Emacs user, you can also configure flycheck-hledger to run these checks,
providing instant feedback as you edit the journal.

Here are the checks currently available.
Generally, they are performed in the order they are shown here (and only the first failure is reported).

### Basic checks

These important checks are performed by default, by almost all hledger commands:

- **parseable** - data files are in a supported [format](#data-formats),
  with no syntax errors and no invalid include directives.
  This ensures that all files exist and are readable.

- **autobalanced** - all transactions are [balanced](#postings),
  after inferring missing amounts and conversion [costs] where possible,
  and then converting to cost.
  This ensures that each transaction's entry is well formed.

- **assertions** - all [balance assertions] in the journal are passing.
  Balance assertions are a strong defense against errors; they help catch many problems.
  You can disable this check by adding `-I`/`--ignore-assertions` to a command or to your config file.
  When you want to re-enable it, use `-s`/`--strict` or `hledger check assertions`.

### Strict checks

These additional checks are performed by any command when the `-s`/`--strict` flag is used ([strict mode]). 
Strict mode also always enables the `assertions` check.
These provide extra error-catching power when you are serious about keeping your data clean and free of typos:

- **balanced** - more strict than `autobalanced`: any commodity conversions must be explicit,
  using either [cost notation](#recording-costs) or [equity postings](#equity-conversion-postings).
  This prevents wrong conversions caused by typos.

- **commodities** - all commodity symbols used must be [declared](#commodity-error-checking).
  This guards against mistyping or omitting commodity symbols.
  commodity declarations also set their precision for display and transaction balancing.

- **accounts** - all account names used must be [declared](#account-error-checking).
  This prevents the use of mis-spelled or outdated account names.

### Other checks

These are not wanted by everyone, but can be run using the `check` command:

- **tags** - all tags used must be [declared](#tag-directive).
  This prevents mis-spelled tag names.
  Note hledger fairly often finds unintended tags in comments.

- **payees** - all payees used in transactions must be [declared](#payee-directive).
  This will force you to declare any new payee name before using it.
  Most people will probably find this a bit too strict.

- **ordereddates** - within each file, transactions must be ordered by date.
  This is a simple and effective error catcher. It's not included in strict mode,
  but you can add it by running `hledger check -s ordereddates`.
  If enabled, this check is performed before balance assertions.

- **recentassertions** - all accounts with balance assertions must have one that's
  within the 7 days before their latest posting.
  This will encourage adding balance assertions for your active asset/liability accounts,
  which in turn should encourage you to reconcile regularly with those real world balances -
  another strong defense against errors.
  [`hledger close --assert`](#close---assert) can help generate assertion entries.
  Over time the older assertions become somewhat redundant, and you can remove them if you like
  (they don't affect performance much, but they add some noise to the journal).

- **uniqueleafnames** - no two accounts may have the same last account name part
  (eg the `checking` in `assets:bank:checking`).
  This ensures each account can be matched by a unique short name, easier to remember and to type.

### Custom checks

You can build your own custom checks with [add-on command scripts].
See also [Cookbook > Scripting](scripting.html).
Here are some examples from [hledger/bin/](https://github.com/simonmichael/hledger/tree/master/bin):

- **hledger-check-tagfiles** - all tag values containing / (a forward slash) exist as file paths

- **hledger-check-fancyassertions** - more complex balance assertions are passing


[add-on command scripts]:    #add-on-commands
[balance assertions]: #balance-assertions
[strict mode]:        #strict-mode
[costs]: #costs
