## check

Check for various kinds of errors in your data. 

_FLAGS

hledger provides a number of built-in correctness checks to help validate your data and prevent errors.
Some are run automatically, some when you enable `--strict` mode;
or you can run any of them on demand with this `check` command.
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
  This ensures that each individual transaction is well formed.

- **assertions** - all [balance assertions] in the journal are passing.
  Balance assertions are like canaries in your journal, they catch many problems.
  This check can sometimes get in the way, eg when you are troubleshooting bookkeeping errors,
  or parsing a journal fragment, so it can be disabled temporarily with `-I`/`--ignore-assertions`.

### Strict checks

These additional checks are performed by any command when the `-s`/`--strict` flag is used (AKA [strict mode]). 
They provide extra error-catching power when you are serious about keeping your data clean and free of typos:

- **balanced** - like `autobalanced`, but in [conversion transactions](#recording-costs),
  costs must be written explicitly. This ensures some redundancy in the entry, guarding against typos.

- **commodities** - all commodity symbols used [must be declared](#commodity-error-checking).
  This guards against forgetting or mistyping commodity symbols.

- **accounts** - all account names used [must be declared](#account-error-checking).
  This prevents the use of mistyped or outdated account names.

### Other checks

These other checks are not wanted by everyone, but can be run using the `check` command:

- **ordereddates** - within each file, transactions are ordered by date.
  This is a simple and effective error catcher, and you should use it.
  Alas! not everyone wants it. If you do, use `hledger check -s ordereddates`.
  Note, this check is performed early, before balance assertions
  (because copy-pasted dates are often the root cause of balance assertion failures).

- **payees** - all payees used by transactions [must be declared](#payee-directive).
  This will force you to always use known/declared payee names. 
  For most people this is a bit too restrictive.

- **tags** - all tags used by transactions [must be declared](#tag-directive).
  This prevents mistyped tag names.

- **recentassertions** - all accounts with balance assertions must have
  a balance assertion within the last 7 days before their latest posting.
  This encourages you to add balance assertions fairly regularly for
  your active asset/liability accounts, which in turn should encourage
  you to check and reconcile with their real world balances fairly regularly.
  [`close --assert`](#close---assert) can be helpful.
  (The older balance assertions become redundant; you can remove them
  periodically, or leave them in place, perhaps commented, as documentation.)

- **uniqueleafnames** - no two accounts may have the same leaf name.
  The leaf name is the last colon-separated part of an account name, eg
  `checking` in `assets:bank:checking`.
  This encourages you to keep those unique, effectively giving each account
  a short name which is easier to remember and to type in reporting commands.

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
