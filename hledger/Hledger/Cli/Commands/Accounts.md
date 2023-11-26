## accounts

Show account names.

_FLAGS

This command lists account names.
By default it shows all known accounts, either used in transactions or declared with account directives.

With query arguments, only matched account names and account names referenced by matched postings are shown.

Or it can show just
the used accounts (`--used`/`-u`),
the declared accounts (`--declared`/`-d`),
the accounts declared but not used (`--unused`),
the accounts used but not declared (`--undeclared`),
or the first account matched by an account name pattern, if any (`--find`).

It shows a flat list by default. With `--tree`, it uses indentation to
show the account hierarchy.
In flat mode you can add `--drop N` to omit the first few account name components.
Account names can be depth-clipped with `depth:N` or `--depth N` or `-N`.

With `--types`, it also shows each account's type, if it's known.
(See Declaring accounts > Account types.)

With `--positions`, it also shows the file and line number of each
account's declaration, if any, and the account's overall declaration order;
these may be useful when troubleshooting account display order.

With `--directives`, it adds the `account` keyword, showing
valid account directives which can be pasted into a journal file.
This is useful together with `--undeclared` when updating your account declarations
to satisfy `hledger check accounts`.

The `--find` flag can be used to look up a single account name, in the same way
that the `aregister` command does. It returns the alphanumerically-first matched
account name, or if none can be found, it fails with a non-zero exit code.

Examples:

```cli
$ hledger accounts
assets:bank:checking
assets:bank:saving
assets:cash
expenses:food
expenses:supplies
income:gifts
income:salary
liabilities:debts
```
```cli
$ hledger accounts --undeclared --directives >> $LEDGER_FILE
$ hledger check accounts
```
