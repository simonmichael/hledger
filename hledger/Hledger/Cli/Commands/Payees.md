## payees

List the payee/payer names used or declared in the journal.

```flags
Flags:
     --used                 list payees used
     --declared             list payees declared
     --undeclared           list payees used but not declared
     --unused               list payees declared but not used
     --find                 list the first payee matched by the first
                            argument (a case-insensitive infix regexp)
```

This command lists unique payee/payer names -
all of them by default,
or just the ones which have been used in transaction descriptions,
or declared with `payee` directives,
or used but not declared,
or declared but not used,
or just the first one matched by a pattern (with `--find`, returning a non-zero exit code if it fails).

The payee/payer name is the part of the transaction description before a | character
(or if there is no |, the whole description).

You can add [query arguments](#queries) to select a subset of transactions or payees.

Example:
```cli
$ hledger payees
Store Name
Gas Station
Person A
```
