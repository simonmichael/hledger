balancesheet, bs\
This command displays a simple balance sheet, showing historical ending
balances of asset and liability accounts (ignoring any report begin date).
It assumes that these accounts are under a top-level `asset` or `liability`
account (case insensitive, plural forms also  allowed).

Note this report shows all account balances with normal positive sign
(like conventional financial statements, unlike balance/print/register)
(experimental).

_FLAGS_

Example:

```shell
$ hledger balancesheet
Balance Sheet

Assets:
                 $-1  assets
                  $1    bank:saving
                 $-2    cash
--------------------
                 $-1

Liabilities:
                  $1  liabilities:debts
--------------------
                  $1

Total:
--------------------
                   0
```

With a [reporting interval](#reporting-interval), multiple columns
will be shown, one for each report period.
As with [multicolumn balance reports](#multicolumn-balance-reports),
you can alter the report mode with `--change`/`--cumulative`/`--historical`.
Normally balancesheet shows historical ending balances, which is what
you need for a balance sheet; note this means it ignores report begin
dates (and `-T/--row-total`, since summing end balances generally does not make sense).

This command also supports
[output destination](hledger.html#output-destination) and
[output format](hledger.html#output-format) selection.
