cashflow, cf\
This command displays a simple cashflow statement, showing changes
in "cash" accounts. It assumes that these accounts are under a top-level
`asset` account (case insensitive, plural forms also allowed) and do not
contain `receivable` or `A/R` in their name.
Note this report shows all account balances with normal positive sign
(like conventional financial statements, unlike balance/print/register)
(experimental).

_FLAGS_

Example:
```shell
$ hledger cashflow
Cashflow Statement

Cash flows:
                 $-1  assets
                  $1    bank:saving
                 $-2    cash
--------------------
                 $-1

Total:
--------------------
                 $-1
```

With a [reporting interval](#reporting-interval), multiple columns
will be shown, one for each report period.
Normally cashflow shows changes in assets per period, though
as with [multicolumn balance reports](#multicolumn-balance-reports)
you can alter the report mode with `--change`/`--cumulative`/`--historical`.

This command also supports
[output destination](hledger.html#output-destination) and
[output format](hledger.html#output-format) selection.
