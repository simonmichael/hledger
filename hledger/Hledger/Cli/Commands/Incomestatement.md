incomestatement, is\
This command displays a simple income statement, showing revenues
and expenses during a period. It assumes that these accounts are under a
top-level `revenue` or `income` or `expense` account (case insensitive,
plural forms also allowed).
Note this report shows all account balances with normal positive sign
(like conventional financial statements, unlike balance/print/register)
(experimental).

_FLAGS_

This command displays a simple
[income statement](http://en.wikipedia.org/wiki/Income_statement).  It
currently assumes that you have top-level accounts named `income` (or
`revenue`) and `expense` (plural forms also allowed.)

```shell
$ hledger incomestatement
Income Statement

Revenues:
                 $-2  income
                 $-1    gifts
                 $-1    salary
--------------------
                 $-2

Expenses:
                  $2  expenses
                  $1    food
                  $1    supplies
--------------------
                  $2

Total:
--------------------
                   0
```

With a [reporting interval](#reporting-interval), multiple columns
will be shown, one for each report period.
Normally incomestatement shows revenues/expenses per period, though
as with [multicolumn balance reports](#multicolumn-balance-reports)
you can alter the report mode with `--change`/`--cumulative`/`--historical`.

This command also supports
[output destination](hledger.html#output-destination) and
[output format](hledger.html#output-format) selection.
