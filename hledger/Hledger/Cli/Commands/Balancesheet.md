balancesheet, bs\
This command displays a balance sheet, showing historical ending
balances of asset and liability accounts. (To see equity as well,
use the [balancesheetequity](#balancesheetequity) command.)
Amounts are shown with normal positive sign, as in conventional
financial statements.

_FLAGS

The asset and liability accounts shown are those accounts declared
with the `Asset` or `Cash` or `Liability` type, or otherwise all
accounts under a top-level `asset` or `liability` account (case
insensitive, plurals allowed).

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
Instead of absolute values [percentages](#percentages) can be displayed
with `-%`.

This command also supports the
[output destination](hledger.html#output-destination) and
[output format](hledger.html#output-format) options
The output formats supported are
`txt`, `csv`, `html`, and (experimental) `json`.
