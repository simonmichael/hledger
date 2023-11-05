## balancesheetequity

(bse)

This command displays a [balance sheet](https://en.wikipedia.org/wiki/Balance_sheet), 
showing historical ending balances of asset, liability and equity accounts.
Amounts are shown with normal positive sign, as in conventional
financial statements.

_FLAGS

This report shows accounts declared with the `Asset`, `Cash`, `Liability` or `Equity` type
(see [account types](https://hledger.org/hledger.html#account-types)).
Or if no such accounts are declared, it shows top-level accounts named
`asset`, `liability` or `equity` (case insensitive, plurals allowed) and their subaccounts.

Example:
```shell
$ hledger balancesheetequity
Balance Sheet With Equity

Assets:
                 $-2  assets
                  $1    bank:saving
                 $-3    cash
--------------------
                 $-2

Liabilities:
                  $1  liabilities:debts
--------------------
                  $1

Equity:
		  $1  equity:owner
--------------------
		  $1

Total:
--------------------
                   0
```

This command is a higher-level variant of the [`balance`](#balance) command,
and supports many of that command's features, such as multi-period reports.
It is similar to `hledger balance -H assets liabilities equity`,
but with smarter account detection, and liabilities/equity displayed with
their sign flipped.

This command also supports the
[output destination](hledger.html#output-destination) and
[output format](hledger.html#output-format) options
The output formats supported are
`txt`, `csv`, `tsv`, `html`, and (experimental) `json`.
