## incomestatement

(is)

This command displays an
[income statement](http://en.wikipedia.org/wiki/Income_statement), 
showing revenues and expenses during one or more periods. 
Amounts are shown with normal positive sign, as in conventional
financial statements.

_FLAGS

This report shows accounts declared with the `Revenue` or `Expense` type
(see [account types](https://hledger.org/hledger.html#account-types)).
Or if no such accounts are declared, it shows top-level accounts named
`revenue` or `income` or `expense` (case insensitive, plurals allowed) and their subaccounts.

Example:
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

This command is a higher-level variant of the [`balance`](#balance) command,
and supports many of that command's features, such as multi-period reports.
It is similar to `hledger balance '(revenues|income)' expenses`,
but with smarter account detection, and revenues/income displayed with
their sign flipped.

This command also supports the
[output destination](hledger.html#output-destination) and
[output format](hledger.html#output-format) options
The output formats supported are
`txt`, `csv`, `html`, and (experimental) `json`.
