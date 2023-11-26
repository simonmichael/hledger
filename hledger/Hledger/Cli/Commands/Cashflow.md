## cashflow

(cf)

This command displays a [cashflow statement](https://en.wikipedia.org/wiki/Cash_flow_statement), 
showing the inflows and outflows affecting "cash" (ie, liquid, easily convertible) assets.
Amounts are shown with normal positive sign, as in conventional
financial statements.

_FLAGS

This report shows accounts declared with the `Cash` type
(see [account types](https://hledger.org/hledger.html#account-types)).
Or if no such accounts are declared, it shows accounts 

- under a top-level account named `asset` (case insensitive, plural allowed) 
- whose name contains some variation of `cash`, `bank`, `checking` or `saving`.

More precisely: all accounts matching this case insensitive regular expression:

`^assets?(:.+)?:(cash|bank|che(ck|que?)(ing)?|savings?|currentcash)(:|$)`

and their subaccounts.

An example cashflow report:
```cli
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

This command is a higher-level variant of the [`balance`](#balance) command,
and supports many of that command's features, such as multi-period reports.
It is similar to `hledger balance assets not:fixed not:investment not:receivable`,
but with smarter account detection.

This command also supports the
[output destination](hledger.html#output-destination) and
[output format](hledger.html#output-format) options
The output formats supported are
`txt`, `csv`, `tsv`, `html`, and (experimental) `json`.
