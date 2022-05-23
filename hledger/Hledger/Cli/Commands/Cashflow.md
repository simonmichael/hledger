cashflow, cf\
This command displays a cashflow statement, showing the inflows and
outflows affecting "cash" (ie, liquid, easily convertible) assets.
Amounts are shown with normal positive sign, as in conventional
financial statements.

_FLAGS

"Cash" assets are those accounts which are (or whose parents are) 
declared as `Cash` by an account directive, like this:

```journal
account some:liquid:asset    ; type:C
```

Or if there are no such declarations, all accounts

- under a top-level `asset` account (case insensitive, plural allowed) 
- with some variation of `cash`, `bank`, `checking` or `saving` in their name.

More precisely: all accounts matching this case insensitive regular expression:

`^assets?(:.+)?:(cash|bank|che(ck|que?)(ing)?|savings?|currentcash)(:|$)`

and their subaccounts.

An example cashflow report:
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

This command is a higher-level variant of the [`balance`](#balance) command,
and supports many of that command's features, such as multi-period reports.
It is similar to `hledger balance assets not:fixed not:investment not:receivable`,
but with smarter account detection.

This command also supports the
[output destination](hledger.html#output-destination) and
[output format](hledger.html#output-format) options
The output formats supported are
`txt`, `csv`, `html`, and (experimental) `json`.
