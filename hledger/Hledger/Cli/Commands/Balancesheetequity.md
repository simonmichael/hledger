## balancesheetequity

(bse)

This command displays a [balance sheet](https://en.wikipedia.org/wiki/Balance_sheet), 
showing historical ending balances of asset, liability and equity accounts.
Amounts are shown with normal positive sign, as in conventional
financial statements.

```flags
Flags:
     --sum                  show sum of posting amounts (default)
     --valuechange          show total change of period-end historical
                            balance value (caused by deposits, withdrawals,
                            market price fluctuations)
     --gain                 show unrealised capital gain/loss (historical
                            balance value minus cost basis)
     --count                show the count of postings
     --change               accumulate amounts from column start to column
                            end (in multicolumn reports)
     --cumulative           accumulate amounts from report start (specified
                            by e.g. -b/--begin) to column end
  -H --historical           accumulate amounts from journal start to column
                            end (includes postings before report start date)
                            (default)
  -l --flat                 show accounts as a flat list (default). Amounts
                            exclude subaccount amounts, except where the
                            account is depth-clipped.
  -t --tree                 show accounts as a tree. Amounts include
                            subaccount amounts.
     --drop=N               flat mode: omit N leading account name parts
     --declared             include non-parent declared accounts (best used
                            with -E)
  -A --average              show a row average column (in multicolumn
                            reports)
  -T --row-total            show a row total column (in multicolumn reports)
     --summary-only         display only row summaries (e.g. row total,
                            average) (in multicolumn reports)
  -N --no-total             omit the final total row
     --no-elide             don't squash boring parent accounts (in tree
                            mode)
     --format=FORMATSTR     use this custom line format (in simple reports)
  -S --sort-amount          sort by amount instead of account code/name
  -% --percent              express values in percentage of each column's
                            total
     --layout=ARG           how to show multi-commodity amounts:
                            'wide[,WIDTH]': all commodities on one line
                            'tall'        : each commodity on a new line
                            'bare'        : bare numbers, symbols in a column
     --base-url=URLPREFIX   in html output, generate hyperlinks to
                            hledger-web, with this prefix. (Usually the base
                            url shown by hledger-web; can also be relative.)
  -O --output-format=FMT    select the output format. Supported formats:
                            txt, html, csv, tsv, json.
  -o --output-file=FILE     write output to FILE. A file extension matching
                            one of the above formats selects that format.
```

This report shows accounts declared with the `Asset`, `Cash`, `Liability` or `Equity` type
(see [account types](https://hledger.org/hledger.html#account-types)).
Or if no such accounts are declared, it shows top-level accounts named
`asset`, `liability` or `equity` (case insensitive, plurals allowed) and their subaccounts.

Example:
```cli
$ hledger balancesheetequity
Balance Sheet With Equity 2008-12-31

                    || 2008-12-31 
====================++============
 Assets             ||            
--------------------++------------
 assets:bank:saving ||         $1 
 assets:cash        ||        $-2 
--------------------++------------
                    ||        $-1 
====================++============
 Liabilities        ||            
--------------------++------------
 liabilities:debts  ||        $-1 
--------------------++------------
                    ||        $-1 
====================++============
 Equity             ||            
--------------------++------------
--------------------++------------
                    ||          0 
====================++============
 Net:               ||          0 
```

This command is a higher-level variant of the [`balance`](#balance) command,
and supports many of that command's features, such as multi-period reports.
It is similar to `hledger balance -H assets liabilities equity`,
but with smarter account detection, and liabilities/equity displayed with
their sign flipped.

This report is the easiest way to see if the [accounting equation] (A+L+E = 0) is satisfied
(after you have done a [`close --retain`](#close---retain) to merge revenues and expenses with equity,
and perhaps added [`--infer-equity`](#inferring-equity-conversion-postings) to balance your commodity conversions).

This command also supports the
[output destination](hledger.html#output-destination) and
[output format](hledger.html#output-format) options
The output formats supported are
`txt`, `csv`, `tsv`, `html`, and `json`.

[accounting equation]: https://plaintextaccounting.org/FAQ#what-is-the-accounting-equation
