## aregister

(areg)

Show the transactions and running balances in one account,
with each transaction on one line.

```flags
Flags:
     --txn-dates            filter strictly by transaction date, not posting
                            date. Warning: this can show a wrong running
                            balance.
     --no-elide             don't show only 2 commodities per amount
     --cumulative           show running total from report start date
     --no-header            omit header row in table output
  -w --width=N              set output width (default: terminal width or
                            $COLUMNS). -wN,M sets description width as well.
     --align-all            guarantee alignment across all lines (slower)
  -O --output-format=FMT    select the output format. Supported formats:
                            txt, html, csv, tsv, json.
  -o --output-file=FILE     write output to FILE. A file extension matching
                            one of the above formats selects that format.
```

`aregister` shows the overall transactions affecting a particular account (and
any subaccounts). Each report line represents one transaction in this account.
Transactions before the report start date are included in the running balance
(`--historical` mode is the default).
You can suppress this behaviour using the `--cumulative` option.

This is a more "real world", bank-like view than the [`register`](#register) 
command (which shows individual postings, possibly from multiple accounts,
not necessarily in historical mode). 
As a quick rule of thumb:
- use `aregister` for reviewing and reconciling real-world asset/liability accounts
- use `register` for reviewing detailed revenues/expenses.

`aregister` requires one argument: the account to report on.
You can write either the full account name, or a case-insensitive regular expression 
which will select the alphabetically first matched account.

When there are multiple matches, the alphabetically-first choice can be surprising; 
eg if you have `assets:per:checking 1` and `assets:biz:checking 2` accounts,
`hledger areg checking` would select `assets:biz:checking 2`.
It's just a convenience to save typing, so if in doubt, write the full account name,
or a distinctive substring that matches uniquely.

Transactions involving subaccounts of this account will also be shown.
`aregister` ignores depth limits, so its final total will always match 
a balance report with similar arguments.

Any additional arguments form a [query](#queries) which will filter the
transactions shown. Note some queries will disturb the running balance,
causing it to be different from the account's real-world running balance.

An example: this shows the transactions and historical running balance
during july, in the first account whose name contains "checking":

```cli
$ hledger areg checking date:jul
```

Each `aregister` line item shows:

- the transaction's date (or the relevant posting's date if different, see below)
- the names of all the other account(s) involved in this transaction (probably abbreviated)
- the total change to this account's balance from this transaction
- the account's historical running balance after this transaction.

Transactions making a net change of zero are not shown by default;
add the `-E/--empty` flag to show them.

For performance reasons, column widths are chosen based on the first 1000 lines;
this means unusually wide values in later lines can cause visual discontinuities
as column widths are adjusted. If you want to ensure perfect alignment, 
at the cost of more time and memory, use the `--align-all` flag.

By default, `aregister` shows a header above the data.
However, when reporting in a language different from English,
it is easier to omit this header and prepend your own one.
For this purpose, use the `--no-header` option.

This command also supports the
[output destination](hledger.html#output-destination) and
[output format](hledger.html#output-format) options.
The output formats supported are `txt`, `csv`, `tsv` (*Added in 1.32*), `html`, `fods` (*Added in 1.41*) and `json`.

### aregister and posting dates

aregister always shows one line (and date and amount) per transaction.
But sometimes transactions have postings with different dates.  Also,
not all of a transaction's postings may be within the report period.
To resolve this, aregister shows the earliest of the transaction's
date and posting dates that is in-period, and the sum of the in-period
postings.  In other words it will show a combined line item with just
the earliest date, and the running balance will (temporarily, until
the transaction's last posting) be inaccurate. Use `register -H` if
you need to see the individual postings.

There is also a `--txn-dates` flag, which filters strictly by
transaction date, ignoring posting dates. This too can cause an
inaccurate running balance.

