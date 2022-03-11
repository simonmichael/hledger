aregister, areg\

Show the transactions and running historical balance of a single account,
with each transaction displayed as one line.

_FLAGS

`aregister` shows the overall transactions affecting a particular account (and
any subaccounts). Each report line represents one transaction in this account.
Transactions before the report start date are always included in the running balance
(`--historical` mode is always on).

This is a more "real world", bank-like view than the [`register`](#register) 
command (which shows individual postings, possibly from multiple accounts,
not necessarily in historical mode). 
As a quick rule of thumb:
- use `aregister` for reviewing and reconciling real-world asset/liability accounts
- use `register` for reviewing detailed revenues/expenses.

`aregister` requires one argument: the account to report on.
You can write either the full account name, or a case-insensitive regular expression 
which will select the alphabetically first matched account.
(Eg if you have `assets:aaa:checking` and `assets:bbb:checking` accounts,
`hledger areg checking` would select `assets:aaa:checking`.)

Transactions involving subaccounts of this account will also be shown.
`aregister` ignores depth limits, so its final total will always match 
a balance report with similar arguments.

Any additional arguments form a [query](#queries) which will filter the
transactions shown. Note some queries will disturb the running balance,
causing it to be different from the account's real-world running balance.

An example: this shows the transactions and historical running balance
during july, in the first account whose name contains "checking":

```shell
$ hledger areg checking date:jul
```

Each `aregister` line item shows:

- the transaction's date (or the relevant posting's date if different, see below)
- the names of all the other account(s) involved in this transaction (probably abbreviated)
- the total change to this account's balance from this transaction
- the account's historical running balance after this transaction.

Transactions making a net change of zero are not shown by default;
add the `-E/--empty` flag to show them.

For performance reasons, column widths are chosen based on the first 100 lines;
this means unusually wide values in later lines can cause visual discontinuities
as column widths are adjusted. If you want to ensure perfect alignment, 
at the cost of more time and memory, use the `--align-all` flag.

This command also supports the
[output destination](hledger.html#output-destination) and
[output format](hledger.html#output-format) options.
The output formats supported are `txt`, `csv`, and `json`.

### aregister and custom posting dates

Transactions whose date is outside the report period can still be
shown, if they have a posting to this account dated inside the report
period. (And in this case it's the posting date that is shown.) 
This ensures that `aregister` can show an accurate historical running
balance, matching the one shown by `register -H` with the same
arguments.

To filter strictly by transaction date instead, add the `--txn-dates`
flag. If you use this flag and some of your postings have custom
dates, it's probably best to assume the running balance is wrong.

