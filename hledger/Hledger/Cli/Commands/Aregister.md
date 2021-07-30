aregister, areg\

Show the transactions and running historical balance in an account,
with each line item representing one transaction.

_FLAGS

`aregister` shows the transactions affecting a particular account and
its subaccounts, with each line item representing a whole transaction -
as in bank statements, hledger-ui, hledger-web and other accounting apps.

This is unlike the [`register`](#register) command, 
which shows individual postings, and not necessarily from a single account.

Also, `aregister` always shows the historical running balance, which
includes any balance from transactions before the report start date. 
So `aregister` should always show the accurate real-world account balance
on each day. (Assuming opening balances are recorded correctly, and
allowing for date disagreements.)

As a quick rule of thumb,
use `aregister` for reconciling real-world asset/liability accounts
and `register` for reviewing detailed revenues/expenses.

`aregister` shows the register for just one account (and its subaccounts).
This account must be specified as the first argument. You can write either
the full account name, or a case-insensitive regular expression which will 
select the alphabetically first matched account.
(Eg if you have `assets:aaa:checking` and `assets:bbb:checking` accounts,
`hledger areg checking` would select `assets:aaa:checking`.)

Any additional arguments form a query which will filter the
transactions shown.

An example: this shows the transactions and historical running balance
during july in the first account whose name contains "checking":

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

`aregister` ignores a depth limit, so its final total will always match a balance report with similar arguments.

This command also supports the
[output destination](hledger.html#output-destination) and
[output format](hledger.html#output-format) options
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

