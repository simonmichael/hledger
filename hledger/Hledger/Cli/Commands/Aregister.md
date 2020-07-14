aregister, areg\
Show transactions affecting a particular account, and the account's
running balance.

_FLAGS

`aregister` shows the transactions affecting a particular account
(and its subaccounts), from the point of view of that account. 
Each line shows:

- the transaction's (or posting's, see below) date
- the names of the other account(s) involved
- the net change to this account's balance
- the account's historical running balance (including balance from
  transactions before the report start date).

With `aregister`, each line represents a whole transaction - as in
hledger-ui, hledger-web, and your bank statement. By contrast, the
`register` command shows individual postings, across all accounts.
You might prefer `aregister` for reconciling with real-world
asset/liability accounts, and `register` for reviewing detailed
revenues/expenses.

An account must be specified as the first argument, which should be
the full account name or an account pattern (regular expression).
aregister will show transactions in this account (the first one
matched) and any of its subaccounts.

Any additional arguments form a query which will filter the
transactions shown.

Transactions making a net change of zero are not shown by default;
add the `-E/--empty` flag to show them.

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

### Output format

This command also supports the
[output destination](hledger.html#output-destination) and
[output format](hledger.html#output-format) options
The output formats supported are `txt`, `csv`, and `json`.

Examples:

Show all transactions and historical running balance in the first
account whose name contains "checking":
```shell
$ hledger areg checking
```

Show transactions and historical running balance in all asset accounts
during july:
```shell
$ hledger areg assets date:jul
```
