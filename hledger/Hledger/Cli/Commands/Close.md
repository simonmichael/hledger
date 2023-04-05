## close

(equity)

`close [--open | --migrate | --retain] [QUERY]`

Transfer balances to and/or from another account (usually equity).
Useful for migrating balances to a new journal file, 
or for merging earnings into equity at end of accounting period.

By default, it prints a "closing balances" transaction that zeroes out
all accounts, transferring their balances to `equity:opening/closing balances`.

_FLAGS

This command is useful in several situations.
It has four main modes, corresponding to the most common use cases:

By default, it prints a "closing balances" transaction that zeroes out all accounts.

With `--open`, it instead prints an "opening balances" transaction that restores the balances
of asset, liability and most equity accounts. This is similar to Ledger's equity command,
and could be useful for propagating balances to a new file.

With `--migrate`, it prints both the closing and opening transactions,
for asset, liability and most equity accounts.
This is the preferred way to migrate balances to a new file:
the opening transaction should be inserted at the start of the new file,
and the closing transaction should be added at the end of the old file.
Now, the files can be combined for multi-file reporting, without disturbing balances
(because the redundant closing/opening transactions cancel each other out).

With `--retain`, it prints a "retain earnings" transaction that transfers
revenue and expense balances to `equity:retained earnings`.
Businesses traditionally do this at the end of each accounting period;
it is less necessary with computer-based accounting, but it could still be useful
if you want to see the accounting equation A=L+E satisfied.

In all modes, those defaults can be overridden:

- the transaction descriptions can be changed with `--close-desc=DESC` and `--open-desc=DESC`
- the account to transfer to/from can be changed with `--close-acct=ACCT`
- the accounts to be closed/opened can be changed with `QUERY` (an account query).

By default just one equity posting, with an implicit amount, will be used.
With `--x/--explicit` the amount will be shown explicitly,
and if it involves multiple commodities, a separate posting
will be generated for each commodity.

With `--interleaved`, each equity posting is shown next to the 
corresponding source/destination posting.

The default closing date is yesterday, or the journal's end date, whichever is later.
You can change this by specifying a [report end date](#report-start--end-date);
(The report start date does not matter.)
The last day of the report period will be the closing date;
eg `-e 2022` means "close on 2022-12-31".
The opening date is always the day after the closing date.

### close and costs

With `--show-costs`, any amount costs are shown, with separate postings for each cost.
(This currently the best way to view investment assets, showing lots and cost bases.)
If you have many currency conversion or investment transactions, it can generate very large journal entries.

### close and balance assertions

Balance assertions will be generated, verifying that the accounts have been reset to zero
(and then restored to their previous balances, if there is an opening transaction).

These provide useful error checking, but you can ignore them temporarily with `-I`,
or remove them if you prefer.

You probably should avoid filtering transactions by status or realness
(`-C`, `-R`, `status:`), or generating postings (`--auto`),
with this command, since the balance assertions would depend on these.

Note custom posting dates spanning the file boundary will disrupt the balance assertions:

```journal
2023-12-30 a purchase made in december, cleared in january
    expenses:food          5
    assets:bank:checking  -5  ; date: 2023-01-02
```

To solve that you can transfer the money to and from a temporary account,
in effect splitting the multi-day transaction into two single-day transactions:

```journal
; in 2022.journal:
2022-12-30 a purchase made in december, cleared in january
    expenses:food          5
    equity:pending        -5

; in 2023.journal:
2023-01-02 last year's transaction cleared
    equity:pending         5 = 0
    assets:bank:checking  -5
```

### Example: retain earnings

<!-- XXX update -->

Record 2022's revenues/expenses as retained earnings on 2022-12-31,
appending the generated transaction to the journal:
 
```shell
$ hledger close --retain -f 2022.journal -p 2022 >> 2022.journal
```

Now 2022's income statement will show only zeroes.
To see it again, exclude the retain transaction. Eg:
```shell
$ hledger -f 2022.journal is not:desc:'retain earnings'
```

### Example: migrate balances to a new file

Close assets/liabilities/equity on 2022-12-31 and re-open them on 2023-01-01:

```shell
$ hledger close --migrate -f 2022.journal -p 2022
# copy/paste the closing transaction to the end of 2022.journal
# copy/paste the opening transaction to the start of 2023.journal
```

<!--
Or, you can automate more by generating one transaction at a time:

```shell
$ hledger close --close -f 2022.journal -p 2022 >> 2023.journal  # do this one first
$ hledger close --open  -f 2022.journal -p 2022 >> 2022.journal
```
-->

Now 2022's balance sheet will show only zeroes, indicating a balanced accounting equation.
([Unless](/investments.html#a-more-correct-entry) you are using @/@@ notation - in that case, try adding --infer-equity.)
To see it again, exclude the closing transaction. Eg:
```shell
$ hledger -f 2022.journal bs not:desc:'closing balances'
```

### Example: excluding closing/opening transactions

When combining many files for multi-year reports, 
the closing/opening transactions cause some noise in reports like `print` and `register`.
You can exclude them as shown above, but `not:desc:...` could be fragile,
and also you will need to avoid excluding the very first opening transaction,
which can be awkward. Here is a way to do it, using tags:
add `clopen:` tags to all opening/closing balances transactions except the first,
like this:

```journal
; 2021.journal
2021-06-01 first opening balances
...
2021-12-31 closing balances  ; clopen:2022
...
```

```journal
; 2022.journal
2022-01-01 opening balances  ; clopen:2022
...
2022-12-31 closing balances  ; clopen:2023
...
```
```journal
; 2023.journal
2023-01-01 opening balances  ; clopen:2023
...
```

Now, assuming a combined journal like:

```journal
; all.journal
include 2021.journal
include 2022.journal
include 2023.journal
```

The `clopen:` tag can exclude all but the first opening transaction.
To show a clean multi-year checking register:
```shell
$ hledger -f all.journal areg checking not:tag:clopen
```

And the year values allow more precision.
To show 2022's year-end balance sheet:
```shell
$ hledger -f all.journal bs -e2023 not:tag:clopen=2023
```
