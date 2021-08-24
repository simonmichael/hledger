close, equity\
Prints a sample "closing" transaction bringing specified account balances to zero,
and an inverse "opening" transaction restoring the same account balances.

If like most people you split your journal files by time, eg by year:
at the end of the year you can use this command to "close out" your 
asset and liability (and perhaps equity) balances in the old file, and reinitialise them in the new file.
This helps ensure that report balances remain correct whether you are including old files or not.
(Because all closing/opening transactions except the very first will cancel out - see example below.)

Some people also use this command to close out revenue and expense balances at the end of an accounting period.
This properly records the period's profit/loss as "retained earnings" (part of equity),
and allows the accounting equation (A-L=E) to balance, 
which you could then check by the [bse](#balancesheetequity) report's zero total.

_FLAGS

You can print just the closing transaction by using the `--close` flag,
or just the opening transaction with the `--open` flag.

Their descriptions are `closing balances` and `opening balances` by default;
you can customise these with the `--close-desc` and `--open-desc` options.

Just one balancing equity posting is used by default, with the amount left implicit.
The default account name is `equity:opening/closing balances`.
You can customise the account name(s) with `--close-acct` and `--open-acct`.
(If you specify only one of these, it will be used for both.)

With `--x/--explicit`, the equity posting's amount will be shown explicitly,
and if it involves multiple commodities, there will be a separate equity posting for each commodity
(as in the print command).

With `--interleaved`, each equity posting is shown next to the posting it balances 
(good for troubleshooting).

### close and prices

Transaction prices are ignored (and discarded) by closing/opening transactions, by default.
With `--show-costs`, they are preserved;
there will be a separate equity posting for each cost in each commodity.
This means `balance -B` reports will look the same after the transition.
Note if you have many foreign currency or investment transactions,
this will generate very large journal entries.

### close date

The default closing date is yesterday, or the journal's end date, whichever is later.

Unless you are running `close` on exactly the first day of the new period, 
you'll want to override the closing date. 
This is done by specifying a [report end date](#report-start--end-date),
where "last day of the report period" will be the closing date.
The opening date is always the following day.
So to close on (end of) 2020-12-31 and open on (start of) 2021-01-01, any of these will work:

| end date argument | explanation
|-------------------|----------------------------------------------------------------------
| `-e 2021-01-01`   | [end dates](#report-start--end-date) are exclusive
| `-e 2021`         | equivalent, per [smart dates](#smart-dates) 
| `-p 2020`         | equivalent, the [period's](#period-expressions) begin date is ignored 
| `date:2020`       | equivalent [query](#queries)

### Example: close asset/liability accounts for file transition

Carrying asset/liability balances from 2020.journal into a new file for 2021:

```shell
$ hledger close -f 2020.journal -p 2020 assets liabilities
# copy/paste the closing transaction to the end of 2020.journal
# copy/paste the opening transaction to the start of 2021.journal
```

Or:

```shell
$ hledger close -f 2020.journal -p 2020 assets liabilities --open  >> 2021.journal  # add 2021's first transaction
$ hledger close -f 2020.journal -p 2020 assets liabilities --close >> 2020.journal  # add 2020's last transaction
```

Now,

```shell
$ hledger bs -f 2021.journal                   # just new file - balances correct
$ hledger bs -f 2020.journal -f 2021.journal   # old and new files - balances correct
$ hledger bs -f 2020.journal                   # just old files - balances are zero ?
                                               # (exclude final closing txn, see below)
```

### Hiding opening/closing transactions

Although the closing/opening transactions cancel out, they will be visible in reports like `print` and `register`, 
creating some visual clutter. You can exclude them all with a query, like:

```shell
$ hledger print not:desc:'opening|closing'             # less typing
$ hledger print not:'equity:opening/closing balances'  # more precise
```

But when reporting on multiple files, this can get a bit tricky;
you may need to keep the earliest opening balances, for a historical register report;
or you may need to suppress a closing transaction, to see year-end balances.
If you find yourself needing more precise [queries](#queries), here's one solution:
add more easily-matched tags to opening/closing transactions, like this:

```journal
; 2019.journal
2019-01-01 opening balances  ; earliest opening txn, no tag here
...
2019-12-31 closing balances  ; close:2019
...
```
```journal
; 2020.journal
2020-01-01 opening balances  ; open:2020
...
2020-12-31 closing balances  ; close:2020
...
```
```journal
; 2021.journal
2021-01-01 opening balances  ; open:2021
...
```

Now with
```journal
; all.journal
include 2019.journal
include 2020.journal
include 2021.journal
```
you could do eg:
```shell
$ hledger -f all.journal reg -H checking not:tag:'open|close'
    # all years checking register, hiding non-essential opening/closing txns

$ hledger -f all.journal bs -p 2020 not:tag:close=2020
    # 2020 year end balances, suppressing 2020 closing txn

$ hledger -f 2020.journal bs not:tag:close
    # 2020 year end balances, easier case

```

### close and balance assertions

The closing and opening transactions will include balance assertions, 
verifying that the accounts have first been reset to zero and then restored to their previous balance.
These provide valuable error checking, alerting you when things get out of line,
but you can ignore them temporarily with `-I` or just remove them if you prefer.

You probably shouldn't use status or realness filters (like -C or -R or `status:`) with `close`, 
or the generated balance assertions will depend on these flags.
Likewise, if you run this command with `--auto`, the balance assertions would probably always require `--auto`.

Multi-day transactions (where some postings have a different date) break the balance assertions,
because the money is temporarily "invisible" while in transit:

```journal
2020/12/30 a purchase made in december, cleared in the next year
    expenses:food          5
    assets:bank:checking  -5  ; date: 2021/1/2
```

To fix the assertions, you can add a temporary account to track such in-transit money
(splitting the multi-day transaction into two single-day transactions):

```journal
; in 2020.journal:
2020/12/30 a purchase made in december, cleared in the next year
    expenses:food          5
    liabilities:pending

; in 2021.journal:
2021/1/2 clearance of last year's pending transactions
    liabilities:pending    5 = 0
    assets:bank:checking
```

### Example: close revenue/expense accounts to retained earnings

Here, the opening transaction is supressed with `--close`, as it's probably not needed.
Also you'll want to use a different equity account name:
 
```shell
$ hledger close -f 2021.journal -p 2021Q1 --close --close-acct='equity:retained earnings' revenues expenses >> 2021.journal
    # close 2021 first quarter revenues/expenses
```

Or, operating on the default journal:

```shell
$ hledger close -p Q1 --close --close-acct='equity:retained earnings' revenues expenses >> $LEDGER_FILE
    # close current year's first quarter revenues/expenses
```


Now, eg:

```shell
$ hledger bse -p Q1
    # Q1 full balance sheet, total should be zero

$ hledger is -p Q1 not:'retained earnings'
    # Q1 income statement, must suppress the closing txn
```
