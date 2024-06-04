## close

(equity)

`close` generates several kinds of "closing" and/or "opening" transactions, useful in certain situations, including
migrating balances to a new journal file, retaining earnings into equity, consolidating balances, or viewing lots.
Like `print`, it prints valid journal entries.
You can append or copy these to your journal file(s) when you are happy with how they look.

```flags
Flags:
     --migrate[=NEW]        show closing and opening transactions, for Asset
                            and Liability accounts by default, tagged for easy
                            matching. The tag's default value can be overridden
                            by providing NEW.
     --close[=NEW]          (default) show a closing transaction
     --open[=NEW]           show an opening transaction
     --assign[=NEW]         show opening balance assignments
     --assert[=NEW]         show closing balance assertions
     --retain[=NEW]         show a retain earnings transaction, for Revenue
                            and Expense accounts by default
  -x --explicit             show all amounts explicitly
     --show-costs           show amounts with different costs separately
     --interleaved          show source and destination postings together
     --assertion-type=TYPE  =, ==, =* or ==*
     --close-desc=DESC      set closing transaction's description
     --close-acct=ACCT      set closing transaction's destination account
     --open-desc=DESC       set opening transaction's description
     --open-acct=ACCT       set opening transaction's source account
     --round=TYPE           how much rounding or padding should be done when
                            displaying amounts ?
                            none - show original decimal digits,
                                   as in journal
                            soft - just add or remove decimal zeros
                                   to match precision (default)
                            hard - round posting amounts to precision
                                   (can unbalance transactions)
                            all  - also round cost amounts to precision
                                   (can unbalance transactions)
```

`close` currently has six modes, selected by a single mode flag:

### close --migrate

This is the most common mode.
It prints a "closing balances" transaction that zeroes out all asset and liability balances (by default),
and an opposite "opening balances" transaction that restores them again.
The balancing account will be `equity:opening/closing balances` (or another specified by `--close-acct` or `--open-acct`).

This is useful when migrating balances to a new journal file at the start of a new year.
Essentially, you run `hledger close --migrate=NEWYEAR -e NEWYEAR`
and then copy the closing transaction to the end of the old file
and the opening transaction to the start of the new file.
The opening transaction sets correct starting balances in the new file when it is used alone,
and the closing transaction keeps balances correct when you use both old and new files together,
by cancelling out the following opening transaction and preventing buildup of duplicated opening balances.
Think of the closing/opening pair as "moving the balances into the next file".

You can close a different set of accounts by providing a query.
Eg if you want to include equity, you can add `assets liabilities equity` or [`type:ALE`](hledger.md#account-types) arguments.
(The balancing account is always excluded.)
Revenues and expenses usually are not migrated to a new file directly; see `--retain` below.

The generated transactions will have a `start:` tag, with its value set to 
`--migrate`'s `NEW` argument if any, for easier matching or exclusion.
When `NEW` is not specified, it will be inferred if possible by incrementing
a number (eg a year number) within the default journal's main file name.
The other modes behave similarly.

### close --close

This prints just the closing balances transaction of `--migrate`.
It is the default behaviour if you specify no mode flag.
Using the customisation options below, you can move balances from any set of accounts to a different account.

### close --open

This prints just the opening balances transaction of `--migrate`.
It is similar to [Ledger's equity command](https://ledger-cli.org/doc/ledger3.html#The-equity-command).

### close --assert

This prints a "closing balances" transaction (with `balances:` tag), 
that just declares [balance assertions](#balance-assertions) for the current balances without changing them.
It could be useful as documention and to guard against changes.

### close --assign

This prints an "opening balances" transaction that
restores the account balances using [balance assignments](#balance-assignments).
Balance assignments work regardless of any previous balance, so a preceding closing balances transaction is not needed.

However, omitting the closing balances transaction would unbalance equity.
This is relatively harmless for personal reports, but it disturbs the accounting equation, removing a source of error detection.
So `--migrate` is generally the best way to set to set balances in new files, [for now](https://github.com/simonmichael/hledger/issues/2151).

### close --retain

This is like `--close` with different defaults:
it prints a "retain earnings" transaction (with `retain:` tag), 
that transfers revenue and expense balances to `equity:retained earnings`.

This is a different kind of closing, called "retaining earnings" or "closing the books";
it is traditionally performed by businesses at the end of each accounting period,
to consolidate revenues and expenses into the main equity balance.
("Revenues" and "expenses" are actually equity by another name, kept separate temporarily for reporting purposes.)

In personal accounting you generally don't need to do this,
unless you want the `balancesheetequity` report to show a zero total, demonstrating that the accounting equation (A-L=E) is satisfied.

### close customisation

In all modes, the following things can be overridden:

- the accounts to be closed/opened, with account query arguments
- the balancing account, with `--close-acct=ACCT` and/or `--open-acct=ACCT`
- the transaction descriptions, with `--close-desc=DESC` and `--open-desc=DESC`
- the transaction's tag value, with a `--MODE=NEW` option argument
- the closing/opening dates, with `-e OPENDATE`

By default, the closing date is yesterday, or the journal's end date, whichever is later;
and the opening date is always one day after the closing date.
You can change these by specifying a [report end date](#report-start--end-date);
the closing date will be the last day of the report period.
Eg `-e 2024` means "close on 2023-12-31, open on 2024-01-01".

With `--x/--explicit`, the balancing amount will be shown explicitly,
and if it involves multiple commodities, a separate posting will be generated for each of them
(similar to `print -x`).

With `--interleaved`, each individual transfer is shown with source and destination postings next to each other
(perhaps useful for troubleshooting).

With `--show-costs`, balances' costs are also shown, with different costs kept separate.
This may generate very large journal entries, if you have many currency conversions or investment transactions.
`close --show-costs` is currently the best way to view investment lots with hledger.
(To move or dispose of lots, see the more capable [`hledger-move`](/scripts.md#hledger-move) script.)

### close and balance assertions

`close` adds balance assertions verifying
that the accounts have been reset to zero in a closing transaction
or restored to their previous balances in an opening transaction.
These provide useful error checking, but you can ignore them temporarily with `-I`,
or remove them if you prefer.

Single-commodity, subaccount-exclusive balance assertions (`=`) are generated by default.
This can be changed with `--assertion-type='==*'` (eg).

When running `close` you should probably avoid using `-C`, `-R`, `status:` (filtering by status or realness)
or `--auto` (generating postings), since the generated balance assertions would then require these.

Transactions with multiple dates (eg posting dates) spanning the file boundary
also can disrupt the balance assertions:

```journal
2023-12-30 a purchase made in december, cleared in january
    expenses:food          5
    assets:bank:checking  -5  ; date: 2023-01-02
```

To solve this you can transfer the money to and from a temporary account,
splitting the multi-day transaction into two single-day transactions:

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

### close examples

#### Retain earnings

Record 2022's revenues/expenses as retained earnings on 2022-12-31,
appending the generated transaction to the journal:
 
```cli
$ hledger close --retain -f 2022.journal -p 2022 >> 2022.journal
```

After this, to see 2022's revenues and expenses you must exclude the retain earnings transaction:

```cli
$ hledger -f 2022.journal is not:desc:'retain earnings'
```

#### Migrate balances to a new file

Close assets/liabilities on 2022-12-31 and re-open them on 2023-01-01:

```cli
$ hledger close --migrate -f 2022.journal -p 2022
# copy/paste the closing transaction to the end of 2022.journal
# copy/paste the opening transaction to the start of 2023.journal
```

After this, to see 2022's end-of-year balances you must exclude the closing balances transaction:

```cli
$ hledger -f 2022.journal bs not:desc:'closing balances'
```

For more flexibility, it helps to tag closing and opening transactions with eg `start:NEWYEAR`,
then you can ensure correct balances by excluding all opening/closing transactions except the first, like so:

```cli
$ hledger bs -Y -f 2021.j -f 2022.j -f 2023.j expr:'tag:start=2021 or not tag:start'
$ hledger bs -Y -f 2021.j -f 2022.j           expr:'tag:start=2021 or not tag:start'
$ hledger bs -Y -f 2022.j -f 2023.j           expr:'tag:start=2022 or not tag:start'
$ hledger bs -Y -f 2021.j                     expr:'tag:start=2021 or not tag:start'
$ hledger bs -Y -f 2022.j                     expr:'tag:start=2022 or not tag:start'
$ hledger bs -Y -f 2023.j                     # unclosed file, no query needed
```

#### More detailed close examples

See [examples/multi-year](https://github.com/simonmichael/hledger/tree/master/examples/multi-year/).
