## close

(equity)

`close` prints several kinds of "closing" and/or "opening" transactions, useful in various situations:
migrating balances to a new journal file, retaining earnings into equity, consolidating balances, viewing lot costs..
Like `print`, it prints valid journal entries.
You can copy these into your journal file(s) when you are happy with how they look.

```flags
Flags:
     --clopen[=TAGVAL]      show closing and opening balances transactions,
                            for AL accounts by default
     --close[=TAGVAL]       show just a closing balances transaction
     --open[=TAGVAL]        show just an opening balances transaction
     --assert[=TAGVAL]      show a balance assertions transaction
     --assign[=TAGVAL]      show a balance assignments transaction
     --retain[=TAGVAL]      show a retain earnings transaction, for RX
                            accounts by default
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
                                   as in journal (default)
                            soft - just add or remove decimal zeros
                                   to match precision
                            hard - round posting amounts to precision
                                   (can unbalance transactions)
                            all  - also round cost amounts to precision
                                   (can unbalance transactions)
```

`close` has six modes, selected by choosing one of the mode flags (`--close` is the default).
They all do much the same operation, but with different defaults, useful in different situations.

### close --clopen

This is useful if migrating balances to a new journal file at the start of a new year.
It prints a "closing balances" transaction that zeroes out account balances (Asset and Liability accounts, by default),
and an opposite "opening balances" transaction that restores them again.
Typically, you would run
```
hledger close --clopen -e NEWYEAR >> $LEDGER_FILE
```
and then move the opening transaction from the old file to the new file
(and probably also update your LEDGER_FILE environment variable).

Why might you do this ? If your reports are fast, you may not need it.
But at some point you will probably want to partition your data by time,
for performance or data integrity or regulatory reasons.
A new file or set of files per year is common.
Then, having each file/fileset "bookended" with opening and closing balance transactions
will allow you to freely pick and choose which files to read -
just the current year, any past year, any sequence of years, or all of them -
while showing correct account balances in each case.
The earliest opening balances transaction sets correct starting balances,
and any later closing/opening pairs will harmlessly cancel each other out.

The balances will be transferred to and from `equity:opening/closing balances` by default.
You can override this by using `--close-acct` and/or `--open-acct`.

You can select a different set of accounts to close/open by providing an account query.
Eg to add Equity accounts, provide arguments like `assets liabilities equity` or `type:ALE`.
When migrating to a new file, you'll usually want to bring along the AL or ALE accounts,
but not the RX accounts (Revenue, Expense).

Assertions will be added indicating and checking the new balances of the closed/opened accounts.

The generated transactions will have a `clopen:` tag.
If the main journal's base file name contains a number (eg a year number),
the tag's value will be that base file name with the number incremented.
Or you can choose the tag value yourself, by using `--clopen=TAGVAL`.

### close --close

This prints just the closing balances transaction of `--clopen`.
It is the default if you don't specify a mode.

More customisation options are described below.
Among other things, you can use `close --close` to generate a transaction
moving the balances from any set of accounts, to a different account.
(If you need to move just a portion of the balance, see [hledger-move](https://hledger.org/scripts.html#hledger-move).)

### close --open

This prints just the opening balances transaction of `--clopen`.
(It is similar to [Ledger's equity command](https://ledger-cli.org/doc/ledger3.html#The-equity-command).)

### close --assert

This prints a transaction that [asserts](#balance-assertions) the account balances as they are on the end date (and adds an `assert:` tag).
It could be useful as documention and to guard against changes.

### close --assign

This prints a transaction that [assigns](#balance-assignments) the account balances as they are on the end date (and adds an "assign:" tag).
Unlike balance assertions, assignments will post changes to balances as needed to reach the specified amounts.

This is another way to set starting balances when migrating to a new file,
and it will set them correctly even in the presence of earlier files which do not have a closing balances transaction.
However, it can hide errors, and disturb the accounting equation,
so `--clopen` is usually [recommended](https://github.com/simonmichael/hledger/issues/2151).

### close --retain

This is like `--close`, but it closes Revenue and Expense account balances by default.
They will be transferred to `equity:retained earnings`, or another account specified with `--close-acct`.

Revenues and expenses correspond to changes in equity.
They are categorised separately for reporting purposes,
but traditionally at the end of each accounting period, businesses consolidate them into equity,
This is called "retaining earnings", or "closing the books".
 
In personal accounting, there's not much reason to do this, and most people don't.
(One reason to do it is to help the `balancesheetequity` report show a zero total,
demonstrating that the accounting equation (A-L=E) is satisfied.)

### close customisation

In all modes, the following things can be overridden:

- the accounts to be closed/opened, with account query arguments
- the closing/opening dates, with `-e OPENDATE`
- the balancing account, with `--close-acct=ACCT` and/or `--open-acct=ACCT`
- the transaction descriptions, with `--close-desc=DESC` and `--open-desc=DESC`
- the value of the transaction's `clopen` tag, with an argument for the mode flag.\
  (Eg: `--close=foo` or `--clopen=2025-main`)

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
$ hledger close --clopen -f 2022.journal -p 2022
# copy/paste the closing transaction to the end of 2022.journal
# copy/paste the opening transaction to the start of 2023.journal
```

After this, to see 2022's end-of-year balances you must exclude the closing balances transaction:

```cli
$ hledger -f 2022.journal bs not:desc:'closing balances'
```

For more flexibility, it helps to tag closing and opening transactions with eg `clopen:NEWYEAR`,
then you can ensure correct balances by excluding all opening/closing transactions except the first, like so:

```cli
$ hledger bs -Y -f 2021.j -f 2022.j -f 2023.j expr:'tag:clopen=2021 or not tag:clopen'
$ hledger bs -Y -f 2021.j -f 2022.j           expr:'tag:clopen=2021 or not tag:clopen'
$ hledger bs -Y -f 2022.j -f 2023.j           expr:'tag:clopen=2022 or not tag:clopen'
$ hledger bs -Y -f 2021.j                     expr:'tag:clopen=2021 or not tag:clopen'
$ hledger bs -Y -f 2022.j                     expr:'tag:clopen=2022 or not tag:clopen'
$ hledger bs -Y -f 2023.j                     # unclosed file, no query needed
```

#### More detailed close examples

See [examples/multi-year](https://github.com/simonmichael/hledger/tree/master/examples/multi-year/).
