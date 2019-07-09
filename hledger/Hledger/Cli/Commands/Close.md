close, equity\
Prints a "closing balances" transaction and an "opening balances" transaction
that bring account balances to and from zero, respectively.
Useful for bringing asset/liability balances forward into a new journal file,
or for closing out revenues/expenses to retained earnings at the end of a
period.

_FLAGS_

The closing transaction transfers balances to "equity:closing balances".
The opening transaction transfers balances from "equity:opening balances".
You can choose to print just one of the transactions by using the
`--opening` or `--closing` flag.

If you split your journal files by time (eg yearly), you will
typically run this command at the end of the year, and save the
closing transaction as last entry of the old file, and the opening
transaction as the first entry of the new file.
This makes the files self contained, so that correct balances are
reported no matter which of them are loaded. Ie, if you load just one
file, the balances are initialised correctly; or if you load several
files, the redundant closing/opening transactions cancel each other
out. (They will show up in print or register reports; you can exclude
them with a query like `not:desc:'(opening|closing) balances'`.)

If you're running a business, you might also use this command to
"close the books" at the end of an accounting period, transferring
income statement account balances to retained earnings. (You may want
to change the equity account name to something like 
"equity:retained earnings".)

By default, the closing transaction is dated yesterday, the balances 
are calculated as of end of yesterday, and the opening transaction is dated today.
To close on some other date, use: `hledger close -e OPENINGDATE`.
Eg, to close/open on the 2018/2019 boundary, use `-e 2019`.
You can also use -p or `date:PERIOD` (any starting date is ignored).

Both transactions will include balance assertions for the
closed/reopened accounts.  You probably shouldn't use status or
realness filters (like -C or -R or `status:`) with this command, or
the generated balance assertions will depend on these flags.
Likewise, if you run this command with --auto, the balance assertions
will probably always require --auto.

When account balances have cost information (transaction prices), the 
closing/opening transactions will preserve it, so that eg balance -B reports
will not be affected.

Examples:

Carrying asset/liability balances into a new file for 2019, all from command line:

*Warning: we use `>>` here to append; be careful not to type a single `>` which would wipe your journal!*

```shell
$ hledger close -f 2018.journal -e 2019 assets liabilities --opening >>2019.journal
$ hledger close -f 2018.journal -e 2019 assets liabilities --closing >>2018.journal
```

Now:

```shell
$ hledger bs -f 2019.journal                   # one file - balances are correct
$ hledger bs -f 2018.journal -f 2019.journal   # two files - balances still correct
$ hledger bs -f 2018.journal not:desc:closing  # to see year-end balances, must exclude closing txn
```

Transactions spanning the closing date can complicate matters, breaking balance assertions:

```journal
2018/12/30 a purchase made in 2018, clearing the following year
    expenses:food          5
    assets:bank:checking  -5  ; [2019/1/2]
```

Here's one way to resolve that:

```journal
; in 2018.journal:
2018/12/30 a purchase made in 2018, clearing the following year
    expenses:food          5
    liabilities:pending

; in 2019.journal:
2019/1/2 clearance of last year's pending transactions
    liabilities:pending    5 = 0
    assets:checking
```
