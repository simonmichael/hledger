# ADD-ON COMMANDS

Add-on commands are executables in your PATH whose name starts with
`hledger-` and ends with any of these file extensions:
none, `.hs`,`.lhs`,`.pl`,`.py`,`.rb`,`.rkt`,`.sh`,`.bat`,`.com`,`.exe`.
Also, an add-on's name may not be the same as any built-in command or alias.

hledger will detect these and include them in the command list and let
you invoke them with `hledger ADDONCMD`. However there are some limitations:

- Options appearing before ADDONCMD will be visible only to hledger and will not be passed to the add-on.
  Eg: `hledger -h web` shows hledger's usage, `hledger web -h` shows hledger-web's usage.
- Options understood only by the add-on must go after a `--` argument to hide them from hledger, which would otherwise reject them.
  Eg: `hledger web -- --server`.

Sometimes it may be more convenient to just run the add-on directly, eg: `hledger-web --server`.

Add-ons which are written in haskell can take advantage of the hledger-lib library
for journal parsing, reporting, command-line options, etc.

Here are some hledger add-ons available from Hackage, 
the [extra](https://github.com/simonmichael/hledger/tree/master/extra) directory in the hledger source,
or elsewhere:

## Official add-ons

These are maintained and released along with hledger.   

### api
Web API server, see [hledger-api](hledger-api.html).

### ui
Curses-style interface, see [hledger-ui](hledger-ui.html).

### web
Web interface, see [hledger-web](hledger-web.html).

## Third party add-ons

These are maintained separately from hledger, and usually updated shortly after a hledger release.

### diff

[hledger-diff](http://hackage.haskell.org/package/hledger-diff)
Shows differences in an account's transactions between one journal file and another.

### iadd

[hledger-iadd](http://hackage.haskell.org/package/hledger-iadd)
A curses-style, more interactive replacement for the [add command](/hledger.html#add). 

### interest

[hledger-interest](http://hackage.haskell.org/package/hledger-interest)
Generates interest transactions for an account according to various schemes. 

### irr
[hledger-irr](http://hackage.haskell.org/package/hledger-irr)
Calculates the internal rate of return of an investment account.

## Experimental add-ons
  
These add-ons are available in source form 
[in the hledger repo](https://github.com/simonmichael/hledger/tree/master/bin). 
Installing them is [pretty easy](/download.html#d).
Reading and copying these is a good way to start making your own add-ons.
These may be less mature and documented than built-in commands.

### autosync

[hledger-autosync](https://github.com/simonmichael/hledger/blob/master/bin/hledger-autosync) 
is a symbolic link for easily running 
[ledger-autosync](https://pypi.python.org/pypi/ledger-autosync), if installed. 
ledger-autosync does deduplicating conversion of OFX data and some CSV formats,
and can also download the data 
[if your bank offers OFX Direct Connect](http://wiki.gnucash.org/wiki/OFX_Direct_Connect_Bank_Settings). 

### budget

[hledger-budget.hs](https://github.com/simonmichael/hledger/blob/master/bin/hledger-budget.hs#L10)
adds more budget-tracking features to hledger.

### chart

[hledger-chart.hs](https://github.com/simonmichael/hledger/blob/master/bin/hledger-chart.hs#L47)
is an old pie chart generator, in need of some love.

### check

[hledger-check.hs](https://github.com/simonmichael/hledger/blob/master/bin/hledger-check.hs)
checks more powerful account balance assertions.

### check-dates

[hledger-check-dates.hs](https://github.com/simonmichael/hledger/blob/master/bin/hledger-check-dates.hs#L15)
checks that journal entries are ordered by date.

### dupes

[hledger-dupes.hs](https://github.com/simonmichael/hledger/blob/master/bin/hledger-dupes.hs#L21)
checks for account names sharing the same leaf name.

### equity

[hledger-equity.hs](https://github.com/simonmichael/hledger/blob/master/bin/hledger-equity.hs#L17)
prints balance-resetting transactions, useful for bringing account balances across file boundaries. 

### prices

[hledger-prices.hs](https://github.com/simonmichael/hledger/blob/master/bin/hledger-prices.hs)
prints all prices from the journal.

### print-unique

[hledger-print-unique.hs](https://github.com/simonmichael/hledger/blob/master/bin/hledger-print-unique.hs#L15)
prints transactions which do not reuse an already-seen description.

### register-match

[hledger-register-match.hs](https://github.com/simonmichael/hledger/blob/master/bin/hledger-register-match.hs#L23)
helps ledger-autosync detect already-seen transactions when importing.

### rewrite

[hledger-rewrite.hs](https://github.com/simonmichael/hledger/blob/master/bin/hledger-rewrite.hs#L28)
Adds one or more custom postings to matched transactions.

