# COMMANDS

hledger provides a number of subcommands; `hledger` with no arguments shows a list.

Select a subcommand by writing its name as first argument (eg `hledger incomestatement`). You can also write any unambiguous prefix of a command name (`hledger inc`), or one of the standard short aliases displayed in the command list (`hledger is`).

If you install additional `hledger-*` packages,
or if you put programs or scripts named `hledger-NAME` in your PATH, these will also be listed as hledger subcommands.

Here is a summary (see http://hledger.org/manual#commands for the full command help):

## Data entry

### add
prompt for transactions and add them to the journal.

This is the only hledger command that writes to the journal file.
It appends only, existing transactions are not changed.

`--no-new-accounts`
: don't allow creating new accounts; helps prevent typos when entering account names

## Reporting

### accounts

``` {.shell .right}
$ hledger accounts --tree
assets
  bank
    checking
    saving
  cash
expenses
  food
  supplies
income
  gifts
  salary
liabilities
  debts
```

``` {.shell .right}
$ hledger accounts --drop 1
bank:checking
bank:saving
cash
food
supplies
gifts
salary
debts
```

``` {.shell .right}
$ hledger accounts
assets:bank:checking
assets:bank:saving
assets:cash
expenses:food
expenses:supplies
income:gifts
income:salary
liabilities:debts
```

show account names

`--tree`
: show short account names, as a tree

`--flat`
: show full account names, as a list (default)

`--drop=N`
: in flat mode: omit N leading account name parts

This command lists all account names that are in use (ie, all the
accounts which have at least one transaction posting to them).  With
query arguments, only matched account names are shown.

It shows a flat list by default. In this mode you can add `--drop N`
to omit the first few account name components.

With `--tree`, it shows the account hierarchy.

### activity
show an ascii barchart of posting counts per interval
(default: daily)

### balance, bal
show accounts and balances

`--tree`
: show short account names, as a tree

`--flat`
: show full account names, as a list (default)

`--drop=N`
: in flat mode: omit N leading account name parts

`--format=LINEFORMAT`
: in single-column balance reports: use this custom line format

`--no-elide`
: in tree mode: don't squash boring parent accounts

`-H --historical`
: in multicolumn mode: show historical ending balances

`--cumulative`
: in multicolumn mode: show accumulated ending balances

`-A --average`
: in multicolumn mode: show a row average column

`-T --row-total`
: in multicolumn mode: show a row total column

`-N --no-total`
: don't show the final total row

`-V --value`
: show amounts as their current market value in their default valuation commodity

`-o FILE[.FMT] --output-file=FILE[.FMT]`
: write output to FILE instead of stdout. A recognised FMT suffix influences the format.

`-O FMT --output-format=FMT     `
: select the output format. Supported formats:
txt, csv.

### balancesheet, bs
show a balance sheet

`--flat`
: show full account names, as a list (default)

`--drop=N`
: in flat mode: omit N leading account name parts

### cashflow, cf
show a cashflow statement

`--flat`
: show full account names, as a list (default)

`--drop=N`
: in flat mode: omit N leading account name parts

### incomestatement, is
show an income statement

`--flat`
: show full account names, as a list (default)

`--drop=N`
: in flat mode: omit N leading account name parts

### print
show transactions from the journal

`-m STR --match=STR             `
: show the transaction whose description is most similar to STR, and is most recent

`-o FILE[.FMT] --output-file=FILE[.FMT]`
: write output to FILE instead of stdout. A recognised FMT suffix influences the format.

`-O FMT --output-format=FMT     `
: select the output format. Supported formats:
txt, csv.

### register, reg
show postings and running total

`-H --historical`
: include prior postings in the running total

`-A --average`
: show a running average instead of the running total (implies --empty)

`-r --related`
: show postings' siblings instead

`-w N --width=N`
: set output width (default: terminal width or COLUMNS. -wN,M sets description width as well)

`-o FILE[.FMT] --output-file=FILE[.FMT]`
: write output to FILE instead of stdout. A recognised FMT suffix influences the format.

`-O FMT --output-format=FMT     `
: select the output format. Supported formats:
txt, csv.

### stats
show some journal statistics

`-o FILE[.FMT] --output-file=FILE[.FMT]`
: write output to FILE instead of stdout. A recognised FMT suffix influences the format.

## Add-on commands

Additional commands will be available when executables or scripts
named "`hledger-`CMD" are installed in the PATH. These are often
provided by a package of the same name, or you can make your own custom scripts
(haskell scripts can use hledger-lib allowing tight integration).
Some available add-ons are:

### autosync
download OFX bank data and/or convert OFX to hledger journal format

### diff
show transactions present in one journal file but not another

### interest
generate interest transactions

### irr
calculate internal rate of return

### ui
curses-style interface, see hledger-ui(1)

### web
web interface, see hledger-web(1)

