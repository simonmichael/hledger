# COMMANDS

hledger provides a number of subcommands; `hledger` with no arguments
shows a list.

If you install additional `hledger-*` packages, or if you put programs
or scripts named `hledger-NAME` in your PATH, these will also be
listed as subcommands.

Run a subcommand by writing its name as first argument (eg `hledger
incomestatement`). You can also write any unambiguous prefix of a
command name (`hledger inc`), or one of the standard short aliases
displayed in the command list (`hledger is`).

<!--
---
comment:
for each command: name, synopsis, description, examples.
...
-->

## accounts
Show account names.

`--tree`
: show short account names, as a tree

`--flat`
: show full account names, as a list (default)

`--drop=N`
: in flat mode: omit N leading account name parts

This command lists all account names that are in use (ie, all the
accounts which have at least one transaction posting to them).  With
query arguments, only matched account names are shown.

It shows a flat list by default. With `--tree`, it uses indentation to
show the account hierarchy.

In flat mode you can add `--drop N` to omit the first few account name
components.

Examples:

_col3_({{
_shell_({{
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
}})
}},{{
_shell_({{
$ hledger accounts --drop 1
bank:checking
bank:saving
cash
food
supplies
gifts
salary
debts
}})
}},{{
_shell_({{
$ hledger accounts
assets:bank:checking
assets:bank:saving
assets:cash
expenses:food
expenses:supplies
income:gifts
income:salary
liabilities:debts
}})
}})

## activity
Show an ascii barchart of posting counts per interval.

The activity command displays an ascii histogram showing
transaction counts by day, week, month or other reporting interval
(by day is the default). With query arguments, it counts only matched transactions.

```shell
$ hledger activity --quarterly
2008-01-01 **
2008-04-01 *******
2008-07-01 
2008-10-01 **
```

## add
Prompt for transactions and add them to the journal.

`--no-new-accounts`
: don't allow creating new accounts; helps prevent typos when entering account names

Many hledger users edit their journals directly with a text editor, or generate them from CSV.
For more interactive data entry, there is the `add` command, 
which prompts interactively on the console for new transactions, and appends
them to the journal file (if there are multiple `-f FILE` options, the first file is used.)
Existing transactions are not changed.
This is the only hledger command that writes to the journal file.

To use it, just run `hledger add` and follow the prompts.
You can add as many transactions as you like; when you are finished,
enter `.` or press control-d or control-c to exit.

Features:

- add tries to provide useful defaults, using the most similar recent
  transaction (by description) as a template.
- You can also set the initial defaults with command line arguments.
- [Readline-style edit keys](http://tiswww.case.edu/php/chet/readline/rluserman.html#SEC3)
  can be used during data entry.
- The tab key will auto-complete whenever possible - accounts,
  descriptions, dates (`yesterday`, `today`, `tomorrow`). If the input
  area is empty, it will insert the default value.
- If the journal defines a [default commodity](#default-commodity),
  it will be added to any bare numbers entered.
- A parenthesised transaction [code](#entries) may be entered following a date.
- [Comments](#comments) and tags may be entered following a description or amount.
- If you make a mistake, enter `<` at any prompt to restart the transaction.
- Input prompts are displayed in a different colour when the terminal supports it.

Example (see the [tutorial](step-by-step.html#record-a-transaction-with-hledger-add) for a detailed explanation):

_shell_({{
$ hledger add
Adding transactions to journal file /src/hledger/examples/sample.journal
Any command line arguments will be used as defaults.
Use tab key to complete, readline keys to edit, enter to accept defaults.
An optional (CODE) may follow transaction dates.
An optional ; COMMENT may follow descriptions or amounts.
If you make a mistake, enter < at any prompt to restart the transaction.
To end a transaction, enter . when prompted.
To quit, enter . at a date prompt or press control-d or control-c.
Date [2015/05/22]: 
Description: supermarket
Account 1: expenses:food
Amount  1: $10
Account 2: assets:checking
Amount  2 [$-10.0]: 
Account 3 (or . or enter to finish this transaction): .
2015/05/22 supermarket
    expenses:food             $10
    assets:checking        $-10.0

Save this transaction to the journal ? [y]: 
Saved.
Starting the next transaction (. or ctrl-D/ctrl-C to quit)
Date [2015/05/22]: <CTRL-D> $
}})

_include_({{balance.m4.md}})

## balancesheet
Show a balance sheet. Alias: bs.

`--flat`
: show full account names, as a list (default)

`--drop=N`
: in flat mode: omit N leading account name parts

This command displays a simple
[balance sheet](http://en.wikipedia.org/wiki/Balance_sheet). It currently
assumes that you have top-level accounts named `asset` and `liability`
(plural forms also allowed.)

```shell
$ hledger balancesheet
Balance Sheet

Assets:
                 $-1  assets
                  $1    bank:saving
                 $-2    cash
--------------------
                 $-1

Liabilities:
                  $1  liabilities:debts
--------------------
                  $1

Total:
--------------------
                   0
```

## cashflow
Show a cashflow statement. Alias: cf.

`--flat`
: show full account names, as a list (default)

`--drop=N`
: in flat mode: omit N leading account name parts

This command displays a simple
[cashflow statement](http://en.wikipedia.org/wiki/Cash_flow_statement)
It shows the change in all "cash" (ie, liquid assets) accounts for the
period. It currently assumes that cash accounts are under a top-level
account named `asset` and do not contain `receivable` or `A/R` (plural
forms also allowed.)

```shell
$ hledger cashflow
Cashflow Statement

Cash flows:
                 $-1  assets
                  $1    bank:saving
                 $-2    cash
--------------------
                 $-1

Total:
--------------------
                 $-1
```

## help
Show any of the hledger manuals.

The `help` command displays any of the main [hledger man pages](/docs.html).
(Unlike `hledger --help`, which displays only the hledger man page.)
Run it with no arguments to list available topics (their names are shortened for easier typing),
and run `hledger help TOPIC` to select one.
The output is similar to a man page, but fixed width.
It may be long, so you may wish to pipe it into a pager.
See also [info](#info) and [man](#man).

_shell_({{
$ hledger help
Choose a topic, eg: hledger help cli
cli, ui, web, api, journal, csv, timeclock, timedot
}})

_shell_({{
$ hledger help cli | less

hledger(1)                   hledger User Manuals                   hledger(1)



NAME
       hledger - a command-line accounting tool

SYNOPSIS
       hledger [-f FILE] COMMAND [OPTIONS] [CMDARGS]
       hledger [-f FILE] ADDONCMD -- [OPTIONS] [CMDARGS]
:
}})

## incomestatement
Show an income statement. Alias: is.

`--flat`
: show full account names, as a list (default)

`--drop=N`
: in flat mode: omit N leading account name parts

This command displays a simple
[income statement](http://en.wikipedia.org/wiki/Income_statement).  It
currently assumes that you have top-level accounts named `income` (or
`revenue`) and `expense` (plural forms also allowed.)

```shell
$ hledger incomestatement
Income Statement

Revenues:
                 $-2  income
                 $-1    gifts
                 $-1    salary
--------------------
                 $-2

Expenses:
                  $2  expenses
                  $1    food
                  $1    supplies
--------------------
                  $2

Total:
--------------------
                   0
```

## info
Show any of the hledger manuals using info.

The `info` command displays any of the [hledger reference manuals](/docs.html)
using the [info](https://en.wikipedia.org/wiki/Info_(Unix)) hypertextual documentation viewer.
This can be a very efficient way to browse large manuals.
It requires the "info" program to be available in your PATH.

As with [help](#help), run it with no arguments to list available topics (manuals).

## man
Show any of the hledger manuals using man.

The `man` command displays any of the [hledger reference manuals](/docs.html)
using [man](https://en.wikipedia.org/wiki/Man_page), the standard documentation viewer on unix systems.
This will fit the text to your terminal width, and probably invoke a pager automatically.
It requires the "man" program to be available in your PATH.

As with [help](#help), run it with no arguments to list available topics (manuals).

## print
Show transactions from the journal.

`       --explicit`
: show all amounts explicitly

`-m STR --match=STR             `
: show the transaction whose description is most similar to STR, and is most recent

`-O FMT --output-format=FMT     `
: select the output format. Supported formats:
txt, csv.

`-o FILE --output-file=FILE`
: write output to FILE.  A file extension matching one of the above formats selects that format.

```shell
$ hledger print
2008/01/01 income
    assets:bank:checking            $1
    income:salary                  $-1

2008/06/01 gift
    assets:bank:checking            $1
    income:gifts                   $-1

2008/06/02 save
    assets:bank:saving              $1
    assets:bank:checking           $-1

2008/06/03 * eat & shop
    expenses:food                $1
    expenses:supplies            $1
    assets:cash                 $-2

2008/12/31 * pay off
    liabilities:debts               $1
    assets:bank:checking           $-1
```

The print command displays full journal entries (transactions) from the journal file, tidily formatted.

As of hledger 1.2, print's output is always a valid [hledger journal](/journal.html).
However it may not preserve all original content, eg it does not print directives or inter-transaction comments.

Normally, transactions' implicit/explicit amount style is preserved:
when an amount is omitted in the journal, it will be omitted in the output.
You can use the `--explicit` flag to make all amounts explicit, which can be
useful for troubleshooting or for making your journal more readable and
robust against data entry errors.
Note, in this mode postings with a multi-commodity amount
(possible with an implicit amount in a multi-commodity transaction)
will be split into multiple single-commodity postings, for valid journal output.

With -B/--cost, amounts with [transaction prices](/journal.html#transaction-prices)
are converted to cost (using the transaction price).

The print command also supports 
[output destination](#output-destination)
and
[CSV output](#csv-output).
Here's an example of print's CSV output:
```shell
$ hledger print -Ocsv
"txnidx","date","date2","status","code","description","comment","account","amount","commodity","credit","debit","posting-status","posting-comment"
"1","2008/01/01","","","","income","","assets:bank:checking","1","$","","1","",""
"1","2008/01/01","","","","income","","income:salary","-1","$","1","","",""
"2","2008/06/01","","","","gift","","assets:bank:checking","1","$","","1","",""
"2","2008/06/01","","","","gift","","income:gifts","-1","$","1","","",""
"3","2008/06/02","","","","save","","assets:bank:saving","1","$","","1","",""
"3","2008/06/02","","","","save","","assets:bank:checking","-1","$","1","","",""
"4","2008/06/03","","*","","eat & shop","","expenses:food","1","$","","1","",""
"4","2008/06/03","","*","","eat & shop","","expenses:supplies","1","$","","1","",""
"4","2008/06/03","","*","","eat & shop","","assets:cash","-2","$","2","","",""
"5","2008/12/31","","*","","pay off","","liabilities:debts","1","$","","1","",""
"5","2008/12/31","","*","","pay off","","assets:bank:checking","-1","$","1","","",""
```
- There is one CSV record per posting, with the parent transaction's fields repeated.
- The "txnidx" (transaction index) field shows which postings belong to the same transaction.
  (This number might change if transactions are reordered within the file,
  files are parsed/included in a different order, etc.)
- The amount is separated into "commodity" (the symbol) and "amount" (numeric quantity) fields.
- The numeric amount is repeated in either the "credit" or "debit" column, for convenience.
  (Those names are not accurate in the accounting sense; it just puts negative amounts under
  credit and zero or greater amounts under debit.)


## register
Show postings and their running total. Alias: reg.

`--cumulative`
: show running total from report start date (default)

`-H --historical`
: show historical running total/balance (includes postings before report start date)

`-A --average`
: show running average of posting amounts instead of total (implies --empty)

`-r --related`
: show postings' siblings instead

`-w N --width=N`
: set output width (default: terminal width or COLUMNS. -wN,M sets description width as well)

`-O FMT --output-format=FMT     `
: select the output format. Supported formats:
txt, csv.

`-o FILE --output-file=FILE`
: write output to FILE.  A file extension matching one of the above formats selects that format.

The register command displays postings, one per line, and their
running total.  This is typically used with a [query](#queries)
selecting a particular account, to see that account's activity:

```shell
$ hledger register checking
2008/01/01 income               assets:bank:checking            $1            $1
2008/06/01 gift                 assets:bank:checking            $1            $2
2008/06/02 save                 assets:bank:checking           $-1            $1
2008/12/31 pay off              assets:bank:checking           $-1             0
```

The `--historical`/`-H` flag adds the balance from any undisplayed
prior postings to the running total.  This is useful when you want to
see only recent activity, with a historically accurate running balance:

```shell
$ hledger register checking -b 2008/6 --historical
2008/06/01 gift                 assets:bank:checking            $1            $2
2008/06/02 save                 assets:bank:checking           $-1            $1
2008/12/31 pay off              assets:bank:checking           $-1             0
```

The `--depth` option limits the amount of sub-account detail displayed.

The `--average`/`-A` flag shows the running average posting amount
instead of the running total (so, the final number displayed is the
average for the whole report period). This flag implies `--empty` (see below).
It is affected by `--historical`.
It works best when showing just one account and one commodity.

The `--related`/`-r` flag shows the *other* postings in the transactions
of the postings which would normally be shown.

With a [reporting interval](#reporting-interval), register shows
summary postings, one per interval, aggregating the postings to each account:

```shell
$ hledger register --monthly income
2008/01                 income:salary                          $-1           $-1
2008/06                 income:gifts                           $-1           $-2
```
Periods with no activity, and summary postings with a zero amount, are
not shown by default; use the `--empty`/`-E` flag to see them:

```shell
$ hledger register --monthly income -E
2008/01                 income:salary                          $-1           $-1
2008/02                                                          0           $-1
2008/03                                                          0           $-1
2008/04                                                          0           $-1
2008/05                                                          0           $-1
2008/06                 income:gifts                           $-1           $-2
2008/07                                                          0           $-2
2008/08                                                          0           $-2
2008/09                                                          0           $-2
2008/10                                                          0           $-2
2008/11                                                          0           $-2
2008/12                                                          0           $-2
```

Often, you'll want to see just one line per interval.
The `--depth` option helps with this, causing subaccounts to be aggregated:

```shell
$ hledger register --monthly assets --depth 1h
2008/01                 assets                                  $1            $1
2008/06                 assets                                 $-1             0
2008/12                 assets                                 $-1           $-1
```

Note when using report intervals, if you specify start/end dates these
will be adjusted outward if necessary to contain a whole number of
intervals. This ensures that the first and last intervals are full
length and comparable to the others in the report.

### Custom register output

register uses the full terminal width by default, except on windows.
You can override this by setting the `COLUMNS` environment variable (not a bash shell variable)
or by using the `--width`/`-w` option.

The description and account columns normally share the space equally
(about half of (width - 40) each). You can adjust this by adding a
description width as part of --width's argument, comma-separated:
`--width W,D` . Here's a diagram:
```
<--------------------------------- width (W) ---------------------------------->
date (10)  description (D)       account (W-41-D)     amount (12)   balance (12)
DDDDDDDDDD dddddddddddddddddddd  aaaaaaaaaaaaaaaaaaa  AAAAAAAAAAAA  AAAAAAAAAAAA
```
and some examples:
```shell
$ hledger reg                     # use terminal width (or 80 on windows)
$ hledger reg -w 100              # use width 100
$ COLUMNS=100 hledger reg         # set with one-time environment variable
$ export COLUMNS=100; hledger reg # set till session end (or window resize)
$ hledger reg -w 100,40           # set overall width 100, description width 40
$ hledger reg -w $COLUMNS,40      # use terminal width, and set description width
```

The register command also supports the
`-o/--output-file` and `-O/--output-format` options for controlling
[output destination](#output-destination) and [CSV output](#csv-output).

## stats
Show some journal statistics.

`-o FILE --output-file=FILE`
: write output to FILE.  A file extension matching one of the above formats selects that format.

```shell
$ hledger stats
Main journal file        : /src/hledger/examples/sample.journal
Included journal files   : 
Transactions span        : 2008-01-01 to 2009-01-01 (366 days)
Last transaction         : 2008-12-31 (2333 days ago)
Transactions             : 5 (0.0 per day)
Transactions last 30 days: 0 (0.0 per day)
Transactions last 7 days : 0 (0.0 per day)
Payees/descriptions      : 5
Accounts                 : 8 (depth 3)
Commodities              : 1 ($)
```

The stats command displays summary information for the whole journal, or
a matched part of it. With a [reporting interval](#reporting-interval),
it shows a report for each report period.

The stats command also supports `-o/--output-file`
for controlling [output destination](#output-destination).

## test
Run built-in unit tests.

```shell
$ hledger test
Cases: 74  Tried: 74  Errors: 0  Failures: 0
```

This command runs hledger's built-in unit tests and displays a quick report.
With a regular expression argument, it selects only tests with matching names.
It's mainly used in development, but it's also nice to be able to
check your hledger executable for smoke at any time.

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

### budget

[hledger-budget.hs](https://github.com/simonmichael/hledger/blob/master/bin/hledger-budget.hs#L10)
A tool adding more budget-tracking features to hledger.

### chart

[hledger-chart.hs](https://github.com/simonmichael/hledger/blob/master/bin/hledger-chart.hs#L47)
An old pie chart generator, in need of some love.

### check-dates

[hledger-check-dates.hs](https://github.com/simonmichael/hledger/blob/master/bin/hledger-check-dates.hs#L15)
Checks that journal entries are ordered by date.

### dupes

[hledger-dupes.hs](https://github.com/simonmichael/hledger/blob/master/bin/hledger-dupes.hs#L21)
Checks for account names sharing the same leaf name.

### equity

[hledger-equity.hs](https://github.com/simonmichael/hledger/blob/master/bin/hledger-equity.hs#L17)
Prints balance-resetting transactions useful for bringing account balances across file boundaries. 

### print-unique

[hledger-print-unique.hs](https://github.com/simonmichael/hledger/blob/master/bin/hledger-print-unique.hs#L15)
Prints transactions which do not reuse an already-seen description.

### register-match

[hledger-register-match.hs](https://github.com/simonmichael/hledger/blob/master/bin/hledger-register-match.hs#L23)
Helps ledger-autosync recognise already-imported transactions.

### rewrite

[hledger-rewrite.hs](https://github.com/simonmichael/hledger/blob/master/bin/hledger-rewrite.hs#L28)
Adds one or more custom postings to matched transactions.

