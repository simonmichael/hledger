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
Adding transactions to journal file /src/hledger/data/sample.journal
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

_include_({{commands-balance.m4.md}})

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

`-m STR --match=STR             `
: show the transaction whose description is most similar to STR, and is most recent

`-o FILE[.FMT] --output-file=FILE[.FMT]`
: write output to FILE instead of stdout. A recognised FMT suffix influences the format.

`-O FMT --output-format=FMT     `
: select the output format. Supported formats:
txt, csv.

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

The print command displays full transactions from the journal file,
tidily formatted and showing all amounts explicitly. The output of
print is always a valid hledger journal, but it does always not
preserve all original content exactly (eg directives).

hledger's print command also shows all unit prices in effect, or (with
-B/--cost) shows cost amounts.

The print command also supports 
[output destination](#output-destination)
and
[CSV output](#csv-output).

## register
Show postings and their running total. Alias: reg.

`-H --historical`
: show historical running balance, reflecting prior postings

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

`-o FILE[.FMT] --output-file=FILE[.FMT]`
: write output to FILE instead of stdout. A recognised FMT suffix influences the format.

```shell
$ hledger stats
Main journal file        : /src/hledger/data/sample.journal
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

## api
Web API server, see [hledger-api](hledger-api.html).

## autosync
Download OFX bank data and/or convert OFX to hledger journal format.

```shell
$ hledger autosync --help
usage: hledger-autosync [-h] [-m MAX] [-r] [-a ACCOUNT] [-l LEDGER] [-i INDENT]
                        [--initial] [--fid FID] [--assertions] [-d] [--hledger]
                        [--slow] [--which]
                        [PATH]

Synchronize ledger.

positional arguments:
  PATH                  do not sync; import from OFX file

optional arguments:
  -h, --help            show this help message and exit
  -m MAX, --max MAX     maximum number of days to process
  -r, --resync          do not stop until max days reached
  -a ACCOUNT, --account ACCOUNT
                        set account name for import
  -l LEDGER, --ledger LEDGER
                        specify ledger file to READ for syncing
  -i INDENT, --indent INDENT
                        number of spaces to use for indentation
  --initial             create initial balance entries
  --fid FID             pass in fid value for OFX files that do not supply it
  --assertions          create balance assertion entries
  -d, --debug           enable debug logging
  --hledger             force use of hledger (on by default if invoked as hledger-
                        autosync)
  --slow                use slow, but possibly more robust, method of calling ledger
                        (no subprocess)
  --which               display which version of ledger/hledger/ledger-python will
                        be used by ledger-autosync to check for previous
                        transactions
$ head acct1.ofx
OFXHEADER:100
DATA:OFXSGML
VERSION:102
SECURITY:NONE
ENCODING:USASCII
CHARSET:1252
COMPRESSION:NONE
OLDFILEUID:NONE
NEWFILEUIDe:8509488b59d1bb45

$ hledger autosync acct1.ofx
2013/08/30 MONTHLY SERVICE FEE
    ; ofxid: 3000.4303001832.201308301
    WF:4303001832                               -$6.00
    [assets:business:bank:wf:bchecking:banking]  $6.00

```

[ledger-autosync](https://bitbucket.org/egh/ledger-autosync/commits/all),
which includes a `hledger-autosync` alias, downloads transactions
from your bank(s) via OFX, and prints just the new ones as journal
entries which you can add to your journal. It can also operate on .OFX
files which you've downloaded manually. It can be a nice alternative
to hledger's built-in CSV reader, especially if your bank supports OFX
download.


## diff
Show transactions present in one journal file but not another

```shell
$ hledger diff --help
Usage: hledger-diff account:name left.journal right.journal
$ cat a.journal
1/1
 (acct:one)  1

$ cat b.journal
1/1
 (acct:one)  1
2/2
 (acct:two)  2

$ hledger diff acct:two a.journal b.journal
Unmatched transactions in the first journal:

Unmatched transactions in the second journal:

2015/02/02
    (acct:two)            $2

```

[hledger-diff](http://hackage.haskell.org/package/hledger-diff)
compares two journal files. Given an account name, it prints out the
transactions affecting that account which are in one journal file but
not in the other.  This can be useful for reconciling existing
journals with bank statements.

## equity
Print a journal entry that resets account balances to zero.

```shell
$ hledger balance --flat -E assets liabilities
                   0  assets:bank:checking
                  $1  assets:bank:saving
                 $-2  assets:cash
                  $1  liabilities:debts
--------------------
                   0
$ hledger equity assets liabilities
2015/05/23
    assets:bank:saving                $-1
    assets:cash                        $2
    liabilities:debts                 $-1
    equity:closing balances             0

2015/05/23
    assets:bank:saving                 $1
    assets:cash                       $-2
    liabilities:debts                  $1
    equity:opening balances             0

```

This prints a journal entry which zeroes out the specified accounts
(or all accounts) with a transfer to/from "equity:closing balances"
(like Ledger's equity command). Also, it prints an similar entry with
opposite sign for restoring the balances from "equity:opening
balances".

These can be useful for ending one journal file and starting a new
one, respectively. By zeroing your asset and liability accounts at the
end of a file and restoring them at the start of the next one, you
will see correct asset/liability balances whether you run hledger on
just one file, or on several files concatenated with [include](#include).

## interest
Generate interest transactions.

```shell
$ hledger interest --help
Usage: hledger-interest [OPTION...] ACCOUNT
  -h          --help            print this message and exit
  -V          --version         show version number and exit
  -v          --verbose         echo input ledger to stdout (default)
  -q          --quiet           don't echo input ledger to stdout
              --today           compute interest up until today
  -f FILE     --file=FILE       input ledger file (pass '-' for stdin)
  -s ACCOUNT  --source=ACCOUNT  interest source account
  -t ACCOUNT  --target=ACCOUNT  interest target account
              --act             use 'act' day counting convention
              --30-360          use '30/360' day counting convention
              --30E-360         use '30E/360' day counting convention
              --30E-360isda     use '30E/360isda' day counting convention
              --constant=RATE   constant interest rate
              --annual=RATE     annual interest rate
              --bgb288          compute interest according to German BGB288
              --ing-diba        compute interest according for Ing-Diba Tagesgeld account
```

```shell
$ cat interest.journal
2008/09/26 Loan
     Assets:Bank          EUR 10000.00
     Liabilities:Bank

2008/11/27 Payment
     Assets:Bank          EUR -3771.12
     Liabilities:Bank

2009/05/03 Payment
     Assets:Bank          EUR -1200.00
     Liabilities:Bank

2010/12/10 Payment
     Assets:Bank          EUR -3700.00
     Liabilities:Bank
```

```shell
$ hledger interest -- -f interest.journal --source=Expenses:Interest \
    --target=Liabilities:Bank --30-360 --annual=0.05 Liabilities:Bank
2008/09/26 Loan
    Assets:Bank       EUR 10000.00
    Liabilities:Bank  EUR -10000.00

2008/11/27 0.05% interest for EUR -10000.00 over 61 days
    Liabilities:Bank     EUR -84.72
    Expenses:Interest     EUR 84.72

2008/11/27 Payment
    Assets:Bank       EUR -3771.12
    Liabilities:Bank   EUR 3771.12

2008/12/31 0.05% interest for EUR -6313.60 over 34 days
    Liabilities:Bank     EUR -29.81
    Expenses:Interest     EUR 29.81

2009/05/03 0.05% interest for EUR -6343.42 over 123 days
    Liabilities:Bank    EUR -108.37
    Expenses:Interest    EUR 108.37

2009/05/03 Payment
    Assets:Bank       EUR -1200.00
    Liabilities:Bank   EUR 1200.00

2009/12/31 0.05% interest for EUR -5251.78 over 238 days
    Liabilities:Bank    EUR -173.60
    Expenses:Interest    EUR 173.60

2010/12/10 0.05% interest for EUR -5425.38 over 340 days
    Liabilities:Bank    EUR -256.20
    Expenses:Interest    EUR 256.20

2010/12/10 Payment
    Assets:Bank       EUR -3700.00
    Liabilities:Bank   EUR 3700.00

```

[hledger-interest](http://hackage.haskell.org/package/hledger-interest)
computes interests for a given account. Using command line flags,
the program can be configured to use various schemes for day-counting,
such as act/act, 30/360, 30E/360, and 30/360isda. Furthermore, it
supports a (small) number of interest schemes, i.e. annual interest
with a fixed rate and the scheme mandated by the German BGB288
(Basiszins für Verbrauchergeschäfte). See the package page for more.

## irr
Calculate internal rate of return.

```shell
$ hledger irr --help
Usage: hledger-irr [OPTION...]
  -h          --help                        print this message and exit
  -V          --version                     show version number and exit
  -c          --cashflow                    also show all revant transactions
  -f FILE     --file=FILE                   input ledger file (pass '-' for stdin)
  -i ACCOUNT  --investment-account=ACCOUNT  investment account
  -t ACCOUNT  --interest-account=ACCOUNT    interest/gain/fees/losses account
  -b DATE     --begin=DATE                  calculate interest from this date
  -e DATE     --end=DATE                    calculate interest until this date
  -D          --daily                       calculate interest for each day
  -W          --weekly                      calculate interest for each week
  -M          --monthly                     calculate interest for each month
  -Y          --yearly                      calculate interest for each year
```

```shell
$ cat irr.journal 
2011-01-01 Some wild speculation – I wonder if it pays off
   Speculation   €100.00
   Cash

2011-02-01 More speculation (and adjustment of value)
   Cash         -€10.00
   Rate Gain     -€1.00
   Speculation

2011-03-01 Lets pull out some money (and adjustment of value)
   Cash          €30.00
   Rate Gain     -€3.00
   Speculation

2011-04-01 More speculation (and it lost some money!)
   Cash         -€50.00
   Rate Gain     € 5.00
   Speculation

2011-05-01 Getting some money out (and adjustment of value)
   Speculation  -€44.00
   Rate Gain    -€ 3.00
   Cash

2011-06-01 Emptying the account (after adjusting the value)
   Speculation   -€85.00
   Cash           €90.00
   Rate Gain     -€ 5.00
```

```shell
$ hledger-irr -f irr.journal -t "Rate Gain" -i Speculation  --monthly
2011/01/01 - 2011/02/01: 12.49%
2011/02/01 - 2011/03/01: 41.55%
2011/03/01 - 2011/04/01: -51.44%
2011/04/01 - 2011/05/01: 32.24%
2011/05/01 - 2011/06/01: 95.92%
```

[hledger-irr](http://hackage.haskell.org/package/hledger-irr)
computes the internal rate of return, also known as the effective
interest rate, of a given investment. After specifying what account
holds the investment, and what account stores the gains (or losses, or
fees, or cost), it calculates the hypothetical annual rate of fixed
rate investment that would have provided the exact same cash flow.
See the package page for more.

## print-unique
Print only only journal entries which have a unique description.

```shell
$ cat unique.journal
1/1 test
 (acct:one)  1
2/2 test
 (acct:two)  2
$ LEDGER_FILE=unique.journal hledger print-unique
(-f option not supported)
2015/01/01 test
    (acct:one)             1

```

## rewrite
Prints all journal entries, adding specified custom postings to matched entries.

[hledger-rewrite.hs](https://github.com/simonmichael/hledger/blob/master/extra/hledger-rewrite.hs),
in hledger's extra directory (compilation optional), adds postings to existing transactions,
optionally with an amount based on the existing transaction's first amount. See the script for more details.

```shell
$ hledger rewrite -- [QUERY]        --add-posting "ACCT  AMTEXPR" ...
$ hledger rewrite -- ^income        --add-posting '(liabilities:tax)  *.33'
$ hledger rewrite -- expenses:gifts --add-posting '(budget:gifts)  *-1"'
```

## ui
Curses-style interface, see [hledger-ui](hledger-ui.html).

## web
Web interface, see [hledger-web](hledger-web.html).

