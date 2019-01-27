# COMMANDS

hledger provides a number of subcommands; `hledger` with no arguments
shows a list.

If you install additional `hledger-*` packages, or if you put programs
or scripts named `hledger-NAME` in your PATH, these will also be
listed as subcommands.

Run a subcommand by writing its name as first argument (eg `hledger
incomestatement`). You can also write one of the standard short aliases
displayed in parentheses in the command list (`hledger b`), or any
any unambiguous prefix of a command name (`hledger inc`). 

Here are all the builtin commands in alphabetical order.
See also `hledger` for a more organised command list,
and `hledger CMD -h` for detailed command help.  

<!--
---
comment:
for each command: name, synopsis, description, examples.
...
-->

## accounts

_include_(Hledger/Cli/Commands/Accounts.md)

## activity

_include_(Hledger/Cli/Commands/Activity.md)

## add

_include_(Hledger/Cli/Commands/Add.md)

_include_({{hledger_balance.m4.md}})

## balancesheet
  This command displays a simple balance sheet, showing historical ending
  balances of asset and liability accounts (ignoring any report begin date).
  It assumes that these accounts are under a top-level `asset` or `liability`
  account (case insensitive, plural forms also  allowed).
  Note this report shows all account balances with normal positive sign
  (like conventional financial statements, unlike balance/print/register)
  (experimental). (bs)

`--change`
: show balance change in each period, instead of historical ending balances

`--cumulative`
: show balance change accumulated across periods (in multicolumn reports), instead of historical ending balances

`-H --historical`
: show historical ending balance in each period (includes postings before report start date) (default)

`--tree`
: show accounts as a tree; amounts include subaccounts (default in simple reports)

`--flat`
: show accounts as a list; amounts exclude subaccounts except when account is depth-clipped (default in multicolumn reports)

`-A --average`
: show a row average column (in multicolumn mode)

`-T --row-total`
: show a row total column (in multicolumn mode)

`-N --no-total`
: don't show the final total row

`--drop=N`
: omit N leading account name parts (in flat mode)

`--no-elide`
: don't squash boring parent accounts (in tree mode)

`--format=LINEFORMAT`
: in single-column balance reports: use this custom line format

`--sort-amount`
: sort by amount instead of account code/name

Example:
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

With a [reporting interval](#reporting-interval), multiple columns
will be shown, one for each report period.
As with [multicolumn balance reports](#multicolumn-balance-reports),
you can alter the report mode with `--change`/`--cumulative`/`--historical`.
Normally balancesheet shows historical ending balances, which is what
you need for a balance sheet; note this means it ignores report begin
dates.

This command also supports [output destination](/manual.html#output-destination) and [output format](/manual.html#output-format) selection.

## balancesheetequity
Just like [balancesheet](#balancesheet), but also reports Equity
(which it assumes is under a top-level `equity` account).

Example:
```shell
$ hledger balancesheetequity
Balance Sheet With Equity

Assets:
                 $-2  assets
                  $1    bank:saving
                 $-3    cash
--------------------
                 $-2

Liabilities:
                  $1  liabilities:debts
--------------------
                  $1

Equity:
		  $1  equity:owner
--------------------
		  $1

Total:
--------------------
                   0
```

## cashflow
  This command displays a simple cashflow statement, showing changes
  in "cash" accounts. It assumes that these accounts are under a top-level
  `asset` account (case insensitive, plural forms also allowed) and do not
  contain `receivable` or `A/R` in their name.
  Note this report shows all account balances with normal positive sign
  (like conventional financial statements, unlike balance/print/register)
  (experimental). (cf)

`--change`
: show balance change in each period (default)

`--cumulative`
: show balance change accumulated across periods (in multicolumn reports), instead of changes during periods

`-H --historical`
: show historical ending balance in each period (includes postings before report start date), instead of changes during each period

`--tree`
: show accounts as a tree; amounts include subaccounts (default in simple reports)

`--flat`
: show accounts as a list; amounts exclude subaccounts except when account is depth-clipped (default in multicolumn reports)

`-A --average`
: show a row average column (in multicolumn mode)

`-T --row-total`
: show a row total column (in multicolumn mode)

`-N --no-total`
: don't show the final total row (in simple reports)

`--drop=N`
: omit N leading account name parts (in flat mode)

`--no-elide`
: don't squash boring parent accounts (in tree mode)

`--format=LINEFORMAT`
: in single-column balance reports: use this custom line format

`--sort-amount`
: sort by amount instead of account code/name

Example:
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

With a [reporting interval](#reporting-interval), multiple columns
will be shown, one for each report period.
Normally cashflow shows changes in assets per period, though
as with [multicolumn balance reports](#multicolumn-balance-reports)
you can alter the report mode with `--change`/`--cumulative`/`--historical`.

This command also supports [output destination](/manual.html#output-destination) and [output format](/manual.html#output-format) selection.

## check-dates
Check that transactions are sorted by increasing date.
With a query, only matched transactions' dates are checked.

## check-dupes
Report account names having the same leaf but different prefixes. 
An example: http://stefanorodighiero.net/software/hledger-dupes.html

## close

_include_(Hledger/Cli/Commands/Close.md)

## files
List all files included in the journal. With a REGEX argument,
only file names matching the regular expression (case sensitive) are shown.

## help
Show any of the hledger manuals.

The `help` command displays any of the main [hledger manuals](/docs.html), in one of several ways.
Run it with no argument to list the manuals, or provide a full or partial manual name to select one.

hledger manuals are available in several formats.
hledger help will use the first of these display methods that it finds: 
info, man, $PAGER, less, stdout (or when non-interactive, just stdout). 
You can force a particular viewer with the `--info`, `--man`, `--pager`, `--cat` flags.

_shell_({{
$ hledger help
Please choose a manual by typing "hledger help MANUAL" (a substring is ok).
Manuals: hledger hledger-ui hledger-web hledger-api journal csv timeclock timedot
}})

_shell_({{
$ hledger help h --man

hledger(1)                    hledger User Manuals                    hledger(1)

NAME
       hledger - a command-line accounting tool

SYNOPSIS
       hledger [-f FILE] COMMAND [OPTIONS] [ARGS]
       hledger [-f FILE] ADDONCMD -- [OPTIONS] [ARGS]
       hledger

DESCRIPTION
       hledger  is  a  cross-platform  program  for tracking money, time, or any
...
}})

## import
Read new transactions added to each FILE since last run, and add them to
the main journal file.

`--dry-run`
: just show the transactions to be imported

The input files are specified as arguments - no need to write -f before each one.
So eg to add new transactions from all CSV files to the main journal, it's just: 
`hledger import *.csv`

New transactions are detected in the same way as print --new: 
by assuming transactions are always added to the input files in increasing date order,
and by saving `.latest.FILE` state files.

The --dry-run output is in journal format, so you can filter it, eg 
to see only uncategorised transactions: 
```shell
$ hledger import --dry ... | hledger -f- print unknown --ignore-assertions
```

## incomestatement
  This command displays a simple income statement, showing revenues
  and expenses during a period. It assumes that these accounts are under a
  top-level `revenue` or `income` or `expense` account (case insensitive,
  plural forms also allowed).
  Note this report shows all account balances with normal positive sign
  (like conventional financial statements, unlike balance/print/register)
  (experimental). (is)

`--change`
: show balance change in each period (default)

`--cumulative`
: show balance change accumulated across periods (in multicolumn reports), instead of changes during periods

`-H --historical`
: show historical ending balance in each period (includes postings before report start date), instead of changes during each period

`--tree`
: show accounts as a tree; amounts include subaccounts (default in simple reports)

`--flat`
: show accounts as a list; amounts exclude subaccounts except when account is depth-clipped (default in multicolumn reports)

`-A --average`
: show a row average column (in multicolumn mode)

`-T --row-total`
: show a row total column (in multicolumn mode)

`-N --no-total`
: don't show the final total row

`--drop=N`
: omit N leading account name parts (in flat mode)

`--no-elide`
: don't squash boring parent accounts (in tree mode)

`--format=LINEFORMAT`
: in single-column balance reports: use this custom line format

`--sort-amount`
: sort by amount instead of account code/name

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

With a [reporting interval](#reporting-interval), multiple columns
will be shown, one for each report period.
Normally incomestatement shows revenues/expenses per period, though
as with [multicolumn balance reports](#multicolumn-balance-reports)
you can alter the report mode with `--change`/`--cumulative`/`--historical`.

This command also supports [output destination](/manual.html#output-destination) and [output format](/manual.html#output-format) selection.

## prices
Print [market price directives](/manual#market-prices) from the journal.
With --costs, also print synthetic market prices based on [transaction prices](/manual#transaction-prices).
With --inverted-costs, also print inverse prices based on transaction prices.
Prices (and postings providing prices) can be filtered by a query.

## print
Show transactions from the journal. Aliases: p, txns.

`-m STR --match=STR             `
: show the transaction whose description is most similar to STR, and is most recent

`       --new`
: show only newer-dated transactions added in each file since last run

`-x     --explicit`
: show all amounts explicitly

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

The print command displays full journal entries (transactions) from the journal file in date order, tidily formatted.
print's output is always a valid [hledger journal](/journal.html).
It preserves all transaction information, but it does not preserve directives or inter-transaction comments

Normally, the journal entry's explicit or implicit amount style is preserved.
Ie when an amount is omitted in the journal, it will be omitted in the output.
You can use the `-x`/`--explicit` flag to make all amounts explicit, which can be
useful for troubleshooting or for making your journal more readable and
robust against data entry errors.
Note, `-x` will cause postings with a multi-commodity amount
(these can arise when a multi-commodity transaction has an implicit amount)
will be split into multiple single-commodity postings, for valid journal output.

With `-B`/`--cost`, amounts with [transaction prices](/journal.html#transaction-prices)
are converted to cost using that price. This can be used for troubleshooting.

With `-m`/`--match` and a STR argument, print will show at most one transaction: the one 
one whose description is most similar to STR, and is most recent. STR should contain at
least two characters. If there is no similar-enough match, no transaction will be shown.

With `--new`, for each FILE being read, hledger reads (and writes) a special 
state file (`.latest.FILE` in the same directory), containing the latest transaction date(s)
that were seen last time FILE was read. When this file is found, only transactions 
with newer dates (and new transactions on the latest date) are printed.
This is useful for ignoring already-seen entries in import data, such as downloaded CSV files.
Eg:
```console
$ hledger -f bank1.csv print --new
# shows transactions added since last print --new on this file
```
This assumes that transactions added to FILE always have same or increasing dates, 
and that transactions on the same day do not get reordered.
See also the [import](#import) command.    

This command also supports [output destination](/manual.html#output-destination) and [output format](/manual.html#output-format) selection.
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

## print-unique
Print transactions which do not reuse an already-seen description.

## register
Show postings and their running total. Aliases: r, reg.

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

This command also supports [output destination](/manual.html#output-destination) and [output format](/manual.html#output-format) selection.

## register-match
Print the one posting whose transaction description is closest to DESC, 
in the style of the register command.
Helps ledger-autosync detect already-seen transactions when importing.

## rewrite
Print all transactions, adding custom postings to the matched ones.

## roi
Shows time-weighted (TWR) and money-weighted (IRR) rate of return on your investments.
See `roi --help` for more.   

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

This command also supports [output destination](/manual.html#output-destination) and [output format](/manual.html#output-format) selection.

## tags
List all the tag names used in the journal. With a TAGREGEX argument,
only tag names matching the regular expression (case insensitive) are shown. 
With additional QUERY arguments, only transactions matching the query are considered.  

## test
Run built-in unit tests.

Prints test names and their results on stdout.
If any test fails or gives an error, the exit code will be non-zero.

Test names include a group prefix.
If a (exact, case sensitive) group prefix, or a full test name is provided as the first argument, 
only that group or test is run.

If a numeric second argument is provided, it will set the randomness seed,
for repeatable results from tests using randomness (currently none of them). 

This is mainly used by developers, but it's nice to be able to sanity-check your installed hledger executable at any time.
All tests are expected to pass - if you ever see otherwise, something has gone wrong, please report a bug! 

_include_(hledger_addons.m4.md)
