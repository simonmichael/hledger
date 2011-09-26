---
title: hledger user manual
---

# User manual

## Introduction

hledger is a program for tracking money, time, or any other commodity,
using a simple, editable file format and the powerful principles of
double-entry accounting. It was inspired by [ledger](http://ledger-cli.org).

hledger's basic function is to read a plain text file describing (eg)
financial transactions, and quickly generate useful reports via the
command line. It can also help you record transactions, or (via add-ons)
provide a local web interface for editing, or publish live financial data
on the web.

You can use it to, eg:

- track spending and income
- track unpaid or due invoices
- track time and report by day/week/month/project
- get accurate numbers for client billing and tax filing

hledger aims to help both computer experts and regular folks gain clarity
in their finances. For the moment, it may be a little more suited to
techies.  Please give it a try and let me know how we're doing.

hledger is copyright (c) 2007-2011
[Simon&nbsp;Michael&nbsp;<simon@joyful.com>](mailto:simon@joyful.com) and
contributors, and released as Free Software under GPL version 3 or later.

This is the user manual and reference for hledger version 0.16.

## Installing

hledger works on linux, mac and windows. You can download and run current release
binaries from the [download page](DOWNLOAD.html).

Or, you can build the current release from source using cabal-install.
Ensure you have [GHC](http://hackage.haskell.org/ghc/) (6.12 or greater)
or the [Haskell Platform](http://hackage.haskell.org/platform/) installed,
then:

    $ cabal update
    $ cabal install hledger

You can also install some optional [add-ons](#add-on-commands) providing
extra features. These vary in maturity and supportedness and may not be
available on all platforms (check the download page to see platform
support).

    $ cabal install hledger-web
    $ cabal install hledger-vty
    $ cabal install hledger-chart
    $ cabal install hledger-interest

Or, you can build the latest [development version](http://joyful.com/darcsweb/darcsweb.cgi?r=hledger) of (most of) these like so:

    $ cabal update
    $ darcs get --lazy http://joyful.com/repos/hledger
    $ cd hledger
    $ make install

**Installation notes:**

- When installing with cabal, dependency problems are common. These can often be worked around by making sure to cabal update, using --constraint, and/or ghc-pkg unregister-ing obsolete package versions.
- If you have non-ascii journal data, you may need to [set a suitable locale](#usage-issues)
- hledger-chart requires additional GTK-related libraries, see [Gtk2Hs installation notes](http://code.haskell.org/gtk2hs/INSTALL). On ubuntu, install the `libghc6-gtk-dev` package.
- hledger-vty requires curses-related libraries (ubuntu package: `libncurses5-dev`) and is not buildable on microsoft windows (except possibly via cygwin.)
- If you have trouble, please see [Troubleshooting](#troubleshooting) and ask for [Support](DEVELOPMENT.html#support).

## Usage

Basic usage is:

    $ hledger COMMAND [OPTIONS] [ARGS]

Most [commands](#commands) query or operate on a
[journal file](#the-journal-file), which by default is `.hledger.journal`
in your home directory. You can specify a different file with the `-f`
option or `LEDGER_FILE` environment variable, or standard input with `-f
-`.  If the journal file does not exist, an empty one will be
created. Aside from this, only the `add` and `web` commands can modify the
journal.

Options are similar across most commands, with some variations; use
`hledger COMMAND --help` for details. Most options must appear somewhere
after COMMAND, not before it. The `-f` option can appear anywhere.

Arguments are also command-specific, but usually they are
[filter patterns](#filter-patterns) which select a subset of the journal,
eg transactions in a certain account.

To get started quickly, run `hledger add` and follow the prompts to enter
some transactions.  Or, save this
[sample file](http://joyful.com/repos/hledger/data/sample.journal) as
`.hledger.journal` in your home directory. Now try commands like these:

    $ hledger                               # show available commands
    $ hledger add                           # add some new transactions to the journal file
    $ hledger balance                       # all accounts with aggregated balances
    $ hledger balance --help                # show help for balance command
    $ hledger balance --depth 1             # only top-level accounts
    $ hledger register                      # show a register of postings from all transactions
    $ hledger reg income                    # show postings to/from income accounts
    $ hledger reg checking                  # show postings to/from checking account
    $ hledger reg desc:shop                 # show postings with shop in the description
    $ hledger activity                      # show transactions per day as a bar chart
    
## The journal file

hledger reads data from a plain text file, called a *journal* because it
represents a standard accounting
[general journal](http://en.wikipedia.org/wiki/General_journal).  It
contains a number of transaction entries, each describing a transfer of
money (or any commodity) between two or more named accounts, in a simple
format readable by both hledger and humans.

You can use hledger without learning any more about this file; just use
the [add](#add) or [web](#web) commands. Many users, though, also edit the
journal file directly with a text editor, perhaps assisted by the helper
modes for emacs or vi. Note the file uses unix line endings on all
platforms.

hledger's file format aims to be [compatible](#file-format-compatibility)
with c++ ledger, so you can use both tools on your journal.

Here's an example:

    ; A sample journal file. This is a comment.
    
    2008/01/01 income               ; <- transaction's first line starts in column 0, contains date and description
        assets:bank:checking  $1    ; <- posting lines start with whitespace, each contains an account name
        income:salary        $-1    ;    followed by at least two spaces and an amount
    
    2008/06/01 gift
        assets:bank:checking  $1    ; <- at least two postings in a transaction
        income:gifts         $-1    ; <- their amounts must balance to 0
    
    2008/06/02 save
        assets:bank:saving    $1
        assets:bank:checking        ; <- one amount may be omitted; here $-1 is inferred
    
    2008/06/03 eat & shop           ; <- description can be anything
        expenses:food         $1
        expenses:supplies     $1    ; <- this transaction debits two expense accounts
        assets:cash                 ; <- $-2 inferred
    
    2008/12/31 * pay off            ; <- an optional * after the date means "cleared" (or anything you want)
        liabilities:debts     $1
        assets:bank:checking

### Transactions

Each transaction begins with a date in column 0, followed by an optional
description, then two or more postings (of some amount to some account),
each on their own line.

The posting amounts within a transaction must always balance, ie add up to
0.  You can leave one amount blank and it will be inferred.

### Account names

Account names typically have several parts separated by a full colon, from
which hledger derives a hierarchical chart of accounts. They can be
anything you like, but in finance there are traditionally five top-level
accounts: `assets`, `liabilities`, `income`, `expenses`, and `equity`.

Account names may contain single spaces, eg: `assets:accounts receivable`.

### Amounts

After the account name, separated by *two or more spaces*, there is
usually an amount.  This is a number, optionally with a currency symbol or
commodity name on either the left or right. Commodity names which contain
more than just letters should be enclosed in double quotes.

Negative amounts usually have the minus sign next to the number: `$-1`.
Or it may go before the symbol: `-$1`.

The number may optionally have a decimal point, either a period (`.`) or a
comma (`,`). hledger's reports will generally use the highest precision
you have used in each commodity.

Numbers may also have digit group separators, eg thousands separators.
hledger's reports will follow the digit groups you have used.  The
separator character is either comma or period - whichever one you did not
use as a decimal point. If using digit group separators you should write
at least one number with a decimal point, so hledger will know which is
which. Eg: `1,000.00` or `1.000,00`.

### Simple dates

Within a journal file, transaction dates always follow a year/month/day
format, although several different separator characters are accepted. Some
examples:

> `2010/01/31`, `2010/1/31`, `2010-1-31`, `2010.1.31`

Writing the year is optional if you set a default year with a Y directive.
This is a line containing `Y` and the year; it affects subsequent
transactions, like so:

    Y2009
    
    12/15  ; equivalent to 2009/12/15
      ...
    
    Y2010
    
    1/31  ; equivalent to 2010/1/31
      ...

### Actual & effective dates

Most of the time, a simple transaction date is all you need. However
real-life transactions sometimes involve more than one date.  For example,
you buy a movie ticket on friday with a debit card, and the transaction is
charged to your bank account on monday.  Or you write a cheque to someone
and they deposit it weeks later.

When you don't care about this, just pick one date for your journal
transaction; either will do. But when you want to model reality more
accurately (eg: to match your daily bank balance), write both dates,
separated by an equals sign. Following ledger's convention, the *actual
date* (or "bank date") goes on the left, and is used by default, the
*effective date* (or "your date") goes on the right, and is used when the
`--effective` flag is provided. Here are some mnemonics to prevent confusion:

- ACTUAL=EFFECTIVE. The actual date is (by definition) the one on the left. A before E.
- BANKDATE=MYDATE. You can usually think "actual is bank's, effective is mine".
- LATER=EARLIER. The effective date is usually the chronologically earlier one.
- "The cheque took EFFECT then, but ACTUALLY cleared weeks later."

Example:

    ; ACTUAL=EFFECTIVE
    ; The effective date's year is optional, defaulting to the actual date's
    2010/2/23=2/19 movie ticket
      expenses:cinema                   $10
      assets:checking

    $ hledger register checking
    2010/02/23 movie ticket         assets:checking                $-10         $-10

    $ hledger register checking --effective
    2010/02/19 movie ticket         assets:checking                $-10         $-10

### Default commodity

You can set a default commodity or currency with a D directive. The
commodity will be used for any subsequent amounts which have no commodity
symbol. This directive also influences the display format for amounts in
that commodity.

    ; default commodity: british pound, comma thousands separator, two decimal places
    D £1,000.00
    
    2010/1/1
      a  2340   ; no commodity symbol, will use the above
      b

### Prices

You can specify a commodity's unit price or exchange rate, in terms of
another commodity. To set the price for a single posting's amount, write
`@ UNITPRICE` after the amount, where UNITPRICE is the per-unit price in a
different commodity:

    2009/1/2
     assets:cash:foreign currency       €100 @ $1.35  ; one hundred euros priced at $1.35 each
     assets:cash

Or, you can write `@@ TOTALPRICE`, which is sometimes more convenient:

    2009/1/2
     assets:cash:foreign currency       €100 @@ $135  ; one hundred euros priced at $135 for the lot (equivalent to the above)
     assets:cash

Or, you can set the price for this commodity as of a certain date, using a
historical price directive (P) as shown:

    ; the exchange rate for euro is $1.35 on 2009/1/1 (and thereafter, until a newer price directive is found)
    ; four space-separated fields: P, date, commodity symbol, unit price in 2nd commodity
    P 2009/1/1 € $1.35  
    
    2009/1/2
     expenses:foreign currency       €100
     assets

Note: a time and numeric time zone are allowed in historical price directives, but currently ignored.

Or, you can write a transaction in two commodities, without prices but
with all amounts specified, and a conversion price will be inferred so as
to balance the transaction:

    2009/1/2
     expenses:foreign currency       €100
     assets                         $-135

The print command shows any prices in effect. So the first example above gives:

    $ hledger print
    2009/01/02
        expenses:foreign currency  €100 @ $1.35
        assets                     €-100 @ $1.35

To see amounts converted to their total cost, use the `--cost/-B` flag
with any command:

    $ hledger print --cost
    2009/01/02 x
        expenses:foreign currency       $135.00
        assets                         $-135.00

In other words the `--cost/-B` flag converts amounts to their price's
commodity. (It will not look up the price of a price.)

Note hledger handles prices differently from c++ ledger in this respect:
we assume unit prices do not vary over time.  This is good for simple
reporting of foreign currency transactions, but not for tracking
fluctuating-value investments or capital gains.

### Including other files

You can pull in the content of additional journal files, by writing lines like this:

    !include path/to/file.journal

The `!include` directive may only be used in journal files, and currently
it may only include other journal files (eg, not timelog files.)

### Default parent account

You can specify a parent account which will be prepended to all accounts
within a section of the journal. Use the `!account` directive like so:

    !account home
    
    2010/1/1
        food    $10
        cash
    
    !end

If `!end` is omitted, the effect lasts to the end of the file.
The above is equivalent to:

    2010/01/01
        home:food           $10
        home:cash          $-10

Included files are also affected, eg:

    !account business
    !include biz.journal
    !end
    !account personal
    !include personal.journal
    !end

### Account aliases

You can define account aliases to rewrite certain account names (and their subaccounts).
This tends to be a little more reliable than post-processing with sed or similar.
The directive is `alias ORIG = ALIAS`, where ORIG and ALIAS are full account names.
To forget all aliases defined to this point, use `end aliases`.

Here's an example: say a sole proprietor has a personal.journal:

    1/1
        expenses:food  $1
        assets:cash

and a business.journal:

    1/1
        expenses:office supplies  $1
        assets:business checking

Here each entity has a simple journal with its own simple chart of
accounts.  But at tax reporting time, we need to view these as a single
entity.  So in unified.journal we adjust the personal account names to fit
within the business chart of accounts:

    alias expenses    = equity:draw:personal
    alias assets:cash = assets:personal cash
    include personal.journal
    end aliases
    include business.journal

giving:

    $ hledger -f unified.journal print
    2011/01/01
        equity:draw:personal:food            $1
        assets:personal cash                $-1
    
    2011/01/01
        expenses:office supplies            $1
        assets:business checking           $-1

You can also specify aliases on the command line. This could be useful to
rewrite account names when sharing a report with someone else, such as
your accountant:

    $ hledger --alias 'my earning=income:business'

Command-line alias options are applied after any alias directives in the
journal.  At most one alias directive and one alias option will be applied
to each account name.

## Commands

hledger provides a number of subcommands, in the style of git or darcs.
Run `hledger` with no arguments to see a list.  Most are built in to the
core hledger package, while [add-on commands](#add-on-commands) will
appear if you install additional hledger-* packages. You can also install
your own subcommands by putting programs or scripts named `hledger-NAME`
in your PATH.

### Misc commands

Here are some miscellaneous commands you might use to get started:

#### add

The add command prompts interactively for new transactions, and appends
them to the journal file. Each transaction is appended when you complete
it by entering `.` (period) at the account prompt.  Enter control-D or
control-C when you are done.

The add command tries to be helpful, providing:

- Sensible defaults

- History awareness: if there are existing transactions approximately
  matching the description you enter, they will be displayed and the best
  match will provide defaults for the other fields. If you specify
  [filter pattern(s)](#filter-patterns) on the command line, only matching
  transactions will be considered as history.

- Readline-style input: during data entry, the usual editing keys should
  work.

- Auto-completion for account names: while entering account names, the tab
  key will auto-complete as far as possible, or list the available
  options.

- Default commodity awareness: if the journal has a
  [default commodity directive](#default-commodity), that will be applied
  to any bare numbers entered.

Examples:

    $ hledger add
    $ hledger -f home.journal add equity:bob

#### convert

The convert command reads a
[CSV](http://en.wikipedia.org/wiki/Comma-separated_values) file you have
downloaded from your bank, and prints out the transactions in journal
format, suitable for adding to your journal. (It does not alter your
journal directly.) This can be a lot quicker than entering every
transaction by hand; the downside is that you are less likely to notice if
your bank makes an error.

Use it like this:

    $ hledger convert FILE.csv

where FILE.csv is your downloaded bank data.

If this is the first time converting FILE.csv, you'll see a very poor
default conversion.  convert gets the necessary hints from a conversion
rules file, named FILE.rules by default, which it auto-creates the first
time. You'll need to add directives to this rules file, described below,
and convert again, until the conversion is accurate. These rules will be
reused for FILE.csv in future and should soon require only minor
adjustments if any.

Finally, copy only the new transactions to your main journal. hledger
doesn't detect transactions already copied last time, so you'll need to
skip over those. The converted transactions are always sorted by date. You
may find it easier to pipe the output into a temporary file and copy/paste
from there:

    $ hledger convert FILE.csv >FILE.journal

You can also convert standard input by specifying no CSV file (or `-`); in
this case you must specify the rules file with `--rules-file`. Eg:

    $ cat foo.csv | fixup | hledger convert --rules-file foo.rules

##### convert rules file

convert's \*.rules file contains primarily (1) CSV field definitions and
(2) rules for assigning each transaction's account(s). Typically you will
have one csv file and one rules file per bank account and rely on the file
naming convention described above. If you have many CSV files for each
account, have many accounts in the same bank or for any other reason need
to re-use the rules file you can specify it explicitly with the
`--rules-file` option.

Here's an example rules file for converting csv data from a Wells Fargo
checking account in the USA:

    ; field definitions

    date-field 0
    description-field 4
    amount-field 1
    currency $
    base-account assets:bank:checking

    ; account-assigning regexps:
    
    SPECTRUM
    expenses:health:gym
    
    ITUNES
    BLKBSTR=BLOCKBUSTER
    expenses:entertainment
    
    (TO|FROM) SAVINGS
    assets:bank:savings

This says:

-   the first csv field is the date, the second is the amount, the
    fifth is the description

-   prepend a dollar sign to the amount field

-   the account corresponding to this csv file is
    assets:bank:checking

-   if description contains SPECTRUM (case-insensitive), the
    transaction is a gym expense

-   if description contains ITUNES or BLKBSTR, the transaction is
    an entertainment expense; also rewrite BLKBSTR as BLOCKBUSTER

-   if description contains TO SAVINGS or FROM SAVINGS, the
    transaction is a savings transfer

Here are the available rules file directives.  All are optional and will
use defaults if not specified. They are written one per line, at the
beginning of the rules file. Each directive is a name and a value
separated by whitespace. Note: watch out for parse errors in directives,
they may not be reported clearly.

####### account-field

If the CSV file contains data corresponding to several accounts (for
example - bulk export from other accounting software), you can use
account-field to override value of base-account. When account-field value
is empty, base-account will be used.

####### account2-field

If the CSV file contains fields for both accounts in the transaction, you
can use account2-field in addition to account-field.  If account2-field is
unspecified, the [account-assigning rules](#account-assigning-rules) are used.

####### amount-field

This directive specifies the CSV field containing the transaction amount.

As well as a simple number, the amount can be a hledger-style total or
per-unit price. For example, lets assume that your base account
"bank-current" is in GBP, and your CSV specifies amount of "10 USD @@ 15
GBP", and account-assigning rules selected account "travel-expenses" for
this transaction. As a result, "travel-expenses" would be credited by "10
USD @@ 15 GBP", and "bank-current" would be debited by "-15 GBP". This way
you could track the expenses in the currencies there were made, while
keeping your base account in single currency

####### code-field

####### currency-field

####### date-field

####### date-format

The date-format directive specifies a custom format for the date field, in
the same way as Haskell's
[formatTime](http://hackage.haskell.org/packages/archive/time/latest/doc/html/Data-Time-Format.html#v:formatTime)
function. The '%d' and '%m' specifiers expect leading zeroes. The '%y'
specifier works better when hledger is built with version 1.2.0.5 or
greater of the time library.

####### description-field

This directive can specify a simple field number like the others, or a
custom format in order to combine more than one CSV field. For example,
given the CSV record:

    11/2009/09,"Flubber Co",50,"My comment"

the directive:

    description-field %(1)/%(3)

will generate a transaction with this description:

    Flubber Co/My comment

####### effective-date-field

####### in-field

####### out-field

If the CSV file uses two different columns for in and out movements, use
the `in-field` and `out-field` directives instead of `amount-field`.  Note
that the numbers are assumed to be positive, implying that an "out"
movement gets recorded as a transaction with a negative amount.

####### status-field

####### base-account

####### currency

###### account-assigning rules

The remainder of the file is account-assigning rules. Each is a paragraph
consisting of one or more description-matching patterns (case-insensitive
regular expressions), one per line, followed by the account name to use
when the transaction's description matches any of these patterns.

A match pattern may be followed by a replacement pattern, separated by
`=`, which rewrites the matched part of the description. Use this if you
want to clean up messy bank data. To rewrite the entire description, use a
match pattern like `.*PAT.*=REPL`. Within a replacement pattern, you can
refer to the matched text with `\0` and any regex groups with `\1`, `\2`
in the usual way.

###### comments

Lines beginning with ; or \# are ignored (but avoid using comments inside an account rule).



#### test

This command runs hledger's internal self-tests and displays a quick
report. The -v option shows more detail, and a pattern can be provided to
filter tests by name. It's mainly used in development, but it's also nice
to be able to test for smoke at any time.

Examples:

    $ hledger test
    $ hledger test -v balance

### Reporting commands

These are the commands for querying your ledger.

#### balance

The balance command displays accounts and their balances, indented to show the account hierarchy.
Examples:

    $ hledger balance
    $ hledger balance food -p 'last month'

A final total is displayed, use `--no-total` to suppress this. Also, the
`--depth N` option shows accounts only to the specified depth, useful for
an overview:

    $ for y in 2006 2007 2008 2009 2010; do echo; echo $y; hledger -f $y.journal balance ^expenses --depth 2; done

With `--flat`, a non-hierarchical list of full account names is displayed
instead. This mode shows just the accounts actually contributing to the
balance, making the arithmetic a little more obvious to non-hledger users.
In this mode you can also use `--drop N` to elide the first few account
name components. Note `--depth` doesn't work too well with `--flat` currently;
it hides deeper accounts rather than aggregating them.

#### print

The print command displays full transactions from the journal file, tidily
formatted and showing all amounts explicitly. The output of print is
always a valid hledger journal.

hledger's print command also shows all unit prices in effect, or (with
-B/--cost) shows cost amounts.

Examples:

    $ hledger print
    $ hledger print employees:bob | hledger -f- register expenses

#### register

The register command displays postings, one per line, and their running total.
With no [filter patterns](#filter-patterns), this is not all that different from [print](#print):

    $ hledger register

More typically, use it to see a specific account's activity:

    $ hledger register assets:bank:checking

The `--depth` option limits the amount of sub-account detail displayed:

    $ hledger register assets:bank:checking --depth 2

With a [reporting interval](#reporting-interval) it shows aggregated
summary postings within each interval:

    $ hledger register --monthly rent
    $ hledger register --monthly -E food --depth 4

#### activity

The activity command displays a quick textual bar chart showing
transaction counts by day, week, month or other reporting interval.

Examples:

    $ hledger activity -p weekly dining

#### stats

The stats command displays summary information for the whole journal, or
a matched part of it.

Examples:

    $ hledger stats
    $ hledger stats -p 'monthly in 2009'

### Add-on commands

The following extra commands will be available if they have been
[installed](#installing) (run `hledger` by itself to find out):

#### chart

The chart command saves an image file, by default "hledger.png", showing a
basic pie chart of your top account balances. Note that positive and
negative balances will not be displayed together in the same chart; any
balances not matching the sign of the first one will be ignored.

chart-specific options:

    -o/--chart-output=IMGFILE  output filename (default: hledger.png)

You can specify a different output file name with -o/--output. The data
currently will always be in PNG format.

    --chart-items=N            number of accounts to show (default: 10)

The number of top accounts to show (default is 10).

    --chart-size=WIDTHxHEIGHT  image size (default: 600x400)

You can adjust the image resolution with --size=WIDTHxHEIGHT (in pixels).

To show only accounts above a certain depth, use the --depth option;
otherwise the chart can include accounts of any depth. When a parent and
child account both appear in a chart, the parent's balance will be
exclusive of the child's.

Examples:

    $ hledger chart assets --depth 2
    $ hledger chart liabilities --depth 2
    $ hledger chart ^expenses -o balance.png --size 1000x600 --items 20
    $ for m in 01 02 03 04 05 06 07 08 09 10 11 12; do hledger chart -p 2009/$m ^expenses --depth 2 -o expenses-2009$m.png --size 400x300; done

#### vty

The vty command starts a simple curses-style (full-screen, text) user
interface, which allows interactive navigation of the
print/register/balance reports. This lets you browse around and explore
your numbers quickly with less typing.

vty-specific options:

    --debug-vty  run with no terminal output, showing console

Examples:

    $ hledger vty
    $ hledger vty -BE food

#### web

The web command runs a HTTP server providing a web-based user interface
([release demo](http://demo.hledger.org),
[latest demo](http://demo.hledger.org:5001)).
The web UI provides reporting, including a more useful account register
view, and also data entry and modification.

web-specific options:

    --port=N           serve on tcp port N (default 5000)
    --base-url=URL     use this base url (default http://localhost:PORT)

If you want to visit the web UI from other machines, you'll need to use
this option to fix the hyperlinks. Just give your machine's host name or
ip address instead of localhost. This option is also lets you conform to a
custom url scheme when running hledger-web behind a reverse proxy as part
of a larger site. Note that the PORT in the base url need not be the same
as the `--port` argument.

**Warning:**
Unlike other hledger commands, `web` can alter existing journal data, via
the edit form.  A numbered backup of the file will be saved on each edit,
normally (ie if file permissions allow, disk is not full, etc.)  Also,
there is no built-in access control. So unless you run it behind an
authenticating proxy, any visitor to your server will be able to see and
overwrite the journal file (and included files.)

**Support files**

hledger-web requires certain support files (images, stylesheets,
javascript etc.) to be present in a particular location when it
runs. Specifically, they need to be in a `web` directory, under the
`.hledger` directory, under the current directory when you start
hledger-web. To make this easy, hledger-web will auto-create these files
in `./.hledger/web/` if they do not exist. Currently, after doing this
it will exit, with a notice explaining what happened, and you'll have to
run it a second time.

The above is a compromise to satisfy certain technical constraints, but
there is an advantage: you can easily customise the web UI's appearance
(even while it is running) by editing these files. This is useful eg for
integrating with an existing site. You can also run with different
customisations by starting hledger-web from a different current
directory. Note this means you should be aware of where you start
hledger-web from, otherwise it may just create a new copy of the support
files and ignore your stylings. To keep things simple you might choose to
always run it from the same place, eg your home directory.

Also note that when you upgrade hledger-web in future, these files will
need to be upgraded too, probably by removing them and letting them be
recreated.  So if you do customise them, remember what you changed; a
version control system such as darcs will help here.

**File changes are detected**

As noted, changes to the support files will take effect immediately,
without a restart.  This applies to the journal data too; you can directly
edit the journal file(s) (or, eg, commit a change within a version control
system) while the web UI is running, and the changes will be visible on
the next page reload.

**Malformed edits are rejected**

The journal file must remain in good [hledger format](#the-journal-file) so
that hledger can parse it. The web add and edit forms ensure this by not
allowing edits which would introduce parse errors. If a direct edit makes
the journal file unparseable, the web UI will show the error instead of
data, until the file has been fixed.

Examples:

    $ hledger-web
    $ hledger-web -E -B --depth 2 -f some.journal
    $ hledger-web --port 5010 --base-url http://some.vhost.com --debug

## Reporting options

The following additional features and options allow for fine-grained
reporting. They are common to most commands, where applicable.

### Filter patterns

Most commands accept one or more filter pattern arguments after the
command name, to select a subset of transactions or postings. There are
two kinds of pattern:

-   an account pattern, which is a regular expression. This is
    matched against postings' accounts. Optionally, it may be prefixed
    with `not:` in which case the match is negated.

-   a description pattern, like the above but prefixed with
    `desc:`. This is matched against transactions' descriptions. Note,
    when negating a desc: pattern, not: goes last, eg:
    `desc:not:someregexp`.


When you specify multiple filter patterns, hledger generally selects the
transactions or postings which match (or negatively match)

> *any of the account patterns* AND
> *any of the description patterns*

The [print](#print) command selects transactions which

> *match any of the description patterns* AND
> *have any postings matching any of the positive account patterns*
> AND
> *have no postings matching any of the negative account patterns*

### Smart dates

Unlike the journal file, hledger's user interface accepts more flexible
"smart dates", for example in the `-b` and `-e` options, period
expressions, display expressions, the add command and the web add form.
Smart dates allow some natural english words, will assume 1 where
less-significant date parts are unspecified, and can be relative to
today's date. Examples:

- `2009/1/1`, `2009/01/01`, `2009-1-1`, `2009.1.1` (simple dates)
- `2009/1`, `2009` (these also mean january 1, 2009)
- `1/1`, `january`, `jan`, `this year` (relative dates, meaning january 1 of this year)
- `next year` (january 1, next year)
- `this month` (the 1st of the current month)
- `this week` (the most recent monday)
- `last week` (the monday of the week before this one)
- `today`, `yesterday`, `tomorrow`

Spaces in smart dates are optional, so eg: `-b lastmonth` is valid.

### Period expressions

hledger supports flexible "period expressions" with the `-p/--period`
option to select transactions within a period of time (eg in 2009) and/or
with a reporting interval (eg weekly). hledger period expressions are
similar but not identical to c++ ledger's.

Here is a basic period expression specifying the first quarter of 2009.
Note the start date is always included and the end date is always excluded:

    -p "from 2009/1/1 to 2009/4/1"

Keywords like "from" and "to" are optional, and so are the spaces.  Just
don't run two dates together:

    -p2009/1/1to2009/4/1
    -p"2009/1/1 2009/4/1"

Dates are [smart dates](#smart-dates), so if the current year is 2009, the
above can also be written as:

    -p "1/1 to 4/1"
    -p "january to apr"
    -p "this year to 4/1"

If you specify only one date, the missing start or end date will be the
earliest or latest transaction in your journal:

    -p "from 2009/1/1"  (everything after january 1, 2009)
    -p "from 2009/1"    (the same)
    -p "from 2009"      (the same)
    -p "to 2009"        (everything before january 1, 2009)

A single date with no "from" or "to" defines both the start and end date
like so:

    -p "2009"           (the year 2009;    equivalent to "2009/1/1 to 2010/1/1")
    -p "2009/1"         (the month of jan; equivalent to "2009/1/1 to 2009/2/1")
    -p "2009/1/1"       (just that day;    equivalent to "2009/1/1 to 2009/1/2")

The `-b/--begin` and `-e/--end` options may be used as a shorthand for `-p
'from ...'` and `-p 'to ...'` respectively.

Note, however: a `-p/--period` option in the command line will cause any
`-b`/`-e`/`-D`/`-W`/`-M`/`-Q`/`-Y` flags to be ignored.

### Reporting interval

Period expressions can also begin with (or be) a reporting interval, which
affects commands like [register](#register) and [activity](#activity).
The reporting interval can be `daily`, `weekly`, `monthly`, `quarterly`, `yearly`,
or one of the `every ...` expressions below, optionally followed by `in`.
Examples:

    -p "weekly from 2009/1/1 to 2009/4/1"
    -p "monthly in 2008"
    -p "bimonthly from 2008"
    -p "quarterly"
    -p "every 2 weeks"
    -p "every 5 days from 1/3"
    -p "every 15th day of month"
    -p "every 4th day of week"

A reporting interval may also be specified with the `-D/--daily`,
`-W/--weekly`, `-M/--monthly`, `-Q/--quarterly`, and `-Y/--yearly`
options. But as noted above, a --period option will override these.

### Display expressions

Unlike a [period expression](#period-expressions), which selects the
transactions to be used for calculation, a display expression (specified
with `-d/--display`) selects which transactions will be displayed. This
useful, say, if you want to see your checking register just for this
month, but with an accurate running balance based on all transactions. Eg:

    $ hledger register checking --display "d>=[1]"

meaning "make a register report of all checking transactions, but display
only the ones with date on or after the 1st of this month." This the only
kind of display expression we currently support, ie transactions before or
after a given (smart) date.

### Depth limiting

With the `--depth N` option, reports will show only the uppermost accounts
in the account tree, down to level N. See the [balance](#balance),
[register](#register) and [chart](#chart) examples.

### Timelog reporting

hledger can also read time log files in (a subset of) timeclock.el's
format, containing clock-in and clock-out entries like so:

    i 2009/03/31 22:21:45 projects:A
    o 2009/04/01 02:00:34

hledger treats the clock-in description ("projects:A") as an account name,
and creates a virtual transaction (or several - one per day) with the
appropriate amount of hours. From the time log above, hledger print gives:

    2009/03/31 * 22:21-23:59
        (projects:A)          1.6h
    
    2009/04/01 * 00:00-02:00
        (projects:A)          2.0h

Here is a
[sample.timelog](http://joyful.com/repos/hledger/data/sample.timelog) to
download and some queries to try:

    hledger -f sample.timelog balance                               # current time balances
    hledger -f sample.timelog register -p 2009/3                    # sessions in march 2009
    hledger -f sample.timelog register -p weekly --depth 1 --empty  # time summary by week

To generate time logs, ie to clock in and clock out, you could:

- use emacs and the built-in timeclock.el, or
  the extended [timeclock-x.el](http://www.emacswiki.org/emacs/timeclock-x.el)
  and perhaps the extras in [ledgerutils.el](http://joyful.com/repos/ledgertools/ledgerutils.el)

- at the command line, use these bash aliases:

        alias ti="echo i `date '+%Y-%m-%d %H:%M:%S'` \$* >>$TIMELOG"
        alias to="echo o `date '+%Y-%m-%d %H:%M:%S'` >>$TIMELOG"

- or use the old `ti` and `to` scripts in the [c++ ledger 2.x repository](https://github.com/jwiegley/ledger/tree/maint/scripts).
  These rely on a "timeclock" executable which I think is just the ledger 2 executable renamed.

### Custom output formats

The `--format FMT` option will customize the line format of the balance
command's output (only, for now). `FMT` is a C printf/strftime-style
format string, with the exception that field names are enclosed in
parentheses:

    %[-][MIN][.MAX]([FIELD])

If the minus sign is given, the text is left justified. The `MIN` field
specified a minimum number of characters in width. After the value is
injected into the string, spaces is added to make sure the string is at
least as long as `MIN`. Similary, the `MAX` field specifies the maximum
number of characters. The string will be cut if the injected string is too
long.

- `%-(total)   ` the total of an account, left justified
- `%20(total)  ` The same, right justified, at least 20 chars wide
- `%.20(total) ` The same, no more than 20 chars wide
- `%-.20(total)` Left justified, maximum twenty chars wide

The following `FIELD` types are currently supported:

- `account` inserts the account name
- `depth_spacer` inserts a space for each level of an account's
  depth. That is, if an account has two parents, this construct will
  insert two spaces. If a minimum width is specified, that much space is
  inserted for each level of depth. Thus `%5_`, for an account with four
  parents, will insert twenty spaces.
- `total` inserts the total for the account

Examples:

If you want the account before the total you can use this format:

    $ hledger balance --format "%20(account) %-(total)"
                  assets $-1
             bank:saving $1
                    cash $-2
                expenses $2
                    food $1
                supplies $1
                  income $-2
                   gifts $-1
                  salary $-1
       liabilities:debts $1
    --------------------
                       0

Or, if you'd like to export the balance sheet:

    $ hledger balance --format "%(total);%(account)" --no-total
    $-1;assets
    $1;bank:saving
    $-2;cash
    $2;expenses
    $1;food
    $1;supplies
    $-2;income
    $-1;gifts
    $-1;salary
    $1;liabilities:debts

The default output format is `%20(total)  %2(depth_spacer)%-(account)`


## Appendices

### Compatibility with c++ ledger

hledger mimics a subset of ledger 3.x, and adds some features of its own.
We currently support:

- regular journal transactions
- journal format (we should be able to parse most ledger journals)
- timelog format
- multiple commodities
- prices and price history (with non-changing prices)
- virtual postings
- filtering by account and description
- print, register & balance commands
- period expressions quite similar to ledger's
- display expressions containing just a simple date predicate
- basic support (read: incomplete) for display formatting

We do not support:

- periodic and modifier transactions
- fluctuating prices
- display formats (actually, a small subset is supported)
- budget reports

And we add these commands:

- add
- chart
- vty
- web

#### Implementation

Unlike c++ ledger, hledger is written in the Haskell programming
language. Haskell enables a coding style known as pure lazy functional
programming, which holds the promise of more robust and maintainable
software built with fewer lines of code. Haskell also provides a more
abstracted, portable platform which can make deployment and installation
easier in some cases. Haskell also brings some new challenges such as
managing memory growth.

#### File format compatibility

hledger's file format is mostly identical with that of c++ ledger, with
some features being accepted but ignored (eg, modifier entries and
periodic entries). There are subtle differences in parser behaviour, eg
comments may be permissible in different places. hledger does not allow
separate dates for individual postings, or AMT1=AMT2 or { } syntax.

Generally, it's easy to keep a journal file that works with both hledger
and c++ ledger if you avoid these.  Occasionally you'll need to make small
adjustments to restore compatibility for one or the other.

See also:
[other differences](#other-differences),
[usage issues](#usage-issues).

#### Features not supported

c++ ledger features not currently supported include: modifier and periodic
entries, and the following c++ ledger options and commands:

    Basic options:
    -o, --output FILE      write output to FILE
    -i, --init-file FILE   initialize ledger using FILE (default: ~/.ledgerrc)
    -a, --account NAME     use NAME for the default account (useful with QIF)
    
    Report filtering:
    -c, --current          show only current and past entries (not future)
        --period-sort EXPR sort each report period's entries by EXPR
    -L, --actual           consider only actual (non-automated) transactions
    -r, --related          calculate report using related transactions
        --budget           generate budget entries based on periodic entries
        --add-budget       show all transactions plus the budget
        --unbudgeted       show only unbudgeted transactions
        --forecast EXPR    generate forecast entries while EXPR is true
    -l, --limit EXPR       calculate only transactions matching EXPR
    -t, --amount EXPR      use EXPR to calculate the displayed amount
    -T, --total EXPR       use EXPR to calculate the displayed total
    
    Output customization:
    -n, --collapse         Only show totals in the top-most accounts.
    -s, --subtotal         other: show subtotals
    -P, --by-payee         show summarized totals by payee
    -x, --comm-as-payee    set commodity name as the payee, for reporting
        --dow              show a days-of-the-week report
    -S, --sort EXPR        sort report according to the value expression EXPR
    -w, --wide             for the default register report, use 132 columns
        --head COUNT       show only the first COUNT entries (negative inverts)
        --tail COUNT       show only the last COUNT entries (negative inverts)
        --pager PAGER      send all output through the given PAGER program
    -A, --average          report average transaction amount
    -D, --deviation        report deviation from the average
    -%, --percentage       report balance totals as a percentile of the parent
        --totals           in the "xml" report, include running total
    -j, --amount-data      print only raw amount data (useful for scripting)
    -J, --total-data       print only raw total data
    -y, --date-format STR  use STR as the date format (default: %Y/%m/%d)
    -F, --format STR       use STR as the format; for each report type, use:
        --balance-format      --register-format       --print-format
        --plot-amount-format  --plot-total-format     --equity-format
        --prices-format       --wide-register-format
    
    Commodity reporting:
        --price-db FILE    sets the price database to FILE (def: ~/.pricedb)
    -L, --price-exp MINS   download quotes only if newer than MINS (def: 1440)
    -Q, --download         download price information when needed
    -O, --quantity         report commodity totals (this is the default)
    -V, --market           report last known market value
    -g, --performance      report gain/loss for each displayed transaction
    -G, --gain             report net gain/loss
    
    Commands:
    xml      [REGEXP]...   print matching entries in XML format
    equity   [REGEXP]...   output equity entries for matching accounts
    prices   [REGEXP]...   display price history for matching commodities
    entry DATE PAYEE AMT   output a derived entry, based on the arguments

#### Other differences

-   hledger recognises description and negative patterns by "desc:"
    and "not:" prefixes, unlike ledger 3's free-form parser

-   hledger doesn't require a space before command-line option
    values, eg either `-f-` or `-f -` is fine

-   hledger's weekly reporting intervals always start on mondays

-   hledger shows start and end dates of the intervals requested,
    not just the span containing data

-   hledger always shows timelog balances in hours

-   hledger splits multi-day timelog sessions at midnight

-   hledger doesn't track the value of commodities with varying
    price; prices are fixed as of the transaction date

-   hledger's output follows the decimal point character, digit grouping,
    and digit group separator character used in the journal.

-   hledger print shows amounts for all postings, and shows unit
    prices for amounts which have them. (This currently means that
    it does not print multi-commodity transactions in valid journal format.)

  - hledger's default commodity directive (D) sets the commodity for
    subsequent commodityless amounts, and contributes to that commodity's
    display settings. ledger uses D only for commodity display settings
    and for the entry command.

-   hledger generates a description for timelog sessions, instead of
    taking it from the clock-out entry

### Troubleshooting

#### Installation issues

cabal builds a lot of fast-evolving software, and it's not always smooth
sailing.  Here are some known issues and things to try:

- **Ask for help on [#hledger](irc://freenode.net/#hledger) or [#haskell](irc://freenode.net/#haskell).**
  Eg: join the #hledger channel with your IRC client and type: "sm: I did ... and ... happened", then leave
  that window open until you get helped.

- **Did you cabal update ?** If you didn't already, `cabal update` and try again.

- **Do you have a new enough version of GHC ?** hledger supports GHC 6.10
  and 6.12. Building with the `-fweb` flag requires 6.12 or greater.

- **An error while building non-hledger packages.**
  Resolve these problem packages one at a time. Eg, cabal install pkg1.
  Look for the cause of the failure near the end of the output. If it's
  not apparent, try again with `-v2` or `-v3` for more verbose output.

- **ExitFailure 11 from cabal**
  Probably http://hackage.haskell.org/trac/hackage/ticket/777

- **Could not run happy.**
  A package (eg haskell-src-exts) needs to run the `happy` executable.
  If not using the haskell platform, install the appropriate platform
  package which provides it (eg apt-get install happy).

- <a name="iconv" />**Undefined symbols: ... _iconv ...**
  If cabal gives this error:

        Linking dist/build/hledger/hledger ...
        Undefined symbols:
          "_iconv_close", referenced from:
              _hs_iconv_close in libHSbase-4.2.0.2.a(iconv.o)
          "_iconv", referenced from:
              _hs_iconv in libHSbase-4.2.0.2.a(iconv.o)
          "_iconv_open", referenced from:
              _hs_iconv_open in libHSbase-4.2.0.2.a(iconv.o)

    you are probably on a mac with macports libraries installed, causing
    [this issue](http://hackage.haskell.org/trac/ghc/ticket/4068).
    To work around temporarily, add this --extra-lib-dirs flag:

        $ cabal install hledger --extra-lib-dirs=/usr/lib

    or permanently, add this to ~/.cabal/config:
    
        extra-lib-dirs: /usr/lib

- **A ghc: panic! (the 'impossible' happened)** might be
    [this issue](http://hackage.haskell.org/trac/ghc/ticket/3862)

- **This package indirectly depends on multiple versions of the same package.**
  You may have previously installed some of hledger's dependencies
  depending on different versions of (eg) parsec. Then cabal install hledger gives
  an error like this:

        Warning: This package indirectly depends on multiple versions of the same
        package. This is highly likely to cause a compile failure.
        package yesod-0.5.0.3 requires parsec-2.1.0.1
        package csv-0.1.1 requires parsec-3.1.0
        ...

    The above example could be resolved by, eg:

        $ cabal install yesod --reinstall --constraint 'parsec == 3.1.0"

- **Another error while building a hledger package.**
    The current hledger release might have an error in its code or package
    dependencies. You could try [installing](#installing) the latest
    development version.

- **Do you have a new enough version of cabal-install ?**
  Recent versions tend to be better at resolving dependencies.  The error
  `setup: failed to parse output of 'ghc-pkg dump'` is another symptom of
  this.  To update, do:
  
        $ cabal update
        $ cabal install cabal-install
        $ cabal clean
        
    then try installing hledger again.

- **cabal fails to resolve dependencies.**
  It's possible for cabal to get confused, eg if you have
  installed/updated many cabal package versions or GHC itself. You can
  sometimes work around this by using cabal install's `--constraint`
  option. Another (drastic) way is to purge all unnecessary package
  versions by removing (or renaming) ~/.ghc, then trying cabal install
  again.

#### Usage issues

Here are some issues you might encounter when you run hledger:

- <a name="locale" />**non-ascii data gives "Illegal byte sequence" or "Invalid or incomplete multibyte or wide character" errors**

    hledger and other executables produced by GHC will give this error if
    asked to read a non-ascii file when a proper system locale is not
    configured. Eg, it's common for journal files to be UTF-8-encoded, in
    which case the system must have a UTF-8-aware locale installed and
    selected.  You can also select such a locale temporarily by setting
    the LANG environment variable on the command line. Here's an example,
    using ubuntu:
    
        $ file my.journal
        my.journal: UTF-8 Unicode text
        $ locale -a
        C
        en_US.utf8
        POSIX
        $ LANG=en_US.utf8 hledger -f my.journal print
  
    If we prefer, say, fr_FR.utf8, we'd better make sure it's installed:
    
        $ apt-get install language-pack-fr
        $ locale -a
        C
        en_US.utf8
        fr_BE.utf8
        fr_CA.utf8
        fr_CH.utf8
        fr_FR.utf8
        fr_LU.utf8
        POSIX
        $ LANG=fr_FR.utf8 hledger -f my.journal print

    Also note that on ubuntu variant spellings of "utf8", like "fr_FR.UTF8", are allowed,
    while on mac osx it must be exactly "fr_FR.UTF-8".

    Here's one way to set LANG permanently:
  
        $ echo "export LANG=en_US.UTF-8" >>~/.bash_profile
        $ bash --login

- **hledger fails to parse some valid ledger files**

    See [file format compatibility](#file-format-compatibility).

### Examples and recipes

-   Here's a bash function that will run hledger chart and display
    the image in your (graphical) emacs:

        function chart () {
          hledger chart $* && emacsclient -n hledger.png
        }

    Example:

        $ chart food --depth 2 -p jan

See also the [examples](http://joyful.com/repos/hledger/examples) directory.

### Other resources

- The rest of the [hledger.org](http://hledger.org) site.

- The [c++ ledger site](https://github.com/jwiegley/ledger/wiki).
  Also the [c++ ledger 2.x manual](http://joyful.com/repos/ledger/doc/ledger.html)
  is slightly outdated but informative.

- [Why you need accounting](http://podcastle.org/2009/10/09/pc-miniature-38-accounting-for-dragons)
