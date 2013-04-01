---
title: hledger user manual
---

# User manual

For: latest developer version

## Introduction

[hledger](http://hledger.org) is a program for tracking money, time,
or any other commodity, using a simple, editable file format and
double-entry accounting, inspired by and largely compatible with
[ledger](http://ledger-cli.org).  hledger is Free Software released
under GPL version 3 or later.

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

hledger works on linux, mac and windows. You can fund ready-to-run
binaries of the latest release - see the [download page](DOWNLOAD.html).
Otherwise, fetch and build the latest release from Hackage with cabal-install.
Eg:

    $ cabal update
    $ cabal install hledger [hledger-web]
    ...
    $ hledger --version
    hledger 0.19.4
    
For more help with this, see the [Installation Guide](INSTALL.html).

## Basic Usage

Basic usage is:

    $ hledger COMMAND [OPTIONS] [ARGS]

Most [commands](#commands) query or operate on a
[journal file](#the-journal-file), which by default is `.hledger.journal`
in your home directory. You can specify a different file with the `-f`
option or `LEDGER_FILE` environment variable, or standard input with `-f
-`.

Options are similar across most commands, with some variations; use
`hledger COMMAND --help` for details. Most options must appear somewhere
after COMMAND, not before it. The `-f` option can appear anywhere.

Arguments are also command-specific, but usually they form a
[query](#queries) which selects a subset of the journal, eg transactions
in a certain account.

To create an initial journal, run `hledger add` and follow the prompts to
enter some transactions.  Or, save this
[sample file](http://hub.darcs.net/simon/hledger/data/sample.journal) as
`.hledger.journal` in your home directory. Now try commands like these:

    $ hledger                               # show available commands
    $ hledger add                           # add more transactions to the journal file
    $ hledger balance                       # all accounts with aggregated balances
    $ hledger balance --help                # show help for balance command
    $ hledger balance --depth 1             # only top-level accounts
    $ hledger register                      # show a register of postings from all transactions
    $ hledger reg income                    # show postings to/from income accounts
    $ hledger reg checking                  # show postings to/from checking account
    $ hledger reg desc:shop                 # show postings with shop in the description
    $ hledger activity                      # show transactions per day as a bar chart
    
## Data formats

### Journal files

hledger's usual data source is a plain text file containing journal entries in hledger journal format.
This file represents a standard accounting [general journal](http://en.wikipedia.org/wiki/General_journal).
I use file names ending in `.journal`, but that's not required.
The journal file contains a number of transaction entries, 
each describing a transfer of money (or any commodity) between two or more named accounts,
in a simple format readable by both hledger and humans.

hledger's journal format is a compatible subset, mostly,
of [c++ ledger's journal format](http://ledger-cli.org/3.0/doc/ledger3.html#Journal-Format),
so hledger can work with [compatible](LEDGER.html) c++ ledger journal files as well.
It's safe, and encouraged, to run both hledger and ledger on the same journal file,
eg to validate the results you're getting.

You can use hledger without learning any more about this file; 
just use the [add](#add) or [web](#web) commands to create and update it. 
Many users, though, also edit the journal file directly with a text editor, perhaps assisted by the helper modes for emacs or vim.
Note the file uses unix line endings on all platforms. <!-- XXX retest & remove -->

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
    
    2008/12/31 * pay off            ; <- an optional * or ! after the date means "cleared" (or anything you want)
        liabilities:debts     $1
        assets:bank:checking

Now let's explore the available journal file syntax in detail.

#### Transactions

Each transaction begins with a date in column 0, followed by an optional
description, then two or more postings (of some amount to some account),
each on their own line.

The posting amounts within a transaction must always balance, ie add up to
0.  You can leave one amount blank and it will be inferred.

#### Account names

Account names typically have several parts separated by a full colon, from
which hledger derives a hierarchical chart of accounts. They can be
anything you like, but in finance there are traditionally five top-level
accounts: `assets`, `liabilities`, `income`, `expenses`, and `equity`.

Account names may contain single spaces, eg: `assets:accounts receivable`.

#### Amounts

After the account name, separated by ***two or more*** spaces, there is
usually an amount.  This is a number, optionally with a currency symbol or
commodity name on either the left or right. Commodity names which contain
more than just letters should be enclosed in double quotes.

Negative amounts usually have the minus sign next to the number: `$-1`.
Or it may go before the symbol: `-$1`.

hledger supports flexible decimal points and digit group separators so you
can use your country's convention.  Numbers can use either a period (`.`)
or a comma (`,`) as decimal point. They can also have digit group
separators at any position (eg thousands separators) which can be comma or
period - whichever one you did not use as a decimal point. If you use
digit group separators, you must also include a decimal point in at least
one number in the same commodity, so that hledger knows which character is
which. Eg, write `$1,000.00` or `$1.000,00`.

#### Amount styles

Based on how you format amounts, hledger will infer canonical display
styles for each commodity, and use these when displaying amounts in that
commodity. Amount styles include:

- the position (left or right) and spacing (space or no separator) of the commodity symbol
- the digit group separator character (comma or period) and digit group sizes, if any
- the decimal point character (period or comma)
- the display precision (number of decimal places displayed)

The canonical style is generally the style of the first amount seen in a commodity
(which may be in a [default commodity directive](#default-commodity).
The precision is the highest precision seen among all amounts in the commmodity.

#### Simple dates

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

#### Secondary dates

Real-life transactions sometimes involve more than one date - eg the date
you write a cheque, and the date it clears in your bank.  When you want to
model this, eg for more accurate balances, write both dates separated by
an equals sign. The *primary date*, on the left, is used by default; the
*secondary date*, on the right, is used when the `--date2` flag is specified
(`--aux-date` or `--effective` will also work).

Their meaning is up to you, but it's best to follow a consistent rule. I
write the bank's clearing date as primary, and the date I initiated the
transaction as secondary (if needed).

Example:

    ; PRIMARY=SECONDARY
    ; The secondary date's year is optional, defaulting to the primary's
    2010/2/23=2/19 movie ticket
      expenses:cinema                   $10
      assets:checking

    $ hledger register checking
    2010/02/23 movie ticket         assets:checking                $-10         $-10

    $ hledger register checking --date2
    2010/02/19 movie ticket         assets:checking                $-10         $-10

#### Posting dates

[Comments and tags](#comments) are covered below, but while we are talking
about dates: you can give individual postings a different date from their
parent transaction, by adding a posting tag like `date:DATE`, where DATE is
a [simple date](#simple-dates). The secondary date can be set with
`date2:DATE2`. If present, these dates will take precedence in reports.

Ledger's bracketed posting date syntax (`[DATE]`,
`[DATE=DATE2]` or `[=DATE2]` in a posting comment)
is also supported, as an alternate spelling of the date tags.

#### Default commodity

You can set a default commodity, to be used for any subsequent amounts
which have no commodity symbol, with the D directive:

    ; set british pound as default commodity
    ; also sets canonical style for pound amounts, since it's the first one
    ; (pound symbol on left, comma thousands separator, two decimal places)
    D £1,000.00
    
    2010/1/1
      a  2340    ; no symbol, will use pound
      b

A default commodity directive may also influence the canonical
[amount style](#commodity-display-settings) for the commodity.

#### Prices

##### Transaction prices

When recording an amount, you can also record its price in another
commodity. This documents an exchange rate that was applied within
this transaction (or to be precise, within the posting). There are
three ways to specify a transaction price:

1. Write the unit price (exchange rate) explicitly as `@ UNITPRICE` after the amount:

        2009/1/1
         assets:foreign currency   €100 @ $1.35  ; one hundred euros at $1.35 each
         assets:cash

2. Or write the total price for this amount as `@@ TOTALPRICE`:

        2009/1/1
         assets:foreign currency   €100 @@ $135  ; one hundred euros at $135 for the lot
         assets:cash

3. Or fully specify all posting amounts using exactly two commodities:

        2009/1/1
         assets:foreign currency   €100          ; one hundred euros
         assets:cash              $-135          ; exchanged for $135

You can use the `--cost/-B` flag with reporting commands to see such
amounts converted to their price's commodity. Eg, using any of the above
examples we get:

    $ hledger print --cost
    2009/01/01
        assets:foreign currency       $135.00
        assets                       $-135.00

##### Historical prices

You can also record a series of historical prices for a commodity using P
directives. Typically these are used to record daily market prices or
exchange rates. ledger uses them to calculate market value with -V, but
hledger currently ignores them. They look like this:
<!-- (A time and numeric time zone are allowed but ignored, like ledger.) -->

        ; Historical price directives look like: P DATE COMMODITYSYMBOL UNITPRICE
        ; These say the euro's exchange rate is $1.35 during 2009 and
        ; $1.40 from 2010/1/1 on.
        P 2009/1/1 € $1.35  
        P 2010/1/1 € $1.40
        
#### Balance Assertions

ledger supports
[balance assertions](http://ledger-cli.org/3.0/doc/ledger3.html#Balance-assertions):
following a posting's amount, an equals sign and another amount which is
the expected balance in this account at this point. hledger does not
currently enforce these but will ignore them, so you can put them in your
journal and test with ledger if needed.

#### Fixed Lot Prices

Similarly, we ignore ledger's 
[fixed lot prices](http://ledger-cli.org/3.0/doc/ledger3.html#Fixing-lot-prices).
hledger's [prices](#transaction-prices) always work like ledger's fixed lot prices.

#### Comments

A semicolon in the journal file marks the start of a comment. You can
write comments on their own line between transactions, like so:

    ; Also known as a "journal comment". Whitespace before the ; is allowed.

You can also write transaction- or posting-specific comments following the
transaction's first line or the posting, on the same line and/or indented
on following lines. Some examples:

    ; a journal comment
    2012/5/14 something  ; and now a transaction comment
      ; another comment for this transaction
      posting1  1  ; a comment for posting 1
      posting2
      ; a comment for posting 2
      ; another comment for posting 2
    ; another journal comment (because not indented)

Currently `print` preserves transaction and posting comments but not
journal comments.

A "tag comment" is a transaction or posting comment containing a tag,
explained in the next section.

#### Tags

You can attach named tags, optionally with values, to transactions and
postings, and then filter reports by tag (this is the same as Ledger's
[metadata](http://ledger-cli.org/3.0/doc/ledger3.html#Metadata) feature,
except our tag values are just strings.)

Tags names are unspaced words followed by a colon, anywhere within a
transaction or posting comment. Tag values are the (whitespace-trimmed)
text after a tag name, up to the next newline or comma (allowing multiple
tags on one line). For example:

    1/1 a transaction    ; TAG1: , TAG2: tag2's value
        ; TAG3: a third transaction tag
        a  $1  ; TAG4: a posting tag

Querying by tag is work in progress; for now you can test for existence of
a tag with `tag:NAME`.
<!-- tag:NAME=EXACTVALUE` -->

#### Including other files

You can pull in the content of additional journal files, by writing lines like this:

    !include path/to/file.journal

The `!include` directive may only be used in journal files, and currently
it may only include other journal files (eg, not timelog files.)

#### Default parent account

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

#### Account aliases

You can define account aliases to rewrite certain account names (and their subaccounts).
This tends to be a little more reliable than post-processing with sed or similar.
The directive is `alias ORIG = ALIAS`, where ORIG and ALIAS are full account names.
To forget all aliases defined to this point, use `end aliases`.
For an example, see [How to use account aliases](ALIASES.html).


### CSV files

hledger can also read
[CSV](http://en.wikipedia.org/wiki/Comma-separated_values) files,
translating the CSV records into journal entries on the fly. 
We must provide some some conversion hints in a "rules file", named
like the CSV file with an extra `.rules` suffix (you can choose another name with `--rules-file`).  

If the rules file does not exist, it will be created with default rules, which you'll need to tweak.
Here's a minimal rules file. It says that the first and second CSV fields
are the journal entry's date and amount:

    fields date, amount

Lines beginning with `#` or `;` and blank lines are ignored.
The following kinds of rule can appear in any order:

**fields** *CSVFIELDNAME1*, *CSVFIELDNAME1*, ...
:   (Field list) This names the CSV fields (names may not contain whitespace or `;` or `#`),
    and also assigns them to journal entry fields when you use any of these names:

        date
        date2
        status
        code
        description
        comment
        account1
        account2
        currency
        amount
        amount-in
        amount-out
   
*JOURNALFIELDNAME* *FIELDVALUE*
:   (Field assignment) This assigns the given text value
    to a journal entry field (one of the field names above).
    CSV fields can be referenced with `%CSVFIELDNAME` or `%N` (N starts at 1) and will be interpolated.

    You can use a field list, field assignments, or both.
    At least the `date` and `amount` fields must be assigned.

**if** *PATTERNS*<br>&nbsp;&nbsp;*FIELDASSIGNMENTS*
:   (Conditional block) This applies the field assignments only to CSV records matched by one of the PATTERNS.
    PATTERNS is one or more regular expressions on the same or following lines.
    <!-- then an optional `~` (indicating case-insensitive infix regular expression matching),\ -->
    These are followed by one or more indented field assignment lines.\
    In this example, any CSV record containing "groc" (case insensitive, anywhere within the whole record)
    will have its account2 and comment set as shown:

        if groc
         account2 expenses:groceries
         comment  household stuff

**skip** [*N*]
:   Skip this number of CSV records (1 by default).
    Use this to skip CSV header lines.
    <!-- hledger tries to skip initial CSV header lines automatically. -->
    <!-- If it guesses wrong, use this directive to skip exactly N lines. -->
    <!-- This can also be used in a conditional block to ignore certain CSV records. -->

**date-format** *DATEFMT*
:   This is required if the values for `date` or `date2` fields are not in YYYY/MM/DD format (or close to it).
    DATEFMT specifies a strptime-style date parsing pattern containing [year/month/date format codes](http://hackage.haskell.org/packages/archive/time/latest/doc/html/Data-Time-Format.html#v:formatTime).
    Some common values:

        %-d/%-m/%Y
        %-m/%-d/%Y
        %Y-%h-%d

Typically you'll keep one rules file for each account which you
download as CSV. For an example, see [How to read CSV files](CSV.html).

### Timelog files

hledger can also read time log files. These are (a subset of) timeclock.el's
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
[sample.timelog](http://hub.darcs.net/simon/hledger/data/sample.timelog) to
download and some queries to try:

    hledger -f sample.timelog balance                               # current time balances
    hledger -f sample.timelog register -p 2009/3                    # sessions in march 2009
    hledger -f sample.timelog register -p weekly --depth 1 --empty  # time summary by week

To generate time logs, ie to clock in and clock out, you could:

- use emacs and the built-in timeclock.el, or
  the extended [timeclock-x.el](http://www.emacswiki.org/emacs/timeclock-x.el)
  and perhaps the extras in [ledgerutils.el](http://hub.darcs.net/simon/ledgertools/ledgerutils.el)

- at the command line, use these bash aliases:

        alias ti="echo i `date '+%Y-%m-%d %H:%M:%S'` \$* >>$TIMELOG"
        alias to="echo o `date '+%Y-%m-%d %H:%M:%S'` >>$TIMELOG"

- or use the old `ti` and `to` scripts in the [c++ ledger 2.x repository](https://github.com/jwiegley/ledger/tree/maint/scripts).
  These rely on a "timeclock" executable which I think is just the ledger 2 executable renamed.

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
  match will provide defaults for the other fields. If you specify a
  [query](#queries) on the command line, only matching transactions will
  be considered as history.

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

#### test

This command runs hledger's built-in unit tests and displays a quick
report. A pattern can be provided to filter tests by name. It's mainly
used in development, but it's also nice to be able to check hledger for
smoke at any time.

Examples:

    $ hledger test
    $ hledger test -v balance

### Reporting commands

These are the commands for querying your ledger.

#### print

The print command displays full transactions from the journal file, tidily
formatted and showing all amounts explicitly. The output of print is
always a valid hledger journal, but it might not preserve the original
content absolutely intact (eg comments.)

hledger's print command also shows all unit prices in effect, or (with
-B/--cost) shows cost amounts.

Examples:

    $ hledger print
    $ hledger print employees:bob | hledger -f- register expenses

#### register

The register command displays postings, one per line, and their running
total.  With no [query terms](#queries), this is not all that different
from [print](#print):

    $ hledger register

More typically, use it to see a specific account's activity:

    $ hledger register assets:bank:checking

The `--depth` option limits the amount of sub-account detail displayed:

    $ hledger register assets:bank:checking --depth 2

With a [reporting interval](#reporting-interval) it shows aggregated
summary postings within each interval:

    $ hledger register --monthly rent
    $ hledger register --monthly -E food --depth 4

The `--width`/`-w` option adjusts the width of the output. By default,
this is 80 characters. To allow more space for descriptions and account
names, use `-w` to increase the width to 120 characters, or `-wN` to set
any desired width (at least 50 recommended, with no space before the N -
eg `-w200` or `--width=200`,

The `--related`/`-r` flag shows the *other* postings in the transactions
of the postings which would normally be shown.

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

#### incomestatement

This command displays a simple
[income statement](http://en.wikipedia.org/wiki/Income_statement).  It
currently assumes that you have top-level accounts named `income` (or
`revenue`) and `expense` (plural forms also allowed.)

#### balancesheet

This command displays a simple
[balance sheet](http://en.wikipedia.org/wiki/Balance_sheet). It currently
assumes that you have top-level accounts named `asset`, `liability` and
`equity` (plural forms also allowed.)

#### cashflow

This command displays a simplified
[cashflow statement](http://en.wikipedia.org/wiki/Cash_flow_statement)
(without the traditional segmentation into operating, investing, and
financing cash flows.) It shows the change in all "cash" accounts for the
period. It currently assumes that cash accounts are under a top-level
account named `asset` and do not contain `receivable` or `A/R` (plural
forms also allowed.)

#### activity

The activity command displays a simplistic textual bar chart showing
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

#### web

The web command (provided by the hledger-web package) runs a web
server providing a web-based user interface
([release demo](http://demo.hledger.org),
[latest demo](http://demo.hledger.org:5001)).  The web UI provides
reporting, including a more useful account register view, and also data
entry and editing.

web-specific options:

    --port=N           serve on tcp port N (default 5000)
    --base-url=URL     use this base url (default http://localhost:PORT)

If you want to visit the web UI from other machines, you'll need to use
this option to fix the hyperlinks. Just give your machine's host name or
ip address instead of localhost. This option is also lets you conform to a
custom url scheme when running hledger-web behind a reverse proxy as part
of a larger site. Note that the PORT in the base url need not be the same
as the `--port` argument.

Warning: unlike other hledger commands, `web` can alter existing journal
data, via the edit form.  A numbered backup of the file will be saved on
each edit, normally (ie if file permissions allow, disk is not full, etc.)
Also, there is no built-in access control. So unless you run it behind an
authenticating proxy, any visitor to your server will be able to see and
overwrite the journal file (and included files.)

hledger-web disallows edits which would leave the journal file not in
valid [journal format](#the-journal-file). If the file becomes unparseable
by other means, hledger-web will show an error until the file has been
fixed.

Examples:

    $ hledger-web
    $ hledger-web -E -B --depth 2 -f some.journal
    $ hledger-web --port 5010 --base-url http://some.vhost.com --debug

#### vty

The vty command (provided by the hledger-vty package) starts a simple
curses-style (full-screen, text) user interface, which allows interactive
navigation of the print/register/balance reports. This lets you browse
around and explore your numbers quickly with less typing.

vty-specific options:

    --debug-vty  run with no terminal output, showing console

Examples:

    $ hledger vty
    $ hledger vty -BE food

#### chart

The chart command (provided by the hledger-chart package) saves an image
file, by default "hledger.png", showing a basic pie chart of your top
account balances. Note that positive and negative balances will not be
displayed together in the same chart; any balances not matching the sign
of the first one will be ignored.

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

## Reporting options

The following additional features and options allow for fine-grained
reporting. They are common to most commands, where applicable.

### Queries

Most commands accept an optional query expression, written as arguments
after the command name, to filter the data (or in some cases, to modify
the output). The syntax is similar to a Google search expression: one or
more space-separated search terms, optional prefixes to match specific
fields, quotes to enclose whitespace etc. Each query term can be any of
the following:

- `REGEX` - match account names by this regular expression
- `acct:REGEX` - same as above
- `desc:REGEX` - match transaction descriptions by regular expression
- `date:PERIODEXPR` - match dates within the specified [period](#period-expressions)
- `edate:PERIODEXPR` - as above, but match secondary dates
- `status:1` or `status:0` - match cleared/uncleared transactions
- `tag:NAME[=REGEX]` - match by exact [tag](#tags) name, and optionally match the tag value by regular expression
- `depth:N` - match (or display, depending on command) accounts at or above this [depth](#depth-limiting)
- `not:` before any of the above negates the match

<!--
- `TAGNAME:[TAGVALUEREGEX]` - match a tag name exactly, and optionally
  the value by regular expression.
- `code:CODEREGEX`
- `type:regular|virtual|balancedvirtual`
- `comment:COMMENTREGEX`
- `amount:AMOUNTEXPR`
- `commodity:COMMODITYSYMBOLREGEX`
Any of these can be prefixed with `not:` or `!` to negate the match.
-->

Multiple query terms will select transactions/postings/accounts which match
(or negatively match)

> *any of the description terms AND*  
> *any of the account terms AND*  
> *all the other terms*

With the [print](#print) command, they select transactions which

> *match any of the description terms AND*  
> *have any postings matching any of the positive account terms AND*  
> *have no postings matching any of the negative account terms AND*  
> *match all the other terms*

Note many of the above query terms can also be expressed as command-line
flags; you can use either, or both at once.

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

### Troubleshooting

Here are some issues you might encounter when you run hledger:
Please also seek
[support](DEVELOPMENT.html#support) from the
[IRC channel](irc://irc.freenode.net/#ledger),
[mail list](http://list.hledger.org) or
[bug tracker](http://bugs.hledger.org).

#. **hledger installed, but running hledger says something like No command 'hledger' found**  
  cabal installs binaries into a special directory, which should be added
  to your PATH environment variable.  On unix-like systems, it is
  ~/.cabal/bin.

#. **hledger fails to parse some valid ledger files**  
  See [file format compatibility](#file-format-compatibility).

#. <a name="locale" />**hledger gives "Illegal byte sequence" or "Invalid or incomplete multibyte or wide character" errors**  
  In order to handle non-ascii letters and symbols (like £), hledger needs
  an appropriate locale. This is usually configured system-wide; you can
  also configure it temporarily.  The locale may need to be one that
  supports UTF-8, if you built hledger with GHC < 7.2 (or possibly always,
  I'm not sure yet).
  
    Here's an example of setting the locale temporarily, on ubuntu gnu/linux:
    
        $ file my.journal
        my.journal: UTF-8 Unicode text                 # <- the file is UTF8-encoded
        $ locale -a
        C
        en_US.utf8                             # <- a UTF8-aware locale is available
        POSIX
        $ LANG=en_US.utf8 hledger -f my.journal print   # <- use it for this command

     Here's one way to set it permanently, there are probably better ways:

        $ echo "export LANG=en_US.UTF-8" >>~/.bash_profile
        $ bash --login

     If we preferred to use eg `fr_FR.utf8`, we might have to install that first:

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

    Note some platforms allow variant locale spellings, but not all (ubuntu
    accepts `fr_FR.UTF8`, mac osx requires exactly `fr_FR.UTF-8`).

### Examples and recipes

-   Here's a bash function that will run hledger chart and display
    the image in your (graphical) emacs:

        function chart () {
          hledger chart $* && emacsclient -n hledger.png
        }

    Example:

        $ chart food --depth 2 -p jan

See also the [extra](http://hub.darcs.net/simon//hledger/extra) directory.

### Other resources

- The rest of the [hledger.org](http://hledger.org) site.

- The [c++ ledger site](http://ledger-cli.org) and highly informative [manual](http://ledger-cli.org/3.0/doc/ledger3.html).

- [Why you need accounting](http://podcastle.org/2009/10/09/pc-miniature-38-accounting-for-dragons)
