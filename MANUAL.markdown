---
title: hledger manual
---

# hledger manual

## About

hledger is a program for tracking money, time, or any other commodity,
using a simple, editable file format and the powerful principles of
double-entry accounting. It was inspired by [ledger](#faq).

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

hledger is copyright (c) 2007-2010
[Simon&nbsp;Michael&nbsp;<simon@joyful.com>](mailto:simon@joyful.com) and
contributors, and released as Free Software under GPL version 3 or later.

This is the manual for hledger 0.13.0.

## Frequently asked questions

- **How does hledger relate to John Wiegley's ledger project ?**

    hledger was very much inspired by, and is partly a clone of,
    [ledger](http://wiki.github.com/jwiegley/ledger) (also called "c++
    ledger" in these docs.) I was a happy ledger user and contributor for
    some time, and I still use it occasionally. I wrote hledger because I
    wanted to develop financial tools in the Haskell programming language
    and ecosystem, whose advantages I believe are compelling. I have also
    tried to make hledger a little more simple, usable, installable, and
    documented, and to provide alternate user interfaces and other
    enhancements to make it more widely useful.
    
    ledger has more advanced command-line power features (periodic
    transactions, budgets, capital gains tracking, value expressions,
    custom output formats, ...)  and it remains faster and more memory
    efficient on large data files.
    
    The two projects (indeed the whole family of ledger-inspired projects)
    collaborate freely, and we share ledger's IRC channel (but have our
    own mail list.) We stay compatible with ledger wherever possible,
    intending that you can use both tools on the same data, each for its
    strengths.  Here is
    [more detail about compatibility](#compatibility-with-c-ledger).

## Installing

hledger works on all major platforms *(except microsoft windows, as of 
version 0.13; to be fixed)*. You can download and run current release binaries from
the [download page](DOWNLOAD.html).

Or, you can build the current release from source using cabal-install.
Ensure you have a working
[haskell environment](http://hackage.haskell.org/platform/), then:

    $ cabal update
    $ cabal install hledger

You can also build these optional [add-ons](#add-on-commands) providing
extra features:

    $ cabal install hledger-web
    $ cabal install hledger-vty
    $ cabal install hledger-chart

Or, you can build the latest [development version](http://joyful.com/darcsweb/darcsweb.cgi?r=hledger):

    $ cabal update
    $ darcs get --lazy http://joyful.com/repos/hledger
    $ cd hledger
    $ make install

You may encounter dependency issues when using cabal, which can often be
worked around by (a) being sure to cabal update, (b) using --constraint,
(c) unregistering obsolete package versions from your system.  Otherwise,
please see [Troubleshooting](#troubleshooting) and seek
[Support](DEVELOPMENT.html#support).

More installation tips:

- hledger-chart: requires additional GTK-related libraries and possibly [other things](http://code.haskell.org/gtk2hs/INSTALL). On ubuntu, install the `libghc6-gtk-dev` package.
- hledger-vty: requires curses-related libraries (ubuntu package: `libncurses5-dev`). Not buildable on microsoft windows, except possibly via cygwin.
- hledger-web: building requires GHC 6.12 or greater.

## Basic usage

Basic usage is:

    $ hledger [OPTIONS] COMMAND [FILTER PATTERNS]

hledger first looks for data in a specially-formatted
[journal file](#journal-file).  You can specify the file with the -f
option or the `LEDGER` environment variable; otherwise hledger assumes it
is a file named `.journal` in your home directory. If the journal file
does not exist, it will be auto-created.

[Options](#overview) may appear anywhere on the command line.

[Commands](#core-commands) are described below.  Note that most hledger
commands are read-only, that is they can not modify your data. The
exceptions are the add command which is append-only, and the (add-on) web
command which can change everything.

[Filter patterns](#filter-patterns) are often used to select a subset of the
journal data, eg to report only food-related transactions.

To try it out, just run `hledger add` and enter some transactions.  (Or,
save the [sample file](#journal-file) as `.journal` in your home
directory.) Now try some of these commands:

    $ hledger --help                        # show command-line help
    $ hledger add                           # add some new transactions to the journal file
    $ hledger balance                       # all accounts with aggregated balances
    $ hledger bal --depth 1                 # only top-level accounts
    $ hledger register                      # transaction register
    $ hledger reg income                    # transactions to/from an income account
    $ hledger reg checking                  # checking transactions
    $ hledger reg desc:shop                 # transactions with shop in the description
    $ hledger histogram                     # transactions per day, or other interval

You'll find more examples below.

<a name="faq" />

## Reference

### Overview

This version of hledger mimics a subset of ledger 3.x, and adds some
features of its own. We currently support regular journal transactions, timelog
entries, multiple commodities, (fixed) price history, virtual postings,
filtering by account and description, the familiar print, register &
balance commands and several new commands. We handle (almost) the full
period expression syntax, and very limited display expressions consisting
of a simple date predicate.

Here is the command-line help:

    Usage: hledger [OPTIONS] COMMAND [PATTERNS]
           hledger [OPTIONS] convert CSVFILE
           hledger [OPTIONS] stats

    hledger reads your ~/.journal file, or another specified with $LEDGER or -f FILE

    COMMAND is one of (may be abbreviated):
      add       - prompt for new transactions and add them to the journal
      balance   - show accounts, with balances
      convert   - read CSV bank data and display in journal format
      histogram - show a barchart of transactions per day or other interval
      print     - show transactions in journal format
      register  - show transactions as a register with running balance
      stats     - show various statistics for a journal
      test      - run self-tests

    PATTERNS are regular expressions which filter by account name.
    Prefix with desc: to filter by transaction description instead.
    Prefix with not: to negate a pattern. When using both, not: comes last.

    DATES can be y/m/d or ledger-style smart dates like "last month".

    Use --help-options to see OPTIONS, or --help-all/-H.

    Options:

      -f FILE  --file=FILE        use a different journal/timelog file; - means stdin
               --no-new-accounts  don't allow to create new accounts
      -b DATE  --begin=DATE       report on transactions on or after this date
      -e DATE  --end=DATE         report on transactions before this date
      -p EXPR  --period=EXPR      report on transactions during the specified period
                                  and/or with the specified reporting interval
      -C       --cleared          report only on cleared transactions
      -U       --uncleared        report only on uncleared transactions
      -B       --cost, --basis    report cost of commodities
               --depth=N          hide sub-accounts deeper than this
      -d EXPR  --display=EXPR     show only transactions matching EXPR (where
                                  EXPR is 'dOP[DATE]' and OP is <, <=, =, >=, >)
               --effective        use transactions' effective dates, if any
      -E       --empty            show empty/zero things which are normally elided
      -R       --real             report only on real (non-virtual) transactions
               --flat             balance: show full account names, unindented
               --drop=N           balance: with --flat, elide first N account name components
               --no-total         balance: hide the final total
      -D       --daily            register, stats: report by day
      -W       --weekly           register, stats: report by week
      -M       --monthly          register, stats: report by month
      -Q       --quarterly        register, stats: report by quarter
      -Y       --yearly           register, stats: report by year
      -v       --verbose          show more verbose output
               --debug            show extra debug output; implies verbose
               --binary-filename  show the download filename for this hledger build
      -V       --version          show version information
      -h       --help             show basic command-line usage
               --help-options     show command-line options
      -H       --help-all         show command-line usage and options

<a name="file-format" />

### Journal file

hledger reads data from a plain text file, called a *journal* because
it represents a standard accounting [general
journal](http://en.wikipedia.org/wiki/General_journal).  It contains a
number of transactions, each describing a transfer of money (or
any commodity) between two or more named accounts, in a simple
format readable by both hledger and humans.

You can use hledger without learning any more about this file; just
use the [add](#add) or [web](#web) commands.

Many users, though, edit the journal file directly with a text
editor. This is a distinguishing feature of hledger (and c++ ledger.)
You can even do this while the web interface is running, and see the
changes right away.

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

Each transaction has a date, optional description, and two or more
postings, of some amount to some account. The amounts within a transaction must balance,
ie add up to 0. Or, you can leave one amount blank and it will be inferred.

Note that account names may contain single spaces, while the amount must
be separated from the account name by at least two spaces.

An amount is a number, with an optional currency symbol or commodity name
on either the left or right. Commodity names which contain more than just
letters should be enclosed in double quotes. Negative amounts usually have
the minus sign next to the number (`$-1`), but it may also go before the
currency symbol/commodity name (`-$1`).

hledger's file format aims to be compatible with c++ ledger, so you
can use both tools on your journal. For more details, see [File format
compatibility](#file-format-compatibility).

### Core commands

These commands are provided by the main hledger package and are always
available. The most frequently used commands are [print](#print),
[register](#register) and [balance](#balance).

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

- Default commodity awareness: if the journal specifies a
  [default commodity directive](#default-commodity), that will be applied
  to any bare numbers entered.

Examples:

    $ hledger add
    $ hledger -f home.journal add equity:bob

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

#### convert

The convert command reads a
[CSV](http://en.wikipedia.org/wiki/Comma-separated_values) file you have
downloaded from your bank, and prints out the transactions in journal
format, suitable for adding to your journal. It does not alter your journal
directly.

This can be a lot quicker than entering every transaction by hand.  (The
downside is that you are less likely to notice if your bank makes an
error!) Use it like this:

    $ hledger convert FILE.csv >FILE.journal

where FILE.csv is your downloaded csv file. This will convert the csv data
using conversion rules defined in FILE.rules (auto-creating this file if
needed), and save the output into a temporary journal file. Then you should
review FILE.journal for problems; update the rules and convert again if
needed; and finally copy/paste transactions which are new into your main
journal.

<!-- ###### .rules file -->

convert requires a \*.rules file containing data definitions and rules for
assigning destination accounts to transactions; it will be auto-created if
missing. Typically you will have one csv file and one rules file per bank
account. Here's an example rules file for converting csv data from a Wells
Fargo checking account:

    base-account assets:bank:checking
    date-field 0
    description-field 4
    amount-field 1
    currency $
    
    ; account-assigning rules:
    
    SPECTRUM
    expenses:health:gym
    
    ITUNES
    BLKBSTR=BLOCKBUSTER
    expenses:entertainment
    
    (TO|FROM) SAVINGS
    assets:bank:savings

This says:

-   the account corresponding to this csv file is
    assets:bank:checking
-   the first csv field is the date, the second is the amount, the
    fifth is the description
-   prepend a dollar sign to the amount field
-   if description contains SPECTRUM (case-insensitive), the
    transaction is a gym expense
-   if description contains ITUNES or BLKBSTR, the transaction is
    an entertainment expense; also rewrite BLKBSTR as BLOCKBUSTER
-   if description contains TO SAVINGS or FROM SAVINGS, the
    transaction is a savings transfer

Notes:

-   Lines beginning with ; or \# are ignored (but avoid using inside an
    account rule)

-   Definitions must come first, one per line, all in one
    paragraph. Each is a name and a value separated by whitespace.
    Supported names are: base-account, date-field, status-field,
    code-field, description-field, amount-field, currency-field,
    currency. All are optional and will use defaults if not specified.

-   The remainder of the file is account-assigning rules. Each is a
    paragraph consisting of one or more description-matching patterns
    (case-insensitive regular expressions), one per line, followed by
    the account name to use when the transaction's description matches
    any of these patterns.

-   A match pattern may be followed by a replacement pattern,
    separated by `=`, which rewrites the matched part of the
    description. Use this if you want to clean up messy bank data. To
    rewrite the entire description, use a match pattern like
    `.*PAT.*=REPL`. Within a replacement pattern, you can refer to the
    matched text with `\0` and any regex groups with `\1`, `\2` in the
    usual way.

#### histogram

The histogram command displays a quick bar chart showing transaction
counts by day, week, month or other reporting interval.

Examples:

    $ hledger histogram -p weekly dining

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

#### stats

The stats command displays quick summary information for the whole journal,
or by period.

Examples:

    $ hledger stats
    $ hledger stats -p 'monthly in 2009'

#### test

This command runs hledger's internal self-tests and displays a quick
report. The -v option shows more detail, and a pattern can be provided to
filter tests by name. It's mainly used in development, but it's also nice
to be able to test for smoke at any time.

Examples:

    $ hledger test
    $ hledger test -v balance

### Add-on commands

The following extra commands will be available if they have been
[installed](#installing).  Add-ons may differ from hledger core in their
level of support and maturity and may not be available on all platforms;
if available, they are provided on the download page.  Note currently you
must invoke add-on commands like, eg: `$ hledger-web ...`, not `$ hledger
web ...`. The hledger-NAME executables support the usual hledger options,
plus any specific options of their own.

#### chart

The chart command saves an image file, by default "hledger.png", showing a
basic pie chart of your top account balances. Note that positive and
negative balances will not be displayed together in the same chart; any
balances not matching the sign of the first one will be ignored.

chart-specific options:

##### --output

You can specify a different output file name with -o/--output. The data
currently will always be in PNG format.

##### --size

You can adjust the image resolution with --size=WIDTHxHEIGHT (in pixels).

##### --items

Set the number of accounts to show with --items=N (default is 10).

To show only accounts above a certain depth, use the --depth option;
otherwise the chart can include accounts of any depth. When a parent and
child account both appear in a chart, the parent's balance will be
exclusive of the child's.

Examples:

    $ hledger-chart assets --depth 2
    $ hledger-chart liabilities --depth 2
    $ hledger-chart ^expenses -o balance.png --size 1000x600 --items 20
    $ for m in 01 02 03 04 05 06 07 08 09 10 11 12; do hledger-chart -p 2009/$m ^expenses --depth 2 -o expenses-2009$m.png --size 400x300; done

#### vty

The vty command starts a simple curses-style (full-screen, text) user
interface, which allows interactive navigation of the
print/register/balance reports. This lets you browse around and explore
your numbers quickly with less typing.

vty-specific options:

##### --debug-vty

    --debug-vty  run with no terminal output, showing console

Examples:

    $ hledger-vty
    $ hledger-vty -BE food

#### web

The web command starts a web server providing a web-based user interface,
and if possible opens a web browser to view it. The web UI combines the
features of the print, register, balance and add commands, and adds a
general edit command.

##### data safety

Warning: the web UI's edit form can alter your existing journal data (it
is the only hledger feature that can do so.)  Any visitor to the web UI
can edit or overwrite the journal file (and any included files); hledger
provides no access control. A numbered backup of the file is saved on each
edit, normally - ie if file permissions allow, disk is not full, etc.

##### web support files

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
version control system such as darcs will work well here.

##### detecting changes

As noted, changes to the support files will take effect immediately,
without a restart.  This applies to the journal data too; you can directly
edit the journal file(s) (or, eg, commit a change within a version control
system) while the web UI is running, and the changes will be visible on
the next page reload.

##### malformed edits

The journal file must remain in good [hledger format](#journal-file) so
that hledger can parse it. The web add and edit forms ensure this by not
allowing edits which would introduce parse errors. If a direct edit makes
the journal file unparseable, the web UI will show the error instead of
data, until the file has been fixed.

There are some web-specific options:

##### --port

    --port=N           serve on tcp port N (default 5000)

The server listens on port 5000 by default; use --port to change that.

##### --base-url

    --base-url=URL     use this base url (default http://localhost:PORT)

If you want to visit the web UI from other machines, you'll need to use
this option to fix the hyperlinks. Just give your machine's host name or
ip address instead of localhost. This option is also lets you conform to a
custom url scheme when running hledger-web behind a reverse proxy as part
of a larger site. Note that the PORT in the base url need not be the same
as the `--port` argument.

Examples:

    $ hledger-web
    $ hledger-web -E -B --depth 2 -f some.journal
    $ hledger-web --port 5010 --base-url http://some.vhost.com --debug

### Other features

Here are some additional hledger features and concepts that affect most
commands.

#### Filter patterns

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

#### Dates

##### Simple dates

Within a journal file, transaction dates always follow a year/month/day
format, although several different separator characters are accepted. Some
examples:

> `2010/01/31`, `2010/1/31`, `2010-1-31`, `2010.1.31`

Simple dates are always unambiguous, but writing the year is optional if
you define a..

##### Default year

You can set a default year for simple dates with a `Y` directive in the
journal, as below. Then subsequent dates may be written as month/day. Eg:

    Y2009
    
    12/15  ; <- equivalent to 2009/12/15
      ...
    
    Y2010
    
    1/31  ; <- equivalent to 2010/1/31
      ...

##### Actual & effective dates

This is a more advanced feature of dates in the journal file. Real-life
transactions sometimes involve more than one date. For example, you buy a
movie ticket on friday with a debit card, and the transaction is charged
to your bank account on monday.  Or you write a cheque to someone and they
deposit it weeks later.

For most transactions, you won't care which date is recorded in your
journal - either will do, especially if the dates are not far apart. But
when you need to model reality (here, your daily bank balance) more
accurately, you can record two dates, separated by an equals sign.  By
default, the first date is used in reports; to use the second one instead,
run hledger with the `--effective` flag.

About the terminology: we follow c++ ledger's usage, calling these the
*actual date* (on the left) and the *effective date* (on the right).
hledger doesn't actually care what these terms mean, but here are some
mnemonics to help keep our usage consistent and avoid confusion:

- "The movie ticket purchase took EFFECT on friday, but ACTUALLY appeared in my bank balance on monday."

- "The payment by cheque took EFFECT then, but ACTUALLY cleared weeks later."

- ACTUAL=EFFECTIVE. The actual date is by definition the one on the left,
  the effective date is on the right. A before E.

- LATER=EARLIER. The effective date is usually the chronologically earlier one.

- BANKDATE=MYDATE. You can usually think of effective date as "my date" and actual date as "bank's date".
  If you record a transaction manually, you'll use the effective (your) date.
  If you convert a transaction from bank data, it will have the actual (bank's) date.

Example:

    ; journal transaction with ACTUAL=EFFECTIVE
    2010/2/23=2010/2/19 movie ticket
      expenses:cinema                   $10
      assets:checking

    $ hledger register checking
    2010/02/23 movie ticket         assets:checking                $-10         $-10

    $ hledger register checking --effective
    2010/02/19 movie ticket         assets:checking                $-10         $-10


##### Smart dates

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

#### Period expressions

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

The `-b/--begin` and `-e/--end` options may be used as a shorthand for
`-p 'from ...'` and `-p 'to ...'` respectively. But note [-p overrides other flags](#p-overrides-other-flags).

##### Reporting interval

Period expressions can also begin with (or be) a reporting interval, which
affects commands like [register](#register) and [histogram](#histogram).
The reporting interval is one of: `daily`, `weekly`, `monthly`,
`quarterly`, or `yearly`, optionally followed by an `in`
keyword. Examples:

    -p "weekly from 2009/1/1 to 2009/4/1"
    -p "monthly in 2008"
    -p "monthly from 2008"
    -p "quarterly"

A reporting interval may also be specified with the `-D/--daily`,
`-W/--weekly`, `-M/--monthly`, `-Q/--quarterly`, and `-Y/--yearly`
options. But remember:

##### -p overrides other flags

A `-p/--period` option on the command line will cause any
`-b`/`-e`/`-D`/`-W`/`-M`/`-Q`/`-Y` flags to be ignored.

#### Display expressions

Unlike a [period expression](#period-expressions), which selects the
transactions to be used for calculation, a display expression specified
with the `-d/--display` option selects which transactions will be
displayed. This useful, say, if you want to see your checking register
just for this month, but with an accurate running balance based on all
transactions. Eg:

    $ hledger register checking --display "d>=[1]"

meaning "make a register report of all checking transactions, but display
only the ones with date on or after the 1st of this month."

This is really all that we support of c++ ledger's display expressions:
namely transactions before or after a given (smart) date.

#### Depth limiting

With the `--depth N` option, reports will show only the uppermost accounts
in the account tree, down to level N. See the [balance](#balance),
[register](#register) and [chart](#chart) examples.

#### Prices

You can specify a commodity's unit price, or exchange rate, in terms of
another commodity. There are two ways.

First, you can set the price explicitly for a single posting by writing `@
PRICE` after the amount. PRICE is another amount in a different
commodity. Eg, here one hundred euros was purchased at $1.35 per euro:

    2009/1/2 x
     expenses:foreign currency       €100 @ $1.35
     assets

Secondly, you can set the price for a commodity as of a certain date, by
entering a historical price record. These are lines beginning with "P",
appearing anywhere in the journal between transactions. Eg, here we say the
exchange rate for 1 euro is $1.35 on 2009/1/1 (and thereafter, until a
newer price record is found):

    P 2009/1/1 € $1.35  ; <- historical price: P, date, commodity symbol, price in 2nd commodity (space-separated)
    
    2009/1/2 x
     expenses:foreign currency       €100
     assets

The print command shows any unit prices in effect. Either example above
will show:

    $ hledger print
    2009/01/02 x
        expenses:foreign currency  €100 @ $1.35
        assets                     €-100 @ $1.35

To see amounts converted to their total cost, use the `--cost/-B` flag
with any command:

    $ hledger print --cost
    2009/01/02 x
        expenses:foreign currency       $135.00
        assets                         $-135.00

The `--cost/-B` flag does only one lookup step, ie it will not look up the
price of a price's commodity.

Note hledger handles prices differently from c++ ledger in one important
respect: we assume unit prices do not vary over time.  This is good for
simple reporting of foreign currency transactions, but not for tracking
fluctuating-value investments or capital gains.

#### Including other files

You can pull in the content of additional journal files, by writing lines like this:

    !include path/to/file.journal

The `!include` directive may only be used in journal files, and currently
it may only include other journal files (eg, not timelog files.)

#### Default commodity

You can set a default commodity with a `D` directive in the journal. This
will be used for any subsequent amounts with no commodity symbol,
including the commodity display settings (left or right symbol, spacing,
thousands separator, and precision.)

    ; default commodity: british pound, comma thousands separator, two decimal places
    D £1,000.00
    
    2010/1/1
      a  2340.11   ; <- no commodity symbol, so will use the above
      b

#### Default parent account

You can specify a default parent account within a section of the journal with
the `!account` directive:

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

#### Timelog reporting

hledger will also read timelog files in timeclock.el format. As a
convenience, if you invoke hledger via an "hours" symlink or copy, it uses
your timelog file (`~/.timelog` or `$TIMELOG`) by default, rather than your
journal.

Timelog entries look like this:

    i 2009/03/31 22:21:45 some:project
    o 2009/04/01 02:00:34

The clockin description is treated as an account name. Here are some
queries to try (after downloading
[sample.timelog](http://joyful.com/repos/hledger/sample.timelog)):

    ln -s `which hledger` ~/bin/hours            # set up "hours" in your path
    export TIMELOG=sample.timelog
    hours                                        # show all time balances
    hours -p 'last week'                         # last week
    hours -p thismonth                           # the space is optional
    hours -p 'from 1/15' register project        # project sessions since jan 15
    hours -p 'weekly' reg --depth 1 -E           # weekly time summary

This is a useful feature, if you can find a way to efficiently record
timelog entries. The "ti" and "to" scripts may be available from the c++
ledger 2.x repository. I use
[timeclock-x.el](http://www.emacswiki.org/emacs/timeclock-x.el) and
[ledgerutils.el](http://joyful.com/repos/ledgertools/ledgerutils.el) in
emacs.

### Compatibility with c++ ledger

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
some features being accepted but ignored. (Eg modifier entries, periodic
entries, metadata, per-posting cleared flags). There are also some subtle
differences in parser behaviour (eg comments may be permissible in
different places.)

Generally, it's easy to keep a journal file that works with both hledger
and c++ledger if you avoid the more esoteric syntax.  Occasionally you'll
need to make small edits to restore compatibility for one or the other.

hledger does not allow separate dates for individual postings, unlike c++
ledger.

Likewise, hledger does not support per-posting cleared status. It does
ignore a cleared flag (`*`) at the start of a posting, so that the account
name is parsed correctly.

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

-   hledger period expressions don't support "biweekly",
    "bimonthly", or "every N days/weeks/..."

-   hledger always shows timelog balances in hours

-   hledger splits multi-day timelog sessions at midnight

-   hledger doesn't track the value of commodities with varying
    price; prices are fixed as of the transaction date

-   hledger print shows amounts for all postings, and shows unit
    prices for amounts which have them. (This currently means that
    it does not print multi-commodity transactions in valid journal format.)

## Troubleshooting

### Installation issues

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

### Usage issues

Here are some issues you might encounter when you run hledger:

- <a name="locale" />**hledger: ... hGetContents: invalid argument (Illegal byte sequence)**
    You may get this error when running hledger with a journal containing
    non-ascii text, on a machine using the default C locale. You can check
    this like so:
  
        $ locale
        LANG=
        LC_COLLATE="C"
        LC_CTYPE="C"
        LC_MESSAGES="C"
        LC_MONETARY="C"
        LC_NUMERIC="C"
        LC_TIME="C"
        LC_ALL=
        $ file my.journal
        .../.journal: UTF-8 Unicode C++ program text
  
    In this case you need to set the `LANG` environment variable to a
    locale suitable for the encoding shown (eg UTF-8). You
    can set it temporarily when you run hledger:
  
        $ LANG=en_US.UTF-8 hledger ...
      
    or permanently:
  
        $ echo "export LANG=en_US.UTF-8" >>~/.bash_profile
        $ bash --login

## Examples and recipes

-   Here's a bash function that will run hledger chart and display
    the image in your (graphical) emacs:

        function chart () {
          hledger chart $* && emacsclient -n hledger.png
        }

    Example:

        $ chart food --depth 2 -p jan

## Other resources

- The rest of the [hledger.org](http://hledger.org) site.

- The [c++ ledger site](https://github.com/jwiegley/ledger/wiki).
  Also the [c++ ledger 2.x manual](http://joyful.com/repos/ledger/doc/ledger.html)
  is slightly outdated but informative.

- [Why you need accounting](http://podcastle.org/2009/10/09/pc-miniature-38-accounting-for-dragons)
