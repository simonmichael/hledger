---
title: hledger manual
---

# hledger manual

This is the official hledger manual. You may also want to visit the
[http://hledger.org](http://hledger.org) home page, the
[hledger for techies](README2.html) page, and for background,
[c++ ledger's manual](http://joyful.com/repos/ledger/doc/ledger.html).

## User Guide

### Introduction

hledger is a program for tracking money, time, or any other commodity,
using a plain text file format and the simple but powerful principles of
double-entry accounting.

It is modelled closely on
[John Wiegley's ledger](http://wiki.github.com/jwiegley/ledger) (aka "c++
ledger"), with some features removed and some new ones added. I wrote
hledger because I wanted to build financial tools in the Haskell
programming language rather than in C++.

hledger's basic function is to generate register and balance reports from
a plain text ledger file, at the command line or via the web or curses
interface. You can use it to, eg,

-   track spending and income
-   see time reports by day/week/month/project
-   get accurate numbers for client billing and tax filing
-   track invoices

hledger aims to help both computer experts and every-day users gain
clarity in their finances and time management. For now though, it is most
useful to technically-minded folks who are comfortable with command-line
tools.

hledger is copyright (c) 2007-2009 Simon Michael
<[simon@joyful.com](mailto:simon@joyful.com)\> and contributors and
released as Free Software under GPL version 3 or later.

### Installing

hledger works on all major platforms; here are the [release
notes](http://hledger.org/NEWS.html). One of these pre-built
[binaries](http://hledger.org/binaries/) might work for you, but at
present these are not very up-to-date, so the usual thing is to build
with the cabal-install tool:

1. If you don't already have the Glasgow Haskell Compiler and
   cabal-install, download and install the
   [Haskell Platform](http://hackage.haskell.org/platform/).  Or, you may
   be able to use platform packages; eg on Ubuntu Lucid, do `apt-get
   install ghc6 cabal-install happy`.

2. Make sure ~/.cabal/bin is in your path. This is useful so that you can
   run hledger by just typing "hledger", and necessary if (eg) you install
   with -fweb, to avoid an installation failure..

3. Install hledger with cabal-install:

        cabal update
        cabal install hledger

    You can add the following options to the install command to build
    extra features (if you're new to cabal, I recommend you get the basic
    install working first, then add these one at a time):

    - `-fvty` - builds the [ui](#ui) command. (Not available on microsoft
        windows.)

    - `-fweb` - builds the [web](#web) command.

    - `-fchart` builds the [chart](#chart) command. This requires
        [gtk2hs](http://www.haskell.org/gtk2hs/download/), which you'll
        need to install yourself as it's not yet provided by the haskell
        platform or cabal.

#### Installation troubleshooting

cabal builds a lot of fast-evolving software, and it's not always smooth
sailing.  Here are some known issues and things to try:

- **Ask for help on [#hledger](irc://freenode.net/#hledger) or [#haskell](irc://freenode.net/#haskell)**

- **cabal update.** If you didn't already, ``cabal update`` and try again.

- **Do you have a new enough version of GHC ?** As of 2010, 6.10 and 6.12
    are supported, 6.8 might or might not work.

- **Do you have a new enough version of cabal-install ?**
  Newer versions tend to be better at resolving problems. 0.6.2 has been
  known to fail where newer versions succeed.

- **Could not run trhsx.**
  You are installing with -fweb, which needs to run the ``trhsx`` executable.
  It is installed by the hsx package in ~/.cabal/bin, which needs to be in
  your path.

- **Installation fails due to problems with a hledger package.**
    The current hledger release might have a coding error, or dependency
    error. You could try installing the
    [previous version](http://hackage.haskell.org/package/hledger):

        cabal install hledger-0.x

    or (preferably) the latest development version: install
    [darcs](http://darcs.net) and then:

        darcs get --lazy http://joyful.com/repos/hledger
        cd hledger/hledger-lib
        cabal install
        cd ..
        cabal install [-f...]

- **Installation fails due to problems with other packages.**
  Resolve the problem packages one at a time. Eg, cabal install pkg1.
  Look for the cause of the failure near the end of the output. If it's
  not apparent, try again with `-v2` or `-v3` for more verbose output.

- **Could not run happy.**
  A package (eg haskell-src-exts) needs to run the ``happy`` executable.
  If not using the haskell platform, install the appropriate platform
  package which provides it (eg apt-get install happy).

- **GHC panic while installing** might be due to
    [http://hackage.haskell.org/trac/ghc/ticket/3862](http://hackage.haskell.org/trac/ghc/ticket/3862)

- **cabal could not reconcile dependencies**
  In some cases, especially if you have installed/updated many cabal
  package versions or GHC itself, cabal may not be able to reconcile the
  package dependencies. You can sometimes work around this by using
  cabal's `--constraint` option. Another way is to purge all unnecessary
  package versions by removing (or renaming) ~/.ghc, then trying cabal
  install again. Also remember that `-fwebyesod` requires GHC 6.12 or greater.

- <a name="locale" />**hledger: ... hGetContents: invalid argument (Illegal byte sequence)**
  You may get this error when running hledger built with GHC 6.12 on a
  mac, when the locale is unset (check it at the terminal prompt):
  
        $ locale
        LANG=
        LC_COLLATE="C"
        LC_CTYPE="C"
        LC_MESSAGES="C"
        LC_MONETARY="C"
        LC_NUMERIC="C"
        LC_TIME="C"
        LC_ALL=

    and there is non-ascii text in your journal file:

        $ file my.journal
        .../.journal: UTF-8 Unicode C++ program text
  
    In this case you need to set the `LANG` environment variable to a
    locale suitable for the encoding shown (almost certainly UTF-8). You
    can set it every time you run hledger:
  
        $ LANG=en_US.UTF-8 hledger ...
      
    or configure it permanently:
  
        $ echo "export LANG=en_US.UTF-8" >>~/.bash_profile
        $ bash --login

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

    you are probably on a mac with macports libraries installed:
    [http://hackage.haskell.org/trac/ghc/ticket/4068](http://hackage.haskell.org/trac/ghc/ticket/4068).
    To work around, add this --extra-lib-dirs flag:

        $ cabal install hledger --extra-lib-dirs=/usr/lib

- **setup: failed to parse output of 'ghc-pkg dump'**
  This probably means
  [you need a newer version of cabal-install](http://stackoverflow.com/questions/1908333/getting-cabal-to-work-with-ghc-6-12-1).
  Do eg:
  
        $ cabal update
        $ cabal install cabal-install
        
    then try installing hledger again.

### Basic usage

Basic usage is:

    hledger [OPTIONS] [COMMAND [PATTERNS]]

[OPTIONS](#overview) may appear anywhere on the command line.
[COMMAND](#commands) is one of: add, balance, chart, convert, histogram,
print, register, stats, ui, web, test (defaulting to balance). The
optional [PATTERNS](#filter-patterns) are regular expressions which select
a subset of the ledger data.

hledger looks for data in a ledger file, usually `.ledger` in your home
directory. You can specify a different file with the -f option (use - for
standard input) or `LEDGER` environment variable.

To get started, make yourself a ledger file containing some
transactions. You can copy the sample file below (or
[sample.ledger](http://joyful.com/repos/hledger/sample.ledger)) and save
it as `.ledger` in your home directory. Or, just run `hledger add` and
enter a few transactions. Now you can try some of these commands, or read
on:

    hledger --help                        # show command-line help
    hledger balance                       # all accounts with aggregated balances
    hledger bal --depth 1                 # only top-level accounts
    hledger register                      # transaction register
    hledger reg income                    # transactions to/from an income account
    hledger reg checking                  # checking transactions
    hledger reg desc:shop                 # transactions with shop in the description
    hledger histogram                     # transactions per day, or other interval
    hledger add                           # add some new transactions to the ledger file
    hledger ui                            # curses ui, if installed with -fvty
    hledger web                           # web ui, if installed with -fweb
    hledger chart                         # make a balance chart, if installed with -fchart

You'll find more examples below.

### File format

hledger's data file, aka the ledger, is a plain text representation of a
standard accounting journal. It contains a number of transactions, each
describing a transfer of money (or another commodity) between two or more
named accounts. Here's an example:

    ; A sample ledger file. This is a comment.
    
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

Each transaction has a date, description, and two or more postings (of
some amount to some account) which must balance to 0. As a convenience,
one posting's amount may be left blank and will be inferred.

Note that account names may contain single spaces, while the amount must
be separated from the account name by at least two spaces.

An amount is a number, with an optional currency/commodity symbol or word
on either the left or right. Note: when writing a negative amount with a
left-side currency symbol, the minus goes after the symbol, eg `$-1`.

This file format is also compatible with c++ ledger, so you can use both
tools. For more details, see
[File format compatibility](#file-format-compatibility).

## Reference

### Overview

This version of hledger mimics a subset of ledger 3.x, and adds some
features of its own. We currently support regular ledger entries, timelog
entries, multiple commodities, (fixed) price history, virtual postings,
filtering by account and description, the familiar print, register &
balance commands and several new commands. We handle (almost) the full
period expression syntax, and very limited display expressions consisting
of a simple date predicate.

Here is the command-line help:

    Usage: hledger [OPTIONS] [COMMAND [PATTERNS]]
           hledger [OPTIONS] convert CSVFILE
           hledger [OPTIONS] stats
    
    hledger uses your ~/.ledger or $LEDGER file, or another specified with -f
    
    COMMAND is one of (may be abbreviated):
     add       - prompt for new transactions and add them to the ledger
     balance   - show accounts, with balances
     convert   - read CSV bank data and display in ledger format
     histogram - show a barchart of transactions per day or other interval
     print     - show transactions in ledger format
     register  - show transactions as a register with running balance
     stats     - show various statistics for a ledger
     ui        - run a simple text-based UI
     web       - run a simple web-based UI
     chart     - generate balances pie chart
     test      - run self-tests
    
    PATTERNS are regular expressions which filter by account name.
    Prefix with desc: to filter by transaction description instead.
    Prefix with not: to negate a pattern. When using both, not: comes last.
    
    DATES can be y/m/d or ledger-style smart dates like "last month".
    
    Options:
     -f FILE  --file=FILE          use a different ledger/timelog file; - means stdin
              --no-new-accounts    don't allow to create new accounts
     -b DATE  --begin=DATE         report on transactions on or after this date
     -e DATE  --end=DATE           report on transactions before this date
     -p EXPR  --period=EXPR        report on transactions during the specified period
                                   and/or with the specified reporting interval
     -C       --cleared            report only on cleared transactions
     -U       --uncleared          report only on uncleared transactions
     -B       --cost, --basis      report cost of commodities
              --depth=N            hide accounts/transactions deeper than this
     -d EXPR  --display=EXPR       show only transactions matching EXPR (where
                                   EXPR is 'dOP[DATE]' and OP is <, <=, =, >=, >)
              --effective          use transactions' effective dates, if any
     -E       --empty              show empty/zero things which are normally elided
     -R       --real               report only on real (non-virtual) transactions
              --no-total           balance report: hide the final total
     -W       --weekly             register report: show weekly summary
     -M       --monthly            register report: show monthly summary
     -Q       --quarterly          register report: show quarterly summary
     -Y       --yearly             register report: show yearly summary
              --base-url           web: use this base url (default http://localhost:PORT)
              --port               web: serve on tcp port N (default 5000)
     -h       --help               show this help
     -V       --version            show version information
     -v       --verbose            show verbose test output
              --binary-filename    show the download filename for this hledger build
              --debug              show extra debug output; implies verbose
              --debug-no-ui        run ui commands with no output
     -o FILE  --output=FILE        chart: output filename (default: hledger.png)
              --items=N            chart: number of accounts to show (default: 10)
              --size=WIDTHxHEIGHT  chart: image size (default: 600x400)

### Commands

#### Reporting commands

These commands are read-only, that is they never modify your data.

##### print

The print command displays full transactions from the ledger file, tidily
formatted and showing all amounts explicitly. The output of print is
always valid ledger data.

hledger's print command also shows all unit prices in effect, or (with
-B/--cost) shows cost amounts.

Examples:

    $ hledger print
    $ hledger print employees:bob | hledger -f- register expenses

##### register

The register command displays postings, one per line, and their running
total. With a [reporting interval](#reporting-interval) it will aggregate
similar postings within each interval.

Examples:

    $ hledger register
    $ hledger register --monthly -E rent

##### balance

The balance command displays accounts and their balances.

Examples:

    $ hledger balance
    $ hledger balance food -p 'last month'
    $ for y in 2006 2007 2008 2009 2010; do echo; echo $y; hledger -f $y.ledger balance ^expenses --depth 2; done

##### chart

(optional feature)

The chart command saves a pie chart of your top account balances to an
image file (usually "hledger.png", or use -o/--output FILE). You can
adjust the image resolution with --size=WIDTHxHEIGHT, and the number of
accounts with --items=N.

Note that positive and negative balances will not be displayed together in
the same chart; any balances not matching the sign of the first one will
be omitted.

To show only accounts above a certain depth, use the --depth
option. Otherwise, the chart can include accounts at any depth. If a
parent and child account are both displayed, the parent's balance excludes
the child's.

Examples:

    $ hledger chart assets --depth 2
    $ hledger chart liabilities --depth 2
    $ hledger chart ^expenses -o balance.png --size 1000x600 --items 20
    $ for m in 01 02 03 04 05 06 07 08 09 10 11 12; do hledger -p 2009/$m chart ^expenses --depth 2 -o expenses-2009$m.png --size 400x300; done

##### histogram

The histogram command displays a quick bar chart showing transaction
counts, per day, week, month or other reporting interval. It is
experimental.

Examples:

    $ hledger histogram -p weekly dining

##### stats

The stats command displays quick summary information for the ledger.

Examples:

    $ hledger stats

##### ui

(optional feature)

The ui command starts hledger's curses (full-screen, text) user interface,
which allows interactive navigation of the print/register/balance
reports. This lets you browse around your numbers and get quick insights
with less typing.

Examples:

$ hledger ui $ hledger ui -BE food

#### Modifying commands

The following commands can alter your ledger file.

##### add

The add command prompts interactively for new transactions, and adds them
to the ledger. It is experimental.

Examples:

$ hledger add $ hledger add accounts:personal:bob

##### web

(optional feature)

The web command starts hledger's web interface, and tries to open a web
browser to view it (if this fails, you'll have to visit the indicated url
yourself.) The web ui combines the features of the print, register,
balance and add commands.

Note there are two alternate implementations of the web command - the old
one, built with `-fweb`:

    $ hledger web

and the new one, built with `-fwebyesod`, which you run in the same way:

    $ hledger web
    
We will assume the latter in the rest of these docs. Some more examples:
    
    $ hledger web -E -B  p 'this year'
    $ hledger web --base-url http://this.vhost.com --port 5010 --debug -f my.journal

The new web ui adds an edit command. Warning: this is the first hledger
feature which can alter your existing journal data.  You can edit, or
ERASE, the (top-level) journal file through the web ui. There is no access
control. A numbered backup of the file will be saved at each edit, in
normal circumstances (eg if file permissions allow, disk is not full, etc.)

#### Other commands

##### convert

The convert command reads a
[CSV](http://en.wikipedia.org/wiki/Comma-separated_values) file you have
downloaded from your bank, and prints out the transactions in ledger
format, suitable for adding to your ledger. It does not alter your ledger
directly.

This can be a lot quicker than entering every transaction by hand.  (The
downside is that you are less likely to notice if your bank makes an
error!) Use it like this:

    $ hledger convert FILE.csv >FILE.ledger

where FILE.csv is your downloaded csv file. This will convert the csv data
using conversion rules defined in FILE.rules (auto-creating this file if
needed), and save the output into a temporary ledger file. Then you should
review FILE.ledger for problems; update the rules and convert again if
needed; and finally copy/paste transactions which are new into your main
ledger.

###### .rules file

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
    
    # account-assigning rules
    
    SPECTRUM
    expenses:health:gym
    
    ITUNES
    BLKBSTR=BLOCKBUSTER
    expenses:entertainment
    
    (TO|FROM) SAVINGS
    assets:bank:savings

This says:

-   the ledger account corresponding to this csv file is
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

-   Lines beginning with \# or ; are ignored (but avoid using
    inside an account rule)

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


##### test

This command runs hledger's internal self-tests and displays a quick
report. The -v option shows more detail, and a pattern can be provided to
filter tests by name. It's mainly used in development, but it's also nice
to be able to run a sanity check at any time..

Examples:

    $ hledger test
    $ hledger test -v balance

### Other features

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

Within a ledger file, dates must follow a fairly simple year/month/day
format. Examples:

> `2010/01/31` or `2010/1/31` or `2010-1-31` or `2010.1.31`

The [add](#add) command and the [web](#web) add form, as well as some
other places, accept [smart dates](#smart-dates) - more about those below.

##### Default year

You can set a default year with a `Y` directive in the ledger, then
subsequent dates may be written as month/day. Eg:

    Y2009
    
    12/15 ...
    
    Y2010
    
    1/31 ...

##### Actual and effective dates

Frequently, a real-life transaction has two (or more) dates of
interest. For example, you might make a purchase on friday with a debit
card, and it might clear (take effect in your bank account) on
tuesday. It's sometimes useful to model this accurately, so that your
hledger balances match reality. So, you can specify two dates for a
transaction, the *actual* and *effective* date, separated by `=`. Eg:

    2010/2/19=2010/2/23 ...

Then you can use the `--effective` flag to prefer the effective (second)
date, if any, in reports. This is useful for, eg, seeing a more accurate
daily balance while reconciling a bank account.

So, what do *actual* and *effective* mean, exactly ? I always assumed that
the actual date comes first, and is "the date you enacted the
transaction", while the effective date comes second, and is optional, and
is "the date the transaction took effect" (showed up in your bank
balance).

Unfortunately, this is the reverse of c++ ledger's interpretation (cf
[differences](#other-differences)). However it's not *too* serious; just
remember that hledger requires the first date; allows an optional second
date which the `--effective` flag will select; and the meaning of "actual"
and "effective" is up to you.

The second date may omit the year if it is the same as the first's.

##### Smart dates

In [period expressions](#period-expressions), the `-b` and `-e` options,
the [add](#add) command and the [web](#web) add form, more flexible "smart
dates" are allowed. Here are some examples:

-   `2009/1/1`, `2009/01/01`, `2009-1-1`, `2009.1.1`, `2009/1`,
    `2009` (january 1, 2009)
-   `1/1`, `january`, `jan`, `this year` (january 1, this year)
-   `next year` (january 1, next year)
-   `this month` (the 1st of the current month)
-   `this week` (the most recent monday)
-   `last week` (the monday of the week before this one)
-   `today`, `yesterday`, `tomorrow`

Spaces are optional, so eg: `-p lastmonth` is valid.

#### Period expressions

hledger supports flexible "period expressions" with the `-p/--period`
option to select transactions within a period of time (like 2009) and/or
with a reporting interval (like weekly). hledger period expressions are
similar but not identical to c++ ledger's.

Here is a basic period expression specifying the first quarter of 2009
(start date is always included, end date is always excluded):

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
earliest or latest transaction in your ledger data:

    -p "from 2009/1/1"  (everything after january 1, 2009)
    -p "from 2009/1"    (the same)
    -p "from 2009"      (the same)
    -p "to 2009"        (everything before january 1, 2009)

A single date with no "from" or "to" defines both the start and end date
like so:

    -p "2009"           (the year 2009;    equivalent to "2009/1/1 to 2010/1/1")
    -p "2009/1"         (the month of jan; equivalent to "2009/1/1 to 2009/2/1")
    -p "2009/1/1"       (just that day;    equivalent to "2009/1/1 to 2009/1/2")

##### Reporting interval

You can also specify a reporting interval, which causes the "register"
command to summarise the transactions in each interval.  It goes before
the dates, and can be: "daily", "weekly", "monthly", "quarterly", or
"yearly". An "in" keyword is optional, and so are the dates:

    -p "weekly from 2009/1/1 to 2009/4/1"
    -p "monthly in 2008"
    -p "monthly from 2008"
    -p "quarterly"

A reporting interval may also be specified with the -W/--weekly,
-M/--monthly, -Q/--quarterly, and -Y/--yearly options. However..

##### -p overrides other flags

Note: any period option on the command line will override the -b, -e, -W,
-Q and -Y flags.

#### Display expressions

A display expression with the `-d/--display` option selects which
transactions will be displayed (unlike a
[period expression](#period-expressions), which selects the transactions
to be used for calculation).

hledger currently supports a very small subset of c++ ledger's display
expressions, namely: transactions before or after a date.  This is useful
for displaying your recent check register with an accurate running
total. Note the use of \>= here to include the first of the month:

    $ hledger register -d "d>=[this month]"

#### Depth limiting

With the `--depth N` option, reports will show only the uppermost accounts
in the account tree, down to level N. This is most useful with
[balance](#balance) (and [chart](#chart)). For example:

    $ hledger balance --depth 2

shows a more concise balance report displaying only the top two levels of
accounts. This example with [register](#register):

    $ hledger register --depth 1

would show only the postings to top-level accounts, which usually means
none.

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
appearing anywhere in the ledger between transactions. Eg, here we say the
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

#### Timelog reporting

hledger will also read timelog files in timeclock.el format. As a
convenience, if you invoke hledger via an "hours" symlink or copy, it uses
your timelog file (\~/.timelog or $TIMELOG) by default, rather than your
ledger.

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

hledger's file format is mostly identical with that of c++ ledger version
2, with some features (like modifier and periodic entries) being accepted,
but ignored. There are also some subtle differences in parser behaviour
(eg comments may be permissible in different places.) C++ ledger version 3
has introduced additional syntax, which current hledger probably fails to
parse.

Generally, it's easy to keep a ledger file that works with both hledger
and c++ledger if you avoid the more esoteric syntax.  Occasionally you'll
need to make small edits to restore compatibility for one or the other.

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
    values, you can write -f-
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
    prices for amounts which have them
-   hledger assumes a transaction's actual date comes first, and is
    required, while the optional effective date comes second (cf
    [Actual and effective dates](#actual-and-effective-dates))
-   hledger does not support per-posting actual or effective dates

### More examples and recipes

-   Here's a bash function that will run hledger chart and display
    the image in your (graphical) emacs:

        function chart () {
          hledger chart $* && emacsclient -n hledger.png
        }

    Example:

        $ chart food --depth 2 -p jan

