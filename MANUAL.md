---
title: hledger user manual
---

# User manual

Version: 0.20pre

## Introduction

[hledger](http://hledger.org) is a program for tracking money, time, or
any other commodity, using a simple, editable file format and the powerful
principles of double-entry accounting. It was inspired by
[ledger](http://ledger-cli.org).  hledger is Free Software released under
GPL version 3 or later.

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

## Installing

hledger works on linux, mac and windows. You can fund ready-to-run
binaries of the latest release - see the [download page](DOWNLOAD.html).

Otherwise, build the latest release from Hackage using cabal-install.
Ensure you have [GHC](http://hackage.haskell.org/ghc/) 7.0 or greater or
the [Haskell Platform](http://hackage.haskell.org/platform/) installed,
then:

    $ cabal update
    $ cabal install hledger

To also install the web interface, do:

    $ cabal install hledger-web

Then try it:

    $ hledger

If you get "hledger not found" or similar, you should add cabal's bin
directory to your PATH environment variable. Eg on unix-like systems,
something like:

    $ echo 'export PATH=$PATH:~/cabal/bin' >> ~/.bash_profile
    $ source ~/.bash_profile

To build the latest [development version](DEVELOPMENT.html) do:

    $ cabal update
    $ darcs get --lazy http://hub.darcs.net/simon/hledger
    $ cd hledger
    $ make install (or do cabal install inside hledger-lib/, hledger/ etc.)

Some add-on packages are available on Hackage:
[hledger-vty](http://hackage.haskell.org/package/hledger-vty),
[hledger-chart](http://hackage.haskell.org/package/hledger-chart),
[hledger-interest](http://hackage.haskell.org/package/hledger-interest).
These are without an active maintainer, and/or platform-specific, so installing them may be harder.

Note: to use non-ascii characters like £, you might need to [configure a suitable locale](#locale).

<div class="alert">
If you have trouble, see [Troubleshooting](#troubleshooting).
</div>

## Usage

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
    
## The journal file

hledger normally reads data from a plain text file in hledger journal
format.  hledger can read some [other file formats](#other-file-formats)
as well, but first we'll discuss hledger's journal format. Note this is
compatible subset of
[c++ ledger's journal format](http://ledger-cli.org/3.0/doc/ledger3.html#Journal-Format),
so hledger can work with many c++ ledger journal files as well.

The journal file is so called because it represents a standard accounting
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
    
    2008/12/31 * pay off            ; <- an optional * or ! after the date means "cleared" (or anything you want)
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

### Commodity display settings

Based on how you format amounts, hledger will infer canonical display
settings for each commodity, and use them consistently when displaying
amounts in that commodity. Display settings include:

- the position and spacing of the currency/commodity symbol
- the digit group separator character and digit group sizes, if any
- the decimal point character
- the number of decimal places

The canonical settings are those of the first amount seen in the
commodity, with the decimal places adjusted upward to the highest
precision seen in the commodity.

[Default commodity](#default-commodity) directives also influence the
commodity display settings (note: only if they have a commodity symbol).

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

### Primary & secondary dates

Most of the time, a simple transaction date is all you need. However
real-life transactions sometimes involve more than one date. Eg, cheque
writing and clearing dates. When you want to model this, eg so that your
daily checking account balance is more accurate, write both dates,
separated by an equals sign. The *primary date* goes on the left, and is
used by default; the *secondary date* goes on the right, and is used when
the `--date2` flag is provided. (You can also spell this `--aux-date`,
like ledger, or `--effective` like older versions).

These used to be called "actual" and "effective" dates. Their meaning is
up to you, but it's best to follow a consistent rule. I write the bank's
clearing date as primary, and the date I initiated the transaction as
secondary (if needed).

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

### Default commodity

You can set a default commodity or currency with a D directive. This will
be used for any subsequent amounts which have no commodity symbol.

    ; default commodity: british pound, comma thousands separator, two decimal places
    D £1,000.00
    
    2010/1/1
      a  2340   ; no commodity symbol, will use the above
      b

If such an amount is the first seen in that commodity, the canonical
[commodity display settings](#commodity-display-settings) will also be
taken from the directive (note: only if it includes a commodity symbol).

### Prices

#### Transaction prices

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

#### Historical prices

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
        
### Balance Assertions

ledger supports
[balance assertions](http://ledger-cli.org/3.0/doc/ledger3.html#Balance-assertions):
following a posting's amount, an equals sign and another amount which is
the expected balance in this account at this point. hledger does not
currently enforce these but will ignore them, so you can put them in your
journal and test with ledger if needed.

### Fixed Lot Prices

Similarly, we ignore ledger's 
[fixed lot prices](http://ledger-cli.org/3.0/doc/ledger3.html#Fixing-lot-prices).
hledger's [prices](#transaction-prices) always work like ledger's fixed lot prices.

### Comments

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

### Tags

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

### Posting dates

You can give individual postings a different date from their parent
transaction, by adding a [posting tag]("tags") like `date:DATE` where
DATE is a [simple date](#simple-dates). The secondary date can be set
with `date2:DATE2`. If present, these dates will take precedence in
reports.

Ledger's bracketed posting date syntax (`[DATE]`,
`[DATE=DATE2]` or `[=DATE2]` in a posting comment)
is also supported, as an alternate spelling of the date tags.

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

## Other file formats

In addition to the usual [journal files](#the-journal-file), hledger can
read [timelog files](#timelog-reporting).

Since version 0.18, hledger can also read
[CSV](http://en.wikipedia.org/wiki/Comma-separated_values) files natively
(previous versions provided a special `convert` command.)

An arbitrary CSV file does not provide enough information to be parsed as
a journal. So when reading CSV, hledger looks for an additional
[rules file](#the-rules-file), which identifies the CSV fields and assigns
accounts. For reading `FILE.csv`, hledger uses `FILE.csv.rules` in the same
directory, auto-creating it if needed. You should configure the rules file
to get the best data from your CSV file. You can specify a different rules
file with `--rules-file` (useful when reading from standard input).

An example - sample.csv:

    sample.csv:
    "Date","Note","Amount"
    "2012/3/22","TRANSFER TO SAVINGS","-10.00"
    "2012/3/23","SOMETHING ELSE","5.50"

sample.rules:

    skip-lines 1
    date-field 0
    description-field 1
    amount-field 2
    currency $
    base-account assets:bank:checking

    SAVINGS
    assets:bank:savings

the resulting journal:

    $ hledger -f sample.csv print
    using conversion rules file sample.rules
    2012/03/22 TRANSFER TO SAVINGS
        assets:bank:savings         $10.00
        assets:bank:checking       $-10.00

    2012/03/23 SOMETHING ELSE
        income:unknown              $-5.50
        assets:bank:checking         $5.50

### The rules file

A rules file consists of the following optional directives, followed by
account-assigning rules.  (Tip: rules file parse errors are not the
greatest, so check your rules file format if you're getting unexpected
results.)

`account-field`

> If the CSV file contains data corresponding to several accounts (for
> example - bulk export from other accounting software), the specified
> field's value, if non-empty, will override the value of `base-account`.

`account2-field`

> If the CSV file contains fields for both accounts in the transaction,
> you can use this in addition to `account-field`.  If `account2-field` is
> unspecified, the [account-assigning rules](#account-assigning-rules) are
> used.

`amount-field`

> This directive specifies the CSV field containing the transaction
> amount.  The field may contain a simple number or an hledger-style
> [amount](#amounts), perhaps with a [price](#prices). See also
> `amount-in-field`, `amount-out-field`, `currency-field` and
> `base-currency`.

`amount-in-field`

`amount-out-field`

> If the CSV file uses two different columns for in and out movements, use
> these directives instead of `amount-field`.  Note these expect each
> record to have a positive number in one of these fields and nothing in
> the other.

`base-account`

> A default account to use in all transactions. May be overridden by
> `account1-field` and `account2-field`.

`base-currency`

> A default currency symbol which will be prepended to all amounts.
> See also `currency-field`.

`code-field`

> Which field contains the transaction code or check number (`(NNN)`).

`currency-field`

> The currency symbol in this field will be prepended to all amounts. This
> overrides `base-currency`.

`date-field`

> Which field contains the transaction date. A number of common
> four-digit-year date formats are understood by default; other formats
> will require a `date-format` directive.

`date-format`

> This directive specifies one additional format to try when parsing the
> date field, using the syntax of Haskell's
> [formatTime](http://hackage.haskell.org/packages/archive/time/latest/doc/html/Data-Time-Format.html#v:formatTime).
> Eg, if the CSV dates are non-padded D/M/YY, use:
>
>     date-format %-d/%-m/%y
>
> Note custom date formats work best when hledger is built with version
> 1.2.0.5 or greater of the [time](http://hackage.haskell.org/package/time) library.

`description-field`

> Which field contains the transaction's description. This can be a simple
> field number, or a custom format combining multiple fields, eg:
> 
>     description-field %(1) - %(3)

`date2-field`

> Which field contains the transaction's [secondary date](#primary-secondary-dates).

`status-field`

> Which field contains the transaction cleared status (`*`).

`skip-lines`

> How many lines to skip in the beginning of the file, e.g. to skip a
> line of column headings.

Account-assigning rules select an account to transfer to based on the
description field (unless `account2-field` is used.) Each
account-assigning rule is a paragraph consisting of one or more
case-insensitive regular expressions), one per line, followed by the
account name to use when the transaction's description matches any of
these patterns. Eg:

    WHOLE FOODS
    SUPERMARKET
    expenses:food:groceries

If you want to clean up messy bank data, you can add `=` and a replacement
pattern, which rewrites the matched part of the description. (To rewrite
the entire description, use `.*PAT.*=REPL`). You can also refer to matched
groups in the usual way with `\0` etc. Eg:

    BLKBSTR=BLOCKBUSTER
    expenses:entertainment

Lines beginning with `;` or `#` are ignored - just don't use them in the
middle of an account-assigning rule.


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
    $ hledger -f home.journal add equity:bob

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

- hledger recognises description and negative patterns by "desc:"
  and "not:" prefixes, unlike ledger 3's free-form parser

- hledger doesn't require a space before command-line option
  values, eg `-fFILE` or `-f FILE` works

- hledger's weekly reporting intervals always start on mondays

- hledger shows start and end dates of the intervals requested,
  not just the span containing data

- hledger always shows timelog balances in hours

- hledger splits multi-day timelog sessions at midnight

- hledger doesn't track the value of commodities with varying
  price; prices are fixed as of the transaction date

- hledger's output follows the decimal point character, digit grouping,
  and digit group separator character used in the journal.

- hledger print shows amounts for all postings, and shows unit prices for
  amounts which have them. (This means that it does not currently print
  multi-commodity transactions in valid journal format.)

- hledger print ignores the --date2 flag, always showing both dates.
  ledger print shows only the secondary date with --aux-date, but not
  vice versa.

- hledger's default commodity directive (D) sets the commodity for
  subsequent commodityless amounts, and sets that commodity's display
  settings if such an amount is the first seen. ledger uses D only for
  commodity display settings and for the entry command.

- hledger generates a description for timelog sessions, instead of
  taking it from the clock-out entry

### Troubleshooting

Sorry you're here! There are a lot of ways things can go wrong. Here are
some known issues and things to try. Please also seek
[support](DEVELOPMENT.html#support) from the
[IRC channel](irc://irc.freenode.net/#ledger),
[mail list](http://list.hledger.org) or
[bug tracker](http://bugs.hledger.org).

#### Installation issues

Starting from the top, consider whether each of these might apply to
you. Tip: blindly reinstalling/upgrading everything in sight probably
won't work, it's better to go in small steps and understand the problem,
or get help.

#. **Running hledger says something like No command 'hledger' found**  
  cabal installs binaries into a special directory, which should be added
  to your PATH environment variable.  On unix-like systems, it is
  ~/.cabal/bin.

#. **Did you cabal update ?**  
  If not, `cabal update` and try again.

#. **Do you have a new enough version of GHC ?**  
  Run `ghc --version`. hledger requires GHC 7.0 or greater
  (on [some platforms](#5551), 7.2.1 can be helpful).

#. **Do you have a new enough version of cabal ?**  
  Avoid ancient versions.  `cabal --version` should report at least
  0.10 (and 0.14 is much better). You may be able to upgrade it with:
  
        $ cabal update
        $ cabal install cabal-install-0.14

#. **Are your installed GHC/cabal packages in good repair ?**  
  Run `ghc-pkg check`. If it reports problems, some of your packages have
  become inconsistent, and you should fix these first. 
  [ghc-pkg-clean](https://gist.github.com/1185421) is an easy way.

#. <a name="cabaldeps" />**cabal can't satisfy the new dependencies due to old installed packages**  
  Cabal dependency failures become more likely as you install more
  packages over time. If `cabal install hledger-web --dry` says it can't
  satisfy dependencies, you have this problem. You can:
  
    a. try to understand which packages to remove (with `ghc-pkg unregister`)
       or which constraints to add (with `--constraint 'PKG == ...'`) to help cabal
       find a solution

    b. install into a fresh environment created with
       [virthualenv](http://hackage.haskell.org/package/virthualenv) or
       [cabal-dev](http://hackage.haskell.org/package/cabal-dev)

    c. or (easiest) erase your installed packages with
       [ghc-pkg-reset](https://gist.github.com/1185421) and try again.

#. **Dependency or compilation error in one of the new packages ?**  
   If cabal starts downloading and building packages and then terminates
   with an error, look at the output carefully and identify the problem
   package(s).  If necessary, add `-v2` or `-v3` for more verbose
   output. You can install the new packages one at a time to troubleshoot,
   but remember cabal is smarter when installing all packages at once.

    Often the problem is that you need to install a particular C library
   using your platform's package management system. Or the dependencies
   specified on a package may need updating. Or there may be a compilation
   error.  If you find an error in a hledger package, check the
   [recent commits](http://hub.darcs.net/simon/hledger/changes) to
   see if the [latest development version](#installing) might have a fix.

#. **ExitFailure 11**  
  See
  [http://hackage.haskell.org/trac/hackage/ticket/777](http://hackage.haskell.org/trac/hackage/ticket/777).
  This means that a build process has been killed, usually because it grew
  too large.  This is common on memory-limited VPS's and with GHC 7.4.1.
  Look for some memory-hogging processes you can kill, increase your RAM,
  or limit GHC's heap size by doing `cabal install ... --ghc-options='+RTS
  -M400m'` (400 megabytes works well on my 1G VPS, adjust up or down..)

#. <a name="5551" />**Can't load .so/.DLL for: ncursesw (/usr/lib/libncursesw.so: file too short)**  
  (or similar): cf [GHC bug #5551](http://hackage.haskell.org/trac/ghc/ticket/5551).
  Upgrade GHC to 7.2.1, or try your luck with [this workaround](http://eclipsefp.github.com/faq.html).

#. <a name="iconv" />**Undefined iconv symbols on OS X**  
   This kind of error:

        Linking dist/build/hledger/hledger ...
        Undefined symbols:
          "_iconv_close", referenced from:
              _hs_iconv_close in libHSbase-4.2.0.2.a(iconv.o)
          "_iconv", referenced from:
              _hs_iconv in libHSbase-4.2.0.2.a(iconv.o)
          "_iconv_open", referenced from:
              _hs_iconv_open in libHSbase-4.2.0.2.a(iconv.o)

    probably means you are on a mac with macports libraries installed, cf
    [http://hackage.haskell.org/trac/ghc/ticket/4068](http://hackage.haskell.org/trac/ghc/ticket/4068).
    To work around temporarily, add this --extra-lib-dirs flag:

        $ cabal install hledger --extra-lib-dirs=/usr/lib

    or permanently, add this to ~/.cabal/config:
    
        extra-lib-dirs: /usr/lib

#. **hledger-vty requires curses-related libraries**  
  On Ubuntu, eg, you'll need the `libncurses5-dev` package. On Windows,
  these are not available (unless perhaps via Cygwin.)

#. **hledger-chart requires GTK-related libraries**  
  On Ubuntu, eg, install the `libghc6-gtk-dev` package. See also [Gtk2Hs installation notes](http://code.haskell.org/gtk2hs/INSTALL).

#### Usage issues

Here are some issues you might encounter when you run hledger:

12. **hledger fails to parse some valid ledger files**  
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
