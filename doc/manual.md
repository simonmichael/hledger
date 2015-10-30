<!-- hledger.org and hledger repo versions last synced: 2014/5/1 -->

* toc

# hledger User Manual

## Introduction and overview

[hledger](/) is a program for tracking money, time, or any other
commodity, using double-entry accounting and a simple, editable file
format.  It is inspired by and largely compatible with
[ledger](http://ledger-cli.org). Its basic function is to read a plain
text file describing financial transactions, and quickly generate
useful reports via the command line. It can also help you record
transactions, and there is also a web interface. You can use it to,
eg:

- track spending and income
- track unpaid or due invoices
- track time and report by day/week/month/project
- get accurate numbers for client billing and tax filing

hledger is Free Software released under GPL version 3 or later, and is
tested on unix, mac and windows.  See [Download](download.html) for
installation help.

This manual is the reference for every part of hledger's functionality;
this version documents hledger and hledger-web 0.27
<!-- [hledger 0.25](http://hackage.haskell.org/package/hledger-0.25) -->
<!-- and [hledger-web 0.25](http://hackage.haskell.org/package/hledger-web-0.25). -->
If you find anything missing or incorrect, please report it as a bug.
Patches and feedback are always welcome.

If you're just starting with hledger, there's no need to read all of this.
Instead, I suggest (in addition to, or instead of, the step-by-step [tutorial](step-by-step)):

1. read [Usage](#usage) to learn the basic UI and start a journal file
2. and the first part of [Journal](#journal) which explains the journal file
3. then try out the [commands](#commands)
4. when you're ready, learn how to refine them with search [queries](#queries) and command-line [options](#options).

Here is an overview of hledger's commands.

**Built-in commands:**

- [accounts](#accounts) - show account names
- [activity](#activity) - show a histogram of transaction activity
- [add](#add) - interactively prompt for new journal entries
- [balance](#balance) - show accounts and their balances in one or more periods
- [balancesheet](#balancesheet) - show asset and liability balances
- [cashflow](#cashflow) - show changes in asset balances
- [incomestatement](#incomestatement) - show revenues and expenses
- [print](#print) - show journal entries
- [register](#register) - show postings, usually to a specific account, in one or more periods
- [stats](#stats) - show some journal summary info
- [test](#test) - run built-in unit tests

**Add-on commands:**
(install the corresponding packages to make these available)

- [autosync](#autosync) - downloads OFX data from banks, converts OFX to journal entries, and prints the new ones
- [diff](#diff) - compare two journal files and show differing transactions
- [interest](#interest) - generate interest transactions for various schemes
- [irr](#irr) - calculate the internal rate of return of an account
- [web](#web) - a web UI for browsing transactions and accounts and adding new ones

**Experimental commands:**
(extra scripts available in the hledger source)

- [equity](#equity) - generate an "opening balances" entry restoring all account balances
- [print-unique](#print-unique) - show only journal entries with unique descriptions
- [rewrite](#rewrite) - like print, but adds custom postings to matched entries

<!-- Unmaintained commands: -->
<!-- - [chart](#chart) -->
<!-- - [vty](#vty) -->

## Usage

Basic usage is:

```shell
$ hledger COMMAND [OPTIONS] [ARGS]
```

Most [commands](#commands) query or operate on a
[journal file](#journal), which by default is `.hledger.journal`
in your home directory. You can specify a different file with the `-f`
option or `LEDGER_FILE` environment variable, or standard input with `-f-`.

Options are similar across most commands, with some variations; use
`hledger COMMAND --help` for details.

Most options must appear after COMMAND, not before it; but the
following general options can appear anywhere: `-f`, `--rules-file`,
`--alias`, `--ignore-assertions`, `--help`, `--debug`, `--version`.

If an option is repeated, the last one takes precedence. Eg `-p jan -p
feb` is equivalent to `-p feb`.

Arguments are also command-specific, but usually they form a
[query](#queries) which selects a subset of the journal, eg transactions
in a certain account.

To create an initial journal, run `hledger add` and follow the prompts to
enter some transactions.  Or, save this
[sample file](https://raw.github.com/simonmichael/hledger/master/data/sample.journal) as
`.hledger.journal` in your home directory. Now try some commands, eg like these:

``` {.shell .bold}
$ hledger                                 # show available commands
$ hledger add                             # add more transactions to the journal file
$ hledger balance                         # all accounts with aggregated balances
$ hledger balance --help                  # show help for balance command
$ hledger balance --depth 1               # only top-level accounts
$ hledger register                        # show account postings, with running total
$ hledger reg income                      # show postings to/from income accounts
$ hledger reg 'assets:some bank:checking' # show postings to/from this checking account
$ hledger print desc:shop                 # show transactions with shop in the description
$ hledger activity -W                     # show transaction counts per week as a bar chart
```

## Data formats

### Journal

hledger's usual data source is a plain text file containing journal entries in hledger journal format.
This file represents a standard accounting [general journal](http://en.wikipedia.org/wiki/General_journal).
I use file names ending in `.journal`, but that's not required.
The journal file contains a number of transaction entries, 
each describing a transfer of money (or any commodity) between two or more named accounts,
in a simple format readable by both hledger and humans.

hledger's journal format is a compatible subset, [mostly](faq.html#file-format-differences),
of [ledger's journal format](http://ledger-cli.org/3.0/doc/ledger3.html#Journal-Format),
so hledger can work with [compatible](faq.html#file-format-differences) ledger journal files as well.
It's safe, and encouraged, to run both hledger and ledger on the same journal file,
eg to validate the results you're getting.

You can use hledger without learning any more about this file; 
just use the [add](#add) or [web](#web) commands to create and update it. 
Many users, though, also edit the journal file directly with a text editor, perhaps assisted by the helper modes for emacs or vim.

Here's an example:

```journal
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
```

Now let's explore the available journal file syntax in detail.

#### Entries

Each journal entry begins with a [simple date](#simple-dates) in
column 0, followed by three optional fields with spaces between them:

- a status flag, which can be empty or `!` or `*` (meaning "uncleared", "pending" and "cleared", or whatever you want)
- a transaction code (eg a check number),
- and/or a description

then some number of postings, of some amount to some account, each on
its own line. Usually there are at least two postings, though one or
even none is possible.

The ([real](#virtual-postings)) posting amounts within a transaction
must always balance, ie add up to 0.  Optionally one amount can be
left blank, in which case it will be inferred.

#### Dates

##### Simple dates

Within a journal file, transaction dates use Y/M/D (or Y-M-D or Y.M.D)
Leading zeroes are optional.
The year may be omitted, in which case it defaults to the current
year, or you can set the default year with a
[default year directive](#default-year).

Some examples: `2010/01/31`, `1/31`, `2010-01-31`, `2010.1.31`.

##### Secondary dates

Real-life transactions sometimes involve more than one date - eg the date
you write a cheque, and the date it clears in your bank.  When you want to
model this, eg for more accurate balances, write both dates separated by
an equals sign. The *primary date*, on the left, is used by default; the
*secondary date*, on the right, is used when the `--date2` flag is specified
(For Ledger compatibility, `--aux-date` or `--effective` also work.)

Their meaning is up to you, but it's best to follow a consistent rule.
Eg write the bank's clearing date as primary, and when needed, the
date the transaction was initiated as secondary.

Here's an example. Note that a secondary date will use the year of the
primary date if unspecified.

```journal
2010/2/23=2/19 movie ticket
  expenses:cinema                   $10
  assets:checking
```

<div style="clear:both;"></div>
```{.shell}
$ hledger register checking
2010/02/23 movie ticket         assets:checking                $-10         $-10
```

<div style="clear:both;"></div>
```{.shell}
$ hledger register checking --date2
2010/02/19 movie ticket         assets:checking                $-10         $-10
```

Secondary dates require some effort: you must use them consistently in
your journal entries and remember whether to use or not use the
`--date2` flag for your reports. Arguably they are now obsolete,
superseded by...

##### Posting dates

You can give individual postings a different date from their parent
transaction, by adding a [posting tag](#tags) (see below) like
`date:DATE`, where DATE is a [simple date](#simple-dates).  This is
probably the best way to control posting dates precisely. Eg in this
example the expense should appear in May reports, and the deduction
from checking should be reported on 6/1 for easy bank reconciliation:

``` {.journal}
2015/5/30
    expenses:food     $10   ; food purchased on saturday 5/30
    assets:checking         ; bank cleared it on monday, date:6/1
```

<div style="clear:both;"></div>
```{.shell}
$ hledger -f tt.j register food
2015/05/30                      expenses:food                  $10           $10
```

<div style="clear:both;"></div>
```{.shell}
$ hledger -f tt.j register checking
2015/06/01                      assets:checking               $-10          $-10
```

A posting date will use the year of the transaction date if unspecified.

You can also set the secondary date, with `date2:DATE2`.
For compatibility, Ledger's older posting date syntax is also
supported: `[DATE]`, `[DATE=DATE2]` or `[=DATE2]` in a posting
comment.

When using any of these forms, be sure to provide a valid simple date
or you'll get a parse error. Eg a `date:` tag with no value is not
allowed.

#### Account names

Account names typically have several parts separated by a full colon, from
which hledger derives a hierarchical chart of accounts. They can be
anything you like, but in finance there are traditionally five top-level
accounts: `assets`, `liabilities`, `income`, `expenses`, and `equity`.

Account names may contain single spaces, eg: `assets:accounts receivable`.
Because of this, they must always be followed by at least two spaces (or newline).

Account names can be [aliased](#account-aliases).

#### Amounts

After the account name, there is usually an amount.
Important: between account name and amount, there must be **two or more** spaces.

The amount is a number, optionally with a currency symbol or commodity name on either the left or right.
Negative amounts may have the minus sign either before or after the currency symbol (`-$1` or `$-1`).
Commodity names which contain more than just letters should be enclosed in double quotes (`1 "person hours"`).

##### Decimal points and digit groups

hledger supports flexible decimal point and digit group separator styles,
to support international variations. Numbers can use either a period (`.`)
or a comma (`,`) as decimal point. They can also have digit group
separators at any position (eg thousands separators) which can be comma or
period - whichever one you did not use as a decimal point. If you use
digit group separators, you must also include a decimal point in at least
one number in the same commodity, so that hledger knows which character is
which. Eg, write `$1,000.00` or `$1.000,00`.

##### Amount display styles

Based on how you format amounts, hledger will infer canonical display
styles for each commodity, and use these when displaying amounts in that
commodity. Amount styles include:

- the position (left or right) and spacing (space or no separator) of the commodity symbol
- the digit group separator character (comma or period) and digit group sizes, if any
- the decimal point character (period or comma)
- the display precision (number of decimal places displayed)

The canonical style is generally the style of the first posting amount seen in a commodity.
However the display precision will be the highest precision seen in all posting amounts in that commmodity.

The precisions used in a price amount, or a D directive, don't affect the canonical display precision directly, but they can affect it indirectly, eg when D's default commodity is applied to a commodity-less amount or when an amountless posting is balanced using a price's commodity (actually this last case does not influence the canonical display precision but probably should).


#### Virtual Postings

When you parenthesise the account name in a posting, that posting is considered *virtual*, which
means:

- it is ignored when checking that the transaction is balanced
- it is excluded from reports when the `--real/-R` flag is used, or the `real:1` query.

You could use this, eg, to set an account's opening balance without needing to use the
`equity:opening balances` account:

```journal
1/1 special unbalanced posting to set initial balance
  (assets:checking)   $1000
```
##### Balanced Virtual Postings

When the account name is bracketed, the posting is *balanced virtual*, which is just like a virtual posting except the balanced virtual postings in a transaction must balance to 0, like the real postings (but separately from them). Balanced virtual postings are also excluded by `--real/-R` or `real:1`.

Virtual postings are a feature inherited from Ledger can can occasionally be useful, but they can be a crutch and you should think twice or three times before using them. You can almost always find an equivalent journal entry using two or more real postings that will be more correct and more error-proof.

#### Balance Assertions

hledger supports ledger-style
[balance assertions](http://ledger-cli.org/3.0/doc/ledger3.html#Balance-assertions)
in journal files.
These look like `=EXPECTEDBALANCE` following a posting's amount. Eg in
this example we assert the expected dollar balance in accounts a and b after
each posting:

``` {.journal}
2013/1/1
  a   $1  =$1
  b       =$-1

2013/1/2
  a   $1  =$2
  b  $-1  =$-2
```

After reading a journal file, hledger will check all balance
assertions and report an error if any of them fail. Balance assertions
can protect you from, eg, inadvertently disrupting reconciled balances
while cleaning up old entries. You can disable them temporarily with
the `--ignore-assertions` flag, which can be useful for
troubleshooting or for reading Ledger files.

##### Assertions and ordering

hledger sorts an account's postings and assertions first by date and
then (for postings on the same day) by parse order. Note this is
different from Ledger, which sorts assertions only by parse
order. (Also, Ledger assertions do not see the accumulated effect of
repeated postings to the same account within a transaction.)

So, hledger balance assertions keep working if you reorder
differently-dated transactions within the journal. But if you reorder
same-dated transactions or postings, assertions might break and require
updating. This order dependence does bring an advantage: precise
control over the order of postings and assertions within a day, so you
can assert intra-day balances.

With [included files](#including-other-files), things are a little
more complicated. Including preserves the ordering of postings and
assertions. If you have multiple postings to an account on the same
day, split across different files, and you also want to assert the
account's balance on the same day, you'll have to put the assertion
in the right file.


##### Assertions and commodities

The asserted balance must be a simple single-commodity amount, and in
fact the assertion checks only this commodity's balance within the
(possibly multi-commodity) account balance.  We could call this a
partial balance assertion.  This is compatible with Ledger, and makes
it possible to make assertions about accounts containing multiple
commodities.

To assert each commodity's balance in such a multi-commodity account,
you can add multiple postings (with amount 0 if necessary). But note
that no matter how many assertions you add, you can't be sure the
account does not contain some unexpected commodity. (We'll add support
for this kind of total balance assertion if there's demand.)

##### Assertions and subaccounts

Balance assertions do not count the balance from subaccounts; they check
the posted account's exclusive balance. For example:
``` {.journal}
1/1
  checking:fund   1 = 1  ; post to this subaccount, its balance is now 1
  checking        1 = 1  ; post to the parent account, its exclusive balance is now 1
  equity
```
The balance report's flat mode shows these exclusive balances more clearly:
```shell
$ hledger bal checking --flat
                   1  checking
                   1  checking:fund
--------------------
                   2
```

###### Assertions and virtual postings

Balance assertions are checked against all postings, both real and
[virtual](#virtual-postings). They are not affected by the `--real/-R`
flag or `real:` query.


#### Prices

##### Transaction prices

When recording a transaction, you can also record an amount's price in another commodity.
This documents the exchange rate, cost (of a purchase), or selling price (of a sale) that was in effect within this particular transaction (or more precisely, within the particular posting).
These transaction prices are fixed, and do not change.

Such priced amounts can be displayed in their transaction price's
commodity, by using the `--cost/-B` flag (B for "cost Basis"),
supported by most hledger commands.

There are three ways to specify a transaction price:

1. Write the unit price (aka exchange rate), as `@ UNITPRICE` after the amount:

    ```journal
    2009/1/1
      assets:foreign currency   €100 @ $1.35  ; one hundred euros at $1.35 each
      assets:cash
    ```

2. Or write the total price, as `@@ TOTALPRICE` after the amount:

    ```journal
    2009/1/1
      assets:foreign currency   €100 @@ $135  ; one hundred euros at $135 for the lot
      assets:cash
    ```

3. Or let hledger infer the price so as to balance the transaction.
   To permit this, you must fully specify all posting amounts, and
   their sum must have a non-zero amount in exactly two commodities:

    ```journal
    2009/1/1
      assets:foreign currency   €100          ; one hundred euros
      assets:cash              $-135          ; exchanged for $135
    ```

With any of the above examples we get:

```shell
$ hledger print -B
2009/01/01
    assets:foreign currency       $135.00
    assets:cash                  $-135.00
```

Example use for transaction prices: recording the effective conversion
rate of purchases made in a foreign currency.

##### Market prices

Market prices are not tied to a particular transaction; they represent
historical exchange rates between two commodities, usually from some
public market which publishes such rates.

When market prices are known, the `-V/--value` option will use them to
convert reported amounts to their market value as of the report end
date. This option is currently available only with the
[balance](#balance) command.

You record market prices (Ledger calls them historical prices) with a
P directive, in the journal or perhaps in a separate
[included](#including-other-files) file.  Market price directives have
the format:
```journal
P DATE COMMODITYSYMBOL UNITPRICE
```
<!-- (A time and numeric time zone are allowed but ignored, like ledger.) -->

For example, the following directives say that the euro's exchange rate was 1.35 US dollars during 2009, and $1.40 from 2010 onward (and unknown before 2009).
```journal
P 2009/1/1 € $1.35
P 2010/1/1 € $1.40
```

Example use for market prices: tracking the value of stocks.

<!--
This is different from Ledger, where transaction prices fluctuate by
default.  Ledger has a different syntax for specifying
[fixed prices](http://ledger-cli.org/3.0/doc/ledger3.html#Fixing-Lot-Prices):
`{=PRICE}`.  hledger parses that syntax, and (currently) ignores it.
-->
<!-- hledger treats this as an alternate spelling of `@ PRICE`, for greater compatibility with Ledger files. -->

#### Comments

Lines in the journal beginning with a semicolon (`;`) or hash (`#`) or
asterisk (`*`) are comments, and will be ignored. (Asterisk comments
make it easy to treat your journal like an org-mode outline in emacs.)

Also, anything between [`comment` and `end comment` directives](#multi-line-comments) is a (multi-line) comment.
If there is no `end comment`, the comment extends to the end of the file.

You can attach comments to a transaction by writing them after the
description and/or indented on the following lines (before the
postings).  Similarly, you can attach comments to an individual
posting by writing them after the amount and/or indented on the
following lines.

Some examples:

```journal
# a journal comment

; also a journal comment

comment
This is a multiline comment,
which continues until a line
where the "end comment" string
appears on its own.
end comment

2012/5/14 something  ; a transaction comment
    ; the transaction comment, continued
    posting1  1  ; a comment for posting 1
    posting2
    ; a comment for posting 2
    ; another comment line for posting 2
; a journal comment (because not indented)
```

#### Tags

A *tag* is a word followed by a full colon inside a transaction or
posting [comment](#comments).  You can write multiple tags, comma
separated. Eg: `; a comment containing sometag:, anothertag:`.  You can search for tags
with the [`tag:` query](manual#queries).

A tag can also have a value, which is any text between the colon and
the next comma or newline, excluding leading/trailing whitespace.
(So hledger tag values can not contain commas or newlines).

Tags in a transaction comment affect the transaction and all of its postings,
while tags in a posting comment affect only that posting.
For example, the following transaction has three tags (A, TAG2, third-tag)
and the posting has four (A, TAG2, third-tag, posting-tag):

``` {.journal}
1/1 a transaction  ; A:, TAG2:
    ; third-tag: a third transaction tag, this time with a value
    (a)  $1  ; posting-tag:
```

Tags are like Ledger's
[metadata](http://ledger-cli.org/3.0/doc/ledger3.html#Metadata)
feature, except hledger's tag values are always simple strings.

#### Directives

##### Account aliases

You can define aliases which rewrite your account names (after reading the journal,
before generating reports). hledger's account aliases can be useful for:

- expanding shorthand account names to their full form, allowing easier data entry and a less verbose journal
- adapting old journals to your current chart of accounts
- experimenting with new account organisations, like a new hierarchy or combining two accounts into one
- customising reports

See also [How to use account aliases](how-to-use-account-aliases.html).

###### Basic aliases

To set an account alias, use the `alias` directive in your journal file.
This affects all subsequent journal entries in the current file or its
[included files](#including-other-files).
The spaces around the = are optional:

``` {.journal}
alias OLD = NEW
```

Or, you can use the `--alias 'OLD=NEW'` option on the command line.
This affects all entries. It's useful for trying out aliases interactively.

OLD and NEW are full account names.
hledger will replace any occurrence of the old account name with the
new one. Subaccounts are also affected. Eg:

``` {.journal}
alias checking = assets:bank:wells fargo:checking
# rewrites "checking" to "assets:bank:wells fargo:checking", or "checking:a" to "assets:bank:wells fargo:checking:a"
```

###### Regex aliases

There is also a more powerful variant that uses a regular expression,
indicated by the forward slashes. (This was the default behaviour in hledger 0.24-0.25):

``` {.journal}
alias /REGEX/ = REPLACEMENT
```

or `--alias '/REGEX/=REPLACEMENT'`.

<!-- (Can also be written `'/REGEX/REPLACEMENT/'`). -->
REGEX is a case-insensitive regular expression. Anywhere it matches
inside an account name, the matched part will be replaced by
REPLACEMENT.
If REGEX contains parenthesised match groups, these can be referenced
by the usual numeric backreferences in REPLACEMENT.
Note, currently regular expression aliases may cause noticeable slow-downs.
(And if you use Ledger on your hledger file, they will be ignored.)
Eg:

``` {.journal}
alias /^(.+):bank:([^:]+)(.*)/ = \1:\2 \3
# rewrites "assets:bank:wells fargo:checking" to  "assets:wells fargo checking"
```

###### Multiple aliases

You can define as many aliases as you like using directives or command-line options.
Aliases are recursive - each alias sees the result of applying previous ones.
(This is different from Ledger, where aliases are non-recursive by default).
Aliases are applied in the following order:

1. alias directives, most recently seen first (recent directives take precedence over earlier ones; directives not yet seen are ignored)
2. alias options, in the order they appear on the command line

###### end aliases

You can clear (forget) all currently defined aliases with the `end aliases` directive:

``` {.journal}
end aliases
```

##### Multi-line comments

A line containing just `comment` starts a multi-line comment, and a
line containing just `end comment` ends it. See [comments](#comments).

##### Default commodity

You can set a default commodity, to be used for amounts without one.
Use the D directive with a sample amount.
The commodity (and the sample amount's display style) will be applied to all subsequent commodity-less amounts, up to the next D directive.
(Note this is different from Ledger's default commodity directive.)

Also note the directive itself does not influence the commodity's default
[display style](#amount-display-styles), but the amount it is
applied to might. Here's an example:

```journal
; set £ as the default commodity
D £1,000.00

2010/1/1
  a  2340
  b

2014/1/1
  c  £1000
  d
```
```{.shell}
$ hledger print
2010/01/01
    a     £2,340.00
    b    £-2,340.00

2014/01/01
    c     £1,000.00
    d    £-1,000.00
```

##### Default parent account

You can specify a parent account which will be prepended to all accounts
within a section of the journal. Use the `account` directive like so:

``` {.journal}
account home

2010/1/1
    food    $10
    cash

end
```

If `end` is omitted, the effect lasts to the end of the file.
The above is equivalent to:

``` {.journal}
2010/01/01
    home:food           $10
    home:cash          $-10
```

Included files are also affected, eg:

``` {.journal}
account business
include biz.journal
end
account personal
include personal.journal
end
```

##### Default year

You can set a default year to be used for subsequent dates which don't
specify a year. This is a line beginning with `Y` followed by the year. Eg:

``` {.journal}
Y2009      ; set default year to 2009

12/15      ; equivalent to 2009/12/15
  expenses  1
  assets

Y2010      ; change default year to 2010

2009/1/30  ; specifies the year, not affected
  expenses  1
  assets

1/31       ; equivalent to 2010/1/31
  expenses  1
  assets
```

##### Including other files

You can pull in the content of additional journal files by writing an
include directive, like this:

``` {.journal}
include path/to/file.journal
```

If the path does not begin with a slash, it is relative to the current file.

The `include` directive may only be used in journal files, and currently
it may only include other journal files (eg, not CSV or timelog files.)


### Timelog

hledger can also read timelog files.
[As with Ledger](http://ledger-cli.org/3.0/doc/ledger3.html#Time-Keeping),
these are (a subset of)
[timeclock.el](http://www.emacswiki.org/emacs/TimeClock)'s format,
containing clock-in and clock-out entries as in the example below.
The date is a [simple date](#simple-dates) (also, [default year directives](#default-year) work).
The time format is HH:MM[:SS][+-ZZZZ]. Seconds and timezone are optional.
The timezone, if present, must be four digits and is ignored
(currently the time is always interpreted as a local time).

```timelog
i 2015/03/30 09:00:00 some:account name  optional description after two spaces
o 2015/03/30 09:20:00
i 2015/03/31 22:21:45 another account
o 2015/04/01 02:00:34
```

hledger treats each clock-in/clock-out pair as a transaction posting
some number of hours to an account. Or if the session spans more than
one day, it is split into several transactions, one for each day. For
the above time log, `hledger print` generates these journal entries:

``` {.shell}
$ hledger -f t.timelog print
2015/03/30 * optional description after two spaces
    (some:account name)         0.33h

2015/03/31 * 22:21-23:59
    (another account)         1.64h

2015/04/01 * 00:00-02:00
    (another account)         2.01h

```

Here is a
[sample.timelog](https://raw.github.com/simonmichael/hledger/master/data/sample.timelog) to
download and some queries to try:

``` {.shell .bold}
$ hledger -f sample.timelog balance                               # current time balances
$ hledger -f sample.timelog register -p 2009/3                    # sessions in march 2009
$ hledger -f sample.timelog register -p weekly --depth 1 --empty  # time summary by week
```

To generate time logs, ie to clock in and clock out, you could:

- use emacs and the built-in timeclock.el, or
  the extended [timeclock-x.el](http://www.emacswiki.org/emacs/timeclock-x.el)
  and perhaps the extras in [ledgerutils.el](http://hub.darcs.net/simon/ledgertools/ledgerutils.el)

- at the command line, use these bash aliases:
    ``` {.shell bold}
    alias ti="echo i `date '+%Y-%m-%d %H:%M:%S'` \$* >>$TIMELOG"
    alias to="echo o `date '+%Y-%m-%d %H:%M:%S'` >>$TIMELOG"
    ```
- or use the old `ti` and `to` scripts in the [ledger 2.x repository](https://github.com/ledger/ledger/tree/maint/scripts).
  These rely on a "timeclock" executable which I think is just the ledger 2 executable renamed.

### CSV

hledger can also read
[CSV](http://en.wikipedia.org/wiki/Comma-separated_values) files,
converting each CSV record into a journal entry (transaction),
if you provide some conversion hints in a "rules file".
This file should be named like the CSV file with an additional `.rules` suffix (eg: `mybank.csv.rules`);
or, you can specify the file with `--rules-file PATH`.
hledger will create it if necessary, with some default rules which you'll need to adjust.
At minimum, the rules file must specify the `date` and `amount` fields.
For an example, see [How to read CSV files](how-to-read-csv-files.html).

(For CSV output, see [CSV output](#csv-output).)


#### CSV rules

The following six kinds of rule can appear in the rules file, in any order.
Blank lines and lines beginning with `#` or `;` are ignored.

**`skip` *N***\
Skip this number of CSV records at the beginning.
You'll need this when your CSV contains header lines. Eg:
<!-- XXX -->
<!-- hledger tries to skip initial CSV header lines automatically. -->
<!-- If it guesses wrong, use this directive to skip exactly N lines. -->
<!-- This can also be used in a conditional block to ignore certain CSV records. -->
```rules
# ignore the first CSV line
skip 1
```

**`date-format` *DATEFMT***\
When your CSV date fields are not formatted like `YYYY/MM/DD` (or `YYYY-MM-DD` or `YYYY.MM.DD`),
you'll need to specify the format.
DATEFMT is a [strptime-like date parsing pattern](http://hackage.haskell.org/packages/archive/time/latest/doc/html/Data-Time-Format.html#v:formatTime),
which must parse the date field values completely. Examples:

``` {.rules .display-table}
# parses "6/11/2013":
date-format %-d/%-m/%Y
```

``` {.rules .display-table}
# parses "11/06/2013":
date-format %m/%d/%Y
```

``` {.rules .display-table}
# parses "2013-Nov-06":
date-format %Y-%h-%d
```

``` {.rules .display-table}
# parses "11/6/2013 11:32 PM":
date-format %-m/%-d/%Y %l:%M %p
```

**`fields` *CSVFIELDNAME1*, *CSVFIELDNAME2*...**\
(Field list)\
This (a) names the CSV fields (names may not contain whitespace),
and (b) assigns them to journal entry fields if you use any of these standard field names:
`date`, `date2`, `status`, `code`, `description`, `comment`, `account1`, `account2`, `amount`, `amount-in`, `amount-out`, `currency`.
Eg:
```rules
# use the 1st, 2nd and 4th CSV fields as the entry date, description and amount
# give the 7th and 8th fields custom names for later reference
fields date, description, , amount, , , somefield, anotherfield
```

***ENTRYFIELDNAME* *FIELDVALUE***\
(Field assignment)\
This sets a journal entry field (one of the standard names above) to the given text value,
which can include CSV field values interpolated by name (`%CSVFIELDNAME`) or 1-based position (`%N`).
<!-- Whitespace before or after the value is ignored. -->
Field assignments can be used instead of or in addition to a field list.
Eg:
```{.rules .display-table}
# set the amount to the 4th CSV field with "USD " prepended
amount USD %4
```
```{.rules .display-table}
# combine three fields to make a comment (containing two tags)
comment note: %somefield - %anotherfield, date: %1
```

**`if` *PATTERN*\
&nbsp;&nbsp;&nbsp;&nbsp;*FIELDASSIGNMENTS*...**\
or\
**`if`\
*PATTERN*\
*PATTERN*...\
&nbsp;&nbsp;&nbsp;&nbsp;*FIELDASSIGNMENTS*...**\
(Conditional block)\
This applies one or more field assignments, only to those CSV records matched by one of the PATTERNs.
The patterns are case-insensitive regular expressions which match anywhere
within the whole CSV record (it's not yet possible to match within a
specific field).  When there are multiple patterns they should be written
on separate lines, unindented.
The field assignments are on separate lines indented by at least one space.
Examples:
```{.rules .display-table}
# if the CSV record contains "groceries", set account2 to "expenses:groceries"
if groceries
 account2 expenses:groceries
```
```{.rules .display-table}
# if the CSV record contains any of these patterns, set account2 and comment as shown
if
monthly service fee
atm transaction fee
banking thru software
 account2 expenses:business:banking
 comment  XXX deductible ? check
```

**`include` *RULESFILE***\
Include another rules file at this point. `RULESFILE` is either an absolute file path or
a path relative to the current file's directory. Eg:
```rules
# rules reused with several CSV files
include common.rules
```

#### Other CSV tips

Each generated journal entry will have two postings, to `account1` and `account2` respectively.
Currently it's not possible to generate entries with more than two postings.

If the CSV has debit/credit amounts in separate fields, assign to the `amount-in` and `amount-out` pseudo fields instead of `amount`.

If the CSV has the currency in a separate field, assign that to the `currency` pseudo field which will be automatically prepended to the amount.
(Or you can do the same thing with a field assignment.)

If an amount value is parenthesised, it will be de-parenthesised and sign-flipped automatically.

The generated journal entries will be sorted by date.
The original order of same-day entries will be preserved, usually.
<!-- (by reversing the CSV entries if they seem to be in reverse date order). -->


## Options

Use `hledger COMMAND --help` to list the options available for that
command.  The following general options are common to most commands,
though not every one is applicable in all cases:

```
General flags:
  -f --file=FILE          use a different input file. For stdin, use -
     --rules-file=RFILE   CSV conversion rules file (default: FILE.rules)
     --alias=OLD=NEW      display accounts named OLD as NEW
     --ignore-assertions  ignore any balance assertions in the journal
  -b --begin=DATE         include postings/txns on or after this date
  -e --end=DATE           include postings/txns before this date
  -D --daily              multiperiod/multicolumn report by day
  -W --weekly             multiperiod/multicolumn report by week
  -M --monthly            multiperiod/multicolumn report by month
  -Q --quarterly          multiperiod/multicolumn report by quarter
  -Y --yearly             multiperiod/multicolumn report by year
  -p --period=PERIODEXP   set start date, end date, and/or reporting interval
                          all at once (overrides the flags above)
     --date2 --aux-date   use postings/txns' secondary dates instead
  -C --cleared            include only cleared postings/txns
     --pending            include only pending postings/txns
  -U --uncleared          include only uncleared (and pending) postings/txns
  -R --real               include only non-virtual postings
     --depth=N            hide accounts/postings deeper than N
  -E --empty              show empty/zero things which are normally omitted
  -B --cost               show amounts in their cost price's commodity
  -h --help               show general help or (after command) command help
     --debug=N            show debug output if N is 1-9 (default: 0)
     --version            show version information
```

Read on for some additional notes.

### Smart dates

Unlike dates in the journal file, hledger's user interfaces accept a
more flexible date syntax.  These "smart" dates allow some english
words, can be relative to today's date, and assume 1 when less-significant date parts are omitted.

Examples:

|
|--------------------------------------------------|------------------------------------------------------|
| `2009/1/1`, `2009/01/01`, `2009-1-1`, `2009.1.1` &nbsp; | simple dates, several separators allowed             |
| `2009/1`, `2009`                                 | same as above - a missing day or month defaults to 1 |
| `1/1`, `january`, `jan`, `this year`             | relative dates, meaning january 1 of the current year|
| `next year`                                      | january 1 of next year                               |
| `this month`                                     | the 1st of the current month                         |
| `this week`                                      | the most recent monday                               |
| `last week`                                      | the monday of the week before this one               |
| `lastweek`                                       | spaces are optional                              |
| `today`, `yesterday`, `tomorrow`                 |                                                      |

### Reporting interval

A reporting interval can be specified so that commands like
[register](#register), [balance](#balance) and [activity](#activity) will divide their
reports into multiple report periods.  The basic intervals can be
selected with one of `-D/--daily`, `-W/--weekly`, `-M/--monthly`,
`-Q/--quarterly`, or `-Y/--yearly`.  More complex intervals may be
specified with a period expression.

### Period expressions

The `-p/--period` option accepts period expressions, a shorthand way
of expressing a start date, end date, and or reporting interval all at
once. Note a period expression on the command line will cause any other date
flags (`-b`/`-e`/`-D`/`-W`/`-M`/`-Q`/`-Y`) to be ignored.

hledger's period expressions are similar to Ledger's, though not identical.
Here's a basic period expression specifying the first quarter of 2009.  Note
hledger always treats start dates as inclusive and end dates as exclusive:

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

Period expressions can also start with (or be) a reporting interval:
`daily`, `weekly`, `monthly`, `quarterly`, `yearly`, or one of the
`every ...` expressions below. Optionally the word `in` may appear
between the reporting interval and the start/end dates.
Examples:

    -p "weekly from 2009/1/1 to 2009/4/1"
    -p "monthly in 2008"
    -p "bimonthly from 2008"
    -p "quarterly"
    -p "every 2 weeks"
    -p "every 5 days from 1/3"
    -p "every 15th day of month"
    -p "every 4th day of week"

### Depth limiting

With the `--depth N` option, commands like [account](#account), [balance](#balance)
and [register](#register) will show only the uppermost accounts in the account
tree, down to level N. Use this when you want a summary with less detail.

### Multiple files

One may specify the `--file FILE` option multiple times. This is equivalent to
concatenating the files to standard input and passing `--file -`, except that
the add command functions normally and adds entries to the first specified file.

## Queries

One of hledger's strengths is being able to quickly report on precise subsets of your data.\
Most commands accept an optional query expression, written as arguments after the command name,
to filter the data by date, account name or other criteria. Query expressions are also used
in the [web ui](#web)'s search form.

The query syntax is similar to a Google search expression: one or
more space-separated search terms, optional prefixes to match specific
fields, quotes to enclose whitespace, etc.
A query term can be any of the following:

---------------------------------- ------------------------------------------------------------------------------------------------------------------------
`REGEX`                            match account names by this regular expression

\                                  \
`acct:REGEX`                       same as above

\                                  \
`amt:N`, `amt:<N`, `amt:<=N`,      match postings with a single-commodity
`amt:>N`, `amt:>=N`                amount that is equal to, less than, or greater than N.
\                                  (Multi-commodity amounts are not tested, and will always match.)
\                                  The comparison has two modes: if N is preceded by a `+` or `-` sign
                                   (or is 0), the two signed numbers are compared. Otherwise, the
                                   absolute magnitudes are compared, ignoring sign.

\                                  \
`code:REGEX`                       match by transaction code (eg check number)

\                                  \
`cur:REGEX`                        match postings or transactions including any amounts
\                                  whose currency/commodity symbol is fully matched by REGEX. (For a
\                                  partial match, use `.*REGEX.*`). Note, to match characters which are
\                                  regex-significant, like the dollar sign (`$`), you need to prepend `\`.
\                                  And when using the command line you need to add one more level
                                   of quoting to hide it from the shell, so eg do: `hledger print cur:'\$'`
                                   or `hledger print cur:\\$`.

\                                  \
`desc:REGEX`                       match transaction descriptions

\                                  \
`date:PERIODEXPR`                  match dates within the specified [period](#period-expressions)
\                                  (which should not include a [reporting interval](#reporting-interval))

\                                  \
`date2:PERIODEXPR`                 as above, but match [secondary dates](#secondary-dates)

\                                  \
`depth:N`                          match (or display, depending on command) accounts at or above this [depth](#depth-limiting)

\                                  \
`real:`, `real:0`                  match real or [virtual](#virtual-postings) postings respectively

\                                  \
`status:*`, `status:!`, `status:`  match cleared, pending, or uncleared/pending transactions respectively
                                   \
                                   \

\                                  \
`tag:REGEX[=REGEX]`                match by [tag](#tags) name, and optionally also by tag value.
\                                  Note a `tag:` query is considered to match a transaction if it matches any of the postings.
\                                  Also remember that postings inherit the tags of their parent transaction.

\                                  \
`not:`                             before any of the above negates the match.

---------------------------------- ------------------------------------------------------------------------------------------------------------------------

\

Note that some of these can also be expressed as command-line options (eg `depth:2` is equivalent to `--depth 2`).
Generally you can mix options and query arguments, and the resulting query will be their intersection.
<!-- (but remember the `-p` [period](#period-expressions) option is a bit special and will cause `-b`, `-e` and other `-p` options to be ignored). -->

hledger query expressions don't support boolean logic (AND, OR, grouping with parentheses).
Instead, multiple query terms are combined as follows:

---

+----------------------------------------+----------------------------------------------------------------------+
|The [print](#print) command             | - match any of the description terms AND                             |
|selects transactions which:             | - have any postings matching any of the positive account terms AND   |
|\                                       | - have no postings matching any of the negative account terms AND    |
|\                                       | - match all the other terms.                                         |
|\                                       |                                                                      |
|\                                       |                                                                      |
+----------------------------------------+----------------------------------------------------------------------+
|All other commands select               | - any of the description terms AND                                   |
|transactions/postings/accounts          | - any of the account terms AND                                       |
|which match (or negatively match):      | - all the other terms.                                               |
|\                                       |                                                                      |
|\                                       |                                                                      |
+----------------------------------------+----------------------------------------------------------------------+

---

## Commands

hledger provides a number of subcommands out of the box; run `hledger` with no arguments to see a list.
More [add-on commands](#add-ons) will appear if you install additional `hledger-*` packages,
or if you put programs or scripts named `hledger-NAME` in your PATH.

To choose a command, write it as the first command-line argument.
You can write its full name (eg `balance`), or one of the
standard short aliases displayed in parentheses in the command list
(eg `bs`), or any unambiguous prefix of a command (eg `inc`).

Below are the commands supported by hledger.
For a quick summary, see the [introduction and overview](#introduction-and-overview) above.
To try out these examples for yourself, use the sample journal. Eg:
```{.shell .bold}
$ wget https://raw.github.com/simonmichael/hledger/master/data/sample.journal
$ export LEDGER_FILE=sample.journal
```

### Built-in commands

#### accounts

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

This command lists all account names, or with query arguments, matched account names.

It shows a flat list by default. In this mode you can add `--drop N`
to omit the first few account name components.

With `--tree`, it shows the account hierarchy.

#### activity

``` {.shell .right}
$ hledger activity --quarterly
2008-01-01 **
2008-04-01 *******
2008-07-01 
2008-10-01 **
```

The activity command displays an ascii histogram showing
transaction counts by day, week, month or other reporting interval
(by day is the default). With query arguments, it counts only matched transactions.

#### add

``` {.shell .right}
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
```

Many hledger users edit their journals directly with a text editor, or generate them from CSV.
For more interactive data entry, there is the `add` command, 
which prompts interactively on the console for new transactions, and appends
them to the journal file. This is the only built-in command which can write to your journal file.

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

Here's [an example](step-by-step.html#record-a-transaction-with-hledger-add).

#### balance

The balance command displays accounts and their balances.
It is the most complex and perhaps most useful command.

##### Simple balance reports

``` {.shell .right}
$ hledger balance
                 $-1  assets
                  $1    bank:saving
                 $-2    cash
                  $2  expenses
                  $1    food
                  $1    supplies
                 $-2  income
                 $-1    gifts
                 $-1    salary
                  $1  liabilities:debts
--------------------
                   0
```

Simple balance reports have no [reporting interval](#reporting-interval).
They show the sum of matched postings in each account.
(If postings are not date-restricted, this is usually the same as the ending balance).

By default, simple balance reports display the accounts as a
hierarchy, with subaccounts indented below their parent. Each
account's balance is the "inclusive" balance - it includes the
balances of any subaccounts.

"Boring parent accounts" (containing a single interesting subaccount
and no balance of their own) are elided into the following line for
more compact output. Use `--no-elide` to prevent this.

Accounts which have zero balance (and no non-zero subaccounts) are
omitted. Use `-E/--empty` to show them.

``` {.shell .right .clear}
$ hledger balance -p 2008/6 expenses --no-total
                  $2  expenses
                  $1    food
                  $1    supplies
```

A final total is displayed by default; use `-N/--no-total` to suppress it.

##### Flat mode

``` {.shell .right}
$ hledger balance -p 2008/6 expenses -N --flat --drop 1
                  $1  food
                  $1  supplies
```

To see a flat list of full account names instead of the default hierarchical display, use `--flat`.
In this mode, accounts (unless depth-clipped) show their "exclusive" balance, excluding any subaccount balances.
In this mode, you can also use `--drop N` to omit the first few account name components.

##### Depth limiting

``` {.shell .right}
$ hledger balance -N --depth 1
                 $-1  assets
                  $2  expenses
                 $-2  income
                  $1  liabilities
```

With `--depth N`, balance shows accounts only to the specified depth.
This is very useful to show a complex charts of accounts in less detail.
In flat mode, balances from accounts below the depth limit will be shown as part of a parent account at the depth limit.

<!-- $ for y in 2006 2007 2008 2009 2010; do echo; echo $y; hledger -f $y.journal balance ^expenses --depth 2; done -->

##### Multicolumn balance reports

With a [reporting interval](#reporting-interval), multiple balance
columns will be shown, one for each report period.
There are three types of multi-column balance report, showing different information:

``` {.shell .right}
$ hledger balance --quarterly income expenses -E
Balance changes in 2008:

                   ||  2008q1  2008q2  2008q3  2008q4 
===================++=================================
 expenses:food     ||       0      $1       0       0 
 expenses:supplies ||       0      $1       0       0 
 income:gifts      ||       0     $-1       0       0 
 income:salary     ||     $-1       0       0       0 
-------------------++---------------------------------
                   ||     $-1      $1       0       0 

```

1. By default: each column shows the sum of postings in that period,
ie the account's change of balance in that period. This is useful eg
for a monthly income statement.
<!--
multi-column income statement: 

   $ hledger balance ^income ^expense -p 'monthly this year' --depth 3

or cashflow statement:

   $ hledger balance ^assets ^liabilities 'not:(receivable|payable)' -p 'weekly this month'
-->

<div style="clear:both;"></div>
``` {.shell .right}
$ hledger balance --quarterly income expenses -E --cumulative
Ending balances (cumulative) in 2008:

                   ||  2008/03/31  2008/06/30  2008/09/30  2008/12/31 
===================++=================================================
 expenses:food     ||           0          $1          $1          $1 
 expenses:supplies ||           0          $1          $1          $1 
 income:gifts      ||           0         $-1         $-1         $-1 
 income:salary     ||         $-1         $-1         $-1         $-1 
-------------------++-------------------------------------------------
                   ||         $-1           0           0           0 

```

2. With `--cumulative`: each column shows the ending balance for that
period, accumulating the changes across periods, starting from 0 at
the report start date. This mode is not often used.

<div style="clear:both;"></div>
``` {.shell .right}
$ hledger balance ^assets ^liabilities -Q 
Balance changes in 2008:

                      ||  2008q1  2008q2  2008q3  2008q4 
======================++=================================
 assets:bank:checking ||      $1       0       0     $-1 
 assets:bank:saving   ||       0      $1       0       0 
 assets:cash          ||       0     $-2       0       0 
 liabilities:debts    ||       0       0       0      $1 
----------------------++---------------------------------
                      ||      $1     $-1       0       0 

$ hledger balance ^assets ^liabilities --quarterly --historical --begin 2008/4/1
Ending balances (historical) in 2008/04/01-2008/12/31:

                      ||  2008/06/30  2008/09/30  2008/12/31 
======================++=====================================
 assets:bank:checking ||          $1          $1           0 
 assets:bank:saving   ||          $1          $1          $1 
 assets:cash          ||         $-2         $-2         $-2 
 liabilities:debts    ||           0           0          $1 
----------------------++-------------------------------------
                      ||           0           0           0 

```

3. With `--historical/-H`: each column shows the actual historical
ending balance for that period, accumulating the changes across
periods, starting from the actual balance at the report start date.
This is useful eg for a multi-period balance sheet, and when
you are showing only the data after a certain start date.

<div style="clear:both;"></div>
``` {.shell .right}
$ hledger balance -Q income expenses --tree -E -TA
Balance changes in 2008:

            ||  2008q1  2008q2  2008q3  2008q4    Total  Average 
============++===================================================
 expenses   ||       0      $2       0       0       $2       $1 
   food     ||       0      $1       0       0       $1        0 
   supplies ||       0      $1       0       0       $1        0 
 income     ||     $-1     $-1       0       0      $-2      $-1 
   gifts    ||       0     $-1       0       0      $-1        0 
   salary   ||     $-1       0       0       0      $-1        0 
------------++---------------------------------------------------
            ||     $-1      $1       0       0        0        0 

# Average is rounded to the dollar here since all journal amounts are

```

Multi-column balance reports display accounts in flat mode by default;
to see the hierarchy, use `--tree`.

Note that with a reporting interval, the report start/end dates will
be "enlarged" if necessary so that they encompass the displayed report
periods. This is so that the first and last periods will be "full" and
comparable to the others.

The `-E/--empty` flag does two things here: first, the report will
show all columns within the specified report period (without -E,
leading and trailing columns with all zeroes are not shown). Second,
all accounts which existed at the report start date will be
considered, not just the ones with activity during the report period
(use -E to include low-activity accounts which would otherwise would
be omitted).

The `-T/--row-total` flag adds an additional column showing the total
for each row.  The `-A/--average` flag adds a column showing the
average value in each row. Note in `--H/--historical` mode only the
average is useful, and in `--cumulative` mode neither is useful.

##### Market value

The `-V/--value` flag converts all the reported amounts to their
"current market value" using their default market price. That is the
latest [market price](#market-prices) (P directive) found in the
journal (or an included file), for the amount's commodity, dated on or
before the report end date.
    
Unlike Ledger, hledger's -V only uses the market prices recorded with
P directives, ignoring transaction prices recorded as part of posting
amounts (which -B/--cost uses). Using -B and -V together is allowed.

<!--
``` {.shell .right}
$ hledger balance -V ...
```
-->

##### Custom balance output

``` {.shell .right}
$ hledger balance --format "%20(account) %12(total)"
              assets          $-1
         bank:saving           $1
                cash          $-2
            expenses           $2
                food           $1
            supplies           $1
              income          $-2
               gifts          $-1
              salary          $-1
   liabilities:debts           $1
---------------------------------
                                0
```

In simple (non-multi-column) balance reports, you can customise the
output with `--format FMT`. FMT (plus a newline) will be displayed for
each account/balance pair. It is a format string with data fields
interpolated by

`%[MIN][.MAX](FIELDNAME)`

where MIN means pad with spaces to at least this width, and MAX means
truncate at this width. The field name must be enclosed in
parentheses. Three fields are available:

- `depth_spacer` - a number of spaces equal to the account's depth, or if MIN is specified, MIN * depth spaces.
- `account`      - the account's name
- `total`        - the account's balance/posted total, right justified

When the total has multiple commodities, by default each commodity is
displayed on a separate line, and the report item will be bottom
aligned.  You can change how such multi-line values are rendered by
beginning the format with a special prefix:

- `%_` - render on multiple lines, bottom-aligned (the default)
- `%^` - render on multiple lines, top-aligned
- `%,` - render on one line, with multi-line values comma-separated

There are some quirks, and experimentation may be needed to get pleasing output.
In one-line mode, `%(depth_spacer)` has no effect, instead `%(account)` has indentation built in.
<!-- XXX retest:
Consistent column widths are not well enforced, causing ragged edges unless you set suitable widths.
Beware of specifying a maximum width; it will clip account names and amounts that are too wide, with no visible indication.
-->

Examples:

- `%(total)`         - the account's total
- `%-20.20(account)` - the account's name, left justified, padded to 20 characters and clipped at 20 characters
- `%20(total)  %2(depth_spacer)%-(account)` - default format for the single-column balance report
- `%,%-50(account)  %25(total)` - account name padded to 50 characters, total padded to 20 characters, with multiple commodities rendered on one line

##### Output destination

```{.shell .bold .right}
$ hledger balance -o -     # write to stdout (the default)
$ hledger balance -o FILE  # write to FILE
```

The balance, print, register and stats commands can write their output to a
destination other than the console. This is controlled by the
`-o/--output-file` option.

##### CSV output

```{.shell .bold .right}
$ hledger balance -O csv       # write CSV to stdout
$ hledger balance -o FILE.csv  # write CSV to FILE.csv
```

The balance, print and register commands can write their output as
CSV. This is useful for exporting data to other applications, eg to
make charts in a spreadsheet. This is controlled by the
`-O/--output-format` option, or by specifying a `.csv` file extension
with `-o/--output-file`.

#### balancesheet

```{.shell .right}
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

This command displays a simple
[balance sheet](http://en.wikipedia.org/wiki/Balance_sheet). It currently
assumes that you have top-level accounts named `asset` and `liability`
(plural forms also allowed.)

#### cashflow

```{.shell .right}
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

This command displays a simple
[cashflow statement](http://en.wikipedia.org/wiki/Cash_flow_statement)
It shows the change in all "cash" (ie, liquid assets) accounts for the
period. It currently assumes that cash accounts are under a top-level
account named `asset` and do not contain `receivable` or `A/R` (plural
forms also allowed.)

#### incomestatement

```{.shell .right}
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

This command displays a simple
[income statement](http://en.wikipedia.org/wiki/Income_statement).  It
currently assumes that you have top-level accounts named `income` (or
`revenue`) and `expense` (plural forms also allowed.)

#### print

```{.shell .right}
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

#### register

```{.shell .right}
$ hledger register checking
2008/01/01 income               assets:bank:checking            $1            $1
2008/06/01 gift                 assets:bank:checking            $1            $2
2008/06/02 save                 assets:bank:checking           $-1            $1
2008/12/31 pay off              assets:bank:checking           $-1             0
```

The register command displays postings, one per line, and their
running total.  This is typically used with a [query](#queries)
selecting a particular account, to see that account's activity.

```{.shell .right .clear}
$ hledger register checking -b 2008/6 --historical
2008/06/01 gift                 assets:bank:checking            $1            $2
2008/06/02 save                 assets:bank:checking           $-1            $1
2008/12/31 pay off              assets:bank:checking           $-1             0
```

The `--historical`/`-H` flag adds the balance from any prior postings
to the running total, to show the actual historical running balance.
This is useful when you want to see just the recent activity.

The `--depth` option limits the amount of sub-account detail displayed.

The `--average`/`-A` flag shows the running average posting amount
instead of the running total (so, the final number displayed is the
average for the whole report period). This flag implies `--empty` (see below).
It works best when showing just one account and one commodity.

The `--related`/`-r` flag shows the *other* postings in the transactions
of the postings which would normally be shown.

```{.shell .right}
$ hledger register --monthly income
2008/01                 income:salary                          $-1           $-1
2008/06                 income:gifts                           $-1           $-2
```
```{.shell .right}
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
```{.shell .right .clear}
$ hledger register --monthly assets --depth 1  # cashflow (changes to assets) by month
2008/01                 assets                                  $1            $1
2008/06                 assets                                 $-1             0
2008/12                 assets                                 $-1           $-1
```

With a [reporting interval](#reporting-interval), register shows
summary postings, one per interval, aggregating the postings to each account.

Periods with no activity, and summary postings with a zero amount, are
not shown by default; use the `--empty`/`-E` flag to see them.

Often, you'll want to see just one line per interval.
The `--depth` option helps with this, causing subaccounts to be aggregated.

Note when using report intervals, if you specify start/end dates these
will be adjusted outward if necessary to contain a whole number of
intervals. This ensures that the first and last intervals are full
length and comparable to the others in the report.

##### Custom register output

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
```{.shell .bold}
$ hledger reg                 # use terminal width (or 80 on windows)
$ hledger reg -w 100          # use width 100
$ COLUMNS=100 hledger reg     # set with one-time environment variable
$ export COLUMNS=100; hledger reg  # set till session end (or window resize)
$ hledger reg -w 100,40       # set overall width 100, description width 40
$ hledger reg -w $COLUMNS,40  # use terminal width, and set description width
```

The register command also supports the
`-o/--output-file` and `-O/--output-format` options for controlling
[output destination](#output-destination) and [CSV output](#csv-output).

#### stats

```{.shell .right}
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

#### test

```{.shell .right}
$ hledger test
Cases: 74  Tried: 74  Errors: 0  Failures: 0
```

This command runs hledger's built-in unit tests and displays a quick report.
With a regular expression argument, it selects only tests with matching names.
It's mainly used in development, but it's also nice to be able to
check your hledger executable for smoke at any time.

### Add-on commands

Add-on commands are executables in your PATH whose name starts with
`hledger-` and ends with no file extension or one of these common
executable extensions:
`.hs`,`.lhs`,`.pl`,`.py`,`.rb`,`.rkt`,`.sh`,`.bat`,`.com`,`.exe`.
(Also, add-on names may not be the same as any built-in command or alias).

hledger will detect these and act as a convenient front end, displaying them in
the command list and letting you invoke them with `hledger ADDON`.
There are some tricks when specifying options:

- Options appearing before ADDON will be visible only to hledger and not be passed to the add-on.
  Eg: `hledger --help web` shows hledger's help, `hledger web --help` shows hledger-web's help.
- Options understood only by the add-on must go after a `--` argument so that hledger does not reject them.
  Eg: `hledger web -- --server`.

Add-ons which are written in haskell can take advantage of hledger's library API
for journal parsing, reports, consistent command-line options etc.
One notable add-on is [hledger-web](#web), which is maintained along with
hledger and supported on the same major platforms. Other add-ons may
have different release schedules and platform support.

#### autosync

```{.shell .right}
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

#### diff

```{.shell .right}
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

#### interest

```{.shell .right}
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

```{.shell .right .clear}
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

```{.shell .right .clear}
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

#### irr

```{.shell .right}
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

```{.shell .right .clear}
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

```{.shell .right .clear}
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

#### ui

[hledger-ui](http://hackage.haskell.org/package/hledger-ui) is hledger's curses-style interface.
It provides a simple full-screen console interface for viewing account
balances and transactions. It is simpler and more efficient for
browsing than the hledger CLI, but lighter and faster than
hledger-web.

```{.shell .noclear}
$ hledger ui -- --help
hledger-ui [OPTIONS] [PATTERNS]
  browse accounts, postings and entries in a full-window curses interface

Flags:
     --theme=THEME         use this custom display theme (default, terminal,
                           greenterm)
     --register=ACCTREGEX  start in the (first) matched account's register
     --flat                show full account names, unindented
     --no-elide            don't compress empty parent accounts on one line
  -V --value               show amounts as their market value in their
                           default valuation commodity (accounts screen)
...
```

##### Keys

Generally the cursor keys navigate; `right` (or `enter`) goes deeper, `left` returns to the previous screen,
`up`/`down`/`page up`/`page down`/`home`/`end` move up and down through lists.

`g` gets the latest data and reloads the screen (and any previous screens). There may be a noticeable pause.

`q` quits the application.

Some screens have additional key bindings, described below.

##### Accounts screen

This is normally the first screen displayed.
It lists accounts and their balances, like hledger's balance command.
By default, it shows all accounts and their latest ending balances.
if you specify a query on the command line, it shows just the matched accounts and the balances from matched transactions.

When not in flat mode, indentation indicates the account hierarchy. `F` toggles flat mode on and off.

By default, all subaccounts are displayed.
To see less detail, set a depth limit by pressing a number key, `1` to `9`.
Or, adjust the depth limit by pressing `-` or `+` (`=` also works).
`0` removes the depth limit.

`C` toggles cleared mode. In cleared mode, the accounts and balances
are derived only from transactions which are marked cleared (*).

Press `right` or `enter` to view an account's transactions register.

##### Register screen

This screen lists all transactions affecting a particular account (like a check register).
In cleared mode (press `C`) it lists only transactions which are marked cleared.
It does not otherwise filter by query.

Note this screen shows transactions, not postings (unlike hledger's
register command). This means:

- Each line represents a whole transaction.

- For each transaction, it shows the other account(s) involved, in
  abbreviated form. (If there are both real and virtual postings, it
  shows only the accounts affected by real postings.)

- It shows the overall change to the current account's balance from
  each transaction; positive for an inflow to this account, negative
  for an outflow.

- When no query other than a date limit is in effect, it shows the
  current account's historic balance as of the transaction date.
  Otherwise it shows a running total starting from zero.  Eg, these
  will show historic balances:

    ```
    $ hledger-ui
    $ hledger-ui --begin 'this month'
    $ hledger-ui --register checking date:2015/10
    ```

    while these will show a running total, since the queries are not just date limits:

    ```
    $ hledger-ui checking
    $ hledger-ui --begin 'this month' desc:market
    $ hledger-ui --register checking --cleared
    ```

Press `right` or `enter` to view the selected transaction in full detail.

##### Transaction screen

This screen shows a single transaction, as a general journal entry,
similar to hledger's print command and journal format (hledger_journal(5)).

The transaction's date(s) and any cleared flag, transaction code,
description, comments, along with all of its account postings are
shown.  Simple transactions have two postings, but there can be more
(or in certain cases, fewer).

`up` and `down` will step through all transactions listed in the
previous account register screen.  In the title bar, the numbers in
parentheses show your position within that account register. They will
vary depending on which account register you came from (remember most
transactions appear in multiple account registers). The #N number
preceding them is the transaction's position within the complete
unfiltered journal, which is a more stable id (at least until the next
reload).

##### Error screen

This screen will appear if there is a problem, such as a parse error,
when you press g to reload. Once you have fixed the problem described,
press g again to reload and restore normal operation.

#### web

<style>
.highslide img {max-width:250px; float:right; margin:0 0 1em 1em;}
.highslide-caption {color:white; background-color:black;}
</style>
<a href="images/hledger-web/normal/register.png" class="highslide" onclick="return hs.expand(this)"><img src="images/hledger-web/normal/register.png" title="Account register view with accounts sidebar" /></a>
<a href="images/hledger-web/normal/journal.png" class="highslide" onclick="return hs.expand(this)"><img src="images/hledger-web/normal/journal.png" title="Journal view" /></a>

[hledger-web](http://hackage.haskell.org/package/hledger-web) provides
a web-based user interface for hledger. You can add new journal
entries with basic auto-completion, and easily browse your accounts,
with a more useful account register view and historical balance
charts.  You can see a live demo (with junk data) at
[demo.hledger.org](http://demo.hledger.org).

```{.shell .noclear}
$ hledger web -- --help
hledger-web [OPTIONS] [PATTERNS]
  start serving the hledger web interface

Flags:
     --server             log requests, and don't browse or auto-exit
     --port=PORT          set the tcp port (default: 5000)
     --base-url=BASEURL   set the base url (default: http://localhost:PORT)
     --file-url=FILEURL   set the static files url (default: BASEURL/static)
...
$ hledger web
Starting web app on port 5000 with base url http://localhost:5000
Starting web browser if possible
Web app will auto-exit after a few minutes with no browsers (or press ctrl-c)
```

By default, `hledger web` starts the web app, displays it in your
default web browser if possible, keeps it running for as long as you
have it open in a browser window, and then exits.

With `--server`, it starts the web app in non-transient mode and logs
requests to the console.  Typically when running hledger web as part
of a website you'll want to use `--base-url` to set the
protocol/hostname/port/path to be used in hyperlinks.  The
`--file-url` option allows static files to be served from a different
url, eg for better caching or cookie-less serving.

<a href="images/hledger-web/normal/help.png" class="highslide" onclick="return hs.expand(this)"><img src="images/hledger-web/normal/help.png" title="Help dialog" /></a>
<a href="images/hledger-web/normal/add.png" class="highslide" onclick="return hs.expand(this)"><img src="images/hledger-web/normal/add.png" title="Add form" /></a>

You can use `--port` to listen on a different TCP port, eg if you are
running multiple hledger-web instances.  This need not be the same as
the PORT in the base url.

Note there is no built-in access control, so you will need to hide
hledger-web behind an authenticating proxy (such as apache or nginx)
if you want to restrict who can see and add entries to your journal.

With [journal](#journal) and [timelog](#timelog) files (but not [CSV](#csv) files, currently)
the web app detects changes and will show the new data on the next request.
If a change makes the file unparseable, hledger-web will show an error
until the file has been fixed.

<!-- edit form -->
<!-- Note: unlike any other hledger command, `web` can alter existing journal -->
<!-- data, via the edit form.  A numbered backup of the file is saved on -->
<!-- each edit, normally (ie if file permissions allow, disk is not full, etc.) -->
<!-- Also, there is no built-in access control. So unless you run it behind an -->
<!-- authenticating proxy, any visitor to your server will be able to see and -->
<!-- overwrite the journal file (and included files.) -->
<!-- hledger-web disallows edits which would leave the journal file not in -->
<!-- valid [journal format](#journal). If the file becomes unparseable -->
<!-- by other means, hledger-web will show an error until the file has been -->
<!-- fixed. -->

### Experimental commands

The following add-ons are examples and experiments provided in the
[extra](https://github.com/simonmichael/hledger/tree/master/extra)
directory in the hledger source.  Add this directory to your PATH to
make them available. The scripts are designed to run interpreted on
unix systems (for tweaking), or you can compile them (for speed and
robustness).

#### equity

```{.shell .right}
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

#### print-unique

```{.shell .right}
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

Print only only journal entries which have a unique description.

#### rewrite

```{.shell .right .bold}
$ hledger rewrite -- [QUERY]        --add-posting "ACCT  AMTEXPR" ...
$ hledger rewrite -- ^income        --add-posting '(liabilities:tax)  *.33'
$ hledger rewrite -- expenses:gifts --add-posting '(budget:gifts)  *-1"'
```

Prints all journal entries, adding specified custom postings to matched entries.


<!--

### Unmaintained commands

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

-->



## Appendices


### Regular Expressions

hledger uses [regular expressions](http://www.regular-expressions.info) in a number of places:

- [query terms](#queries), on the command line and in the hledger-web search form: `REGEX`, `desc:REGEX`, `cur:REGEX`, `tag:...=REGEX`
- [CSV rules](#csv-rules) conditional blocks: `if REGEX ...`
- [account alias](#account-aliases) directives and options: `alias /REGEX/ = REPLACEMENT`, `--alias /REGEX/=REPLACEMENT`

hledger's regular expressions come from the
[regex-tdfa](http://hackage.haskell.org/package/regex-tdfa/docs/Text-Regex-TDFA.html)
library. In general they:

- are case insensitive
- are infix matching (do not need to match the entire thing being matched)
- are [POSIX extended regular expressions](http://www.regular-expressions.info/posix.html#ere)
- also support [GNU word boundaries](http://www.regular-expressions.info/wordboundaries.html) (\\<, \\>, \\b, \\B)
- and parenthesised [capturing groups](http://www.regular-expressions.info/refcapture.html) and numeric backreferences in replacement strings
- do not support [mode modifiers](http://www.regular-expressions.info/modifiers.html) like (?s)

Some things to note:

- In the `alias` directive and `--alias` option, regular expressions
must be enclosed in forward slashes (`/REGEX/`). Elsewhere in hledger,
these are not required.

- To match a regular expression metacharacter like `$` as a literal
character, prepend a backslash. Eg to search for amounts with the
dollar sign in hledger-web, write `cur:\$`.

- On the command line, some metacharacters like `$` have a special
meaning to the shell and so must be escaped a second time, with single
or double quotes or another backslash.  Eg, to match amounts with the
dollar sign from the command line, write `cur:'\$'` or `cur:\\$`.



### Editor support

Add-on modes exist for various text editors, to make working with journal
files easier. They add colour, navigation aids and helpful commands.
For hledger users who edit the journal file directly (the majority),
using one of these modes is quite recommended.

These were written with Ledger in mind, but also work with hledger files:

|
|----------------|----------------------------------------------------|
| Emacs          | <http://www.ledger-cli.org/3.0/doc/ledger-mode.html> |
| Vim            | <https://github.com/ledger/ledger/wiki/Getting-started-with-Vim> |
| Sublime Text   | <https://github.com/ledger/ledger/wiki/Using-Sublime-Text> |
| Textmate       | <https://github.com/ledger/ledger/wiki/Using-TextMate-2> |
| Text Wrangler &nbsp; | <https://github.com/ledger/ledger/wiki/Editing-Ledger-files-with-TextWrangler> |

<!-- Some related LedgerTips:
https://twitter.com/LedgerTips/status/504061626233159681
https://twitter.com/LedgerTips/status/502820400276193280
https://twitter.com/LedgerTips/status/502503912084361216
https://twitter.com/LedgerTips/status/501767602067472384
-->


### Troubleshooting

#### Run-time problems

Here are some issues you might encounter when you run hledger
(and remember you can also seek help from the
[IRC channel](http://irc.hledger.org),
[mail list](http://list.hledger.org) or
[bug tracker](http://bugs.hledger.org)):

**Successfully installed, but "No command 'hledger' found"**  
stack and cabal install binaries into a special directory, which
should be added to your PATH environment variable.  On unix-like
systems, it is ~/.local/bin and ~/.cabal/bin respectively.

**"Illegal byte sequence" or "Invalid or incomplete multibyte or wide character" errors**  
In order to handle non-ascii letters and symbols (like £), hledger needs
an appropriate locale. This is usually configured system-wide; you can
also configure it temporarily.  The locale may need to be one that
supports UTF-8, if you built hledger with GHC < 7.2 (or possibly always,
I'm not sure yet).

Here's an example of setting the locale temporarily, on ubuntu gnu/linux:

```{.shell}
$ file my.journal
my.journal: UTF-8 Unicode text                 # <- the file is UTF8-encoded
$ locale -a
C
en_US.utf8                             # <- a UTF8-aware locale is available
POSIX
$ LANG=en_US.utf8 hledger -f my.journal print   # <- use it for this command
```

Here's one way to set it permanently, there are probably better ways:

```{.shell}
$ echo "export LANG=en_US.UTF-8" >>~/.bash_profile
$ bash --login
```

If we preferred to use eg `fr_FR.utf8`, we might have to install that first:

```{.shell}
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
```

Note some platforms allow variant locale spellings, but not all (ubuntu
accepts `fr_FR.UTF8`, mac osx requires exactly `fr_FR.UTF-8`).


#### Known limitations

**Command line interface**

Add-on command options, unless they are also understood by the main
hledger executable, must be written after `--`, like this:
`hledger web -- --server`

**Differences from Ledger**

Not all of Ledger's journal file syntax is supported. See [file format differences](faq#file-format-differences).

hledger is slower than Ledger, and uses more memory, on large data files.

**Windows limitations**

In a windows CMD window, non-ascii characters and colours are not supported.

In a windows Cygwin/MSYS/Mintty window, the tab key is not supported in hledger add.



