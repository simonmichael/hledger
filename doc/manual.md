<!-- hledger.org and hledger repo versions last synced: 2014/5/1 -->

* toc

# hledger User Manual

This reference manual is for
 hledger 0.23.98 (the latest pre-0.24 HEAD).
 <!-- [hledger 0.23](http://hackage.haskell.org/package/hledger-0.23) -->
 and [hledger-web 0.23](http://hackage.haskell.org/package/hledger-web-0.23).
 <!-- Other versions: [0.22 Manual](0.22/manual). -->

## Introduction

[hledger](/) is a program for tracking money, time,
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

hledger works on unix, mac and windows.
See [Download](download.html) for installation help.

## Usage

Basic usage is:

    $ hledger COMMAND [OPTIONS] [ARGS]

Most [commands](#commands) query or operate on a
[journal file](#journal), which by default is `.hledger.journal`
in your home directory. You can specify a different file with the `-f`
option or `LEDGER_FILE` environment variable, or standard input with `-f-`.

Options are similar across most commands, with some variations; use
`hledger COMMAND --help` for details. Most options must appear
somewhere after COMMAND, not before it. These input and help-related
options can appear anywhere: `-f`, `--rules-file`, `--alias`,
`--ignore-assertions`, `--help`, `--debug`, `--version`.

Arguments are also command-specific, but usually they form a
[query](#queries) which selects a subset of the journal, eg transactions
in a certain account.

To create an initial journal, run `hledger add` and follow the prompts to
enter some transactions.  Or, save this
[sample file](https://raw.github.com/simonmichael/hledger/master/data/sample.journal) as
`.hledger.journal` in your home directory. Now try commands like these:

    $ hledger
        # show available commands
    $ hledger add
        # add more transactions to the journal file
    $ hledger balance
        # all accounts with aggregated balances
    $ hledger balance --help
        # show help for balance command
    $ hledger balance --depth 1
        # only top-level accounts
    $ hledger register
        # show account postings, with running total
    $ hledger reg income
        # show postings to/from income accounts
    $ hledger reg 'assets:some bank:checking'
        # show postings to/from this checking account
    $ hledger print desc:shop
        # show transactions with shop in the description
    $ hledger activity -W
        # show transaction counts per week as a bar chart

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

#### Entries

Each journal entry begins with a [simple date](#simple-dates) in
column 0, followed by three optional fields with spaces between them:
a status flag (`*` or `!` or nothing), a transaction code (eg a check
number), and/or a description; then two or more postings (of some
amount to some account), each on their own line.

The posting amounts within a transaction must always balance, ie add up to
0.  You can leave one amount blank and it will be inferred.

#### Dates

##### Simple dates

Within a journal file, transaction dates always follow a year/month/day
format, although several different separator characters are accepted. Some
examples: `2010/01/31`, `2010/1/31`, `2010-1-31`, `2010.1.31`.

Writing the year is optional if you set a default year with a Y directive.
This is a line containing `Y` and the year; it affects subsequent
transactions, like so:

    Y2009
    
    12/15  ; equivalent to 2009/12/15
      ...
    
    Y2010
    
    1/31  ; equivalent to 2010/1/31
      ...

##### Secondary dates

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

##### Posting dates

[Comments and tags](#comments) are covered below, but while we are talking
about dates: you can give individual postings a different date from their
parent transaction, by adding a posting tag like `date:DATE`, where DATE is
a [simple date](#simple-dates). The secondary date can be set with
`date2:DATE2`. If present, these dates will take precedence in reports.

Ledger's bracketed posting date syntax (`[DATE]`,
`[DATE=DATE2]` or `[=DATE2]` in a posting comment)
is also supported, as an alternate spelling of the date and date2 tags.

Note: if you do use either of these forms, be sure to give them a valid DATE
or you'll get a parse error, eg an empty `date:` tag is not allowed.

#### Accounts

Account names typically have several parts separated by a full colon, from
which hledger derives a hierarchical chart of accounts. They can be
anything you like, but in finance there are traditionally five top-level
accounts: `assets`, `liabilities`, `income`, `expenses`, and `equity`.

Account names may contain single spaces, eg: `assets:accounts receivable`.

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


##### Balance Assertions

hledger supports ledger-style
[balance assertions](http://ledger-cli.org/3.0/doc/ledger3.html#Balance-assertions)
in journal files.
These look like `=EXPECTEDBALANCE` following a posting's amount. Eg in
this example we assert the expected dollar balance in accounts a and b after
each posting:

    2013/1/1
      a   $1  =$1
      b       =$-1

    2013/1/2
      a   $1  =$2
      b  $-1  =$-2

After reading a journal file, hledger will check all balance
assertions and report an error if any of them fail. Balance assertions
can protect you from, eg, inadvertently disrupting reconciled balances
while cleaning up old entries. You can disable them temporarily with
the `--ignore-assertions` flag, which can be useful for
troubleshooting or for reading Ledger files.

###### Assertions and ordering

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


###### Assertions and commodities

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


##### Prices

<!-- ##### Transaction prices -->

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
        assets:cash                  $-135.00

###### Prices are fixed

In hledger, the price used in a given posting is fixed.
This is what you want for eg recording purchases made while travelling abroad,
but you can't (yet) track the value of stocks whose price fluctuates.

This is different from Ledger, where prices fluctuate by default.
Ledger has a different syntax for specifying
[fixed prices](http://ledger-cli.org/3.0/doc/ledger3.html#Fixing-Lot-Prices): `{=PRICE}`.
hledger parses that syntax, and (currently) ignores it.
<!-- hledger treats this as an alternate spelling of `@ PRICE`, for greater compatibility with Ledger files. -->

###### Historical prices

hledger also parses, and currently ignores, ledger-style historical price directives:
<!-- (A time and numeric time zone are allowed but ignored, like ledger.) -->
```
; Historical price directives look like: P DATE COMMODITYSYMBOL UNITPRICE
; These say the euro's exchange rate is $1.35 during 2009 and
; $1.40 from 2010/1/1 on.
P 2009/1/1 € $1.35
P 2010/1/1 € $1.40
```

#### Comments

- A semicolon (`;`) or hash (`#`) in column 0 starts a journal comment line, which hledger will ignore.

- A semicolon after a transaction's description and/or indented on the following lines
starts a transaction comment.

- A semicolon after a posting's amount and/or indented on the following lines starts a posting comment.

- With the `comment` and `end comment` keywords it is possible to have multiline comments.

Transaction and posting comments are displayed by [print](#print), can contain [tags](#tags) and can be [queried](#queries).

Some examples:

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


#### Tags

You can include *tags* (labels), optionally with values,
in transaction and posting comments, and then [query by tag](manual#queries).
This is like Ledger's [metadata](http://ledger-cli.org/3.0/doc/ledger3.html#Metadata)
feature, except hledger's tag values are simple strings.

A tag is any unspaced word immediately followed by a full colon, eg: `sometag:` .
A tag's *value* is the characters following the colon, if any, until the next comma or newline,
with any leading and trailing whitespace removed. 
Comma may be used to write multiple tags on one line.

For example, here is a transaction with three tags, the posting has
one, and all tags have values except TAG1:

    1/1 a transaction    ; TAG1:, TAG2: tag2's value
        ; TAG3: a third transaction tag
        a  $1  ; TAG4: a posting tag

**Things to note:**

In the journal file, a hledger tag value can contain: text, internal whitespace, or punctuation, but not: commas, newlines, or leading/trailing whitespace (putting quotes around it doesn't work, but probably should).

In [tag queries](manual#queries), remember the tag name must match exactly, while the value part is the usual case-insensitive infix regular expression match.
#### Directives

##### Account aliases

You can define account aliases to rewrite certain account names (and their subaccounts).
This tends to be a little more reliable than post-processing with sed or similar.
The directive is `alias ORIG = ALIAS`, where ORIG and ALIAS are full account names.
Eg:

    alias expenses = equity:draw:personal

To forget all aliases defined to this point, use:

    end aliases

You can also specify aliases on the command line:

    $ hledger --alias 'my earning=income:business' ...

Journal directive aliases are applied first, then command-line aliases,
and at most one of each will be applied to each account name.

See also [How to use account aliases](how-to-use-account-aliases.html).

##### Default commodity

You can set a default commodity, to be used for amounts without one.
Use the D directive with a sample amount.
The commodity (and the sample amount's display style) will be applied to all subsequent commodity-less amounts, up to the next D directive.
(Note this is different from Ledger's default commodity directive.)

Also note the directive itself does not influence the commodity's default
[display style](#amount-display-styles), but the amount it is
applied to might. Here's an example:

    ; set £ as the default commodity
    D £1,000.00
    
    2010/1/1
      a  2340
      b
    
    2014/1/1
      c  £1000
      d
    

    $ hledger print
    2010/01/01
        a     £2,340.00
        b    £-2,340.00

    2014/01/01
        c     £1,000.00
        d    £-1,000.00

##### Default parent account

You can specify a parent account which will be prepended to all accounts
within a section of the journal. Use the `account` directive like so:

    account home
    
    2010/1/1
        food    $10
        cash
    
    end

If `!end` is omitted, the effect lasts to the end of the file.
The above is equivalent to:

    2010/01/01
        home:food           $10
        home:cash          $-10

Included files are also affected, eg:

    account business
    include biz.journal
    end
    account personal
    include personal.journal
    end

##### Including other files

You can pull in the content of additional journal files, by writing lines like this:

    include path/to/file.journal

The `include` directive may only be used in journal files, and currently
it may only include other journal files (eg, not CSV or timelog files.)


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
```
# ignore the first CSV line
skip 1
```

**`date-format` *DATEFMT***\
When your CSV date fields are not formatted like `YYYY/MM/DD` (or `YYYY-MM-DD` or `YYYY.MM.DD`),
you'll need to specify the format.
DATEFMT is a [strptime-style date parsing pattern](http://hackage.haskell.org/packages/archive/time/latest/doc/html/Data-Time-Format.html#v:formatTime),
which must parse the date field values completely. Examples:
```
# parses "6/11/2013":
date-format %-d/%-m/%Y
```
```
# parses "11/06/2013":
date-format %m/%d/%Y
```
```
# parses "2013-Nov-06":
date-format %Y-%h-%d
```
```
# parses "11/6/2013 11:32 PM":
date-format %-m/%-d/%Y %l:%M %p
```

**`fields` *CSVFIELDNAME1*, *CSVFIELDNAME2*...**\
(Field list)\
This (a) names the CSV fields (names may not contain whitespace),
and (b) assigns them to journal entry fields if you use any of these standard field names:\
\
`date`, `date2`, `status`, `code`, `description`, `comment`, `account1`, `account2`, `amount`, `amount-in`, `amount-out`, `currency`.\
\
Eg:
```
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
```
# set the amount to the 4th CSV field with "USD " prepended
amount USD %4
```
```
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
```
# if the CSV record contains "groceries", set account2 to "expenses:groceries"
if groceries
 account2 expenses:groceries
```
```
# if the CSV record contains any of these patterns, set account2 and comment as shown
if
monthly service fee
atm transaction fee
banking thru software
 account2 expenses:business:banking
 comment  XXX deductible ? check
```

**`include` *RULESFILE***\
Include another rules file at this point. Eg:
```
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

### Timelog

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
[sample.timelog](https://raw.github.com/simonmichael/hledger/master/data/sample.timelog) to
download and some queries to try:

    hledger -f sample.timelog balance                               # current time balances
    hledger -f sample.timelog register -p 2009/3                    # sessions in march 2009
    hledger -f sample.timelog register -p weekly --depth 1 --empty  # time summary by week

To generate time logs, ie to clock in and clock out, you could:

- use emacs and the built-in timeclock.el, or
  the extended [timeclock-x.el](http://www.emacswiki.org/emacs/timeclock-x.el)
  and perhaps the extras in [ledgerutils.el](http://hub.darcs.net/simon/ledgertools/ledgerutils.el)

- at the command line, use these bash aliases:
  ```
  alias ti="echo i `date '+%Y-%m-%d %H:%M:%S'` \$* >>$TIMELOG"
  alias to="echo o `date '+%Y-%m-%d %H:%M:%S'` >>$TIMELOG"
  ```
- or use the old `ti` and `to` scripts in the [ledger 2.x repository](https://github.com/ledger/ledger/tree/maint/scripts).
  These rely on a "timeclock" executable which I think is just the ledger 2 executable renamed.


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
  -U --uncleared          include only uncleared postings/txns
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

## Queries

One of hledger's strengths is being able to quickly report on precise subsets of your data.\
Most commands accept an optional query expression, written as arguments after the command name,
to filter the data by date, account name or other criteria. Query expressions are also used
in the [web ui](#web)'s search form.

The query syntax is similar to a Google search expression: one or
more space-separated search terms, optional prefixes to match specific
fields, quotes to enclose whitespace, etc.
A query term can be any of the following:

- `REGEX` - match account names by this regular expression
- `acct:REGEX` - same as above
- `code:REGEX` - match by transaction code (eg check number)
- `desc:REGEX` - match transaction descriptions
- `date:PERIODEXPR` - match dates within the specified [period](#period-expressions). *Actually, full period syntax is [not yet supported](https://github.com/simonmichael/hledger/issues/141).*
- `date2:PERIODEXPR` - as above, but match secondary dates
- `tag:NAME[=REGEX]` - match by (exact, case sensitive) [tag](#tags) name, and optionally match the tag value by regular expression. Note `tag:` will match a transaction if it or any its postings have the tag, and will match posting if it or its parent transaction has the tag.
- `depth:N` - match (or display, depending on command) accounts at or above this [depth](#depth-limiting)
- `status:1` or `status:0` - match cleared/uncleared transactions
- `real:1` or `real:0` - match real/virtual-ness
- `empty:1` or `empty:0` - match if amount is/is not zero
- `amt:N`, `amt:<N`, `amt:<=N`, `amt:>N`, `amt:>=N` - match postings with a single-commodity
  amount that is equal to, less than, or greater than N.
  (Multi-commodity amounts are not tested, and will always match.)
  The comparison has two modes: if N is preceded by a `+` or `-` sign
  (or is 0), the two signed numbers are compared. Otherwise, the
  absolute magnitudes are compared, ignoring sign.
- `cur:REGEX` - match postings or transactions including any amounts
  whose currency/commodity symbol is fully matched by REGEX. (For a
  partial match, use `.*REGEX.*`). Note, to match characters which are
  regex-significant, like the dollar sign (`$`), you need to prepend `\`.
  And when using the command line you need to add one more level
  of quoting to hide it from the shell, so eg do: `hledger print cur:'\$'`
  or `hledger print cur:\\$`.
- `not:` before any of the above negates the match

### Combining query arguments

hledger query expressions don't support full boolean logic. Instead, multiple query terms
are combined as follows:

- The [print](#print) command selects transactions which:
  - match any of the description terms AND
  - have any postings matching any of the positive account terms AND
  - have no postings matching any of the negative account terms AND
  - match all the other terms.

<!-- -->

- Other reporting commands (eg [register](#register) and [balance](#balance)) select transactions/postings/accounts which match (or negatively match):
  - any of the description terms AND
  - any of the account terms AND
  - all the other terms.

### Query arguments or options ?

On the command line, some of the query terms above can also be expressed as command-line flags. 
Generally you can mix and match query arguments and flags, and the resulting query will be their intersection.
Remember that a `-p` [period](#period-expressions) flag will cause any other `-b`, `-e` or `-p` flags on the command line to be ignored.


## Commands

hledger provides a number of subcommands out of the box; run `hledger` with no arguments to see a list.
The most-used commands are probably
[balance](#balance),
[register](#register),
[print](#print)
and [accounts](#accounts).

More [add-on commands](#add-ons) will appear if you install additional `hledger-*` packages,
or if you put programs or scripts named `hledger-NAME` in your PATH.

To choose a command, write it as the first command-line argument.
You can write its full name (eg `balance`), or one of the
standard short aliases displayed in parentheses in the command list
(eg `bs`), or any unambiguous prefix of a command (eg `inc`).

### Data entry

Many hledger users edit their journals directly with a text editor, or generate them from CSV.
For more interactive data entry, there is the `add` command and also the `web` add-on (below).

#### add

The add command prompts interactively for new transactions, and appends
them to the journal file. Just run `hledger add` and follow the prompts.
You can add as many transactions as you like; when you are finished,
enter `.` or press control-d or control-c to exit.

Additional convenience features:

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

### Reports

Here are the built-in commands for reporting useful information from your journal,
which hledger's main purpose. (The original commands inherited from Ledger were, simplest first:
[print](#print), [register](#register) and [balance](#balance).)

#### accounts

This command lists matched account names, in a flat list by default, or in hierarchy
with the `--tree` flag. With no query arguments, all account names are listed.

#### activity

The activity command displays an ascii bar chart showing
transaction counts by day, week, month or other reporting interval
(by day is the default).

Examples:

    $ hledger activity -p weekly dining

#### balance

The balance command displays accounts and their balances.

##### Simple balance reports

Simple balance reports have no [reporting interval](#reporting-interval).
They show the sum of matched postings in each account.
(If postings are not date-restricted, this is usually the same as the ending balance).

    $ hledger balance
    $ hledger balance -p 'last month' expenses:food

By default, simple balance reports display the accounts as a
hierarchy, with subaccounts indented below their parent. Each
account's balance is the "inclusive" balance - it includes the
balances of any subaccounts.

"Boring parent accounts" (containing a single interesting subaccount
and no balance of their own) are elided into the following line for
more compact output. Use `--no-elide` to prevent this.

Accounts which have zero balance (and no non-zero subaccounts) are
omitted. Use `-E/--empty` to show them.

A final total is displayed by default; use `--no-total` to suppress it.

##### Flat mode

To see a flat list of full account names instead of the hierarchy, use `--flat`.
In this mode, each account's balance is the "exclusive" balance - it excludes subaccount balances
(except when aggregating deeper accounts at the depth limit, see below).
Also, you can use `--drop N` to omit the first few account name components.

##### Depth limiting

With `--depth N`, balance shows accounts only to the specified depth.
In flat mode, it also aggregates and summarises deeper accounts at the depth limit.
This is very useful to summarise complex charts of accounts.

<!-- $ for y in 2006 2007 2008 2009 2010; do echo; echo $y; hledger -f $y.journal balance ^expenses --depth 2; done -->

##### Multi balance reports

With a [reporting interval](#reporting-interval), multiple balance
columns will be shown, one for each report period.
There are three types of multi-column balance report, showing different information:

1. By default: each column shows the sum of postings in that period,
ie the account's change of balance in that period. This is useful eg
for a monthly income statement.
<!--
multi-column income statement: 

   $ hledger balance ^income ^expense -p 'monthly this year' --depth 3

or cashflow statement:

   $ hledger balance ^assets ^liabilities 'not:(receivable|payable)' -p 'weekly this month'
-->

2. With `--cumulative`: each column shows the ending balance for that
period, accumulating the changes across periods, starting from 0 at
the report start date. This mode is not often used.

3. With `--historical/-H`: each column shows the actual historical
ending balance for that period, accumulating the changes across
periods, starting from the actual balance at the report start date.
This is useful eg for a multi-year balance sheet.
<!--
    $ hledger balance ^assets ^liabilities -YH
-->

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

##### Custom output formats

In simple balance reports (only), the `--format FMT` option will customize
the format of output lines. `FMT` is like a C printf/strftime-style
format string, except that field names are enclosed in parentheses:

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

The default output format is `%20(total)  %2(depth_spacer)%-(account)`.


#### balancesheet

This command displays a simple
[balance sheet](http://en.wikipedia.org/wiki/Balance_sheet). It currently
assumes that you have top-level accounts named `asset` and `liability`
(plural forms also allowed.)

#### cashflow

This command displays a simplified
[cashflow statement](http://en.wikipedia.org/wiki/Cash_flow_statement)
(without the traditional segmentation into operating, investing, and
financing cash flows.) It shows the change in all "cash" accounts for the
period. It currently assumes that cash accounts are under a top-level
account named `asset` and do not contain `receivable` or `A/R` (plural
forms also allowed.)

#### incomestatement

This command displays a simple
[income statement](http://en.wikipedia.org/wiki/Income_statement).  It
currently assumes that you have top-level accounts named `income` (or
`revenue`) and `expense` (plural forms also allowed.)

#### print

The print command displays full transactions from the journal file,
tidily formatted and showing all amounts explicitly. The output of
print is always a valid hledger journal, but it does always not
preserve all original content exactly (eg directives).

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

The `--historical`/`-H` flag adds the balance from any prior postings
to the running total, to show the actual running account balance.

The `--depth` option limits the amount of sub-account detail displayed:

    $ hledger register assets:bank:checking --depth 2

The `--average`/`-A` flag shows the running average posting amount
instead of the running total (so, the final number displayed is the
average for the whole report period). This flag implies `--empty` (see below).
It works best when showing just one account and one commodity.

The `--related`/`-r` flag shows the *other* postings in the transactions
of the postings which would normally be shown.

The `--width`/`-w` option adjusts the width of the output. By default,
this is 80 characters. To allow more space for descriptions and account
names, use `-w N` to increase the width to N characters (the argument is required).

With a [reporting interval](#reporting-interval) register shows
aggregated summary postings, within each interval:

    $ hledger register --monthly rent
    $ hledger register --monthly -E food --depth 4

One summary posting will be shown for each account in each interval.
Summary postings with a zero amount are not shown; use the `--empty`/`-E` flag to show them.

If necessary, use the `--depth` option to summarise the accounts.
It's often most useful to see just one line per interval.

When using report intervals, the report's normal start/end dates are
"enlarged" to contain a whole number of intervals, so that the first
and last intervals will be "full" and comparable to the others.

#### stats

The stats command displays summary information for the whole journal, or
a matched part of it.

Examples:

    $ hledger stats
    $ hledger stats -p 'monthly in 2009'

#### test

This command runs hledger's built-in unit tests and displays a quick
report. A pattern can be provided to filter tests by name. It's mainly
used in development, but it's also nice to be able to check hledger for
smoke at any time.

Examples:

    $ hledger test
    $ hledger test -v balance

### Add-ons

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

[ledger-autosync](https://bitbucket.org/egh/ledger-autosync/commits/all),
which includes a `hledger-autosync` alias, downloads transactions
from your bank(s) via OFX, and prints just the new ones as journal
entries which you can add to your journal. It can also operate on .OFX
files which you've downloaded manually. It can be a nice alternative
to hledger's built-in CSV reader, especially if your bank supports OFX
download.

#### interest

[hledger-interest](http://hackage.haskell.org/package/hledger-interest)
computes interests for a given account. Using command line flags,
the program can be configured to use various schemes for day-counting,
such as act/act, 30/360, 30E/360, and 30/360isda. Furthermore, it
supports a (small) number of interest schemes, i.e. annual interest
with a fixed rate and the scheme mandated by the German BGB288
(Basiszins für Verbrauchergeschäfte). See the package page for more.

#### irr

[hledger-irr](http://hackage.haskell.org/package/hledger-irr)
computes the internal rate of return, also known as the effective
interest rate, of a given investment. After specifying what account
holds the investment, and what account stores the gains (or losses, or
fees, or cost), it calculates the hypothetical annual rate of fixed
rate investment that would have provided the exact same cash flow.
See the package page for more.

#### web

[hledger-web](http://hackage.haskell.org/package/hledger-web)
provides a web-based user interface for viewing and modifying your ledger.
It includes an account register view that is more useful than the command-line register, and basic data entry and editing.
You can see it running at [demo.hledger.org](http://demo.hledger.org).

web-specific options:

    --server            log requests, don't exit on inactivity
    --port=N            serve on tcp port N (default 5000)
    --base-url=URL      use this base url (default http://localhost:PORT/)
    --static-root=URL   use this base url for static files (default http://localhost:PORT/static)

By default, the web command starts a transient local web app and displays it in your default web browser ("local ui mode").
With `--server`, it starts the web app, leaves it running, and also logs requests to the console ("server mode").

Typically in server mode you'll also want to use
`--base-url` to set the protocol/hostname/port/path to be used in
hyperlinks.

You can use `--port` to listen on a different TCP port, eg if you are running multiple hledger-web
instances.  Note `--port`'s argument need not be the same as the PORT
in the base url.

The more advanced option `--static-root` allows the static files served from a
separate base url.  This enables the optimization that the static files can be
served from a generic web server like apache, which is good at handling static
files and caching. One can also serve the files in a separate domain to reduce
cookies overhead.

**Note:** unlike any other hledger command, `web` can alter existing journal
data, via the edit form.  A numbered backup of the file is saved on
each edit, normally (ie if file permissions allow, disk is not full, etc.)
Also, there is no built-in access control. So unless you run it behind an
authenticating proxy, any visitor to your server will be able to see and
overwrite the journal file (and included files.)

hledger-web disallows edits which would leave the journal file not in
valid [journal format](#journal). If the file becomes unparseable
by other means, hledger-web will show an error until the file has been
fixed.

Examples:

    $ hledger-web
    $ hledger-web -E -B --depth 2 -f some.journal
    $ hledger-web --server --port 5010 --base-url http://some.vhost.com --debug=1

### Experimental

The following add-ons are examples and experiments provided in the
[extra](https://github.com/simonmichael/hledger/tree/master/extra)
directory in the hledger source.  Add this directory to your PATH to
make them available. The scripts are designed to run interpreted on
unix systems (for tweaking), or you can compile them (for speed and
robustness).

#### equity

Like ledger's equity command, this prints a single journal entry with
postings matching the current balance in each account (or the
specified accounts) in the default journal. An entry like this is
useful to carry over asset and liability balances when beginning a new
journal file, eg at the start of the year.

You can also use the same entry with signs reversed to close out the
old file, resetting balances to 0. This means you'll see the correct
asset/liability balances whether you use one file or a whole sequence
of files as input to hledger.

#### print-unique

Prints only journal entries which are unique (by description).

#### rewrite

Prints all journal entries, adding specified custom postings to matched entries.


<!-- unmaintained:

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


## Editor support

Add-on modes exist for various text editors, to make working with journal
files easier. They add colour, navigation aids and helpful commands.
For hledger users who edit the journal file directly (the majority),
using one of these modes is quite recommended.

These were written with Ledger in mind, but also work with hledger files:

|----------------|----------------------------------------------------|
| Emacs          | <http://www.ledger-cli.org/3.0/doc/ledger-mode.html> |
| Vim            | <https://github.com/ledger/ledger/wiki/Getting-started-with-Vim> |
| Sublime Text   | <https://github.com/ledger/ledger/wiki/Using-Sublime-Text> |
| Textmate       | <https://github.com/ledger/ledger/wiki/Using-TextMate-2> |
| Text Wrangler &nbsp;  | <https://github.com/ledger/ledger/wiki/Editing-Ledger-files-with-TextWrangler> |

<!-- Some related LedgerTips:
https://twitter.com/LedgerTips/status/504061626233159681
https://twitter.com/LedgerTips/status/502820400276193280
https://twitter.com/LedgerTips/status/502503912084361216
https://twitter.com/LedgerTips/status/501767602067472384
-->




## Troubleshooting

### Run-time problems

Here are some issues you might encounter when you run hledger
(and remember you can also seek help from the
[IRC channel](http://irc.hledger.org),
[mail list](http://list.hledger.org) or
[bug tracker](http://bugs.hledger.org)):

#### Successfully installed, but "No command 'hledger' found"
cabal installs binaries into a special directory, which should be added
to your PATH environment variable.  On unix-like systems, it is
~/.cabal/bin.

#### "Illegal byte sequence" or "Invalid or incomplete multibyte or wide character" errors
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


### Known limitations

Here are some things to be aware of.

#### Add-on-specific options must follow --

When invoking an add-on via hledger, add-on flags which are not also
understood by the main hledger executable must have a `--` argument
preceding them. Eg hledger-web's `--server` flag must be used like so:
`hledger web -- --server`.

#### -w/--width and --debug options must be written without whitespace

Up to hledger 0.23, these optional-value flags [did not work](https://github.com/simonmichael/hledger/issues/149) with whitespace between the flag and value.
IE these worked: `--debug`, `-w`, `--debug=2`, `-w100`, but these did not: `--debug 2`, `-w 100`.
From 0.24, a value is required and the whitespace does not matter.

#### Not all of Ledger's journal file syntax is supported

See [file format differences](faq#file-format-differences).

#### balance is less speedy than Ledger's on large data files

hledger's balance command (in particular) takes more time, and uses more memory, than Ledger's.
This becomes more noticeable with large data files.

#### Windows CMD.EXE

Non-ascii characters and colours are not supported.

#### Windows cygwin/msys/mintty

The tab key is not supported in hledger add.

