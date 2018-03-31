# hledger

This doc is for version **1.5**. []{.docversions}

-   toc

## NAME

hledger - a command-line accounting tool

## SYNOPSIS

`hledger [-f FILE] COMMAND [OPTIONS] [ARGS]`\
`hledger [-f FILE] ADDONCMD -- [OPTIONS] [ARGS]`\
`hledger`

## DESCRIPTION

hledger is a cross-platform program for tracking money, time, or any
other commodity, using double-entry accounting and a simple, editable
file format. hledger is inspired by and largely compatible with
ledger(1).\
Tested on unix, mac, windows, hledger aims to be a reliable, practical
tool for daily use.

This is hledger's command-line interface (there are also curses and web
interfaces). Its basic function is to read a plain text file describing
financial transactions (in accounting terms, a general journal) and
print useful reports on standard output, or export them as CSV. hledger
can also read some other file formats such as CSV files, translating
them to journal format. Additionally, hledger lists other hledger-\*
executables found in the user's \$PATH and can invoke them as
subcommands.

hledger reads data from one or more files in hledger journal, timeclock,
timedot, or CSV format specified with `-f`, or `$LEDGER_FILE`, or
`$HOME/.hledger.journal` (on windows, perhaps
`C:/Users/USER/.hledger.journal`). If using `$LEDGER_FILE`, note this
must be a real environment variable, not a shell variable. You can
specify standard input with `-f-`.

Transactions are dated movements of money between two (or more) named
accounts, and are recorded with journal entries like this:

``` {.journal}
2015/10/16 bought food
 expenses:food          $10
 assets:cash
```

For more about this format, see hledger\_journal(5).

Most users use a text editor to edit the journal, usually with an editor
mode such as ledger-mode for added convenience. hledger's interactive
add command is another way to record new transactions. hledger never
changes existing transactions.

To get started, you can either save some entries like the above in
`~/.hledger.journal`, or run `hledger add` and follow the prompts. Then
try some commands like `hledger print` or `hledger balance`. Run
`hledger` with no arguments for a list of commands.

## EXAMPLES

Two simple transactions in hledger journal format:

``` {.journal}
2015/9/30 gift received
  assets:cash   $20
  income:gifts

2015/10/16 farmers market
  expenses:food    $10
  assets:cash
```

Some basic reports:

``` {.shell}
$ hledger print
2015/09/30 gift received
    assets:cash            $20
    income:gifts          $-20

2015/10/16 farmers market
    expenses:food           $10
    assets:cash            $-10
```

``` {.shell}
$ hledger accounts --tree
assets
  cash
expenses
  food
income
  gifts
```

``` {.shell}
$ hledger balance
                 $10  assets:cash
                 $10  expenses:food
                $-20  income:gifts
--------------------
                   0
```

``` {.shell}
$ hledger register cash
2015/09/30 gift received   assets:cash               $20           $20
2015/10/16 farmers market  assets:cash              $-10           $10
```

More commands:

``` {.shell}
$ hledger                                 # show available commands
$ hledger add                             # add more transactions to the journal file
$ hledger balance                         # all accounts with aggregated balances
$ hledger balance --help                  # show detailed help for balance command
$ hledger balance --depth 1               # only top-level accounts
$ hledger register                        # show account postings, with running total
$ hledger reg income                      # show postings to/from income accounts
$ hledger reg 'assets:some bank:checking' # show postings to/from this checking account
$ hledger print desc:shop                 # show transactions with shop in the description
$ hledger activity -W                     # show transaction counts per week as a bar chart
```

## OPTIONS

### General options

To see general usage help, including general options which are supported
by most hledger commands, run `hledger -h`.

General help options:

`-h --help`
:   show general usage (or after COMMAND, command usage)

`--version`
:   show version

`--debug[=N]`
:   show debug output (levels 1-9, default: 1)

General input options:

`-f FILE --file=FILE`
:   use a different input file. For stdin, use - (default:
    `$LEDGER_FILE` or `$HOME/.hledger.journal`)

`--rules-file=RULESFILE`
:   Conversion rules file to use when reading CSV (default: FILE.rules)

`--alias=OLD=NEW`
:   rename accounts named OLD to NEW

`--anon`
:   anonymize accounts and payees

`--pivot FIELDNAME`
:   use some other field or tag for the account name

`-I --ignore-assertions`
:   ignore any failing balance assertions

General reporting options:

`-b --begin=DATE`
:   include postings/txns on or after this date

`-e --end=DATE`
:   include postings/txns before this date

`-D --daily`
:   multiperiod/multicolumn report by day

`-W --weekly`
:   multiperiod/multicolumn report by week

`-M --monthly`
:   multiperiod/multicolumn report by month

`-Q --quarterly`
:   multiperiod/multicolumn report by quarter

`-Y --yearly`
:   multiperiod/multicolumn report by year

`-p --period=PERIODEXP`
:   set start date, end date, and/or reporting interval all at once
    using [period expressions](manual.html#period-expressions) syntax
    (overrides the flags above)

`--date2`
:   match the secondary date instead (see command help for other
    effects)

`-U --unmarked`
:   include only unmarked postings/txns (can combine with -P or -C)

`-P --pending`
:   include only pending postings/txns

`-C --cleared`
:   include only cleared postings/txns

`-R --real`
:   include only non-virtual postings

`-NUM --depth=NUM`
:   hide/aggregate accounts or postings more than NUM levels deep

`-E --empty`
:   show items with zero amount, normally hidden

`-B --cost`
:   convert amounts to their cost at transaction time (using the
    [transaction price](journal.html#transaction-prices), if any)

`-V --value`
:   convert amounts to their market value on the report end date (using
    the most recent applicable [market
    price](journal.html#market-prices), if any)

`--auto`
:   apply [automated posting
    rules](journal.html#automated-posting-rules) to modify transactions.

`--forecast`
:   apply [periodic transaction](journal.html#periodic-transactions)
    rules to generate future transactions, to 6 months from now or
    report end date.

When a reporting option appears more than once in the command line, the
last one takes precedence.

Some reporting options can also be written as [query
arguments](#queries).

### Command options

To see options for a particular command, including command-specific
options, run: `hledger COMMAND -h`.

Command-specific options must be written after the command name, eg:
`hledger print -x`.

Additionally, if the command is an [addon](#commands), you may need to
put its options after a double-hyphen, eg: `hledger ui -- --watch`. Or,
you can run the addon executable directly: `hledger-ui --watch`.

### Command arguments

Most hledger commands accept arguments after the command name, which are
often a [query](#queries), filtering the data in some way.

### Argument files

You can save a set of command line options/arguments in a file, one per
line, and then reuse them by writing `@FILENAME` in a command line. To
prevent this expansion of `@`-arguments, precede them with a `--`
argument. For more, see [Save frequently used options](argfiles.html).

### Special characters

Option and argument values which contain problematic characters should
be escaped with double quotes, backslashes, or (best) single quotes.
Problematic characters means spaces, and also characters which are
significant to your command shell, such as less-than/greater-than. Eg:
`hledger register -p 'last year' "accounts receivable (receivable|payable)" amt:\>100`.

Characters which are significant both to the shell and in [regular
expressions](#regular-expressions) sometimes need to be double-escaped.
These include parentheses, the pipe symbol and the dollar sign. Eg, to
match the dollar symbol, bash users should do:
`hledger balance cur:'\$'` or `hledger balance cur:\\$`.

When hledger is invoking an addon executable (like hledger-ui), options
and arguments get de-escaped once more, so you might need
*triple*-escaping. Eg: `hledger ui cur:'\\$'` or `hledger ui cur:\\\\$`
in bash. (The number of backslashes in fish shell is left as an exercise
for the reader.)

Inside a file used for [argument expansion](#argument-expansion), one
less level of escaping is enough. (And in this case, backslashes seem to
work better than quotes. Eg: `cur:\$`).

If in doubt, keep things simple:

-   run add-on executables directly
-   write options after the command
-   enclose problematic args in single quotes
-   if needed, also add a backslash to escape regexp metacharacters

If you're really stumped, add `--debug=2` to troubleshoot.

### Input files

hledger reads transactions from a data file (and the add command writes
to it). By default this file is `$HOME/.hledger.journal` (or on Windows,
something like `C:/Users/USER/.hledger.journal`). You can override this
with the `$LEDGER_FILE` environment variable:

``` {.bash}
$ setenv LEDGER_FILE ~/finance/2016.journal
$ hledger stats
```

or with the `-f/--file` option:

``` {.bash}
$ hledger -f /some/file stats
```

The file name `-` (hyphen) means standard input:

``` {.bash}
$ cat some.journal | hledger -f-
```

Usually the data file is in hledger's journal format, but it can also be
one of several other formats, listed below. hledger detects the format
automatically based on the file extension, or if that is not recognised,
by trying each built-in "reader" in turn:

  ---------------------------------------------------------------------------
  Reader:       Reads:                             Used for file extensions:
  ------------- ---------------------------------- --------------------------
  `journal`     hledger's journal format, also     `.journal` `.j` `.hledger`
                some Ledger journals               `.ledger`

  `timeclock`   timeclock files (precise time      `.timeclock`
                logging)                           

  `timedot`     timedot files (approximate time    `.timedot`
                logging)                           

  `csv`         comma-separated values (data       `.csv`
                interchange)                       
  ---------------------------------------------------------------------------

If needed (eg to ensure correct error messages when a file has the
"wrong" extension), you can force a specific reader/format by prepending
it to the file path with a colon. Examples:

``` {.bash}
$ hledger -f csv:/some/csv-file.dat stats
$ echo 'i 2009/13/1 08:00:00' | hledger print -ftimeclock:-
```

You can also specify multiple `-f` options, to read multiple files as
one big journal. There are some limitations with this:

-   directives in one file will not affect the other files
-   [balance assertions](/journal.html#balance-assertions) will not see
    any account balances from previous files

If you need those, either use the [include
directive](/journal.html#including-other-files), or concatenate the
files, eg: `cat a.journal b.journal | hledger -f- CMD`.

### Smart dates

hledger's user interfaces accept a flexible "smart date" syntax (unlike
dates in the journal file). Smart dates allow some english words, can be
relative to today's date, and can have less-significant date parts
omitted (defaulting to 1).

Examples:

  -------------------------------------------------- -------------------------------------------------------
  `2009/1/1`, `2009/01/01`, `2009-1-1`, `2009.1.1`   simple dates, several separators allowed
  `2009/1`, `2009`                                   same as above - a missing day or month defaults to 1
  `1/1`, `january`, `jan`, `this year`               relative dates, meaning january 1 of the current year
  `next year`                                        january 1 of next year
  `this month`                                       the 1st of the current month
  `this week`                                        the most recent monday
  `last week`                                        the monday of the week before this one
  `lastweek`                                         spaces are optional
  `today`, `yesterday`, `tomorrow`                   
  -------------------------------------------------- -------------------------------------------------------

### Report start & end date

Most hledger reports show the full span of time represented by the
journal data, by default. So, the effective report start and end dates
will be the earliest and latest transaction or posting dates found in
the journal.

Often you will want to see a shorter time span, such as the current
month. You can specify a start and/or end date using
[`-b/--begin`](#reporting-options), [`-e/--end`](#reporting-options),
[`-p/--period`](#period-expressions) or a [`date:` query](#queries)
(described below). All of these accept the [smart date](#smart-dates)
syntax. One important thing to be aware of when specifying end dates: as
in Ledger, end dates are exclusive, so you need to write the date
*after* the last day you want to include.

Examples:

  ------------------- ---------------------------------------------------------------------------------------------
  `-b 2016/3/17`      begin on St. Patrick's day 2016
  `-e 12/1`           end at the start of december 1st of the current year (11/30 will be the last date included)
  `-b thismonth`      all transactions on or after the 1st of the current month
  `-p thismonth`      all transactions in the current month
  `date:2016/3/17-`   the above written as queries instead
  `date:-12/1`        
  `date:thismonth-`   
  `date:thismonth`    
  ------------------- ---------------------------------------------------------------------------------------------

### Report intervals

A report interval can be specified so that commands like
[register](#register), [balance](#balance) and [activity](#activity)
will divide their reports into multiple subperiods. The basic intervals
can be selected with one of `-D/--daily`, `-W/--weekly`, `-M/--monthly`,
`-Q/--quarterly`, or `-Y/--yearly`. More complex intervals may be
specified with a [period expression](#period-expressions). Report
intervals can not be specified with a [query](#queries), currently.

### Period expressions

The `-p/--period` option accepts period expressions, a shorthand way of
expressing a start date, end date, and/or report interval all at once.

Here's a basic period expression specifying the first quarter of 2009.
Note, hledger always treats start dates as inclusive and end dates as
exclusive:

`-p "from 2009/1/1 to 2009/4/1"`

Keywords like "from" and "to" are optional, and so are the spaces, as
long as you don't run two dates together. "to" can also be written as
"-". These are equivalent to the above:

  --------------------------
  `-p "2009/1/1 2009/4/1"`
  `-p2009/1/1to2009/4/1`
  `-p2009/1/1-2009/4/1`
  --------------------------

Dates are [smart dates](#smart-dates), so if the current year is 2009,
the above can also be written as:

  -------------------------
  `-p "1/1 4/1"`
  `-p "january-apr"`
  `-p "this year to 4/1"`
  -------------------------

If you specify only one date, the missing start or end date will be the
earliest or latest transaction in your journal:

  ---------------------- -----------------------------------
  `-p "from 2009/1/1"`   everything after january 1, 2009
  `-p "from 2009/1"`     the same
  `-p "from 2009"`       the same
  `-p "to 2009"`         everything before january 1, 2009
  ---------------------- -----------------------------------

A single date with no "from" or "to" defines both the start and end date
like so:

  ----------------- --------------------------------------------------------
  `-p "2009"`       the year 2009; equivalent to "2009/1/1 to 2010/1/1"
  `-p "2009/1"`     the month of jan; equivalent to "2009/1/1 to 2009/2/1"
  `-p "2009/1/1"`   just that day; equivalent to "2009/1/1 to 2009/1/2"
  ----------------- --------------------------------------------------------

The argument of `-p` can also begin with, or be, a [report
interval](#report-intervals) expression. The basic report intervals are
`daily`, `weekly`, `monthly`, `quarterly`, or `yearly`, which have the
same effect as the `-D`,`-W`,`-M`,`-Q`, or `-Y` flags. Between report
interval and start/end dates (if any), the word `in` is optional.
Examples:

  -----------------------------------------
  `-p "weekly from 2009/1/1 to 2009/4/1"`
  `-p "monthly in 2008"`
  `-p "quarterly"`
  -----------------------------------------

Note that `weekly`, `monthly`, `quarterly` and `yearly` intervals will
always start on the first day on week, month, quarter or year
accordingly, and will end on the last day of same period, even if
associated period expression specifies different explicit start and end
date.

For example:

  -------------------------------------------------------------------------------------------------------------------------------------
  `-p "weekly from 2009/1/1 to 2009/4/1"` -- starts on 2008/12/29, closest preceeding Monday
  `-p "monthly in 2008/11/25"` -- starts on 2018/11/01
  `-p "quarterly from 2009-05-05 to 2009-06-01"` - starts on 2009/04/01, ends on 2009/06/30, which are first and last days of Q2 2009
  `-p "yearly from 2009-12-29"` - starts on 2009/01/01, first day of 2009
  -------------------------------------------------------------------------------------------------------------------------------------

The following more complex report intervals are also supported:
`biweekly`, `bimonthly`, `every day|week|month|quarter|year`,
`every N days|weeks|months|quarters|years`.

All of these will start on the first day of the requested period and end
on the last one, as described above.

Examples:

  --------------------------------------------------------------------------------------------------
  `-p "bimonthly from 2008"` -- periods will have boundaries on 2008/01/01, 2008/03/01, ...
  `-p "every 2 weeks"` -- starts on closest preceeding Monday
  `-p "every 5 month from 2009/03"` -- periods will have boundaries on 2009/03/01, 2009/08/01, ...
  --------------------------------------------------------------------------------------------------

If you want intervals that start on arbitrary day of your choosing and
span a week, month or year, you need to use any of the following:

`every Nth day of week`, `every <weekday>`, `every Nth day [of month]`,
`every Nth weekday [of month]`, `every MM/DD [of year]`,
`every Nth MMM [of year]`, `every MMM Nth [of year]`.

Examples:

  -------------------------------------------------------------------------------------
  `-p "every 2nd day of week"` -- periods will go from Tue to Tue
  `-p "every Tue"` -- same
  `-p "every 15th day"` -- period boundaries will be on 15th of each month
  `-p "every 2nd Monday"` -- period boundaries will be on second Monday of each month
  `-p "every 11/05"` -- yearly periods with boundaries on 5th of Nov
  `-p "every 5th Nov"` -- same
  `-p "every Nov 5th"` -- same
  -------------------------------------------------------------------------------------

Show historical balances at end of 15th each month (N is exclusive end
date):

`hledger balance -H -p "every 16th day"`

Group postings from start of wednesday to end of next tuesday (N is
start date and exclusive end date):

`hledger register checking -p "every 3rd day of week"`

### Depth limiting

With the `--depth N` option (short form: `-N`), commands like
[account](#account), [balance](#balance) and [register](#register) will
show only the uppermost accounts in the account tree, down to level N.
Use this when you want a summary with less detail. This flag has the
same effect as a `depth:` query argument (so `-2`, `--depth=2` or
`depth:2` are basically equivalent).

### Pivoting

Normally hledger sums amounts, and organizes them in a hierarchy, based
on account name. The `--pivot FIELD` option causes it to sum and
organize hierarchy based on the value of some other field instead. FIELD
can be: `code`, `description`, `payee`, `note`, or the full name (case
insensitive) of any [tag](/journal.html#tags). As with account names,
values containing `colon:separated:parts` will be displayed
hierarchically in reports.

`--pivot` is a general option affecting all reports; you can think of
hledger transforming the journal before any other processing, replacing
every posting's account name with the value of the specified field on
that posting, inheriting it from the transaction or using a blank value
if it's not present.

An example:

``` {.journal}
2016/02/16 Member Fee Payment
    assets:bank account                    2 EUR
    income:member fees                    -2 EUR  ; member: John Doe
```

Normal balance report showing account names:

``` {.shell}
$ hledger balance
               2 EUR  assets:bank account
              -2 EUR  income:member fees
--------------------
                   0
```

Pivoted balance report, using member: tag values instead:

``` {.shell}
$ hledger balance --pivot member
               2 EUR
              -2 EUR  John Doe
--------------------
                   0
```

One way to show only amounts with a member: value (using a
[query](#queries), described below):

``` {.shell}
$ hledger balance --pivot member tag:member=.
              -2 EUR  John Doe
--------------------
              -2 EUR
```

Another way (the acct: query matches against the pivoted "account
name"):

    $ hledger balance --pivot member acct:.
                  -2 EUR  John Doe
    --------------------
                  -2 EUR

### Cost

The `-B/--cost` flag converts amounts to their cost at transaction time,
if they have a [transaction price](/journal.html#transaction-prices)
specified.

### Market value

The `-V/--value` flag converts reported amounts to their current market
value. Specifically, when there is a [market
price](journal.html#market-prices) (P directive) for the amount's
commodity, dated on or before today's date (or the [report end
date](#report-start-end-date) if specified), the amount will be
converted to the price's commodity.

When there are multiple applicable P directives, -V chooses the most
recent one, or in case of equal dates, the last-parsed one.

For example:

``` {.journal}
# one euro is worth this many dollars from nov 1
P 2016/11/01 € $1.10

# purchase some euros on nov 3
2016/11/3
    assets:euros        €100
    assets:checking

# the euro is worth fewer dollars by dec 21
P 2016/12/21 € $1.03
```

How many euros do I have ?

    $ hledger -f t.j bal euros
                    €100  assets:euros

What are they worth on nov 3 ? (no report end date specified, defaults
to the last date in the journal)

    $ hledger -f t.j bal euros -V
                 $110.00  assets:euros

What are they worth on dec 21 ?

    $ hledger -f t.j bal euros -V -e 2016/12/21
                 $103.00  assets:euros

Currently, hledger's -V only uses market prices recorded with P
directives, not [transaction prices](journal.html#transaction-prices)
(unlike Ledger).

### Combining -B and -V

Using -B/--cost and -V/--value together is currently allowed, but the
results are probably not meaningful. Let us know if you find a use for
this.

### Regular expressions

hledger uses [regular expressions](http://www.regular-expressions.info)
in a number of places:

-   [query terms](#queries), on the command line and in the hledger-web
    search form: `REGEX`, `desc:REGEX`, `cur:REGEX`, `tag:...=REGEX`
-   [CSV rules](#csv-rules) conditional blocks: `if REGEX ...`
-   [account alias](#account-aliases) directives and options:
    `alias /REGEX/ = REPLACEMENT`, `--alias /REGEX/=REPLACEMENT`

hledger's regular expressions come from the
[regex-tdfa](http://hackage.haskell.org/package/regex-tdfa/docs/Text-Regex-TDFA.html)
library. In general they:

-   are case insensitive
-   are infix matching (do not need to match the entire thing being
    matched)
-   are [POSIX extended regular
    expressions](http://www.regular-expressions.info/posix.html#ere)
-   also support [GNU word
    boundaries](http://www.regular-expressions.info/wordboundaries.html)
    (\\\<, \\\>, \\b, \\B)
-   and parenthesised [capturing
    groups](http://www.regular-expressions.info/refcapture.html) and
    numeric backreferences in replacement strings
-   do not support [mode
    modifiers](http://www.regular-expressions.info/modifiers.html) like
    (?s)

Some things to note:

-   In the `alias` directive and `--alias` option, regular expressions
    must be enclosed in forward slashes (`/REGEX/`). Elsewhere in
    hledger, these are not required.

-   In queries, to match a regular expression metacharacter like `$` as
    a literal character, prepend a backslash. Eg to search for amounts
    with the dollar sign in hledger-web, write `cur:\$`.

-   On the command line, some metacharacters like `$` have a special
    meaning to the shell and so must be escaped at least once more. See
    [Special characters](#special-characters).

## QUERIES

One of hledger's strengths is being able to quickly report on precise
subsets of your data. Most commands accept an optional query expression,
written as arguments after the command name, to filter the data by date,
account name or other criteria. The syntax is similar to a web search:
one or more space-separated search terms, quotes to enclose whitespace,
prefixes to match specific fields, a not: prefix to negate the match.

We do not yet support arbitrary boolean combinations of search terms;
instead most commands show transactions/postings/accounts which match
(or negatively match):

-   any of the description terms AND
-   any of the account terms AND
-   any of the status terms AND
-   all the other terms.

The [print](/manual.html#print) command instead shows transactions
which:

-   match any of the description terms AND
-   have any postings matching any of the positive account terms AND
-   have no postings matching any of the negative account terms AND
-   match all the other terms.

The following kinds of search terms can be used. Remember these can also
be prefixed with **`not:`**, eg to exclude a particular subaccount.

**`REGEX`**
:   match account names by this regular expression. (No prefix is
    equivalent to `acct:`).

**`acct:REGEX`**
:   same as above

**`amt:N, amt:<N, amt:<=N, amt:>N, amt:>=N`**
:   match postings with a single-commodity amount that is equal to, less
    than, or greater than N. (Multi-commodity amounts are not tested,
    and will always match.) The comparison has two modes: if N is
    preceded by a + or - sign (or is 0), the two signed numbers are
    compared. Otherwise, the absolute magnitudes are compared, ignoring
    sign.

**`code:REGEX`**
:   match by transaction code (eg check number)

**`cur:REGEX`**
:   match postings or transactions including any amounts whose
    currency/commodity symbol is fully matched by REGEX. (For a partial
    match, use `.*REGEX.*`). Note, to match characters which are
    regex-significant, like the dollar sign (`$`), you need to prepend
    `\`. And when using the command line you need to add one more level
    of quoting to hide it from the shell, so eg do:
    `hledger print cur:'\$'` or `hledger print cur:\\$`.

**`desc:REGEX`**
:   match transaction descriptions.

**`date:PERIODEXPR`**
:   match dates within the specified period. PERIODEXPR is a [period
    expression](#period-expressions) (with no report interval).
    Examples: `date:2016`, `date:thismonth`, `date:2000/2/1-2/15`,
    `date:lastweek-`. If the `--date2` command line flag is present,
    this matches [secondary dates](manual.html#secondary-dates) instead.

**`date2:PERIODEXPR`**
:   match secondary dates within the specified period.

**`depth:N`**
:   match (or display, depending on command) accounts at or above this
    depth

**`note:REGEX`**
:   match transaction [notes](/manual.html#payee-and-note) (part of
    description right of `|`, or whole description when there's no `|`)

**`payee:REGEX`**
:   match transaction [payee/payer names](/manual.html#payee-and-note)
    (part of description left of `|`, or whole description when there's
    no `|`)

**`real:, real:0`**
:   match real or virtual postings respectively

**`status:, status:!, status:*`**
:   match unmarked, pending, or cleared transactions respectively

**`tag:REGEX[=REGEX]`**
:   match by tag name, and optionally also by tag value. Note a tag:
    query is considered to match a transaction if it matches any of the
    postings. Also remember that postings inherit the tags of their
    parent transaction.

The following special search term is used automatically in hledger-web,
only:

**`inacct:ACCTNAME`**
:   tells hledger-web to show the transaction register for this account.
    Can be filtered further with `acct` etc.

Some of these can also be expressed as command-line options (eg
`depth:2` is equivalent to `--depth 2`). Generally you can mix options
and query arguments, and the resulting query will be their intersection
(perhaps excluding the `-p/--period` option).

## COMMANDS

hledger provides a number of subcommands; `hledger` with no arguments
shows a list.

If you install additional `hledger-*` packages, or if you put programs
or scripts named `hledger-NAME` in your PATH, these will also be listed
as subcommands.

Run a subcommand by writing its name as first argument (eg
`hledger incomestatement`). You can also write one of the standard short
aliases displayed in parentheses in the command list (`hledger b`), or
any any unambiguous prefix of a command name (`hledger inc`).

Here are all the builtin commands in alphabetical order. See also
`hledger` for a more organised command list, and `hledger CMD -h` for
detailed command help.

<!--
---
comment:
for each command: name, synopsis, description, examples.
...
-->
### accounts

Show account names. Alias: a.

`--tree`
:   show short account names, as a tree

`--flat`
:   show full account names, as a list (default)

`--drop=N`
:   in flat mode: omit N leading account name parts

This command lists all account names that are in use (ie, all the
accounts which have at least one transaction posting to them). With
query arguments, only matched account names are shown.

It shows a flat list by default. With `--tree`, it uses indentation to
show the account hierarchy.

In flat mode you can add `--drop N` to omit the first few account name
components.

Examples:

::: {.container-fluid}
::: {.row}
::: {.col-sm-4}
``` {.shell}
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
:::

::: {.col-sm-4}
``` {.shell}
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
:::

::: {.col-sm-4}
``` {.shell}
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
:::
:::
:::

### activity

Show an ascii barchart of posting counts per interval.

The activity command displays an ascii histogram showing transaction
counts by day, week, month or other reporting interval (by day is the
default). With query arguments, it counts only matched transactions.

``` {.shell}
$ hledger activity --quarterly
2008-01-01 **
2008-04-01 *******
2008-07-01 
2008-10-01 **
```

### add

Prompt for transactions and add them to the journal.

`--no-new-accounts`
:   don't allow creating new accounts; helps prevent typos when entering
    account names

Many hledger users edit their journals directly with a text editor, or
generate them from CSV. For more interactive data entry, there is the
`add` command, which prompts interactively on the console for new
transactions, and appends them to the journal file (if there are
multiple `-f FILE` options, the first file is used.) Existing
transactions are not changed. This is the only hledger command that
writes to the journal file.

To use it, just run `hledger add` and follow the prompts. You can add as
many transactions as you like; when you are finished, enter `.` or press
control-d or control-c to exit.

Features:

-   add tries to provide useful defaults, using the most similar recent
    transaction (by description) as a template.
-   You can also set the initial defaults with command line arguments.
-   [Readline-style edit
    keys](http://tiswww.case.edu/php/chet/readline/rluserman.html#SEC3)
    can be used during data entry.
-   The tab key will auto-complete whenever possible - accounts,
    descriptions, dates (`yesterday`, `today`, `tomorrow`). If the input
    area is empty, it will insert the default value.
-   If the journal defines a [default commodity](#default-commodity), it
    will be added to any bare numbers entered.
-   A parenthesised transaction [code](#entries) may be entered
    following a date.
-   [Comments](#comments) and tags may be entered following a
    description or amount.
-   If you make a mistake, enter `<` at any prompt to restart the
    transaction.
-   Input prompts are displayed in a different colour when the terminal
    supports it.

Example (see the
[tutorial](step-by-step.html#record-a-transaction-with-hledger-add) for
a detailed explanation):

``` {.shell}
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
```

### balance

Show accounts and their balances. Aliases: b, bal.

`--change`
:   show balance change in each period (default)

`--cumulative`
:   show balance change accumulated across periods (in multicolumn
    reports)

`-H --historical`
:   show historical ending balance in each period (includes postings
    before report start date)

`--tree`
:   show accounts as a tree; amounts include subaccounts (default in
    simple reports)

`--flat`
:   show accounts as a list; amounts exclude subaccounts except when
    account is depth-clipped (default in multicolumn reports)

`-A --average`
:   show a row average column (in multicolumn mode)

`-T --row-total`
:   show a row total column (in multicolumn mode)

`-N --no-total`
:   don't show the final total row

`--drop=N`
:   omit N leading account name parts (in flat mode)

`--no-elide`
:   don't squash boring parent accounts (in tree mode)

`--format=LINEFORMAT`
:   in single-column balance reports: use this custom line format

`-O FMT --output-format=FMT`
:   select the output format. Supported formats: txt, csv.

`-o FILE --output-file=FILE`
:   write output to FILE. A file extension matching one of the above
    formats selects that format.

`--pretty-tables`
:   use unicode to display prettier tables.

`--sort-amount`
:   sort by amount instead of account name (in flat mode). With multiple
    columns, sorts by the row total, or by row average if that is
    displayed.

`--budget`
:   show performance compared to budget goals defined by [periodic
    transactions](journal.html#periodic-transactions)

`--show-unbudgeted`
:   with --budget, show unbudgeted accounts also

The balance command displays accounts and balances. It is hledger's most
featureful and versatile command.

``` {.shell}
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

More precisely, the balance command shows the *change* to each account's
balance caused by all (matched) postings. In the common case where you
do not filter by date and your journal sets the correct opening
balances, this is the same as the account's ending balance.

By default, accounts are displayed hierarchically, with subaccounts
indented below their parent. "Boring" accounts, which contain a single
interesting subaccount and no balance of their own, are elided into the
following line for more compact output. (Use `--no-elide` to prevent
this. Eliding of boring accounts is not yet supported in multicolumn
reports.)

Each account's balance is the "inclusive" balance - it includes the
balances of any subaccounts.

Accounts which have zero balance (and no non-zero subaccounts) are
omitted. Use `-E/--empty` to show them.

A final total is displayed by default; use `-N/--no-total` to suppress
it:

``` {.shell}
$ hledger balance -p 2008/6 expenses --no-total
                  $2  expenses
                  $1    food
                  $1    supplies
```

#### Flat mode

To see a flat list of full account names instead of the default
hierarchical display, use `--flat`. In this mode, accounts (unless
depth-clipped) show their "exclusive" balance, excluding any subaccount
balances. In this mode, you can also use `--drop N` to omit the first
few account name components.

``` {.shell}
$ hledger balance -p 2008/6 expenses -N --flat --drop 1
                  $1  food
                  $1  supplies
```

#### Depth limited balance reports

With `--depth N`, balance shows accounts only to the specified depth.
This is very useful to show a complex charts of accounts in less detail.
In flat mode, balances from accounts below the depth limit will be shown
as part of a parent account at the depth limit.

``` {.shell}
$ hledger balance -N --depth 1
                 $-1  assets
                  $2  expenses
                 $-2  income
                  $1  liabilities
```

<!-- $ for y in 2006 2007 2008 2009 2010; do echo; echo $y; hledger -f $y.journal balance ^expenses --depth 2; done -->
#### Multicolumn balance reports

With a [reporting interval](#reporting-interval), multiple balance
columns will be shown, one for each report period. There are three types
of multi-column balance report, showing different information:

1.  By default: each column shows the sum of postings in that period, ie
    the account's change of balance in that period. This is useful eg
    for a monthly income statement: <!--
    multi-column income statement: 

       $ hledger balance ^income ^expense -p 'monthly this year' --depth 3

    or cashflow statement:

       $ hledger balance ^assets ^liabilities 'not:(receivable|payable)' -p 'weekly this month'
    -->

    ``` {.shell}
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

2.  With `--cumulative`: each column shows the ending balance for that
    period, accumulating the changes across periods, starting from 0 at
    the report start date:

    ``` {.shell}
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

3.  With `--historical/-H`: each column shows the actual historical
    ending balance for that period, accumulating the changes across
    periods, starting from the actual balance at the report start date.
    This is useful eg for a multi-period balance sheet, and when you are
    showing only the data after a certain start date:

    ``` {.shell}
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

Multi-column balance reports display accounts in flat mode by default;
to see the hierarchy, use `--tree`.

With a reporting interval (like `--quarterly` above), the report
start/end dates will be adjusted if necessary so that they encompass the
displayed report periods. This is so that the first and last periods
will be "full" and comparable to the others.

The `-E/--empty` flag does two things in multicolumn balance reports:
first, the report will show all columns within the specified report
period (without -E, leading and trailing columns with all zeroes are not
shown). Second, all accounts which existed at the report start date will
be considered, not just the ones with activity during the report period
(use -E to include low-activity accounts which would otherwise would be
omitted).

The `-T/--row-total` flag adds an additional column showing the total
for each row.

The `-A/--average` flag adds a column showing the average value in each
row.

Here's an example of all three:

``` {.shell}
$ hledger balance -Q income expenses --tree -ETA
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

#### Budgets

With `--budget` and a [report interval](manual.html#report-intervals),
all [periodic transactions](journal.html#periodic-transactions) in your
journal with that interval, active during the requested report period,
are interpreted as recurring budget goals for the specified accounts
(and subaccounts), and the report will show the difference between
actual and budgeted balances.

For example, you can take average monthly expenses in the common expense
categories to construct a minimal monthly budget:

``` {.journal}
;; Budget
~ monthly
  income  $2000
  expenses:food    $400
  expenses:bus     $50
  expenses:movies  $30
  assets:bank:checking

;; Two months worth of expenses
2017-11-01
  income  $1950
  expenses:food    $396
  expenses:bus     $49
  expenses:movies  $30
  expenses:supplies  $20
  assets:bank:checking

2017-12-01
  income  $2100
  expenses:food    $412
  expenses:bus     $53
  expenses:gifts   $100
  assets:bank:checking
```

You can now see a monthly budget performance report:

``` {.shell}
$ hledger balance -M --budget
Balance changes in 2017/11/01-2017/12/31:

                       ||                2017/11                  2017/12 
=======================++=================================================
 <unbudgeted>:expenses ||                    $20                     $100 
 assets:bank:checking  || $-2445 [99% of $-2480]  $-2665 [107% of $-2480] 
 expenses:bus          ||       $49 [98% of $50]        $53 [106% of $50] 
 expenses:food         ||     $396 [99% of $400]      $412 [103% of $400] 
 expenses:movies       ||      $30 [100% of $30]            0 [0% of $30] 
 income                ||   $1950 [98% of $2000]    $2100 [105% of $2000] 
-----------------------++-------------------------------------------------
                       ||                      0                        0 
```

You can roll over unspent budgets to next period with `--cumulative`:

``` {.shell}
$ hledger balance -M --budget --cumulative
Ending balances (cumulative) in 2017/11/01-2017/12/31:

                       ||             2017/11/30               2017/12/31 
=======================++=================================================
 <unbudgeted>:expenses ||                    $20                     $120 
 assets:bank:checking  || $-2445 [99% of $-2480]  $-5110 [103% of $-4960] 
 expenses:bus          ||       $49 [98% of $50]      $102 [102% of $100] 
 expenses:food         ||     $396 [99% of $400]      $808 [101% of $800] 
 expenses:movies       ||      $30 [100% of $30]         $30 [50% of $60] 
 income                ||   $1950 [98% of $2000]    $4050 [101% of $4000] 
-----------------------++-------------------------------------------------
                       ||                      0                        0
```

Accounts with no budget goals (not mentioned in the periodic
transactions) will be aggregated under `<unbudgeted>`, unless you add
the `--show-unbudgeted` flag to display them normally:

``` {.shell}
$ hledger balance --budget --show-unbudgeted
Balance changes in 2017/11/01-2017/12/31:

                      ||                2017/11                  2017/12 
======================++=================================================
 assets:bank:checking || $-2445 [99% of $-2480]  $-2665 [107% of $-2480] 
 expenses:bus         ||       $49 [98% of $50]        $53 [106% of $50] 
 expenses:food        ||     $396 [99% of $400]      $412 [103% of $400] 
 expenses:gifts       ||                      0                     $100 
 expenses:movies      ||      $30 [100% of $30]            0 [0% of $30] 
 expenses:supplies    ||                    $20                        0 
 income               ||   $1950 [98% of $2000]    $2100 [105% of $2000] 
----------------------++-------------------------------------------------
                      ||                      0                        0 
```

For more examples and details, see [Budgeting and
Forecasting](budgeting-and-forecasting.html).

#### Custom balance output

In simple (non-multi-column) balance reports, you can customise the
output with `--format FMT`:

``` {.shell}
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

The FMT format string (plus a newline) specifies the formatting applied
to each account/balance pair. It may contain any suitable text, with
data fields interpolated like so:

`%[MIN][.MAX](FIELDNAME)`

-   MIN pads with spaces to at least this width (optional)
-   MAX truncates at this width (optional)
-   FIELDNAME must be enclosed in parentheses, and can be one of:

    -   `depth_spacer` - a number of spaces equal to the account's
        depth, or if MIN is specified, MIN \* depth spaces.
    -   `account` - the account's name
    -   `total` - the account's balance/posted total, right justified

Also, FMT can begin with an optional prefix to control how
multi-commodity amounts are rendered:

-   `%_` - render on multiple lines, bottom-aligned (the default)
-   `%^` - render on multiple lines, top-aligned
-   `%,` - render on one line, comma-separated

There are some quirks. Eg in one-line mode, `%(depth_spacer)` has no
effect, instead `%(account)` has indentation built in. <!-- XXX retest:
Consistent column widths are not well enforced, causing ragged edges unless you set suitable widths.
Beware of specifying a maximum width; it will clip account names and amounts that are too wide, with no visible indication.
--> Experimentation may be needed to get pleasing results.

Some example formats:

-   `%(total)` - the account's total
-   `%-20.20(account)` - the account's name, left justified, padded to
    20 characters and clipped at 20 characters
-   `%,%-50(account)  %25(total)` - account name padded to 50
    characters, total padded to 20 characters, with multiple commodities
    rendered on one line
-   `%20(total)  %2(depth_spacer)%-(account)` - the default format for
    the single-column balance report

#### Colour support

The balance command shows negative amounts in red, if:

-   the `TERM` environment variable is not set to `dumb`
-   the output is not being redirected or piped anywhere

#### Output destination

The balance, print, register and stats commands can write their output
to a destination other than the console. This is controlled by the
`-o/--output-file` option.

``` {.shell}
$ hledger balance -o -     # write to stdout (the default)
$ hledger balance -o FILE  # write to FILE
```

#### CSV output

The balance, print and register commands can write their output as CSV.
This is useful for exporting data to other applications, eg to make
charts in a spreadsheet. This is controlled by the `-O/--output-format`
option, or by specifying a `.csv` file extension with
`-o/--output-file`.

``` {.shell}
$ hledger balance -O csv       # write CSV to stdout
$ hledger balance -o FILE.csv  # write CSV to FILE.csv
```

### balancesheet

Show a balance sheet. Alias: bs.

`--change`
:   show balance change in each period, instead of historical ending
    balances

`--cumulative`
:   show balance change accumulated across periods (in multicolumn
    reports), instead of historical ending balances

`-H --historical`
:   show historical ending balance in each period (includes postings
    before report start date) (default)

`--tree`
:   show accounts as a tree; amounts include subaccounts (default in
    simple reports)

`--flat`
:   show accounts as a list; amounts exclude subaccounts except when
    account is depth-clipped (default in multicolumn reports)

`-A --average`
:   show a row average column (in multicolumn mode)

`-T --row-total`
:   show a row total column (in multicolumn mode)

`-N --no-total`
:   don't show the final total row

`--drop=N`
:   omit N leading account name parts (in flat mode)

`--no-elide`
:   don't squash boring parent accounts (in tree mode)

`--format=LINEFORMAT`
:   in single-column balance reports: use this custom line format

`--sort-amount`
:   sort by amount instead of account name

This command displays a simple [balance
sheet](http://en.wikipedia.org/wiki/Balance_sheet). It currently assumes
that you have top-level accounts named `asset` and `liability` (plural
forms also allowed.)

``` {.shell}
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

With a [reporting interval](#reporting-interval), multiple columns will
be shown, one for each report period. As with [multicolumn balance
reports](#multicolumn-balance-reports), you can alter the report mode
with `--change`/`--cumulative`/`--historical`. Normally balancesheet
shows historical ending balances, which is what you need for a balance
sheet; note this means it ignores report begin dates.

### balancesheetequity

Show a balance sheet including equity. Alias: bse.

Other than showing the equity accounts, this command is exactly the same
as the command balancesheet. Please refer to it for the available
options.

This command displays a
[balancesheet](http://en.wikipedia.org/wiki/Balance_sheet). It currently
assumes that you have top-level accounts named `asset`, `liability` and
`equity` (plural forms also allowed.)

``` {.shell}
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

### cashflow

Show a cashflow statement. Alias: cf.

`--change`
:   show balance change in each period (default)

`--cumulative`
:   show balance change accumulated across periods (in multicolumn
    reports), instead of changes during periods

`-H --historical`
:   show historical ending balance in each period (includes postings
    before report start date), instead of changes during each period

`--tree`
:   show accounts as a tree; amounts include subaccounts (default in
    simple reports)

`--flat`
:   show accounts as a list; amounts exclude subaccounts except when
    account is depth-clipped (default in multicolumn reports)

`-A --average`
:   show a row average column (in multicolumn mode)

`-T --row-total`
:   show a row total column (in multicolumn mode)

`-N --no-total`
:   don't show the final total row (in simple reports)

`--drop=N`
:   omit N leading account name parts (in flat mode)

`--no-elide`
:   don't squash boring parent accounts (in tree mode)

`--format=LINEFORMAT`
:   in single-column balance reports: use this custom line format

`--sort-amount`
:   sort by amount instead of account name

This command displays a simple [cashflow
statement](http://en.wikipedia.org/wiki/Cash_flow_statement) It shows
the change in all "cash" (ie, liquid assets) accounts for the period. It
currently assumes that cash accounts are under a top-level account named
`asset` and do not contain `receivable`, `:A/R` or `:fixed`.

``` {.shell}
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

With a [reporting interval](#reporting-interval), multiple columns will
be shown, one for each report period. Normally cashflow shows changes in
assets per period, though as with [multicolumn balance
reports](#multicolumn-balance-reports) you can alter the report mode
with `--change`/`--cumulative`/`--historical`.

### check-dates

Check that transactions are sorted by increasing date. With a query,
only matched transactions' dates are checked.

### check-dupes

Report account names having the same leaf but different prefixes. An
example: http://stefanorodighiero.net/software/hledger-dupes.html

### equity

Print closing/opening transactions that bring some or all account
balances to zero and back. Can be useful for bringing account balances
across file boundaries.

### help

Show any of the hledger manuals.

The `help` command displays any of the main [hledger
manuals](/docs.html), in one of several ways. Run it with no argument to
list the manuals, or provide a full or partial manual name to select
one.

hledger manuals are available in several formats. hledger help will use
the first of these display methods that it finds: info, man, \$PAGER,
less, stdout (or when non-interactive, just stdout). You can force a
particular viewer with the `--info`, `--man`, `--pager`, `--cat` flags.

``` {.shell}
$ hledger help
Please choose a manual by typing "hledger help MANUAL" (a substring is ok).
Manuals: hledger hledger-ui hledger-web hledger-api journal csv timeclock timedot
```

``` {.shell}
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
```

### import

Read new transactions added to each FILE since last run, and add them to
the main journal file.

`--dry-run`
:   just show the transactions to be imported

The input files are specified as arguments - no need to write -f before
each one. So eg to add new transactions from all CSV files to the main
journal, it's just: `hledger import *.csv`

New transactions are detected in the same way as print --new: by
assuming transactions are always added to the input files in increasing
date order, and by saving `.latest.FILE` state files.

The --dry-run output is in journal format, so you can filter it, eg to
see only uncategorised transactions:

``` {.shell}
$ hledger import --dry ... | hledger -f- print unknown --ignore-assertions
```

### incomestatement

Show an income statement. Alias: is.

`--change`
:   show balance change in each period (default)

`--cumulative`
:   show balance change accumulated across periods (in multicolumn
    reports), instead of changes during periods

`-H --historical`
:   show historical ending balance in each period (includes postings
    before report start date), instead of changes during each period

`--tree`
:   show accounts as a tree; amounts include subaccounts (default in
    simple reports)

`--flat`
:   show accounts as a list; amounts exclude subaccounts except when
    account is depth-clipped (default in multicolumn reports)

`-A --average`
:   show a row average column (in multicolumn mode)

`-T --row-total`
:   show a row total column (in multicolumn mode)

`-N --no-total`
:   don't show the final total row

`--drop=N`
:   omit N leading account name parts (in flat mode)

`--no-elide`
:   don't squash boring parent accounts (in tree mode)

`--format=LINEFORMAT`
:   in single-column balance reports: use this custom line format

`--sort-amount`
:   sort by amount instead of account name

This command displays a simple [income
statement](http://en.wikipedia.org/wiki/Income_statement). It currently
assumes that you have top-level accounts named `income` (or `revenue`)
and `expense` (plural forms also allowed.)

``` {.shell}
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

With a [reporting interval](#reporting-interval), multiple columns will
be shown, one for each report period. Normally incomestatement shows
revenues/expenses per period, though as with [multicolumn balance
reports](#multicolumn-balance-reports) you can alter the report mode
with `--change`/`--cumulative`/`--historical`.

### prices

Print all [market prices](/manual#market-prices) from the journal.

### print

Show transactions from the journal. Aliases: p, txns.

`-m STR --match=STR`
:   show the transaction whose description is most similar to STR, and
    is most recent

`--new`
:   show only newer-dated transactions added in each file since last run

`-x     --explicit`
:   show all amounts explicitly

`-O FMT --output-format=FMT`
:   select the output format. Supported formats: txt, csv.

`-o FILE --output-file=FILE`
:   write output to FILE. A file extension matching one of the above
    formats selects that format.

``` {.shell}
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

The print command displays full journal entries (transactions) from the
journal file in date order, tidily formatted. print's output is always a
valid [hledger journal](/journal.html). It preserves all transaction
information, but it does not preserve directives or inter-transaction
comments

Normally, the journal entry's explicit or implicit amount style is
preserved. Ie when an amount is omitted in the journal, it will be
omitted in the output. You can use the `-x`/`--explicit` flag to make
all amounts explicit, which can be useful for troubleshooting or for
making your journal more readable and robust against data entry errors.
Note, `-x` will cause postings with a multi-commodity amount (these can
arise when a multi-commodity transaction has an implicit amount) will be
split into multiple single-commodity postings, for valid journal output.

With `-B`/`--cost`, amounts with [transaction
prices](/journal.html#transaction-prices) are converted to cost using
that price. This can be used for troubleshooting.

With `-m`/`--match` and a STR argument, print will show at most one
transaction: the one one whose description is most similar to STR, and
is most recent. STR should contain at least two characters. If there is
no similar-enough match, no transaction will be shown.

With `--new`, for each FILE being read, hledger reads (and writes) a
special state file (`.latest.FILE` in the same directory), containing
the latest transaction date(s) that were seen last time FILE was read.
When this file is found, only transactions with newer dates (and new
transactions on the latest date) are printed. This is useful for
ignoring already-seen entries in import data, such as downloaded CSV
files. Eg:

``` {.console}
$ hledger -f bank1.csv print --new
# shows transactions added since last print --new on this file
```

This assumes that transactions added to FILE always have same or
increasing dates, and that transactions on the same day do not get
reordered. See also the [import](#import) command.

The print command also supports [output
destination](#output-destination) and [CSV output](#csv-output). Here's
an example of print's CSV output:

``` {.shell}
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

-   There is one CSV record per posting, with the parent transaction's
    fields repeated.
-   The "txnidx" (transaction index) field shows which postings belong
    to the same transaction. (This number might change if transactions
    are reordered within the file, files are parsed/included in a
    different order, etc.)
-   The amount is separated into "commodity" (the symbol) and "amount"
    (numeric quantity) fields.
-   The numeric amount is repeated in either the "credit" or "debit"
    column, for convenience. (Those names are not accurate in the
    accounting sense; it just puts negative amounts under credit and
    zero or greater amounts under debit.)

### print-unique

Print transactions which do not reuse an already-seen description.

### register

Show postings and their running total. Aliases: r, reg.

`--cumulative`
:   show running total from report start date (default)

`-H --historical`
:   show historical running total/balance (includes postings before
    report start date)

`-A --average`
:   show running average of posting amounts instead of total (implies
    --empty)

`-r --related`
:   show postings' siblings instead

`-w N --width=N`
:   set output width (default: terminal width or COLUMNS. -wN,M sets
    description width as well)

`-O FMT --output-format=FMT`
:   select the output format. Supported formats: txt, csv.

`-o FILE --output-file=FILE`
:   write output to FILE. A file extension matching one of the above
    formats selects that format.

The register command displays postings, one per line, and their running
total. This is typically used with a [query](#queries) selecting a
particular account, to see that account's activity:

``` {.shell}
$ hledger register checking
2008/01/01 income               assets:bank:checking            $1            $1
2008/06/01 gift                 assets:bank:checking            $1            $2
2008/06/02 save                 assets:bank:checking           $-1            $1
2008/12/31 pay off              assets:bank:checking           $-1             0
```

The `--historical`/`-H` flag adds the balance from any undisplayed prior
postings to the running total. This is useful when you want to see only
recent activity, with a historically accurate running balance:

``` {.shell}
$ hledger register checking -b 2008/6 --historical
2008/06/01 gift                 assets:bank:checking            $1            $2
2008/06/02 save                 assets:bank:checking           $-1            $1
2008/12/31 pay off              assets:bank:checking           $-1             0
```

The `--depth` option limits the amount of sub-account detail displayed.

The `--average`/`-A` flag shows the running average posting amount
instead of the running total (so, the final number displayed is the
average for the whole report period). This flag implies `--empty` (see
below). It is affected by `--historical`. It works best when showing
just one account and one commodity.

The `--related`/`-r` flag shows the *other* postings in the transactions
of the postings which would normally be shown.

With a [reporting interval](#reporting-interval), register shows summary
postings, one per interval, aggregating the postings to each account:

``` {.shell}
$ hledger register --monthly income
2008/01                 income:salary                          $-1           $-1
2008/06                 income:gifts                           $-1           $-2
```

Periods with no activity, and summary postings with a zero amount, are
not shown by default; use the `--empty`/`-E` flag to see them:

``` {.shell}
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

Often, you'll want to see just one line per interval. The `--depth`
option helps with this, causing subaccounts to be aggregated:

``` {.shell}
$ hledger register --monthly assets --depth 1h
2008/01                 assets                                  $1            $1
2008/06                 assets                                 $-1             0
2008/12                 assets                                 $-1           $-1
```

Note when using report intervals, if you specify start/end dates these
will be adjusted outward if necessary to contain a whole number of
intervals. This ensures that the first and last intervals are full
length and comparable to the others in the report.

#### Custom register output

register uses the full terminal width by default, except on windows. You
can override this by setting the `COLUMNS` environment variable (not a
bash shell variable) or by using the `--width`/`-w` option.

The description and account columns normally share the space equally
(about half of (width - 40) each). You can adjust this by adding a
description width as part of --width's argument, comma-separated:
`--width W,D` . Here's a diagram:

    <--------------------------------- width (W) ---------------------------------->
    date (10)  description (D)       account (W-41-D)     amount (12)   balance (12)
    DDDDDDDDDD dddddddddddddddddddd  aaaaaaaaaaaaaaaaaaa  AAAAAAAAAAAA  AAAAAAAAAAAA

and some examples:

``` {.shell}
$ hledger reg                     # use terminal width (or 80 on windows)
$ hledger reg -w 100              # use width 100
$ COLUMNS=100 hledger reg         # set with one-time environment variable
$ export COLUMNS=100; hledger reg # set till session end (or window resize)
$ hledger reg -w 100,40           # set overall width 100, description width 40
$ hledger reg -w $COLUMNS,40      # use terminal width, and set description width
```

The register command also supports the `-o/--output-file` and
`-O/--output-format` options for controlling [output
destination](#output-destination) and [CSV output](#csv-output).

### register-match

Print the one posting whose transaction description is closest to DESC,
in the style of the register command. Helps ledger-autosync detect
already-seen transactions when importing.

### rewrite

Print all transactions, adding custom postings to the matched ones.

### stats

Show some journal statistics.

`-o FILE --output-file=FILE`
:   write output to FILE. A file extension matching one of the above
    formats selects that format.

``` {.shell}
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

The stats command also supports `-o/--output-file` for controlling
[output destination](#output-destination).

### tags

List all the tag names used in the journal. With a TAGREGEX argument,
only tag names matching the regular expression (case insensitive) are
shown. With additional QUERY arguments, only transactions matching the
query are considered.

### test

Run built-in unit tests.

``` {.shell}
$ hledger test
Cases: 74  Tried: 74  Errors: 0  Failures: 0
```

This command runs hledger's built-in unit tests and displays a quick
report. With a regular expression argument, it selects only tests with
matching names. It's mainly used in development, but it's also nice to
be able to check your hledger executable for smoke at any time.

## ADD-ON COMMANDS

hledger also searches for external add-on commands, and will include
these in the commands list. These are programs or scripts in your PATH
whose name starts with `hledger-` and ends with a recognised file
extension (currently: no extension, `bat`,`com`,`exe`,
`hs`,`lhs`,`pl`,`py`,`rb`,`rkt`,`sh`).

Add-ons can be invoked like any hledger command, but there are a few
things to be aware of. Eg if the `hledger-web` add-on is installed,

-   `hledger -h web` shows hledger's help, while `hledger web -h` shows
    hledger-web's help.

-   Flags specific to the add-on must have a preceding `--` to hide them
    from hledger. So `hledger web --serve --port 9000` will be rejected;
    you must use `hledger web -- --serve --port 9000`.

-   You can always run add-ons directly if preferred:
    `hledger-web --serve --port 9000`.

Add-ons are a relatively easy way to add local features or experiment
with new ideas. They can be written in any language, but haskell scripts
have a big advantage: they can use the same hledger (and haskell)
library functions that built-in commands do, for command-line options,
journal parsing, reporting, etc.

Here are some hledger add-ons available:

### Official add-ons

These are maintained and released along with hledger.

#### api

[hledger-api](hledger-api.html) serves hledger data as a JSON web API.

#### ui

[hledger-ui](hledger-ui.html) provides an efficient curses-style
interface.

#### web

[hledger-web](hledger-web.html) provides a simple web interface.

### Third party add-ons

These are maintained separately, and usually updated shortly after a
hledger release.

#### diff

[hledger-diff](http://hackage.haskell.org/package/hledger-diff) shows
differences in an account's transactions between one journal file and
another.

#### iadd

[hledger-iadd](http://hackage.haskell.org/package/hledger-iadd) is a
curses-style, more interactive replacement for the [add
command](/hledger.html#add).

#### interest

[hledger-interest](http://hackage.haskell.org/package/hledger-interest)
generates interest transactions for an account according to various
schemes.

#### irr

[hledger-irr](http://hackage.haskell.org/package/hledger-irr) calculates
the internal rate of return of an investment account.

### Experimental add-ons

These are available in source form in the hledger repo's bin/ directory;
installing them is [pretty easy](/download.html#d). They may be less
mature and documented than built-in commands. Reading and tweaking these
is a good way to start making your own!

#### autosync

[hledger-autosync](https://github.com/simonmichael/hledger/blob/master/bin/hledger-autosync)
is a symbolic link for easily running
[ledger-autosync](https://pypi.python.org/pypi/ledger-autosync), if
installed. ledger-autosync does deduplicating conversion of OFX data and
some CSV formats, and can also download the data [if your bank offers
OFX Direct
Connect](http://wiki.gnucash.org/wiki/OFX_Direct_Connect_Bank_Settings).

#### budget

[hledger-budget.hs](https://github.com/simonmichael/hledger/blob/master/bin/hledger-budget.hs#L10)
adds more budget-tracking features to hledger.

#### chart

[hledger-chart.hs](https://github.com/simonmichael/hledger/blob/master/bin/hledger-chart.hs#L47)
is an old pie chart generator, in need of some love.

#### check

[hledger-check.hs](https://github.com/simonmichael/hledger/blob/master/bin/hledger-check.hs)
checks more powerful account balance assertions.

## ENVIRONMENT

**COLUMNS** The screen width used by the register command. Default: the
full terminal width.

**LEDGER\_FILE** The journal file path when not specified with `-f`.
Default: `~/.hledger.journal` (on windows, perhaps
`C:/Users/USER/.hledger.journal`).

## FILES

Reads data from one or more files in hledger journal, timeclock,
timedot, or CSV format specified with `-f`, or `$LEDGER_FILE`, or
`$HOME/.hledger.journal` (on windows, perhaps
`C:/Users/USER/.hledger.journal`).

## BUGS

The need to precede addon command options with `--` when invoked from
hledger is awkward.

When input data contains non-ascii characters, a suitable system locale
must be configured (or there will be an unhelpful error). Eg on POSIX,
set LANG to something other than C.

In a Microsoft Windows CMD window, non-ascii characters and colours are
not supported.

In a Cygwin/MSYS/Mintty window, the tab key is not supported in hledger
add.

Not all of Ledger's journal file syntax is supported. See [file format
differences](https://github.com/simonmichael/hledger/wiki/FAQ#file-formats).

On large data files, hledger is slower and uses more memory than Ledger.

## TROUBLESHOOTING

Here are some issues you might encounter when you run hledger (and
remember you can also seek help from the [IRC
channel](http://irc.hledger.org), [mail list](http://list.hledger.org)
or [bug tracker](http://bugs.hledger.org)):

**Successfully installed, but "No command 'hledger' found"**\
stack and cabal install binaries into a special directory, which should
be added to your PATH environment variable. Eg on unix-like systems,
that is \~/.local/bin and \~/.cabal/bin respectively.

**I set a custom LEDGER\_FILE, but hledger is still using the default
file**\
`LEDGER_FILE` should be a real environment variable, not just a shell
variable. The command `env | grep LEDGER_FILE` should show it. You may
need to use `export`. Here's an
[explanation](http://stackoverflow.com/a/7411509).

**"Illegal byte sequence" or "Invalid or incomplete multibyte or wide
character" errors**\
In order to handle non-ascii letters and symbols (like £), hledger needs
an appropriate locale. This is usually configured system-wide; you can
also configure it temporarily. The locale may need to be one that
supports UTF-8, if you built hledger with GHC \< 7.2 (or possibly
always, I'm not sure yet).

Here's an example of setting the locale temporarily, on ubuntu
gnu/linux:

``` {.shell}
$ file my.journal
my.journal: UTF-8 Unicode text                 # <- the file is UTF8-encoded
$ locale -a
C
en_US.utf8                             # <- a UTF8-aware locale is available
POSIX
$ LANG=en_US.utf8 hledger -f my.journal print   # <- use it for this command
```

Here's one way to set it permanently, there are probably better ways:

``` {.shell}
$ echo "export LANG=en_US.UTF-8" >>~/.bash_profile
$ bash --login
```

If we preferred to use eg `fr_FR.utf8`, we might have to install that
first:

``` {.shell}
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
