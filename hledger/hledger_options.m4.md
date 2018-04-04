# OPTIONS

## General options

To see general usage help, including general options 
which are supported by most hledger commands, run `hledger -h`.

General help options:

_helpoptions_

General input options:

_inputoptions_

General reporting options:

_reportingoptions_

## Command options

To see options for a particular command, including command-specific options, run: `hledger COMMAND -h`.

Command-specific options must be written after the command name, eg: `hledger print -x`.

Additionally, if the command is an [addon](#commands), 
you may need to put its options after a double-hyphen, eg: `hledger ui -- --watch`.
Or, you can run the addon executable directly: `hledger-ui --watch`.

## Command arguments

Most hledger commands accept arguments after the command name, 
which are often a [query](#queries), filtering the data in some way. 

## Argument files

You can save a set of command line options/arguments in a file, one per line,
and then reuse them by writing `@FILENAME` in a command line.
To prevent this expansion of `@`-arguments, precede them with a `--` argument.
For more, see [Save frequently used options](argfiles.html).

## Special characters

Option and argument values which contain problematic characters
should be escaped with double quotes, backslashes, or (best) single quotes.
Problematic characters means spaces, and also characters which are significant to your 
command shell, such as less-than/greater-than.
Eg: `hledger register -p 'last year' "accounts receivable (receivable|payable)" amt:\>100`.

Characters which are significant both to the shell and in 
[regular expressions](#regular-expressions) sometimes need to be double-escaped.
These include parentheses, the pipe symbol and the dollar sign.
Eg, to match the dollar symbol, bash users should do: `hledger balance cur:'\$'` or 
`hledger balance cur:\\$`.

When hledger is invoking an addon executable (like hledger-ui), 
options and arguments get de-escaped once more, 
so you might need *triple*-escaping.
Eg: `hledger ui cur:'\\$'` or `hledger ui cur:\\\\$` in bash.
(The number of backslashes in fish shell is left as an exercise for the reader.)

Inside a file used for [argument expansion](#argument-expansion), one less level of escaping is enough.
(And in this case, backslashes seem to work better than quotes. Eg: `cur:\$`).

If in doubt, keep things simple:

- run add-on executables directly
- write options after the command
- enclose problematic args in single quotes
- if needed, also add a backslash to escape regexp metacharacters

If you're really stumped, add `--debug=2` to troubleshoot.

## Input files

hledger reads transactions from a data file (and the add command writes to it).
By default this file is `$HOME/.hledger.journal` 
(or on Windows, something like `C:/Users/USER/.hledger.journal`).
You can override this with the `$LEDGER_FILE` environment variable:
```bash
$ setenv LEDGER_FILE ~/finance/2016.journal
$ hledger stats
```
or with the `-f/--file` option:
```bash
$ hledger -f /some/file stats
```

The file name `-` (hyphen) means standard input:
```bash
$ cat some.journal | hledger -f-
```

Usually the data file is in hledger's journal format, 
but it can also be one of several other formats, listed below.
hledger detects the format automatically based on the file extension,
or if that is not recognised, by trying each built-in "reader" in turn:

| Reader:         | Reads:                                                | Used for file extensions:                          
|-----------------|-------------------------------------------------------|-------------------------------------------
| `journal`       | hledger's journal format, also some Ledger journals   | `.journal` `.j` `.hledger` `.ledger`   
| `timeclock`     | timeclock files (precise time logging)                | `.timeclock`                              
| `timedot`       | timedot files (approximate time logging)              | `.timedot`                                
| `csv`           | comma-separated values (data interchange)             | `.csv`                                    

If needed (eg to ensure correct error messages when a file has the "wrong" extension), 
you can force a specific reader/format by prepending it to the file path with a colon. 
Examples:
```bash
$ hledger -f csv:/some/csv-file.dat stats
$ echo 'i 2009/13/1 08:00:00' | hledger print -ftimeclock:-
```

You can also specify multiple `-f` options, to read multiple files as one big journal.
There are some limitations with this:

- directives in one file will not affect the other files
- [balance assertions](/journal.html#balance-assertions) will not see any account balances from previous files

If you need those, either use the [include directive](/journal.html#including-other-files),
or concatenate the files, eg: `cat a.journal b.journal | hledger -f- CMD`.

## Smart dates

hledger's user interfaces accept a flexible "smart date" syntax (unlike dates in the journal file). 
Smart dates allow some english words, can be relative to today's date, 
and can have less-significant date parts omitted (defaulting to 1).

Examples:

--------------------------------------------- -----------------------------------------------------------------------------
`2004/10/1`, `2004-01-01`, `2004.9.1`         exact date, several separators allowed. Year is 4+ digits, month is 1-12, day is 1-31
`2004`                                        start of year 
`2004/10`                                     start of month 
`10/1`                                        month and day in current year
`21`                                          day in current month
`october, oct`                                start of month in current year
`yesterday, today, tomorrow`                  -1, 0, 1 days from today
`last/this/next day/week/month/quarter/year`  -1, 0, 1 periods from the current period
`20181201`                                    8 digit YYYYMMDD with valid year month and day
`201812`                                      6 digit YYYYMM with valid year and month
--------------------------------------------- -----------------------------------------------------------------------------

Counterexamples - malformed digit sequences might give surprising results:

--------------------------------------------- -----------------------------------------------------------------------------
`201813`                                      6 digits with an invalid month is parsed as start of 6-digit year
`20181301`                                    8 digits with an invalid month is parsed as start of 8-digit year
`20181232`                                    8 digits with an invalid day gives an error
`201801012`                                   9+ digits beginning with a valid YYYYMMDD gives an error
--------------------------------------------- -----------------------------------------------------------------------------

## Report start & end date

Most hledger reports show the full span of time represented by the journal data, by default.
So, the effective report start and end dates will be the earliest and latest transaction or posting dates found in the journal.

Often you will want to see a shorter time span, such as the current month.
You can specify a start and/or end date using 
[`-b/--begin`](#reporting-options), 
[`-e/--end`](#reporting-options), 
[`-p/--period`](#period-expressions) 
or a [`date:` query](#queries) (described below).
All of these accept the [smart date](#smart-dates) syntax.
One important thing to be aware of when specifying end dates: as in Ledger, end dates are exclusive, 
so you need to write the date *after* the last day you want to include.

Examples:

-----------------------------------  -------------------------------------------------------------------------------------------
`-b 2016/3/17`                       begin on St. Patrick's day 2016
`-e 12/1`                            end at the start of december 1st of the current year (11/30 will be the last date included)
`-b thismonth`                       all transactions on or after the 1st of the current month
`-p thismonth`                       all transactions in the current month  
`date:2016/3/17-`                    the above written as queries instead
`date:-12/1`   
`date:thismonth-`
`date:thismonth`
---

##  Report intervals

A report interval can be specified so that commands like
[register](#register), [balance](#balance) and [activity](#activity) will divide their
reports into multiple subperiods.  The basic intervals can be
selected with one of `-D/--daily`, `-W/--weekly`, `-M/--monthly`,
`-Q/--quarterly`, or `-Y/--yearly`.  More complex intervals may be
specified with a [period expression](#period-expressions). 
Report intervals can not be specified with a [query](#queries), currently. 

## Period expressions

The `-p/--period` option accepts period expressions, a shorthand way
of expressing a start date, end date, and/or report interval all at
once.

Here's a basic period expression specifying the first quarter of 2009. Note,
hledger always treats start dates as inclusive and end dates as exclusive:

`-p "from 2009/1/1 to 2009/4/1"`

Keywords like "from" and "to" are optional, and so are the spaces, as long
as you don't run two dates together. "to" can also be written as "-".
These are equivalent to the above:

------------------------------
`-p "2009/1/1 2009/4/1"`
`-p2009/1/1to2009/4/1`
`-p2009/1/1-2009/4/1`
------------------------------

Dates are [smart dates](#smart-dates), so if the current year is 2009, the
above can also be written as:

------------------------------
`-p "1/1 4/1"`
`-p "january-apr"`
`-p "this year to 4/1"`
------------------------------

If you specify only one date, the missing start or end date will be the
earliest or latest transaction in your journal:

---------------------------- ---------------------------------
`-p "from 2009/1/1"`         everything after january 1, 2009
`-p "from 2009/1"`           the same
`-p "from 2009"`             the same
`-p "to 2009"`               everything before january 1, 2009
---------------------------- ---------------------------------

A single date with no "from" or "to" defines both the start and end date
like so:

--------------------- ------------------------------------------------------
`-p "2009"`           the year 2009;    equivalent to "2009/1/1 to 2010/1/1"
`-p "2009/1"`         the month of jan; equivalent to "2009/1/1 to 2009/2/1"
`-p "2009/1/1"`       just that day;    equivalent to "2009/1/1 to 2009/1/2"
--------------------- ------------------------------------------------------

The argument of `-p` can also begin with, or be, a [report interval](#report-intervals) expression.
The basic report intervals are `daily`, `weekly`, `monthly`, `quarterly`, or `yearly`, 
which have the same effect as the `-D`,`-W`,`-M`,`-Q`, or `-Y` flags.
Between report interval and start/end dates (if any), the word `in` is optional.
Examples: 

------------------------------------------
`-p "weekly from 2009/1/1 to 2009/4/1"`
`-p "monthly in 2008"`                          
`-p "quarterly"`
------------------------------------------

Note that `weekly`, `monthly`, `quarterly` and `yearly` intervals will
always start on the first day on week, month, quarter or year
accordingly, and will end on the last day of same period, even if
associated period expression specifies different explicit start and end date.

For example:

------------------------------------------
`-p "weekly from 2009/1/1 to 2009/4/1"` -- starts on 2008/12/29, closest preceeding Monday
`-p "monthly in 2008/11/25"` -- starts on 2018/11/01                  
`-p "quarterly from 2009-05-05 to 2009-06-01"` - starts on 2009/04/01, ends on 2009/06/30, which are first and last days of Q2 2009 
`-p "yearly from 2009-12-29"` - starts on 2009/01/01, first day of 2009
------------------------------------------

The following more complex report intervals are also supported:
`biweekly`, 
`bimonthly`,
`every day|week|month|quarter|year`,
`every N days|weeks|months|quarters|years`.


All of these will start on the first day of the requested period and end on the last one, as described above.

Examples:

------------------------------------------
`-p "bimonthly from 2008"` -- periods will have boundaries on 2008/01/01, 2008/03/01, ...
`-p "every 2 weeks"`  -- starts on closest preceeding Monday
`-p "every 5 month from 2009/03"` -- periods will have boundaries on 2009/03/01, 2009/08/01, ...
------------------------------------------

If you want intervals that start on arbitrary day of your choosing and span a week, month or year, you need to use any of the following:

`every Nth day of week`,
`every <weekday>`,
`every Nth day [of month]`,
`every Nth weekday [of month]`,
`every MM/DD [of year]`,
`every Nth MMM [of year]`,
`every MMM Nth [of year]`.

Examples:

------------------------------------------
`-p "every 2nd day of week"` -- periods will go from Tue to Tue
`-p "every Tue"` -- same
`-p "every 15th day"` -- period boundaries will be on 15th of each month
`-p "every 2nd Monday"` -- period boundaries will be on second Monday of each month
`-p "every 11/05"` -- yearly periods with boundaries on 5th of Nov
`-p "every 5th Nov"` -- same
`-p "every Nov 5th"` -- same
------------------------------------------


Show historical balances at end of 15th each month (N is exclusive end date):

`hledger balance -H -p "every 16th day"`

Group postings from start of wednesday to end of next tuesday (N is start date and exclusive end date):

`hledger register checking -p "every 3rd day of week"`   

## Depth limiting

With the `--depth N` option (short form: `-N`), commands like [account](#account), [balance](#balance)
and [register](#register) will show only the uppermost accounts in the account
tree, down to level N. Use this when you want a summary with less detail.
This flag has the same effect as a `depth:` query argument
(so `-2`, `--depth=2` or `depth:2` are basically equivalent).   

## Pivoting

Normally hledger sums amounts, and organizes them in a hierarchy, based on account name.
The `--pivot FIELD` option causes it to sum and organize hierarchy based on the value of some other field instead.
FIELD can be:
`code`, `description`, `payee`, `note`, 
or the full name (case insensitive) of any [tag](/journal.html#tags).
As with account names, values containing `colon:separated:parts` will be displayed hierarchically in reports.

`--pivot` is a general option affecting all reports; you can think of hledger transforming 
the journal before any other processing, replacing every posting's account name with 
the value of the specified field on that posting, inheriting it from the transaction 
or using a blank value if it's not present.

An example:

```journal
2016/02/16 Member Fee Payment
    assets:bank account                    2 EUR
    income:member fees                    -2 EUR  ; member: John Doe
```
Normal balance report showing account names:
```shell
$ hledger balance
               2 EUR  assets:bank account
              -2 EUR  income:member fees
--------------------
                   0
```
Pivoted balance report, using member: tag values instead:
```shell
$ hledger balance --pivot member
               2 EUR
              -2 EUR  John Doe
--------------------
                   0
```
One way to show only amounts with a member: value (using a [query](#queries), described below):
```shell
$ hledger balance --pivot member tag:member=.
              -2 EUR  John Doe
--------------------
              -2 EUR
```
Another way (the acct: query matches against the pivoted "account name"):
```
$ hledger balance --pivot member acct:.
              -2 EUR  John Doe
--------------------
              -2 EUR
```

## Cost

The `-B/--cost` flag converts amounts to their cost at transaction time, 
if they have a [transaction price](/journal.html#transaction-prices) specified.

## Market value

The `-V/--value` flag converts reported amounts to their current market value.
Specifically, when there is a [market price](journal.html#market-prices) (P directive) for the amount's commodity, 
dated on or before today's date (or the [report end date](#report-start-end-date) if specified),
the amount will be converted to the price's commodity.

When there are multiple applicable P directives, -V chooses the most 
recent one, or in case of equal dates, the last-parsed one. 

For example:

```journal
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
```
$ hledger -f t.j bal euros
                €100  assets:euros
```
What are they worth on nov 3 ? (no report end date specified, defaults to the last date in the journal)
```
$ hledger -f t.j bal euros -V
             $110.00  assets:euros
```
What are they worth on dec 21 ?
```
$ hledger -f t.j bal euros -V -e 2016/12/21
             $103.00  assets:euros
```

Currently, hledger's -V only uses market prices recorded with P directives,
not [transaction prices](journal.html#transaction-prices) (unlike Ledger).

## Combining -B and -V

Using -B/--cost and -V/--value together is currently allowed, but the
results are probably not meaningful. Let us know if you find a use for this. 

## Output destination

Some commands (print, register, stats, the balance commands) 
can write their output to a destination other than the console. 
This is controlled by the `-o/--output-file` option.

```shell
$ hledger balance -o -     # write to stdout (the default)
$ hledger balance -o FILE  # write to FILE
```

## Output format

Some commands can write their output in other formats.
Eg print and register can output CSV, and the balance commands can output CSV or HTML.
This is controlled by the `-O/--output-format` option, or by specifying a `.csv` or `.html` file extension with `-o/--output-file`.

```shell
$ hledger balance -O csv       # write CSV to stdout
$ hledger balance -o FILE.csv  # write CSV to FILE.csv
```

## Regular expressions

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

- In queries, to match a regular expression metacharacter like `$` 
as a literal character, prepend a backslash. Eg to search for amounts with the
dollar sign in hledger-web, write `cur:\$`.

- On the command line, some metacharacters like `$` have a special
meaning to the shell and so must be escaped at least once more.
See [Special characters](#special-characters). 


