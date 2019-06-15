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
For more, see [Save frequently used options](Save-frequently-used-options.html).

## Special characters in arguments and queries

In shell command lines, option and argument values which contain "problematic" characters,
ie spaces, 
and also characters significant to your shell such as `<`, `>`, `(`, `)`, `|` and `$`,
should be escaped by enclosing them in quotes or by writing backslashes before the characters.
Eg:

`hledger register -p 'last year' "accounts receivable (receivable|payable)" amt:\>100`.

### More escaping

Characters significant both to the shell and in [regular expressions](#regular-expressions) 
may need one extra level of escaping. These include parentheses, the pipe symbol and the dollar sign.
Eg, to match the dollar symbol, bash users should do:

`hledger balance cur:'\$'`

or:

`hledger balance cur:\\$`

### Even more escaping

When hledger runs an addon executable (eg you type `hledger ui`, hledger runs `hledger-ui`), 
it de-escapes command-line options and arguments once, so you might need to *triple*-escape.
Eg in bash, running the ui command and matching the dollar sign, it's: 

`hledger ui cur:'\\$'`

or:

`hledger ui cur:\\\\$`

If you asked why *four* slashes above, this may help:

----------------- --------
unescaped:        `$`
escaped:          `\$`
double-escaped:   `\\$`
triple-escaped:   `\\\\$`
----------------- --------

(The number of backslashes in fish shell is left as an exercise for the reader.)

You can always avoid the extra escaping for addons by running the addon directly:

`hledger-ui cur:\\$`

### Less escaping

Inside an [argument file](#argument-expansion),
or in the search field of hledger-ui or hledger-web, 
or at a GHCI prompt, 
you need one less level of escaping than at the command line.
And backslashes may work better than quotes. 
Eg:

`ghci> :main balance cur:\$`

## Command line tips

If in doubt, keep things simple:

- write options after the command (`hledger CMD -OPTIONS ARGS`)
- run add-on executables directly (`hledger-ui -OPTIONS ARGS`)
- enclose problematic args in single quotes
- if needed, also add a backslash to escape regexp metacharacters

To find out exactly how a command line is being parsed, add `--debug=2` to troubleshoot.

## Unicode characters

hledger is expected to handle non-ascii characters correctly:

- they should be parsed correctly in input files and on the command
line, by all hledger tools (add, iadd, hledger-web's search/add/edit
forms, etc.)

- they should be displayed correctly by all hledger tools,
  and on-screen alignment should be preserved.

This requires a well-configured environment. Here are some tips:

- A system locale must be configured, and it must be one that can
  decode the characters being used. 
  In bash, you can set a locale like this: `export LANG=en_US.UTF-8`. 
  There are some more details in [Troubleshooting](#troubleshooting).
  This step is essential - without it, hledger will quit on encountering
  a non-ascii character (as with all GHC-compiled programs).

- your terminal software (eg Terminal.app, iTerm, CMD.exe, xterm..)  must support unicode

- the terminal must be using a font which includes the required unicode glyphs

- the terminal should be configured to display wide characters as double width (for report alignment)

- on Windows, for best results you should run hledger in the same kind of environment in which it was built.
  Eg hledger built in the standard CMD.EXE environment (like the binaries on our download page)
  might show display problems when run in a cygwin or msys terminal, and vice versa.
  (See eg [#961](https://github.com/simonmichael/hledger/issues/961#issuecomment-471229644)).



## Input files

hledger reads transactions from a data file (and the add command writes to it).
By default this file is `$HOME/.hledger.journal` 
(or on Windows, something like `C:/Users/USER/.hledger.journal`).
You can override this with the `$LEDGER_FILE` environment variable:
```shell
$ setenv LEDGER_FILE ~/finance/2016.journal
$ hledger stats
```
or with the `-f/--file` option:
```shell
$ hledger -f /some/file stats
```

The file name `-` (hyphen) means standard input:
```shell
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
```shell
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
```shell
$ hledger balance --pivot member acct:.
              -2 EUR  John Doe
--------------------
              -2 EUR
```

## Valuation

### -B: Cost

The `-B/--cost` flag converts amounts to their cost (or selling price) at transaction time, 
if they have a [transaction price](/journal.html#transaction-prices) specified.
This flag is equivalent to `--value=cost`, described below.

### -V: Market value

The `-V/--market` flag converts reported amounts to their market value in a default valuation commodity,
using the [historical market prices](journal.html#market-prices) in effect on a default valuation date.

For single period reports, the valuation date is today.
For [multiperiod reports](#report-intervals), it is the last day of each subperiod.

The valuation commodity will be the one referenced in the latest
applicable market price dated on or before the valuation date.
If most of your P declarations lead to a single home currency, this will usually be what you want.

Unlike the similar flag in Ledger, it does not infer market prices
from [transaction prices](/manual.html#transaction-prices). In
hledger, -B uses transaction prices, -V and -X use market prices.

It is equivalent to `--value=now` or `--value=end`.

Here's a quick example:

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
```shell
$ hledger -f t.j bal -N euros
                €100  assets:euros
```
What are they worth at end of nov 3 ?
```shell
$ hledger -f t.j bal -N euros -V -e 2016/11/4
             $110.00  assets:euros
```
What are they worth after 2016/12/21 ? (no report end date specified, defaults to today)
```shell
$ hledger -f t.j bal -N euros -V
             $103.00  assets:euros
```

### -X: Market value in specified commodity

The `-X/--exchange` option is like `-V/--market` except it takes a
commodity symbol argument, so that you can select a different target commodity.
It is similar to the same option in Ledger, with the same caveat mentioned for `-V`/`--value` above.
It is equivalent to `--value=now,COMM` or `--value=end,COMM`; for more details, read on.

### --value

*(experimental, added 201905)*

`-B`, `-V` and `-X` are special cases of the more general `--value` option:

     --value=TYPE[,COMM]  TYPE is cost, end, now or YYYY-MM-DD.
                          COMM is an optional commodity symbol.
                          Shows amounts converted to:
                          - cost commodity using transaction prices (then optionally to COMM using market prices at period end(s))
                          - default valuation commodity (or COMM) using market prices at period end(s)
                          - default valuation commodity (or COMM) using current market prices
                          - default valuation commodity (or COMM) using market prices at some date

#### Valuation type

TYPE is one of these keywords, or their first letter, or a date (which
must be 8 digits with `-` or `/` or `.` separators):

`--value=cost`
: Convert amounts to cost, using the prices recorded in transactions.
  `-B`/`--cost` is equivalent to this.

`--value=end`
: Convert amounts to their value in default valuation commodity using market prices 
  on the last day of the report period (or of each subperiod in a multiperiod report).
  When no report period is specified, uses the journal's last transaction date.

`--value=now`
: Convert amounts to their value in default valuation commodity using current market prices 
  (as of when report is generated). `-V`/`--market` is equivalent to this.

`--value=YYYY-MM-DD`
: Convert amounts to their value in default valuation commodity using market prices 
  on this date.  Eg `--value=2019-04-25`.

#### Valuation commodity

The default valuation commodity is the commodity mentioned in the most
recent applicable market price declaration. When all your price
declarations lead to a single home currency, this will usually do what
you want.

To select a different valuation commodity: write the commodity symbol
after the valuation type, separated by a comma (eg: **`--value=now,EUR`**). 
This will use, in this preferred order:

- declared prices (from source commodity to valuation commodity)
- reverse prices (declared prices from valuation to source commodity, inverted)
- indirect prices (prices calculated from the shortest chain of declared or reverse prices from source to valuation commodity).

#### --value examples

Here are the effects of `--value` as seen with `print`:

```journal
P 2000-01-01 A  1 B
P 2000-02-01 A  2 B
P 2000-03-01 A  3 B
P 2000-04-01 A  4 B

2000-01-01
  (a)      1 A @ 5 B

2000-02-01
  (a)      1 A @ 6 B

2000-03-01
  (a)      1 A @ 7 B
```

Show the cost of each posting:
```shell
$ hledger -f- print --value=cost
2000/01/01
    (a)             5 B

2000/02/01
    (a)             6 B

2000/03/01
    (a)             7 B

```

Show the value as of the last day of the report period (2000-02-29):
```shell
$ hledger -f- print --value=end date:2000/01-2000/03
2000-01-01
    (a)             2 B

2000-02-01
    (a)             2 B

```

With no report period specified, that shows the value as of the last day of the journal (2000-03-01):
```shell
$ hledger -f- print --value=end
2000/01/01
    (a)             3 B

2000/02/01
    (a)             3 B

2000/03/01
    (a)             3 B

```

Show the current value (the 2000-04-01 price is still in effect today):
```shell
$ hledger -f- print --value=now
2000-01-01
    (a)             4 B

2000-02-01
    (a)             4 B

2000-03-01
    (a)             4 B

```

Show the value on 2000/01/15:
```shell
$ hledger -f- print --value=2000-01-15
2000/01/01
    (a)             1 B

2000/02/01
    (a)             1 B

2000/03/01
    (a)             1 B

```

You may need to explicitly set a commodity's display style, when reverse prices are used.
Eg this output might be surprising:
```
P 2000-01-01 A 2B

2000-01-01
  a  1B
  b
```
```
$ hledger print -x -X A
2000/01/01
    a               0
    b               0

```
Explanation: because there's no amount or commodity directive specifying a display style
for A, 0.5A gets the default style, which shows no decimal digits. Because the displayed
amount looks like zero, the commodity symbol and minus sign are not displayed either.
Adding a commodity directive sets a more useful display style for A:
```
P 2000-01-01 A 2B
commodity 0.00A

2000-01-01
  a  1B
  b
```
```
$ hledger print -X A
2000/01/01
    a           0.50A
    b          -0.50A

```

#### Effect of --value on reports

Below is how `--value` affects each of hledger's reports, currently.
You're not expected to remember all this, but when troubleshooting a report, look here.
If you find problems - useless reports, misbehaving reports, or error
messages being printed - please report them (with reproducible examples) eg at
[#329](https://github.com/simonmichael/hledger/issues/329).

| Report type                      | `--value` `cost`&nbsp;              | `--value` `end`&nbsp;                                               | `--value` `DATE`/`now`&nbsp;                     |
|----------------------------------|-------------------------------------|---------------------------------------------------------------------|--------------------------------------------------|
| <br>**print**                    |                                     |                                                                     |                                                  |
| posting amounts                  | cost, as recorded in transaction    | market value at report end                                          | market value at DATE                             |
| balance assertions/assignments   | show unvalued                       | show unvalued                                                       | show unvalued                                    |
| <br>**register**                 |                                     |                                                                     |                                                  |
| starting balance with -H         | cost of starting balance            | market value at day before report start                             | market value at DATE                             |
| posting amounts                  | cost                                | market value at report end                                          | market value at DATE                             |
| posting amounts, multiperiod     | summarised cost                     | market value each summary posting at period end                     | market value each summary posting at DATE        |
| running total/average            | sum/average of the displayed values | sum/average of the displayed values                                 | sum/average of the displayed values              |
| <br>**balance (bs, cf, is..)**   |                                     |                                                                     |                                                  |
| starting balances with -H        | costs of starting balances          | market value at day before report start of sum of previous postings | market value at DATE of sum of previous postings |
| balances, simple balance report  | summed costs                        | market value at period end of sum of postings                       | market value at DATE of sum of postings          |
| balances, multiperiod report     | summed costs                        | market value at period end of sum of postings                       | market value at DATE of sum of postings          |
| budget amounts with --budget     | costs of budget amounts             | budget-setting periodic txns are valued at period end               | budget-setting periodic txns are valued at DATE  |
| column/row/grand totals/averages | sum/average of the displayed values | market value at period end of sum/average of postings               | market value at DATE of sum/average of postings  |

### Combining -B, -V, -X, --value

The rightmost of these flags wins.

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
- [account alias](#rewriting-accounts) directives and options: `alias /REGEX/ = REPLACEMENT`, `--alias /REGEX/=REPLACEMENT`

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


