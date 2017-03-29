# OPTIONS

## General options

To see general usage help, including general options 
which are supported by most hledger commands, run `hledger -h`.
(Note -h and --help are different, like git.)

General help options:

_helpoptions_

General input options:

_inputoptions_

General reporting options:

_reportingoptions_

Note when multiple similar reporting options are provided, the last one takes precedence.
Eg `-p feb -p mar` is equivalent to `-p mar`.

Some of these can also be written as [queries](#queries).

## Command options

To see options for a particular command, including command-specific options, run: `hledger COMMAND -h`.

Command-specific options must be written after the command name, eg: `hledger print -x`.

Additionally, if the command is an [addon](#commands), 
you may need to put its options after a double-hyphen, eg: `hledger ui -- --watch`.
Or, you can run the addon executable directly: `hledger-ui --watch`.

## Command arguments

Most hledger commands accept arguments after the command name, 
which are often a [query](#queries), filtering the data in some way. 

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

There's more.. options and arguments get de-escaped when hledger 
is passing them to an addon executable. In this case you might need *triple*-escaping.
Eg: `hledger ui cur:'\\$'` or `hledger ui cur:\\\\$`.

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

------------------------------------------------- -----------------------------------------------------------------------------
`2009/1/1`, `2009/01/01`, `2009-1-1`, `2009.1.1`  simple dates, several separators allowed
`2009/1`, `2009`                                  same as above - a missing day or month defaults to 1
`1/1`, `january`, `jan`, `this year`              relative dates, meaning january 1 of the current year
`next year`                                       january 1 of next year
`this month`                                      the 1st of the current month
`this week`                                       the most recent monday
`last week`                                       the monday of the week before this one
`lastweek`                                        spaces are optional
`today`, `yesterday`, `tomorrow`
---

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

The following more complex report intervals are also supported:
`biweekly`, 
`bimonthly`,
`every N days|weeks|months|quarters|years`,
`every Nth day [of month]`,
`every Nth day of week`.

Examples:

------------------------------------------
`-p "bimonthly from 2008"`
`-p "every 2 weeks"`
`-p "every 5 days from 1/3"`
------------------------------------------

Show historical balances at end of 15th each month (N is exclusive end date):

`hledger balance -H -p "every 16th day"`

Group postings from start of wednesday to end of next tuesday (N is start date and exclusive end date):

`hledger register checking -p "every 3rd day of week"`   

## Depth limiting

With the `--depth N` option, commands like [account](#account), [balance](#balance)
and [register](#register) will show only the uppermost accounts in the account
tree, down to level N. Use this when you want a summary with less detail.

## Pivoting

Normally hledger sums amounts, and organizes them in a hierarchy, based on account name.
The `--pivot TAGNAME` option causes it to sum and organize hierarchy based on some other field instead.

TAGNAME is the full, case-insensitive name of a [tag](/journal.html#tags) you have defined,
or one of the built-in implicit tags (like `code` or `payee`).
As with account names, when tag values have `multiple:colon-separated:parts` hledger will build hierarchy,
displayed in tree-mode reports, summarisable with a depth limit, and so on.

`--pivot` affects all reports, and is one of those options you can write before the command name if you wish.
You can think of hledger transforming the journal before any other processing,
replacing every posting's account name with the value of the specified tag on that posting,
inheriting it from the transaction or using a blank value if it's not present.

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

- To match a regular expression metacharacter like `$` as a literal
character, prepend a backslash. Eg to search for amounts with the
dollar sign in hledger-web, write `cur:\$`.

- On the command line, some metacharacters like `$` have a special
meaning to the shell and so must be escaped a second time, with single
or double quotes or another backslash.  Eg, to match amounts with the
dollar sign from the command line, write `cur:'\$'` or `cur:\\$`.


