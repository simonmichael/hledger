# OPTIONS

To see general usage and the command list: `hledger -h` or just `hledger`.
To see usage for a specific command: `hledger COMMAND -h`.

hledger has several kinds of options:

- General options are always available and can appear anywhere on the command line.
  `hledger -h` shows these. Eg: `hledger --version`.

- Common reporting options are available with most commands. 
  These and all other non-general options must be written after COMMAND.
  `hledger COMMAND -h` shows these. Eg: `hledger register --cleared`.

- Command-specific options are also provided by some commands. 
  `hledger COMMAND -h` shows these too. Eg: `hledger register --average`.

- Some hledger commands come from separate [add-on executables](#commands),
  which have their own options. 
  `hledger COMMAND -h` shows these, as usual. 
  Such options, if not also supported by hledger, 
  should be written following a double hyphen argument (`--`)
  so that hledger's option parser does not complain.
  Eg: `hledger ui -- --register=checking`.
  Or, you can just run the add-on directly:
  `hledger-ui --register=checking`.

Command arguments may also follow the command name.
In most cases these specify a [query](#queries) which filters the data. 
Command options and arguments can be intermixed.

Option and argument values containing problematic characters
should be escaped with double quotes, backslashes, or (best) single quotes.
This means spaces, but also characters which are significant to your 
command shell, such as less-than/greater-than.
Eg: `hledger register -p 'last year' "accounts receivable (receivable|payable)" amt:\>100`.

Characters which are significant to the shell and also in 
[regular expressions](#regular-expressions), like parentheses, 
the pipe symbol and the dollar sign, must sometimes be double-escaped.
Eg, to match the dollar symbol: `hledger balance cur:'\$'` or 
`hledger balance cur:\\$`.

There's more.. options and arguments being passed by hledger to an
add-on executable get de-escaped once in the process. In this case you 
might need triple-escaping.
Eg: `hledger ui cur:'\\$'` or `hledger ui cur:\\\\$`.

If in doubt, keep things simple:

- write options after the command
- enclose problematic args in single quotes
- if needed, also add a backslash to escape regexp metacharacters
- run add-on executables directly

If you're really curious, add `--debug=2` for troubleshooting.


## General options

Always available, can be written before or after COMMAND. 

_generaloptions_

## Reporting options

Common reporting options, must be written after COMMAND.

_reportingoptions_

If a reporting option occurs more than once on the command line, 
the last one takes precedence.
Eg -p jan -p feb is equivalent to -p feb.

## Input files

hledger reads transactions from a data file (and the add command writes to it).
Usually this is in hledger's journal format, 
but it can also be one of the other supported file types, such as
timeclock, 
timedot, 
CSV, 
or a C++ Ledger journal (partial support).

By default this file is `$HOME/.hledger.journal` 
(or on Windows, something like `C:/Users/USER/.hledger.journal`).
You can override this with the `$LEDGER_FILE` environment variable:
```bash
$ setenv LEDGER_FILE ~/finance/2016.journal
$ hledger stats
```
or with the `-f/--file` option:
```bash
$ hledger -f some/file.ext stats
```

hledger tries to identify the file format based on the file extension, 
as follows:

| File extension:                           | Use format:
|-------------------------------------------|----------------
| `.journal`, `.j`, `.hledger`, `.ledger`   | journal
| `.timeclock`                              | timeclock
| `.timedot`                                | timedot
| `.csv`                                    | CSV

If the file name has some other extension, or none, 
hledger tries each of these formats in turn.
(Plus one more: the experimental "ledger" format, an alternate
 parser for C++ Ledger journals, which we try only as a last resort
 as it's new and hledger's journal parser works better for now.)

The file name `-` (hyphen) means standard input, as usual:
```bash
$ cat some.journal | hledger -f-
```

You can specify multiple `-f` options, to read multiple files as one big journal.
Directives in one file will not affect subsequent files in this case (if you need that, 
use the [include directive](#including-other-files) instead).

## Depth limiting

With the `--depth N` option, commands like [account](#account), [balance](#balance)
and [register](#register) will show only the uppermost accounts in the account
tree, down to level N. Use this when you want a summary with less detail.

## Smart dates

hledger's user interfaces accept a flexible "smart date" syntax (unlike dates in the journal file). Smart dates allow some english words, can be relative to today's date, and can have less-significant date parts omitted (defaulting to 1).

Examples:

------------------------------------------------- ------------------------------------------------------
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

## Report intervals

A report interval can be specified so that commands like
[register](#register), [balance](#balance) and [activity](#activity) will divide their
reports into multiple subperiods.  The basic intervals can be
selected with one of `-D/--daily`, `-W/--weekly`, `-M/--monthly`,
`-Q/--quarterly`, or `-Y/--yearly`.  More complex intervals may be
specified with a period expression.

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


