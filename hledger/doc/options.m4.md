# OPTIONS

To see general help and the command list: `hledger --help` or `hledger`

To see all options available with a command: `hledger COMMAND --help`

Except for the General options below, options must be written after
COMMAND, not before it.

Also, when invoking external add-on commands, their options must be
written after a double hyphen. (Or, you can invoke the external command
directly.) Eg:
_shellbold_({{
$ hledger ui -- --register cash
$ hledger-ui --register cash
}})

Options and command arguments can be intermixed. Arguments are usually
interpreted as a search query which filters the data, see QUERIES.

There are three kinds of options.
General options are always available and can appear anywhere in the command line:

`-h --help`
: show general help or (after command) command help

`--version`
: show version information

`-f FILE --file=FILE`
: use a different input file. For stdin, use -

`--rules-file=RULESFILE`
: Conversion rules file to use when reading CSV (default: FILE.rules)

`--alias=OLD=NEW`
: display accounts named OLD as NEW

`--ignore-assertions`
: ignore any failing balance assertions in the journal

`--debug=N`
: : show debug output if N is 1-9 (default: 0)

Common reporting options are supported by most commands where applicable,
and individual commands may provide additional command-specific options.
Both of these must be written after the command name.

`-b --begin=DATE              `
: include postings/txns on or after this date

`-e --end=DATE                `
: include postings/txns before this date

`-D --daily                   `
: multiperiod/multicolumn report by day

`-W --weekly                  `
: multiperiod/multicolumn report by week

`-M --monthly                 `
: multiperiod/multicolumn report by month

`-Q --quarterly               `
: multiperiod/multicolumn report by quarter

`-Y --yearly                  `
: multiperiod/multicolumn report by year

`-p --period=PERIODEXP        `
: set start date, end date, and/or reporting interval all at once (overrides the flags above)

`--date2 --aux-date`
: use postings/txns' secondary dates instead

`-C --cleared                 `
: include only cleared postings/txns

`--pending`
: include only pending postings/txns

`-U --uncleared               `
: include only uncleared (and pending) postings/txns

`-R --real                    `
: include only non-virtual postings

`--depth=N`
: hide accounts/postings deeper than N

`-E --empty                   `
: show empty/zero things which are normally omitted

`-B --cost                    `
: show amounts in their cost price's commodity

## Multiple files

One may specify the `--file FILE` option multiple times. This is equivalent to
concatenating the files to standard input and passing `--file -`, except that
the add command functions normally and adds entries to the first specified file.

## Repeated options

Otherwise, if a reporting option is repeated, the last one takes precedence. Eg -p jan -p
feb is equivalent to -p feb.

## Depth limiting

With the `--depth N` option, commands like [account](#account), [balance](#balance)
and [register](#register) will show only the uppermost accounts in the account
tree, down to level N. Use this when you want a summary with less detail.

## Smart dates

hledger's user interfaces accept a flexible "smart date" syntax (unlike dates in the journal file). Smart dates allow some english words, can be relative to today's date, and can have less-significant date parts omitted (defaulting to 1).

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

## Reporting interval

A reporting interval can be specified so that commands like
[register](#register), [balance](#balance) and [activity](#activity) will divide their
reports into multiple report periods.  The basic intervals can be
selected with one of `-D/--daily`, `-W/--weekly`, `-M/--monthly`,
`-Q/--quarterly`, or `-Y/--yearly`.  More complex intervals may be
specified with a period expression.

## Period expressions

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

## Regular Expressions

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


