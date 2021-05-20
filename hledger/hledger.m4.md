% hledger(1)
% _author_
% _monthyear_

m4_dnl Quick hledger docs editing intro:
m4_dnl  .m4.md are hledger docs source, processed with m4 to generate markdown.
m4_dnl  Lines beginning with m4_dnl are comments.
m4_dnl  Words enclosed in underscores are macros, defined in doc/common.m4.
m4_dnl  Macro arguments are enclosed in (). Text literals are enclosed in {{}}.
m4_dnl  Macros may depend on command line flags, configured in Shake.hs.
m4_dnl  In Emacs:
m4_dnl   markdown-mode S-TAB cycles visibility, TAB toggles one section.
m4_dnl   C-x n s on a heading narrows to that section (C-x n w to widen again).

m4_dnl Some common markdown links.
m4_dnl These are also usable in hledger/Hledger/Cli/Commands/*.md.
m4_dnl Some are defined there also - don't remove, they are needed there for Shake cmdhelp eg.
m4_dnl Duplicate definitions won't give warnings as long as the target is identical.
m4_dnl Be wary of pandoc/mdbook handling [shortcut] link syntax differently ?

[add-on commands]:     #add-on-commands
[balance assertions]:  #balance-assertions
[balancesheet]:        #balancesheet
[balancesheetequity]:  #balancesheetequity
[cashflow]:            #cashflow
[commands]:            #commands
[common tasks]:        #common-tasks
[csv]:                 #csv-format
[directives]:          #directives
[incomestatement]:     #incomestatement
[journal]:             #journal-format
[period expressions]:  #period-expressions
[queries]:             #queries
[regular expression]:  #regular-expressions
[regular expressions]: #regular-expressions
[strict mode]:         #strict-mode
[timeclock]:           #timeclock-format
[timedot]:             #timedot-format
[transaction prices]:  #transaction-prices
[valuation]:           #valuation

_web_({{
*Quick links:
[Commands],
[Queries],
[Regular expressions],
[Period expressions],
[Journal],
[Directives],
[CSV],
[Timeclock],
[Timedot],
[Valuation],
[Common tasks]*
}})

_man_({{
# NAME
}})

This is the command-line interface (CLI) for the hledger accounting tool.
Here we also describe hledger's concepts and file formats.
This manual is for hledger _version_.

_man_({{
# SYNOPSIS
}})

`hledger`

`hledger [-f FILE] COMMAND [OPTIONS] [ARGS]`

`hledger [-f FILE] ADDONCMD -- [OPTIONS] [ARGS]`

_man_({{
# DESCRIPTION
}})

m4_dnl Include the standard description:
_hledgerdescription_

The basic function of the hledger CLI is to read a plain text file describing
financial transactions (in accounting terms, a general journal) and
print useful reports on standard output, or export them as CSV. hledger
can also read some other file formats such as CSV files, translating
them to journal format. Additionally, hledger lists other hledger-\*
executables found in the user’s \$PATH and can invoke them as subcommands.

hledger reads _files_
If using `$LEDGER_FILE`, note this must be a real environment variable,
not a shell variable.
You can specify standard input with `-f-`.

Transactions are dated movements of money between two (or more) named
accounts, and are recorded with journal entries like this:

m4_dnl Format as a journal snippet:
_journal_({{
2015/10/16 bought food
 expenses:food          $10
 assets:cash
}})

Most users use a text editor to edit the journal, usually with an editor
mode such as ledger-mode for added convenience. hledger’s interactive
add command is another way to record new transactions. hledger never
changes existing transactions.

To get started, you can either save some entries like the above in
`~/.hledger.journal`, or run `hledger add` and follow the prompts. Then
try some commands like `hledger print` or `hledger balance`.
Run `hledger` with no arguments for a list of commands.

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

Additionally, if the command is an [add-on](#addons),
you may need to put its options after a double-hyphen, eg: `hledger ui -- --watch`.
Or, you can run the add-on executable directly: `hledger-ui --watch`.

## Command arguments

Most hledger commands accept arguments after the command name,
which are often a [query](#queries), filtering the data in some way.

You can save a set of command line options/arguments in a file,
and then reuse them by writing `@FILENAME` as a command line argument.
Eg: `hledger bal @foo.args`.
(To prevent this, eg if you have an argument that begins with a literal `@`,
precede it with `--`, eg: `hledger bal -- @ARG`).

Inside the argument file, each line should contain just one option or argument.
Avoid the use of spaces, except inside quotes (or you'll see a confusing error).
Between a flag and its argument, use = (or nothing).
Bad:

    assets depth:2
    -X USD

Good:

    assets
    depth:2
    -X=USD

For special characters (see below), use one less level of quoting than
you would at the command prompt.
Bad:

    -X"$"

Good:

    -X$

See also: [Save frequently used options](save-frequently-used-options.html).

## Special characters

### Single escaping (shell metacharacters)

In shell command lines, characters significant to your shell - such as
spaces, `<`, `>`, `(`, `)`, `|`, `$` and `\` - should be
"shell-escaped" if you want hledger to see them. This is done by
enclosing them in single or double quotes, or by writing a backslash
before them. Eg to match an account name containing a space:

```shell
$ hledger register 'credit card'
```

or:

```shell
$ hledger register credit\ card
```

### Double escaping (regular expression metacharacters)

Characters significant in [regular expressions]
(described below) - such as `.`, `^`, `$`, `[`, `]`, `(`, `)`, `|`,
and `\` - may need to be "regex-escaped" if you don't want them to be
interpreted by hledger's regular expression engine. This is done by
writing backslashes before them, but since backslash is typically also
a shell metacharacter, both shell-escaping and regex-escaping will be
needed. Eg to match a literal `$` sign while using the bash shell:

```shell
$ hledger balance cur:'\$'
```

or:

```shell
$ hledger balance cur:\\$
```

### Triple escaping (for add-on commands)

When you use hledger to run an external add-on command (described
below), one level of shell-escaping is lost from any options or
arguments intended for by the add-on command, so those need an extra
level of shell-escaping. Eg to match a literal `$` sign while using
the bash shell and running an add-on command (`ui`):

```shell
$ hledger ui cur:'\\$'
```

or:

```shell
$ hledger ui cur:\\\\$
```

If you wondered why *four* backslashes, perhaps this helps:

|                 |         |
|-----------------|---------|
| unescaped:      | `$`     |
| escaped:        | `\$`    |
| double-escaped: | `\\$`   |
| triple-escaped: | `\\\\$` |

Or, you can avoid the extra escaping by running the add-on executable directly:

```shell
$ hledger-ui cur:\\$
```

### Less escaping

Options and arguments are sometimes used in places other than the
shell command line, where shell-escaping is not needed, so there you
should use one less level of escaping. Those places include:

- an @argumentfile
- hledger-ui's filter field
- hledger-web's search form
- GHCI's prompt (used by developers).


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

## Regular expressions

hledger uses [regular expressions](http://www.regular-expressions.info) in a number of places:

- [query terms](#queries), on the command line and in the hledger-web search form: `REGEX`, `desc:REGEX`, `cur:REGEX`, `tag:...=REGEX`
- [CSV rules](#csv-rules) conditional blocks: `if REGEX ...`
- [account alias](#rewriting-accounts) directives and options: `alias /REGEX/ = REPLACEMENT`, `--alias /REGEX/=REPLACEMENT`

hledger's regular expressions come from the
[regex-tdfa](http://hackage.haskell.org/package/regex-tdfa/docs/Text-Regex-TDFA.html)
library. 
If they're not doing what you expect, it's important to know exactly what they support:

#. they are case insensitive
#. they are infix matching (they do not need to match the entire thing being matched)
#. they are [POSIX ERE] (extended regular expressions)
#. they also support [GNU word boundaries] (`\b`, `\B`, `\<`, `\>`)
#. they do not support [backreferences]; if you write `\1`, it will match the digit `1`.
   Except when doing text replacement, eg in [account aliases](#regex-aliases),
   where [backreferences] can be used in the replacement string to reference [capturing groups] in the search regexp.
#. they do not support [mode modifiers] (`(?s)`), character classes (`\w`, `\d`), or anything else not mentioned above.

[POSIX ERE]: http://www.regular-expressions.info/posix.html#ere
[backreferences]: https://www.regular-expressions.info/backref.html
[capturing groups]: http://www.regular-expressions.info/refcapture.html
[mode modifiers]: http://www.regular-expressions.info/modifiers.html
[GNU word boundaries]: http://www.regular-expressions.info/wordboundaries.html

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

# ENVIRONMENT

m4_dnl Standard LEDGER_FILE description:
_LEDGER_FILE_

**COLUMNS**
The screen width used by the register command.
Default: the full terminal width.

**NO_COLOR**
If this variable exists with any value, 
hledger will not use ANSI color codes in terminal output.
This overrides the --color/--colour option.

# DATA FILES

hledger reads transactions from one or more data files.
The default data file is `$HOME/.hledger.journal`
(or on Windows, something like `C:/Users/USER/.hledger.journal`).

You can override this with the `$LEDGER_FILE` environment variable:

```shell
$ setenv LEDGER_FILE ~/finance/2016.journal
$ hledger stats
```

or with one or more `-f/--file` options:

```shell
$ hledger -f /some/file -f another_file stats
```

The file name `-` means standard input:

```shell
$ cat some.journal | hledger -f-
```

## Data formats

Usually the data file is in hledger's journal format, but it can be in
any of the supported file formats, which currently are:

| Reader:     | Reads:                                                           | Used for file extensions:            |
|-------------|------------------------------------------------------------------|--------------------------------------|
| `journal`   | hledger journal files and some Ledger journals, for transactions | `.journal` `.j` `.hledger` `.ledger` |
| `timeclock` | timeclock files, for precise time logging                        | `.timeclock`                         |
| `timedot`   | timedot files, for approximate time logging                      | `.timedot`                           |
| `csv`       | comma/semicolon/tab/other-separated values, for data import      | `.csv` `.ssv` `.tsv`                 |

These formats are described in their own sections, below.

hledger detects the format automatically based on the file extensions
shown above. If it can't recognise the file extension, it assumes
`journal` format. So for non-journal files, it's important to use a
recognised file extension, so as to either read successfully or to
show relevant error messages.

You can also force a specific reader/format by prefixing the file path
with the format and a colon. Eg, to read a .dat file as csv format:

```shell
$ hledger -f csv:/some/csv-file.dat stats
```
Or to read stdin (`-`) as timeclock format:
```shell
$ echo 'i 2009/13/1 08:00:00' | hledger print -ftimeclock:-
```

## Multiple files

You can specify multiple `-f` options, to read multiple files as one big journal.
There are some limitations with this:

- most directives do not affect sibling files
- [balance assertions](#balance-assertions) will not see any account balances from previous files

If you need either of those things, you can 

- use a single parent file which [includes](#including-other-files) the others
- or concatenate the files into one before reading, eg: `cat a.journal b.journal | hledger -f- CMD`.

## Strict mode

hledger checks input files for valid data.
By default, the most important errors are detected, while still accepting
easy journal files without a lot of declarations:

- Are the input files parseable, with valid syntax ?
- Are all transactions balanced ?
- Do all balance assertions pass ?

With the `-s`/`--strict` flag, additional checks are performed:

- Are all accounts posted to, declared with an `account` directive ?
  ([Account error checking](#account-error-checking))
- Are all commodities declared with a `commodity` directive ?
  ([Commodity error checking](#commodity-error-checking))

You can also use the [check](#check) command to run these and some additional checks.

# TIME PERIODS
## Smart dates

hledger's user interfaces accept a flexible "smart date" syntax.
Smart dates allow some english words, can be relative to today's date,
and can have less-significant date parts omitted (defaulting to 1).

Examples:

|                                              |                                                                                       |
|----------------------------------------------|---------------------------------------------------------------------------------------|
| `2004/10/1`, `2004-01-01`, `2004.9.1`        | exact date, several separators allowed. Year is 4+ digits, month is 1-12, day is 1-31 |
| `2004`                                       | start of year                                                                         |
| `2004/10`                                    | start of month                                                                        |
| `10/1`                                       | month and day in current year                                                         |
| `21`                                         | day in current month                                                                  |
| `october, oct`                               | start of month in current year                                                        |
| `yesterday, today, tomorrow`                 | -1, 0, 1 days from today                                                              |
| `last/this/next day/week/month/quarter/year` | -1, 0, 1 periods from the current period                                              |
| `20181201`                                   | 8 digit YYYYMMDD with valid year month and day                                        |
| `201812`                                     | 6 digit YYYYMM with valid year and month                                              |

Counterexamples - malformed digit sequences might give surprising results:

|             |                                                                   |
|-------------|-------------------------------------------------------------------|
| `201813`    | 6 digits with an invalid month is parsed as start of 6-digit year |
| `20181301`  | 8 digits with an invalid month is parsed as start of 8-digit year |
| `20181232`  | 8 digits with an invalid day gives an error                       |
| `201801012` | 9+ digits beginning with a valid YYYYMMDD gives an error          |

## Report start & end date

By default, most hledger reports will show the full span of time represented by the journal data.
The report start date will be the earliest transaction or posting date, and the report end date
will be the latest transaction, posting, or market price date.

Often you will want to see a shorter time span, such as the current month.
You can specify a start and/or end date using
[`-b/--begin`](#reporting-options),
[`-e/--end`](#reporting-options),
[`-p/--period`](#period-expressions)
or a [`date:` query](#queries) (described below).
All of these accept the [smart date](#smart-dates) syntax.

Some notes:

- As in Ledger, end dates are exclusive, so you need to write the date *after*
  the last day you want to include.
- As noted in [reporting options](#general-options):
  among start/end dates specified with *options*, the last (i.e. right-most)
  option takes precedence.
- The effective report start and end dates are the intersection of the
  start/end dates from options and that from `date:` queries.
  That is, `date:2019-01 date:2019 -p'2000 to 2030'` yields January 2019, the
  smallest common time span.
- A [report interval](#report-intervals) (see below) will adjust start/end dates,
  when needed, so that they fall on subperiod boundaries.

Examples:

|                    |                                                                                             |
|--------------------|---------------------------------------------------------------------------------------------|
| `-b 2016/3/17`     | begin on St. Patrick’s day 2016                                                             |
| `-e 12/1`          | end at the start of december 1st of the current year (11/30 will be the last date included) |
| `-b thismonth`     | all transactions on or after the 1st of the current month                                   |
| `-p thismonth`     | all transactions in the current month                                                       |
| `date:2016/3/17..` | the above written as queries instead (`..` can also be replaced with `-`)                   |
| `date:..12/1`      |                                                                                             |
| `date:thismonth..` |                                                                                             |
| `date:thismonth`   |                                                                                             |

## Report intervals

A report interval can be specified so that commands like
[register](#register), [balance](#balance) and [activity](#activity)
become multi-period, showing each subperiod as a separate row or
column.

The following "standard" report intervals can be enabled by using
their corresponding flag:

`-D/--daily`, 
`-W/--weekly`, 
`-M/--monthly`,
`-Q/--quarterly`, 
`-Y/--yearly`. 

These standard intervals always start on natural interval boundaries:
eg `--weekly` starts on mondays, `--monthly` starts on the first of
the month, `--yearly` always starts on January 1st, etc.

Certain more complex intervals, and more flexible boundary dates, can
be specified by `-p/--period`. These are described in [period
expressions](#period-expressions), below.

Report intervals can only be specified by the flags above, and not by
[query](#queries) arguments, currently.

Report intervals have another effect: multi-period reports are always
expanded to fill a whole number of subperiods. So if you use a report
interval (other than `--daily`), and you have specified a start or end
date, you may notice those dates being overridden (ie, the report
starts earlier than your requested start date, or ends later than your
requested end date). This is done to ensure "full" first and last
subperiods, so that all subperiods' numbers are comparable.

## Period expressions

The `-p/--period` option accepts period expressions, a shorthand way
of expressing a start date, end date, and/or report interval all at
once.

Here's a basic period expression specifying the first quarter of 2009. Note,
hledger always treats start dates as inclusive and end dates as exclusive:

`-p "from 2009/1/1 to 2009/4/1"`

Keywords like "from" and "to" are optional, and so are the spaces, as long
as you don't run two dates together. "to" can also be written as ".." or "-".
These are equivalent to the above:

|                           |
|---------------------------|
| `-p "2009/1/1 2009/4/1"`  |
| `-p2009/1/1to2009/4/1`    |
| `-p2009/1/1..2009/4/1`    |

Dates are [smart dates](#smart-dates), so if the current year is 2009, the
above can also be written as:

|                         |
|-------------------------|
| `-p "1/1 4/1"`          |
| `-p "january-apr"`      |
| `-p "this year to 4/1"` |

If you specify only one date, the missing start or end date will be the
earliest or latest transaction in your journal:

|                      |                                   |
|----------------------|-----------------------------------|
| `-p "from 2009/1/1"` | everything after january 1, 2009  |
| `-p "from 2009/1"`   | the same                          |
| `-p "from 2009"`     | the same                          |
| `-p "to 2009"`       | everything before january 1, 2009 |

A single date with no "from" or "to" defines both the start and end date
like so:

|                 |                                                             |
|-----------------|-------------------------------------------------------------|
| `-p "2009"`     | the year 2009; equivalent to “2009/1/1 to 2010/1/1”         |
| `-p "2009/1"`   | the month of jan; equivalent to “2009/1/1 to 2009/2/1”      |
| `-p "2009/1/1"` | just that day; equivalent to “2009/1/1 to 2009/1/2”         |

Or you can specify a single quarter like so:

|                 |                                                             |
|-----------------|-------------------------------------------------------------|
| `-p "2009Q1"`   | first quarter of 2009, equivalent to “2009/1/1 to 2009/4/1” |
| `-p "q4"`       | fourth quarter of the current year                          |

The argument of `-p` can also begin with, or be, a [report interval](#report-intervals) expression.
The basic report intervals are `daily`, `weekly`, `monthly`, `quarterly`, or `yearly`,
which have the same effect as the `-D`,`-W`,`-M`,`-Q`, or `-Y` flags.
Between report interval and start/end dates (if any), the word `in` is optional.
Examples:

|                                         |
|-----------------------------------------|
| `-p "weekly from 2009/1/1 to 2009/4/1"` |
| `-p "monthly in 2008"`                  |
| `-p "quarterly"`                        |

Note that `weekly`, `monthly`, `quarterly` and `yearly` intervals will
always start on the first day on week, month, quarter or year
accordingly, and will end on the last day of same period, even if
associated period expression specifies different explicit start and end date.

For example:

|                                                |                                                                                    |
|------------------------------------------------|------------------------------------------------------------------------------------|
| `-p "weekly from 2009/1/1 to 2009/4/1"`        | starts on 2008/12/29, closest preceding Monday                                     |
| `-p "monthly in 2008/11/25"`                   | starts on 2018/11/01                                                               |
| `-p "quarterly from 2009-05-05 to 2009-06-01"` | starts on 2009/04/01, ends on 2009/06/30, which are first and last days of Q2 2009 |
| `-p "yearly from 2009-12-29"`                  | starts on 2009/01/01, first day of 2009                                            |

The following more complex report intervals are also supported:
`biweekly`,
`fortnightly`,
`bimonthly`,
`every day|week|month|quarter|year`,
`every N days|weeks|months|quarters|years`.


All of these will start on the first day of the requested period and end on the last one, as described above.

Examples:

|                                   |                                                             |
|-----------------------------------|-------------------------------------------------------------|
| `-p "bimonthly from 2008"`        | periods will have boundaries on 2008/01/01, 2008/03/01, ... |
| `-p "every 2 weeks"`              | starts on closest preceding Monday                          |
| `-p "every 5 month from 2009/03"` | periods will have boundaries on 2009/03/01, 2009/08/01, ... |

If you want intervals that start on arbitrary day of your choosing and span a week, month or year, you need to use any of the following:

`every Nth day of week`,
`every WEEKDAYNAME` (eg `mon|tue|wed|thu|fri|sat|sun`),
`every Nth day [of month]`,
`every Nth WEEKDAYNAME [of month]`,
`every MM/DD [of year]`,
`every Nth MMM [of year]`,
`every MMM Nth [of year]`.

Examples:

|                              |                                                          |
|------------------------------|----------------------------------------------------------|
| `-p "every 2nd day of week"` | periods will go from Tue to Tue                          |
| `-p "every Tue"`             | same                                                     |
| `-p "every 15th day"`        | period boundaries will be on 15th of each month          |
| `-p "every 2nd Monday"`      | period boundaries will be on second Monday of each month |
| `-p "every 11/05"`           | yearly periods with boundaries on 5th of Nov             |
| `-p "every 5th Nov"`         | same                                                     |
| `-p "every Nov 5th"`         | same                                                     |

Show historical balances at end of 15th each month (N is exclusive end date):

`hledger balance -H -p "every 16th day"`

Group postings from start of wednesday to end of next tuesday (N is start date and exclusive end date):

`hledger register checking -p "every 3rd day of week"`

# DEPTH

With the `--depth N` option (short form: `-N`), commands like [account](#account), [balance](#balance)
and [register](#register) will show only the uppermost accounts in the account
tree, down to level N. Use this when you want a summary with less detail.
This flag has the same effect as a `depth:` query argument
(so `-2`, `--depth=2` or `depth:2` are equivalent).

# QUERIES

One of hledger's strengths is being able to quickly report on precise subsets of your data.
Most commands accept an optional query expression, written as arguments after the command name,
to filter the data by date, account name or other criteria.
The syntax is similar to a web search:
one or more space-separated search terms,
quotes to enclose whitespace,
prefixes to match specific fields,
a not: prefix to negate the match.

We do not yet support arbitrary boolean combinations of search terms;
instead most commands show transactions/postings/accounts which match (or negatively match):

- any of the description terms AND
- any of the account terms AND
- any of the status terms AND
- all the other terms.

The [print](#print) command instead shows transactions which:

- match any of the description terms AND
- have any postings matching any of the positive account terms AND
- have no postings matching any of the negative account terms AND
- match all the other terms.

The following kinds of search terms can be used.
Remember these can also be prefixed with **`not:`**, eg to exclude a particular subaccount.

**`REGEX`, `acct:REGEX`**
: match account names by this [regular expression]. 
(With no prefix, `acct:` is assumed.)

: same as above

**`amt:N, amt:<N, amt:<=N, amt:>N, amt:>=N`**
: match postings with a single-commodity amount that is equal to, less
than, or greater than N.  (Multi-commodity amounts are not tested, and
will always match.)  The comparison has two modes: if N is preceded by
a + or - sign (or is 0), the two signed numbers are
compared. Otherwise, the absolute magnitudes are compared, ignoring
sign.

**`code:REGEX`**
: match by transaction code (eg check number)

**`cur:REGEX`**
: match postings or transactions including any amounts whose
currency/commodity symbol is fully matched by REGEX. (For a partial
match, use `.*REGEX.*`). Note, to match characters which are
regex-significant, like the dollar sign (`$`), you need to prepend `\`.
And when using the command line you need to add one more level of
quoting to hide it from the shell, so eg do: `hledger print cur:'\$'`
or `hledger print cur:\\$`.

**`desc:REGEX`**
: match transaction descriptions.

**`date:PERIODEXPR`**
: match dates within the specified period.
PERIODEXPR is a [period expression](#period-expressions) (with no report interval).
Examples: `date:2016`, `date:thismonth`, `date:2000/2/1-2/15`, `date:lastweek-`.
If the `--date2` command line flag is present, this matches [secondary dates](#secondary-dates) instead.
([Report intervals](#report-intervals) will adjust [start/end dates](report-start--end-date)
to preceding/following subperiod boundaries.)

**`date2:PERIODEXPR`**
: match secondary dates within the specified period.

**`depth:N`**
: match (or display, depending on command) accounts at or above this depth

**`note:REGEX`**
: match transaction [notes](#payee-and-note)
(part of description right of `|`, or whole description when there's no `|`)

**`payee:REGEX`**
: match transaction [payee/payer names](#payee-and-note)
(part of description left of `|`, or whole description when there's no `|`)

**`real:, real:0`**
: match real or virtual postings respectively

**`status:, status:!, status:*`**
: match unmarked, pending, or cleared transactions respectively

**`tag:REGEX[=REGEX]`**
: match by tag name, and optionally also by tag value.  Note a
tag: query is considered to match a transaction if it matches any of
the postings.  Also remember that postings inherit the tags of their
parent transaction.

The following special search term is used automatically in hledger-web, only:

**`inacct:ACCTNAME`**
: tells hledger-web to show the transaction register for this account.
Can be filtered further with `acct` etc.

Some of these can also be expressed as command-line options (eg `depth:2` is equivalent to `--depth 2`).
Generally you can mix options and query arguments, and the resulting query will be their intersection
(perhaps excluding the `-p/--period` option).

# COSTING

The `-B/--cost` flag converts amounts to their cost or sale amount at transaction time,
if they have a [transaction price](#transaction-prices) specified.
If this flag is supplied, hledger will perform cost conversion first, and will apply
any market price valuations (if requested) afterwards.

# VALUATION

Instead of reporting amounts in their original commodity,
hledger can convert them to
cost/sale amount (using the conversion rate recorded in the transaction),
and/or to market value (using some market price on a certain date).
This is controlled by the `--value=TYPE[,COMMODITY]` option, which will be described below.
We also provide the simpler `-V` and `-X COMMODITY` options, and often
one of these is all you need:

## -V: Value

The `-V/--market` flag converts amounts to market value in their
default *valuation commodity*, using the
[market prices](#market-prices) in effect on the *valuation date(s)*, if any.
More on these in a minute.

## -X: Value in specified commodity

The `-X/--exchange=COMM` option is like `-V`, except you tell it which
currency you want to convert to, and it tries to convert everything to that.

## Valuation date

Since market prices can change from day to day, market value reports
have a valuation date (or more than one), which determines which
market prices will be used.

For single period reports, if an explicit
[report end date](#report-start-end-date) is specified, that will be
used as the valuation date; otherwise the valuation date is the journal's end date.

For [multiperiod reports](#report-intervals), each column/period is
valued on the last day of the period, by default.

## Market prices

To convert a commodity A to its market value in another commodity B,
hledger looks for a suitable market price (exchange rate) as follows,
in this order of preference
<!-- (-X tries all of these; -V tries only 1) (really ?) -->
:

1. A *declared market price* or *inferred market price*:
   A's latest market price in B on or before the valuation date
   as declared by a [P directive](#declaring-market-prices), 
   or (with the `--infer-market-price` flag)
   inferred from [transaction prices](#transaction-prices).
   <!-- (Latest by date, then parse order.) -->
   <!-- (A declared price overrides an inferred price on the same date.) -->
  
2. A *reverse market price*:
   the inverse of a declared or inferred market price from B to A.

3. A *forward chain of market prices*:
   a synthetic price formed by combining the shortest chain of
   "forward" (only 1 above) market prices, leading from A to B.

4. *Any chain of market prices*:
   a chain of any market prices, including both forward and
   reverse prices (1 and 2 above), leading from A to B.

There is a limit to the length of these price chains; if hledger
reaches that length without finding a complete chain or exhausting 
all possibilities, it will give up (with a "gave up" message 
visible in `--debug=2` output). That limit is currently 1000.

Amounts for which no suitable market price can be found, are not converted.

## --infer-market-price: market prices from transactions

Normally, market value in hledger is fully controlled by, and requires,
[P directives](#declaring-market-prices) in your journal.
Since adding and updating those can be a chore,
and since transactions usually take place at close to market value,
why not use the recorded [transaction prices](#transaction-prices)
as additional market prices (as Ledger does) ?
We could produce value reports without needing P directives at all.

Adding the `--infer-market-price` flag to `-V`, `-X` or `--value` enables
this. So for example, `hledger bs -V --infer-market-price` will get market
prices both from P directives and from transactions.
(And if both occur on the same day, the P directive takes precedence).

There is a downside: value reports can sometimes  be affected in
confusing/undesired ways by your journal entries. If this happens to
you, read all of this [Valuation](#valuation) section carefully,
and try adding `--debug` or `--debug=2` to troubleshoot.

`--infer-market-price` can infer market prices from:

- multicommodity transactions with explicit prices (`@`/`@@`)

- multicommodity transactions with implicit prices (no `@`, two commodities, unbalanced).
  (With these, the order of postings matters. `hledger print -x` can be useful for troubleshooting.)

- but not, currently, from
  "[more correct](investments.html#a-more-correct-entry)" multicommodity transactions
  (no `@`, multiple commodities, balanced).

## Valuation commodity

**When you specify a valuation commodity (`-X COMM` or `--value TYPE,COMM`):**\
hledger will convert all amounts to COMM,
wherever it can find a suitable market price (including by reversing or chaining prices).

**When you leave the valuation commodity unspecified (`-V` or `--value TYPE`):**\
For each commodity A, hledger picks a default valuation commodity as
follows, in this order of preference:

1. The price commodity from the latest P-declared market price for A
   on or before valuation date.

2. The price commodity from the latest P-declared market price for A on
   any date. (Allows conversion to proceed when there are inferred
   prices before the valuation date.)

3. If there are no P directives at all (any commodity or date) and the
   `--infer-market-price` flag is used: the price commodity from the latest
   transaction-inferred price for A on or before valuation date.

This means:

- If you have [P directives](#declaring-market-prices), 
  they determine which commodities `-V` will convert, and to what.

- If you have no P directives, and use the `--infer-market-price` flag, 
  [transaction prices](#transaction-prices) determine it.

Amounts for which no valuation commodity can be found are not converted.

## Simple valuation examples

Here are some quick examples of `-V`:

```journal
; one euro is worth this many dollars from nov 1
P 2016/11/01 € $1.10

; purchase some euros on nov 3
2016/11/3
    assets:euros        €100
    assets:checking

; the euro is worth fewer dollars by dec 21
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

## --value: Flexible valuation

`-V` and `-X` are special cases of the more general `--value` option:

     --value=TYPE[,COMM]  TYPE is then, end, now or YYYY-MM-DD.
                          COMM is an optional commodity symbol.
                          Shows amounts converted to:
                          - default valuation commodity (or COMM) using market prices at posting dates
                          - default valuation commodity (or COMM) using market prices at period end(s)
                          - default valuation commodity (or COMM) using current market prices
                          - default valuation commodity (or COMM) using market prices at some date

The TYPE part selects cost or value and valuation date:

`--value=then`
: Convert amounts to their value in the [default valuation commodity](#valuation-commodity),
  using market prices on each posting's date.

`--value=end`
: Convert amounts to their value in the default valuation commodity, using market prices
  on the last day of the report period (or if unspecified, the journal's end date);
  or in multiperiod reports, market prices on the last day of each subperiod.

`--value=now`
: Convert amounts to their value in the default valuation commodity
  using current market prices (as of when report is generated).

`--value=YYYY-MM-DD`
: Convert amounts to their value in the default valuation commodity
  using market prices on this date.

To select a different valuation commodity, add the optional `,COMM` part:
a comma, then the target commodity's symbol. Eg: **`--value=now,EUR`**.
hledger will do its best to convert amounts to this commodity, deducing
[market prices](#market-prices) as described above.

## More valuation examples

Here are some examples showing the effect of `--value`, as seen with `print`:

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
$ hledger -f- print --cost
2000-01-01
    (a)             5 B

2000-02-01
    (a)             6 B

2000-03-01
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
2000-01-01
    (a)             3 B

2000-02-01
    (a)             3 B

2000-03-01
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
2000-01-01
    (a)             1 B

2000-02-01
    (a)             1 B

2000-03-01
    (a)             1 B

```

You may need to explicitly set a commodity's display style, when reverse prices are used.
Eg this output might be surprising:
```journal
P 2000-01-01 A 2B

2000-01-01
  a  1B
  b
```
```shell
$ hledger print -x -X A
2000-01-01
    a               0
    b               0

```
Explanation: because there's no amount or commodity directive specifying a display style
for A, 0.5A gets the default style, which shows no decimal digits. Because the displayed
amount looks like zero, the commodity symbol and minus sign are not displayed either.
Adding a commodity directive sets a more useful display style for A:
```journal
P 2000-01-01 A 2B
commodity 0.00A

2000-01-01
  a  1B
  b
```
```shell
$ hledger print -X A
2000-01-01
    a           0.50A
    b          -0.50A

```

## Effect of valuation on reports

Here is a reference for how valuation is supposed to affect each part of hledger's reports (and a glossary).
(It's wide, you'll have to scroll sideways.)
It may be useful when troubleshooting.
If you find problems, please report them, ideally with a reproducible example.
Related:
[#329](https://github.com/simonmichael/hledger/issues/329),
[#1083](https://github.com/simonmichael/hledger/issues/1083).

| Report type                                         | `-B`, `--cost`                                                   | `-V`, `-X`                                                        | `--value=then`                                                                                 | `--value=end`                                                     | `--value=DATE`, `--value=now`           |
|-----------------------------------------------------|------------------------------------------------------------------|-------------------------------------------------------------------|------------------------------------------------------------------------------------------------|-------------------------------------------------------------------|-----------------------------------------|
| **print**                                           |                                                                  |                                                                   |                                                                                                |                                                                   |                                         |
| posting amounts                                     | cost                                                             | value at report end or today                                      | value at posting date                                                                          | value at report or journal end                                    | value at DATE/today                     |
| balance assertions/assignments                      | unchanged                                                        | unchanged                                                         | unchanged                                                                                      | unchanged                                                         | unchanged                               |
| <br>                                                |                                                                  |                                                                   |                                                                                                |                                                                   |                                         |
| **register**                                        |                                                                  |                                                                   |                                                                                                |                                                                   |                                         |
| starting balance (-H)                               | cost                                                             | value at day before report or journal start                       | valued at day each historical posting was made                                                 | value at day before report or journal start                       | value at DATE/today                     |
| posting amounts                                     | cost                                                             | value at report end or today                                      | value at posting date                                                                          | value at report or journal end                                    | value at DATE/today                     |
| summary posting amounts with report interval        | summarised cost                                                  | value at period ends                                              | sum of postings in interval, valued at interval start                                          | value at period ends                                              | value at DATE/today                     |
| running total/average                               | sum/average of displayed values                                  | sum/average of displayed values                                   | sum/average of displayed values                                                                | sum/average of displayed values                                   | sum/average of displayed values         |
| <br>                                                |                                                                  |                                                                   |                                                                                                |                                                                   |                                         |
| **balance (bs, bse, cf, is)**                       |                                                                  |                                                                   |                                                                                                |                                                                   |                                         |
| balance changes                                     | sums of costs                                                    | value at report end or today of sums of postings                  | value at posting date                                                                          | value at report or journal end of sums of postings                | value at DATE/today of sums of postings |
| budget amounts (--budget)                           | like balance changes                                             | like balance changes                                              | like balance changes                                                                           | like balances                                                     | like balance changes                    |
| grand total                                         | sum of displayed values                                          | sum of displayed values                                           | sum of displayed valued                                                                        | sum of displayed values                                           | sum of displayed values                 |
| <br>                                                |                                                                  |                                                                   |                                                                                                |                                                                   |                                         |
| **balance (bs, bse, cf, is) with report interval**  |                                                                  |                                                                   |                                                                                                |                                                                   |                                         |
| starting balances (-H)                              | sums of costs of postings before report start                    | value at report start of sums of all postings before report start | sums of values of postings before report start at respective posting dates                     | value at report start of sums of all postings before report start | sums of postings before report start    |
| balance changes (bal, is, bs --change, cf --change) | sums of costs of postings in period                              | same as --value=end                                               | sums of values of postings in period at respective posting dates                               | balance change in each period, valued at period ends              | value at DATE/today of sums of postings |
| end balances (bal -H, is --H, bs, cf)               | sums of costs of postings from before report start to period end | same as --value=end                                               | sums of values of postings from before period start to period end at respective posting dates  | period end balances, valued at period ends                        | value at DATE/today of sums of postings |
| budget amounts (--budget)                           | like balance changes/end balances                                | like balance changes/end balances                                 | like balance changes/end balances                                                              | like balances                                                     | like balance changes/end balances       |
| row totals, row averages (-T, -A)                   | sums, averages of displayed values                               | sums, averages of displayed values                                | sums, averages of displayed values                                                             | sums, averages of displayed values                                | sums, averages of displayed values      |
| column totals                                       | sums of displayed values                                         | sums of displayed values                                          | sums of displayed values                                                                       | sums of displayed values                                          | sums of displayed values                |
| grand total, grand average                          | sum, average of column totals                                    | sum, average of column totals                                     | sum, average of column totals                                                                  | sum, average of column totals                                     | sum, average of column totals           |
| <br>                                                |                                                                  |                                                                   |                                                                                                |                                                                   |                                         |

`--cumulative` is omitted to save space, it works like `-H` but with a zero starting balance.

**Glossary:**

*cost*
: calculated using price(s) recorded in the transaction(s).

*value*
: market value using available market price declarations, or the unchanged amount if no conversion rate can be found.

*report start*
: the first day of the report period specified with -b or -p or date:, otherwise today.

*report or journal start*
: the first day of the report period specified with -b or -p or date:, otherwise the earliest transaction date in the journal, otherwise today.

*report end*
: the last day of the report period specified with -e or -p or date:, otherwise today.

*report or journal end*
: the last day of the report period specified with -e or -p or date:, otherwise the latest transaction date in the journal, otherwise today.

*report interval*
: a flag (-D/-W/-M/-Q/-Y) or period expression that activates the report's multi-period mode (whether showing one or many subperiods).




# PIVOTING

Normally hledger sums amounts, and organizes them in a hierarchy, based on account name.
The `--pivot FIELD` option causes it to sum and organize hierarchy based on the value of some other field instead.
FIELD can be:
`code`, `description`, `payee`, `note`,
or the full name (case insensitive) of any [tag](#tags).
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

# OUTPUT

## Output destination

hledger commands send their output to the terminal by default.
You can of course redirect this, eg into a file, using standard shell syntax:
```shell
$ hledger print > foo.txt
```

Some commands (print, register, stats, the balance commands) also
provide the `-o/--output-file` option, which does the same thing
without needing the shell. Eg:
```shell
$ hledger print -o foo.txt
$ hledger print -o -        # write to stdout (the default)
```

hledger can optionally produce debug output (if enabled with `--debug=N`); 
this goes to stderr, and is not affected by `-o/--output-file`.
If you need to capture it, use shell redirects, eg: `hledger bal --debug=3 >file 2>&1`.

## Output format

Some commands (print, register, the balance commands) offer a choice of output format. 
In addition to the usual plain text format (`txt`), there are
CSV (`csv`), HTML (`html`), JSON (`json`) and SQL (`sql`).
This is controlled by the `-O/--output-format` option:
```shell
$ hledger print -O csv
```
or, by a file extension specified with `-o/--output-file`:
```shell
$ hledger balancesheet -o foo.html   # write HTML to foo.html
```
The `-O` option can be used to override the file extension if needed:
```shell
$ hledger balancesheet -o foo.txt -O html   # write HTML to foo.txt
```

Some notes about JSON output:

- This feature is marked experimental, and not yet much used; you
  should expect our JSON to evolve. Real-world feedback is welcome.

- Our JSON is rather large and verbose, as it is quite a faithful
  representation of hledger's internal data types. To understand the
  JSON, read the Haskell type definitions, which are mostly in
  https://github.com/simonmichael/hledger/blob/master/hledger-lib/Hledger/Data/Types.hs.

<!--
- The JSON output from hledger commands is essentially the same as the
  JSON served by [hledger-web's JSON API](hledger-web.html#json-api),
  but pretty printed, using line breaks and indentation.
  Our pretty printer has the ability to elide data in certain cases -
  rendering non-strings as if they were strings, or displaying "FOO.."
  instead of FOO's full details. This should never happen in hledger's
  JSON output; if you see otherwise, please report as a bug.
-->

- hledger represents quantities as Decimal values storing up to 255
  significant digits, eg for repeating decimals. Such numbers can
  arise in practice (from automatically-calculated transaction
  prices), and would break most JSON consumers. So in JSON, we show
  quantities as simple Numbers with at most 10 decimal places. We
  don't limit the number of integer digits, but that part is under
  your control.
  We hope this approach will not cause problems in practice; if you
  find otherwise, please let us know. 
  (Cf [#1195](https://github.com/simonmichael/hledger/issues/1195))

Notes about SQL output:

- SQL output is also marked experimental, and much like JSON could use
real-world feedback.

- SQL output is expected to work with sqlite, MySQL and PostgreSQL

- SQL output is structured with the expectations that statements will
  be executed in the empty database. If you already have tables created
  via SQL output of hledger, you would probably want to either clear tables
  of existing data (via `delete` or `truncate` SQL statements) or drop
  tables completely as otherwise your postings will be duped.

# COMMANDS

hledger provides a number of commands for producing reports and managing your data. 
Run `hledger` with no arguments to list the commands available,
and `hledger CMD` to run a command. CMD can be the full command name,
or its standard abbreviation shown in the commands list,
or any unambiguous prefix of the name.
Eg: `hledger bal`.

m4_dnl XXX maybe later
m4_dnl Each command's detailed docs are available :
m4_dnl 
m4_dnl - command line help, eg: `hledger balance --help`
m4_dnl - 
m4_dnl - info manuals, eg: `hledger help --info hledger` (or possibly `info hledger`) <!-- -> m4_dnl Commands -> balance -->
m4_dnl - web manuals, eg: <https://hledger.org/hledger.html#balance>
m4_dnl <!-- - man pages, eg: `man hledger-balance` -->

Here are the built-in commands, with the most often-used in bold:
<!-- keep synced with Hledger.Cli.Commands.commandsList, commands.m4 -->

**Data entry:**

These data entry commands are the only ones which can modify your journal file.
 
- **[add](#add)**                                  - add transactions using guided prompts
- **[import](#import)**                            - add any new transactions from other files (eg csv)

**Data management:**

- [check](#check)                                  - check for various kinds of issue in the data
- [close](#close) (equity)                         - generate balance-resetting transactions
- [diff](#diff)                                    - compare account transactions in two journal files
- [rewrite](#rewrite)                              - generate extra postings, similar to print --auto

**Financial statements:**

- **[aregister](#aregister) (areg)**               - show transactions in a particular account
- **[balancesheet](#balancesheet) (bs)**           - show assets, liabilities and net worth
- [balancesheetequity](#balancesheetequity) (bse)  - show assets, liabilities and equity
- [cashflow](#cashflow) (cf)                       - show changes in liquid assets
- **[incomestatement](#incomestatement) (is)**     - show revenues and expenses
- [roi](#roi)                                      - show return on investments

**Miscellaneous reports:**

- [accounts](#accounts)                            - show account names
- [activity](#activity)                            - show postings-per-interval bar charts
- **[balance](#balance) (bal)**                    - show balance changes/end balances/budgets in any accounts
- [codes](#codes)                                  - show transaction codes
- [commodities](#commodities)                      - show commodity/currency symbols
- [descriptions](#descriptions)                    - show unique transaction descriptions
- [files](#files)                                  - show input file paths
- [help](#help)                                    - show hledger user manuals in several formats
- [notes](#notes)                                  - show unique note segments of transaction descriptions
- [payees](#payees)                                - show unique payee segments of transaction descriptions
- [prices](#prices)                                - show market price records
- **[print](#print)**                              - show transactions (journal entries)
- [print-unique](#print-unique)                    - show only transactions with unique descriptions
- **[register](#register) (reg)**                  - show postings in one or more accounts & running total
- [register-match](#register-match)                - show a recent posting that best matches a description
- [stats](#stats)                                  - show journal statistics
- [tags](#tags)                                    - show tag names
- [test](#test)                                    - run self tests

<a name="addons"></a>
**Add-on commands:**

Programs or scripts named `hledger-SOMETHING` in your PATH are 
[add-on commands](#about-add-on-commands); these appear in the
commands list with a `+` mark. 
Two of these are maintained and released with hledger:

- **[ui](hledger-ui.html)**                        - an efficient terminal interface (TUI) for hledger
- **[web](hledger-web.html)**                      - a simple web interface (WUI) for hledger

And these add-ons are maintained separately:

- [iadd](http://hackage.haskell.org/package/hledger-iadd) - a more interactive alternative for the add command
- [interest](http://hackage.haskell.org/package/hledger-interest) - generates interest transactions according to various schemes
- [stockquotes](http://hackage.haskell.org/package/hledger-stockquotes) - downloads market prices for your commodities from AlphaVantage *(experimental)*

m4_dnl XXX maybe later
m4_dnl _man_({{
m4_dnl For detailed command docs please see the appropriate man page (eg `man hledger-print`), 
m4_dnl or the info or web format of this manual.
m4_dnl }})
m4_dnl _notman_({{

Next, the detailed command docs, in alphabetical order.

m4_dnl cf Hledger/Cli/Commands/commands.m4:
_commands_({{##}})

## About add-on commands

Add-on commands are programs or scripts in your PATH 

- whose name starts with `hledger-`
- whose name ends with a recognised file extension:
  `.bat`,`.com`,`.exe`, `.hs`,`.lhs`,`.pl`,`.py`,`.rb`,`.rkt`,`.sh` or none
- and (on unix, mac) which are executable by the current user.

Add-ons are a relatively easy way to add local features or experiment with new ideas.
They can be written in any language, but haskell scripts have a big advantage:
they can use the same hledger library functions that built-in commands use for command-line options, parsing and reporting.
Some experimental/example add-on scripts can be found in the hledger repo's
[bin/ directory](https://github.com/simonmichael/hledger/tree/master/bin).

Note in a hledger command line, add-on command flags must have a double dash (`--`) preceding them.
Eg you must write:
```shell
$ hledger web -- --serve
```
and not:
```shell
$ hledger web --serve
```
(because the `--serve` flag belongs to `hledger-web`, not `hledger`).

The `-h/--help` and `--version` flags don't require `--`.

If you have any trouble with this, remember you can always run the add-on program directly, eg:
```shell
$ hledger-web --serve
```

# JOURNAL FORMAT

hledger's default file format, representing a General Journal.

hledger's usual data source is a plain text file containing journal entries in hledger journal format.
This file represents a standard accounting [general journal](http://en.wikipedia.org/wiki/General_journal).
I use file names ending in `.journal`, but that's not required.
The journal file contains a number of transaction entries,
each describing a transfer of money (or any commodity) between two or more named accounts,
in a simple format readable by both hledger and humans.

hledger's journal format is a compatible subset, mostly, of [ledger's
journal format](http://ledger-cli.org/3.0/doc/ledger3.html#Journal-Format),
so hledger can work with [compatible](faq.html#file-format-differences)
ledger journal files as well.  It's safe, and encouraged, to run both
hledger and ledger on the same journal file, eg to validate the results
you're getting.

You can use hledger without learning any more about this file; just
use the [add](#add) or [web](#web) or [import](#import) commands to
create and update it.

Many users, though, edit the journal file with a text editor,
and track changes with a version control system such as git.
Editor addons such as
ledger-mode or hledger-mode for Emacs,
vim-ledger for Vim,
and hledger-vscode for Visual Studio Code,
make this easier, adding colour, formatting, tab completion, and useful commands.
See [Editor configuration](editors.html) at hledger.org for the full list.

<!--
Here's an example:

```journal
; A sample journal file. This is a comment.

2008/01/01 income             ; <- transaction's first line starts in column 0, contains date and description
    assets:bank:checking  $1  ; <- posting lines start with whitespace, each contains an account name
    income:salary        $-1  ;    followed by at least two spaces and an amount

2008/06/01 gift
    assets:bank:checking  $1  ; <- at least two postings in a transaction
    income:gifts         $-1  ; <- their amounts must balance to 0

2008/06/02 save
    assets:bank:saving    $1
    assets:bank:checking      ; <- one amount may be omitted; here $-1 is inferred

2008/06/03 eat & shop         ; <- description can be anything
    expenses:food         $1
    expenses:supplies     $1  ; <- this transaction debits two expense accounts
    assets:cash               ; <- $-2 inferred

2008/10/01 take a loan
    assets:bank:checking  $1
    liabilities:debts    $-1

2008/12/31 * pay off          ; <- an optional * or ! after the date means "cleared" (or anything you want)
    liabilities:debts     $1
    assets:bank:checking
```
-->

Here's a description of each part of the file format
(and hledger's data model).
These are mostly in the order you'll use them, but in some cases
related concepts have been grouped together for easy reference,
or linked before they are introduced,
so feel free to skip over anything that looks unnecessary right now.

## Transactions

Transactions are the main unit of information in a journal file.
They represent events, typically a movement of some quantity of
commodities between two or more named accounts.

Each transaction is recorded as a journal entry, beginning with a
[simple date](#simple-dates) in column 0. This can be followed by any
of the following optional fields, separated by spaces:

- a [status](#status) character (empty, `!`, or `*`)
- a code (any short number or text, enclosed in parentheses)
- a description (any remaining text until end of line or a semicolon)
- a comment (any remaining text following a semicolon until end of line,
             and any following indented lines beginning with a semicolon)
- 0 or more indented *posting* lines, describing what was transferred and the accounts involved
  (indented comment lines are also allowed, but not blank lines or non-indented lines).

Here's a simple journal file containing one transaction:
```journal
2008/01/01 income
  assets:bank:checking   $1
  income:salary         $-1
```


## Dates

### Simple dates

Dates in the journal file use *simple dates* format:
`YYYY-MM-DD` or `YYYY/MM/DD` or `YYYY.MM.DD`, with leading zeros optional.
The year may be omitted, in which case it will be inferred from the context:
the current transaction, the default year set with a [default year directive](#default-year),
or the current date when the command is run.
Some examples: `2010-01-31`, `2010/01/31`, `2010.1.31`, `1/31`.

(The UI also accepts simple dates, as well as the more flexible [smart
dates](#smart-dates) documented in the hledger manual.)

### Secondary dates

Real-life transactions sometimes involve more than one date - eg the date
you write a cheque, and the date it clears in your bank.  When you want to
model this, for more accurate daily balances, you can specify individual
[posting dates](#posting-dates).

Or, you can use the older *secondary date* feature
(Ledger calls it auxiliary date or effective date).
Note: we support this for compatibility, but I usually recommend
avoiding this feature; posting dates are almost always clearer and
simpler.
<!-- (Secondary dates require you to remember to use them consistently in -->
<!-- your journal, and to choose them or not for each report.) -->

A secondary date is written after the primary date, following an
equals sign. If the year is omitted, the primary date's year is
assumed. When running reports, the primary (left) date is used by
default, but with the `--date2` flag (or `--aux-date` or
`--effective`), the secondary (right) date will be used instead.

The meaning of secondary dates is up to you, but it's best to follow a
consistent rule.  Eg "primary = the bank's clearing date, secondary =
date the transaction was initiated, if different", as shown here:
```journal
2010/2/23=2/19 movie ticket
  expenses:cinema                   $10
  assets:checking
```
```shell
$ hledger register checking
2010-02-23 movie ticket         assets:checking                $-10         $-10
```
```shell
$ hledger register checking --date2
2010-02-19 movie ticket         assets:checking                $-10         $-10
```

### Posting dates

You can give individual postings a different date from their parent
transaction, by adding a [posting comment](#comments) containing a
[tag](#tags) (see below) like `date:DATE`.  This is probably the best
way to control posting dates precisely. Eg in this example the expense
should appear in May reports, and the deduction from checking should
be reported on 6/1 for easy bank reconciliation:

```journal
2015/5/30
    expenses:food     $10  ; food purchased on saturday 5/30
    assets:checking        ; bank cleared it on monday, date:6/1
```

```shell
$ hledger -f t.j register food
2015-05-30                      expenses:food                  $10           $10
```

```shell
$ hledger -f t.j register checking
2015-06-01                      assets:checking               $-10          $-10
```

DATE should be a [simple date](#simple-dates); if the year is not
specified it will use the year of the transaction's date.  You can set
the secondary date similarly, with `date2:DATE2`.  The `date:` or
`date2:` tags must have a valid simple date value if they are present,
eg a `date:` tag with no value is not allowed.

Ledger's earlier, more compact bracketed date syntax is also
supported: `[DATE]`, `[DATE=DATE2]` or `[=DATE2]`. hledger will
attempt to parse any square-bracketed sequence of the `0123456789/-.=`
characters in this way. With this syntax, DATE infers its year from
the transaction and DATE2 infers its year from DATE.

## Status

Transactions, or individual postings within a transaction,
can have a status mark, which is a single character before
the transaction description or posting account name,
separated from it by a space, indicating one of three statuses:

| mark   | status   |
|--------|----------|
|        | unmarked |
| `!`    | pending  |
| `*`    | cleared  |

When reporting, you can filter by status with
the `-U/--unmarked`, `-P/--pending`, and `-C/--cleared` flags;
or the `status:`, `status:!`, and `status:*` [queries](#queries);
or the U, P, C keys in hledger-ui.

Note, in Ledger and in older versions of hledger, the "unmarked" state is called
"uncleared". As of hledger 1.3 we have renamed it to unmarked for clarity.

To replicate Ledger and old hledger's behaviour of also matching pending, combine -U and -P.

Status marks are optional, but can be helpful eg for reconciling with real-world accounts.
Some editor modes provide highlighting and shortcuts for working with status.
Eg in Emacs ledger-mode, you can toggle transaction status with C-c C-e, or posting status with C-c C-c.

What "uncleared", "pending", and "cleared" actually mean is up to you.
Here's one suggestion:

| status    | meaning                                                            |
|-----------|--------------------------------------------------------------------|
| uncleared | recorded but not yet reconciled; needs review                      |
| pending   | tentatively reconciled (if needed, eg during a big reconciliation) |
| cleared   | complete, reconciled as far as possible, and considered correct    |

With this scheme, you would use
`-PC` to see the current balance at your bank,
`-U` to see things which will probably hit your bank soon (like uncashed checks),
and no flags to see the most up-to-date state of your finances.

## Description

A transaction's description is the rest of the line following the date and status mark (or until a comment begins).
Sometimes called the "narration" in traditional bookkeeping, it can be used for whatever you wish,
or left blank. Transaction descriptions can be queried, unlike [comments](#comments).

### Payee and note

You can optionally include a `|` (pipe) character in descriptions to subdivide the description
into separate fields for payee/payer name on the left (up to the first `|`) and an additional note
field on the right (after the first `|`). This may be worthwhile if you need to do more precise
[querying](#queries) and [pivoting](#pivoting) by payee or by note.

## Comments

Lines in the journal beginning with a semicolon (`;`) or hash (`#`) or
star (`*`) are comments, and will be ignored. (Star comments cause
org-mode nodes to be ignored, allowing emacs users to fold and navigate
their journals with org-mode or orgstruct-mode.)

You can attach comments to a transaction by writing them after the
description and/or indented on the following lines (before the
postings).  Similarly, you can attach comments to an individual
posting by writing them after the amount and/or indented on the
following lines.
Transaction and posting comments must begin with a semicolon (`;`).

Some examples:

```journal
# a file comment
; another file comment
* also a file comment, useful in org/orgstruct mode

comment
A multiline file comment, which continues
until a line containing just "end comment"
(or end of file).
end comment

2012/5/14 something  ; a transaction comment
    ; the transaction comment, continued
    posting1  1  ; a comment for posting 1
    posting2
    ; a comment for posting 2
    ; another comment line for posting 2
; a file comment (because not indented)
```

You can also comment larger regions of a file using [`comment` and `end comment` directives](#comment-blocks).


## Tags

Tags are a way to add extra labels or labelled data to postings and transactions,
which you can then [search](#queries) or [pivot](#pivoting) on.

A simple tag is a word (which may contain hyphens) followed by a full colon,
written inside a transaction or posting [comment](#comments) line:
```journal
2017/1/16 bought groceries  ; sometag:
```

Tags can have a value, which is the text after the colon, up to the next comma or end of line, with leading/trailing whitespace removed:
```journal
    expenses:food    $10 ; a-posting-tag: the tag value
```

Note this means hledger's tag values can not contain commas or newlines.
Ending at commas means you can write multiple short tags on one line, comma separated:
```journal
    assets:checking  ; a comment containing tag1:, tag2: some value ...
```
Here,

- "`a comment containing `" is just comment text, not a tag
- "`tag1`" is a tag with no value
- "`tag2`" is another tag, whose value is "`some value ...`"

Tags in a transaction comment affect the transaction and all of its postings,
while tags in a posting comment affect only that posting.
For example, the following transaction has three tags (`A`, `TAG2`, `third-tag`)
and the posting has four (those plus `posting-tag`):

```journal
1/1 a transaction  ; A:, TAG2:
    ; third-tag: a third transaction tag, <- with a value
    (a)  $1  ; posting-tag:
```

Tags are like Ledger's
[metadata](http://ledger-cli.org/3.0/doc/ledger3.html#Metadata)
feature, except hledger's tag values are simple strings.

## Postings

A posting is an addition of some amount to, or removal of some amount from, an account.
Each posting line begins with at least one space or tab (2 or 4 spaces is common), followed by:

- (optional) a [status](#status) character (empty, `!`, or `*`), followed by a space
- (required) an [account name](#account-names) (any text, optionally containing **single spaces**, until end of line or a double space)
- (optional) **two or more spaces** or tabs followed by an [amount](#amounts).

Positive amounts are being added to the account, negative amounts are being removed.

The amounts within a transaction must always sum up to zero.
As a convenience, one amount may be left blank; it will be inferred so as to balance the transaction.

Be sure to note the unusual two-space delimiter between account name and amount.
This makes it easy to write account names containing spaces.
But if you accidentally leave only one space (or tab) before the amount, the amount will be considered part of the account name.

### Virtual postings

A posting with a parenthesised account name is called a *virtual posting*
or *unbalanced posting*, which means it is exempt from the usual rule
that a transaction's postings must balance add up to zero.

This is not part of double entry accounting, so you might choose to
avoid this feature. Or you can use it sparingly for certain special
cases where it can be convenient. Eg, you could set opening balances
without using a balancing equity account:

```journal
1/1 opening balances
  (assets:checking)   $1000
  (assets:savings)    $2000
```

A posting with a bracketed account name is called a *balanced virtual
posting*. The balanced virtual postings in a transaction must add up
to zero (separately from other postings). Eg:

```journal
1/1 buy food with cash, update budget envelope subaccounts, & something else
  assets:cash                    $-10 ; <- these balance
  expenses:food                    $7 ; <-
  expenses:food                    $3 ; <-
  [assets:checking:budget:food]  $-10    ; <- and these balance
  [assets:checking:available]     $10    ; <-
  (something:else)                 $5       ; <- not required to balance
```

Ordinary non-parenthesised, non-bracketed postings are called *real postings*.
You can exclude virtual postings from reports with the `-R/--real`
flag or `real:1` query.

## Account names

Account names typically have several parts separated by a full colon, from
which hledger derives a hierarchical chart of accounts. They can be
anything you like, but in finance there are traditionally five top-level
accounts: `assets`, `liabilities`, `revenue`, `expenses`, and `equity`.

Account names may contain single spaces, eg: `assets:accounts receivable`.
Because of this, they must always be followed by **two or more spaces** (or newline).

Account names can be [aliased](#rewriting-accounts).

## Amounts

After the account name, there is usually an amount.
(Important: between account name and amount, there must be **two or more spaces**.)

hledger's amount format is flexible, supporting several international formats.
Here are some examples.
Amounts have a number (the "quantity"):

    1

..and usually a currency symbol or commodity name (more on this
below), to the left or right of the quantity, with or without a
separating space:

    $1
    4000 AAPL
    3 "green apples"

Amounts can be preceded by a minus sign (or a plus sign, though plus is the default),
The sign can be written before or after a left-side commodity symbol:

    -$1
    $-1

One or more spaces between the sign and the number are acceptable
when parsing (but they won't be displayed in output):

    + $1
    $-      1

Scientific E notation is allowed:

    1E-6
    EUR 1E3

### Decimal marks, digit group marks

A decimal mark can be written as a period or a comma:

    1.23
    1,23456780000009

In the integer part of the quantity (left of the decimal mark), groups
of digits can optionally be separated by a "digit group mark" - a
space, comma, or period (different from the decimal mark):

         $1,000,000.00
      EUR 2.000.000,00
    INR 9,99,99,999.00
          1 000 000.9455

Note, a number containing a single digit group mark and no decimal mark is ambiguous.
Are these digit group marks or decimal marks ?

    1,000
    1.000

If you don't tell it otherwise, hledger will assume both of the above are decimal marks,
parsing both numbers as 1.
To prevent confusion and undetected typos, 
<!-- especially if your data contains digit group marks, -->
we recommend adding `commodity` directives at the top of your journal
file to explicitly declare the decimal mark (and optionally a digit
group mark) for each commodity. Read on for more about this.

### Commodity

Amounts in hledger have both a "quantity", which is a signed decimal
number, and a "commodity", which is a currency symbol, stock ticker,
or any word or phrase describing something you are tracking.

If the commodity name contains non-letters (spaces, numbers, or
punctuation), you must always write it inside double quotes (`"green
apples"`, `"ABC123"`).

If you write just a bare number, that too will have a commodity, with
name `""`; we call that the "no-symbol commodity".

Actually, hledger combines these single-commodity amounts into more
powerful multi-commodity amounts, which are what it works with most of
the time. A multi-commodity amount could be, eg: `1 USD, 2 EUR, 3.456 TSLA`. 
In practice, you will only see multi-commodity amounts in hledger's
output; you can't write them directly in the journal file. 
<!-- (Though an omitted balancing amount can be multi-commodity.) -->

(If you are writing scripts or working with hledger's internals, these
are the `Amount` and `MixedAmount` types.)

### Commodity directives

You can add `commodity` directives to the journal, preferably at the
top, to declare your commodities and help with number parsing (see
above) and display (see below). These are optional, but recommended.
They are described in more detail in JOURNAL FORMAT -> [Declaring
commodities](#declaring-commodities). Here's a quick example:

```journal
# number format and display style for $, EUR, INR and the no-symbol commodity:
commodity $1,000.00
commodity EUR 1.000,00
commodity INR 9,99,99,999.00
commodity 1 000 000.9455
```

<a name="amount-display-style"></a>

### Commodity display style

For the amounts in each commodity, hledger chooses a consistent display style to use in most reports.
(Exceptions: [price amounts](#transaction-prices), 
and all amounts displayed by the [`print`](#print) command,
are displayed with all of their decimal digits visible.)

A commodity's display style is inferred as follows. 

First, if a [default commodity](#default-commodity) is declared with
`D`, this commodity and its style is applied to any no-symbol amounts
in the journal.

Then each commodity's style is inferred from one of the following, in order of preference:

- The [commodity directive](#declaring-commodities) for that commodity
  (including the no-symbol commodity), if any.
- The amounts in that commodity seen in the journal's transactions.
  (Posting amounts only; prices and periodic or auto rules are ignored, currently.)
- The built-in fallback style, which looks like this: `$1000.00`.
  (Symbol on the left, period decimal mark, two decimal places.)

A style is inferred from journal amounts as follows:

- Use the general style (decimal mark, symbol placement) of the first amount
- Use the first-seen digit group style (digit group mark, digit group sizes), if any
- Use the maximum number of decimal places of all.

Transaction price amounts don't affect the commodity display style directly,
but occasionally they can do so indirectly (eg when a posting's amount is
inferred using a transaction price). If you find this causing
problems, use a commodity directive to fix the display style.

To summarise: each commodity's amounts will be normalised to (a) 
the style declared by a `commodity` directive, or (b)
the style of the first posting amount in the journal,
with the first-seen digit group style and the maximum-seen number of decimal places.
So if your reports are showing amounts in a way you don't like, eg 
with too many decimal places, use a commodity directive. Some examples:

```journal
# declare euro, dollar, bitcoin and no-symbol commodities and set their 
# input number formats and output display styles:
commodity EUR 1.000,
commodity $1000.00
commodity 1000.00000000 BTC
commodity 1 000.
```

### Rounding

Amounts are stored internally as decimal numbers with up to 255 decimal places,
and displayed with the number of decimal places specified by the commodity display style.
Note, hledger uses [banker's rounding](https://en.wikipedia.org/wiki/Bankers_rounding): 
it rounds to the nearest even number, eg 0.5 displayed with zero decimal places is "0").
(Guaranteed since hledger 1.17.1; in older versions this could vary  if hledger was built with Decimal < 0.5.1.)


## Transaction prices

Within a transaction, you can note an amount's price in another commodity.
This can be used to document the cost (in a purchase) or selling price (in a sale).
For example, transaction prices are useful to record purchases of a foreign currency.
Note transaction prices are fixed at the time of the transaction, and do not change over time.
See also [market prices](#declaring-market-prices), which represent prevailing exchange rates on a certain date.

There are several ways to record a transaction price:

1. Write the price per unit, as `@ UNITPRICE` after the amount:

    ```journal
    2009/1/1
      assets:euros     €100 @ $1.35  ; one hundred euros purchased at $1.35 each
      assets:dollars                 ; balancing amount is -$135.00
    ```

2. Write the total price, as `@@ TOTALPRICE` after the amount:

    ```journal
    2009/1/1
      assets:euros     €100 @@ $135  ; one hundred euros purchased at $135 for the lot
      assets:dollars
    ```

3. Specify amounts for all postings, using exactly two commodities,
   and let hledger infer the price that balances the transaction:

    ```journal
    2009/1/1
      assets:euros     €100          ; one hundred euros purchased
      assets:dollars  $-135          ; for $135
    ```

4. Like 1, but the `@` is parenthesised, i.e. `(@)`; this is for
   compatibility with Ledger journals
   ([Virtual posting costs](https://www.ledger-cli.org/3.0/doc/ledger3.html#Virtual-posting-costs)),
   and is equivalent to 1 in hledger.

5. Like 2, but as in 4 the `@@` is parenthesised, i.e. `(@@)`; in hledger,
   this is equivalent to 2.

Use the [`-B/--cost`](#reporting-options) flag to convert
amounts to their transaction price's commodity, if any.
(mnemonic: "B" is from "cost Basis", as in Ledger).
Eg here is how -B affects the balance report for the example above:

```shell
$ hledger bal -N --flat
               $-135  assets:dollars
                €100  assets:euros
$ hledger bal -N --flat -B
               $-135  assets:dollars
                $135  assets:euros    # <- the euros' cost
```

Note -B is sensitive to the order of postings when a transaction price is inferred:
the inferred price will be in the commodity of the last amount.
So if example 3's postings are reversed, while the transaction
is equivalent, -B shows something different:

```journal
2009/1/1
  assets:dollars  $-135              ; 135 dollars sold
  assets:euros     €100              ; for 100 euros
```
```shell
$ hledger bal -N --flat -B
               €-100  assets:dollars  # <- the dollars' selling price
                €100  assets:euros
```

## Lot prices, lot dates

Ledger allows another kind of price, 
[lot price](http://ledger-cli.org/3.0/doc/ledger3.html#Fixing-Lot-Prices)
(four variants: `{UNITPRICE}`, `{{{{TOTALPRICE}}}}`, `{=FIXEDUNITPRICE}`, `{{{{=FIXEDTOTALPRICE}}}}`),
and/or a lot date (`[DATE]`) to be specified.
These are normally used to select a lot when selling investments.
hledger will parse these, for compatibility with Ledger journals, but currently ignores them.
A [transaction price](#transaction-prices), lot price and/or lot date may appear in any order,
after the posting amount and before the balance assertion if any.

## Balance assertions

hledger supports
[Ledger-style balance assertions](http://ledger-cli.org/3.0/doc/ledger3.html#Balance-assertions)
in journal files.
These look like, for example, `= EXPECTEDBALANCE` following a posting's amount.
Eg here we assert the expected dollar balance in accounts a and b after
each posting:

```journal
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
the `-I/--ignore-assertions` flag, which can be useful for
troubleshooting or for reading Ledger files.
(Note: this flag currently does not disable balance assignments, below).

### Assertions and ordering

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

### Assertions and included files

With [included files](#including-other-files), things are a little
more complicated. Including preserves the ordering of postings and
assertions. If you have multiple postings to an account on the same
day, split across different files, and you also want to assert the
account's balance on the same day, you'll have to put the assertion
in the right file.

### Assertions and multiple -f options

Balance assertions don't work well across files specified
with multiple -f options. Use include or [concatenate the files](#input-files)
instead.

### Assertions and commodities

The asserted balance must be a simple single-commodity amount, and in
fact the assertion checks only this commodity's balance within the
(possibly multi-commodity) account balance.
This is how assertions work in Ledger also.
We could call this a "partial" balance assertion.

To assert the balance of more than one commodity in an account,
you can write multiple postings, each asserting one commodity's balance.

You can make a stronger "total" balance assertion by writing a
double equals sign (`== EXPECTEDBALANCE`).
This asserts that there are no other unasserted commodities in the account
(or, that their balance is 0).

``` journal
2013/1/1
  a   $1
  a    1€
  b  $-1
  c   -1€

2013/1/2  ; These assertions succeed
  a    0  =  $1
  a    0  =   1€
  b    0 == $-1
  c    0 ==  -1€

2013/1/3  ; This assertion fails as 'a' also contains 1€
  a    0 ==  $1
```

It's not yet possible to make a complete assertion about a balance that has multiple commodities.
One workaround is to isolate each commodity into its own subaccount:

``` journal
2013/1/1
  a:usd   $1
  a:euro   1€
  b

2013/1/2
  a        0 ==  0
  a:usd    0 == $1
  a:euro   0 ==  1€
```

### Assertions and prices

Balance assertions ignore [transaction prices](#transaction-prices),
and should normally be written without one:

``` journal
2019/1/1
  (a)     $1 @ €1 = $1
```

We do allow prices to be written there, however, and [print](#print) shows them,
even though they don't affect whether the assertion passes or fails.
This is for backward compatibility (hledger's [close](#close) command used to generate balance assertions with prices),
and because [balance *assignments*](#balance-assignments) do use them (see below).

### Assertions and subaccounts

The balance assertions above (`=` and `==`) do not count the balance
from subaccounts; they check the account's exclusive balance only.
You can assert the balance including subaccounts by writing `=*` or `==*`, eg:

```journal
2019/1/1
  equity:opening balances
  checking:a       5
  checking:b       5
  checking         1  ==* 11
```

### Assertions and virtual postings

Balance assertions are checked against all postings, both real and
[virtual](#virtual-postings). They are not affected by the `--real/-R`
flag or `real:` query.

### Assertions and precision

Balance assertions compare the exactly calculated amounts,
which are not always what is shown by reports.
Eg a [commodity directive](#declaring-commodities)
may limit the display precision, but this will not affect balance assertions.
Balance assertion failure messages show exact amounts.

## Balance assignments

[Ledger-style balance assignments](http://ledger-cli.org/3.0/doc/ledger3.html#Balance-assignments) are also supported.
These are like [balance assertions](#balance-assertions), but with no posting amount on the left side of the equals sign;
instead it is calculated automatically so as to satisfy the assertion.
This can be a convenience during data entry, eg when setting opening balances:
```journal
; starting a new journal, set asset account balances
2016/1/1 opening balances
  assets:checking            = $409.32
  assets:savings             = $735.24
  assets:cash                 = $42
  equity:opening balances
```
or when adjusting a balance to reality:
```journal
; no cash left; update balance, record any untracked spending as a generic expense
2016/1/15
  assets:cash    = $0
  expenses:misc
```

The calculated amount depends on the account's balance in the commodity at that point
(which depends on the previously-dated postings of the commodity to that account
since the last balance assertion or assignment).
Note that using balance assignments makes your journal a little less explicit;
to know the exact amount posted, you have to run hledger or do the calculations yourself,
instead of just reading it.

### Balance assignments and prices

A [transaction price](#transaction-prices) in a balance assignment
will cause the calculated amount to have that price attached:

``` journal
2019/1/1
  (a)             = $1 @ €2
```
```
$ hledger print --explicit
2019-01-01
    (a)         $1 @ €2 = $1 @ €2
```

## Directives

A directive is a line in the journal beginning with a special keyword,
that influences how the journal is processed.
hledger's directives are based on a subset of Ledger's, but there are many differences
(and also some differences between hledger versions).

Directives' behaviour and interactions can get a little bit [complex](https://github.com/simonmichael/hledger/issues/793),
so here is a table summarising the directives and their effects, with links to more detailed docs.
Note part of this table is hidden when viewed in a web browser - scroll it sideways to see more.

<!-- <style> -->
<!-- table a code { white-space:nowrap; } -->
<!-- h1,h2,h3,h4,h5,h6 { color:red; } -->
<!-- </style> -->

| directive         | end directive       | subdirectives   | purpose                                                            | can affect (as of 2018/06)
|-------------------|---------------------|-----------------|--------------------------------------------------------------------|----------------------------------------------
| [`account`]       |                     | any text        | document account names, declare account types & display order      | all entries in all files, before or after
| [`alias`]         | `end aliases`       |                 | rewrite account names                                              | following entries until end of current file or end directive
| [`apply account`] | `end apply account` |                 | prepend a common parent to account names                           | following entries until end of current file or end directive
| [`comment`]       | `end comment`       |                 | ignore part of journal                                             | following entries until end of current file or end directive
| [`commodity`]     |                     | `format`        | declare a commodity and its number notation & display style        | number notation: following entries in that commodity in all files <!-- or until end of current file ? -->; <br>display style: amounts of that commodity in reports
| [`D`]             |                     |                 | declare a commodity to be used for commodityless amounts, and its number notation & display style  | default commodity: following commodityless entries until end of current file; <br>number notation: following entries in that commodity until end of current file; <br>display style: amounts of that commodity in reports
| [`include`]       |                     |                 | include entries/directives from another file                       | what the included directives affect
| [`payee`]         |                     |                 | declare a payee name                                               | following entries until end of current file
| [`P`]             |                     |                 | declare a market price for a commodity                             | amounts of that commodity in reports, when -V is used
| [`Y`]             |                     |                 | declare a year for yearless dates                                  | following entries until end of current file
| [`=`]             |                     |                 | declare an auto posting rule, adding postings to other transactions | all entries in parent/current/child files (but not sibling files, see [#1212](https://github.com/simonmichael/hledger/issues/1212))

[`account`]:       #declaring-accounts
[`alias`]:         #rewriting-accounts
[`apply account`]: #default-parent-account
[`comment`]:       #comment-blocks
[`commodity`]:     #declaring-commodities
[`D`]:             #default-commodity
[`include`]:       #including-other-files
[`P`]:             #market-prices
[`Y`]:             #default-year
[`=`]:             #auto-postings

And some definitions:

|                 |                                                                                                                                                                                       |
|-----------------|---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| subdirective    | optional indented directive line immediately following a parent directive                                                                                                             |
| number notation | how to interpret numbers when parsing journal entries (the identity of the decimal separator character). (Currently each commodity can have its own notation, even in the same file.) |
| display style   | how to display amounts of a commodity in reports (symbol side and spacing, digit groups, decimal separator, decimal places)                                                           |
| directive scope | which entries and (when there are multiple files) which files are affected by a directive                                                                                             |

<!-- | **entries affected:**  | -->
<!-- | following     | subsequent entries in the file/parse stream -->
<!-- | delimited     | subsequent entries, until an optional end directive -->
<!-- | all           | all preceding and following entries -->
<!-- | **files affected:**    | -->
<!-- | current       | affects current file only -->
<!-- | children      | affects current file and files included by it -->
<!-- | siblings      | affects current file, included files, and other same-level files, but not higher-level files -->
<!-- | all           | affects all files -->

As you can see, directives vary in which journal entries and files they affect,
and whether they are focussed on input (parsing) or output (reports).
Some directives have multiple effects.

## Directives and multiple files

If you use multiple `-f`/`--file` options, or the `include` directive,
hledger will process multiple input files. But note that directives
which affect input (see above) typically last only until the end of
the file in which they occur. 

This may seem inconvenient, but it's intentional; it makes reports
stable and deterministic, independent of the order of input. Otherwise
you could see different numbers if you happened to write -f options in
a different order, or if you moved includes around while cleaning up
your files.

It can be surprising though; for example, it means that 
[`alias` directives do not affect parent or sibling files](#aliases-and-multiple-files)
(see below).

## Comment blocks

A line containing just `comment` starts a commented region of the file,
and a line containing just `end comment` (or the end of the current file) ends it.
See also [comments](#comments).

## Including other files

You can pull in the content of additional files by writing an include directive, like this:

```journal
include FILEPATH
```

Only journal files can include, and only journal, timeclock or timedot files can be included (not CSV files, currently).

If the file path does not begin with a slash, it is relative to the current file's folder. 

A tilde means home directory, eg: `include ~/main.journal`.

The path may contain [glob patterns] to match multiple files, eg: `include *.journal`.

There is limited support for recursive wildcards: `**/` (the slash is required)
matches 0 or more subdirectories. It's not super convenient since you have to 
avoid include cycles and including directories, but this can be done, eg:
`include */**/*.journal`.

The path may also be prefixed to force a specific file format,
overriding the file extension (as described in
[hledger.1 -> Input files](#input-files)):
`include timedot:~/notes/2020*.md`.

[glob patterns]: https://hackage.haskell.org/package/Glob-0.9.2/docs/System-FilePath-Glob.html#v:compile

## Default year

You can set a default year to be used for subsequent dates which don't
specify a year. This is a line beginning with `Y` followed by the year. Eg:

```journal
Y2009  ; set default year to 2009

12/15  ; equivalent to 2009/12/15
  expenses  1
  assets

Y2010  ; change default year to 2010

2009/1/30  ; specifies the year, not affected
  expenses  1
  assets

1/31   ; equivalent to 2010/1/31
  expenses  1
  assets
```

## Declaring payees

The `payee` directive can be used to declare a limited set of payees which may appear in transaction descriptions.
The ["payees" check](#check) will report an error if any transaction refers to a payee that has not been declared.
Eg:

```journal
payee Whole Foods
```

## Declaring commodities

You can use `commodity` directives to declare your commodities.
In fact the `commodity` directive performs several functions at once:

1. It declares commodities which may be used in the journal.
   This can optionally be enforced, providing useful error checking.
   (Cf [Commodity error checking](#commodity-error-checking))

2. It declares which decimal mark character (period or comma), to
   expect when parsing input - useful to disambiguate international
   number formats in your data. Without this, hledger will parse both
   `1,000` and `1.000` as 1. 
   (Cf [Amounts](#amounts))

3. It declares how to render the commodity's amounts when displaying
   output - the decimal mark, any digit group marks, the number of
   decimal places, symbol placement and so on.
   (Cf [Commodity display style](#commodity-display-style))

You will run into one of the problems solved by commodity directives
sooner or later, so we recommend using them, for robust and
predictable parsing and display.

Generally you should put them at the top of your journal file
(since for function 2, they affect only following amounts,
cf [#793](https://github.com/simonmichael/hledger/issues/793)).

A commodity directive is just the word `commodity` followed by a
sample [amount](#amounts), like this:

```journal
;commodity SAMPLEAMOUNT

commodity $1000.00
commodity 1,000.0000 AAAA  ; optional same-line comment
```

It may also be written on multiple lines, and use the `format`
subdirective, as in Ledger. Note in this case the commodity symbol
appears twice; it must be the same in both places:

```journal
;commodity SYMBOL
;  format SAMPLEAMOUNT

; display indian rupees with currency name on the left,
; thousands, lakhs and crores comma-separated,
; period as decimal point, and two decimal places.
commodity INR
  format INR 1,00,00,000.00
```

Remember that if the commodity symbol contains spaces, numbers, or
punctuation, it must be enclosed in double quotes (cf [Commodity](#commodity)).

The amount's quantity does not matter; only the format is significant.
It must include a decimal mark - either a period or a comma - followed
by 0 or more decimal digits.

A few more examples:
```journal
# number formats for $, EUR, INR and the no-symbol commodity:
commodity $1,000.00
commodity EUR 1.000,00
commodity INR 9,99,99,999.0
commodity 1 000 000.
```

Note hledger normally uses 
[banker's rounding](https://en.wikipedia.org/wiki/Bankers_rounding), 
so 0.5 displayed with zero decimal digits is "0". 
(More at [Commodity display style](#commodity-display-style).)

### Commodity error checking

In [strict mode], enabled with the `-s`/`--strict` flag, hledger will report an error if a
commodity symbol is used that has not been declared by a [`commodity` directive](#declaring-commodities). This works similarly to [account error checking](#account-error-checking), see the notes there for more details.

## Default commodity

The `D` directive sets a default commodity, to be used for any
subsequent commodityless amounts (ie, plain numbers) seen while
parsing the journal. This effect lasts until the next `D` directive,
or the end of the journal.

For compatibility/historical reasons, `D` also acts like a [`commodity` directive](#declaring-commodities)
(setting the commodity's decimal mark for parsing and [display style](#amount-display-format) for output).

As with `commodity`, the amount must include a decimal mark (either period or comma).
If both `commodity` and `D` directives are used for the same commodity, the `commodity` style takes precedence.

The syntax is `D AMOUNT`. Eg:
```journal
; commodity-less amounts should be treated as dollars
; (and displayed with the dollar sign on the left, thousands separators and two decimal places)
D $1,000.00

1/1
  a     5  ; <- commodity-less amount, parsed as $5 and displayed as $5.00
  b
```

## Declaring market prices

The `P` directive declares a market price, which is
an exchange rate between two commodities on a certain date.
(In Ledger, they are called "historical prices".)
These are often obtained from a
[stock exchange](https://en.wikipedia.org/wiki/Stock_exchange),
cryptocurrency exchange, or the
[foreign exchange market](https://en.wikipedia.org/wiki/Foreign_exchange_market).

Here is the format:

```journal
P DATE COMMODITYA COMMODITYBAMOUNT
```
- DATE is a [simple date](#simple-dates)
- COMMODITYA is the symbol of the commodity being priced
- COMMODITYBAMOUNT is an [amount](#amounts) (symbol and quantity) in a
  second commodity, giving the price in commodity B of one unit of commodity A.

These two market price directives say that one euro was worth 1.35 US dollars during 2009,
and $1.40 from 2010 onward:
```journal
P 2009/1/1 € $1.35
P 2010/1/1 € $1.40
```

The `-V`, `-X` and `--value` flags use these market prices to show amount values
in another commodity. See [Valuation](#valuation).

## Declaring accounts

`account` directives can be used to declare accounts 
(ie, the places that amounts are transferred from and to).
Though not required, these declarations can provide several benefits:

- They can document your intended chart of accounts, providing a reference.
- They can help hledger know your accounts' types (asset, liability, equity, revenue, expense),
  useful for reports like balancesheet and incomestatement.
- They control account display order in reports, allowing non-alphabetic sorting
  (eg Revenues to appear above Expenses).
- They can store extra information about accounts (account numbers, notes, etc.)
- They help with account name completion
  in the add command, hledger-iadd, hledger-web, ledger-mode etc.
- In [strict mode], they restrict which accounts may be posted to by transactions,
  which helps detect typos.

The simplest form is just the word `account` followed by a hledger-style
[account name](#account-names), eg this account directive declares the `assets:bank:checking` account: 

```journal
account assets:bank:checking
```

### Account error checking

By default, accounts come into existence when a transaction references them by name.
This is convenient, but it means hledger can't warn you when you mis-spell an account name in the journal.
Usually you'll find the error later, as an extra account in balance reports, 
or an incorrect balance when reconciling.

In [strict mode], enabled with the `-s`/`--strict` flag, hledger will report an error if any transaction uses an account name that has not been declared by an [account directive](#declaring-accounts). Some notes:

- The declaration is case-sensitive; transactions must use the correct account name capitalisation.
- The account directive's scope is "whole file and below" (see [directives](#directives)). This means it affects all of the current file, and any files it includes, but not parent or sibling files. The position of account directives within the file does not matter, though it's usual to put them at the top.
- Accounts can only be declared in `journal` files (but will affect included files in other formats).
- It's currently not possible to declare "all possible subaccounts" with a wildcard; every account posted to must be declared.

### Account comments

[Comments](#comments), beginning with a semicolon, can be added:

- on the same line, **after two or more spaces**
  (because ; is allowed in account names)
- on the next lines, indented

An example of both:
```journal
account assets:bank:checking  ; same-line comment, note 2+ spaces before ;
  ; next-line comment
  ; another with tag, acctno:12345 (not used yet)
```

Same-line comments are not supported by Ledger, or hledger <1.13.

<!-- Account comments may include [tags](#tags), though we don't yet use them for anything. -->

### Account subdirectives

We also allow (and ignore) Ledger-style indented subdirectives, just for compatibility.:
```journal
account assets:bank:checking
  format blah blah  ; <- subdirective, ignored
```

Here is the full syntax of account directives:
```journal
account ACCTNAME  [ACCTTYPE] [;COMMENT]
  [;COMMENTS]
  [LEDGER-STYLE SUBDIRECTIVES, IGNORED]
```

### Account types

hledger recognises five main types of account,
corresponding to the account classes in the [accounting equation]:

`Asset`, `Liability`, `Equity`, `Revenue`, `Expense`.

These account types are important for controlling which accounts
appear in the [balancesheet], [balancesheetequity],
[incomestatement] reports (and probably for other things in future).

Additionally, we recognise the `Cash` type, which is also an `Asset`,
and which causes accounts to appear in the [cashflow] report.
("Cash" here means [liquid assets][CCE], eg bank balances
but typically not investments or receivables.)

[accounting equation]: https://en.wikipedia.org/wiki/Accounting_equation
[CCE]:                 https://en.wikipedia.org/wiki/Cash_and_cash_equivalents

#### Declaring account types

Generally, to make these reports work you should declare your
top-level accounts and their types, 
using [account directives](#declaring-accounts) 
with `type:` [tags](#tags).

The tag's value should be one of:
`Asset`, `Liability`, `Equity`, `Revenue`, `Expense`, `Cash`,
`A`, `L`, `E`, `R`, `X`, `C` (all case insensitive).
The type is inherited by all subaccounts except where they override it.
Here's a complete example:

```journal
account assets       ; type: Asset
account assets:bank  ; type: Cash
account assets:cash  ; type: Cash
account liabilities  ; type: Liability
account equity       ; type: Equity
account revenues     ; type: Revenue
account expenses     ; type: Expense
```

#### Auto-detected account types

If you happen to use common english top-level account names, you may
not need to declare account types, as they will be detected
automatically using the following rules:

| If name matches this [regular expression]: | account type is:
|----------------------------------------------|-----------------
| `^assets?(:|$)`                              | `Asset`
| `^(debts?|liabilit(y|ies))(:|$)`             | `Liability`
| `^equity(:|$)`                               | `Equity`
| `^(income|revenue)s?(:|$)`                   | `Revenue`
| `^expenses?(:|$)`                            | `Expense`

| If account type is `Asset` and name does not contain this regular expression: | account type is:
|-------------------------------------------------------------------------------|-----------------
| `(investment|receivable|:A/R|:fixed)`                                         | `Cash`

Even so, explicit declarations may be a good idea, for clarity and
predictability. 

#### Interference from auto-detected account types

If you assign any account type, it's a good idea to assign all of
them, to prevent any confusion from mixing declared and auto-detected
types. Although it's unlikely to happen in real life, here's an
example: with the following journal, `balancesheetequity` shows
"liabilities" in both Liabilities and Equity sections. Declaring another
account as `type:Liability` would fix it:

```journal
account liabilities  ; type:Equity

2020-01-01
  assets        1
  liabilities   1
  equity       -2
```

#### Old account type syntax

In some hledger journals you might instead see this old syntax (the
letters ALERX, separated from the account name by two or more spaces);
this is deprecated and may be removed soon:

```journal
account assets       A
account liabilities  L
account equity       E
account revenues     R
account expenses     X
```

### Account display order

Account directives also set the order in which accounts are displayed,
eg in reports, the hledger-ui accounts screen, and the hledger-web sidebar.
By default accounts are listed in alphabetical order.
But if you have these account directives in the journal:
```journal
account assets
account liabilities
account equity
account revenues
account expenses
```

you'll see those accounts displayed in declaration order, not alphabetically:
```shell
$ hledger accounts -1
assets
liabilities
equity
revenues
expenses
```

Undeclared accounts, if any, are displayed last, in alphabetical order.

Note that sorting is done at each level of the account tree (within each group of sibling accounts under the same parent).
And currently, this directive:
```journal
account other:zoo
```
would influence the position of `zoo` among `other`'s subaccounts, but not the position of `other` among the top-level accounts.
This means:

- you will sometimes declare parent accounts (eg `account other` above) that you don't intend to post to, just to customize their display order
- sibling accounts stay together (you couldn't display `x:y` in between `a:b` and `a:c`).

## Rewriting accounts

You can define account alias rules which rewrite your account names, or parts of them,
before generating reports.
This can be useful for:

- expanding shorthand account names to their full form, allowing easier data entry and a less verbose journal
- adapting old journals to your current chart of accounts
- experimenting with new account organisations, like a new hierarchy or combining two accounts into one
- customising reports

Account aliases also rewrite account names in [account directives](#declaring-accounts).
They do not affect account names being entered via hledger add or hledger-web.

See also [Rewrite account names](rewrite-account-names.html).

### Basic aliases

To set an account alias, use the `alias` directive in your journal file.
This affects all subsequent journal entries in the current file or its
[included files](#including-other-files)
(but note: [not sibling or parent files](#aliases-and-multiple-files)).
The spaces around the = are optional:

```journal
alias OLD = NEW
```

Or, you can use the `--alias 'OLD=NEW'` option on the command line.
This affects all entries. It's useful for trying out aliases interactively.

OLD and NEW are case sensitive full account names.
hledger will replace any occurrence of the old account name with the
new one. Subaccounts are also affected. Eg:

```journal
alias checking = assets:bank:wells fargo:checking
; rewrites "checking" to "assets:bank:wells fargo:checking", or "checking:a" to "assets:bank:wells fargo:checking:a"
```

### Regex aliases

There is also a more powerful variant that uses a [regular expression],
indicated by the forward slashes:

```journal
alias /REGEX/ = REPLACEMENT
```

or `--alias '/REGEX/=REPLACEMENT'`.

<!-- (Can also be written `'/REGEX/REPLACEMENT/'`). -->
REGEX is a case-insensitive regular expression. Anywhere it matches
inside an account name, the matched part will be replaced by
REPLACEMENT.
If REGEX contains parenthesised match groups, these can be referenced
by the usual numeric backreferences in REPLACEMENT.
Eg:

```journal
alias /^(.+):bank:([^:]+):(.*)/ = \1:\2 \3
; rewrites "assets:bank:wells fargo:checking" to  "assets:wells fargo checking"
```

Also note that REPLACEMENT continues to the end of line (or on command line,
to end of option argument), so it can contain trailing whitespace.

### Combining aliases

You can define as many aliases as you like, using journal directives and/or command line options.

Recursive aliases - where an account name is rewritten by one alias, then by another alias, and so on - are allowed.
Each alias sees the effect of previously applied aliases.

In such cases it can be important to understand which aliases will be applied and in which order.
For (each account name in) each journal entry, we apply:

1. `alias` directives preceding the journal entry, most recently parsed first (ie, reading upward from the journal entry, bottom to top)
2. `--alias` options, in the order they appeared on the command line (left to right).

In other words, for (an account name in) a given journal entry:

- the nearest alias declaration before/above the entry is applied first
- the next alias before/above that will be be applied next, and so on
- aliases defined after/below the entry do not affect it.

This gives nearby aliases precedence over distant ones, and helps
provide semantic stability - aliases will keep working the same way
independent of which files are being read and in which order.

In case of trouble, adding `--debug=6` to the command line will show which aliases are being applied when.

### Aliases and multiple files

As explained at [Directives and multiple files](#directives-and-multiple-files),
`alias` directives do not affect parent or sibling files. Eg in this command,
```shell
hledger -f a.aliases -f b.journal
```
account aliases defined in a.aliases will not affect b.journal. 
Including the aliases doesn't work either:
```journal
include a.aliases

2020-01-01  ; not affected by a.aliases
  foo  1
  bar
```
This means that account aliases should usually be declared at the
start of your top-most file, like this:
```journal
alias foo=Foo
alias bar=Bar

2020-01-01  ; affected by aliases above
  foo  1
  bar

include c.journal  ; also affected
```

### `end aliases`

You can clear (forget) all currently defined aliases with the `end
aliases` directive:

```journal
end aliases
```

## Default parent account

You can specify a parent account which will be prepended to all accounts
within a section of the journal. Use the `apply account` and `end apply account`
directives like so:

```journal
apply account home

2010/1/1
    food    $10
    cash

end apply account
```
which is equivalent to:
```journal
2010/01/01
    home:food           $10
    home:cash          $-10
```

If `end apply account` is omitted, the effect lasts to the end of the file.
Included files are also affected, eg:

```journal
apply account business
include biz.journal
end apply account
apply account personal
include personal.journal
```

Prior to hledger 1.0, legacy `account` and `end` spellings were also supported.

A default parent account also affects [account directives](#declaring-accounts).
It does not affect account names being entered via hledger add or hledger-web.
If account aliases are present, they are applied after the default parent account.

## Periodic transactions

Periodic transaction rules describe transactions that recur.
They allow hledger to generate temporary future transactions to help with forecasting,
so you don't have to write out each one in the journal,
and it's easy to try out different forecasts.

Periodic transactions can be a little tricky, so before you use them,
read this whole section - or at least these tips:

1. Two spaces accidentally added or omitted will cause you trouble - read about this below.
2. For troubleshooting, show the generated transactions with `hledger print --forecast tag:generated` or `hledger register --forecast tag:generated`.
3. Forecasted transactions will begin only after the last non-forecasted transaction's date.
4. Forecasted transactions will end 6 months from today, by default. See below for the exact start/end rules.
5. [period expressions](#period-expressions) can be tricky. Their documentation needs improvement, but is worth studying.
6. Some period expressions with a repeating interval must begin on a natural boundary of that interval.
   Eg in `weekly from DATE`, DATE must be a monday. `~ weekly from 2019/10/1` (a tuesday) will give an error.
7. Other period expressions with an interval are automatically expanded to cover a whole number of that interval.
   (This is done to improve reports, but it also affects periodic transactions. Yes, it's a bit inconsistent with the above.)
   Eg: <br>
   `~ every 10th day of month from 2020/01`, which is equivalent to <br>
   `~ every 10th day of month from 2020/01/01`, will be adjusted to start on 2019/12/10.

Periodic transaction rules also have a second meaning:
they are used to define budget goals, shown in [budget reports](#budget-report).


### Periodic rule syntax

A periodic transaction rule looks like a normal journal entry,
with the date replaced by a tilde (`~`) followed by a
[period expression](#period-expressions)
(mnemonic: `~` looks like a recurring sine wave.):
```journal
~ monthly
    expenses:rent          $2000
    assets:bank:checking
```
There is an additional constraint on the period expression:
the start date must fall on a natural boundary of the interval.
Eg `monthly from 2018/1/1` is valid, but `monthly from 2018/1/15` is not.

Partial or relative dates (M/D, D, tomorrow, last week) in the period expression
can work (useful or not). They will be relative to today's date, unless
a Y default year directive is in effect, in which case they will be relative to Y/1/1.

### Two spaces between period expression and description!

If the period expression is followed by a transaction description,
these must be separated by **two or more spaces**.
This helps hledger know where the period expression ends, so that descriptions
can not accidentally alter their meaning, as in this example:

```
; 2 or more spaces needed here, so the period is not understood as "every 2 months in 2020"
;               ||
;               vv
~ every 2 months  in 2020, we will review
    assets:bank:checking   $1500
    income:acme inc
```

So,

- Do write two spaces between your period expression and your transaction description, if any.
- Don't accidentally write two spaces in the middle of your period expression.

### Forecasting with periodic transactions

The `--forecast` flag activates any periodic transaction rules in the journal.
They will generate temporary recurring transactions,
which are not saved in the journal, but will appear in all reports
(eg [print](#print)).
This can be useful for estimating balances into the future,
or experimenting with different scenarios.
Or, it can be used as a data entry aid: describe recurring
transactions, and every so often copy the output of `print --forecast`
into the journal.

These transactions will have an extra [tag](#tags)
indicating which periodic rule generated them:
`generated-transaction:~ PERIODICEXPR`.
And a similar, hidden tag (beginning with an underscore) which,
because it's never displayed by print, can be used to match
transactions generated "just now":
`_generated-transaction:~ PERIODICEXPR`.

Periodic transactions are generated within some forecast period.
By default, this

- begins on the later of
  - the report start date if specified with -b/-p/date:
  - the day after the latest normal (non-periodic) transaction in the journal,
    or today if there are no normal transactions.

- ends on the report end date if specified with -e/-p/date:,
  or 6 months (180 days) from today.

This means that periodic transactions will begin only after the latest
recorded transaction. And a recorded transaction dated in the future can
prevent generation of periodic transactions.
(You can avoid that by writing the future transaction as a one-time
periodic rule instead - put tilde before the date, eg `~ YYYY-MM-DD ...`).

Or, you can set your own arbitrary "forecast period", which can
overlap recorded transactions, and need not be in the future, by
providing an option argument, like `--forecast=PERIODEXPR`.
Note the equals sign is required, a space won't work.
PERIODEXPR is a [period expression](#period-expressions),
which can specify the start date, end date, or both,
like in a [`date:` query](#queries).
(See also hledger.1 -> [Report start & end date](#report-start-end-date)).
Some examples: `--forecast=202001-202004`, `--forecast=jan-`, `--forecast=2020`.

### Budgeting with periodic transactions

With the `--budget` flag, currently supported by the balance command,
each periodic transaction rule declares recurring budget goals for the specified accounts.
Eg the first example above declares a goal of spending $2000 on rent
(and also, a goal of depositing $2000 into checking) every month.
Goals and actual performance can then be compared in [budget reports](#budget-report).

See also: [Budgeting and Forecasting](budgeting-and-forecasting.html).


<a name="automated-postings"></a>
<a name="auto-postings"></a>

## Auto postings

"Automated postings" or "auto postings" are extra postings which get
added automatically to transactions which match certain queries,
defined by "auto posting rules", when you use the `--auto` flag.

An auto posting rule looks a bit like a transaction:
```journal
= QUERY
    ACCOUNT  AMOUNT
    ...
    ACCOUNT  [AMOUNT]
```
except the first line is an equals sign (mnemonic: `=` suggests matching),
followed by a [query](#queries) (which matches existing postings),
and each "posting" line describes a posting to be generated, 
and the posting amounts can be:

- a normal amount with a commodity symbol, eg `$2`. This will be used as-is.
- a number, eg `2`. The commodity symbol (if any) from the matched
  posting will be added to this.
- a numeric multiplier, eg `*2` (a star followed by a number N). The
  matched posting's amount (and total price, if any) will be
  multiplied by N.
- a multiplier with a commodity symbol, eg `*$2` (a star, number N,
  and symbol S). The matched posting's amount will be multiplied by N,
  and its commodity symbol will be replaced with S.

Any query term containing spaces must be enclosed in single or double
quotes, as on the command line. Eg, note the quotes around the second query term below:
```journal
= expenses:groceries 'expenses:dining out'
    (budget:funds:dining out)                 *-1
```

Some examples:
```journal
; every time I buy food, schedule a dollar donation
= expenses:food
    (liabilities:charity)   $-1

; when I buy a gift, also deduct that amount from a budget envelope subaccount
= expenses:gifts
    assets:checking:gifts  *-1
    assets:checking         *1

2017/12/1
  expenses:food    $10
  assets:checking

2017/12/14
  expenses:gifts   $20
  assets:checking
```
```shell
$ hledger print --auto
2017-12-01
    expenses:food              $10
    assets:checking
    (liabilities:charity)      $-1

2017-12-14
    expenses:gifts             $20
    assets:checking
    assets:checking:gifts     -$20
    assets:checking            $20
```

### Auto postings and multiple files

An auto posting rule can affect any transaction in the current file,
or in any parent file or child file. Note, currently it will not
affect sibling files (when multiple `-f`/`--file` are used - see
[#1212](https://github.com/simonmichael/hledger/issues/1212)).

### Auto postings and dates

A [posting date](#posting-dates) (or secondary date) in the matched posting,
or (taking precedence) a posting date in the auto posting rule itself,
will also be used in the generated posting.

### Auto postings and transaction balancing / inferred amounts / balance assertions

Currently, auto postings are added:

- after [missing amounts are inferred, and transactions are checked for balancedness](#postings),
- but before [balance assertions](#balance-assertions) are checked.

Note this means that journal entries must be balanced both before and
after auto postings are added. This changed in hledger 1.12+; see
[#893](https://github.com/simonmichael/hledger/issues/893) for
background.

### Auto posting tags

Automated postings will have some extra [tags](#tags-1):

- `generated-posting:= QUERY`  - shows this was generated by an auto posting rule, and the query
- `_generated-posting:= QUERY` - a hidden tag, which does not appear in hledger's output.
                                     This can be used to match postings generated "just now",
                                     rather than generated in the past and saved to the journal.

Also, any transaction that has been changed by auto posting rules will have these tags added:

- `modified:` - this transaction was modified
- `_modified:` - a hidden tag not appearing in the comment; this transaction was modified "just now".



# CSV FORMAT

How hledger reads CSV data, and the CSV rules file format.

hledger can read [CSV](http://en.wikipedia.org/wiki/Comma-separated_values) files
(Character Separated Value - usually comma, semicolon, or tab) 
containing dated records as if they were journal files, 
automatically converting each CSV record into a transaction.

(To learn about *writing* CSV, see [CSV output](#csv-output).)

We describe each CSV file's format with a corresponding *rules file*.
By default this is named like the CSV file with a `.rules` extension
added. Eg when reading `FILE.csv`, hledger also looks for
`FILE.csv.rules` in the same directory as `FILE.csv`. You can specify a different
rules file with the `--rules-file` option. If a rules file is not
found, hledger will create a sample rules file, which you'll need to
adjust.

This file contains rules describing the CSV data (header line, fields
layout, date format etc.), and how to construct hledger journal
entries (transactions) from it. Often there will also be a list of
conditional rules for categorising transactions based on their
descriptions. Here's an overview of the CSV rules;
these are described more fully below, after the examples:

|                                           |                                                         |
|-------------------------------------------|---------------------------------------------------------|
| [**`skip`**](#skip)                       | skip one or more header lines or matched CSV records    |
| [**`fields`**](#fields)                   | name CSV fields, assign them to hledger fields          |
| [**field assignment**](#field-assignment) | assign a value to one hledger field, with interpolation |
| [**`separator`**](#separator)             | a custom field separator                                |
| [**`if` block**](#if-block)               | apply some rules to CSV records matched by patterns     |
| [**`if` table**](#if-table)               | apply some rules to CSV records matched by patterns, alternate syntax |
| [**`end`**](#end)                         | skip the remaining CSV records                          |
| [**`date-format`**](#date-format)         | how to parse dates in CSV records                       |
| [**`decimal-mark`**](#decimal-mark)       | the decimal mark used in CSV amounts, if ambiguous      |
| [**`newest-first`**](#newest-first)       | disambiguate record order when there's only one date    |
| [**`include`**](#include)                 | inline another CSV rules file                           |
| [**`balance-type`**](#balance-type)       | choose which type of balance assignments to use         |

Note, for best error messages when reading CSV files, use a `.csv`, `.tsv` or `.ssv`
file extension or file prefix - see [File Extension](#file-extension) below.

There's an introductory [Convert CSV files](convert-csv-files.html) tutorial on hledger.org.

## Examples

Here are some sample hledger CSV rules files. See also the full collection at:\
<https://github.com/simonmichael/hledger/tree/master/examples/csv>

### Basic

At minimum, the rules file must identify the date and amount fields,
and often it also specifies the date format and how many header lines
there are. Here's a simple CSV file and a rules file for it:
```csv
Date, Description, Id, Amount
12/11/2019, Foo, 123, 10.23
```
```rules
# basic.csv.rules
skip         1
fields       date, description, _, amount
date-format  %d/%m/%Y
```
```shell
$ hledger print -f basic.csv
2019-11-12 Foo
    expenses:unknown           10.23
    income:unknown            -10.23

```
Default account names are chosen, since we didn't set them.

### Bank of Ireland

Here's a CSV with two amount fields (Debit and Credit), and a balance field,
which we can use to add balance assertions, which is not necessary but
provides extra error checking:

```csv
Date,Details,Debit,Credit,Balance
07/12/2012,LODGMENT       529898,,10.0,131.21
07/12/2012,PAYMENT,5,,126
```
```rules
# bankofireland-checking.csv.rules

# skip the header line
skip

# name the csv fields, and assign some of them as journal entry fields
fields  date, description, amount-out, amount-in, balance

# We generate balance assertions by assigning to "balance"
# above, but you may sometimes need to remove these because:
#
# - the CSV balance differs from the true balance,
#   by up to 0.0000000000005 in my experience
#
# - it is sometimes calculated based on non-chronological ordering,
#   eg when multiple transactions clear on the same day

# date is in UK/Ireland format
date-format  %d/%m/%Y

# set the currency
currency  EUR

# set the base account for all txns
account1  assets:bank:boi:checking
```
```shell
$ hledger -f bankofireland-checking.csv print
2012-12-07 LODGMENT       529898
    assets:bank:boi:checking         EUR10.0 = EUR131.2
    income:unknown                  EUR-10.0

2012-12-07 PAYMENT
    assets:bank:boi:checking         EUR-5.0 = EUR126.0
    expenses:unknown                  EUR5.0

```
The balance assertions don't raise an error above, because we're
reading directly from CSV, but they will be checked if these entries
are imported into a journal file.

### Amazon

Here we convert amazon.com order history, and use an if block to
generate a third posting if there's a fee.
(In practice you'd probably get this data from your bank instead,
but it's an example.)

```csv
"Date","Type","To/From","Name","Status","Amount","Fees","Transaction ID"
"Jul 29, 2012","Payment","To","Foo.","Completed","$20.00","$0.00","16000000000000DGLNJPI1P9B8DKPVHL"
"Jul 30, 2012","Payment","To","Adapteva, Inc.","Completed","$25.00","$1.00","17LA58JSKRD4HDGLNJPI1P9B8DKPVHL"
```
```rules
# amazon-orders.csv.rules

# skip one header line
skip 1

# name the csv fields, and assign the transaction's date, amount and code.
# Avoided the "status" and "amount" hledger field names to prevent confusion.
fields date, _, toorfrom, name, amzstatus, amzamount, fees, code

# how to parse the date
date-format %b %-d, %Y

# combine two fields to make the description
description %toorfrom %name

# save the status as a tag
comment     status:%amzstatus

# set the base account for all transactions
account1    assets:amazon
# leave amount1 blank so it can balance the other(s).
# I'm assuming amzamount excludes the fees, don't remember

# set a generic account2
account2    expenses:misc
amount2     %amzamount
# and maybe refine it further:
#include categorisation.rules

# add a third posting for fees, but only if they are non-zero.
if %fees [1-9]
 account3    expenses:fees
 amount3     %fees
```
```shell
$ hledger -f amazon-orders.csv print
2012-07-29 (16000000000000DGLNJPI1P9B8DKPVHL) To Foo.  ; status:Completed
    assets:amazon
    expenses:misc          $20.00

2012-07-30 (17LA58JSKRD4HDGLNJPI1P9B8DKPVHL) To Adapteva, Inc.  ; status:Completed
    assets:amazon
    expenses:misc          $25.00
    expenses:fees           $1.00

```

### Paypal

Here's a real-world rules file for (customised) Paypal CSV,
with some Paypal-specific rules, and a second rules file included:

```csv
"Date","Time","TimeZone","Name","Type","Status","Currency","Gross","Fee","Net","From Email Address","To Email Address","Transaction ID","Item Title","Item ID","Reference Txn ID","Receipt ID","Balance","Note"
"10/01/2019","03:46:20","PDT","Calm Radio","Subscription Payment","Completed","USD","-6.99","0.00","-6.99","simon@joyful.com","memberships@calmradio.com","60P57143A8206782E","MONTHLY - $1 for the first 2 Months: Me - Order 99309. Item total: $1.00 USD first 2 months, then $6.99 / Month","","I-R8YLY094FJYR","","-6.99",""
"10/01/2019","03:46:20","PDT","","Bank Deposit to PP Account ","Pending","USD","6.99","0.00","6.99","","simon@joyful.com","0TU1544T080463733","","","60P57143A8206782E","","0.00",""
"10/01/2019","08:57:01","PDT","Patreon","PreApproved Payment Bill User Payment","Completed","USD","-7.00","0.00","-7.00","simon@joyful.com","support@patreon.com","2722394R5F586712G","Patreon* Membership","","B-0PG93074E7M86381M","","-7.00",""
"10/01/2019","08:57:01","PDT","","Bank Deposit to PP Account ","Pending","USD","7.00","0.00","7.00","","simon@joyful.com","71854087RG994194F","Patreon* Membership","","2722394R5F586712G","","0.00",""
"10/19/2019","03:02:12","PDT","Wikimedia Foundation, Inc.","Subscription Payment","Completed","USD","-2.00","0.00","-2.00","simon@joyful.com","tle@wikimedia.org","K9U43044RY432050M","Monthly donation to the Wikimedia Foundation","","I-R5C3YUS3285L","","-2.00",""
"10/19/2019","03:02:12","PDT","","Bank Deposit to PP Account ","Pending","USD","2.00","0.00","2.00","","simon@joyful.com","3XJ107139A851061F","","","K9U43044RY432050M","","0.00",""
"10/22/2019","05:07:06","PDT","Noble Benefactor","Subscription Payment","Completed","USD","10.00","-0.59","9.41","noble@bene.fac.tor","simon@joyful.com","6L8L1662YP1334033","Joyful Systems","","I-KC9VBGY2GWDB","","9.41",""
```

```rules
# paypal-custom.csv.rules

# Tips:
# Export from Activity -> Statements -> Custom -> Activity download
# Suggested transaction type: "Balance affecting"
# Paypal's default fields in 2018 were:
# "Date","Time","TimeZone","Name","Type","Status","Currency","Gross","Fee","Net","From Email Address","To Email Address","Transaction ID","Shipping Address","Address Status","Item Title","Item ID","Shipping and Handling Amount","Insurance Amount","Sales Tax","Option 1 Name","Option 1 Value","Option 2 Name","Option 2 Value","Reference Txn ID","Invoice Number","Custom Number","Quantity","Receipt ID","Balance","Address Line 1","Address Line 2/District/Neighborhood","Town/City","State/Province/Region/County/Territory/Prefecture/Republic","Zip/Postal Code","Country","Contact Phone Number","Subject","Note","Country Code","Balance Impact"
# This rules file assumes the following more detailed fields, configured in "Customize report fields":
# "Date","Time","TimeZone","Name","Type","Status","Currency","Gross","Fee","Net","From Email Address","To Email Address","Transaction ID","Item Title","Item ID","Reference Txn ID","Receipt ID","Balance","Note"

fields date, time, timezone, description_, type, status_, currency, grossamount, feeamount, netamount, fromemail, toemail, code, itemtitle, itemid, referencetxnid, receiptid, balance, note

skip  1

date-format  %-m/%-d/%Y

# ignore some paypal events
if
In Progress
Temporary Hold
Update to
 skip

# add more fields to the description
description %description_ %itemtitle

# save some other fields as tags
comment  itemid:%itemid, fromemail:%fromemail, toemail:%toemail, time:%time, type:%type, status:%status_

# convert to short currency symbols
if %currency USD
 currency $
if %currency EUR
 currency E
if %currency GBP
 currency P

# generate postings

# the first posting will be the money leaving/entering my paypal account
# (negative means leaving my account, in all amount fields)
account1 assets:online:paypal
amount1  %netamount

# the second posting will be money sent to/received from other party
# (account2 is set below)
amount2  -%grossamount

# if there's a fee, add a third posting for the money taken by paypal.
if %feeamount [1-9]
 account3 expenses:banking:paypal
 amount3  -%feeamount
 comment3 business:

# choose an account for the second posting

# override the default account names:
# if the amount is positive, it's income (a debit)
if %grossamount ^[^-]
 account2 income:unknown
# if negative, it's an expense (a credit)
if %grossamount ^-
 account2 expenses:unknown

# apply common rules for setting account2 & other tweaks
include common.rules

# apply some overrides specific to this csv

# Transfers from/to bank. These are usually marked Pending,
# which can be disregarded in this case.
if
Bank Account
Bank Deposit to PP Account
 description %type for %referencetxnid %itemtitle
 account2 assets:bank:wf:pchecking
 account1 assets:online:paypal

# Currency conversions
if Currency Conversion
 account2 equity:currency conversion
```

```rules
# common.rules

if
darcs
noble benefactor
 account2 revenues:foss donations:darcshub
 comment2 business:

if
Calm Radio
 account2 expenses:online:apps

if
electronic frontier foundation
Patreon
wikimedia
Advent of Code
 account2 expenses:dues

if Google
 account2 expenses:online:apps
 description google | music

```

```shell
$ hledger -f paypal-custom.csv  print
2019-10-01 (60P57143A8206782E) Calm Radio MONTHLY - $1 for the first 2 Months: Me - Order 99309. Item total: $1.00 USD first 2 months, then $6.99 / Month  ; itemid:, fromemail:simon@joyful.com, toemail:memberships@calmradio.com, time:03:46:20, type:Subscription Payment, status:Completed
    assets:online:paypal          $-6.99 = $-6.99
    expenses:online:apps           $6.99

2019-10-01 (0TU1544T080463733) Bank Deposit to PP Account for 60P57143A8206782E  ; itemid:, fromemail:, toemail:simon@joyful.com, time:03:46:20, type:Bank Deposit to PP Account, status:Pending
    assets:online:paypal               $6.99 = $0.00
    assets:bank:wf:pchecking          $-6.99

2019-10-01 (2722394R5F586712G) Patreon Patreon* Membership  ; itemid:, fromemail:simon@joyful.com, toemail:support@patreon.com, time:08:57:01, type:PreApproved Payment Bill User Payment, status:Completed
    assets:online:paypal          $-7.00 = $-7.00
    expenses:dues                  $7.00

2019-10-01 (71854087RG994194F) Bank Deposit to PP Account for 2722394R5F586712G Patreon* Membership  ; itemid:, fromemail:, toemail:simon@joyful.com, time:08:57:01, type:Bank Deposit to PP Account, status:Pending
    assets:online:paypal               $7.00 = $0.00
    assets:bank:wf:pchecking          $-7.00

2019-10-19 (K9U43044RY432050M) Wikimedia Foundation, Inc. Monthly donation to the Wikimedia Foundation  ; itemid:, fromemail:simon@joyful.com, toemail:tle@wikimedia.org, time:03:02:12, type:Subscription Payment, status:Completed
    assets:online:paypal             $-2.00 = $-2.00
    expenses:dues                     $2.00
    expenses:banking:paypal      ; business:

2019-10-19 (3XJ107139A851061F) Bank Deposit to PP Account for K9U43044RY432050M  ; itemid:, fromemail:, toemail:simon@joyful.com, time:03:02:12, type:Bank Deposit to PP Account, status:Pending
    assets:online:paypal               $2.00 = $0.00
    assets:bank:wf:pchecking          $-2.00

2019-10-22 (6L8L1662YP1334033) Noble Benefactor Joyful Systems  ; itemid:, fromemail:noble@bene.fac.tor, toemail:simon@joyful.com, time:05:07:06, type:Subscription Payment, status:Completed
    assets:online:paypal                       $9.41 = $9.41
    revenues:foss donations:darcshub         $-10.00  ; business:
    expenses:banking:paypal                    $0.59  ; business:

```


## CSV rules

The following kinds of rule can appear in the rules file, in any order.
Blank lines and lines beginning with `#` or `;` are ignored.


### `skip`

```rules
skip N
```
The word "skip" followed by a number (or no number, meaning 1)
tells hledger to ignore this many non-empty lines preceding the CSV data.
(Empty/blank lines are skipped automatically.)
You'll need this whenever your CSV data contains header lines.

It also has a second purpose: it can be used inside [if blocks](#if-block)
to ignore certain CSV records (described below).


### `fields`

```rules
fields FIELDNAME1, FIELDNAME2, ...
```
A fields list (the word "fields" followed by comma-separated field
names) is the quick way to assign CSV field values to hledger fields.
It does two things:

1. it names the CSV fields.
   This is optional, but can be convenient later for interpolating them.

2. when you use a standard hledger field name,
   it assigns the CSV value to that part of the hledger transaction.

Here's an example that says
"use the 1st, 2nd and 4th fields as the transaction's date, description and amount;
name the last two fields for later reference; and ignore the others":
```rules
fields date, description, , amount, , , somefield, anotherfield
```

Field names may not contain whitespace.
Fields you don't care about can be left unnamed.
Currently there must be least two items (there must be at least one comma).

Note, always use comma in the fields list, even if your CSV uses
[another separator character](#separator).

Here are the standard hledger field/pseudo-field names.
For more about the transaction parts they refer to, see the manual for hledger's journal format.

#### Transaction field names

`date`, `date2`, `status`, `code`, `description`, `comment` can be used to form the
[transaction's](#transactions) first line.

#### Posting field names

##### account

`accountN`, where N is 1 to 99, causes a [posting](#postings) to be generated, 
with that account name.

Most often there are two postings, so you'll want to set `account1` and `account2`.
Typically `account1` is associated with the CSV file, and is set once with a top-level assignment,
while `account2` is set based on each transaction's description, and in conditional blocks.

If a posting's account name is left unset but its amount is set (see below),
a default account name will be chosen (like "expenses:unknown" or "income:unknown").

##### amount

`amountN` sets posting N's amount. 
If the CSV uses separate fields for inflows and outflows, you can
use `amountN-in` and `amountN-out` instead.
By assigning to `amount1`, `amount2`, ... etc. you can generate anywhere
from 0 to 99 postings.

There is also an older, unnumbered form of these names, suitable for
2-posting transactions, which sets both posting 1's and (negated) posting 2's amount:
`amount`, or `amount-in` and `amount-out`.
This is still supported 
because it keeps pre-hledger-1.17 csv rules files working, 
and because it can be more succinct,
and because it converts posting 2's amount to cost if there's a
[transaction price](#transaction-prices), which can be useful.

If you have an existing rules file using the unnumbered form, you
might want to use the numbered form in certain conditional blocks,
without having to update and retest all the old rules. 
To facilitate this, 
posting 1 ignores `amount`/`amount-in`/`amount-out` if any of `amount1`/`amount1-in`/`amount1-out` are assigned,
and posting 2 ignores them if any of `amount2`/`amount2-in`/`amount2-out` are assigned,
avoiding conflicts.


##### currency

If the CSV has the currency symbol in a separate field (ie, not part
of the amount field), you can use `currencyN` to prepend it to posting
N's amount. Or, `currency` with no number affects all postings.

##### balance

`balanceN` sets a [balance assertion](#balance-assertions) amount
(or if the posting amount is left empty, a [balance assignment](#balance-assignments))
on posting N.

Also, for compatibility with hledger <1.17:
`balance` with no number is equivalent to `balance1`.

You can adjust the type of assertion/assignment with the
[`balance-type` rule](#balance-type) (see below).

##### comment

Finally, `commentN` sets a [comment](#comments) on the Nth posting.
Comments can also contain [tags](#tags), as usual.

See TIPS below for more about setting amounts and currency.


### field assignment

```rules
HLEDGERFIELDNAME FIELDVALUE
```

Instead of or in addition to a [fields list](#fields), you can use a
"field assignment" rule to set the value of a single hledger field, by
writing its name (any of the standard hledger field names above)
followed by a text value.
The value may contain interpolated CSV fields,
referenced by their 1-based position in the CSV record (`%N`),
or by the name they were given in the fields list (`%CSVFIELDNAME`).
Some examples:
```rules
# set the amount to the 4th CSV field, with " USD" appended
amount %4 USD

# combine three fields to make a comment, containing note: and date: tags
comment note: %somefield - %anotherfield, date: %1
```
Interpolation strips outer whitespace (so a CSV value like `" 1 "`
becomes `1` when interpolated)
([#1051](https://github.com/simonmichael/hledger/issues/1051)).
See TIPS below for more about referencing other fields.

### `separator`

You can use the `separator` rule to read other kinds of
character-separated data. The argument is any single separator
character, or the words `tab` or `space` (case insensitive). Eg, for
comma-separated values (CSV):

```
separator ,
```

or for semicolon-separated values (SSV):
```
separator ;
```

or for tab-separated values (TSV):
```
separator TAB
```

If the input file has a `.csv`, `.ssv` or `.tsv`
[file extension](#file-extension) (or a `csv:`, `ssv:`, `tsv:` prefix), 
the appropriate separator will be inferred automatically, and you
won't need this rule.

### `if` block

```rules
if MATCHER
 RULE

if
MATCHER
MATCHER
MATCHER
 RULE
 RULE
```

Conditional blocks ("if blocks") are a block of rules that are applied
only to CSV records which match certain patterns. They are often used
for customising account names based on transaction descriptions.

#### Matching the whole record

Each MATCHER can be a record matcher, which looks like this:
```rules
REGEX
```

REGEX is a case-insensitive [regular expression] that tries to match anywhere within the CSV record.
It is a POSIX ERE (extended regular expression) 
that also supports GNU word boundaries (`\b`, `\B`, `\<`, `\>`),
and nothing else.
If you have trouble, be sure to check our doc: https://hledger.org/hledger.html#regular-expressions

Important note: the record that is matched is not the original record, but a synthetic one,
with any enclosing double quotes (but not enclosing whitespace) removed, and always comma-separated
(which means that a field containing a comma will appear like two fields).
Eg, if the original record is `2020-01-01; "Acme, Inc.";  1,000`,
the REGEX will actually see   `2020-01-01,Acme, Inc.,  1,000`).

#### Matching individual fields

Or, MATCHER can be a field matcher, like this:
```rules
%CSVFIELD REGEX
```
which matches just the content of a particular CSV field.
CSVFIELD is a percent sign followed by the field's name or column number, like `%date` or `%1`.

#### Combining matchers

A single matcher can be written on the same line as the "if";
or multiple matchers can be written on the following lines, non-indented.
Multiple matchers are OR'd (any one of them can match), unless one begins with
an `&` symbol, in which case it is AND'ed with the previous matcher.

```rules
if
MATCHER
& MATCHER
 RULE
```

#### Rules applied on successful match

After the patterns there should be one or more rules to apply, all
indented by at least one space. Three kinds of rule are allowed in
conditional blocks:

- [field assignments](#field-assignment) (to set a hledger field)
- [skip](#skip) (to skip the matched CSV record)
- [end](#end) (to skip all remaining CSV records).

Examples:
```rules
# if the CSV record contains "groceries", set account2 to "expenses:groceries"
if groceries
 account2 expenses:groceries
```
```rules
# if the CSV record contains any of these patterns, set account2 and comment as shown
if
monthly service fee
atm transaction fee
banking thru software
 account2 expenses:business:banking
 comment  XXX deductible ? check it
```


### `if` table

```rules
if,CSVFIELDNAME1,CSVFIELDNAME2,...,CSVFIELDNAMEn
MATCHER1,VALUE11,VALUE12,...,VALUE1n
MATCHER2,VALUE21,VALUE22,...,VALUE2n
MATCHER3,VALUE31,VALUE32,...,VALUE3n
<empty line>
```

Conditional tables ("if tables") are a different syntax to specify
field assignments that will be applied only to CSV records which match certain patterns.

MATCHER could be either field or record matcher, as described above. When MATCHER matches,
values from that row would be assigned to the CSV fields named on the `if` line, in the same order.

Therefore `if` table is exactly equivalent to a sequence of of `if` blocks:
```rules
if MATCHER1
  CSVFIELDNAME1 VALUE11
  CSVFIELDNAME2 VALUE12
  ...
  CSVFIELDNAMEn VALUE1n

if MATCHER2
  CSVFIELDNAME1 VALUE21
  CSVFIELDNAME2 VALUE22
  ...
  CSVFIELDNAMEn VALUE2n

if MATCHER3
  CSVFIELDNAME1 VALUE31
  CSVFIELDNAME2 VALUE32
  ...
  CSVFIELDNAMEn VALUE3n
```

Each line starting with MATCHER should contain enough (possibly empty) values for all the listed fields.

Rules would be checked and applied in the order they are listed in the table and, like with `if` blocks, later rules (in the same or another table) or `if` blocks could override the effect of any rule.

Instead of ',' you can use a variety of other non-alphanumeric characters as a separator. First character after `if` is taken to be the separator for the rest of the table. It is the responsibility of the user to ensure that separator does not occur inside MATCHERs and values - there is no way to escape separator.


Example:
```rules
if,account2,comment
atm transaction fee,expenses:business:banking,deductible? check it
%description groceries,expenses:groceries,
2020/01/12.*Plumbing LLC,expenses:house:upkeep,emergency plumbing call-out
```

### `end`

This rule can be used inside [if blocks](#if-block) (only), to make hledger stop
reading this CSV file and move on to the next input file, or to command execution.
Eg:
```rules
# ignore everything following the first empty record
if ,,,,
 end
```


### `date-format`

```rules
date-format DATEFMT
```
This is a helper for the `date` (and `date2`) fields.
If your CSV dates are not formatted like `YYYY-MM-DD`, `YYYY/MM/DD` or `YYYY.MM.DD`,
you'll need to add a date-format rule describing them with a
strptime date parsing pattern, which must parse the CSV date value completely.
Some examples:
``` rules
# MM/DD/YY
date-format %m/%d/%y
```
``` rules
# D/M/YYYY
# The - makes leading zeros optional.
date-format %-d/%-m/%Y
```
``` rules
# YYYY-Mmm-DD
date-format %Y-%h-%d
```
``` rules
# M/D/YYYY HH:MM AM some other junk
# Note the time and junk must be fully parsed, though only the date is used.
date-format %-m/%-d/%Y %l:%M %p some other junk
```
For the supported strptime syntax, see:\
<https://hackage.haskell.org/package/time/docs/Data-Time-Format.html#v:formatTime>


### `decimal-mark`

```rules
decimal-mark .
```
or:
```rules
decimal-mark ,
```

hledger automatically accepts either period or comma as a decimal mark when parsing numbers
(cf [Amounts](#amounts)).
However if any numbers in the CSV contain digit group marks, such as thousand-separating commas,
you should declare the decimal mark explicitly with this rule, to avoid misparsed numbers.

### `newest-first`

hledger always sorts the generated transactions by date.
Transactions on the same date should appear in the same order as their CSV records,
as hledger can usually auto-detect whether the CSV's normal order is oldest first or newest first.
But if all of the following are true:

- the CSV might sometimes contain just one day of data (all records having the same date)
- the CSV records are normally in reverse chronological order (newest at the top)
- and you care about preserving the order of same-day transactions

then, you should add the `newest-first` rule as a hint. Eg:
```rules
# tell hledger explicitly that the CSV is normally newest first
newest-first
```


### `include`

```rules
include RULESFILE
```

This includes the contents of another CSV rules file at this point.
`RULESFILE` is an absolute file path or a path relative to the current file's directory.
This can be useful for sharing common rules between several rules files, eg:
```rules
# someaccount.csv.rules

## someaccount-specific rules
fields   date,description,amount
account1 assets:someaccount
account2 expenses:misc

## common rules
include categorisation.rules
```


### `balance-type`

Balance assertions generated by [assigning to balanceN](#posting-field-names)
are of the simple `=` type by default,
which is a [single-commodity](#assertions-and-commodities),
[subaccount-excluding](#assertions-and-subaccounts) assertion.
You may find the subaccount-including variants more useful,
eg if you have created some virtual subaccounts of checking to help with budgeting.
You can select a different type of assertion with the `balance-type` rule:
```rules
# balance assertions will consider all commodities and all subaccounts
balance-type ==*
```

Here are the balance assertion types for quick reference:
```
=    single commodity, exclude subaccounts
=*   single commodity, include subaccounts
==   multi commodity,  exclude subaccounts
==*  multi commodity,  include subaccounts
```

## Tips

### Rapid feedback

It's a good idea to get rapid feedback while creating/troubleshooting CSV rules.
Here's a good way, using entr from http://eradman.com/entrproject :
```shell
$ ls foo.csv* | entr bash -c 'echo ----; hledger -f foo.csv print desc:SOMEDESC'
```
A desc: query (eg) is used to select just one, or a few, transactions of interest.
"bash -c" is used to run multiple commands, so we can echo a separator each time
the command re-runs, making it easier to read the output.

### Valid CSV

hledger accepts CSV conforming to [RFC 4180](https://tools.ietf.org/html/rfc4180).
When CSV values are enclosed in quotes, note:

- they must be double quotes (not single quotes)
- spaces outside the quotes are [not allowed](https://stackoverflow.com/questions/4863852/space-before-quote-in-csv-field)

### File Extension

To help hledger identify the format and show the right error messages,
CSV/SSV/TSV files should normally be named with a `.csv`, `.ssv` or `.tsv`
filename extension. Or, the file path should be prefixed with `csv:`, `ssv:` or `tsv:`.
Eg:
```shell
$ hledger -f foo.ssv print
```
or:
```
$ cat foo | hledger -f ssv:- foo
```

You can override the file extension with a [separator](#separator) rule if needed.
See also: [Input files](#input-files) in the hledger manual.

### Reading multiple CSV files

If you use multiple `-f` options to read multiple CSV files at once,
hledger will look for a correspondingly-named rules file for each CSV
file. But if you use the `--rules-file` option, that rules file will
be used for all the CSV files.

### Valid transactions

After reading a CSV file, hledger post-processes and validates the
generated journal entries as it would for a journal file - balancing
them, applying balance assignments, and canonicalising amount styles.
Any errors at this stage will be reported in the usual way, displaying
the problem entry.

There is one exception: balance assertions, if you have generated
them, will not be checked, since normally these will work only when
the CSV data is part of the main journal. If you do need to check
balance assertions generated from CSV right away, pipe into another hledger:
```shell
$ hledger -f file.csv print | hledger -f- print
```

### Deduplicating, importing

When you download a CSV file periodically, eg to get your latest bank
transactions, the new file may overlap with the old one, containing
some of the same records.

The [import](#import) command will (a) detect the new
transactions, and (b) append just those transactions to your main
journal. It is idempotent, so you don't have to remember how many
times you ran it or with which version of the CSV.
(It keeps state in a hidden `.latest.FILE.csv` file.)
This is the easiest way to import CSV data. Eg:
```shell
# download the latest CSV files, then run this command.
# Note, no -f flags needed here.
$ hledger import *.csv [--dry]
```
This method works for most CSV files.
(Where records have a stable chronological order, and new records appear only at the new end.)

A number of other tools and workflows, hledger-specific and otherwise,
exist for converting, deduplicating, classifying and managing CSV
data. See:

- <https://hledger.org> -> sidebar -> real world setups
- <https://plaintextaccounting.org> -> data import/conversion

### Setting amounts

Some tips on using the [amount-setting rules](#amount) discussed above.

Here are the ways to set a posting's amount:

1. **If the CSV has a single amount field:**\
   Assign (via a [fields list](#fields) or a [field assignment](#field-assignment)) to `amountN`.
   This sets the Nth posting's amount. N is usually 1 or 2 but can go up to 99.

2. **If the CSV has separate Debit and Credit amount fields:**\
   Assign to `amountN-in` and `amountN-out`.
   This sets posting N's amount to whichever of these has a non-zero value, 
   guessing an appropriate sign.

   - **If hledger guesses the wrong sign:**\
     Prepend a minus sign to flip it. Eg:

      ```rules
      fields date, description, amount-in, amount-out
      amount-out -%amount-out
      ```

   - **If both fields contain a non-zero value:**\
     The `amountN-in`/`amountN-out` rules require that each CSV record has a non-zero value in exactly one of the two fields,
     so that hledger knows which to choose. So these would all be rejected:
  
      ```csv
      "",  ""
      "0", "0"
      "1", "none"
      ```

     If your CSV has amount values like this, use [conditional rules](#if-block) instead.
     For example, to make hledger to choose the value containing non-zero digits:

      ```rules
      fields date, description, in, out
      if %in [1-9]
       amount1 %in
      if %out [1-9]
       amount1 %out
      ```

3. **Using the old numberless syntax:**\
   Assign to `amount` (or to `amount-in` and `amount-out`). 
   This sets posting 1's and posting 2's amounts (and converts posting 2's amount to cost).
   This is supported for backwards compatibility (and occasional convenience).

4. **If the CSV has the balance instead of the transaction amount:**\
   Assign to `balanceN`, which sets posting N's amount indirectly via a
   [balance assignment](#balance-assignments).
   (Old syntax: `balance`, equivalent to `balance1`.)
   
   - **If hledger guesses the wrong default account name:**\
     When setting the amount via balance assertion, hledger may guess the wrong default account name.
     So, set the account name explicitly, eg:

      ```rules
      fields date, description, balance1
      account1 assets:checking
      ```

### Amount signs

There is some special handling for amount signs, to simplify parsing and sign-flipping:

- **If an amount value begins with a plus sign:**\
  that will be removed: `+AMT` becomes `AMT`

- **If an amount value is parenthesised:**\
  it will be de-parenthesised and sign-flipped: `(AMT)` becomes `-AMT`

- **If an amount value has two minus signs (or two sets of parentheses, or a minus sign and parentheses):**\
  they cancel out and will be removed: `--AMT` or `-(AMT)` becomes `AMT`

- **If an amount value contains just a sign (or just a set of parentheses):**\
  that is removed, making it an empty value. `"+"` or `"-"` or `"()"` becomes `""`.

### Setting currency/commodity

If the currency/commodity symbol is included in the  CSV's amount field(s):

```csv
2020-01-01,foo,$123.00
```

you don't have to do anything special for the commodity symbol, it will be assigned as part of the amount. Eg:

```rules
fields date,description,amount
```
```journal
2020-01-01 foo
    expenses:unknown         $123.00
    income:unknown          $-123.00
```

If the currency is provided as a separate CSV field:

```csv
2020-01-01,foo,USD,123.00
```

You can assign that to the `currency` pseudo-field, which has the
special effect of prepending itself to every amount in the
transaction (on the left, with no separating space):
  
```rules
fields date,description,currency,amount
```
```journal
2020-01-01 foo
    expenses:unknown       USD123.00
    income:unknown        USD-123.00
```
<!-- a special case, I don't remember exactly where:
If you write a trailing space after the symbol, there will be a space
between symbol and amount (an exception to the usual whitespace stripping).
-->

Or, you can use a field assignment to construct the amount yourself, with more control.
Eg to put the symbol on the right, and separated by a space:

```rules
fields date,description,cur,amt
amount %amt %cur
```
```journal
2020-01-01 foo
    expenses:unknown        123.00 USD
    income:unknown         -123.00 USD
```
Note we used a temporary field name (`cur`) that is not `currency` -
that would trigger the prepending effect, which we don't want here.

### Amount decimal places

Like amounts in a journal file,
the amounts generated by CSV rules like `amount1` influence 
[commodity display styles](#commodity-display-styles), such as
the number of decimal places displayed in reports.

The original amounts as written in the CSV file do not affect display
style (because we don't yet reliably know their commodity).

### Referencing other fields

In field assignments, you can interpolate only CSV fields, not hledger
fields. In the example below, there's both a CSV field and a hledger
field named amount1, but %amount1 always means the CSV field, not
the hledger field:

```rules
# Name the third CSV field "amount1"
fields date,description,amount1

# Set hledger's amount1 to the CSV amount1 field followed by USD
amount1 %amount1 USD

# Set comment to the CSV amount1 (not the amount1 assigned above)
comment %amount1
```

Here, since there's no CSV amount1 field, %amount1 will produce a literal "amount1":
```rules
fields date,description,csvamount
amount1 %csvamount USD
# Can't interpolate amount1 here
comment %amount1
```

When there are multiple field assignments to the same hledger field,
only the last one takes effect. Here, comment's value will be be B,
or C if "something" is matched, but never A:
```rules
comment A
comment B
if something
 comment C
```

### How CSV rules are evaluated

Here's how to think of CSV rules being evaluated (if you really need to).
First,

- `include` - all includes are inlined, from top to bottom, depth
  first. (At each include point the file is inlined and scanned for
  further includes, recursively, before proceeding.)

Then "global" rules are evaluated, top to bottom. If a rule is
repeated, the last one wins:

- `skip` (at top level)
- `date-format`
- `newest-first`
- `fields` - names the CSV fields, optionally sets up initial assignments to hledger fields

Then for each CSV record in turn:

- test all `if` blocks. If any of them contain a `end` rule, skip all remaining CSV records.
  Otherwise if any of them contain a `skip` rule, skip that many CSV records.
  If there are multiple matched `skip` rules, the first one wins.
- collect all field assignments at top level and in matched `if` blocks.
  When there are multiple assignments for a field, keep only the last one.
- compute a value for each hledger field - either the one that was assigned to it
  (and interpolate the %CSVFIELDNAME references), or a default
- generate a synthetic hledger transaction from these values.

This is all part of the CSV reader, one of several readers hledger can
use to parse input files. When all files have been read successfully,
the transactions are passed as input to whichever hledger command the
user specified.

# TIMECLOCK FORMAT

The time logging format of timeclock.el, as read by hledger.

hledger can read time logs in timeclock format.
[As with Ledger](http://ledger-cli.org/3.0/doc/ledger3.html#Time-Keeping),
these are (a subset of)
[timeclock.el](http://www.emacswiki.org/emacs/TimeClock)'s format,
containing clock-in and clock-out entries as in the example below.
The date is a [simple date](#simple-dates).
The time format is HH:MM[:SS][+-ZZZZ]. Seconds and timezone are optional.
The timezone, if present, must be four digits and is ignored
(currently the time is always interpreted as a local time).

```timeclock
i 2015/03/30 09:00:00 some:account name  optional description after two spaces
o 2015/03/30 09:20:00
i 2015/03/31 22:21:45 another account
o 2015/04/01 02:00:34
```

hledger treats each clock-in/clock-out pair as a transaction posting
some number of hours to an account. Or if the session spans more than
one day, it is split into several transactions, one for each day. For
the above time log, `hledger print` generates these journal entries:

``` shell
$ hledger -f t.timeclock print
2015-03-30 * optional description after two spaces
    (some:account name)         0.33h

2015-03-31 * 22:21-23:59
    (another account)         1.64h

2015-04-01 * 00:00-02:00
    (another account)         2.01h

```

Here is a
[sample.timeclock](https://raw.github.com/simonmichael/hledger/master/examples/sample.timeclock) to
download and some queries to try:

```shell
$ hledger -f sample.timeclock balance                               # current time balances
$ hledger -f sample.timeclock register -p 2009/3                    # sessions in march 2009
$ hledger -f sample.timeclock register -p weekly --depth 1 --empty  # time summary by week
```

To generate time logs, ie to clock in and clock out, you could:

- use emacs and the built-in timeclock.el, or
  the extended [timeclock-x.el](http://www.emacswiki.org/emacs/timeclock-x.el)
  and perhaps the extras in [ledgerutils.el](http://hub.darcs.net/simon/ledgertools/ledgerutils.el)

- at the command line, use these bash aliases:
    ```shell
    alias ti="echo i `date '+%Y-%m-%d %H:%M:%S'` \$* >>$TIMELOG"
    alias to="echo o `date '+%Y-%m-%d %H:%M:%S'` >>$TIMELOG"
    ```
- or use the old `ti` and `to` scripts in the [ledger 2.x repository](https://github.com/ledger/ledger/tree/maint/scripts).
  These rely on a "timeclock" executable which I think is just the ledger 2 executable renamed.

# TIMEDOT FORMAT

hledger's human-friendly time logging format.

Timedot is a plain text format for logging dated, categorised quantities (of time, usually), supported by hledger.
It is convenient for approximate and retroactive time logging,
eg when the real-time clock-in/out required with a timeclock file is too precise or too interruptive.
It can be formatted like a bar chart, making clear at a glance where time was spent.

Though called "timedot", this format is read by hledger as commodityless quantities,
so it could be used to represent dated quantities other than time.
In the docs below we'll assume it's time.

A timedot file contains a series of day entries.
A day entry begins with a non-indented hledger-style
[simple date](#simple-dates) (Y-M-D, Y/M/D, Y.M.D..)
Any additional text on the same line is used as a transaction description for this day.

This is followed by optionally-indented timelog items for that day, one per line.
Each timelog item is a note, usually a hledger:style:account:name representing a time category,
followed by two or more spaces, and a quantity.
Each timelog item generates a hledger transaction.

Quantities can be written as:

- dots: a sequence of dots (.) representing quarter hours.
  Spaces may optionally be used for grouping.
  Eg: .... ..

- an integral or decimal number, representing hours.
  Eg: 1.5

- an integral or decimal number immediately followed by a unit symbol
  `s`, `m`, `h`, `d`, `w`, `mo`, or `y`, representing seconds, minutes, hours, days
  weeks, months or years respectively.
  Eg: 90m.
  The following equivalencies are assumed, currently:
  1m = 60s, 1h = 60m, 1d = 24h, 1w = 7d, 1mo = 30d, 1y=365d.

There is some flexibility allowing notes and todo lists to be kept
right in the time log, if needed:

- Blank lines and lines beginning with `#` or `;` are ignored.

- Lines not ending with a double-space and quantity are parsed as
  items taking no time, which will not appear in balance reports by
  default. (Add -E to see them.)

- Org mode headlines (lines beginning with one or more `*` followed by
  a space) can be used as date lines or timelog items (the stars are
  ignored). Also all org headlines before the first date line are
  ignored. This means org users can manage their timelog as an org
  outline (eg using org-mode/orgstruct-mode in Emacs), for
  organisation, faster navigation, controlling visibility etc.


Examples:

```timedot
# on this day, 6h was spent on client work, 1.5h on haskell FOSS work, etc.
2016/2/1
inc:client1   .... .... .... .... .... ....
fos:haskell   .... ..
biz:research  .

2016/2/2
inc:client1   .... ....
biz:research  .
```

```timedot
2016/2/3
inc:client1   4
fos:hledger   3
biz:research  1
```

```timedot
* Time log
** 2020-01-01
*** adm:time  .
*** adm:finance  .
```

```timedot
* 2020 Work Diary
** Q1
*** 2020-02-29
**** DONE
0700 yoga
**** UNPLANNED
**** BEGUN
hom:chores
 cleaning  ...
 water plants
  outdoor - one full watering can
  indoor - light watering
**** TODO
adm:planning: trip
*** LATER

```

Reporting:

```shell
$ hledger -f t.timedot print date:2016/2/2
2016-02-02 *
    (inc:client1)          2.00

2016-02-02 *
    (biz:research)          0.25
```
```shell
$ hledger -f t.timedot bal --daily --tree
Balance changes in 2016-02-01-2016-02-03:

            ||  2016-02-01d  2016-02-02d  2016-02-03d 
============++========================================
 biz        ||         0.25         0.25         1.00 
   research ||         0.25         0.25         1.00 
 fos        ||         1.50            0         3.00 
   haskell  ||         1.50            0            0 
   hledger  ||            0            0         3.00 
 inc        ||         6.00         2.00         4.00 
   client1  ||         6.00         2.00         4.00 
------------++----------------------------------------
            ||         7.75         2.25         8.00 
```

I prefer to use period for separating account components.
We can make this work with an [account alias](#rewriting-accounts):

```timedot
2016/2/4
fos.hledger.timedot  4
fos.ledger           ..
```
```shell
$ hledger -f t.timedot --alias /\\./=: bal date:2016/2/4 --tree
                4.50  fos
                4.00    hledger:timedot
                0.50    ledger
--------------------
                4.50
```

Here is a
[sample.timedot](https://raw.github.com/simonmichael/hledger/master/examples/sample.timedot).
<!-- to download and some queries to try: -->

<!-- ```shell -->
<!-- $ hledger -f sample.timedot balance                               # current time balances -->
<!-- $ hledger -f sample.timedot register -p 2009/3                    # sessions in march 2009 -->
<!-- $ hledger -f sample.timedot register -p weekly --depth 1 --empty  # time summary by week -->
<!-- ``` -->

# COMMON TASKS

Here are some quick examples of how to do some basic tasks with hledger.
For more details, see the reference section below, the hledger_journal(5) manual,
or the more extensive docs at <https://hledger.org>.

## Getting help

```shell
$ hledger                 # show available commands
$ hledger --help          # show common options
$ hledger CMD --help      # show common and command options, and command help
$ hledger help            # show available manuals/topics
$ hledger help hledger    # show hledger manual as info/man/text (auto-chosen)
$ hledger help journal --man  # show the journal manual as a man page
$ hledger help --help     # show more detailed help for the help command
```

Find more docs, chat, mail list, reddit, issue tracker:
<https://hledger.org#help-feedback>

## Constructing command lines

hledger has an extensive and powerful command line interface. We
strive to keep it simple and ergonomic, but you may run into one of
the confusing real world details described in OPTIONS, below.
If that happens, here are some tips that may help:

- command-specific options must go after the command (it's fine to put all options there) (`hledger CMD OPTS ARGS`)
- running add-on executables directly simplifies command line parsing (`hledger-ui OPTS ARGS`)
- enclose "problematic" args in single quotes
- if needed, also add a backslash to hide regular expression metacharacters from the shell
- to see how a misbehaving command is being parsed, add `--debug=2`.

## Starting a journal file

hledger looks for your accounting data in a journal file, `$HOME/.hledger.journal` by default:
```shell
$ hledger stats
The hledger journal file "/Users/simon/.hledger.journal" was not found.
Please create it first, eg with "hledger add" or a text editor.
Or, specify an existing journal file with -f or LEDGER_FILE.
```

You can override this by setting the `LEDGER_FILE` environment variable.
It's a good practice to keep this important file under version control,
and to start a new file each year. So you could do something like this:
```shell
$ mkdir ~/finance
$ cd ~/finance
$ git init
Initialized empty Git repository in /Users/simon/finance/.git/
$ touch 2020.journal
$ echo "export LEDGER_FILE=$HOME/finance/2020.journal" >> ~/.bashrc
$ source ~/.bashrc
$ hledger stats
Main file                : /Users/simon/finance/2020.journal
Included files           : 
Transactions span        :  to  (0 days)
Last transaction         : none
Transactions             : 0 (0.0 per day)
Transactions last 30 days: 0 (0.0 per day)
Transactions last 7 days : 0 (0.0 per day)
Payees/descriptions      : 0
Accounts                 : 0 (depth 0)
Commodities              : 0 ()
Market prices            : 0 ()
```

## Setting opening balances

Pick a starting date for which you can look up the balances of some
real-world assets (bank accounts, wallet..) and liabilities (credit cards..).

To avoid a lot of data entry, you may want to start with just one or
two accounts, like your checking account or cash wallet; and pick a
recent starting date, like today or the start of the week. You can
always come back later and add more accounts and older transactions,
eg going back to january 1st.

Add an opening balances transaction to the journal, declaring the
balances on this date. Here are two ways to do it:

- The first way: open the journal in any text editor and save an entry like this:
  ```journal
  2020-01-01 * opening balances
      assets:bank:checking                $1000   = $1000
      assets:bank:savings                 $2000   = $2000
      assets:cash                          $100   = $100
      liabilities:creditcard               $-50   = $-50
      equity:opening/closing balances
  ```
  These are start-of-day balances, ie whatever was in the account at the
  end of the previous day.

  The * after the date is an optional status flag.
  Here it means "cleared & confirmed".

  The currency symbols are optional, but usually a good idea as you'll
  be dealing with multiple currencies sooner or later.

  The = amounts are optional balance assertions, providing extra error checking.

- The second way: run `hledger add` and follow the prompts to record a similar transaction:
  ```shell
  $ hledger add
  Adding transactions to journal file /Users/simon/finance/2020.journal
  Any command line arguments will be used as defaults.
  Use tab key to complete, readline keys to edit, enter to accept defaults.
  An optional (CODE) may follow transaction dates.
  An optional ; COMMENT may follow descriptions or amounts.
  If you make a mistake, enter < at any prompt to go one step backward.
  To end a transaction, enter . when prompted.
  To quit, enter . at a date prompt or press control-d or control-c.
  Date [2020-02-07]: 2020-01-01
  Description: * opening balances
  Account 1: assets:bank:checking
  Amount  1: $1000
  Account 2: assets:bank:savings
  Amount  2 [$-1000]: $2000
  Account 3: assets:cash
  Amount  3 [$-3000]: $100
  Account 4: liabilities:creditcard
  Amount  4 [$-3100]: $-50
  Account 5: equity:opening/closing balances
  Amount  5 [$-3050]: 
  Account 6 (or . or enter to finish this transaction): .
  2020-01-01 * opening balances
      assets:bank:checking                      $1000
      assets:bank:savings                       $2000
      assets:cash                                $100
      liabilities:creditcard                     $-50
      equity:opening/closing balances          $-3050
  
  Save this transaction to the journal ? [y]: 
  Saved.
  Starting the next transaction (. or ctrl-D/ctrl-C to quit)
  Date [2020-01-01]: .
  ```

If you're using version control, this could be a good time to commit the journal. Eg:
```shell
$ git commit -m 'initial balances' 2020.journal
```

## Recording transactions

As you spend or receive money, you can record these transactions
using one of the methods above (text editor, hledger add)
or by using the [hledger-iadd](#iadd) or [hledger-web](#web) add-ons,
or by using the [import command](#import) to convert CSV data downloaded from your bank.

Here are some simple transactions, see the hledger_journal(5) manual
and hledger.org for more ideas:

```journal
2020/1/10 * gift received
  assets:cash   $20
  income:gifts

2020.1.12 * farmers market
  expenses:food    $13
  assets:cash

2020-01-15 paycheck
  income:salary
  assets:bank:checking    $1000
```

## Reconciling

Periodically you should reconcile - compare your hledger-reported balances
against external sources of truth, like bank statements or your bank's website -
to be sure that your ledger accurately represents the real-world balances
(and, that the real-world institutions have not made a mistake!).
This gets easy and fast with (1) practice and (2) frequency.
If you do it daily, it can take 2-10 minutes.
If you let it pile up, expect it to take longer as you hunt down errors and discrepancies.

A typical workflow:

1. Reconcile cash.
   Count what's in your wallet.
   Compare with what hledger reports (`hledger bal cash`).
   If they are different, try to remember the missing transaction,
   or look for the error in the already-recorded transactions.
   A register report can be helpful (`hledger reg cash`).
   If you can't find the error, add an adjustment transaction.
   Eg if you have $105 after the above, and can't explain the missing $2, it could be:
   ```journal
   2020-01-16 * adjust cash
       assets:cash    $-2 = $105
       expenses:misc
   ```

2. Reconcile checking.
   Log in to your bank's website.
   Compare today's (cleared) balance with hledger's cleared balance (`hledger bal checking -C`).
   If they are different, track down the error or record the missing transaction(s)
   or add an adjustment transaction, similar to the above.
   Unlike the cash case, you can usually compare the transaction history and running balance from your bank
   with the one reported by `hledger reg checking -C`.
   This will be easier if you generally record transaction dates
   quite similar to your bank's clearing dates.

3. Repeat for other asset/liability accounts.

Tip: instead of the register command, use hledger-ui to see a
live-updating register while you edit the journal:
`hledger-ui --watch --register checking -C`

After reconciling, it could be a good time to mark the reconciled
transactions' status as "cleared and confirmed", if you want to track
that, by adding the `*` marker.
Eg in the paycheck transaction above, insert `*` between `2020-01-15` and `paycheck`

If you're using version control, this can be another good time to commit:
```shell
$ git commit -m 'txns' 2020.journal
```

## Reporting

Here are some basic reports.

Show all transactions:
```shell
$ hledger print
2020-01-01 * opening balances
    assets:bank:checking                      $1000
    assets:bank:savings                       $2000
    assets:cash                                $100
    liabilities:creditcard                     $-50
    equity:opening/closing balances          $-3050

2020-01-10 * gift received
    assets:cash              $20
    income:gifts

2020-01-12 * farmers market
    expenses:food             $13
    assets:cash

2020-01-15 * paycheck
    income:salary
    assets:bank:checking           $1000

2020-01-16 * adjust cash
    assets:cash               $-2 = $105
    expenses:misc

```

Show account names, and their hierarchy:
```shell
$ hledger accounts --tree
assets
  bank
    checking
    savings
  cash
equity
  opening/closing balances
expenses
  food
  misc
income
  gifts
  salary
liabilities
  creditcard
```

Show all account totals:
```shell
$ hledger balance
               $4105  assets
               $4000    bank
               $2000      checking
               $2000      savings
                $105    cash
              $-3050  equity:opening/closing balances
                 $15  expenses
                 $13    food
                  $2    misc
              $-1020  income
                $-20    gifts
              $-1000    salary
                $-50  liabilities:creditcard
--------------------
                   0
```

Show only asset and liability balances, as a flat list, limited to depth 2:
```shell
$ hledger bal assets liabilities --flat -2
               $4000  assets:bank
                $105  assets:cash
                $-50  liabilities:creditcard
--------------------
               $4055
```

Show the same thing without negative numbers, formatted as a simple balance sheet:
```shell
$ hledger bs --flat -2
Balance Sheet 2020-01-16

                        || 2020-01-16 
========================++============
 Assets                 ||            
------------------------++------------
 assets:bank            ||      $4000 
 assets:cash            ||       $105 
------------------------++------------
                        ||      $4105 
========================++============
 Liabilities            ||            
------------------------++------------
 liabilities:creditcard ||        $50 
------------------------++------------
                        ||        $50 
========================++============
 Net:                   ||      $4055 
```
The final total is your "net worth" on the end date.
(Or use `bse` for a full balance sheet with equity.)

Show income and expense totals, formatted as an income statement:
```shell
hledger is 
Income Statement 2020-01-01-2020-01-16

               || 2020-01-01-2020-01-16 
===============++=======================
 Revenues      ||                       
---------------++-----------------------
 income:gifts  ||                   $20 
 income:salary ||                 $1000 
---------------++-----------------------
               ||                 $1020 
===============++=======================
 Expenses      ||                       
---------------++-----------------------
 expenses:food ||                   $13 
 expenses:misc ||                    $2 
---------------++-----------------------
               ||                   $15 
===============++=======================
 Net:          ||                 $1005 
```
The final total is your net income during this period.

Show transactions affecting your wallet, with running total:
```shell
$ hledger register cash
2020-01-01 opening balances     assets:cash                   $100          $100
2020-01-10 gift received        assets:cash                    $20          $120
2020-01-12 farmers market       assets:cash                   $-13          $107
2020-01-16 adjust cash          assets:cash                    $-2          $105
```

Show weekly posting counts as a bar chart:
```shell
$ hledger activity -W
2019-12-30 *****
2020-01-06 ****
2020-01-13 ****
```
## Migrating to a new file

At the end of the year, you may want to continue your journal in a new file,
so that old transactions don't slow down or clutter your reports,
and to help ensure the integrity of your accounting history.
See the [close command](#close).

If using version control, don't forget to `git add` the new file.

# LIMITATIONS

The need to precede add-on command options with `--` when invoked from hledger is awkward.

When input data contains non-ascii characters, a suitable system locale must be configured (or there will be an unhelpful error).
Eg on POSIX, set LANG to something other than C.

In a Microsoft Windows CMD window, non-ascii characters and colours are not supported.

On Windows, non-ascii characters may not display correctly when running a hledger built
in CMD in MSYS/CYGWIN, or vice-versa.

In a Cygwin/MSYS/Mintty window, the tab key is not supported in hledger add.

Not all of Ledger's journal file syntax is supported. See [file format differences](faq.html#file-format-differences).

On large data files, hledger is slower and uses more memory than Ledger.

# TROUBLESHOOTING

Here are some issues you might encounter when you run hledger
(and remember you can also seek help from the
[IRC channel](http://irc.hledger.org),
[mail list](http://list.hledger.org) or
[bug tracker](http://bugs.hledger.org)):

**Successfully installed, but "No command 'hledger' found"**\
stack and cabal install binaries into a special directory, which
should be added to your PATH environment variable.  Eg on unix-like
systems, that is ~/.local/bin and ~/.cabal/bin respectively.

**I set a custom LEDGER_FILE, but hledger is still using the default file**\
`LEDGER_FILE` should be a real environment variable, not just a shell variable.
The command `env | grep LEDGER_FILE` should show it.
You may need to use `export`. Here's an [explanation](http://stackoverflow.com/a/7411509).

**Getting errors like "Illegal byte sequence" or "Invalid or incomplete multibyte or wide character" or "commitAndReleaseBuffer: invalid argument (invalid character)"**\
Programs compiled with GHC (hledger, haskell build tools, etc.) 
need to have a UTF-8-aware locale configured in the environment, 
otherwise they will fail with these kinds of errors when they encounter non-ascii characters.

To fix it, set the LANG environment variable to some locale which supports UTF-8.
The locale you choose must be installed on your system.

Here's an example of setting LANG temporarily, on Ubuntu GNU/Linux:

```shell
$ file my.journal
my.journal: UTF-8 Unicode text         # the file is UTF8-encoded
$ echo $LANG
C                                      # LANG is set to the default locale, which does not support UTF8
$ locale -a                            # which locales are installed ?
C
en_US.utf8                             # here's a UTF8-aware one we can use
POSIX
$ LANG=en_US.utf8 hledger -f my.journal print   # ensure it is used for this command
```

If available, `C.UTF-8` will also work.
If your preferred locale isn't listed by `locale -a`, you might need to install it. Eg on Ubuntu/Debian:

```shell
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

Here's how you could set it permanently, if you use a bash shell:

```shell
$ echo "export LANG=en_US.utf8" >>~/.bash_profile
$ bash --login
```

Exact spelling and capitalisation may be important. Note the difference on MacOS (`UTF-8`, not `utf8`).
Some platforms (eg ubuntu) allow variant spellings, but others (eg macos) require it to be exact:

```shell
$ locale -a | grep -iE en_us.*utf
en_US.UTF-8
$ LANG=en_US.UTF-8 hledger -f my.journal print
```
