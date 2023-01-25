% hledger(1)
% _author_
% _monthyear_

m4_dnl  Lines beginning with m4_dnl are comments. See help at end of file.
m4_dnl  The macro below includes the content only in the web format:
_web_({{
*Quick links:
[Commands][commands-list],
[Queries],
[Regular expressions],
[Period expressions],
[Journal],
[Directive effects],
[CSV],
[Timeclock],
[Timedot],
[Valuation],
[Common tasks]*
}})

_notinfo_({{
# NAME
}})

hledger - robust, friendly plain text accounting (CLI version)

_notinfo_({{
# SYNOPSIS
}})

`hledger`\
`hledger [-f FILE] COMMAND [OPTS] [ARGS]`\
`hledger [-f FILE] ADDONCMD -- [OPTS] [ARGS]`

_notinfo_({{
# INTRODUCTION
}})

_hledgerdescription_

This manual is for hledger's command line interface, version _version_.
It also describes the common options, file formats and concepts used by all hledger programs. 
It might accidentally teach you some bookkeeping/accounting as well!
You don't need to know everything in here to use hledger productively,
but when you have a question about functionality, this doc should answer it.
It is detailed, so do skip ahead or skim when needed.
You can read it on hledger.org, or as an info manual or man page on your system.
You can also get it from hledger itself with\
`hledger --man`, `hledger --info` or `hledger help [TOPIC]`.

The main function of the hledger CLI is
to read plain text files describing financial transactions,
crunch the numbers,
and print a useful report on the terminal (or save it as HTML, CSV, JSON or SQL).
Many reports are available, as subcommands.
hledger will also detect other `hledger-*` executables as extra subcommands.

hledger reads _inputfiles_
hledger CLI can also read from stdin with `-f-`; more on that below.

Here is a small but valid hledger journal file describing one transaction:

_journal_({{
2015-10-16 bought food
  expenses:food          $10
  assets:cash
}})

Transactions are dated movements of money (etc.) between two or more *accounts*:
bank accounts, your wallet, revenue/expense categories, people, etc.
You can choose any account names you wish, using `:` to indicate subaccounts.
There must be at least two spaces between account name and amount.
Positive amounts are inflow to that account (*debit*), negatives are outflow from it (*credit*).
(Some reports show revenue, liability and equity account balances as negative numbers
as a result; this is normal.)

hledger’s add command can help you add transactions,
or you can install other data entry UIs like hledger-web or hledger-iadd.
For more extensive/efficient changes, use a text editor:
Emacs + ledger-mode, VIM + vim-ledger, or VS Code + hledger-vscode
are some good choices (see <https://hledger.org/editors.html>).

To get started, run `hledger add` and follow the prompts,
or save some entries like the above in `$HOME/.hledger.journal`,
then try commands like:\
`hledger print -x`\
`hledger aregister assets`\
`hledger balance`\
`hledger balancesheet`\
`hledger incomestatement`.\
Run `hledger` to list the commands.
See also the "Starting a journal file" and "Setting opening balances" sections
in [PART 5: COMMON TASKS](#part-5-common-tasks).

# PART 1: USER INTERFACE

# Options

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

See also: [Save frequently used options](/save-frequently-used-options.html).

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

Windows users should keep in mind that `cmd` treats single quote as a
regular character, so you should be using double quotes exclusively.
PowerShell treats both single and double quotes as quotes.

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
- [account alias](#account-aliases) directives and options: `alias /REGEX/ = REPLACEMENT`, `--alias /REGEX/=REPLACEMENT`

hledger's regular expressions come from the
[regex-tdfa](http://hackage.haskell.org/package/regex-tdfa/docs/Text-Regex-TDFA.html)
library. 
If they're not doing what you expect, it's important to know exactly what they support:

1. they are case insensitive
2. they are infix matching (they do not need to match the entire thing being matched)
3. they are [POSIX ERE] (extended regular expressions)
4. they also support [GNU word boundaries] (`\b`, `\B`, `\<`, `\>`)
5. they do not support [backreferences]; if you write `\1`, it will match the digit `1`.
   Except when doing text replacement, eg in [account aliases](#regex-aliases),
   where [backreferences] can be used in the replacement string to reference [capturing groups] in the search regexp.
6. they do not support [mode modifiers] (`(?s)`), character classes (`\w`, `\d`), or anything else not mentioned above.

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

# Environment

_LEDGER_FILE_

**COLUMNS**
The screen width used by the register command.
Default: the full terminal width.

**NO_COLOR**
If this variable exists with any value, 
hledger will not use ANSI color codes in terminal output.
This is overriden by the --color/--colour option.

# Input

hledger reads transactions from one or more data files.
The default data file is `$HOME/.hledger.journal`
(or on Windows, something like `C:\Users\YOURNAME\.hledger.journal`).

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

These formats are described in more detail below.

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

- most [directives do not affect sibling files](#directives-and-multiple-files)
- [balance assertions](#balance-assertions) will not see any account balances from previous files

If you need either of those things, you can 

- use a single parent file which [includes](#including-files) the others
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
- Are all commodity conversions declared explicitly ?

You can use the [check](#check) command to run individual checks -- the
ones listed above and some more.

# Commands

hledger provides a number of built-in subcommands (described [below](#part-4-commands)).
Most of these read your data without changing it, and display a report.
A few assist with data entry and management.

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

## Add-on commands
<!-- #add-on-commands: the long explanation of add-on commands. See also #addons. -->

Add-on commands are extra subcommands provided by programs or scripts in your PATH 

- whose name starts with `hledger-`
- whose name ends with a recognised file extension:
  `.bat`,`.com`,`.exe`, `.hs`,`.lhs`,`.pl`,`.py`,`.rb`,`.rkt`,`.sh` or none
- and (on unix, mac) which are executable by the current user.

Addons can be written in any language, but haskell scripts or programs have a big advantage:
they can use hledger's library code, for command-line options, parsing and reporting.

Several add-on commands are installed by the 
[hledger-install script](https://hledger.org/install.html#build-methods).
See <https://hledger.org/scripts.html> for more details.


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

# Output

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

## Output format

Some commands offer other kinds of output, not just text on the terminal.
Here are those commands and the formats currently supported:

| -                  | txt   | csv   | html    | json | sql |
|--------------------|-------|-------|---------|------|-----|
| aregister          | Y     | Y     |         | Y    |     |
| balance            | Y *<sup>1</sup>* | Y *<sup>1</sup>* | Y *<sup>1,2</sup>* | Y    |     |
| balancesheet       | Y *<sup>1</sup>* | Y *<sup>1</sup>* | Y *<sup>1</sup>*   | Y    |     |
| balancesheetequity | Y *<sup>1</sup>* | Y *<sup>1</sup>* | Y *<sup>1</sup>*   | Y    |     |
| cashflow           | Y *<sup>1</sup>* | Y *<sup>1</sup>* | Y *<sup>1</sup>*   | Y    |     |
| incomestatement    | Y *<sup>1</sup>* | Y *<sup>1</sup>* | Y *<sup>1</sup>*   | Y    |     |
| print              | Y     | Y     |         | Y    | Y   |
| register           | Y     | Y     |         | Y    |     |

- *<sup>1</sup> Also affected by the balance commands' [`--layout` option](#commodity-layout).*
- *<sup>2</sup> `balance` does not support html output without a report interval or with `--budget`.*

<!--
| accounts              |     |     |      |      |     |
| activity              |     |     |      |      |     |
| add                   |     |     |      |      |     |
| check                 |     |     |      |      |     |
| check-fancyassertions |     |     |      |      |     |
| check-tagfiles        |     |     |      |      |     |
| close                 |     |     |      |      |     |
| codes                 |     |     |      |      |     |
| commodities           |     |     |      |      |     |
| descriptions          |     |     |      |      |     |
| diff                  |     |     |      |      |     |
| files                 |     |     |      |      |     |
| iadd                  |     |     |      |      |     |
| import                |     |     |      |      |     |
| interest              |     |     |      |      |     |
| notes                 |     |     |      |      |     |
| payees                |     |     |      |      |     |
| prices                |     |     |      |      |     |
| rewrite               |     |     |      |      |     |
| roi                   |     |     |      |      |     |
| stats                 |     |     |      |      |     |
| stockquotes           |     |     |      |      |     |
| tags                  |     |     |      |      |     |
| test                  |     |     |      |      |     |
-->

The output format is selected by the `-O/--output-format=FMT` option:
```shell
$ hledger print -O csv    # print CSV on stdout
```

or by the filename extension of an output file specified with the `-o/--output-file=FILE.FMT` option:
```shell
$ hledger balancesheet -o foo.csv    # write CSV to foo.csv
```

The `-O` option can be combined with `-o` to override the file extension, if needed:
```shell
$ hledger balancesheet -o foo.txt -O csv    # write CSV to foo.txt
```

Some notes about the various output formats:

### CSV output

- In CSV output, [digit group marks](#decimal-marks-digit-group-marks) (such as thousands separators)
  are disabled automatically.

### HTML output

- HTML output can be styled by an optional `hledger.css` file in the same directory.

### JSON output

- This is not yet much used; real-world feedback is welcome.

- Our JSON is rather large and verbose, since it is a faithful
  representation of hledger's internal data types. To understand the
  JSON, read the Haskell type definitions, which are mostly in
  <https://github.com/simonmichael/hledger/blob/master/hledger-lib/Hledger/Data/Types.hs>.

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

### SQL output

- This is not yet much used; real-world feedback is welcome.

- SQL output is expected to work with sqlite, MySQL and PostgreSQL

- SQL output is structured with the expectations that statements will
  be executed in the empty database. If you already have tables created
  via SQL output of hledger, you would probably want to either clear tables
  of existing data (via `delete` or `truncate` SQL statements) or drop
  tables completely as otherwise your postings will be duped.

## Commodity styles

When displaying amounts, hledger infers a standard display style for
each commodity/currency, as described below in
[Commodity display style](#commodity-display-style).

If needed, this can be overridden by a `-c/--commodity-style` option
(except for [cost amounts](#costs) and amounts displayed
by the [`print`](#print) command, which are always displayed with all
decimal digits).
For example, the following will force dollar amounts to be displayed as shown:

```shell
$ hledger print -c '$1.000,0'
```

This option can repeated to set the display style for multiple
commodities/currencies. Its argument is as described in 
the [commodity directive](#commodities).

## Colour

In terminal output, some commands can produce colour when the terminal supports it:

- if the `--color/--colour` option is given a value of `yes` or `always`
  (or `no` or `never`), colour will (or will not) be used;
- otherwise, if the `NO_COLOR` environment variable is set, colour will not be used;
- otherwise, colour will be used if the output (terminal or file) supports it.

## Box-drawing

In terminal output, you can enable unicode box-drawing characters to
render prettier tables:

- if the `--pretty` option is given a value of `yes` or `always`
  (or `no` or `never`), unicode characters will (or will not) be used;
- otherwise, unicode characters will not be used.

## Debug output

We intend hledger to be relatively easy to troubleshoot, introspect and develop.
You can add `--debug[=N]` to any hledger command line to see additional debug output.
N ranges from 1 (least output, the default) to 9 (maximum output).
Typically you would start with 1 and increase until you are seeing enough.
Debug output goes to stderr, and is not affected by `-o/--output-file` (unless you redirect stderr to stdout, eg: `2>&1`).
It will be interleaved with normal output, which can help reveal when parts of the code are evaluated.
To capture debug output in a log file instead, you can usually redirect stderr, eg:
```shell
hledger bal --debug=3 2>hledger.log
```

# Limitations

The need to precede add-on command options with `--` when invoked from hledger is awkward.

When input data contains non-ascii characters, a suitable system locale must be configured (or there will be an unhelpful error).
Eg on POSIX, set LANG to something other than C.

In a Microsoft Windows CMD window, non-ascii characters and colours are not supported.

On Windows, non-ascii characters may not display correctly when running a hledger built
in CMD in MSYS/CYGWIN, or vice-versa.

In a Cygwin/MSYS/Mintty window, the tab key is not supported in hledger add.

Not all of Ledger's journal file syntax is supported.
See [hledger and Ledger > Differences > journal format](/ledger.html#journal-format).

On large data files, hledger is slower and uses more memory than Ledger.

# Troubleshooting

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


# PART 2: DATA FORMATS

<a name="journal-format"></a>

# Journal

hledger's default file format, representing a General Journal.
Here's a cheatsheet/mini-tutorial,
or you can skip ahead to [About journal format](#about-journal-format).

## Journal cheatsheet

<!-- keep synced with hledger/test/ledger-compat/syntax/supported.test -->
```journal
# Here is the main syntax of hledger's journal format
# (omitting extra Ledger compatibility syntax).
# hledger journals contain comments, directives, and transactions, in any order:

###############################################################################
# 1. Comment lines are for notes or temporarily disabling things.
# They begin with #, ;, or a line containing the word "comment".

# hash comment line
; semicolon comment line
comment
These lines
are commented.
end comment

# Some but not all hledger entries can have same-line comments attached to them,
# from ; (semicolon) to end of line.

###############################################################################
# 2. Directives modify parsing or reports in some way.
# They begin with a word or letter (or symbol).

account actifs     ; type:A, declare an account that is an Asset. 2+ spaces before ;.
account passifs    ; type:L, declare an account that is a Liability, and so on.. (ALERX)
alias chkg = assets:checking
commodity $0.00
decimal-mark .
include /dev/null
payee Whole Foods
P 2022-01-01 AAAA $1.40
~ monthly    budget goals  ; <- 2+ spaces between period expression and description
    expenses:food       $400
    expenses:home      $1000
    budgeted

###############################################################################
# 3. Transactions are what it's all about; they are dated events,
# usually describing movements of money.
# They begin with a date.

# DATE DESCRIPTION           ; This is a transaction comment.
#   ACCOUNT NAME 1  AMOUNT1  ; <- posting 1. This is a posting comment.
#   ACCOUNT NAME 2  AMOUNT2  ; <- posting 2. Postings must be indented.
#               ; ^^ At least 2 spaces between account and amount.
#   ...  ; Any number of postings is allowed. The amounts must balance (sum to 0).

2022-01-01 opening balances are declared this way
    assets:checking          $1000  ; Account names can be anything. lower case is easy to type.
    assets:savings           $1000  ; assets, liabilities, equity, revenues, expenses are common.
    assets:cash:wallet        $100  ; : indicates subaccounts.
    liabilities:credit card  $-200  ; liabilities, equity, revenues balances are usually negative.
    equity                          ; One amount can be left blank; $-1900 is inferred here.

2022-04-15 * (#12345) pay taxes
    ; There can be a ! or * after the date meaning "pending" or "cleared".
    ; There can be a transaction code (text in parentheses) after the date/status.
    ; Amounts' sign represents direction of flow, or credit/debit:
    assets:checking          $-500  ; minus means removed from this account (credit)
    expenses:tax:us:2021      $500  ; plus  means added to this account (debit)
                                    ; revenue/expense categories are also "accounts"

2022-01-01 Whole Foods | payee name and description can be separated by a pipe char
    ; Transaction or posting comments can contain searchable tags,
    ; written NAME: or NAME:VALUE (value ends at comma or end of line).
    ; Here's tag1:with a value, and an empty tag2:
    ; A few tags have special meaning. A "date:" tag on a posting adjusts its date.
    assets:checking           $-50  ; date:2022-01-03, checking cleared two days later
    expenses:food

2022-01-01                          ; The description is optional.
    ; Any currency/commodity symbols are allowed, on either side.
    assets:cash:wallet     GBP -10
    expenses:clothing       GBP 10
    assets:gringotts           -10 gold
    assets:pouch                10 gold
    revenues:gifts              -2 "Liquorice Wands"  ; Complex symbols
    assets:bag                   2 "Liquorice Wands"  ; must be double-quoted.

2022-01-01 Cost in another commodity can be noted with @ or @@
    assets:investments           2.0 AAAA @ $1.50  ; @  means per-unit cost
    assets:investments           3.0 AAAA @@ $4    ; @@ means total cost
    assets:checking            $-7.00

2022-01-02 assert balances
    ; Balances can be asserted for extra error checking, in any transaction.
    assets:investments           0 AAAA = 5.0 AAAA
    assets:pouch                 0 gold = 10 gold
    assets:savings              $0      = $1000

1999-12-31 Ordering transactions by date is recommended but not required.
    ; Postings are not required.

2022.01.01 These date
2022/1/1   formats are
12/31      also allowed (but consistent YYYY-MM-DD is recommended).
```

## About journal format

hledger's usual data source is a plain text file containing journal entries in hledger journal format.
This file represents a standard accounting [general journal](http://en.wikipedia.org/wiki/General_journal).
I use file names ending in `.journal`, but that's not required.
The journal file contains a number of transaction entries,
each describing a transfer of money (or any commodity) between two or more named accounts,
in a simple format readable by both hledger and humans.

hledger's journal format is a compatible subset, mostly, of [ledger's
journal format](http://ledger-cli.org/3.0/doc/ledger3.html#Journal-Format),
so hledger can work with [compatible](/ledger.html#journal-format)
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
See [Editor configuration](/editors.html) at hledger.org for the full list.

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

Here's a description of each part of the file format (and hledger's data model).

A hledger journal file can contain three kinds of thing:
file comments, transactions, and/or directives
(counting periodic transaction rules and auto posting rules as directives).

## Comments

Lines in the journal will be ignored if they begin with a hash (`#`) or a semicolon (`;`). (See also [Other syntax](#other-syntax).)
hledger will also ignore regions beginning with a `comment` line and ending with an `end comment` line (or file end).
Here's a suggestion for choosing between them:

- `#` for top-level notes
- `;` for commenting out things temporarily
- `comment` for quickly commenting large regions (remember it's there, or you might get confused)

Eg:
```journal
# a comment line
; another commentline
comment
A multi-line comment block,
continuing until "end comment" directive
or the end of the current file.
end comment
```

Some hledger entries can have same-line comments attached to them, from ; (semicolon) to end of line.
See Transaction comments, Posting comments, and Account comments below.

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
- 0 or more indented [*posting* lines](#postings), describing what was transferred and the accounts involved
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

### Posting dates

You can give individual postings a different date from their parent
transaction, by adding a [posting comment](#posting-comment) containing a
[tag](#tags) (see 
below) like `date:DATE`.  This is probably the best
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
specified it will use the year of the transaction's date.  
The `date:` tag must have a valid simple date value if it is present,
eg a `date:` tag with no value is not allowed.

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

## Code

After the status mark, but before the description, you can optionally
write a transaction "code", enclosed in parentheses. This is a good
place to record a check number, or some other important transaction id
or reference number.

## Description

A transaction's description is the rest of the line following the date and status mark (or until a comment begins).
Sometimes called the "narration" in traditional bookkeeping, it can be used for whatever you wish,
or left blank. Transaction descriptions can be queried, unlike [comments](#transaction-comments).

### Payee and note

You can optionally include a `|` (pipe) character in descriptions to subdivide the description
into separate fields for payee/payer name on the left (up to the first `|`) and an additional note
field on the right (after the first `|`). This may be worthwhile if you need to do more precise
[querying](#queries) and [pivoting](#pivoting) by payee or by note.

## Transaction comments

Text following `;`, after a transaction description,
and/or on indented lines immediately below it, form comments for that transaction.
They are reproduced by `print` but otherwise ignored,
except they may contain [tags](#tags-1), which are not ignored.

```journal
2012-01-01 something  ; a transaction comment
    ; a second line of transaction comment
    expenses   1
    assets
```

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

## Account names

Accounts are the main way of categorising things in hledger.
As in Double Entry Bookkeeping, they can represent real world accounts (such as a bank account),
or more abstract categories such as "money borrowed from Frank" or "money spent on electricity".

You can use any account names you like, but we usually start with the traditional accounting categories,
which in english are `assets`, `liabilities`, `equity`, `revenues`, `expenses`.
(You might see these referred to as A, L, E, R, X for short.)

For more precise reporting, we usually divide the top level accounts into more detailed subaccounts,
by writing a full colon between account name parts. 
For example, from the account names `assets:bank:checking` and `expenses:food`, 
hledger will infer this hierarchy of five accounts:
```
assets
assets:bank
assets:bank:checking
expenses
expenses:food
```
Shown as an outline, the hierarchical tree structure is more clear:
```
assets
 bank
  checking
expenses
 food
```

hledger reports can summarise the account tree to any depth,
so you can go as deep as you like with subcategories,
but keeping your account names relatively simple may be best when starting out.

Account names may be capitalised or not; they may contain letters, numbers, symbols, or single spaces. 
Note, when an account name and an amount are written on the same line,
they must be separated by **two or more spaces** (or tabs).

Parentheses or brackets enclosing the full account name indicate [virtual postings](#virtual-postings),
described below.
Parentheses or brackets internal to the account name have no special meaning.

Account names can be altered temporarily or permanently by [account aliases](#account-aliases).

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

A *decimal mark* can be written as a period or a comma:

    1.23
    1,23456780000009

In the integer part of the quantity (left of the decimal mark), groups
of digits can optionally be separated by a *digit group mark* - a
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

To prevent confusing parsing mistakes and undetected typos, 
especially if your data contains digit group marks (eg, thousands separators),
we recommend explicitly declaring the decimal mark character in each journal file,
using a directive at the top of the file.
The [`decimal-mark`](#decimal-mark) directive is best,
otherwise [`commodity`](#commodities) directives will also work.
These are described below.

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

### Directives influencing number parsing and display

You can add `decimal-mark` and `commodity` directives to the journal,
to declare and control these things more explicitly and precisely.
These are described below, but here's a quick example:

```journal
# the decimal mark character used by all amounts in this file (all commodities)
decimal-mark .

# display styles for the $, EUR, INR and no-symbol commodities:
commodity $1,000.00
commodity EUR 1.000,00
commodity INR 9,99,99,999.00
commodity 1 000 000.9455
```

<a name="amount-display-style"></a>

### Commodity display style

For the amounts in each commodity, hledger chooses a consistent display style to use in most reports.
(Exceptions: [price amounts](#costs), 
and all amounts displayed by the [`print`](#print) command,
are displayed with all of their decimal digits visible.)

A commodity's display style is inferred as follows. 

First, if a [default commodity](#default-commodity) is declared with
`D`, this commodity and its style is applied to any no-symbol amounts
in the journal.

Then each commodity's style is inferred from one of the following, in order of preference:

- The [commodity directive](#commodities) for that commodity
  (including the no-symbol commodity), if any.
- The amounts in that commodity seen in the journal's transactions.
  (Posting amounts only; prices and periodic or auto rules are ignored, currently.)
- The built-in fallback style, which looks like this: `$1000.00`.
  (Symbol on the left, period decimal mark, two decimal places.)

A style is inferred from journal amounts as follows:

- Use the general style (decimal mark, symbol placement) of the first amount
- Use the first-seen digit group style (digit group mark, digit group sizes), if any
- Use the maximum number of decimal places of all.

Cost amounts don't affect the commodity display style directly,
but occasionally they can do so indirectly (eg when a posting's amount is
inferred using a cost). If you find this causing
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

The inferred commodity style can be [overridden](#commodity-styles) by supplying a command line option.

### Rounding

Amounts are stored internally as decimal numbers with up to 255 decimal places,
and displayed with the number of decimal places specified by the commodity display style.
Note, hledger uses [banker's rounding](https://en.wikipedia.org/wiki/Bankers_rounding): 
it rounds to the nearest even number, eg 0.5 displayed with zero decimal places is "0").


<a name="transaction-prices"></a>

## Costs

After a posting amount, you can note its cost (when buying) or selling price (when selling) in another commodity,
by writing either `@ UNITPRICE` or `@@ TOTALPRICE` after it.
This indicates a conversion transaction, where one commodity is exchanged for another.

(You might also see this called "transaction price" in hledger docs, discussions, or code;
that term was directionally neutral and reminded that it is a price specific to a transaction,
but we now just call it "cost", with the understanding that the transaction could be a purchase or a sale.)

Costs are usually written explicitly with `@` or `@@`, but can also be
inferred automatically for simple multi-commodity transactions.
Note, if costs are inferred, the order of postings is significant;
the first posting will have a cost attached, in the commodity of the second.

As an example, here are several ways to record purchases of a foreign
currency in hledger, using the cost notation either explicitly or
implicitly:

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
   and let hledger infer the price that balances the transaction.
   Note the effect of posting order: the price is added to first posting,
   making it `€100 @@ $135`, as in example 2:

    ```journal
    2009/1/1
      assets:euros     €100          ; one hundred euros purchased
      assets:dollars  $-135          ; for $135
    ```

Amounts can be converted to cost at report time using the [`-B/--cost`](#reporting-options) flag;
this is discussed more in the ˜[COST REPORTING](#cost-reporting) section.

### Other cost/lot notations

A slight digression for Ledger and Beancount users. Ledger has a number of cost/lot-related notations:

- `@ UNITCOST` and `@@ TOTALCOST`
  - expresses a conversion rate, as in hledger
  - when buying, also creates a lot than can be selected at selling time

- `(@) UNITCOST` and `(@@) TOTALCOST` ([virtual cost][ledger: virtual posting costs])
  - like the above, but also means "this cost was exceptional, don't use it when inferring market prices".

Currently, hledger treats the above like `@` and `@@`; the parentheses are ignored.

- `{=FIXEDUNITCOST}` and `{{{{=FIXEDTOTALCOST}}}}` ([fixed price][ledger: fixing lot prices])
  - when buying, means "this cost is also the fixed price, don't let it fluctuate in value reports"

- `{UNITCOST}` and `{{{{TOTALCOST}}}}` ([lot price][ledger: buying and selling stock])
  - can be used identically to `@ UNITCOST` and `@@ TOTALCOST`, also creates a lot
  - when selling, combined with `@ ...`, specifies an investment lot by its cost basis; does not check if that lot is present

- and related: `[YYYY/MM/DD]` ([lot date][ledger: lot dates])
  - when buying, attaches this acquisition date to the lot
  - when selling, selects a lot by its acquisition date

- `(SOME TEXT)` ([lot note][ledger: lot notes])
  - when buying, attaches this note to the lot
  - when selling, selects a lot by its note

Currently, hledger accepts any or all of the above in any order after the posting amount, but ignores them.
(This can break transaction balancing.)

[ledger: virtual posting costs]:    https://www.ledger-cli.org/3.0/doc/ledger3.html#Virtual-posting-costs
[ledger: buying and selling stock]: https://www.ledger-cli.org/3.0/doc/ledger3.html#Buying-and-Selling-Stock
[ledger: fixing lot prices]:        https://www.ledger-cli.org/3.0/doc/ledger3.html#Fixing-Lot-Prices
[ledger: lot dates]:                https://www.ledger-cli.org/3.0/doc/ledger3.html#Lot-dates
[ledger: lot notes]:                https://www.ledger-cli.org/3.0/doc/ledger3.html#Lot-notes

For Beancount users, the [notation][beancount: costs and prices] and [behaviour][beancount: how inventories work] is different:

- `@ UNITCOST` and `@@ TOTALCOST`
  - expresses a cost without creating a lot, as in hledger
  - when buying (augmenting) or selling (reducing) a lot, combined with `{...}`: documents the cost/selling price (not used for transaction balancing)

- `{UNITCOST}` and `{{{{TOTALCOST}}}}`
  - when buying (augmenting), expresses the cost for transaction balancing, and also creates a lot with this cost basis attached
  - when selling (reducing),
    - selects a lot by its cost basis
    - raises an error if that lot is not present or can not be selected unambiguously (depending on booking method configured)
    - expresses the selling price for transaction balancing

Currently, hledger accepts the `{UNITCOST}`/`{{{{TOTALCOST}}}}` notation but ignores it.

- variations: `{}`, `{YYYY-MM-DD}`, `{"LABEL"}`, `{UNITCOST, "LABEL"}`, `{UNITCOST, YYYY-MM-DD, "LABEL"}` etc.

Currently, hledger rejects these.


[beancount: costs and prices]:      https://beancount.github.io/docs/beancount_language_syntax.html#costs-and-prices
[beancount: how inventories work]:  https://beancount.github.io/docs/how_inventories_work.html


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
(Note: this flag currently does not disable balance assignments, described below).

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

### Assertions and multiple included files

Multiple files included with the [`include` directive](#including-files)
are processed as if concatenated into one file, preserving
their order and the posting order within each file.
It means that balance assertions in later files will see balance from earlier files.

And if you have multiple postings to an account on the same day, split
across multiple files, and you want to assert the account's balance on
that day, you'll need to put the assertion in the right file - the
last one in the sequence, probably.

### Assertions and multiple -f files

Unlike `include`, when multiple files are specified on the command
line with multiple `-f/--file` options, balance assertions will not
see balance from earlier files. This can be useful when you do not
want problems in earlier files to disrupt valid assertions in later
files.

If you do want assertions to see balance from earlier files, use
`include`, or concatenate the files temporarily.

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
This asserts that there are no other commodities in the account
besides the asserted one (or at least, that their balance is 0).

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

Balance assertions ignore [costs](#costs),
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

Balance assertions always consider both real and [virtual](#virtual-postings) postings;
they are not affected by the `--real/-R` flag or `real:` query.

### Assertions and auto postings

Balance assertions *are* affected by the `--auto` flag, which
generates [auto postings](#auto-postings), which can alter account
balances.  Because auto postings are optional in hledger, accounts
affected by them effectively have two balances. But balance assertions
can only test one or the other of these. So to avoid making fragile
assertions, either:

- assert the balance calculated with `--auto`, and always use `--auto` with that file
- or assert the balance calculated without `--auto`, and never use `--auto` with that file
- or avoid balance assertions on accounts affected by auto postings (or avoid auto postings entirely).

### Assertions and precision

Balance assertions compare the exactly calculated amounts,
which are not always what is shown by reports.
Eg a [commodity directive](#commodities)
may limit the display precision, but this will not affect balance assertions.
Balance assertion failure messages show exact amounts.

## Posting comments

Text following `;`, at the end of a posting line, 
and/or on indented lines immediately below it, form comments for that posting.
They are reproduced by `print` but otherwise ignored,
except they may contain [tags](#tags-1), which are not ignored.

```journal
2012-01-01
    expenses   1  ; a comment for posting 1
    assets
    ; a comment for posting 2
    ; a second comment line for posting 2
```

## Tags

Tags are a way to add extra labels or labelled data to transactions, postings, or accounts,
which you can then [search](#queries) or [pivot](#pivoting) on.

They are written as a word (optionally hyphenated) immediately followed by a full colon,
in a transaction or posting or account directive's [comment](#account-comments).
(This is an exception to the usual rule that things in comments are ignored.)
Eg, here four different tags are recorded: one on the checking account,
two on the transaction, and one on the expenses posting:

```journal
account assets:checking         ; accounttag:

2017/1/16 bought groceries      ; transactiontag-1:
    ; transactiontag-2:
    assets:checking        $-1
    expenses:food           $1  ; postingtag:
```

Postings also inherit tags from their transaction and their account.
And transactions also acquire tags from their postings (and postings' accounts).
So in the example above, the expenses posting effectively has all four tags
(by inheriting from account and transaction), 
and the transaction also has all four tags (by acquiring from the expenses posting).

You can list tag names with `hledger tags [NAMEREGEX]`,
or match by tag name with a `tag:NAMEREGEX` query.

### Tag values

Tags can have a value, which is any text after the colon up until a comma or end of line
(with surrounding whitespace removed).
Note this means that hledger tag values can not contain commas.
Eg in the following posting, the three tags' values are "value 1", "value 2", and "" (empty) respectively:
```journal
    expenses:food   $10    ; foo, tag1: value 1 , tag2:value 2, bar tag3: , baz
```

Note that tags can be repeated, and are additive rather than overriding:
when the same tag name is seen again with a new value, the new name:value pair is added to the tags.
(It is not possible to override a tag's value or remove a tag.)

You can list a tag's values with `hledger tags TAGNAME --values`,
or match by tag value with a `tag:NAMEREGEX=VALUEREGEX` query.

## Directives

A directive is a line in the journal beginning with a special keyword,
that influences how the journal is processed, how things are displayed, and so on.
hledger's directives are based on (a subset of) Ledger's, but there are many differences,
and also some differences between hledger versions.
Here are some more definitions:

- *subdirective* - Some directives support subdirectives, written
  indented below the parent directive.

- *decimal mark* - The character to interpret as a decimal mark
  (period or comma) when parsing amounts of a commodity.

- *display style* - How to display amounts of a commodity in output:
  symbol side and spacing, digit groups, decimal mark, and number of
  decimal places.

Directives are not required when starting out with hledger,
but you will probably want to add some as your needs grow. 
Here some key directives for particular needs:

| purpose                                                      | directives                            |
|--------------------------------------------------------------|---------------------------------------|
| **READING DATA:**                                            |                                       |
| Declare file's decimal mark to help parse amounts accurately | [`decimal-mark`]                      |
| Rewrite account names                                        | [`alias`]                             |
| Comment out sections of the data                             | [`comment`]                           |
| Include extra data files                                     | [`include`]                           |
| **GENERATING DATA:**                                         |                                       |
| Generate recurring transactions or budget goals              | [`~`]                                 |
| Generate extra postings on transactions                      | [`=`]                                 |
| **CHECKING FOR ERRORS:**                                     |                                       |
| Define valid entities to provide more error checking         | [`account`], [`commodity`], [`payee`] |
| **REPORTING:**                                               |                                       |
| Declare accounts' type and display order                     | [`account`]                           |
| Declare commodity display styles                             | [`commodity`]                         |
| Declare market prices                                        | [`P`]                                 |

### Directive effects

And here is what each directive does, and which files and journal entries (transactions) it affects:

<!-- <style> -->
<!-- table a code { white-space:nowrap; } -->
<!-- h1,h2,h3,h4,h5,h6 { color:red; } -->
<!-- </style> -->

| directive                     | what it does                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       | ends at file end?   |
|-------------------------------|------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|---------------------|
| **[`account`]**               | Declares an account, for [checking](#check) all entries in all files; <br>and its [display order](#account-display-order) and [type](#declaring-account-types). <br>Subdirectives: any text, ignored.                                                                                                                                                                                                                                                                                                                                                                              | N                   |
| **[`alias`]**                 | Rewrites account names, in following entries until end of current file or [`end aliases`]. <br>Command line equivalent: [`--alias`]                                                                                                                                                                                                                                                                                                                                                                                                                                                | Y                   |
| **[`comment`]**               | Ignores part of the journal file, until end of current file or `end comment`.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      | Y                   |
| **[`commodity`]**             | Declares up to four things: <br>1. a commodity symbol, for checking all amounts in all files <br>2. the decimal mark for parsing amounts of this commodity, in the following entries until end of current file (if there is no `decimal-mark` directive) <br>3. and the display style for amounts of this commodity <br>4. which is also the precision to use for balanced-transaction checking in this commodity.<br> Takes precedence over `D`. <br>Subdirectives: `format` (Ledger-compatible syntax). <br>Command line equivalent: [`-c/--commodity-style`](#commodity-styles) | N,<br>Y,<br>N,<br>N |
| **[`decimal-mark`]**          | Declares the decimal mark, for parsing amounts of all commodities in following entries until next `decimal-mark` or end of current file. Included files can override. Takes precedence over `commodity` and `D`.                                                                                                                                                                                                                                                                                                                                                                   | Y                   |
| **[`include`]**               | Includes entries and directives from another file, as if they were written inline. <br>Command line alternative: multiple [`-f/--file`](#multiple-files)                                                                                                                                                                                                                                                                                                                                                                                                                           | N                   |
| **[`payee`]**                 | Declares a payee name, for checking all entries in all files.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      | N                   |
| **[`P`]**                     | Declares the market price of a commodity on some date, for [value reports](#valuation).                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            | N                   |
| **[`~`]** (tilde)             | Declares a periodic transaction rule that generates future transactions with `--forecast` and budget goals with `balance --budget`.                                                                                                                                                                                                                                                                                                                                                                                                                                                | N                   |
| Other syntax:                 |                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    |                     |
| **[`apply account`]**         | Prepends a common parent account to all account names, in following entries until end of current file or `end apply account`.                                                                                                                                                                                                                                                                                                                                                                                                                                                      | Y                   |
| **[`D`]**                     | Sets a default commodity to use for no-symbol amounts;<br>and, if there is no `commodity` directive for this commodity: its decimal mark, balancing precision, and display style, as above.                                                                                                                                                                                                                                                                                                                                                                                        | Y,<br>Y,<br>N,<br>N |
| **[`Y`]**                     | Sets a default year to use for any yearless dates, in following entries until end of current file.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 | Y                   |
| **[`=`]** (equals)            | Declares an auto posting rule that generates extra postings on matched transactions with `--auto`, in current, parent, and child files (but not sibling files, see [#1212](https://github.com/simonmichael/hledger/issues/1212)).                                                                                                                                                                                                                                                                                                                                                  | partly              |
| **[Other Ledger directives]** | Other directives from Ledger's file format are accepted but ignored.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               |                     |

[`=`]:                 #auto-postings
[`D`]:                 #default-commodity
[`P`]:                 #market-prices
[`Y`]:                 #default-year
[`account`]:           #accounts
[`alias`]:             #account-aliases
[`--alias`]:           #account-aliases
[`apply account`]:     #default-parent-account
[`comment`]:           #file-comment
[`commodity`]:         #commodities
[`end aliases`]:       #end-aliases
[`include`]:           #including-files
[`payee`]:             #payees
[`~`]:                 #periodic-transactions
[Other Ledger directives]: #other-ledger-directives

### Directives and multiple files

If you use multiple `-f`/`--file` options, or the `include` directive,
hledger will process multiple input files. But directives which affect
input typically have effect only until the end of the file in which
they occur (and on any included files in that region).

This may seem inconvenient, but it's intentional; it makes reports
stable and deterministic, independent of the order of input. Otherwise
you could see different numbers if you happened to write -f options in
a different order, or if you moved includes around while cleaning up
your files.

It can be surprising though; for example, it means that 
[`alias` directives do not affect parent or sibling files](#aliases-and-multiple-files)
(see below).

## Accounts

`account` directives can be used to declare accounts 
(ie, the places that amounts are transferred from and to).
Though not required, these declarations can provide several benefits:

- They can document your intended chart of accounts, providing a reference.
- In [strict mode], they restrict which accounts may be posted to by transactions, which helps detect typos.
- They control account display order in reports, allowing non-alphabetic sorting (eg Revenues to appear above Expenses).
- They help with account name completion (in hledger add, hledger-web, hledger-iadd, ledger-mode, etc.)
- They can store additional account information as [comments](#account-comments), or as [tags](#tags) which can be used to filter or pivot reports.
- They can help hledger know your accounts' types (asset, liability, equity, revenue, expense), affecting reports like 
  [balancesheet](#balancesheet) and [incomestatement](#incomestatement).

They are written as the word `account` followed by a hledger-style [account name](#account-names), eg: 

```journal
account assets:bank:checking
```

Note, however, that accounts declared in account directives are not allowed to have surrounding
brackets and parentheses, unlike accounts used in postings.
So the following journal will not parse:

```journal 
account (assets:bank:checking)
```

### Account comments

Text following **two or more spaces** and `;` at the end of an account directive line,
and/or following `;` on indented lines immediately below it, form comments for that account.
They are ignored except they may contain [tags](#tags-1), which are not ignored.

The two-space requirement for same-line account comments is because `;` is allowed in account names.

```journal
account assets:bank:checking    ; same-line comment, at least 2 spaces before the semicolon
  ; next-line comment
  ; some tags - type:A, acctnum:12345
```

### Account subdirectives

Ledger-style indented subdirectives are also accepted, but currently ignored:

```journal
account assets:bank:checking
  format subdirective is ignored
```

### Account error checking

By default, accounts need not be declared; they come into existence when a posting references them.
This is convenient, but it means hledger can't warn you when you mis-spell an account name in the journal.
Usually you'll find that error later, as an extra account in balance reports, 
or an incorrect balance when reconciling.

In [strict mode], enabled with the `-s`/`--strict` flag, hledger will report an error if any transaction uses an account name that has not been declared by an [account directive](#account). Some notes:

- The declaration is case-sensitive; transactions must use the correct account name capitalisation.
- The account directive's scope is "whole file and below" (see [directives](#directives)). This means it affects all of the current file, and any files it includes, but not parent or sibling files. The position of account directives within the file does not matter, though it's usual to put them at the top.
- Accounts can only be declared in `journal` files, but will affect [included](#including-files) files of all types.
- It's currently not possible to declare "all possible subaccounts" with a wildcard; every account posted to must be declared.

### Account display order

The order in which account directives are written influences 
the order in which accounts appear in reports, hledger-ui, hledger-web etc.
By default accounts appear in alphabetical order,
but if you add these account directives to the journal file:
```journal
account assets
account liabilities
account equity
account revenues
account expenses
```

those accounts will be displayed in declaration order:
```shell
$ hledger accounts -1
assets
liabilities
equity
revenues
expenses
```

Any undeclared accounts are displayed last, in alphabetical order.

Sorting is done at each level of the account tree, 
within each group of sibling accounts under the same parent.
And currently, this directive:
```journal
account other:zoo
```
would influence the position of `zoo` among `other`'s subaccounts, but not the position of `other` among the top-level accounts.
This means:

- you will sometimes declare parent accounts (eg `account other` above) that you don't intend to post to, just to customize their display order
- sibling accounts stay together (you couldn't display `x:y` in between `a:b` and `a:c`).

### Account types

hledger knows that accounts come in several types: assets, liabilities, expenses and so on. 
This enables easy reports like [balancesheet] and [incomestatement], and filtering by account type with the [`type:` query](#queries). 

As a convenience, hledger will detect these account types automatically if you are using common english-language top-level account names (described below). 
But generally we recommend you declare types explicitly, 
by adding a `type:` [tag](#tags-1) to your top-level account directives. 
Subaccounts will inherit the type of their parent. 
The tag's value should be one of the [five main account types]:

- `A` or `Asset`		(things you own)
- `L` or `Liability`	(things you owe)
- `E` or `Equity`		(investment/ownership; balanced counterpart of assets & liabilities)
- `R` or `Revenue`		(what you received money from, AKA income; technically part of Equity)
- `X` or `Expense`		(what you spend money on; technically part of Equity)

or, it can be (these are used less often):

- `C` or `Cash`			(a subtype of Asset, indicating [liquid assets][CCE] for the [cashflow] report)
- `V` or `Conversion`	(a subtype of Equity, for conversions (see [COST REPORTING](#cost-reporting)).)

Here is a typical set of account type declarations:

```journal
account assets             ; type: A
account liabilities        ; type: L
account equity             ; type: E
account revenues           ; type: R
account expenses           ; type: X

account assets:bank        ; type: C
account assets:cash        ; type: C

account equity:conversion  ; type: V
```

[five main account types]:      https://en.wikipedia.org/wiki/Chart_of_accounts#Types_of_accounts
[accounting equation]: https://en.wikipedia.org/wiki/Accounting_equation
[CCE]:                 https://en.wikipedia.org/wiki/Cash_and_cash_equivalents
[account directive]:   #account

Here are some tips for working with account types.

- The rules for inferring types from account names are as follows.
  These are just a convenience that sometimes help new users get going;
  if they don't work for you, just ignore them and declare your account types.
  See also [Regular expressions](#regular-expressions).
  Note the Cash regexp changed in hledger 1.24.99.2.
  <!-- monospace to work around https://github.com/simonmichael/hledger/issues/1573 -->
  ```
  If account's name contains this (CI) regular expression:            | its type is:
  --------------------------------------------------------------------|-------------
  ^assets?(:.+)?:(cash|bank|che(ck|que?)(ing)?|savings?|current)(:|$) | Cash
  ^assets?(:|$)                                                       | Asset
  ^(debts?|liabilit(y|ies))(:|$)                                      | Liability
  ^equity:(trad(e|ing)|conversion)s?(:|$)                             | Conversion
  ^equity(:|$)                                                        | Equity
  ^(income|revenue)s?(:|$)                                            | Revenue
  ^expenses?(:|$)                                                     | Expense
  ```

- If you declare any account types, it's a good idea to declare an account for all of the
  account types, because a mixture of declared and name-inferred types can disrupt certain reports.

- Certain uses of [account aliases](#account-aliases) can disrupt account types.
  See [Rewriting accounts > Aliases and account types](#aliases-and-account-types).

- As mentioned above, subaccounts will inherit a type from their parent account. 
  More precisely, an account's type is decided by the first of these that exists:

  1. A `type:` declaration for this account.
  2. A `type:` declaration in the parent accounts above it, preferring the nearest.
  3. An account type inferred from this account's name.
  4. An account type inferred from a parent account's name, preferring the nearest parent.
  5. Otherwise, it will have no type.

- For troubleshooting, you can list accounts and their types with:
  ```
  $ hledger accounts --types [ACCTPAT] [-DEPTH] [type:TYPECODES]
  ```

## Account aliases

You can define account alias rules which rewrite your account names, or parts of them,
before generating reports.
This can be useful for:

- expanding shorthand account names to their full form, allowing easier data entry and a less verbose journal
- adapting old journals to your current chart of accounts
- experimenting with new account organisations, like a new hierarchy
- combining two accounts into one, eg to see their sum or difference on one line
- customising reports

Account aliases also rewrite account names in [account directives](#account).
They do not affect account names being entered via hledger add or hledger-web.

Account aliases are very powerful.
They are generally easy to use correctly, but you can also generate 
invalid account names with them; more on this below.

See also [Rewrite account names](/rewrite-account-names.html).

### Basic aliases

To set an account alias, use the `alias` directive in your journal file.
This affects all subsequent journal entries in the current file or its
[included files](#including-files)
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
indicated by wrapping the pattern in forward slashes.
(This is the only place where hledger requires forward slashes around a regular expression.)

Eg:
```journal
alias /REGEX/ = REPLACEMENT
```
or:
```cli
$ hledger --alias '/REGEX/=REPLACEMENT' ...
```

Any part of an account name matched by REGEX will be replaced by REPLACEMENT.
REGEX is case-insensitive as usual. 

If you need to match a forward slash, escape it with a backslash, eg `/\/=:`.

If REGEX contains parenthesised match groups, these can be referenced
by the usual backslash and number in REPLACEMENT:

```journal
alias /^(.+):bank:([^:]+):(.*)/ = \1:\2 \3
; rewrites "assets:bank:wells fargo:checking" to  "assets:wells fargo checking"
```

REPLACEMENT continues to the end of line (or on command line, to end of option argument), 
so it can contain trailing whitespace.

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

You can clear (forget) all currently defined aliases
(seen in the journal so far, or defined on the command line)
with this directive:

```journal
end aliases
```

### Aliases can generate bad account names

Be aware that account aliases can produce malformed account names,
which could cause confusing reports or invalid [`print`](#print) output.
For example, you could erase all account names:

```journal
2021-01-01
  a:aa     1
  b
```
```shell
$ hledger print --alias '/.*/='
2021-01-01
                   1

```

The above `print` output is not a valid journal. 
Or you could insert an illegal double space, causing `print` output
that would give a different journal when reparsed:

```journal
2021-01-01
  old    1
  other
```
```shell
$ hledger print --alias old="new  USD" | hledger -f- print
2021-01-01
    new             USD 1
    other
```

### Aliases and account types

If an account with a type declaration (see [Declaring accounts > Account types](#account-types))
is renamed by an alias, normally the account type remains in effect.

However, renaming in a way that reshapes the account tree 
(eg renaming parent accounts but not their children, or vice versa) 
could prevent child accounts from inheriting the account type of their parents.
<!--
Eg: with `account expenses  ; type:X` and `hledger acc --alias /expenses$/=expenses:uncategorised`, 
"expenses:uncategorised" is declared to be type X and "expenses" has no type declared, 
nor do its children (because of the $, they do not get aliased and remain under "expenses").
-->

Secondly, if an account's type is being inferred from its name,
renaming it by an alias could prevent or alter that.
<!--
With `hledger -f examples/sample.journal acc --alias expenses=foo` and no account declarations, 
it tries and fails to infer a type for "foo".
-->

If you are using account aliases and the [`type:` query](#queries) is not matching accounts as you expect,
try troubleshooting with the accounts command, eg something like:

```shell
$ hledger accounts --alias assets=bassetts type:a
```

## Commodities

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

Other indented subdirectives are currently ignored.

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

Even in the presence of commodity directives, the commodity display style 
can still be [overridden](#commodity-styles) by supplying a command line option.

### Commodity error checking

In [strict mode], enabled with the `-s`/`--strict` flag, hledger will report an error if a
commodity symbol is used that has not been declared by a [`commodity` directive](#commodities).
This works similarly to [account error checking](#account-error-checking), see the notes there for more details.

Note, this disallows amounts without a commodity symbol,
because currently it's not possible (?) to declare the "no-symbol" commodity with a directive.
This is one exception for convenience: zero amounts are always allowed to have no commodity symbol.

## Decimal mark

You can use a `decimal-mark` directive - usually one per file, at the
top of the file - to declare which character represents a decimal mark
when parsing amounts in this file. It can look like
```journal
decimal-mark .
```
or
```journal
decimal-mark ,
```

This prevents any [ambiguity](#decimal-marks-digit-group-marks) when
parsing numbers in the file, so we recommend it, especially if the
file contains digit group marks (eg thousands separators).

## Include files

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

## Market prices

The `P` directive declares a market price, which is
a conversion rate between two commodities on a certain date.
These are often obtained from a
[stock exchange](https://en.wikipedia.org/wiki/Stock_exchange),
cryptocurrency exchange, or the
[foreign exchange market](https://en.wikipedia.org/wiki/Foreign_exchange_market).

The format is:

```journal
P DATE COMMODITY1SYMBOL COMMODITY2AMOUNT
```
DATE is a [simple date](#simple-dates),
COMMODITY1SYMBOL is the symbol of the commodity being priced,
and COMMODITY2AMOUNT is the [amount](#amounts) (symbol and quantity) of commodity 2
that one unit of commodity 1 is worth on this date.
Examples:
```journal
# one euro was worth $1.35 from 2009-01-01 onward:
P 2009-01-01 € $1.35

# and $1.40 from 2010-01-01 onward:
P 2010-01-01 € $1.40
```

The `-V`, `-X` and `--value` flags use these market prices to show amount values
in another commodity. See [Valuation](#valuation).

<a name="automated-postings"></a>

## Payees

The `payee` directive can be used to declare a limited set of payees which may appear in transaction descriptions.
The ["payees" check](#check) will report an error if any transaction refers to a payee that has not been declared.
Eg:

```journal
payee Whole Foods
```

Any indented subdirectives are currently ignored.

## Periodic transactions

Periodic transaction rules describe transactions that recur.
They allow hledger to generate temporary future transactions (visible only in reports)
to help with [forecasting](#forecasting) or [budgeting](#budgeting).

Periodic transactions can be a little tricky, so before you use them,
read this whole section, or at least these tips:

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

### Periodic rules and relative dates

Partial or relative dates (like `12/31`, `25`, `tomorrow`, `last week`, `next quarter`)
are usually not recommended in periodic rules, since the results will change as time passes.
If used, they will be interpreted relative to, in  order of preference:

1. the first day of the default year specified by a recent `Y` directive
2. or the date specified with `--today`
3. or the date on which you are running the report.

They will not be affected at all by report period or forecast period dates.

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

## Other syntax

hledger journal format supports quite a few other features,
mainly to make interoperating with or converting from Ledger easier.
Note some of the features below are powerful and can be useful in special cases,
but in general, features in this section are considered less important
or even not recommended for most users.
Downsides are mentioned to help you decide if you want to use them.

### Auto postings

"Automated postings" or "auto postings" are extra postings which get
added automatically to transactions which match certain queries,
defined by "auto posting rules", when you use the `--auto` flag.

Downsides: depending on generated data for your reports makes
your financial data less portable, less future-proof,
and less trustworthy in an audit. Also, because the feature
is optional, other features like balance assertions can break
depending on whether it is on or off.

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

#### Auto postings and multiple files

An auto posting rule can affect any transaction in the current file,
or in any parent file or child file. Note, currently it will not
affect sibling files (when multiple `-f`/`--file` are used - see
[#1212](https://github.com/simonmichael/hledger/issues/1212)).

#### Auto postings and dates

A [posting date](#posting-dates) (or secondary date) in the matched posting,
or (taking precedence) a posting date in the auto posting rule itself,
will also be used in the generated posting.

#### Auto postings and transaction balancing / inferred amounts / balance assertions

Currently, auto postings are added:

- after [missing amounts are inferred, and transactions are checked for balancedness](#postings),
- but before [balance assertions](#balance-assertions) are checked.

Note this means that journal entries must be balanced both before and
after auto postings are added. This changed in hledger 1.12+; see
[#893](https://github.com/simonmichael/hledger/issues/893) for
background.

This also means that you cannot have more than one auto-posting with a missing
amount applied to a given transaction, as it will be unable to infer amounts.

#### Auto posting tags

Automated postings will have some extra [tags](#tags-1):

- `generated-posting:= QUERY`  - shows this was generated by an auto posting rule, and the query
- `_generated-posting:= QUERY` - a hidden tag, which does not appear in hledger's output.
                                     This can be used to match postings generated "just now",
                                     rather than generated in the past and saved to the journal.

Also, any transaction that has been changed by auto posting rules will have these tags added:

- `modified:` - this transaction was modified
- `_modified:` - a hidden tag not appearing in the comment; this transaction was modified "just now".


### Balance assignments

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

Downsides: using balance assignments makes your journal less explicit;
to know the exact amount posted, you have to run hledger or do the
calculations yourself, instead of just reading it.  Also balance
assignments' forcing of balances can hide errors.  These things make
your financial data less portable, less future-proof, and less
trustworthy in an audit.

#### Balance assignments and prices

A [cost](#costs) in a balance assignment
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

### Bracketed posting dates

For setting [posting dates](#posting-dates) and [secondary posting dates](#secondary-dates),
Ledger's bracketed date syntax is also supported:
`[DATE]`, `[DATE=DATE2]` or `[=DATE2]` in posting comments. 
hledger will attempt to parse any square-bracketed sequence of the
`0123456789/-.=` characters in this way.
With this syntax, DATE infers its year from the transaction and DATE2
infers its year from DATE.

Downsides: another syntax to learn, redundant with hledger's
`date:`/`date2:` tags, and confusingly similar to Ledger's lot date syntax.

### Default commodity

The `D` directive sets a default commodity, to be used for any
subsequent commodityless amounts (ie, plain numbers) seen while
parsing the journal. This effect lasts until the next `D` directive,
or the end of the journal.

For compatibility/historical reasons, `D` also acts like a [`commodity` directive](#commodities)
(setting the commodity's decimal mark for parsing and [display style](#amount-display-format) for output).

The syntax is `D AMOUNT`.
As with `commodity`, the amount must include a decimal mark (either period or comma).
Eg:
```journal
; commodity-less amounts should be treated as dollars
; (and displayed with the dollar sign on the left, thousands separators and two decimal places)
D $1,000.00

1/1
  a     5  ; <- commodity-less amount, parsed as $5 and displayed as $5.00
  b
```

If both `commodity` and `D` directives are found for a commodity, 
`commodity` takes precedence for setting decimal mark and display style.

If you are using `D` and also [checking](#check) commodities, you will need
to add a `commodity` directive similar to the `D`. 
(The `hledger check commodities` command expects `commodity` directives, and ignores `D`).

Downsides: omitting commodity symbols
makes your financial data less explicit, less portable, and less trustworthy in an audit.
It is usually an unsustainable shortcut; sooner or later you will want to track multiple commodities.
D is overloaded with functions redundant with `commodity` and `decimal mark`.
And it works differently from Ledger's `D`.

### Default parent account

The `apply account` directive sets a parent account which will be prepended
to all accounts in following entries, until an `end apply account` directive
or end of current file.
Eg:

```journal
apply account home

2010/1/1
    food    $10
    cash

end apply account
```

is equivalent to:
```journal
2010/01/01
    home:food           $10
    home:cash          $-10
```

`account` directives are also affected, and so is any `include`d content.

Account names entered via hledger add or hledger-web are not affected.

Account aliases, if any, are applied after the parent account is prepended.

Downsides: this can make your financial data less explicit, less portable,
and less trustworthy in an audit.

### Default year

You can set a default year to be used for subsequent dates which don't
specify a year. This is a line beginning with `Y` (or `year` or `apply year`) followed by the year. Eg:

```journal
Y2009  ; set default year to 2009

12/15  ; equivalent to 2009/12/15
  expenses  1
  assets

year 2010  ; change default year to 2010

2009/1/30  ; specifies the year, not affected
  expenses  1
  assets

1/31   ; equivalent to 2010/1/31
  expenses  1
  assets
```

Downsides: 
omitting the year (from primary transaction dates, at least) 
makes your financial data less explicit, less portable, and less trustworthy in an audit.
Such dates can get separated from their corresponding Y directive,
eg when evaluating a region of the journal in your editor.
A missing Y directive makes reports dependent on today's date.

### Secondary dates

A secondary date is written after the primary date, following an
equals sign. If the year is omitted, the primary date's year is
assumed. When running reports, the primary (left) date is used by
default, but with the `--date2` flag (or `--aux-date` or
`--effective`), the secondary (right) date will be used instead.

The meaning of secondary dates is up to you, but it's best to follow a
consistent rule.  Eg "primary = the bank's clearing date, secondary =
date the transaction was initiated, if different".

Downsides: makes your financial data more complicated, less portable,
and less trustworthy in an audit. Keeping the meaning of the two dates
consistent requires discipline, and you have to remember which
reporting mode is appropriate for a given report.
[Posting dates](#posting-dates) are simpler and better.

### Star comments

Lines beginning with `*` (star/asterisk) are also comment lines. 
This feature allows Emacs users to insert org headings in their journal,
allowing them to fold/unfold/navigate it like an outline when viewed with org mode.

Downsides: another, unconventional comment syntax to learn.
Decreases your journal's portability.
And switching to Emacs org mode just for folding/unfolding meant losing the benefits of ledger mode;
nowadays you can add outshine mode to ledger mode to get folding
without losing ledger mode's features.

### Valuation expressions

Ledger allows a valuation function or value to be written in double parentheses after an amount.
hledger ignores these.

### Virtual postings

A posting with parentheses around the account name is called a *virtual posting* or *unbalanced posting*,
which means it is exempt from the usual rule that a transaction's postings must balance add up to zero.

This is not part of double entry bookkeeping, so you might choose to avoid this feature.
Or you can use it sparingly for certain special cases where it can be convenient.
Eg, you could set opening balances without using a balancing equity account:

```journal
2022-01-01 opening balances
  (assets:checking)   $1000
  (assets:savings)    $2000
```

A posting with brackets around the account name is called a *balanced virtual posting*.
The balanced virtual postings in a transaction must add up to zero (separately from other postings).
Eg:

```journal
2022-01-01 buy food with cash, update budget envelope subaccounts, & something else
  assets:cash                    $-10  ; <- these balance each other
  expenses:food                    $7  ; <-
  expenses:food                    $3  ; <-
  [assets:checking:budget:food]  $-10  ;   <- and these balance each other
  [assets:checking:available]     $10  ;   <-
  (something:else)                 $5  ;     <- this is not required to balance
```

Postings whose account names are neither parenthesised nor bracketed are called *real postings*.
You can exclude virtual postings from reports with the `-R/--real` flag or a `real:1` query.

Downsides: violates double entry bookkeeping, 
can be used to avoid figuring out correct entries,
makes your financial data less portable and less trustworthy in an audit.


### Other Ledger directives

These other Ledger directives are currently accepted but ignored.
This allows hledger to read more Ledger files, 
but be aware that hledger's reports may differ from Ledger's if you use these.

```journal
apply fixed COMM AMT
apply tag   TAG
assert      EXPR
bucket / A  ACCT
capture     ACCT REGEX
check       EXPR
define      VAR=EXPR
end apply fixed
end apply tag
end apply year
end tag
eval / expr EXPR
python
  PYTHONCODE
tag         NAME
value       EXPR
--command-line-flags
```

See also <https://hledger.org/ledger.html> for a detailed hledger/Ledger syntax comparison.

<a name="csv-format"></a>

# CSV

hledger can read [CSV](http://en.wikipedia.org/wiki/Comma-separated_values) files
(Character Separated Value - usually comma, semicolon, or tab) containing dated records,
automatically converting each record into a transaction.

(To learn about *writing* CSV, see [CSV output](#csv-output).)

For best error messages when reading CSV/TSV/SSV files,
make sure they have a corresponding `.csv`, `.tsv` or `.ssv` file extension
or use a hledger file prefix (see [File Extension](#file-extension) below).

Each CSV file must be described by a corresponding *rules file*.  
This contains rules describing the CSV data (header line, fields
layout, date format etc.), how to construct hledger transactions from
it, and how to categorise transactions based on description or other
attributes.

By default hledger looks for a rules file named like the CSV file with
an extra `.rules` extension, in the same directory. Eg when asked to
read `foo/FILE.csv`, hledger looks for `foo/FILE.csv.rules`. 
You can specify a different rules file with the `--rules-file` option. 
If no rules file is found, hledger will create a sample rules file,
which you'll need to adjust.

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
fields       date, description, , amount
date-format  %d/%m/%Y
```
```shell
$ hledger print -f basic.csv
2019-11-12 Foo
    expenses:unknown           10.23
    income:unknown            -10.23

```

There's an introductory [Importing CSV data](/import-csv.html) tutorial on hledger.org,
and more [CSV rules examples](#csv-rules-examples) below,
and a larger collection at <https://github.com/simonmichael/hledger/tree/master/examples/csv>.

## CSV rules cheatsheet

The following kinds of rule can appear in the rules file, in any order.
(Blank lines and lines beginning with `#` or `;` or `*` are ignored.)

|                                                 |                                                                                                |
|-------------------------------------------------|------------------------------------------------------------------------------------------------|
| [**`separator`**](#separator)                   | declare the field separator, instead of relying on file extension                              |
| [**`skip`**](#skip)                             | skip one or more header lines at start of file                                                 |
| [**`date-format`**](#date-format)               | declare how to parse CSV dates/date-times                                                      |
| [**`timezone`**](#timezone)                     | declare the time zone of ambiguous CSV date-times                                              |
| [**`newest-first`**](#newest-first)             | improve txn order when: there are multiple records, newest first, all with the same date       |
| [**`intra-day-reversed`**](#intra-day-reversed) | improve txn order when: same-day txns are in opposite order to the overall file                |
| [**`decimal-mark`**](#decimal-mark-1)           | declare the decimal mark used in CSV amounts, when ambiguous                                   |
| [**`fields` list**](#fields-list)               | name CSV fields for easy reference, and optionally assign their values to hledger fields       |
| [**Field assignment**](#field-assignment)       | assign a CSV value or interpolated text value to a hledger field                               |
| [**`if` block**](#if-block)                     | conditionally assign values to hledger fields, or `skip` a record or `end` (skip rest of file) |
| [**`if` table**](#if-table)                     | conditionally assign values to hledger fields, using compact syntax                            |
| [**`balance-type`**](#balance-type)             | select which type of balance assertions/assignments to generate                                |
| [**`include`**](#include)                       | inline another CSV rules file                                                                  |

[Working with CSV](#working-with-csv) tips can be found below,
including [How CSV rules are evaluated](#how-csv-rules-are-evaluated).

## `separator`

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

## `skip`

```rules
skip N
```
The word `skip` followed by a number (or no number, meaning 1)
tells hledger to ignore this many non-empty lines at the start of the input data.
(Empty/blank lines are skipped automatically, so you don't need to count those.)
You'll need this whenever your CSV data contains header lines.
Header lines skipped in this way are ignored, and not parsed as CSV.

`skip` can also be used inside [if blocks](#if-block) (described below),
to skip individual data records.
Note records skipped in this way are still required to be [valid CSV](#valid-csv), even though otherwise ignored.

## `date-format`

```rules
date-format DATEFMT
```
This is a helper for the `date` (and `date2`) fields.
If your CSV dates are not formatted like `YYYY-MM-DD`, `YYYY/MM/DD` or `YYYY.MM.DD`,
you'll need to add a date-format rule describing them with a strptime-style date parsing pattern - 
see <https://hackage.haskell.org/package/time/docs/Data-Time-Format.html#v:formatTime>.
The pattern must parse the CSV date value completely.
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

## `timezone`

```rules
timezone TIMEZONE
```

When CSV contains date-times that are implicitly in some time zone
other than yours, but containing no explicit time zone information,
you can use this rule to declare the CSV's native time zone,
which helps prevent off-by-one dates.

When the CSV date-times do contain time zone information, 
you don't need this rule; instead, use `%Z` in `date-format`
(or `%z`, `%EZ`, `%Ez`; see the formatTime link above).

In either of these cases, hledger will do a time-zone-aware conversion,
localising the CSV date-times to your current system time zone.
If you prefer to localise to some other time zone, eg for reproducibility,
you can (on unix at least) set the output timezone with the TZ environment variable, eg:
```shell
$ TZ=-1000 hledger print -f foo.csv  # or TZ=-1000 hledger import foo.csv
```

`timezone` currently does not understand timezone names, except
"UTC", "GMT", "EST", "EDT", "CST", "CDT", "MST", "MDT", "PST", or "PDT".
For others, use numeric format: +HHMM or -HHMM.

## `newest-first`

hledger tries to ensure that the generated transactions will be ordered chronologically,
including intra-day transactions.
Usually it can auto-detect how the CSV records are ordered.
But if it encounters CSV where all records are on the same date,
it assumes that the records are oldest first.
If in fact the CSV's records are normally newest first, like:
```csv
2022-10-01, txn 3...
2022-10-01, txn 2...
2022-10-01, txn 1...
```
you can add the `newest-first` rule to help
hledger generate the transactions in correct order.

```rules
# same-day CSV records are newest first
newest-first
```

## `intra-day-reversed`

CSV records for each day are sometimes ordered in reverse compared to the overall date order.
Eg, here dates are newest first, but the transactions on each date are oldest first:
```csv
2022-10-02, txn 3...
2022-10-02, txn 4...
2022-10-01, txn 1...
2022-10-01, txn 2...
```
In this situation, add the `intra-day-reversed` rule, and hledger will compensate,
improving the order of transactions.
```rules
# transactions within each day are reversed with respect to the overall date order
intra-day-reversed
```



## `decimal-mark`

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

## `fields` list

```rules
fields FIELDNAME1, FIELDNAME2, ...
```
A fields list (the word `fields` followed by comma-separated field names) is optional, but convenient.
It does two things:

1. It names the CSV field in each column.
   This can be convenient if you are referencing them in other rules,
   so you can say `%SomeField` instead of remembering `%13`.

2. Whenever you use one of the special [hledger field names](#field names) (described below),
   it assigns the CSV value in this position to that hledger field.
   This is the quickest way to populate hledger's fields and build a transaction.

Here's an example that says
"use the 1st, 2nd and 4th fields as the transaction's date, description and amount;
name the last two fields for later reference; and ignore the others":
```rules
fields date, description, , amount, , , somefield, anotherfield
```

In a fields list, the separator is always comma; it is unrelated to the CSV file's separator.
Also:

- There must be least two items in the list (at least one comma).
- Field names may not contain spaces. Spaces before/after field names are optional.
- Field names may contain `_` (underscore) or `-` (hyphen).
- Fields you don't care about can be given a dummy name or an empty name.

If the CSV contains column headings, it's convenient to use these for your field names,
suitably modified (eg lower-cased with spaces replaced by underscores).

Sometimes you may want to alter a CSV field name to avoid assigning to a hledger field with the same name.
Eg you could call the CSV's "balance" field `balance_` to avoid directly setting hledger's `balance` field (and generating a balance assertion).

## Field assignment

```rules
HLEDGERFIELD FIELDVALUE
```

Field assignments are the more flexible way to assign CSV values to hledger fields.
They can be used instead of or in addition to a [fields list](#fields-list) (see above).

To assign a value to a hledger field, write the [field name](#field-names)
(any of the standard hledger field/pseudo-field names, defined below),
a space, followed by a text value on the same line.
This text value may interpolate CSV fields,
referenced by their 1-based position in the CSV record (`%N`),
or by the name they were given in the fields list (`%CSVFIELD`).

Some examples:

```rules
# set the amount to the 4th CSV field, with " USD" appended
amount %4 USD

# combine three fields to make a comment, containing note: and date: tags
comment note: %somefield - %anotherfield, date: %1
```

Tips:

- Interpolation strips outer whitespace (so a CSV value like `" 1 "`
becomes `1` when interpolated)
([#1051](https://github.com/simonmichael/hledger/issues/1051)).
- Interpolations always refer to a CSV field - 
  you can't interpolate a hledger field.
  (See [Referencing other fields](#referencing-other-fields) below).

## Field names

Note the two kinds of field names mentioned here, and used only in hledger CSV rules files:

1. **CSV field names** (`CSVFIELD` in these docs):
   you can optionally name the CSV columns for easy reference
   (since hledger doesn't yet automatically recognise column headings in a CSV file),
   by writing arbitrary names in a `fields` list, eg:
   ```csv
   fields When, What, Some_Id, Net, Total, Foo, Bar
   ```

2. Special **hledger field names** (`HLEDGERFIELD` in these docs):
   you must set at least some of these to generate the hledger transaction from a CSV record,
   by writing them as the left hand side of a [field assignment](#field-assignment), eg:
   ```csv
   date        %When
   code        %Some_Id
   description %What
   comment     %Foo %Bar
   amount1     $ %Total
   ```
   or directly in a [`fields` list](#fields-list):
   ```csv
   fields date, description, code, , amount1, Foo, Bar
   currency $
   comment  %Foo %Bar
   ```
   
Here are all the special hledger field names available, and what happens when you assign values to them:

### date field

Assigning to `date` sets the [transaction date](#simple-dates).

### date2 field

`date2` sets the transaction's [secondary date](#secondary-dates), if any.

### status field

`status` sets the transaction's [status](#status), if any.

### code field

`code` sets the transaction's [code](#code), if any.

### description field

`description` sets the transaction's [description](#description), if any.

### comment field

`comment` sets the transaction's [comment](#transaction-comments), if any.

`commentN`, where N is a number, sets the Nth posting's comment.

You can assign multi-line comments by writing literal `\n` in the code. A comment starting with `\n` will begin on a new line.

Comments can contain [tags](#tags), as usual.

### account field

Assigning to `accountN`, where N is 1 to 99, 
sets the account name of the Nth [posting](#postings),
and causes that posting to be generated.

Most often there are two postings, so you'll want to set `account1` and `account2`.
Typically `account1` is associated with the CSV file, and is set once with a top-level assignment,
while `account2` is set based on each transaction's description, in [conditional rules](#if-blocks).

If a posting's account name is left unset but its amount is set (see below),
a default account name will be chosen (like "expenses:unknown" or "income:unknown").

### amount field

There are several "amount" field name variants, useful for different situations:


- `amountN` sets the amount of the Nth posting, and causes that posting to be generated.
  By assigning to `amount1`, `amount2`, ... etc. you can generate up to 99 postings.
  Posting numbers don't have to be consecutive; in certain situations using a high number
  might be helpful to influence the layout of postings.
  
- `amountN-in` and `amountN-out` should be used instead, as a pair, when and only when
  the amount must be obtained from two CSV fields.
  Eg when the CSV has separate Debit and Credit fields instead of a single Amount field.
  Note:
  
  - Don't think "-in is for the first posting and -out is for the second posting" - that's not correct.
    Think: "`amountN-in` and `amountN-out` together detect the amount for posting N, by inspecting two CSV fields at once."
  - hledger assumes both CSV fields are unsigned, and will automatically negate the -out value.
  - It also expects that at least one of the values is empty or zero, so it knows which one to ignore.
    If that's not the case you'll need an if rule (see Setting amounts below).
  
- `amount`, with no posting number (and similarly, `amount-in` and `amount-out` with no number)
  are an older syntax. We keep them for backwards compatibility, and because they have special behaviour
  that is sometimes convenient:
  
  - They set the amount of posting 1 and (negated) the amount of posting 2.
  - Posting 2's amount will be converted to cost if it has a [cost price](#costs).
  - Any of the newer rules for posting 1 or 2 (like `amount1`, or `amount2-in` and `amount2-out`)
    will take precedence. This allows incrementally migrating old rules files to the new syntax.

There's more to say about amount-setting that doesn't fit here; please see also ["Setting amounts"](#setting-amounts) below.

### currency field

`currency` sets a currency symbol, to be prepended to all postings' amounts.
You can use this if the CSV amounts do not have a currency symbol, eg if it is in a separate column.

`currencyN` prepends a currency symbol to just the Nth posting's amount.

### balance field

`balanceN` sets a [balance assertion](#balance-assertions) amount
(or if the posting amount is left empty, a [balance assignment](#balance-assignments))
on posting N.

`balance` is a compatibility spelling for hledger <1.17;
it is equivalent to `balance1`.

You can adjust the type of assertion/assignment with the
[`balance-type` rule](#balance-type) (see below).

See [Tips](#tips) below for more about setting amounts and currency.


## `if` block

Rules can be applied conditionally, depending on patterns in the CSV data.
This allows flexibility; in particular, it is how you can categorise transactions,
selecting an appropriate account name based on their description (for example).
There are two ways to write conditional rules: "if blocks", described here,
and "if tables", described below.

An if block is the word `if` 
and one or more "matcher" expressions (can be a word or phrase),
one per line, starting either on the same or next line;
followed by one or more indented rules.
Eg,

```rules
if MATCHER
 RULE
```

or

```rules
if
MATCHER
MATCHER
MATCHER
 RULE
 RULE
```

If any of the matchers succeeds, all of the indented rules will be applied.
They are usually [field assignments](#field-assignments),
but the following special rules may also be used within an if block:

- `skip` - skips the matched CSV record (generating no transaction from it)
- `end`  - skips the rest of the current CSV file.

Some examples:

```rules
# if the record contains "groceries", set account2 to "expenses:groceries"
if groceries
 account2 expenses:groceries
```

```rules
# if the record contains any of these phrases, set account2 and a transaction comment as shown
if
monthly service fee
atm transaction fee
banking thru software
 account2 expenses:business:banking
 comment  XXX deductible ? check it
```

```rules
# if an empty record is seen (assuming five fields), ignore the rest of the CSV file
if ,,,,
 end
```

## Matchers

There are two kinds:

1. A record matcher is a word or single-line text fragment or regular expression (`REGEX`), 
   which hledger will try to match case-insensitively anywhere within the CSV record.\
   Eg: `whole foods`

2. A field matcher is preceded with a percent sign and [CSV field name](#field-names) (`%CSVFIELD REGEX`).
   hledger will try to match these just within the named CSV field.\
   Eg: `%date 2023`

The regular expression is (as usual in hledger) a POSIX extended regular expression,
that also supports GNU word boundaries (`\b`, `\B`, `\<`, `\>`),
and nothing else.
If you have trouble, see "Regular expressions" in the hledger manual (<https://hledger.org/hledger.html#regular-expressions>).

With record matchers, it's important to know that the record matched is not the original CSV record, but a modified one:
separators will be converted to commas, and enclosing double quotes (but not enclosing whitespace) are removed.
So for example, when reading an SSV file, if the original record was:
```ssv
2020-01-01; "Acme, Inc.";  1,000
```
the regex would see, and try to match, this modified record text:
```
2020-01-01,Acme, Inc.,  1,000
```

When an if block has multiple matchers, they are combined as follows:

- By default they are OR'd (any one of them can match)
- When a matcher is preceded by ampersand (`&`) it will be AND'ed with the previous matcher (both of them must match).

There's not yet an easy syntax to negate a matcher.

## `if` table

"if tables" are an alternative to [if blocks](#if-blocks);
they can express many matchers and field assignments in a more compact tabular format, like this:

```rules
if,HLEDGERFIELD1,HLEDGERFIELD2,...
MATCHERA,VALUE1,VALUE2,...
MATCHERB,VALUE1,VALUE2,...
MATCHERC,VALUE1,VALUE2,...
<empty line>
```

The first character after `if` is taken to be the separator for the rest of the table.
It should be a non-alphanumeric character like `,` or `|` that does not appear anywhere else in the table.
(Note: it is unrelated to the CSV file's separator.)
Whitespace can be used in the matcher lines for readability, but not in the if line currently.
The table must be terminated by an empty line (or end of file).
Each line must contain the same number of separators; empty values are allowed.

The above means: try all of the matchers; whenever a matcher succeeds, 
assign all of the values on that line to the corresponding hledger fields;
later lines can overrider earlier ones.
It is equivalent to this sequence of if blocks:

```rules
if MATCHERA
  HLEDGERFIELD1 VALUE1
  HLEDGERFIELD2 VALUE2
  ...

if MATCHERB
  HLEDGERFIELD1 VALUE1
  HLEDGERFIELD2 VALUE2
  ...

if MATCHERC
  HLEDGERFIELD1 VALUE1
  HLEDGERFIELD2 VALUE2
  ...
```

Example:
```rules
if,account2,comment
atm transaction fee,expenses:business:banking,deductible? check it
%description groceries,expenses:groceries,
2020/01/12.*Plumbing LLC,expenses:house:upkeep,emergency plumbing call-out
```

## `balance-type`

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

## `include`

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

## Working with CSV

Some tips:

### Rapid feedback

It's a good idea to get rapid feedback while creating/troubleshooting CSV rules.
Here's a good way, using entr from [eradman.com/entrproject](https://eradman.com/entrproject):
```shell
$ ls foo.csv* | entr bash -c 'echo ----; hledger -f foo.csv print desc:SOMEDESC'
```
A desc: query (eg) is used to select just one, or a few, transactions of interest.
"bash -c" is used to run multiple commands, so we can echo a separator each time
the command re-runs, making it easier to read the output.

### Valid CSV

Note that hledger will only accept valid CSV conforming to [RFC 4180](https://tools.ietf.org/html/rfc4180),
and equivalent SSV and TSV formats (like RFC 4180 but with semicolon or tab as separators).
This means, eg:

- Values may be enclosed in double quotes, or not. Enclosing in single quotes is not allowed. (Eg `'A','B'` is rejected.)
- When values are enclosed in double quotes, spaces outside the quotes are
  [not allowed](https://stackoverflow.com/questions/4863852/space-before-quote-in-csv-field). (Eg `"A", "B"` is rejected.)
- When values are not enclosed in quotes, they may not contain double quotes. (Eg `A"A, B` is rejected.)

If your CSV/SSV/TSV is not valid in this sense, you'll need to transform it before reading with hledger.
Try using sed, or a more permissive CSV parser like [python's csv lib](https://docs.python.org/3/library/csv.html).

### File Extension

To help hledger choose the CSV file reader and show the right error messages
(and choose the right field separator character by default),
it's best if CSV/SSV/TSV files are named with a `.csv`, `.ssv` or `.tsv`
filename extension. 
(More about this at [Data formats](#data-formats).)

When reading files with the "wrong" extension, you can ensure the CSV reader
(and the default field separator) by prefixing the file path with `csv:`, `ssv:` or `tsv:`:
Eg:
```shell
$ hledger -f ssv:foo.dat print
```

You can also override the default field separator with a [separator](#separator) rule if needed.

### Reading CSV from standard input

You'll need the file format prefix when reading CSV from stdin also, since hledger assumes journal format by default.
Eg:

```
$ cat foo.dat | hledger -f ssv:- print
```

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

- <https://hledger.org/cookbook.html#setups-and-workflows>
- <https://plaintextaccounting.org> -> data import/conversion

### Setting amounts

Continuing from [amount field](#amount-field) above, here are more tips on handling
various amount-setting situations:

1. **If the amount is in a single CSV field:**\

   a. **If its sign indicates direction of flow:**\
   Assign it to `amountN`, to set the Nth posting's amount.
   N is usually 1 or 2 but can go up to 99.

   b. **If another field indicates direction of flow:**\
   Use one or more conditional rules to set the appropriate amount sign. Eg:
   ```rules
   # assume a withdrawal unless Type contains "deposit":
   amount1  -%Amount
   if %Type deposit
     amount1  %Amount
   ```

2. **If the amount is in one of two CSV fields (eg Debit and Credit):**\

   a. **If both fields are unsigned:**\
     Assign the fields to `amountN-in` and `amountN-out`.
     This sets posting N's amount to whichever of these has a non-zero value.
     If it's the -out value, the amount will be negated.

   b. **If either field is signed:**\
     Use a [conditional rule](#if-block) to flip the sign when needed.
     Eg below, the -out value already has a minus sign so we undo hledger's automatic
	 negating by [negating once more](#amount-signs) 
	 (but only if the field is non-empty, so that we don't leave a minus sign by itself):

     ```rules
     fields date, description, amount1-in, amount1-out
     if %amount1-out [1-9]
      amount1-out -%amount1-out
     ```
 
   c. **If both fields can contain a non-zero value (or both can be empty):**\
     The -in/-out rules normally choose the value which is non-zero/non-empty.
     Some value pairs can be ambiguous, such as `1` and `none`.
     For such cases, use [conditional rules](#if-block) to help select the amount.
     Eg, to handle the above you could select the value containing non-zero digits:
     ```rules
     fields date, description, in, out
     if %in [1-9]
      amount1 %in
     if %out [1-9]
      amount1 %out
     ```

3. **If you want posting 2's amount converted to cost:**\
   Use the unnumbered `amount` (or `amount-in` and `amount-out`) syntax.

4. **If the CSV has only balance amounts, not transaction amounts:**\
   Assign to `balanceN`, to set a [balance assignment](#balance-assignments) on the Nth posting,
   causing the posting's amount to be calculated automatically.
   `balance` with no number is equivalent to `balance1`.
   In this situation hledger is more likely to guess the wrong default account name,
   so you may need to set that explicitly.

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
  (and interpolate the %CSVFIELD references), or a default
- generate a hledger transaction (journal entry) from these values.

This is all part of the CSV reader, one of several readers hledger can
use to parse input files. When all files have been read successfully,
the transactions are passed as input to whichever hledger command the
user specified.


<a name="timeclock-format"></a>

### Well factored rules

Some things than can help reduce duplication and complexity in rules files:

- Extracting common rules usable with multiple CSV files into a `common.rules`, and adding `include common.rules` to each CSV's rules file.

- Splitting if blocks into smaller if blocks, extracting the frequently used parts.

## CSV rules examples

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

### Coinbase

A simple example with some CSV from Coinbase. The spot price is recorded using cost notation. 
The legacy `amount` field name conveniently sets amount 2 (posting 2's amount) to the total cost.
```csv
# Timestamp,Transaction Type,Asset,Quantity Transacted,Spot Price Currency,Spot Price at Transaction,Subtotal,Total (inclusive of fees and/or spread),Fees and/or Spread,Notes
# 2021-12-30T06:57:59Z,Receive,USDC,100,GBP,0.740000,"","","","Received 100.00 USDC from an external account"
```
```rules
# coinbase.csv.rules
skip         1
fields       Timestamp,Transaction_Type,Asset,Quantity_Transacted,Spot_Price_Currency,Spot_Price_at_Transaction,Subtotal,Total,Fees_Spread,Notes
date         %Timestamp
date-format  %Y-%m-%dT%T%Z
description  %Notes
account1     assets:coinbase:cc
amount       %Quantity_Transacted %Asset @ %Spot_Price_at_Transaction %Spot_Price_Currency
```
```shell
$ hledger print -f coinbase.csv
2021-12-30 Received 100.00 USDC from an external account
    assets:coinbase:cc    100 USDC @ 0.740000 GBP
    income:unknown                 -74.000000 GBP
```

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

# Timeclock

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
Lines beginning with `#` or `;` or `*`, and blank lines, are ignored.

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


<a name="timedot-format"></a>

# Timedot


`timedot` format is hledger's human-friendly time logging format.
Compared to [`timeclock` format](#timeclock), it is

- convenient for quick, approximate, and retroactive time logging
- readable: you can see at a glance where time was spent.

A timedot file contains a series of day entries, which might look like this:

```timedot
2021-08-04
hom:errands          .... ....
fos:hledger:timedot  ..         ; docs
per:admin:finance    
```

hledger reads this as three time transactions on this day,
with each dot representing a quarter-hour spent:

```shell
$ hledger -f a.timedot print   # .timedot file extension activates the timedot reader
2021-08-04 *
    (hom:errands)            2.00

2021-08-04 *
    (fos:hledger:timedot)    0.50

2021-08-04 *
    (per:admin:finance)      0
```

A day entry begins with a date line:

- a non-indented **[simple date](#simple-dates)** (Y-M-D, Y/M/D, or Y.M.D).

Optionally this can be followed on the same line by

- a common **transaction description** for this day
- a common **transaction comment** for this day, after a semicolon (`;`).

After the date line are zero or more optionally-indented 
time transaction lines, consisting of:

- an **account name** - any word or phrase, usually a 
  hledger-style [account name](#account-names).
- **two or more spaces** - a field separator, 
  required if there is an amount (as in journal format).
- a **timedot amount** - dots representing quarter hours, 
  or a number representing hours.
- an optional **comment** beginning with semicolon. This is ignored.

In more detail, timedot amounts can be:

- **dots**: zero or more period characters, each representing one quarter-hour.
  Spaces are ignored and can be used for grouping.
  Eg: `.... ..`

- a **number**, representing hours. Eg: `1.5`

- a **number immediately followed by a unit symbol**
  `s`, `m`, `h`, `d`, `w`, `mo`, or `y`, 
  representing seconds, minutes, hours, days weeks, months or years.
  Eg `1.5h` or `90m`.
  The following equivalencies are assumed:\
  `60s` = `1m`,
  `60m` = `1h`,
  `24h` = `1d`,
  `7d` = `1w`,
  `30d` = `1mo`,
  `365d` = `1y`.
  (This unit will not be visible in the generated transaction amount, which is always in hours.)

There is some added flexibility to help with keeping time log data
in the same file as your notes, todo lists, etc.:

- Blank lines and lines beginning with `#` or `;` are ignored.

- Before the first date line, lines beginning with `*` are ignored.
  From the first date line onward, a sequence of `*`'s followed by a space
  at beginning of lines (ie, the headline prefix used by Emacs Org mode) is ignored.
  This means the time log can be kept under an Org headline,
  and date lines or time transaction lines can be Org headlines.

- Lines not ending with a double-space and amount are 
  parsed as transactions with zero amount.
  (Most hledger reports hide these by default; add -E to see them.)

More examples:

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
$ hledger -f a.timedot print date:2016/2/2
2016-02-02 *
    (inc:client1)          2.00

2016-02-02 *
    (biz:research)          0.25
```
```shell
$ hledger -f a.timedot bal --daily --tree
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


Using period instead of colon as account name separator:

```timedot
2016/2/4
fos.hledger.timedot  4
fos.ledger           ..
```
```shell
$ hledger -f a.timedot --alias /\\./=: bal --tree
                4.50  fos
                4.00    hledger:timedot
                0.50    ledger
--------------------
                4.50
```

A
[sample.timedot](https://raw.github.com/simonmichael/hledger/master/examples/sample.timedot)
file.
<!-- to download and some queries to try: -->

<!-- ```shell -->
<!-- $ hledger -f sample.timedot balance                               # current time balances -->
<!-- $ hledger -f sample.timedot register -p 2009/3                    # sessions in march 2009 -->
<!-- $ hledger -f sample.timedot register -p weekly --depth 1 --empty  # time summary by week -->
<!-- ``` -->


# PART 3: REPORTING CONCEPTS

# Time periods

<a name="report-period"></a>

## Report start & end date

By default, most hledger reports will show the full span of time represented by the journal data.
The report start date will be the earliest transaction or posting date,
and the report end date will be the latest transaction, posting, or market price date.

Often you will want to see a shorter time span, such as the current month.
You can specify a start and/or end date using
[`-b/--begin`](#reporting-options),
[`-e/--end`](#reporting-options),
[`-p/--period`](#period-expressions)
or a [`date:` query](#queries) (described below).
All of these accept the [smart date](#smart-dates) syntax (below).

Some notes:

- End dates are exclusive, as in Ledger, so you should write the date *after*
  the last day you want to see in the report.
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
| `in n days/weeks/months/quarters/years`      | n periods from the current period                                                     |
| `n days/weeks/months/quarters/years ahead`   | n periods from the current period                                                     |
| `n days/weeks/months/quarters/years ago`     | -n periods from the current period                                                    |
| `20181201`                                   | 8 digit YYYYMMDD with valid year month and day                                        |
| `201812`                                     | 6 digit YYYYMM with valid year and month                                              |

Counterexamples - malformed digit sequences might give surprising results:

|             |                                                                   |
|-------------|-------------------------------------------------------------------|
| `201813`    | 6 digits with an invalid month is parsed as start of 6-digit year |
| `20181301`  | 8 digits with an invalid month is parsed as start of 8-digit year |
| `20181232`  | 8 digits with an invalid day gives an error                       |
| `201801012` | 9+ digits beginning with a valid YYYYMMDD gives an error          |

Note "today's date" can be overridden with the `--today` option, in case it's
needed for testing or for recreating old reports. (Except for periodic
transaction rules; those are not affected by `--today`.)

## Report intervals

A report interval can be specified so that commands like
[register](#register), [balance](#balance) and [activity](#activity)
become multi-period, showing each subperiod as a separate row or
column.

The following "standard" report intervals can be enabled by using
their corresponding flag:

- `-D/--daily`
- `-W/--weekly`
- `-M/--monthly`
- `-Q/--quarterly`
- `-Y/--yearly`

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

To summarise:

- In multiperiod reports, all subperiods are forced to be the same length, to simplify reporting.
- Reports with the standard `--weekly`/`--monthly`/`--quarterly`/`--yearly`  intervals
  are required to start on the first day of a week/month/quarter/year. 
  We'd like more flexibility here but it isn't supported yet.
- `--period` (below) can specify more complex intervals, starting on any date.

## Period expressions

The `-p/--period` option accepts period expressions, a shorthand way
of expressing a start date, end date, and/or report interval all at
once.

Here's a basic period expression specifying the first quarter of 2009. Note,
hledger always treats start dates as inclusive and end dates as exclusive:

|                                  |
|----------------------------------|
| `-p "from 2009/1/1 to 2009/4/1"` |

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
| `-p "since 2009/1"`  | the same, since is a synonym      |
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

### Period expressions with a report interval

`-p/--period`'s argument can also begin with, or entirely consist of, 
a [report interval](#report-intervals). 
This should be separated from the start/end dates (if any) by a space, or the word `in`.
The basic intervals (which can also be written as command line flags) 
are `daily`, `weekly`, `monthly`, `quarterly`, and `yearly`.
Some examples:

|                                         |
|-----------------------------------------|
| `-p "weekly from 2009/1/1 to 2009/4/1"` |
| `-p "monthly in 2008"`                  |
| `-p "quarterly"`                        |

As mentioned above, the `weekly`, `monthly`, `quarterly` and `yearly` intervals 
require a report start date that is the first day of a week, month, quarter or year.
And, report start/end dates will be expanded if needed to span a whole number of intervals.

For example:

|                                                |                                                                                    |
|------------------------------------------------|------------------------------------------------------------------------------------|
| `-p "weekly from 2009/1/1 to 2009/4/1"`        | starts on 2008/12/29, closest preceding Monday                                     |
| `-p "monthly in 2008/11/25"`                   | starts on 2018/11/01                                                               |
| `-p "quarterly from 2009-05-05 to 2009-06-01"` | starts on 2009/04/01, ends on 2009/06/30, which are first and last days of Q2 2009 |
| `-p "yearly from 2009-12-29"`                  | starts on 2009/01/01, first day of 2009                                            |

### More complex report intervals

Some more complex kinds of interval are also supported in period expressions:

- `biweekly`
- `fortnightly`
- `bimonthly`
- `every day|week|month|quarter|year`
- `every N days|weeks|months|quarters|years`

These too will cause report start/end dates to be expanded, if needed,
to span a whole number of intervals.
Examples:

|                                    |                                                             |
|------------------------------------|-------------------------------------------------------------|
| `-p "bimonthly from 2008"`         | periods will have boundaries on 2008/01/01, 2008/03/01, ... |
| `-p "every 2 weeks"`               | starts on closest preceding Monday                          |
| `-p "every 5 months from 2009/03"` | periods will have boundaries on 2009/03/01, 2009/08/01, ... |

### Intervals with custom start date

All intervals mentioned above are required to start on their natural calendar boundaries,
but the following intervals can start on any date:

Weekly on custom day:

- `every Nth day of week` (`th`, `nd`, `rd`, or `st` are all accepted after the number)
- `every WEEKDAYNAME` (full or three-letter english weekday name, case insensitive)

Monthly on custom day:

- `every Nth day [of month]`
- `every Nth WEEKDAYNAME [of month]`

Yearly on custom day:

- `every MM/DD [of year]` (month number and day of month number)
- `every MONTHNAME DDth [of year]` (full or three-letter english month name, case insensitive, and day of month number)
- `every DDth MONTHNAME [of year]` (equivalent to the above)

Examples:

|                              |                                                          |
|------------------------------|----------------------------------------------------------|
| `-p "every 2nd day of week"` | periods will go from Tue to Tue                          |
| `-p "every Tue"`             | same                                                     |
| `-p "every 15th day"`        | period boundaries will be on 15th of each month          |
| `-p "every 2nd Monday"`      | period boundaries will be on second Monday of each month |
| `-p "every 11/05"`           | yearly periods with boundaries on 5th of November        |
| `-p "every 5th November"`    | same                                                     |
| `-p "every Nov 5th"`         | same                                                     |

Show historical balances at end of the 15th day of each month (N is an end date, exclusive as always):

```shell
$ hledger balance -H -p "every 16th day"
```

Group postings from the start of wednesday to end of the following tuesday (N is both (inclusive) start date and (exclusive) end date):

```shell
$ hledger register checking -p "every 3rd day of week"
```

### Periods or dates ?

Report intervals like the above are most often used with `-p|--period`,
to divide reports into multiple subperiods -
each generated date marks a subperiod boundary.
Here, the periods between the dates are what's important.

But report intervals can also be used 
with `--forecast` to generate future transactions,
or with `balance --budget` to generate budget goal-setting transactions.
For these, the dates themselves are what matters.

### Events on multiple weekdays

The `every WEEKDAYNAME` form has a special variant with multiple day names, comma-separated. 
Eg: `every mon,thu,sat`.
Also, `weekday` and `weekendday` are shorthand for `mon,tue,wed,thu,fri` and `sat,sun` 
respectively.

This form is mainly intended for use with `--forecast`, to generate 
[periodic transactions](#periodic-transactions) on arbitrary days of the week.
It may be less useful with `-p`, since it divides each week into subperiods 
of unequal length. (Because gaps between periods are not allowed;
if you'd like to change this, see [#1632](https://github.com/simonmichael/hledger/pull/1632).)

Examples:

|                              |                                                                                        |
|------------------------------|----------------------------------------------------------------------------------------|
| `-p "every mon,wed,fri"`     | dates will be Mon, Wed, Fri; <br>periods will be Mon-Tue, Wed-Thu, Fri-Sun             |
| `-p "every weekday"`         | dates will be Mon, Tue, Wed, Thu, Fri; <br>periods will be Mon, Tue, Wed, Thu, Fri-Sun |
| `-p "every weekendday"`      | dates will be Sat, Sun; <br>periods will be Sat, Sun-Fri                               |

# Depth

With the `--depth NUM` option (short form: `-NUM`), 
reports will show accounts only to the specified depth, hiding deeper subaccounts.
Use this when you want a summary with less detail.
This flag has the same effect as a `depth:` query argument: `depth:2`, `--depth=2` or `-2` are equivalent.

# Queries

One of hledger's strengths is being able to quickly report on a precise subset of your data. 
Most hledger commands accept optional query arguments to restrict their scope.
The syntax is as follows:

- Zero or more space-separated query terms. 
These are most often [account name](#account-names) substrings:

  `utilities food:groceries`

- Terms with spaces or other [special characters](#special-characters) should be enclosed in quotes:

  `"personal care"`

- [Regular expressions](#regular-expressions) are also supported:

  `"^expenses\b" "accounts (payable|receivable)"`

- Add a query type prefix to match other parts of the data:

  `date:202012- desc:amazon cur:USD amt:">100" status:`

- Add a `not:` prefix to negate a term:

  `not:cur:USD`

## Query types

Here are the types of query term available.
Remember these can also be prefixed with **`not:`** to convert them into a negative match.

**`acct:REGEX`, `REGEX`**\
Match account names containing this (case insensitive) [regular expression]. 
This is the default query type when there is no prefix,
and regular expression syntax is typically not needed,
so usually we just write an account name substring, like `expenses` or `food`.

**`amt:N, amt:<N, amt:<=N, amt:>N, amt:>=N`**\
Match postings with a single-commodity amount equal to, less than, or greater than N.
(Postings with multi-commodity amounts are not tested and will always match.)
The comparison has two modes: 
if N is preceded by a + or - sign (or is 0), the two signed numbers are compared. 
Otherwise, the absolute magnitudes are compared, ignoring sign.

**`code:REGEX`**\
Match by transaction code (eg check number).

**`cur:REGEX`**\
Match postings or transactions including any amounts whose
currency/commodity symbol is fully matched by REGEX. (For a partial
match, use `.*REGEX.*`). 
Note, to match [special characters](#special-characters) which are regex-significant, you need to escape them with `\`.
And for characters which are significant to your shell you may need one more level of escaping. 
So eg to match the dollar sign:\
`hledger print cur:\\$`.

**`desc:REGEX`**\
Match transaction descriptions.

**`date:PERIODEXPR`**\
Match dates (or with the `--date2` flag, [secondary dates](#secondary-dates))
within the specified period.
PERIODEXPR is a [period expression](#period-expressions) with no report interval.
Examples:\
`date:2016`, `date:thismonth`, `date:2/1-2/15`, `date:2021-07-27..nextquarter`.


**`date2:PERIODEXPR`**\
Match secondary dates within the specified period (independent of the `--date2` flag).

**`depth:N`**\
Match (or display, depending on command) accounts at or above this depth.

**`note:REGEX`**\
Match transaction [notes](#payee-and-note)
(the part of the description right of `|`, or the whole description if there's no `|`).

**`payee:REGEX`**\
Match transaction [payee/payer names](#payee-and-note)
(the part of the description left of `|`, or the whole description if there's no `|`).

**`real:, real:0`**\
Match real or virtual postings respectively.

**`status:, status:!, status:*`**\
Match unmarked, pending, or cleared transactions respectively.

**`type:TYPECODES`**\
Match by account type (see [Declaring accounts > Account types](#account-types)).
`TYPECODES` is one or more of the single-letter account type codes
`ALERXCV`, case insensitive. 
Note `type:A` and `type:E` will also match their respective subtypes `C` (Cash) and `V` (Conversion).
Certain kinds of account alias can disrupt account types, see 
[Rewriting accounts > Aliases and account types](#aliases-and-account-types).

**`tag:REGEX[=REGEX]`**\
Match by tag name, and optionally also by tag value.
(To match only by value, use `tag:.=REGEX`.)

When querying by tag, note that:

- Accounts also inherit the tags of their parent accounts
- Postings also inherit the tags of their account and their transaction 
- Transactions also acquire the tags of their postings.

(**`inacct:ACCTNAME`**\
A special query term used automatically in hledger-web only:
tells hledger-web to show the transaction register for an account.)

## Combining query terms

When given multiple query terms, most commands select things which match:

- any of the description terms AND
- any of the account terms AND
- any of the status terms AND
- all the other terms.

The [print](#print) command is a little different, showing transactions which:

- match any of the description terms AND
- have any postings matching any of the positive account terms AND
- have no postings matching any of the negative account terms AND
- match all the other terms.

Although these fixed rules are enough for many needs,
we do not support full boolean expressions ([#203](https://github.com/simonmichael/hledger/issues/203)),
(and you should not write AND or OR in your queries).
This makes certain queries hard to express, but here are some tricks that can help:

1. Use a doubled `not:` prefix.
    Eg, to print only the food expenses paid with cash:

    ```shell
    $ hledger print food not:not:cash
    ```

2. Or pre-filter the transactions with `print`, 
   piping the result into a second hledger command
   (with balance assertions disabled):

    ```shell
    $ hledger print cash | hledger -f- -I balance food
    ```

## Queries and command options

Some queries can also be expressed as command-line options:
`depth:2` is equivalent to `--depth 2`, 
`date:2020` is equivalent to `-p 2020`, etc.
When you mix command options and query arguments, 
generally the resulting query is their intersection.

## Queries and account aliases

When account names are [rewritten](#account-aliases) with `--alias` or `alias`,
`acct:` will match either the old or the new account name.

## Queries and valuation

When amounts are converted to other commodities in [cost](#cost-reporting) or [value](#valuation) reports,
`cur:` and `amt:` match the old commodity symbol and the old amount quantity, 
not the new ones
(except in hledger 1.22.0 where it's reversed, see [#1625](https://github.com/simonmichael/hledger/issues/1625)).

## Querying with account aliases

When account names are [rewritten](#account-aliases) with `--alias` or `alias`,
note that `acct:` will match either the old or the new account name.

## Querying with cost or value

When amounts are converted to other commodities in [cost](#cost-reporting) or [value](#valuation) reports,
note that `cur:` matches the new commodity symbol, and not the old one,
and `amt:` matches the new quantity, and not the old one.
Note: this changed in hledger 1.22, previously it was the reverse, 
see the discussion at [#1625](https://github.com/simonmichael/hledger/issues/1625).

# Pivoting

Normally, hledger groups and sums amounts within each account.
The `--pivot FIELD` option substitutes some other transaction field for account names,
causing amounts to be grouped and summed by that field's value instead.
FIELD can be any of the transaction fields `status`, `code`, `description`, `payee`, `note`, or a tag name.
When pivoting on a tag and a posting has multiple values of that tag, only the first value is displayed.
Values containing `colon:separated:parts` will be displayed hierarchically, like account names.

Some examples:

```journal
2016/02/16 Yearly Dues Payment
    assets:bank account                 2 EUR
    income:dues                        -2 EUR  ; member: John Doe
```
Normal balance report showing account names:
```shell
$ hledger balance
               2 EUR  assets:bank account
              -2 EUR  income:dues
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
One way to show only amounts with a member: value (using a [query](#queries)):
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

# Generating data

Two features for generating transient data (visible only at report time)
are built in to hledger's journal format:

- [Auto posting](#auto-postings) rules can generate extra postings on certain transactions.
  They are activated by the `--auto` flag.

- [Periodic transaction](#periodic-transactions) rules can generate repeating transactions, 
  usually dated in the future, to help with forecasting or budgeting.
  They are activated by the `--forecast` or `balance --budget` options, described next.

# Forecasting

The `--forecast` flag activates any [periodic transaction rules](#periodic-transactions) 
in the journal. These will generate temporary additional transactions, 
usually recurring and in the future, which will appear in all reports.
`hledger print --forecast` is a good way to see them.

This can be useful for estimating balances into the future,
perhaps experimenting with different scenarios.

It could also be useful for scripted data entry: you could describe recurring
transactions, and every so often copy the output of `print --forecast`
into the journal.

The generated transactions will have an extra [tag](#tags), like
`generated-transaction:~ PERIODICEXPR`, 
indicating which periodic rule generated them.
There is also a similar, hidden tag, named `_generated-transaction:`, 
which you can use to reliably match transactions generated "just now"
(rather than `print`ed in the past).

The forecast transactions are generated within a *forecast period*,
which is independent of the [report period](#report-start--end-date).
(Forecast period sets the bounds for generated transactions,
report period controls which transactions are reported.)
The forecast period begins on:

- the start date provided within `--forecast`'s argument, if any
- otherwise, the later of
  - the report start date, if specified (with `-b`/`-p`/`date:`)
  - the day after the latest ordinary transaction in the journal, if any
- otherwise today.

It ends on:

- the end date provided within `--forecast`'s argument, if any
- otherwise, the report end date, if specified (with `-e`/`-p`/`date:`)
- otherwise 180 days (6 months) from today.

Note, this means that ordinary transactions will suppress periodic transactions,
by default; the periodic transactions will not start until after the last ordinary transaction.
This is usually convenient, but you can get around it in two ways:

- If you need to record some transactions in the future, make them 
  periodic transactions (with a single occurrence, eg: `~ YYYY-MM-DD`) rather than ordinary transactions.
  That way they won't suppress other periodic transactions.

- Or give `--forecast` a [period expression](#period-expressions) argument.
  A forecast period specified this way can overlap ordinary transactions, 
  and need not be in the future. Some things to note:

  - You must use `=` between flag and argument; a space won't work.
  - The period expression can specify the forecast period's start date, end date, or both.
    See also [Report start & end date](#report-start--end-date).
  - The period expression should not specify a [report interval](#report-intervals).
    (Each periodic transaction rule specifies its own interval.)

Some examples: `--forecast=202001-202004`, `--forecast=jan-`, `--forecast=2021`.

# Budgeting

With the balance command's [`--budget` report](#budget-report),
each periodic transaction rule generates recurring budget goals in specified accounts,
and goals and actual performance can be compared.
See the balance command's doc below.
 
See also: [Budgeting and Forecasting](/budgeting-and-forecasting.html).


# Cost reporting

This section is about recording the cost of things, in transactions
where one commodity is exchanged for another. 
Eg an exchange of currency, or a stock purchase or sale.
First, a quick glossary:

- Conversion - an exchange of one currency or commodity for another.
  Eg a foreign currency exchange, or a purchase or sale of stock or
  cryptocurrency.

- Conversion transaction - a transaction involving one or more conversions.

- Conversion rate - the cost per unit of one commodity in the other,
  ie the exchange rate.

- Cost - how much of one commodity was paid to acquire the other.  And
  more generally, in hledger docs: the amount exchanged in the
  "secondary" commodity (usually your base currency), whether in a
  purchase or a sale, and whether expressed per unit or in total. Also,
  the "@/@@ PRICE" notation used to represent this.

## -B: Convert to cost

As discussed in [JOURNAL > Costs](#costs),
when recording a transaction you can also record the amount's cost in another commodity,
by adding `@ UNITPRICE` or `@@ TOTALPRICE`.

Then you can see a report with amounts converted to cost, by adding
the [`-B/--cost`](#reporting-options) flag. (Mnemonic: "B" from "cost
Basis", as in Ledger).  Eg:

```journal
2022-01-01
  assets:dollars  $-135          ; 135 dollars is exchanged for..
  assets:euros     €100 @ $1.35  ; one hundred euros purchased at $1.35 each
```

```shell
$ hledger bal -N
               $-135  assets:dollars
                €100  assets:euros
$ hledger bal -N -B
               $-135  assets:dollars
                $135  assets:euros    # <- the euros' cost
```

Notes:

-B is sensitive to the order of postings when a cost is inferred:
the inferred price will be in the commodity of the last amount.
So if example 3's postings are reversed, while the transaction
is equivalent, -B shows something different:

```journal
2022-01-01
  assets:dollars  $-135              ; 135 dollars sold
  assets:euros     €100              ; for 100 euros
```
```shell
$ hledger bal -N -B
               €-100  assets:dollars  # <- the dollars' selling price
                €100  assets:euros
```

The @/@@ cost notation is convenient, but has some drawbacks: it does
not truly balance the transaction, so it disrupts the accounting
equation and tends to causes a non-zero total in balance reports.

## Equity conversion postings

By contrast, conventional double entry bookkeeping (DEB) uses a different notation:
an extra pair of equity postings to balance conversion transactions.
In this style, the above entry might be written:

```journal
2022-01-01 one hundred euros purchased at $1.35 each
    assets:dollars      $-135
    equity:conversion    $135
    equity:conversion   €-100
    assets:euros         €100
```

This style is more correct, but it's also more verbose and makes cost
reporting more difficult for PTA tools.

Happily, current hledger can read either notation, or convert one to
the other when needed, so you can use the one you prefer.

You can even use cost notation and equivalent conversion postings at the same time, for clarity.
hledger will ignore the redundancy.
But be sure the cost and conversion posting amounts match, or you'll see 
a not-so-clear transaction balancing error message.

## Inferring equity postings from cost

With `--infer-equity`, hledger detects transactions written with PTA cost notation
and adds equity conversion postings to them:

```journal
2022-01-01
  assets:dollars  -$135
  assets:euros     €100 @ $1.35
```
```shell
$ hledger print --infer-equity
2022-01-01
    assets:dollars                    $-135
    assets:euros               €100 @ $1.35
    equity:conversion:$-€:€           €-100  ; generated-posting:
    equity:conversion:$-€:$         $135.00  ; generated-posting:
```

The conversion account names can be changed with the
[conversion account type declaration](#account-types).

--infer-equity is useful when when transactions have been recorded
using cost notation, to help preserve the accounting equation 
and balance reports' zero total, or to produce more conventional 
journal entries for sharing with non-PTA-users.

## Inferring cost from equity postings

The reverse operation is possible using `--infer-costs`, which
detects transactions written with equity conversion postings
and adds cost notation to them:

```journal
2022-01-01
    assets:dollars            $-135
    equity:conversion          $135
    equity:conversion         €-100
    assets:euros               €100
```
```shell
$ hledger print --infer-costs
2022-01-01
    assets:dollars       $-135 @@ €100
    equity:conversion             $135
    equity:conversion            €-100
    assets:euros                  €100
```

--infer-costs is useful when combined with -B/--cost, 
allowing cost reporting even when transactions have been recorded 
using equity postings:

```shell
$ hledger print --infer-costs -B
2009-01-01
    assets:dollars           €-100
    assets:euros              €100
```

Notes: 

For `--infer-costs` to work, an exchange must consist of four postings:

1. two non-equity postings
2. two equity postings, next to one another
2. the equity accounts must be declared, with account type `V`/`Conversion`
   (or if they are not declared, they must be named
   `equity:conversion`, `equity:trade`, `equity:trading` or subaccounts of these)
3. the equity postings' amounts must exactly match the non-equity postings' amounts.

Multiple such exchanges can coexist within a single transaction.

When inferring cost, the order of postings matters:
the cost is added to the first of the non-equity postings involved in the exchange,
in the commodity of the last non-equity posting involved in the exchange.
If you don't want to write your postings in the required order,
you can use explicit cost notation instead.

--infer-equity and --infer-costs can be used together, if you have a
mixture of both notations in your journal.

## When to infer cost/equity

Inferring equity postings or costs is still fairly new, so not enabled by default.
We're not sure yet if that should change. Here are two suggestions to try,
experience reports welcome:

1. When you use -B, always use --infer-costs as well. Eg: `hledger bal -B --infer-costs`

2. Always run hledger with both flags enabled. Eg: `alias hl="hledger --infer-equity --infer-costs"`

## How to record conversions

Essentially there are four ways to record a conversion transaction in hledger.
Here are all of them, with pros and cons.

### Conversion with implicit cost

Let's assume 100 EUR is converted to 120 USD.  You can just record the
outflow (100 EUR) and inflow (120 USD) in the appropriate asset
account:

```journal
2021-01-01
    assets:cash    -100 EUR
    assets:cash     120 USD
```

hledger will assume this transaction is balanced, inferring that the
conversion rate must be 1 EUR = 1.20 USD. You can see the inferred
rate by using `hledger print -x`.

Pro: 

- Concise, easy

Con: 

- Less error checking - typos in amounts or commodity symbols may not be detected
- Conversion rate is not clear
- Disturbs the accounting equation, unless you add the --infer-equity flag

You can prevent accidental implicit conversions due to a mistyped
commodity symbol, by using `hledger check commodities`.

You can prevent implicit conversions entirely, by using `hledger check
balancednoautoconversion`, or `-s/--strict`.

### Conversion with explicit cost

You can add the conversion rate using @ notation:

```journal
2021-01-01
    assets:cash        -100 EUR @ 1.20 USD
    assets:cash         120 USD
```

Now hledger will check that 100 * 1.20 = 120, and would report an error otherwise.

Pro: 

- Still concise
- Makes the conversion rate clear
- Provides more error checking

Con: 

- Disturbs the accounting equation, unless you add the --infer-equity flag

### Conversion with equity postings

In strict double entry bookkeeping, the above transaction is not
balanced in EUR or in USD, since some EUR disappears, and some USD
appears. This violates the accounting equation (A+L+E=0), and prevents
reports like `balancesheetequity` from showing a zero total.

The proper way to make it balance is to add a balancing posting for
each commodity, using an equity account:

```journal
2021-01-01
    assets:cash        -100 EUR
    equity:conversion   100 EUR
    equity:conversion  -120 USD
    assets:cash         120 USD
```

Pro: 

- Preserves the accounting equation
- Keeps track of conversions and related gains/losses in one place
- Standard, works in any double entry accounting system

Con: 

- More verbose
- Conversion rate is not obvious
- Cost reporting requires adding the --infer-costs flag

### Conversion with equity postings and explicit cost

Here both equity postings and @ notation are used together.

```journal
2021-01-01
    assets:cash        -100 EUR @ 1.20 USD
    equity:conversion   100 EUR
    equity:conversion  -120 USD
    assets:cash         120 USD
```

Pro: 

- Preserves the accounting equation
- Keeps track of conversions and related gains/losses in one place
- Makes the conversion rate clear
- Provides more error checking

Con: 

- Most verbose
- Not compatible with ledger

## Cost tips

- Recording the cost/conversion rate explicitly is good because it makes that clear and helps detect errors.
- Recording equity postings is good because it is correct bookkeeping and preserves the accounting equation.
- Combining these is possible.
- When you want to see the cost (or sale proceeds) of things, use `-B` (short form of `--cost`).
- If you use conversion postings without cost notation, add `--infer-costs` also.
- If you use cost notation without conversion postings, and you want to see a balanced balance sheet or print correct journal entries, use `--infer-equity`.
- Conversion to cost is performed before valuation (described next).

# Valuation

Instead of reporting amounts in their original commodity, hledger can convert them to
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

## Finding market price

To convert a commodity A to its market value in another commodity B,
hledger looks for a suitable market price (exchange rate) as follows,
in this order of preference
<!-- (-X tries all of these; -V tries only 1) (really ?) -->
:

1. A *declared market price* or *inferred market price*:
   A's latest market price in B on or before the valuation date
   as declared by a [P directive](#market-prices), 
   or (with the `--infer-market-prices` flag)
   inferred from [costs](#costs).
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

## --infer-market-prices: market prices from transactions

Normally, market value in hledger is fully controlled by, and requires,
[P directives](#market-prices) in your journal.
Since adding and updating those can be a chore,
and since transactions usually take place at close to market value,
why not use the recorded [costs](#costs)
as additional market prices (as Ledger does) ?
Adding the `--infer-market-prices` flag to `-V`, `-X` or `--value` enables this.

So for example, `hledger bs -V --infer-market-prices` will get market
prices both from P directives and from transactions.
If both occur on the same day, the P directive takes precedence.

There is a downside: value reports can sometimes  be affected in
confusing/undesired ways by your journal entries. If this happens to
you, read all of this [Valuation](#valuation) section carefully,
and try adding `--debug` or `--debug=2` to troubleshoot.

`--infer-market-prices` can infer market prices from:

- multicommodity transactions with explicit prices (`@`/`@@`)

- multicommodity transactions with implicit prices (no `@`, two commodities, unbalanced).
  (With these, the order of postings matters. `hledger print -x` can be useful for troubleshooting.)

- [multicommodity transactions with equity postings](#conversion-with-equity-postings),
  if cost is inferred with [`--infer-costs`](#inferring-cost-from-equity-postings).
  
There is a limitation (bug) currently: when a valuation commodity is not specified, 
prices inferred with `--infer-market-prices` do not help select a default valuation commodity,
as `P` prices would.
So conversion might not happen because no valuation commodity was detected (`--debug=2` will show this). 
To be safe, specify the valuation commmodity, eg:

- `-X EUR --infer-market-prices`, not `-V --infer-market-prices`
- `--value=then,EUR --infer-market-prices`, not `--value=then --infer-market-prices`

Signed costs and market prices can be confusing.
For reference, here is the current behaviour, since hledger 1.25.
(If you think it should work differently, see [#1870](https://github.com/simonmichael/hledger/issues/1870).)

```journal
2022-01-01 Positive Unit prices
    a        A 1
    b        B -1 @ A 1

2022-01-01 Positive Total prices
    a        A 1
    b        B -1 @@ A 1


2022-01-02 Negative unit prices
    a        A 1
    b        B 1 @ A -1

2022-01-02 Negative total prices
    a        A 1
    b        B 1 @@ A -1


2022-01-03 Double Negative unit prices
    a        A -1
    b        B -1 @ A -1

2022-01-03 Double Negative total prices
    a        A -1
    b        B -1 @@ A -1
```

All of the transactions above are considered balanced (and on each day, the two transactions are considered equivalent).
Here are the market prices inferred for B:

```shell
$ hledger -f- --infer-market-prices prices
P 2022-01-01 B A 1
P 2022-01-01 B A 1.0
P 2022-01-02 B A -1
P 2022-01-02 B A -1.0
P 2022-01-03 B A -1
P 2022-01-03 B A -1.0
```

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
   `--infer-market-prices` flag is used: the price commodity from the latest
   transaction-inferred price for A on or before valuation date.

This means:

- If you have [P directives](#market-prices), 
  they determine which commodities `-V` will convert, and to what.

- If you have no P directives, and use the `--infer-market-prices` flag, 
  [costs](#costs) determine it.

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

## Interaction of valuation and queries

When matching postings based on queries in the presence of valuation, the
following happens.

1. The query is separated into two parts:
    1. the currency (`cur:`) or amount (`amt:`).
    2. all other parts.
2. The postings are matched to the currency and amount queries based on pre-valued amounts.
3. Valuation is applied to the postings.
4. The postings are matched to the other parts of the query based on post-valued amounts.

See:
[1625](https://github.com/simonmichael/hledger/issues/1625)


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
| starting balance (-H)                               | cost                                                             | value at report or journal end                                    | valued at day each historical posting was made                                                 | value at report or journal end                                    | value at DATE/today                     |
| starting balance (-H) with report interval          | cost                                                             | value at day before report or journal start                       | valued at day each historical posting was made                                                 | value at day before report or journal start                       | value at DATE/today                     |
| posting amounts                                     | cost                                                             | value at report or journal end                                    | value at posting date                                                                          | value at report or journal end                                    | value at DATE/today                     |
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


# PART 4: COMMANDS

Here are the built-in [commands](#commands), with the most often-used in bold:
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
- **[register](#register) (reg)**                  - show postings in one or more accounts & running total
- [stats](#stats)                                  - show journal statistics
- [tags](#tags)                                    - show tag names
- [test](#test)                                    - run self tests

<a name="addons"></a>
<!-- #addons: the short explanation and list of common add-on commands. See also #add-on-commands. -->

And here are some typical [add-on commands](#add-on-commands)
installed by the [hledger-install script](https://hledger.org/install.html#build-methods).
If installed, these will also appear in hledger's commands list, with a `+` mark:

- **[ui](hledger-ui.html)**   - hledger's official curses-style TUI
- **[web](hledger-web.html)** - hledger's official web UI
- [iadd](https://hackage.haskell.org/package/hledger-iadd)                - a popular alternative to hledger's `add` command.
- [interest](https://hackage.haskell.org/package/hledger-interest)        - generates interest transactions
- [stockquotes](https://hackage.haskell.org/package/hledger-stockquotes)  - downloads market prices. *(Alpha quality, needs your help.)*

m4_dnl XXX maybe later
m4_dnl _man_({{
m4_dnl For detailed command docs please see the appropriate man page (eg `man hledger-print`), 
m4_dnl or the info or web format of this manual.
m4_dnl }})
m4_dnl _notman_({{

Next, each command is described in detail, in alphabetical order.

m4_dnl cf Hledger/Cli/Commands/commands.m4:
_commands_({{##}})

<a name="common-tasks"></a>

# PART 5: COMMON TASKS

Here are some quick examples of how to do some basic tasks with hledger.

## Getting help

Here's how to list commands and view options and command docs:

```shell
$ hledger                # show available commands
$ hledger --help         # show common options
$ hledger CMD --help     # show CMD's options, common options and CMD's documentation
```

You can also view your hledger version's manual in several formats
by using the [help command](#help). Eg:
```shell
$ hledger help           # show the hledger manual with info, man or $PAGER (best available)
$ hledger help journal   # show the journal topic in the hledger manual
$ hledger help --help    # find out more about the help command
```

To view manuals and introductory docs on the web, visit <https://hledger.org>.
Chat and mail list support and discussion archives can be found at <https://hledger.org/support>.

## Constructing command lines

hledger has a flexible command line interface.
We strive to keep it simple and ergonomic, but if you run into one of
the sharp edges described in [OPTIONS](#options),
here are some tips that might help:

- command-specific options must go after the command (it's fine to put common options there too: `hledger CMD OPTS ARGS`)
- running add-on executables directly simplifies command line parsing (`hledger-ui OPTS ARGS`)
- enclose "problematic" args in single quotes
- if needed, also add a backslash to hide regular expression metacharacters from the shell
- to see how a misbehaving command line is being parsed, add `--debug=2`.

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
$ hledger bal assets liabilities -2
               $4000  assets:bank
                $105  assets:cash
                $-50  liabilities:creditcard
--------------------
               $4055
```

Show the same thing without negative numbers, formatted as a simple balance sheet:
```shell
$ hledger bs -2
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
[commands-list]:       #part-4-commands
[common tasks]:        #common-tasks
[csv]:                 #csv
[directives]:          #directives
[directive effects]:   #directive-effects
[incomestatement]:     #incomestatement
[journal]:             #journal
[period expressions]:  #period-expressions
[queries]:             #queries
[regular expression]:  #regular-expressions
[regular expressions]: #regular-expressions
[strict mode]:         #strict-mode
[timeclock]:           #timeclock
[timedot]:             #timedot
[costs]:  #costs
[valuation]:           #valuation

m4_dnl Tips for editing hledger .m4.md docs.
m4_dnl
m4_dnl  .m4.md are hledger docs source files processed with m4 to generate markdown.
m4_dnl  Lines beginning with m4_dnl are comments.
m4_dnl  Words enclosed in underscores are macros, defined in doc/common.m4.
m4_dnl  Macro arguments are enclosed in ().
m4_dnl  Literal text macro arguments are enclosed in {{}}.
m4_dnl  "{{foo}}" in docs can be written as "{{{{foo}}}}".
m4_dnl  Macros can depend on command line flags, configured in Shake.hs.
m4_dnl  Emacs markdown-mode can be helpful:
m4_dnl   S-TAB cycles visibility of all sections.
m4_dnl   TAB on a heading toggles that section.
m4_dnl   C-x n s on a heading narrows to that section, C-x n w widens again.
