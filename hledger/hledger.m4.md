% hledger(1)
% _author_
% _monthyear_

m4_dnl  Lines beginning with m4_dnl are comments. See help at end of file.
m4_dnl  The macro below includes the content only in the web format:
_notinfo_({{
# NAME
}})

hledger - a robust, friendly plain text accounting app (command line version).

_notinfo_({{
# SYNOPSIS
}})

`hledger`\
or\
`hledger COMMAND [OPTS] [ARGS]`\
or\
`hledger ADDONCMD [OPTS] -- [ADDONOPTS] [ADDONARGS]`

_notinfo_({{
# DESCRIPTION
}})

_hledgerdescription_

This manual is for hledger's command line interface, version _version_.
It also describes the common options, file formats and concepts used by all hledger programs. 
It might accidentally teach you some bookkeeping/accounting as well!
You don't need to know everything in here to use hledger productively,
but when you have a question about functionality, this doc should answer it.
It is detailed, so do skip ahead or skim when needed.
You can read it on hledger.org, or as an info manual or man page on your system.
You can also open a built-in copy, at a point of interest, by running\
`hledger --man [CMD]`, `hledger --info [CMD]` or `hledger help [TOPIC]`.

(And for shorter help, try `hledger --tldr [CMD]`.)

The main function of the hledger CLI is
to read plain text files describing financial transactions,
crunch the numbers,
and print a useful report on the terminal (or save it as HTML, CSV, JSON or SQL).
Many reports are available, as subcommands.
hledger will also detect other `hledger-*` executables as extra subcommands.

hledger usually _inputfiles_

Here is a small journal file describing one transaction:

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
then try commands like:
```cli
$ hledger print -x
$ hledger aregister assets
$ hledger balance
$ hledger balancesheet
$ hledger incomestatement
```
Run `hledger` to list the commands.
See also the "Starting a journal file" and "Setting opening balances" sections
in [PART 5: COMMON TASKS](#part-5-common-tasks).

# PART 1: USER INTERFACE

# Input

hledger reads one or more data files, each time you run it. 
You can specify a file with `-f`, like so
```cli
$ hledger -f FILE print
```

Files are most often in hledger's journal format, with the `.journal` file extension (`.hledger` or `.j` also work);
these files describe transactions, like an accounting general journal.

When no file is specified, hledger looks for `.hledger.journal` in your home directory.

But most people prefer to keep financial files in a dedicated folder, perhaps with version control.
Also, starting a new journal file each year is common (it's not required, but helps keep things fast and organised).
So we usually configure a different journal file, by setting the `LEDGER_FILE` environment variable,
to something like `~/finance/2023.journal`.
For more about how to do that on your system, see [Common tasks > Setting LEDGER_FILE](#setting-ledger_file).

## Text encoding

Data files containing non-ascii characters must use UTF-8 encoding.
An optional [byte order mark (BOM)](https://www.unicode.org/faq/utf_bom.html#BOM) is allowed, at the beginning of the file (only).

Also, your system should be configured with a locale that can decode UTF-8 text.
On some unix systems, you may need set the `LANG` environment variable, eg.
You can read more about this in [Unicode characters](#unicode-characters), below.

On unix systems you can check a file's encoding with the `file` command.
If you need to import from a UTF-16-encoded CSV file, say,
you can convert it to UTF-8 with the `iconv` command. 

## Data formats

Usually the data file is in hledger's journal format, but it can be in any of the supported file formats, which currently are:

| Reader:                   | Reads:                                                           | Automatically used for files with extensions: |
|---------------------------|------------------------------------------------------------------|-----------------------------------------------|
| [`journal`](#journal)     | hledger journal files and some Ledger journals, for transactions | `.journal` `.j` `.hledger` `.ledger`          |
| [`timeclock`](#timeclock) | timeclock files, for precise time logging                        | `.timeclock`                                  |
| [`timedot`](#timedot)     | timedot files, for approximate time logging                      | `.timedot`                                    |
| [`csv`](#csv)             | Comma or other character separated values, for data import       | `.csv`                                        |
| [`ssv`](#csv)             | Semicolon separated values                                       | `.ssv`                                        |
| [`tsv`](#csv)             | Tab separated values                                             | `.tsv`                                        |
| [`rules`](#csv)           | CSV/SSV/TSV/other separated values, alternate way                | `.rules`                                      |

These formats are described in more detail below.

hledger detects the format automatically based on the file extensions shown above.
If it can't recognise the file extension, it assumes `journal` format.
So for non-journal files, it's important to use a recognised file extension,
so as to either read successfully or to show relevant error messages.

You can also force a specific reader/format by prefixing the file path with the format and a colon.
Eg, to read a .dat file containing tab separated values:

```cli
$ hledger -f tsv:/some/file.dat stats
```

## Standard input

The file name `-` means standard input:

```cli
$ cat FILE | hledger -f- print
```

If reading non-journal data in this way, you'll need to write the format as a prefix,
like `timeclock:` here:

```cli
$ echo 'i 2009/13/1 08:00:00' | hledger print -f timeclock:-
```

## Multiple files

You can specify multiple `-f` options, to read multiple files as one big journal. When doing this, note that certain features (described below) will be affected:

- [Balance assertions](#balance-assertions) will not see the effect of transactions in previous files. (Usually this doesn't matter as each file will set the corresponding opening balances.)
- Some [directives](#directives) will not affect previous or subsequent files.

If needed, you can work around these by using a single parent file which [includes](#include-directive) the others, or concatenating the files into one, eg: `cat a.journal b.journal | hledger -f- CMD`.

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

hledger provides various subcommands for getting things done.
Most of these commands do not change the journal file; they just read it and output a report.
A few commands assist with adding data and file management.

To show the commands list, run `hledger` with no arguments.
The commands are described in detail in [PART 4: COMMANDS](#part-4-commands), below.

To use a particular command, run `hledger CMD [CMDOPTS] [CMDARGS]`,

- CMD is the full command name,
or its standard abbreviation shown in the commands list,
or any unambiguous prefix of the name.

- CMDOPTS are command-specific options, if any.
Command-specific options must be written after the command name.
Eg: `hledger print -x`.

- CMDARGS are additional arguments to the command, if any.
Most hledger commands accept arguments representing a [query](#queries), to limit the data in some way.
Eg: `hledger reg assets:checking`.

To list a command's options, arguments, and documentation in the terminal, run `hledger CMD -h`.
Eg: `hledger bal -h`.

## Add-on commands

In addition to the built-in commands, you can install *add-on commands*:
programs or scripts named "hledger-SOMETHING", which will also appear in hledger's commands list.
If you used the [hledger-install script](https://hledger.org/install.html#build-methods),
you will have several add-ons installed already.
Some more can be found in hledger's bin/ directory, documented at <https://hledger.org/scripts.html>.

More precisely, add-on commands are programs or scripts in your shell's PATH,
whose name starts with "hledger-"
and ends with no extension or a recognised extension
(".bat", ".com", ".exe", ".hs", ".js", ".lhs", ".lua", ".php", ".pl", ".py", ".rb", ".rkt", or ".sh"),
and (on unix and mac) which has executable permission for the current user.

m4_dnl Addons can be written in any language, but Haskell scripts or programs can 
m4_dnl call hledger's code directly, which means they can do anything built-in commands can.
m4_dnl Scripts/programs in other languages can't do this, but they can use hledger's
m4_dnl command-line interface, or output formats like CSV or JSON.

You can run add-on commands using hledger, much like built-in commands:
`hledger ADDONCMD [-- ADDONCMDOPTS] [ADDONCMDARGS]`.
But note the double hyphen argument, required before add-on-specific options.
Eg: `hledger ui -- --watch` or `hledger web -- --serve`. 
If this causes difficulty, you can always run the add-on directly, without using `hledger`:
`hledger-ui --watch` or `hledger-web --serve`.

# Options

Run `hledger -h` to see general command line help.
Options can be written either before or after the command name.
These options are specific to the `hledger` CLI:

```
Flags:
     --conf=CONFFILE        Use extra options defined in this config file. If
                            not specified, searches upward and in XDG config
                            dir for hledger.conf (or .hledger.conf in $HOME).
  -n --no-conf              ignore any config file
```

And the following general options are common to most hledger commands:

_generaloptions_

Usually hledger accepts any unambiguous flag prefix,
eg you can write `--tl` instead of `--tldr` or `--dry` instead of `--dry-run`.

If the same option appears more than once in a command, usually the last (right-most) wins.

With most commands, arguments are interpreted as a hledger [query](hledger.md#queries) which filter the data.
Some queries can be expressed either with options or with arguments.

Below are more tips for using the command line interface -
feel free to skip these until you need them.

## Special characters

### Single escaping (shell metacharacters)

In shell command lines, characters significant to your shell - such as
spaces, `<`, `>`, `(`, `)`, `|`, `$` and `\` - should be
"shell-escaped" if you want hledger to see them. This is done by
enclosing them in single or double quotes, or by writing a backslash
before them. Eg to match an account name containing a space:

```cli
$ hledger register 'credit card'
```

or:

```cli
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

```cli
$ hledger balance cur:'\$'
```

or:

```cli
$ hledger balance cur:\\$
```

### Triple escaping (for add-on commands)

When you use hledger to run an external add-on command (described
below), one level of shell-escaping is lost from any options or
arguments intended for by the add-on command, so those need an extra
level of shell-escaping. Eg to match a literal `$` sign while using
the bash shell and running an add-on command (`ui`):

```cli
$ hledger ui cur:'\\$'
```

or:

```cli
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

```cli
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

- Your terminal software (eg Terminal.app, iTerm, CMD.exe, xterm..)  must support unicode.
  On Windows, you may need to use Windows Terminal and/or enable UTF-8 support.

- The terminal must be using a font which includes the required unicode glyphs.

- The terminal should be configured to display wide characters as double width (for report alignment).

- On Windows, for best results you should run hledger in the same kind of environment in which it was built.
  Eg hledger built in the standard CMD.EXE environment (like the binaries on our download page)
  might show display problems when run in a cygwin or msys terminal, and vice versa.
  (See eg [#961](https://github.com/simonmichael/hledger/issues/961#issuecomment-471229644)).

## Regular expressions

A [regular expression](https://en.wikipedia.org/wiki/regular_expression) (regexp)
is a small piece of text where certain characters
(like `.`, `^`, `$`, `+`, `*`, `()`, `|`, `[]`, `\`) have special meanings,
forming a tiny language for matching text precisely - very useful in hledger and elsewhere. 
To learn all about them, visit [regular-expressions.info](https://www.regular-expressions.info).

hledger supports regexps whenever you are entering a pattern to match something, eg in
[query arguments](#queries), 
[account aliases](#alias-directive),
[CSV if rules](#if-block),
hledger-web's search form,
hledger-ui's `/` search,
etc.
You may need to wrap them in quotes, especially at the command line (see [Special characters](#special-characters) above).
Here are some examples:

Account name queries (quoted for command line use):
```
Regular expression:  Matches:
-------------------  ------------------------------------------------------------
bank                 assets:bank, assets:bank:savings, expenses:art:banksy, ...
:bank                assets:bank:savings, expenses:art:banksy
:bank:               assets:bank:savings
'^bank'              none of those ( ^ matches beginning of text )
'bank$'              assets:bank   ( $ matches end of text )
'big \$ bank'        big $ bank    ( \ disables following character's special meaning )
'\bbank\b'           assets:bank, assets:bank:savings  ( \b matches word boundaries )
'(sav|check)ing'     saving or checking  ( (|) matches either alternative )
'saving|checking'    saving or checking  ( outer parentheses are not needed )
'savings?'           saving or savings   ( ? matches 0 or 1 of the preceding thing )
'my +bank'           my bank, my  bank, ... ( + matches 1 or more of the preceding thing )
'my *bank'           mybank, my bank, my  bank, ... ( * matches 0 or more of the preceding thing )
'b.nk'               bank, bonk, b nk, ... ( . matches any character )
```

Some other queries:
```
desc:'amazon|amzn|audible'  Amazon transactions
cur:EUR              amounts with commodity symbol containing EUR
cur:'\$'             amounts with commodity symbol containing $
cur:'^\$$'           only $ amounts, not eg AU$ or CA$
cur:....?            amounts with 4-or-more-character symbols
tag:.=202[1-3]       things with any tag whose value contains 2021, 2022 or 2023
```

Account name aliases: accept `.` instead of `:` as account separator:
```
alias /\./=:         replaces all periods in account names with colons
```

Show multiple top-level accounts combined as one:
```
--alias='/^[^:]+/=combined'  ( [^:] matches any character other than : )
```

Show accounts with the second-level part removed:
```
--alias '/^([^:]+):[^:]+/ = \1'
                     match a top-level account and a second-level account
                     and replace those with just the top-level account
                     ( \1 in the replacement text means "whatever was matched
                     by the first parenthesised part of the regexp"
```

CSV rules: match CSV records containing dining-related MCC codes:
```
if \?MCC581[124]
```

Match CSV records with a specific amount around the end/start of month:
```
if %amount \b3\.99
&  %date   (29|30|31|01|02|03)$
```

### hledger's regular expressions

hledger's regular expressions come from the
[regex-tdfa](http://hackage.haskell.org/package/regex-tdfa/docs/Text-Regex-TDFA.html)
library. 
If they're not doing what you expect, it's important to know exactly what they support:

1. they are case insensitive
2. they are infix matching (they do not need to match the entire thing being matched)
3. they are [POSIX ERE] (extended regular expressions)
4. they also support [GNU word boundaries] (`\b`, `\B`, `\<`, `\>`)
5. [backreferences] are supported when doing text replacement in [account
   aliases](#regex-aliases) or [CSV rules](#csv-rules), where [backreferences]
   can be used in the replacement string to reference [capturing groups] in the
   search regexp. Otherwise, if you write `\1`, it will match the digit `1`.
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

## Argument files

You can save a set of command line options and arguments in a file,
and then reuse them by writing `@FILENAME` as a command line argument.
Eg: `hledger bal @foo.args`.

(Inside the argument file, each line should contain just one option or argument.
Don't use spaces except inside quotes; write `=` or nothing between a flag and its argument.
For the special characters mentioned above, use one less level of quoting than
you would at the command prompt.)

Argument files are now superseded by..

## Config files

hledger will read extra command line options from a `hledger.conf` config file.
These will be inserted early in the command line, so your later options can override them if needed.
The config file can contain general options (which will be used with all commands that support them), and command-specific options (or arguments).
[hledger.conf.sample](https://github.com/simonmichael/hledger/blob/master/hledger.conf.sample) is an example,
which you can install as, eg, `./hledger.conf` or `$HOME/.hledger.conf`.

To be precise, hledger looks for `hledger.conf` in the current directory or above,
or in your home directory (with a dotted name, `~/.hledger.conf`),
or finally in your XDG config directory (`~/.config/hledger/hledger.conf`).
Or you can select a particular config file by using the `--conf` option,
or by adding a `hledger --conf` shebang line to a config file and executing it like a script (see the example file).
You can inspect the finding and processing of config files with `--debug` or `--debug=8`.

If you want to run hledger without a config file, to ensure standard defaults and behaviour, use the `-n/--no-conf` flag.
This is recommended when using hledger in scripts, and when troubleshooting problems.

When both `--conf` and `--no-conf` options are used, the last (right-most) wins.

*(in master, experimental)*

# Output

## Output destination

hledger commands send their output to the terminal by default.
You can of course redirect this, eg into a file, using standard shell syntax:
```cli
$ hledger print > foo.txt
```

Some commands (print, register, stats, the balance commands) also
provide the `-o/--output-file` option, which does the same thing
without needing the shell. Eg:
```cli
$ hledger print -o foo.txt
$ hledger print -o -        # write to stdout (the default)
```

## Output format

Some commands offer other kinds of output, not just text on the terminal.
Here are those commands and the formats currently supported:

| -                  | txt              | csv/tsv          | html               | json | sql |
|--------------------|------------------|------------------|--------------------|------|-----|
| aregister          | Y                | Y                | Y                  | Y    |     |
| balance            | Y *<sup>1</sup>* | Y *<sup>1</sup>* | Y *<sup>1,2</sup>* | Y    |     |
| balancesheet       | Y *<sup>1</sup>* | Y *<sup>1</sup>* | Y *<sup>1</sup>*   | Y    |     |
| balancesheetequity | Y *<sup>1</sup>* | Y *<sup>1</sup>* | Y *<sup>1</sup>*   | Y    |     |
| cashflow           | Y *<sup>1</sup>* | Y *<sup>1</sup>* | Y *<sup>1</sup>*   | Y    |     |
| incomestatement    | Y *<sup>1</sup>* | Y *<sup>1</sup>* | Y *<sup>1</sup>*   | Y    |     |
| print              | Y                | Y                |                    | Y    | Y   |
| register           | Y                | Y                |                    | Y    |     |

- *<sup>1</sup> Also affected by the balance commands' [`--layout` option](#balance-report-layout).*
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
```cli
$ hledger print -O csv    # print CSV on stdout
```

or by the filename extension of an output file specified with the `-o/--output-file=FILE.FMT` option:
```cli
$ hledger balancesheet -o foo.csv    # write CSV to foo.csv
```

The `-O` option can be combined with `-o` to override the file extension, if needed:
```cli
$ hledger balancesheet -o foo.txt -O csv    # write CSV to foo.txt
```

Some notes about the various output formats:

### CSV output

- In CSV output, [digit group marks](#digit-group-marks) (such as thousands separators)
  are disabled automatically.

### HTML output

- HTML output can be styled by an optional `hledger.css` file in the same directory.

### JSON output

- This is not yet much used; real-world feedback is welcome.

- Our JSON is rather large and verbose, since it is a faithful
  representation of hledger's internal data types. To understand the
  JSON, read the Haskell type definitions, which are mostly in
  <https://github.com/simonmichael/hledger/blob/master/hledger-lib/Hledger/Data/Types.hs>.
  [hledger-web's OpenAPI specification][openapi.yaml] may also be relevant.

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

[openapi.yaml]: https://github.com/simonmichael/hledger/blob/master/hledger-web/config/openapi.yaml

### SQL output

- This is not yet much used; real-world feedback is welcome.

- SQL output is expected to work at least with SQLite, MySQL and Postgres.

- For SQLite, it will be more useful if you modify the generated `id` field
  to be a PRIMARY KEY. Eg:
  ```
  $ hledger print -O sql | sed 's/id serial/id INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL/g' | ...
  ```

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

```cli
$ hledger print -c '$1.000,0'
```

This option can repeated to set the display style for multiple
commodities/currencies. Its argument is as described in 
the [commodity directive](#commodity-directive).

In some cases hledger will adjust number formatting to improve their parseability
(such as adding [trailing decimal marks](#trailing-decimal-marks) when needed).

## Colour

In terminal output, some commands can produce colour when the terminal supports it:

- if the `--color/--colour` option is given a value of `yes` or `always`
  (or `no` or `never`), colour will (or will not) be used;
- otherwise, if the `NO_COLOR` environment variable is set, colour will not be used;
- otherwise, colour will be used if the output (terminal or file) supports it.

## Box-drawing

In terminal (text) output, to minimise the risk of display problems,
table borders are drawn using only ascii characters by default.

To see tables with prettier unicode box-drawing characters, add the `--pretty` flag.
This will also show outer borders and inter-column borders.

## Paging

When showing long output in the terminal, hledger will try to use
the pager specified by the `PAGER` environment variable, or `less`, or `more`.
(A pager is a helper program that shows one page at a time rather than scrolling everything off screen).
Currently it does this only for help output, not for reports; specifically,

- when listing commands, with `hledger`
- when showing help with `hledger [CMD] --help`,
- when viewing manuals with `hledger help` or `hledger --man`.

Note the pager is expected to handle ANSI codes, which hledger uses eg for bold emphasis. 
For the common pager `less` (and its `more` compatibility mode),
we add `R` to the `LESS` and `MORE` environment variables to make this work.
If you use a different pager, you might need to configure it similarly, to avoid seeing junk on screen (let us know).
Otherwise, you can set the `NO_COLOR` environment variable to 1 to disable all ANSI output (see [Colour](#colour)).

## Debug output

We intend hledger to be relatively easy to troubleshoot, introspect and develop.
You can add `--debug[=N]` to any hledger command line to see additional debug output.
N ranges from 1 (least output, the default) to 9 (maximum output).
Typically you would start with 1 and increase until you are seeing enough.
Debug output goes to stderr, and is not affected by `-o/--output-file` (unless you redirect stderr to stdout, eg: `2>&1`).
It will be interleaved with normal output, which can help reveal when parts of the code are evaluated.
To capture debug output in a log file instead, you can usually redirect stderr, eg:
```cli
hledger bal --debug=3 2>hledger.log
```

# Environment

These environment variables affect hledger:

**COLUMNS**
This is normally set by your terminal;
some hledger commands (`register`) will format their output to this width.
If not set, they will try to use the available terminal width.

**LEDGER_FILE**
The main journal file to use when not specified with `-f/--file`.
Default: `$HOME/.hledger.journal`.

**NO_COLOR**
If this environment variable exists (with any value, including empty),
hledger will not use ANSI color codes in terminal output,
unless overridden by an explicit `--color=y`/`--colour=y` option.

# PART 2: DATA FORMATS

<a name="journal-format"></a>

# Journal

hledger's usual data source is a plain text file containing journal entries in hledger `journal` format.
If you're looking for a quick reference, jump ahead to the
[journal cheatsheet](#journal-cheatsheet) (or use the table of contents at <https://hledger.org/hledger.html>).

This file represents an accounting [General Journal](http://en.wikipedia.org/wiki/General_journal).
The `.journal` file extension is most often used, though not strictly required.
The journal file contains a number of transaction entries,
each describing a transfer of money (or any commodity) between two or more named accounts,
in a simple format readable by both hledger and humans.

hledger's journal format is compatible with most of 
[Ledger's journal format](http://ledger-cli.org/3.0/doc/ledger3.html#Journal-Format), but not all of it.
The differences and interoperation tips are described at [hledger and Ledger](/ledger.html).
With some care, and by avoiding incompatible features, you can keep your hledger journal
readable by Ledger and vice versa. This can useful eg for comparing the behaviour of one app
against the other.

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

A hledger journal file can contain three kinds of thing:
comment lines, transactions, and/or directives (including periodic transaction rules and auto posting rules).
Understanding the journal file format will also give you a good understanding of hledger's data model.
Here's a quick cheatsheet/overview, followed by detailed descriptions of each part.

## Journal cheatsheet

<!-- keep synced with hledger/test/ledger-compat/syntax/supported.test -->
```journal
# Here is the main syntax of hledger's journal format
# (omitting extra Ledger compatibility syntax).

###############################################################################

# 1. These are comment lines, for notes or temporarily disabling things.
; They begin with # or ;

comment
Or, lines can be enclosed within "comment" / "end comment".
This is a block of 
commented lines.
end comment

# Some journal entries can have semicolon comments at end of line  ; like this
# Some of them require 2 or more spaces before the semicolon.

###############################################################################

# 2. Directives customise processing or output in some way.
# You don't need any directives to get started.
# But they can add more error checking, or change how things are displayed.
# They begin with a word, letter, or symbol. 
# They are most often placed at the top, before transactions.

account assets             ; Declare valid account names and display order.
account assets:savings     ; A subaccount. This one represents a bank account.
account assets:checking    ; Another. Note, 2+ spaces after the account name.
account assets:receivable  ; Accounting type is inferred from english names,
account passifs            ; or declared with a "type" tag, type:L
account expenses           ; type:X
                           ; A follow-on comment line, indented.
account expenses:rent      ; Expense and revenue categories are also accounts.
                           ; Subaccounts inherit their parent's type.

commodity $0.00         ; Declare valid commodities and their display styles.
commodity 1.000,00 EUR

decimal-mark .          ; The decimal mark used in this file (if ambiguous).

payee Whole Foods       ; Declare a valid payee name.

tag trip                ; Declare a valid tag name.

P 2024-03-01 AAPL $179  ; Declare a market price for AAPL in $ on this date.

include other.journal   ; Include another journal file here.

# Declare a recurring "periodic transaction", for budget/forecast reports
~ monthly  set budget goals  ; <- Note, 2+ spaces before the description.
    (expenses:rent)      $1000
    (expenses:food)       $500

# Declare an auto posting rule, to modify existing transactions in reports
= revenues:consulting
    liabilities:tax:2024:us          *0.25  ; Add a tax liability & expense
    expenses:tax:2024:us            *-0.25  ; for 25% of the revenue.

###############################################################################

# 3. Transactions are what it's all about.
# They are dated events, usually movements of money between 2 or more accounts.
# They begin with a numeric date.
# Here is their basic shape:
#
# DATE DESCRIPTION    ; The transaction's date and optional description.
#   ACCOUNT1  AMOUNT  ; A posting of an amount to/from this account, indented.
#   ACCOUNT2  AMOUNT  ; A second posting, balancing the first.
#   ...               ; More if needed. Amounts must sum to zero.
#                     ; Note, 2+ spaces between account names and amounts.

2024-01-01 opening balances         ; At the start, declare pre-existing balances this way.
    assets:savings          $10000  ; Account names can be anything. lower case is easy to type.
    assets:checking          $1000  ; assets, liabilities, equity, revenues, expenses are common.
    liabilities:credit card  $-500  ; liabilities, equity, revenues balances are usually negative.
    equity:start                    ; One amount can be left blank. $-10500 is inferred here.
                                    ; Some of these accounts we didn't declare above,
                                    ; so -s/--strict would complain.

2024-01-03 ! (12345) pay rent
    ; Additional transaction comment lines, indented.
    ; There can be a ! or * after the date meaning "pending" or "cleared".
    ; There can be a parenthesised (code) after the date/status.
                                    ; Amounts' sign shows direction of flow.
    assets:checking          $-500  ; Minus means removed from this account (credit).
    expenses:rent             $500  ; Plus means added to this account (debit).

; Keeping transactions in date order is optional (but helps error checking).

2024-01-02 Gringott's Bank | withdrawal  ; Description can be PAYEE | NOTE
    assets:bank:gold       -10 gold
    assets:pouch            10 gold

2024-01-02 shopping
    expenses:clothing        1 gold
    expenses:wands           5 gold
    assets:pouch            -6 gold

2024-01-02 receive gift
    revenues:gifts          -3 "Chocolate Frogs"  ; Complex commodity symbols
    assets:pouch             3 "Chocolate Frogs"  ; must be in double quotes.

2024-01-15 buy some shares, in two lots                 ; Cost can be noted.
    assets:investments:2024-01-15     2.0 AAAA @ $1.50  ; @  means per-unit cost
    assets:investments:2024-01-15-02  3.0 AAAA @@ $4    ; @@ means total cost
                      ; ^ Per-lot subaccounts are sometimes useful.
    assets:checking                 $-7

2024-01-15 assert some account balances on this date
    ; Balances can be asserted in any transaction, with =, for extra error checking.
    ; Assertion txns like this one can be made with hledger close --assert --show-costs
    ;
    assets:savings                    $0                   = $10000
    assets:checking                   $0                   =   $493
    assets:bank:gold                   0 gold              =    -10 gold
    assets:pouch                       0 gold              =      4 gold
    assets:pouch                       0 "Chocolate Frogs" =      3 "Chocolate Frogs"
    assets:investments:2024-01-15      0.0 AAAA            =      2.0 AAAA @  $1.50
    assets:investments:2024-01-15-02   0.0 AAAA            =      3.0 AAAA @@ $4
    liabilities:credit card           $0                   =  $-500

2024-02-01 note some event, or a transaction not yet fully entered, on this date
    ; Postings are not required.

; Some other date formats are allowed (but, consistent YYYY-MM-DD is useful).
2024.01.01
2024/1/1
```

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
They represent events, typically a movement of some quantity of commodities between two or more named accounts.

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
the current transaction, the default year set with a [`Y` directive](#y-directive),
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

```cli
$ hledger -f t.j register food
2015-05-30                      expenses:food                  $10           $10
```

```cli
$ hledger -f t.j register checking
2015-06-01                      assets:checking               $-10          $-10
```

DATE should be a [simple date](#simple-dates); if the year is not
specified it will use the year of the transaction's date.  
The `date:` tag must have a valid simple date value if it is present,
eg a `date:` tag with no value is not allowed.

## Status

Transactions (or individual postings within a transaction)
can have a status mark, which is a single character before
the transaction description (or posting account name),
separated from it by a space, indicating one of three statuses:

| mark   | status   |
|--------|----------|
|        | unmarked |
| `!`    | pending  |
| `*`    | cleared  |

When reporting, you can filter by status with
the `-U/--unmarked`, `-P/--pending`, and `-C/--cleared` flags
(and you can combine these, eg `-UP` to match all except cleared things).
Or you can use the `status:`, `status:!`, and `status:*` [queries](#queries),
or the U, P, C keys in hledger-ui.

(Note: in Ledger the "unmarked" state is called "uncleared";
in hledger we renamed it to "unmarked" for semantic clarity.)

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

After the date, status mark and/or code fields, the rest of the line (or until a comment is begun with `;`) is the transaction's description.
Here you can describe the transaction (called the "narration" in traditional bookkeeping),
or you can record a payee/payer name,
or you can leave it empty.

Transaction descriptions show up in [print](#print) output and in [register](#register) reports,
and can be listed with the [descriptions](#descriptions) command.

You can [query](#queries) by description with `desc:DESCREGEX`, 
or [pivot](#pivoting) on description with `--pivot desc`.

### Payee and note

Sometimes people want a dedicated payee/payer field that can be queried and checked more strictly.
If you want that, you can write a `|` (pipe) character in the description.
This divides it into a "payee" field on the left, and a "note" field on the right.
(Either can be empty.)

You can query these with `payee:PAYEEREGEX` and `note:NOTEREGEX`,
list their values with the [payees](#payees) and [notes](#notes) commands,
or pivot on `payee` or `note`.

Note: in transactions with no `|` character, description, payee, and note all have the same value.
Once a `|` is added, they become distinct.
(If you'd like to change this behaviour, please propose it on the mail list.)

If you want more strict error checking, you can declare the valid payee names with [payee directives](#payee-directive), 
and then enforce these with [hledger check payees](#check).
(Note: because of the above, for this you'll need to ensure every transaction description contains a `|` and therefore a checkable payee name, even if it's empty.)


## Transaction comments

Text following `;`, after a transaction description,
and/or on indented lines immediately below it, form comments for that transaction.
They are reproduced by `print` but otherwise ignored,
except they may contain [tags](#tags), which are not ignored.

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
- (optional) **two or more spaces** (or tabs) followed by an [amount](#amounts).

If the amount is positive, it is being added to the account;
if negative, it is being removed from the account.

The posting amounts in a transaction must sum up to zero, indicating that the inflows and outflows are equal. We call this a balanced transaction.
(You can read more about the nitty-gritty details of "sum up to zero" in [Transaction balancing](#transaction-balancing) below.)

As a convenience, you can optionally leave one amount blank; hledger will infer what it should be so as to balance the transaction.

### Debits and credits

The traditional accounting concepts of debit and credit of course exist in hledger, but we represent them with numeric sign, as described above.
Positive and negative posting amounts represent debits and credits respectively.

You don't need to remember that, but if you would like to - eg for helping newcomers or for talking with your accountant - here's a handy mnemonic:

*`debit  / plus  / left  / short  words`*\
*`credit / minus / right / longer words`*

### The two space delimiter

Be sure to notice the unusual separator between the account name and the following amount.
Because hledger allows account names with spaces in them, you must separate the account name and amount (if any) by **two or more spaces** (or tabs).
It's easy to forget at first. If you ever see the amount being treated as part of the account name, you'll know you probably need to add another space between them.

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

Account names can be altered temporarily or permanently by [account aliases](#alias-directive).

## Amounts

After the account name, there is usually an amount.
(Remember: between account name and amount, there must be two or more spaces.)

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

<a name="decimal-marks-digit-group-marks"></a>

### Decimal marks

A *decimal mark* can be written as a period or a comma:

    1.23
    1,23

Both of these are common in [international number formats][international number formats],
so hledger is not biased towards one or the other.
Because hledger also supports digit group marks (eg thousands separators),
this means that a number like `1,000` or `1.000` containing just one period or comma is ambiguous.
In such cases, hledger by default assumes it is a decimal mark, and will parse both of those as 1.

[international number formats]: https://en.wikipedia.org/wiki/Decimal_separator#Conventions_worldwide

To help hledger parse such ambiguous numbers more accurately,
if you use digit group marks, we recommend declaring the decimal mark explicitly.
The best way is to add a [`decimal-mark`](#decimal-mark-directive) directive at the top of each data file, like this:
```journal
decimal-mark .
```
Or you can declare it per commodity with [`commodity`](#commodity-directive) directives, described below.

hledger also accepts numbers like `10.` with no digits after the decimal mark
(and will sometimes display numbers that way to disambiguate them - see
[Trailing decimal marks](#trailing-decimal-marks)).

### Digit group marks

In the integer part of the amount quantity (left of the decimal mark),
groups of digits can optionally be separated by a *digit group mark* -
a comma or period (whichever is not used as decimal mark), 
or a space (several Unicode space variants, like no-break space, are also accepted).
<!--
space,
no-break space,
en space,
em space,
punctuation space,
thin space,
narrow no-break space,
medium mathematical space).
-->
So these are all valid amounts in a journal file:

         $1,000,000.00
      EUR 2.000.000,00
    INR 9,99,99,999.00
          1 000 000.00   ; <- ordinary space  
          1 000 000.00   ; <- no-break space

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
<!-- (If you are writing scripts or working with hledger's internals, this is the `MixedAmount` type.) -->

By default, the format of amounts in the journal influences how hledger displays them in output.
This is explained in [Commodity display style](#commodity-display-style) below.

<a name="transaction-prices"></a>

### Costs

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
this is discussed more in the [Cost reporting](#cost-reporting) section.

Note that the cost normally should be a positive amount, though it's not required to be.
This can be a little confusing, see discussion at 
[--infer-market-prices: market prices from transactions](#--infer-market-prices-market-prices-from-transactions).

## Balance assertions

hledger supports
[Ledger-style balance assertions](http://ledger-cli.org/3.0/doc/ledger3.html#Balance-assertions)
in journal files.
These look like, for example, `= EXPECTEDBALANCE` following a posting's amount.
Eg here we assert the expected dollar balance in accounts a and b after
each posting:

```journal
2013/1/1
  a   $1 =  $1
  b      = $-1

2013/1/2
  a   $1 =  $2
  b  $-1 = $-2
```

After reading a journal file, hledger will check all balance
assertions and report an error if any of them fail. Balance assertions
can protect you from, eg, inadvertently disrupting reconciled balances
while cleaning up old entries. You can disable them temporarily with
the `-I/--ignore-assertions` flag, which can be useful for
troubleshooting or for reading Ledger files.
(Note: this flag currently does not disable balance assignments, described below).

### Assertions and ordering

hledger calculates and checks an account's balance assertions in date order
(and when there are multiple assertions on the same day, in parse order). 
Note this is different from Ledger, which checks assertions always in parse order, ignoring dates.

This means in hledger you can freely reorder transactions, postings, or files, and balance assertions will usually keep working.
The exception is when you reorder multiple postings on the same day, to the same account, which have balance assertions; those will likely need updating.

### Assertions and multiple included files

Multiple files included with the [`include` directive](#include-directive)
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

### Assertions and costs

Balance assertions ignore [costs](#costs),
and should normally be written without one:

``` journal
2019/1/1
  (a)     $1 @ €1 = $1
```

We do allow costs to be written in balance assertion amounts, however, and [print](#print) shows them,
but they don't affect whether the assertion passes or fails.
This is for backward compatibility (hledger's [close](#close) command used to generate balance assertions with costs),
and because [balance *assignments*](#balance-assignments) do use costs (see below).

### Assertions and commodities

The balance assertions described so far are "**single commodity balance assertions**":
they assert and check the balance in one commodity, ignoring any others that may be present.
This is how balance assertions work in Ledger also.

If an account contains multiple commodities, you can assert their balances
by writing multiple postings with balance assertions, one for each commodity:

``` journal
2013/1/1
  usd   $-1
  eur   €-1
  both

2013/1/2
  both    0 = $1
  both    0 = €1
```

In hledger you can make a stronger "**sole commodity balance assertion**" by writing two equals signs (`== EXPECTEDBALANCE`).
This also asserts that there are no other commodities in the account besides the asserted one (or at least, that their current balance is zero):

```
2013/1/1
  usd   $-1  == $-1  ; these sole commodity assertions succeed
  eur   €-1  == €-1
  both      ;==  $1  ; this one would fail because 'both' contains $ and €
```

It's less easy to make a "**sole commodities balance assertion**" (note the plural) -
ie, asserting that an account contains two or more specified commodities and no others.
It can be done by 

1. isolating each commodity in a subaccount, and asserting those
2. and also asserting there are no commodities in the parent account itself:

``` journal
2013/1/1
  usd       $-1
  eur       €-1
  both        0 == 0   ; nothing up my sleeve
  both:usd   $1 == $1  ; a dollar here
  both:eur   €1 == €1  ; a euro there
```

### Assertions and subaccounts

All of the balance assertions above (both `=` and `==`) are "**subaccount-exclusive balance assertions**";
they ignore any balances that exist in deeper subaccounts.

In hledger you can make "**subaccount-inclusive balance assertions**" by adding a star after the equals (`=*` or `==*`):

```journal
2019/1/1
  equity:start
  assets:checking  $10
  assets:savings   $10
  assets            $0 ==* $20  ; assets + subaccounts contains $20 and nothing else
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
Eg a [commodity directive](#commodity-directive)
may limit the display precision, but this will not affect balance assertions.
Balance assertion failure messages show exact amounts.

## Posting comments

Text following `;`, at the end of a posting line, 
and/or on indented lines immediately below it, form comments for that posting.
They are reproduced by `print` but otherwise ignored,
except they may contain [tags](#tags), which are not ignored.

```journal
2012-01-01
    expenses   1  ; a comment for posting 1
    assets
    ; a comment for posting 2
    ; a second comment line for posting 2
```

## Transaction balancing

How exactly does hledger decide when a transaction is balanced ?
The general goal is that if you look at the journal entry and calculate the amounts'
sum perfectly with pencil and paper, hledger should agree with you.

Real world transactions, especially for investments or cryptocurrencies,
often involve imprecise costs, complex decimals, and/or infinitely-recurring decimals,
which are difficult or inconvenient to handle on a computer.
So to be a practical accounting system, hledger allows some imprecision when checking transaction balancedness.
The question is, how much imprecision should be allowed ?

hledger currently decides it based on the [commodity display styles](#commodity-display-style):
if the postings' sum would appear to be zero when displayed with the standard display precisions, the transaction is considered balanced.

Or equivalently: if the journal entry is displayed with amounts rounded to the 
standard display precisions (with `hledger print --round=hard`), and a human with
pencil and paper would agree that those displayed amounts add up to zero,
the transaction is considered balanced.

This has some advantages: it is fairly intuitive, general not hard-coded, yet configurable when needed.
On the downside it means that transaction balancedness is related to commodity display precisions,
so eg when using `-c/--commodity-style` to display things with more than usual precision,
you might need to fix some of your journal entries (ie, add decimal digits to make them balance more precisely).

Other PTA tools (Ledger, Beancount..) have their own ways of doing it.
Possible improvements are discussed at [#1964](https://github.com/simonmichael/hledger/issues/1964).

Note: if you have multiple journal files, and are relying on commodity directives to make imprecise journal entries balance,
the directives' placement might be important - see [`commodity` directive](#commodity-directive).

## Tags

<!-- same section name as Commands > tags, if reordering these update all #tags[-1] links -->

Tags are a way to add extra labels or data fields to transactions, postings, or accounts,
which you can then [search](#queries) or [pivot](#pivoting) on.

A tag is a word, optionally hyphenated, immediately followed by a full colon,
in the [comment](#account-comments) of a transaction, a posting, or an account directive.
Eg: `2024-01-01 a transaction   ; foo:`
Note this is an exception to the usual rule that things in comments are ignored.

You can write multiple tags on one line, separated by comma.
Or you can write each tag on its own comment line (no comma needed in this case).

For example, here are five different tags: 
one on the `assets:checking` account, two on the transaction, and two on the `expenses:food` posting:

```journal
account assets:checking         ; accounttag:

2017/1/16 bought groceries      ; transactiontag-1:
    ; transactiontag-2:
    assets:checking        $-1
    expenses:food           $1  ; postingtag:, another-posting-tag:
```

Postings also inherit tags from their transaction and their account.
And transactions also acquire tags from their postings (and postings' accounts).
So in the example above, the expenses posting effectively has all five tags
(by inheriting from the account and transaction), 
and the transaction also has all five tags (by acquiring from the expenses posting).

### Tag names

Most non-whitespace characters are allowed in tag names. Eg `😀:` is a valid tag.

You can list the tag names used in your journal with the [tags](#tags) command:\
`hledger tags [NAMEREGEX]`

In commands which use a [query](#queries), you can match by tag name. Eg:\
`hledger print tag:NAMEREGEX`

You can declare valid tag names with the [tag directive](#tag-directive) and then check them with the [check](#check) command.

### Special tags

Some tag names have special significance to hledger.
There's not much harm in using them yourself, but some could produce an error message, particularly the `date:` and `type:` tags.
They are explained elsewhere, but here is a quick list for reference:

<!-- keep synced with JournalChecks.hs -->
Tags you can set to influence hledger's behaviour:
```
 date                   -- overrides a posting's date
 date2                  -- overrides a posting's secondary date
 type                   -- declares an account's type
```
Tags hledger adds to indicate generated data:
```
 t                      -- appears on postings generated by timedot letters
 assert                 -- appears on txns generated by close --assert
 retain                 -- appears on txns generated by close --retain
 start                  -- appears on txns generated by close --migrate/--close/--open/--assign
 generated-transaction  -- appears on generated periodic txns (with --verbose-tags)
 generated-posting      -- appears on generated auto postings (with --verbose-tags)
 modified               -- appears on txns which have had auto postings added (with --verbose-tags)
Not displayed, but queryable:
 _generated-transaction -- exists on generated periodic txns (always)
 _generated-posting     -- exists on generated auto postings (always)
 _modified              -- exists on txns which have had auto postings added (always)
```
Tags hledger uses internally:
```
 _conversion-matched    -- exists on postings which have been matched with a nearby @/@@ cost annotation
```

### Tag values

Tags can have a value, which is any text after the colon up until a comma or end of line, with surrounding whitespace removed.
Ending at comma allows us to write multiple tags on one line, but also means that tag values can not contain commas.

Eg in the following posting, the three tags' values are "value 1", "value 2", and "" (empty) respectively:
```journal
    expenses:food   $10    ; foo, tag1: value 1 , tag2:value 2, bar tag3: , baz
```

Multiple tags with the same name are additive rather than overriding:
when the same tag name is seen again with a new value, the new name:value pair is added to the tags.
It is not possible to override a previous tag's value or remove a tag.

You can list all the values used for a particular tag in the journal with\
`hledger tags TAGNAME --values`

You can match on tag values with a query like `tag:NAMEREGEX=VALUEREGEX`

## Directives

Besides transactions, there is something else you can put in a `journal` file: directives.
These are declarations, beginning with a keyword, that modify hledger's behaviour.
Some directives can have more specific subdirectives, indented below them.
hledger's directives are similar to Ledger's in many cases, but there are also many [differences](ledger.md).
Directives are not required, but can be useful. Here are the main directives:

| purpose                                                       | directive                                      |
|---------------------------------------------------------------|------------------------------------------------|
| **READING DATA:**                                             |                                                |
| Rewrite account names                                         | [`alias`]                                      |
| Comment out sections of the file                              | [`comment`]                                    |
| Declare file's decimal mark, to help parse amounts accurately | [`decimal-mark`]                               |
| Include other data files                                      | [`include`]                                    |
| **GENERATING DATA:**                                          |                                                |
| Generate recurring transactions or budget goals               | [`~`]                                          |
| Generate extra postings on existing transactions              | [`=`]                                          |
| **CHECKING FOR ERRORS:**                                      |                                                |
| Define valid entities to provide more error checking          | [`account`], [`commodity`], [`payee`], [`tag`] |
| **REPORTING:**                                                |                                                |
| Declare accounts' type and display order                      | [`account`]                                    |
| Declare commodity display styles                              | [`commodity`]                                  |
| Declare market prices                                         | [`P`]                                          |

### Directives and multiple files

Directives vary in their scope, ie which journal entries and which input files they affect.
Most often, a directive will affect the following entries and included files if any, until the end of the current file - and no further.
You might find this inconvenient! 
For example, `alias` directives [do not affect parent or sibling files](#aliases-and-multiple-files).
But there are usually workarounds; for example, put `alias` directives in your top-most file, before including other files.

The restriction, though it may be annoying at first, is in a good cause; it allows reports to be stable and deterministic, independent of the order of input. Without it, reports could show different numbers depending on the order of -f options, or the positions of include directives in your files.

### Directive effects

Here are all hledger's directives, with their effects and scope summarised - nine main directives, plus four others which we consider non-essential:

| directive                     | what it does                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       | ends at file end?   |
|-------------------------------|------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|---------------------|
| **[`account`]**               | Declares an account, for [checking](#check) all entries in all files; <br>and its [display order](#account-display-order) and [type](#declaring-account-types). <br>Subdirectives: any text, ignored.                                                                                                                                                                                                                                                                                                                                                                              | N                   |
| **[`alias`]**                 | Rewrites account names, in following entries until end of current file or [`end aliases`]. <br>Command line equivalent: [`--alias`]                                                                                                                                                                                                                                                                                                                                                                                                                                                | Y                   |
| **[`comment`]**               | Ignores part of the journal file, until end of current file or `end comment`.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      | Y                   |
| **[`commodity`]**             | Declares up to four things: <br>1. a commodity symbol, for checking all amounts in all files <br>2. the display style for all amounts of this commodity <br>3. the decimal mark for parsing amounts of this commodity, in the rest of this file and its children, if there is no `decimal-mark` directive <br>4. the precision to use for balanced-transaction checking in this commodity, in this file and its children. <br> Takes precedence over `D`. <br>Subdirectives: `format` (ignored). <br>Command line equivalent: [`-c/--commodity-style`](#commodity-styles)          | N,<br>N,<br>Y,<br>Y |
| **[`decimal-mark`]**          | Declares the decimal mark, for parsing amounts of all commodities in following entries until next `decimal-mark` or end of current file. Included files can override. Takes precedence over `commodity` and `D`.                                                                                                                                                                                                                                                                                                                                                                   | Y                   |
| **[`include`]**               | Includes entries and directives from another file, as if they were written inline. <br>Command line alternative: multiple [`-f/--file`](#multiple-files)                                                                                                                                                                                                                                                                                                                                                                                                                           | N                   |
| **[`payee`]**                 | Declares a payee name, for checking all entries in all files.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      | N                   |
| **[`P`]**                     | Declares the market price of a commodity on some date, for [value reports](#value-reporting).                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      | N                   |
| **[`~`]** (tilde)             | Declares a periodic transaction rule that generates future transactions with `--forecast` and budget goals with `balance --budget`.                                                                                                                                                                                                                                                                                                                                                                                                                                                | N                   |
| Other syntax:                 |                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    |                     |
| **[`apply account`]**         | Prepends a common parent account to all account names, in following entries until end of current file or `end apply account`.                                                                                                                                                                                                                                                                                                                                                                                                                                                      | Y                   |
| **[`D`]**                     | Sets a default commodity to use for no-symbol amounts;<br>and, if there is no `commodity` directive for this commodity: its decimal mark, balancing precision, and display style, as above.                                                                                                                                                                                                                                                                                                                                                                                        | Y,<br>Y,<br>N,<br>N |
| **[`Y`]**                     | Sets a default year to use for any yearless dates, in following entries until end of current file.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 | Y                   |
| **[`=`]** (equals)            | Declares an auto posting rule that generates extra postings on matched transactions with `--auto`, in current, parent, and child files (but not sibling files, see [#1212](https://github.com/simonmichael/hledger/issues/1212)).                                                                                                                                                                                                                                                                                                                                                  | partly              |
| **[Other Ledger directives]** | Other directives from Ledger's file format are accepted but ignored.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               |                     |

[`=`]:                       #auto-postings
[`D`]:                       #d-directive
[`P`]:                       #p-directive
[`Y`]:                       #y-directive
[`account`]:                 #account-directive
[`alias`]:                   #alias-directive
[`--alias`]:                 #alias-directive
[`apply account`]:           #apply-account-directive
[`comment`]:                 #comments
[`commodity`]:               #commodity-directive
[`decimal-mark`]:              #decimal-mark-directive
[`end aliases`]:             #end-aliases-directive
[`include`]:                 #include-directive
[`payee`]:                   #payee-directive
[`tag`]:                     #tag-directive
[`~`]:                       #periodic-transactions
[Other Ledger directives]:   #other-ledger-directives



## `account` directive

`account` directives can be used to declare accounts 
(ie, the places that amounts are transferred from and to).
Though not required, these declarations can provide several benefits:

- They can document your intended chart of accounts, providing a reference.
- They can store additional account information as [comments](#account-comments),
  or as [tags](#tags) which can be used to filter or pivot reports.
- They can restrict which accounts may be posted to by transactions, eg in [strict mode], which helps prevent errors.
- They influence account display order in reports, allowing non-alphabetic sorting (eg Revenues to appear above Expenses).
- They can help hledger know your accounts' types (asset, liability, equity, revenue, expense), enabling reports like [balancesheet](#balancesheet) and [incomestatement](#incomestatement).
- They help with account name completion (in hledger add, hledger-web, hledger-iadd, ledger-mode, etc.)

They are written as the word `account` followed by a hledger-style [account name](#account-names). Eg:

```journal
account assets:bank:checking
```

Ledger-style indented subdirectives are also accepted, but ignored:

```journal
account assets:bank:checking
  format subdirective  ; currently ignored
```

### Account comments

Text following **two or more spaces** and `;` at the end of an account directive line,
and/or following `;` on indented lines immediately below it, form comments for that account.
They are ignored except they may contain [tags](#tags), which are not ignored.

The two-space requirement for same-line account comments is because `;` is allowed in account names.

```journal
account assets:bank:checking    ; same-line comment, at least 2 spaces before the semicolon
  ; next-line comment
  ; some tags - type:A, acctnum:12345
```

### Account error checking

By default, accounts need not be declared; they come into existence when a posting references them.
This is convenient, but it means hledger can't warn you when you mis-spell an account name in the journal.
Usually you'll find that error later, as an extra account in balance reports, 
or an incorrect balance when reconciling.

In [strict mode], enabled with the `-s`/`--strict` flag, hledger will report an error if any transaction uses an account name that has not been declared by an [account directive](#account). Some notes:

- The declaration is case-sensitive; transactions must use the correct account name capitalisation.
- The account directive's scope is "whole file and below" (see [directives](#directives)). This means it affects all of the current file, and any files it includes, but not parent or sibling files. The position of account directives within the file does not matter, though it's usual to put them at the top.
- Accounts can only be declared in `journal` files, but will affect [included](#include-directive) files of all types.
- It's currently not possible to declare "all possible subaccounts" with a wildcard; every account posted to must be declared.

### Account display order

Account directives also cause hledger to display accounts in a particular order, not just alphabetically.
Eg, here is a conventional ordering for the top-level accounts:

```journal
account assets
account liabilities
account equity
account revenues
account expenses
```

Now hledger displays them in that order:
```cli
$ hledger accounts
assets
liabilities
equity
revenues
expenses
```

If there are undeclared accounts, those will be displayed last, in alphabetical order.

Sorting is done within each group of sibling accounts, at each level of the account tree.
Eg, a declaration like `account parent:child` influences `child`'s position among its siblings.

Note, it does not affect `parent`'s position; for that, you need an `account parent` declaration.

Sibling accounts are always displayed together; hledger won't display `x:y` in between `a:b` and `a:c`.

An account directive both declares an account as a valid posting target, and declares its display order; you can't easily do one without the other.

### Account types

hledger knows that accounts come in several types: assets, liabilities, expenses and so on. 
This enables easy reports like [balancesheet] and [incomestatement], and filtering by account type with the [`type:` query](#queries). 

As a convenience, hledger will detect these account types automatically if you are using common english-language top-level account names (described below). 
But it's more robust to declare accounts' types explicitly, by adding `type:` [tags](#tags) to their account directives.
The tag's value should be one of the [five main account types]:

- `A` or `Asset`		(things you own)
- `L` or `Liability`	(things you owe)
- `E` or `Equity`		(investment/ownership; balanced counterpart of assets & liabilities)
- `R` or `Revenue`		(what you received money from, AKA income; technically part of Equity)
- `X` or `Expense`		(what you spend money on; technically part of Equity)

or, it can be (these are used less often):

- `C` or `Cash`			(a subtype of Asset, indicating [liquid assets][CCE] for the [cashflow] report)
- `V` or `Conversion`	(a subtype of Equity, for conversions (see [Cost reporting](#cost-reporting)).)

Subaccounts inherit their parent's type, or they can override it.
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
[CCE]:                 https://en.wikipedia.org/wiki/Cash_and_cash_equivalents
[account directive]:   #account

Here are some tips for working with account types.

- The rules for inferring types from account names are as follows.
  These are just a convenience that sometimes help new users get going;
  if they don't work for you, just ignore them and declare your account types.
  See also [Regular expressions](#regular-expressions).
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

- Certain uses of [account aliases](#alias-directive) can disrupt account types.
  See [Rewriting accounts > Aliases and account types](#aliases-and-account-types).

- As mentioned above, subaccounts will inherit a type from their parent account. 
  More precisely, an account's type is decided by the first of these that exists:

  1. A `type:` declaration for this account.
  2. A `type:` declaration in the parent accounts above it, preferring the nearest.
  3. An account type inferred from this account's name.
  4. An account type inferred from a parent account's name, preferring the nearest parent.
  5. Otherwise, it will have no type.

- For troubleshooting, you can list accounts and their types with:
  ```cli
  $ hledger accounts --types [ACCTPAT] [-DEPTH] [type:TYPECODES]
  ```

## `alias` directive

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
[included files](#include-directive)
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
```cli
hledger -f a.aliases -f b.journal
```
account aliases defined in a.aliases will not affect b.journal. 
Including the aliases doesn't work either:
```journal
include a.aliases

2023-01-01  ; not affected by a.aliases
  foo  1
  bar
```
This means that account aliases should usually be declared at the
start of your top-most file, like this:
```journal
alias foo=Foo
alias bar=Bar

2023-01-01  ; affected by aliases above
  foo  1
  bar

include c.journal  ; also affected
```

### `end aliases` directive

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
```cli
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
```cli
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

```cli
$ hledger accounts --alias assets=bassetts type:a
```

## `commodity` directive

The `commodity` directive performs several functions:

1. It declares which commodity symbols may be used in the journal,
   enabling useful error checking with [strict mode] or the check command.
   See [Commodity error checking](#commodity-error-checking) below.

2. It declares how all amounts in this commodity should be displayed, eg how many decimals to show.
   See [Commodity display style](#commodity-display-style) above.

3. (If no `decimal-mark` directive is in effect:)
   It sets the decimal mark to expect (period or comma) when parsing amounts in this commodity,
   in this file and files it includes, from the directive until end of current file.
   See [Decimal marks](#decimal-marks) above.

4. It declares the precision with which this commodity's amounts should be compared when checking for balanced transactions,
   anywhere in this file and files it includes, until end of current file.

Declaring commodities solves several common parsing/display problems, so we recommend it.

Note that effects 3 and 4 above end at the end of the directive's file,
and will not affect sibling or parent files.
So if you are relying on them (especially 4) and using multiple files,
placing your commodity directives in a top-level parent file might be important.
Or, keep your decimal marks unambiguous and your entries well balanced and precise.

(Related: [#793](https://github.com/simonmichael/hledger/issues/793))

### Commodity directive syntax

A commodity directive is normally the word `commodity`
followed by a sample [amount](#amounts) (and optionally a comment).
Only the amount's symbol and format is significant.
Eg:

```journal
commodity $1000.00
commodity 1.000,00 EUR
commodity 1 000 000.0000   ; the no-symbol commodity
```

Commodities do not have tags (tags in the comment will be ignored).

A commodity directive's sample amount must always include a period or comma decimal mark
(this rule helps disambiguate decimal marks and digit group marks).
If you don't want to show any decimal digits, write the decimal mark at the end:

```journal
commodity 1000. AAAA       ; show AAAA with no decimals
```

Commodity symbols containing spaces, numbers, or punctuation must be enclosed in double quotes, [as usual](#commodity):

```journal
commodity 1.0000 "AAAA 2023"
```

Commodity directives normally include a sample amount, but can declare only a symbol (ie, just function 1 above):

```journal
commodity $
commodity INR
commodity "AAAA 2023"
commodity ""               ; the no-symbol commodity
```

Commodity directives may also be written with an indented `format` subdirective, as in Ledger.
The symbol is repeated and must be the same in both places.
Other subdirectives are currently ignored:

```journal
; display indian rupees with currency name on the left,
; thousands, lakhs and crores comma-separated,
; period as decimal point, and two decimal places.
commodity INR
  format INR 1,00,00,000.00
  an unsupported subdirective  ; ignored by hledger
```

### Commodity error checking

In [strict mode] (`-s`/`--strict`) (or when you run `hledger check commodities`),
hledger will report an error if an undeclared commodity symbol is used.
(With one exception: zero amounts are always allowed to have no commodity symbol.)
It works like [account error checking](#account-error-checking) (described above).


## `decimal-mark` directive

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

This prevents any [ambiguity](#decimal-mark) when
parsing numbers in the file, so we recommend it, especially if the
file contains digit group marks (eg thousands separators).

## `include` directive

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
[Data formats](#data-formats)):
`include timedot:~/notes/2023*.md`.

[glob patterns]: https://hackage.haskell.org/package/Glob-0.9.2/docs/System-FilePath-Glob.html#v:compile

## `P` directive

The `P` directive declares a market price, which is
a conversion rate between two commodities on a certain date.
This allows [value reports](#value-reporting) to convert amounts of one commodity
to their value in another, on or after that date.
These prices are often obtained from
a [stock exchange](https://en.wikipedia.org/wiki/Stock_exchange),
[cryptocurrency exchange](https://en.wikipedia.org/wiki/Cryptocurrency_exchange),
the or [foreign exchange market](https://en.wikipedia.org/wiki/Foreign_exchange_market).

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
in another commodity. See [Value reporting](#value-reporting).

<a name="automated-postings"></a>

## `payee` directive

`payee PAYEE NAME`

This directive can be used to declare a limited set of payees which may appear in [transaction descriptions](#descriptions).
The ["payees" check](#check) will report an error if any transaction refers to a payee that has not been declared.
Eg:

```journal
payee Whole Foods    ; a comment
```
Payees do not have tags (tags in the comment will be ignored).

To declare the empty payee name, use `""`.
```journal
payee ""
```

Ledger-style indented subdirectives, if any, are currently ignored.

## `tag` directive

`tag TAGNAME`

This directive can be used to declare a limited set of tag names allowed in [tags](#tags).
TAGNAME should be a valid tag name (no spaces). Eg:

```journal
tag  item-id
```
Any indented subdirectives are currently ignored.

The ["tags" check](#check) will report an error if any undeclared tag name is used.
It is quite easy to accidentally create a tag through normal use of colons in [comments](#comments);
if you want to prevent this, you can declare and check your tags .

## Periodic transactions

The `~` directive declares a "periodic rule" which generates temporary extra transactions, usually recurring at some interval,
when hledger is run with the `--forecast` flag.
These "forecast transactions" are useful for [forecasting](#forecasting) future activity.
They exist only for the duration of the report, and only when `--forecast` is used; they are not saved in the journal file by hledger.

Periodic rules also have a second use: with the `--budget` flag they set budget goals for [budgeting](#budgeting).

Periodic rules can be a little tricky, so before you use them, read this whole section, or at least the following tips:

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
   `~ every 10th day of month from 2023/01`, which is equivalent to <br>
   `~ every 10th day of month from 2023/01/01`, will be adjusted to start on 2019/12/10.


### Periodic rule syntax

A periodic transaction rule looks like a normal journal entry,
with the date replaced by a tilde (`~`) followed by a
[period expression](#period-expressions)
(mnemonic: `~` looks like a recurring sine wave.):
```journal
# every first of month
~ monthly
    expenses:rent          $2000
    assets:bank:checking

# every 15th of month in 2023's first quarter:
~ monthly from 2023-04-15 to 2023-06-16
    expenses:utilities          $400
    assets:bank:checking
```

The period expression is the same syntax used for specifying multi-period reports,
just interpreted differently; there, it specifies report periods; 
here it specifies recurrence dates (the periods' start dates).

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

```journal
; 2 or more spaces needed here, so the period is not understood as "every 2 months in 2023"
;               ||
;               vv
~ every 2 months  in 2023, we will review
    assets:bank:checking   $1500
    income:acme inc
```

So,

- Do write two spaces between your period expression and your transaction description, if any.
- Don't accidentally write two spaces in the middle of your period expression.

## Auto postings

The `=` directive declares an "auto posting rule", 
which adds extra [postings](#postings) to existing transactions.
(Remember, postings are the account name & amount lines below a transaction's date & description.)

In the journal, an auto posting rule looks quite like a transaction,
but instead of date and description it has `=` (mnemonic: "match") and a [query](#queries), like this:

```journal
= QUERY
    ACCOUNT    AMOUNT
    ...
```

Queries are just like command line queries; an account name substring is most common.
Query terms containing spaces should be enclosed in single or double quotes.

Each `=` rule works like this: when hledger is run with the `--auto` flag,
wherever the QUERY matches a posting in the journal,
the rule's postings are added to that transaction, immediately below the matched posting.
Note these generated postings are temporary, existing only for the duration of the report,
and only when `--auto` is used; they are not saved in the journal file by hledger.

Generated postings' amounts can depend on the matched posting's amount.
So auto postings can be useful for, eg, adding tax postings with a standard percentage.
AMOUNT can be:

- a number with no commodity symbol, like `2`. 
  The matched posting's commodity symbol will be added to this.

- a normal amount with a commodity symbol, like `$2`.
  This will be used as-is.

- an asterisk followed by a number, like `*2`.
  This will multiply the matched posting's amount (and total price, if any) by the number.

- an asterisk followed by an amount with commodity symbol, like `*$2`.
  This multiplies and also replaces the commodity symbol with this new one.

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
```cli
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

Note that depending fully on generated data such as this has some drawbacks -
it's less portable, less future-proof, less auditable by others, and less robust
(eg your balance assertions will depend on whether you use or don't use `--auto`).
An alternative is to use auto postings in "one time" fashion -
use them to help build a complex journal entry, view it with `hledger print --auto`,
and then copy that output into the journal file to make it permanent.

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

This also means that you cannot have more than one auto-posting with a missing
amount applied to a given transaction, as it will be unable to infer amounts.

### Auto posting tags

Automated postings will have some extra [tags](#tags):

- `generated-posting:= QUERY`  - shows this was generated by an auto posting rule, and the query
- `_generated-posting:= QUERY` - a hidden tag, which does not appear in hledger's output.
                                     This can be used to match postings generated "just now",
                                     rather than generated in the past and saved to the journal.

Also, any transaction that has been changed by auto posting rules will have these tags added:

- `modified:` - this transaction was modified
- `_modified:` - a hidden tag not appearing in the comment; this transaction was modified "just now".

### Auto postings on forecast transactions only

Tip: you can can make auto postings that will apply to forecast transactions
but not recorded transactions, by adding `tag:_generated-transaction` to their QUERY.
This can be useful when generating new journal entries to be saved in the journal.

## Other syntax

hledger journal format supports quite a few other features,
mainly to make interoperating with or converting from Ledger easier.
Note some of the features below are powerful and can be useful in special cases,
but in general, features in this section are considered less important
or even not recommended for most users.
Downsides are mentioned to help you decide if you want to use them.

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

#### Balance assignments and costs

A [cost](#costs) in a balance assignment
will cause the calculated amount to have that cost attached:

``` journal
2019/1/1
  (a)             = $1 @ €2
```
```cli
$ hledger print --explicit
2019-01-01
    (a)         $1 @ €2 = $1 @ €2
```

#### Balance assignments and multiple files

Balance assignments handle multiple files [like balance assertions](#assertions-and-multiple--f-files).
They see balance from other files previously included from the current file,
but not from previous sibling or parent files.

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

### `D` directive

`D AMOUNT`

This directive sets a default commodity, to be used for any
subsequent commodityless amounts (ie, plain numbers) seen while
parsing the journal. This effect lasts until the next `D` directive,
or the end of the current file.

For compatibility/historical reasons, `D` also acts like a [`commodity` directive](#commodity-directive)
(setting the commodity's decimal mark for parsing and [display style](#amount-display-format) for output).
So its argument is not just a commodity symbol, but a full amount demonstrating the style.
The amount must include a decimal mark (either period or comma).
Eg:
```journal
; commodity-less amounts should be treated as dollars
; (and displayed with the dollar sign on the left, thousands separators and two decimal places)
D $1,000.00

1/1
  a     5  ; <- commodity-less amount, parsed as $5 and displayed as $5.00
  b
```

Interactions with other directives:

For setting a commodity's display style, a `commodity` directive has highest priority,
then a `D` directive.

For detecting a commodity's decimal mark during parsing, `decimal-mark` has highest priority,
then `commodity`, then `D`.

For checking commodity symbols with the [check command](#check),
a `commodity` directive is required (`hledger check commodities` ignores `D` directives).

Downsides:
omitting commodity symbols
makes your financial data less explicit, less portable, and less trustworthy in an audit.
It is usually an unsustainable shortcut; sooner or later you will want to track multiple commodities.
D is overloaded with functions redundant with `commodity` and `decimal-mark`.
And it works differently from Ledger's `D`.

### `apply account` directive

This directive sets a default parent account, which will be prepended
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

### `Y` directive

`Y YEAR`

or (deprecated backward-compatible forms):

`year YEAR`
`apply year YEAR`

The space is optional.
This sets a default year to be used for subsequent dates which don't specify a year.  Eg:

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

A secondary date is written after the primary date, following an equals sign: `DATE1=DATE2`.
If the year is omitted, the primary date's year is assumed.
When running reports, the primary (left side) date is used by default,
but with the `--date2` flag (`--aux-date` or`--effective` also work, for Ledger users),
the secondary (right side) date will be used instead.

The meaning of secondary dates is up to you. Eg it could be
"primary is the bank's clearing date, secondary is the date the transaction was initiated, if different".

In practice, this feature usually adds confusion:

- You have to remember the primary and secondary dates' meaning, and follow that consistently.
- It splits your bookkeeping into two modes, and you have to remember which mode is appropriate for a given report.
- Usually your balance assertions will work with only one of these modes.
- It makes your financial data more complicated, less portable, and less clear in an audit.
- It interacts with every feature, creating an ongoing cost for implementors.
- It distracts new users and supporters.
- [Posting dates](#posting-dates) are simpler and work better.

So secondary dates are officially deprecated in hledger,
remaining only as a Ledger compatibility aid;
we recommend using posting dates instead.

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

A posting with parentheses around the account name, like `(some:account)   10`, is called an *unbalanced virtual posting*.
These postings do not participate in transaction balancing.
(And if you write them without an amount, a zero amount is always inferred.)
These can occasionally be convenient for special circumstances,
but they violate double entry bookkeeping and make your data less portable across applications,
so many people avoid using them at all.

A posting with brackets around the account name (`[some:account]`) is called a *balanced virtual posting*.
The balanced virtual postings in a transaction must add up to zero, just like ordinary postings,
but separately from them. These are not part of double entry bookkeeping either, but they are at
least balanced. An example:

```journal
2022-01-01 buy food with cash, update budget envelope subaccounts, & something else
  assets:cash                    $-10  ; <- these balance each other
  expenses:food                    $7  ; <-
  expenses:food                    $3  ; <-
  [assets:checking:budget:food]  $-10  ;   <- and these balance each other
  [assets:checking:available]     $10  ;   <-
  (something:else)                 $5  ;     <- this is not required to balance
```

Ordinary postings, whose account names are neither parenthesised nor bracketed, are called *real postings*.
You can exclude virtual postings from reports with the `-R/--real` flag or a `real:1` query.

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

By default, hledger expects this rules file to be named like the CSV file, 
with an extra `.rules` extension added, in the same directory. 
Eg when asked to read `foo/FILE.csv`, hledger looks for `foo/FILE.csv.rules`. 
You can specify a different rules file with the `--rules` option.

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
```cli
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
| [**`source`**](#source)                         | optionally declare which file to read data from                                                |
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

## `source`

If you tell hledger to read a csv file with `-f foo.csv`, it will look for rules in `foo.csv.rules`.
Or, you can tell it to read the rules file, with `-f foo.csv.rules`, and it will look for data in `foo.csv` (since 1.30).

These are mostly equivalent, but the second method provides some extra features.
For one, the data file can be missing, without causing an error; it is just considered empty.
And, you can specify a different data file by adding a "source" rule:

```rules
source ./Checking1.csv
```

If you specify just a file name with no path, hledger will look for it
in your system's downloads directory (`~/Downloads`, currently):

```rules
source Checking1.csv
```

And if you specify a glob pattern, hledger will read the most recent of the matched files
(useful with repeated downloads):

```rules
source Checking1*.csv
```

See also ["Working with CSV > Reading files specified by rule"](#reading-files-specified-by-rule).

## `separator`

You can use the `separator` rule to read other kinds of
character-separated data. The argument is any single separator
character, or the words `tab` or `space` (case insensitive). Eg, for
comma-separated values (CSV):

```rules
separator ,
```

or for semicolon-separated values (SSV):
```rules
separator ;
```

or for tab-separated values (TSV):
```rules
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
You'll need this whenever your CSV data contains header lines.
Note, empty and blank lines are skipped automatically, so you don't need to count those.

`skip` has a second meaning: it can be used inside [if blocks](#if-block) (described below),
to skip one or more records whenever the condition is true.
Records skipped in this way are ignored, except they are still required to be [valid CSV](#valid-csv).

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
```cli
$ TZ=-1000 hledger print -f foo.csv  # or TZ=-1000 hledger import foo.csv
```

`timezone` currently does not understand timezone names, except
"UTC", "GMT", "EST", "EDT", "CST", "CDT", "MST", "MDT", "PST", or "PDT".
For others, use numeric format: +HHMM or -HHMM.

## `newest-first`

hledger tries to ensure that the generated transactions will be ordered chronologically,
including same-day transactions.
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

If CSV records within a single day are ordered opposite to the overall record order,
you can add the `intra-day-reversed` rule to improve the order of journal entries.
Eg, here the overall record order is newest first, but same-day records are oldest first:
```csv
2022-10-02, txn 3...
2022-10-02, txn 4...
2022-10-01, txn 1...
2022-10-01, txn 2...
```
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

2. Whenever you use one of the special [hledger field names](#field-names) (described below),
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
referenced either by their 1-based position in the CSV record (`%N`)
or by the name they were given in the fields list (`%CSVFIELD`),
and regular expression [match groups](#match-groups) (`\N`).

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
   ```rules
   fields When, What, Some_Id, Net, Total, Foo, Bar
   ```

2. Special **hledger field names** (`HLEDGERFIELD` in these docs):
   you must set at least some of these to generate the hledger transaction from a CSV record,
   by writing them as the left hand side of a [field assignment](#field-assignment), eg:
   ```rules
   date        %When
   code        %Some_Id
   description %What
   comment     %Foo %Bar
   amount1     $ %Total
   ```
   or directly in a [`fields` list](#fields-list):
   ```rules
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

There are several ways to set posting amounts from CSV, useful in different situations.

1. **`amount`** is the oldest and simplest. Assigning to this sets the amount of the first and second postings.
  In the second posting, the amount will be negated; also, if it has a [cost](#costs) attached, it will be converted to cost.
  
2. **`amount-in`** and **`amount-out`** work exactly like the above, but should be used when
  the CSV has two amount fields (such as "Debit" and "Credit", or "Inflow" and "Outflow").
  Whichever field has a non-zero value will be used as the amount of the first and second postings.
  Here are some tips to avoid confusion:
  
    - It's not "amount-in for posting 1 and amount-out for posting 2",
      it is "extract a single amount from the amount-in or amount-out field, and use that for posting 1 and (negated) for posting 2".
    - Don't use both `amount` and `amount-in`/`amount-out` in the same rules file;
      choose based on whether the amount is in a single CSV field or spread across two fields.
    - In each record, at most one of the two CSV fields should contain a non-zero amount; the other field must contain a zero or nothing.
    - hledger assumes both CSV fields contain unsigned numbers, and it automatically negates the amount-out values.
    - If the data doesn't fit these requirements, you'll probably need an if rule (see below).

3. **`amountN`** (where N is a number from 1 to 99) sets the amount of only a single posting:
  the Nth posting in the transaction.
  You'll usually need at least two such assignments to make a balanced transaction.
  You can also generate more than two postings, to represent more complex transactions.
  The posting numbers don't have to be consecutive; with if rules, higher posting numbers
  can be useful to ensure a certain order of postings.
  
4. **`amountN-in`** and **`amountN-out`** work exactly like the above, but should be used when
  the CSV has two amount fields.
  This is analogous to `amount-in` and `amount-out`, and those tips also apply here.

5. Remember that a `fields` list can also do assignments. So in a fields list if you name a CSV field
  "amount", that counts as assigning to `amount`. (If you don't want that, call it something else in the fields list,
  like "amount_".)

6. The above don't handle every situation; if you need more flexibility, use an `if` rule
  to set amounts conditionally. See "[Working with CSV > Setting amounts](#setting-amounts)" below
  for more on this and on amount-setting generally.

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

See the [Working with CSV](#working-with-csv) tips below for more about setting amounts and currency.


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

### What matchers match

With record matchers, it's important to know that the record matched is not the original CSV record, but a modified one:
separators will be converted to commas, and enclosing double quotes (but not enclosing whitespace) are removed.
So for example, when reading an SSV file, if the original record was:
```ssv
2023-01-01; "Acme, Inc.";  1,000
```
the regex would see, and try to match, this modified record text:
```
2023-01-01,Acme, Inc.,  1,000
```

### Combining matchers

When an if block has multiple matchers, they are combined as follows:

- By default they are OR'd (any of them can match)
- When a matcher is preceded by ampersand (`&`, at the start of the line) it will be AND'ed with the previous matcher (all in the AND'ed group must match)
- *Added in 1.32* When a matcher is preceded by an exclamation mark (`!`), it is negated (it must not match).

Note [currently](https://github.com/simonmichael/hledger/pull/2088#issuecomment-1844200398) there is a limitation:
you can't use both `&` and `!` on the same line (you can't AND a negated matcher).

### Match groups

*Added in 1.32*

Matchers can define match groups: parenthesised portions of the regular expression
which are available for reference in field assignments. Groups are enclosed
in regular parentheses (`(` and `)`) and can be nested. Each group is available
in field assignments using the token `\N`, where N is an index into the match groups
for this conditional block (e.g. `\1`, `\2`, etc.).

Example: Warp credit card payment postings to the beginning of the billing period (Month
start), to match how they are presented in statements, using [posting dates](#posting-dates):

```rules
if %date (....-..)-..
  comment2 date:\1-01
```

Another example: Read the expense account from the CSV field, but throw away a prefix:

```rules
if %account1 liabilities:family:(expenses:.*)
    account1 \1
```

## `if` table

"if tables" are an alternative to [if blocks](#if-blocks);
they can express many matchers and field assignments in a more compact tabular format, like this:

```rules
if,HLEDGERFIELD1,HLEDGERFIELD2,...
MATCHERA,VALUE1,VALUE2,...
MATCHERB,VALUE1,VALUE2,...
; Comment line that explains MATCHERC
MATCHERC,VALUE1,VALUE2,...
<empty line>
```

The first character after `if` is taken to be this if table's field separator.
It is unrelated to the separator used in the CSV file.
It should be a non-alphanumeric character like `,` or `|` that does not appear anywhere else in the table
(it should not be used in field names or matchers or values, and it cannot be escaped with a backslash).

Each line must contain the same number of separators; empty values are allowed.
Whitespace can be used in the matcher lines for readability (but not in the if line, currently).
You can use the comment lines in the table body.
The table must be terminated by an empty line (or end of file).

An if table like the above is interpreted as follows:
try all of the matchers; 
whenever a matcher succeeds, assign all of the values on that line to the corresponding hledger fields;
If multiple lines match, later lines will override fields assigned by the earlier ones - just like the sequence of `if` blocks would behave.

If table presented above is equivalent to this sequence of if blocks:

```rules
if MATCHERA
  HLEDGERFIELD1 VALUE1
  HLEDGERFIELD2 VALUE2
  ...

if MATCHERB
  HLEDGERFIELD1 VALUE1
  HLEDGERFIELD2 VALUE2
  ...

; Comment line which explains MATCHERC
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
;; Comment line that desribes why this particular date is special
2023/01/12.*Plumbing LLC,expenses:house:upkeep,emergency plumbing call-out
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
```cli
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
```cli
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
hledger will look for a correspondingly-named rules file for each CSV file.
But if you specify a rules file with `--rules`, that rules file will be used for all the CSV files.

### Reading files specified by rule

Instead of specifying a CSV file in the command line, you can specify
a rules file, as in `hledger -f foo.csv.rules CMD`. 
By default this will read data from foo.csv in the same directory,
but you can add a [source](#source) rule to specify a different data file,
perhaps located in your web browser's download directory.

This feature was added in hledger 1.30, so you won't see it in most CSV rules examples.
But it helps remove some of the busywork of managing CSV downloads.
Most of your financial institutions's default CSV filenames are
different and can be recognised by a glob pattern.  So you can put a
rule like `source Checking1*.csv` in foo-checking.csv.rules, and then
periodically follow a workflow like:

1. Download CSV from Foo's website, using your browser's defaults
2. Run `hledger import foo-checking.csv.rules` to import any new transactions

After import, you can: discard the CSV, or leave it where it is for a
while, or move it into your archives, as you prefer. If you do nothing,
next time your browser will save something like Checking1-2.csv, 
and hledger will use that because of the `*` wild card and because
it is the most recent.

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
```cli
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
```cli
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

Continuing from [amount field](#amount-field) above, here are more tips for amount-setting:

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

2. **If the amount is in two CSV fields (such as Debit and Credit, or In and Out):**\

   a. **If both fields are unsigned:**\
     Assign one field to `amountN-in` and the other to `amountN-out`.
     hledger will automatically negate the "out" field,
     and will use whichever field value is non-zero as posting N's amount.

   b. **If either field is signed:**\
     You will probably need to override hledger's sign for one or the other field,
     as in the following example:
     ```rules
     # Negate the -out value, but only if it is not empty:
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

There is some special handling making it easier to parse and to reverse amount signs. (This only works for whole amounts, not for cost amounts such as COST in `amount1  AMT @ COST`):

- **If an amount value begins with a plus sign:**\
  that will be removed: `+AMT` becomes `AMT`

- **If an amount value is parenthesised:**\
  it will be de-parenthesised and sign-flipped: `(AMT)` becomes `-AMT`

- **If an amount value has two minus signs (or two sets of parentheses, or a minus sign and parentheses):**\
  they cancel out and will be removed: `--AMT` or `-(AMT)` becomes `AMT`

- **If an amount value contains just a sign (or just a set of parentheses):**\
  that is removed, making it an empty value. `"+"` or `"-"` or `"()"` becomes `""`.

It's not possible (without preprocessing the CSV) to set an amount to its absolute value, ie discard its sign. 

### Setting currency/commodity

If the currency/commodity symbol is included in the  CSV's amount field(s):

```csv
2023-01-01,foo,$123.00
```

you don't have to do anything special for the commodity symbol, it will be assigned as part of the amount. Eg:

```rules
fields date,description,amount
```
```journal
2023-01-01 foo
    expenses:unknown         $123.00
    income:unknown          $-123.00
```

If the currency is provided as a separate CSV field:

```csv
2023-01-01,foo,USD,123.00
```

You can assign that to the `currency` pseudo-field, which has the
special effect of prepending itself to every amount in the
transaction (on the left, with no separating space):
  
```rules
fields date,description,currency,amount
```
```journal
2023-01-01 foo
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
2023-01-01 foo
    expenses:unknown        123.00 USD
    income:unknown         -123.00 USD
```
Note we used a temporary field name (`cur`) that is not `currency` -
that would trigger the prepending effect, which we don't want here.

### Amount decimal places

When you are reading CSV data, eg with a command like `hledger -f foo.csv print`,
hledger will infer each commodity's decimal precision (and other [commodity display styles](#commodity-display-styles)) from the amounts -
much as when reading a journal file without `commodity` directives (see the link).

Note, the commodity styles are not inferred from the numbers in the original CSV data;
rather, they are inferred from the amounts generated by the CSV rules.

When you are importing CSV data with the `import` command, eg `hledger import foo.csv`, there's another step:
`import` tries to make the new entries [conform](#commodity-display-styles) to the journal's existing styles.
So for each commodity - let's say it's EUR - `import` will choose:

1. the style declared for EUR by [a `commodity` directive](#commodity-directive) in the journal
2. otherwise, the style inferred from EUR amounts in the journal
3. otherwise, the style inferred from EUR amounts generated by the CSV rules.

TLDR: if `import` is not generating the precisions or styles you want, add a `commodity` directive to specify them.


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
```cli
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
```cli
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
```cli
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

```cli
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
i 2015/03/30 09:00:00 some account  optional description after 2 spaces ; optional comment, tags:
o 2015/03/30 09:20:00
i 2015/03/31 22:21:45 another:account
o 2015/04/01 02:00:34
```

hledger treats each clock-in/clock-out pair as a transaction posting
some number of hours to an account. Or if the session spans more than
one day, it is split into several transactions, one for each day. For
the above time log, `hledger print` generates these journal entries:

```cli
$ hledger -f t.timeclock print
2015-03-30 * optional description after 2 spaces   ; optional comment, tags:
    (some account)           0.33h

2015-03-31 * 22:21-23:59
    (another:account)           1.64h

2015-04-01 * 00:00-02:00
    (another:account)           2.01h

```

Here is a
[sample.timeclock](https://raw.github.com/simonmichael/hledger/master/examples/sample.timeclock) to
download and some queries to try:

```cli
$ hledger -f sample.timeclock balance                               # current time balances
$ hledger -f sample.timeclock register -p 2009/3                    # sessions in march 2009
$ hledger -f sample.timeclock register -p weekly --depth 1 --empty  # time summary by week
```

To generate time logs, ie to clock in and clock out, you could:

- use emacs and the built-in timeclock.el, or
  the extended [timeclock-x.el](http://www.emacswiki.org/emacs/timeclock-x.el)
  and perhaps the extras in [ledgerutils.el](http://hub.darcs.net/simon/ledgertools/ledgerutils.el)

- at the command line, use these bash aliases:
    ```cli
    alias ti="echo i `date '+%Y-%m-%d %H:%M:%S'` \$* >>$TIMELOG"
    alias to="echo o `date '+%Y-%m-%d %H:%M:%S'` >>$TIMELOG"
    ```
- or use the old `ti` and `to` scripts in the [ledger 2.x repository](https://github.com/ledger/ledger/tree/maint/scripts).
  These rely on a "timeclock" executable which I think is just the ledger 2 executable renamed.


<a name="timedot-format"></a>

# Timedot


`timedot` format is hledger's human-friendly time logging format.
Compared to [`timeclock` format](#timeclock), it is
more convenient for quick, approximate, and retroactive time logging,
and more human-readable (you can see at a glance where time was spent).
A quick example:

```timedot
2023-05-01
hom:errands          .... ....  ; two hours; the space is ignored
fos:hledger:timedot  ..         ; half an hour
per:admin:finance               ; no time spent yet
```

hledger reads this as a transaction on this day with three
(unbalanced) postings, where each dot represents "0.25".
No commodity symbol is assumed, but we typically interpret it as hours.


```cli
$ hledger -f a.timedot print   # .timedot file extension (or timedot: prefix) is required
2023-05-01 *
    (hom:errands)                    2.00  ; two hours
    (fos:hledger:timedot)            0.50  ; half an hour
    (per:admin:finance)                 0
```

A timedot file contains a series of transactions (usually one per day).
Each begins with a **[simple date](#simple-dates)** (Y-M-D, Y/M/D, or Y.M.D),
optionally be followed on the same line by a transaction description,
and/or a transaction comment following a semicolon.

After the date line are zero or more time postings, consisting of:

- **An account name** - any hledger-style [account name](#account-names), optionally indented.

- **Two or more spaces** - required if there is an amount (as in journal format).

- **A timedot amount**, which can be

  - empty (representing zero)

  - a number, optionally followed by a unit `s`, `m`, `h`, `d`, `w`, `mo`, or `y`,
    representing a precise number of seconds, minutes, hours, days weeks, months or years
    (hours is assumed by default), which will be converted to hours according to
    60s  = 1m,
    60m  = 1h,
    24h  = 1d,
    7d   = 1w,
    30d  = 1mo,
    365d = 1y.
  
  - one or more dots (period characters), each representing 0.25.
    These are the dots in "timedot".
    Spaces are ignored and can be used for grouping/alignment.

  - *Added in 1.32* one or more letters. These are like dots but they also generate
    a tag `t:` (short for "type") with the letter as its value,
    and a separate posting for each of the values.
    This provides a second dimension of categorisation,
    viewable in reports with `--pivot t`.

- **An optional comment** following a semicolon (a hledger-style [posting comment](#posting-comments)).

There is some flexibility to help with keeping time log data and notes in the same file:

- Blank lines and lines beginning with `#` or `;` are ignored.

- After the first date line, lines which do not contain a double space 
  are parsed as postings with zero amount.
  (hledger's register reports will show these if you add -E).

- Before the first date line, lines beginning with `*` (eg org headings) are ignored.
  And from the first date line onward, Emacs org mode heading prefixes at the start of lines
  (one or more `*`'s followed by a space) will be ignored.
  This means the time log can also be a org outline.

## Timedot examples

Numbers:

```timedot
2016/2/3
inc:client1   4
fos:hledger   3h
biz:research  60m
```

Dots:

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
```cli
$ hledger -f a.timedot print date:2016/2/2
2016-02-02 *
    (inc:client1)          2.00

2016-02-02 *
    (biz:research)          0.25
```
```cli
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

Letters:

```timedot
# Activity types:
#  c cleanup/catchup/repair
#  e enhancement
#  s support
#  l learning/research

2023-11-01
work:adm  ccecces
```
```cli
$ hledger -f a.timedot print
2023-11-01
    (work:adm)  1     ; t:c
    (work:adm)  0.5   ; t:e
    (work:adm)  0.25  ; t:s

```
```cli
$ hledger -f a.timedot bal
                1.75  work:adm
--------------------
                1.75  
```
```cli
$ hledger -f a.timedot bal --pivot t
                1.00  c
                0.50  e
                0.25  s
--------------------
                1.75  
```

Org:

```timedot
* 2023 Work Diary
** Q1
*** 2023-02-29
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

Using `.` as account name separator:

```timedot
2016/2/4
fos.hledger.timedot  4h
fos.ledger           ..
```
```cli
$ hledger -f a.timedot --alias '/\./=:' bal -t
                4.50  fos
                4.00    hledger:timedot
                0.50    ledger
--------------------
                4.50
```

<!--
Another sample, download from 
https://raw.github.com/simonmichael/hledger/master/examples/sample.timedot and try:
```cli
$ hledger -f sample.timedot balance                               # current time balances
$ hledger -f sample.timedot register -p 2009/3                    # sessions in march 2009
$ hledger -f sample.timedot register -p weekly --depth 1 --empty  # time summary by week
```
```timedot
# sample.timedot
# This is a comment line
; Also a comment line
* Org headings before the first date are also comment lines

2023-01-01 transaction description
biz:research  ....
inc:client1   .... ..

2023-01-01 different transaction, same day ; with a comment and transaction-tag:
; more transaction comment lines ? currently ignored
fos:haskell  .... ; a posting comment and posting-tag:
; more posting comment lines ? currently ignored
per:admin    ....

** 2023-01-02  ; dates are allowed to be org headings
```
-->


# PART 3: REPORTING CONCEPTS

# Time periods

<a name="report-period"></a>

## Report start & end date

Most hledger reports will by default show the full time period represented by the journal.
The report start date will be the earliest transaction or posting date,
and the report end date will be the latest transaction, posting, or market price date.

Often you will want to see a shorter period, such as the current month.
You can specify a start and/or end date with the
[`-b/--begin`](#general-reporting-options),
[`-e/--end`](#general-reporting-options),
or
[`-p/--period`](#period-expressions) options,
or a [`date:`](#queries) query argument, described below.
All of these accept the [smart date](#smart-dates) syntax, also described below.

End dates are exclusive; specify the day after the last day you want to see in the report.

When dates are specified by multiple options, the last (right-most) option wins.
And when `date:` queries and date options are combined, the report period will be their intersection.

Examples:

`-b 2016/3/17`
: beginning on St. Patrick’s day 2016

`-e 12/1`
: ending at the start of December 1st in the current year

`-p 'this month'`
: during the current month

`-p thismonth`
: same as above, spaces are optional

`-b 2023`
: beginning on the first day of 2023

`date:2023..` or `date:2023-`
: same as above

`-b 2024 -e 2025 -p '2000 to 2030' date:2020-01 date:2020 `
:\
during January 2020 (the smallest common period, with the -p overriding -b and -e)

## Smart dates

In hledger's user interfaces (though not in the journal file), you can optionally use "smart date" syntax.
Smart dates can be written with english words, can be relative, and can have parts omitted.
Missing parts are inferred as 1, when needed.
Smart dates can be interpreted as dates or periods depending on context.

Examples:

`2004-01-01`, `2004/10/1`, `2004.9.1`, `20240504`
:\
Exact dates. The year must have at least four digits, the month must be 1-12, the day must be 1-31, the separator can be `-` or `/` or `.` or nothing.

`2004-10`
: start of month

`2004`
: start of year

`10/1` or `oct` or `october`
: October 1st in current year

`21`
: 21st day in current month

`yesterday, today, tomorrow`
: -1, 0, 1 days from today

`last/this/next day/week/month/quarter/year`
: -1, 0, 1 periods from the current period

`in n days/weeks/months/quarters/years`
: n periods from the current period

`n days/weeks/months/quarters/years ahead`
: n periods from the current period

`n days/weeks/months/quarters/years ago`
: -n periods from the current period

`20181201`
: 8 digit YYYYMMDD with valid year month and day

`201812`
: 6 digit YYYYMM with valid year and month


Dates with no separators are allowed but might give surprising results if mistyped:

- `20181301` (YYYYMMDD with an invalid month) is parsed as an eight-digit year
- `20181232` (YYYYMMDD with an invalid day) gives a parse error
- `201801012` (a valid YYYYMMDD followed by additional digits) gives a parse error

The meaning of relative dates depends on today's date.
If you need to test or reproduce old reports, you can use the `--today` option to override that.
(Except for periodic transaction rules, which are not affected by `--today`.)

## Report intervals

A report interval can be specified so that reports like
[register](#register), [balance](#balance) or [activity](#activity)
become multi-period, showing each subperiod as a separate row or column.

The following standard intervals can be enabled with command-line flags:

- `-D/--daily`
- `-W/--weekly`
- `-M/--monthly`
- `-Q/--quarterly`
- `-Y/--yearly`

More complex intervals can be specified using `-p/--period`, described below.

## Start date adjustment

When there is a report interval (other than daily), a "soft" report start date -
ie one which was not specified explicitly, but inferred, perhaps from the journal -
will be adjusted earlier if needed to start on a natural period boundary.

A "hard" report date - one specified explicitly, eg by `-b` - will not be adjusted
(since hledger 1.29).
This makes it possible to start subperiods on any date.
Note if you specify a start date, it's ideal to set it to a period boundary
(eg a monday for weekly reports, a first day of month for monthly reports..),
as this will generate simple period headings; otherwise they will be more verbose.

## End date adjustment

The report end date (even if specified explicitly) will be adjusted later
as needed to enclose a whole number of report intervals.
Eg in a `--yearly` report, all subperiods will be one year long.

## Period expressions

The `-p/--period` option specifies a period expression, which is a compact way
of expressing a start date, end date, and/or report interval.

Here's a period expression with a start and end date (specifying the first quarter of 2009):

|                                  |
|----------------------------------|
| `-p "from 2009/1/1 to 2009/4/1"` |

Several keywords like "from" and "to" are supported for readability; these are optional.
"to" can also be written as ".." or "-".
The spaces are also optional, as long as you don't run two dates together.
So the following are equivalent to the above:

|                           |
|---------------------------|
| `-p "2009/1/1 2009/4/1"`  |
| `-p2009/1/1to2009/4/1`    |
| `-p2009/1/1..2009/4/1`    |

Dates are [smart dates](#smart-dates), so if the current year is 2009,
these are also equivalent to the above:

|                         |
|-------------------------|
| `-p "1/1 4/1"`          |
| `-p "jan-apr"`          |
| `-p "this year to 4/1"` |

If you specify only one date, the missing start or end date will be the
earliest or latest transaction date in the journal:

|                      |                                   |
|----------------------|-----------------------------------|
| `-p "from 2009/1/1"` | everything after january 1, 2009  |
| `-p "since 2009/1"`  | the same, since is a synonym      |
| `-p "from 2009"`     | the same                          |
| `-p "to 2009"`       | everything before january 1, 2009 |

You can also specify a period by writing a single partial or full date:

|                 |                                                                 |
|-----------------|-----------------------------------------------------------------|
| `-p "2009"`     | the year 2009; equivalent to “2009/1/1 to 2010/1/1”             |
| `-p "2009/1"`   | the month of january 2009; equivalent to “2009/1/1 to 2009/2/1” |
| `-p "2009/1/1"` | the first day of 2009; equivalent to “2009/1/1 to 2009/1/2”     |

or by using the "Q" quarter-year syntax (case insensitive):

|                 |                                                             |
|-----------------|-------------------------------------------------------------|
| `-p "2009Q1"`   | first quarter of 2009, equivalent to “2009/1/1 to 2009/4/1” |
| `-p "q4"`       | fourth quarter of the current year                          |

### Period expressions with a report interval

A period expression can also begin with a [report interval](#report-intervals),
separated from the start/end dates (if any) by a space or the word `in`:

|                                         |
|-----------------------------------------|
| `-p "weekly from 2009/1/1 to 2009/4/1"` |
| `-p "monthly in 2008"`                  |
| `-p "quarterly"`                        |

### More complex report intervals

Some more complex intervals can be specified within period expressions, such as:

- `biweekly` (every two weeks)
- `fortnightly`
- `bimonthly` (every two months)
- `every day|week|month|quarter|year`
- `every N days|weeks|months|quarters|years`

Weekly on a custom day:

- `every Nth day of week` (`th`, `nd`, `rd`, or `st` are all accepted after the number)
- `every WEEKDAYNAME` (full or three-letter english weekday name, case insensitive)

Monthly on a custom day:

- `every Nth day [of month]` (`31st day` will be adjusted to each month's last day)
- `every Nth WEEKDAYNAME [of month]`

Yearly on a custom day:

- `every MM/DD [of year]` (month number and day of month number)
- `every MONTHNAME DDth [of year]` (full or three-letter english month name, case insensitive, and day of month number)
- `every DDth MONTHNAME [of year]` (equivalent to the above)

Examples:

|                                    |                                                          |
|------------------------------------|----------------------------------------------------------|
| `-p "bimonthly from 2008"`         |                                                          |
| `-p "every 2 weeks"`               |                                                          |
| `-p "every 5 months from 2009/03"` |                                                          |
| `-p "every 2nd day of week"`       | periods will go from Tue to Tue                          |
| `-p "every Tue"`                   | same                                                     |
| `-p "every 15th day"`              | period boundaries will be on 15th of each month          |
| `-p "every 2nd Monday"`            | period boundaries will be on second Monday of each month |
| `-p "every 11/05"`                 | yearly periods with boundaries on 5th of November        |
| `-p "every 5th November"`          | same                                                     |
| `-p "every Nov 5th"`               | same                                                     |

Show historical balances at end of the 15th day of each month (N is an end date, exclusive as always):

```cli
$ hledger balance -H -p "every 16th day"
```

Group postings from the start of wednesday to end of the following tuesday (N is both (inclusive) start date and (exclusive) end date):

```cli
$ hledger register checking -p "every 3rd day of week"
```

### Multiple weekday intervals

This special form is also supported:

- `every WEEKDAYNAME,WEEKDAYNAME,...` (full or three-letter english weekday names, case insensitive)

Also, `weekday` and `weekendday` are shorthand for `mon,tue,wed,thu,fri` and `sat,sun`.

This is mainly intended for use with `--forecast`, to generate 
[periodic transactions](#periodic-transactions) on arbitrary days of the week.
It may be less useful with `-p`, since it divides each week into subperiods  of unequal length, which is unusual.
(Related: [#1632](https://github.com/simonmichael/hledger/pull/1632))

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
Most hledger commands accept query arguments, to restrict their scope.
Multiple query terms can be provided to build up a more complex query.

- By default, a query term is interpreted as a case-insensitive substring pattern for matching [account names](#account-names):

  `car:fuel`\
  `dining groceries`\

- Patterns containing spaces or other [special characters](#special-characters) must be enclosed in single or double quotes:

  `'personal care'`\

- These patterns are actually regular expressions,
  so you can add regexp metacharacters for more precision
  (see "[Regular expressions](#regular-expressions)" above for details):

  `'^expenses\b'`\
  `'food$'`\
  `'fuel|repair'`\
  `'accounts (payable|receivable)'`\

- To match something other than account name, add one of the query type prefixes described in "Query types" below:

  `date:202312-`\
  `status:`\
  `desc:amazon`\
  `cur:USD`\
  `cur:\\$`\
  `amt:'>0'`\

- Add a `not:` prefix to negate a term:

  `not:status:'*'`\
  `not:desc:'opening|closing'`\
  `not:cur:USD`\

- Terms with different types are AND-ed, terms with the same type are OR-ed (mostly; see "Combining query terms" below).
  The following query:

  `date:2022 desc:amazon desc:amzn`
  
  is interpreted as:
  
  *date is in 2022 AND ( transaction description contains "amazon" OR "amzn" )*


## Query types

Here are the types of query term available.
Remember these can also be prefixed with **`not:`** to convert them into a negative match.

**`acct:REGEX`** or **`REGEX`**\
Match account names containing this case insensitive [regular expression]. 
This is the default query type, so we usually don't bother writing the "acct:" prefix.

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

**`expr:"TERM AND NOT (TERM OR TERM)"`** (eg)\
Match with a boolean combination of queries (which must be enclosed in quotes).
See [Combining query terms](#combining-query-terms) below.

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

When given multiple space-separated query terms, most commands select things which match:

- any of the description terms AND
- any of the account terms AND
- any of the status terms AND
- all the other terms.

The [print](#print) command is a little different, showing transactions which:

- match any of the description terms AND
- have any postings matching any of the positive account terms AND
- have no postings matching any of the negative account terms AND
- match all the other terms.

We also support more complex boolean queries with the `expr:` prefix.
This allows one to combine query terms using `and`, `or`, `not` keywords (case insensitive),
and to group them by enclosing in parentheses.

Some examples:

- Exclude account names containing 'food':

  `expr:"not food"`  (`not:food` is equivalent)

- Match things which have 'cool' in the description and the 'A' tag:

  `expr:"desc:cool and tag:A"`  (`expr:"desc:cool tag:A"` is equivalent)

- Match things which either do not reference the 'expenses:food' account, or do have the 'A' tag:

  `expr:"not expenses:food or tag:A"`

- Match things which either do not reference the 'expenses:food' account,
  or which reference the 'expenses:drink' account and also have the 'A' tag:

  `expr:"expenses:food or (expenses:drink and tag:A)"`

`expr:` has a restriction: `date:` queries may not be used inside `or` expressions.
That would allow disjoint report periods or disjoint result sets, with unclear semantics for our reports.


## Queries and command options

Some queries can also be expressed as command-line options:
`depth:2` is equivalent to `--depth 2`, 
`date:2023` is equivalent to `-p 2023`, etc.
When you mix command options and query arguments, 
generally the resulting query is their intersection.

## Queries and account aliases

When account names are [rewritten](#alias-directive) with `--alias` or `alias`,
`acct:` will match either the old or the new account name.

## Queries and valuation

When amounts are converted to other commodities in [cost](#cost-reporting) or [value](#value-reporting) reports,
`cur:` and `amt:` match the old commodity symbol and the old amount quantity, not the new ones.
(Except in hledger 1.22, [#1625](https://github.com/simonmichael/hledger/issues/1625).)

# Pivoting

Normally, hledger groups and sums amounts within each account.
The `--pivot FIELD` option substitutes some other transaction field for account names,
causing amounts to be grouped and summed by that field's value instead.
FIELD can be any of the transaction fields `acct`, `status`, `code`, `desc`, `payee`, `note`, or a tag name.
When pivoting on a tag and a posting has multiple values of that tag, only the first value is displayed.
Values containing `colon:separated:parts` will be displayed hierarchically, like account names.
Multiple, colon-delimited fields can be pivoted simultaneously, generating a hierarchical account name.

Some examples:

```journal
2016/02/16 Yearly Dues Payment
    assets:bank account                 2 EUR
    income:dues                        -2 EUR  ; member: John Doe, kind: Lifetime
```
Normal balance report showing account names:
```cli
$ hledger balance
               2 EUR  assets:bank account
              -2 EUR  income:dues
--------------------
                   0
```
Pivoted balance report, using member: tag values instead:
```cli
$ hledger balance --pivot member
               2 EUR
              -2 EUR  John Doe
--------------------
                   0
```
One way to show only amounts with a member: value (using a [query](#queries)):
```cli
$ hledger balance --pivot member tag:member=.
              -2 EUR  John Doe
--------------------
              -2 EUR
```
Another way (the acct: query matches against the pivoted "account name"):
```cli
$ hledger balance --pivot member acct:.
              -2 EUR  John Doe
--------------------
              -2 EUR
```
Hierarchical reports can be generated with multiple pivots:
```cli
$ hledger balance Income:Dues --pivot kind:member
              -2 EUR  Lifetime:John Doe
--------------------
              -2 EUR
```

# Generating data

hledger can enrich the data provided to it, or generate new data, in a number of ways.
Mostly, this is done only if you request it:

- Missing amounts or missing costs in transactions are inferred automatically when possible.
- The `--infer-equity` flag infers missing conversion equity postings from @/@@ costs.
- The `--infer-costs` flag infers missing costs from conversion equity postings.
- The `--infer-market-prices` flag infers `P` price directives from costs.
- The `--auto` flag adds extra postings to transactions matched by [auto posting rules](#auto-postings).
- The `--forecast` option generates transactions from [periodic transaction rules](#periodic-transactions).
- The `balance --budget` report infers budget goals from periodic transaction rules.
- Commands like `close`, `rewrite`, and `hledger-interest` generate transactions or postings.
- CSV data is converted to transactions by applying CSV conversion rules.. etc.

Such generated data is temporary, existing only at report time.
You can convert it to permanent recorded data by, eg, capturing the output of `hledger print` and saving it in your journal file.
This can sometimes be useful as a data entry aid.

If you are curious what data is being generated and why, run `hledger print -x --verbose-tags`.
`-x/--explicit` shows inferred amounts and `--verbose-tags` adds tags like 
`generated-transaction` (from periodic rules) and `generated-posting`, `modified` (from auto posting rules).
Similar hidden tags (with an underscore prefix) are always present, also,
so you can always match such data with queries like `tag:generated` or `tag:modified`.

# Forecasting

Forecasting, or speculative future reporting, can be useful for estimating future balances, or for exploring different future scenarios.

The simplest and most flexible way to do it with hledger is to manually record a bunch of future-dated transactions. You could keep these in a separate `future.journal` and include that with `-f` only when you want to see them.

## --forecast
There is another way: with the `--forecast` option, hledger can generate temporary "forecast transactions" for reporting purposes, according to [periodic transaction rules](#periodic-transactions) defined in the journal. 
Each rule can generate multiple recurring transactions, so by changing one rule you can change many forecasted transactions.

Forecast transactions usually start after ordinary transactions end. By default, they begin after your latest-dated ordinary transaction, or today, whichever is later, and they end six months from today. (The exact rules are a little more complicated, and are given below.)

This is the "forecast period", which need not be the same as the [report period](#report-period). 
You can override it - eg to forecast farther into the future, or to force forecast transactions to overlap your ordinary transactions - by giving the --forecast option a [period expression](#period-expressions) argument, like `--forecast=..2099` or `--forecast=2023-02-15..`. Note that the `=` is required.

## Inspecting forecast transactions

`print` is the best command for inspecting and troubleshooting forecast transactions. Eg:
```journal
~ monthly from 2022-12-20    rent
    assets:bank:checking
    expenses:rent           $1000
```
```cli
$ hledger print --forecast --today=2023/4/21
2023-05-20 rent
    ; generated-transaction: ~ monthly from 2022-12-20
    assets:bank:checking
    expenses:rent                  $1000

2023-06-20 rent
    ; generated-transaction: ~ monthly from 2022-12-20
    assets:bank:checking
    expenses:rent                  $1000

2023-07-20 rent
    ; generated-transaction: ~ monthly from 2022-12-20
    assets:bank:checking
    expenses:rent                  $1000

2023-08-20 rent
    ; generated-transaction: ~ monthly from 2022-12-20
    assets:bank:checking
    expenses:rent                  $1000

2023-09-20 rent
    ; generated-transaction: ~ monthly from 2022-12-20
    assets:bank:checking
    expenses:rent                  $1000
```

Here there are no ordinary transactions, so the forecasted transactions begin on the first occurence after today's date.
(You won't normally use `--today`; it's just to make these examples reproducible.)

## Forecast reports

Forecast transactions affect all reports, as you would expect. Eg:

```cli
$ hledger areg rent --forecast --today=2023/4/21
Transactions in expenses:rent and subaccounts:
2023-05-20 rent                 as:ba:checking               $1000         $1000
2023-06-20 rent                 as:ba:checking               $1000         $2000
2023-07-20 rent                 as:ba:checking               $1000         $3000
2023-08-20 rent                 as:ba:checking               $1000         $4000
2023-09-20 rent                 as:ba:checking               $1000         $5000
```

```cli
$ hledger bal -M expenses --forecast --today=2023/4/21
Balance changes in 2023-05-01..2023-09-30:

               ||   May    Jun    Jul    Aug    Sep 
===============++===================================
 expenses:rent || $1000  $1000  $1000  $1000  $1000 
---------------++-----------------------------------
               || $1000  $1000  $1000  $1000  $1000 
```

## Forecast tags

Forecast transactions generated by --forecast have a hidden tag, `_generated-transaction`. 
So if you ever need to match forecast transactions, you could use `tag:_generated-transaction` (or just `tag:generated`) in a query.

For troubleshooting, you can add the `--verbose-tags` flag. Then, visible `generated-transaction` tags will be added also,
so you can view them with the `print` command. Their value indicates which periodic rule was responsible.

## Forecast period, in detail

Forecast start/end dates are chosen so as to do something useful by default in almost all situations, while also being flexible. Here are (with luck) the exact rules, to help with troubleshooting:

The forecast period starts on:

- the later of
  - the start date in the periodic transaction rule
  - the start date in `--forecast`'s argument
- otherwise (if those are not available): the later of
  - the report start date specified with `-b`/`-p`/`date:`
  - the day after the latest ordinary transaction in the journal
- otherwise (if none of these are available): today.

The forecast period ends on:

- the earlier of
  - the end date in the periodic transaction rule
  - the end date in `--forecast`'s argument
- otherwise: the report end date specified with `-e`/`-p`/`date:`
- otherwise: 180 days (~6 months) from today.

## Forecast troubleshooting

When --forecast is not doing what you expect, one of these tips should help:

- Remember to use the `--forecast` option.
- Remember to have at least one periodic transaction rule in your journal. 
- Test with `print --forecast`.
- Check for typos or too-restrictive start/end dates in your periodic transaction rule.
- Leave at least 2 spaces between the rule's period expression and description fields.
- Check for future-dated ordinary transactions suppressing forecasted transactions.
- Try setting explicit report start and/or end dates with `-b`, `-e`, `-p` or `date:`
- Try adding the `-E` flag to encourage display of empty periods/zero transactions.
- Try setting explicit forecast start and/or end dates with `--forecast=START..END`
- Consult [Forecast period, in detail](#forecast-period-in-detail), above.
- Check inside the engine: add `--debug=2` (eg).

# Budgeting

With the balance command's [`--budget` report](#budget-report),
each periodic transaction rule generates recurring budget goals in specified accounts,
and goals and actual performance can be compared.
See the balance command's doc below.

You can generate budget goals and forecast transactions at the same time, from the same or different periodic transaction rules: `hledger bal -M --budget --forecast ...`

See also: [Budgeting and Forecasting](/budgeting-and-forecasting.html).

# Amount formatting

<a name="amount-display-style"></a>

## Commodity display style

For the amounts in each commodity, hledger chooses a consistent display style 
(symbol placement, decimal mark and digit group marks, number of decimal digits)
to use in most reports. This is inferred as follows:

First, if there's a [`D` directive](#d-directive) declaring a default commodity,
that commodity symbol and amount format is applied to all no-symbol amounts in the journal.

Then each commodity's display style is determined from its
[`commodity` directive](#commodity-directive). We recommend always
declaring commodities with `commodity` directives, since they help
ensure consistent display styles and precisions, and bring other
benefits such as error checking for commodity symbols.
Here's an example:

```journal
# Set display styles (and decimal marks, for parsing, if there is no decimal-mark directive)
# for the $, EUR, INR and no-symbol commodities:
commodity $1,000.00
commodity EUR 1.000,00
commodity INR 9,99,99,999.00
commodity 1 000 000.9455
```

But for convenience, if a `commodity` directive is not present,
hledger infers a commodity's display styles from its amounts as they are written in the journal
(excluding cost amounts and amounts in periodic transaction rules or auto posting rules).
It uses

- the symbol placement and decimal mark of the first amount seen
- the digit group marks of the first amount with digit group marks
- and the maximum number of decimal digits seen across all amounts.

And as fallback if no applicable amounts are found, it would use a
default style, like `$1000.00` (symbol on the left with no space,
period as decimal mark, and two decimal digits).

Finally, commodity styles can be [overridden](#commodity-styles) by
the `-c/--commodity-style` command line option.

## Rounding

Amounts are stored internally as decimal numbers with up to 255 decimal places.
They are displayed 
with their original journal precisions by print and print-like reports,
and rounded to their display precision (the number of decimal digits specified by the commodity display style)
by other reports.
When rounding, hledger uses [banker's rounding](https://en.wikipedia.org/wiki/Bankers_rounding)
(it rounds to the nearest even digit). So eg 0.5 displayed with zero decimal digits appears as "0".

## Trailing decimal marks

If you're wondering why your [`print`](#print) report sometimes shows
trailing decimal marks, with no decimal digits; it does this when
showing amounts that have digit group marks but no decimal digits,
to disambiguate them and allow them to be re-parsed reliably
(see [Decimal marks](#decimal-marks)).
Eg:

```journal
commodity $1,000.00

2023-01-02
    (a)      $1000
```

```cli
$ hledger print
2023-01-02
    (a)        $1,000.

```

If this is a problem (eg when [exporting to Ledger](/ledger.md#hledger-to-ledger)),
you can avoid it by disabling digit group marks, eg with [-c/--commodity](#commodity-styles)
(for each affected commodity):

```cli
$ hledger print -c '$1000.00'
2023-01-02
    (a)          $1000

```

or by forcing print to always show decimal digits, with [--round](#print-amount-style):

```cli
$ hledger print -c '$1,000.00' --round=soft
2023-01-02
    (a)      $1,000.00

```

## Amount parseability

More generally, hledger output falls into three rough categories, which
format amounts a little bit differently to suit different consumers:

**1. "hledger-readable output" - should be readable by hledger (and by humans)**

  - This is produced by reports that show full journal entries: `print`, `import`, `close`, `rewrite` etc.
  - It shows amounts with their original journal precisions, which may not be consistent.
  - It adds a trailing decimal mark when needed to avoid showing ambiguous amounts.
  - It can be parsed reliably (by hledger and ledger2beancount at least, but perhaps not by Ledger..)

**2. "human-readable output" - usually for humans**

  - This is produced by all other reports.
  - It shows amounts with standard display precisions, which will be consistent within each commodity.
  - It shows ambiguous amounts unmodified.
  - It can be parsed reliably in the context of a known report
    (when you know decimals are consistently not being shown, you can assume a single mark is a digit group mark).

**3. "machine-readable output" - usually for other software**

  - This is produced by all reports when an output format like `csv`, `tsv`, `json`, or `sql` is selected.
  - It shows amounts as 1 or 2 do, but without digit group marks.
  - It can be parsed reliably (if needed, the decimal mark can be changed with -c/--commodity-style).

# Cost reporting

In some transactions - for example a currency conversion, or a purchase or sale of stock - one commodity is exchanged for another.
In these transactions there is a conversion rate, also called the cost (when buying) or selling price (when selling).
In hledger docs we just say "cost", for convenience; feel free to mentally translate to "conversion rate" or "selling price" if helpful.

## Recording costs

We'll explore several ways of recording transactions involving costs.
These are also summarised at [hledger Cookbook > Cost notation](/cost-notation.md).

Costs can be recorded explicitly in the journal, using the `@ UNITCOST` or `@@ TOTALCOST` notation described in [Journal > Costs](#costs):

**Variant 1**

```journal
2022-01-01
  assets:dollars    $-135
  assets:euros       €100 @ $1.35   ; $1.35 per euro (unit cost)
```

**Variant 2**

```journal
2022-01-01
  assets:dollars    $-135
  assets:euros       €100 @@ $135   ; $135 total cost
```

Typically, writing the unit cost (variant 1) is preferable;
it can be more effort, requiring more attention to decimal digits;
but it reveals the per-unit cost basis, and makes stock sales easier.

Costs can also be left implicit, and hledger will infer the cost
that is consistent with a balanced transaction:

**Variant 3**

```journal
2022-01-01
  assets:dollars    $-135
  assets:euros       €100
```

Here, hledger will attach a `@@ €100` cost to the first amount (you can see it with `hledger print -x`).
This form looks convenient, but there are downsides:

- It sacrifices some error checking. For example, if you accidentally wrote €10
  instead of €100, hledger would not be able to detect the mistake.

- It is sensitive to the order of postings - if they were reversed, 
  a different entry would be inferred and reports would be different.

- The per-unit cost basis is not easy to read.

So generally this kind of entry is not recommended.
You can make sure you have none of these by using `-s` ([strict mode](#strict-mode)),
or by running `hledger check balanced`.

## Reporting at cost

Now when you add the `-B`/`--cost` flag to reports ("B" is from Ledger's -B/--basis/--cost flag),
any amounts which have been annotated with costs will be converted to their cost's commodity (in the report output).
Ie they will be displayed "at cost" or "at sale price".

Some things to note:

- Costs are attached to specific posting amounts in specific transactions, and once recorded they do not change.
  This contrasts with [market prices](#market-prices), which are ambient and fluctuating.

- Conversion to cost is performed before conversion to market value (described below).

## Equity conversion postings

There is a problem with the entries above - they are not conventional Double Entry Bookkeeping (DEB) notation,
and because of the "magical" transformation of one commodity into another,
they cause an imbalance in the Accounting Equation.
This shows up as a non-zero grand total in balance reports like `hledger bse`.

For most hledger users, this doesn't matter in practice and can safely be ignored !
But if you'd like to learn more, keep reading.

Conventional DEB uses an extra pair of equity postings to balance the transaction.
Of course you can do this in hledger as well:

**Variant 4**

```journal
2022-01-01
    assets:dollars      $-135
    assets:euros         €100
    equity:conversion    $135
    equity:conversion   €-100
```

Now the transaction is perfectly balanced according to standard DEB,
and `hledger bse`'s total will not be disrupted.

And, hledger can still infer the cost for cost reporting,
but it's not done by default - you must add the `--infer-costs` flag like so:

```cli
$ hledger print --infer-costs
2022-01-01 one hundred euros purchased at $1.35 each
    assets:dollars       $-135 @@ €100
    assets:euros                  €100
    equity:conversion             $135
    equity:conversion            €-100

```
```cli
$ hledger bal --infer-costs -B
               €-100  assets:dollars                                                                                                                                              
                €100  assets:euros                                                                                                                                                
--------------------                                                                                                                                                              
                   0                                                                                                                                                              
```

Here are some downsides of this kind of entry:

- The per-unit cost basis is not easy to read.

- Instead of `-B` you must remember to type `-B --infer-costs`.

- `--infer-costs` works only where hledger can identify the two equity:conversion postings
  and match them up with the two non-equity postings.
  So writing the journal entry in a particular format becomes more important. More on this below.

## Inferring equity conversion postings

Can we go in the other direction ? Yes, if you have transactions written with the @/@@ cost notation,
hledger can infer the missing equity postings, if you add the `--infer-equity` flag.
Eg:

```journal
2022-01-01
  assets:dollars  -$135
  assets:euros     €100 @ $1.35
```

```cli
$ hledger print --infer-equity
2022-01-01
    assets:dollars                    $-135
    assets:euros               €100 @ $1.35
    equity:conversion:$-€:€           €-100
    equity:conversion:$-€:$         $135.00
```

The equity account names will be "equity:conversion:A-B:A" and "equity:conversion:A-B:B"
where A is the alphabetically first commodity symbol.
You can customise the "equity:conversion" part by declaring an account with the `V`/`Conversion` [account type](#account-types).

## Combining costs and equity conversion postings

Finally, you can use both the @/@@ cost notation and equity postings at the same time.
This in theory gives the best of all worlds - preserving the accounting equation, 
revealing the per-unit cost basis, and providing more flexibility in how you write the entry:

**Variant 5**

```journal
2022-01-01 one hundred euros purchased at $1.35 each
    assets:dollars      $-135
    equity:conversion    $135
    equity:conversion   €-100
    assets:euros         €100 @ $1.35
```

All the other variants above can (usually) be rewritten to this final form with:
```cli
$ hledger print -x --infer-costs --infer-equity
```

Downsides:

- The precise format of the journal entry becomes more important.
  If hledger can't detect and match up the cost and equity postings, it will give a transaction balancing error.

- The [add](#add) command does not yet accept this kind of entry ([#2056](https://github.com/simonmichael/hledger/issues/2056)).

- This is the most verbose form.

## Requirements for detecting equity conversion postings

`--infer-costs` has certain requirements (unlike `--infer-equity`, which always works).
It will infer costs only in transactions with:

- Two non-equity postings, in different commodities.
  Their order is significant: the cost will be added to the first of them.

- Two postings to equity conversion accounts, next to one another, which balance the two non-equity postings.
  This balancing is checked to the same precision (number of decimal places) used in the conversion posting's amount.
  Equity conversion accounts are:

  - any accounts declared with account type `V`/`Conversion`, or their subaccounts
  - otherwise, accounts named `equity:conversion`, `equity:trade`, or `equity:trading`, or their subaccounts.

And multiple such four-posting groups can coexist within a single transaction.
When `--infer-costs` fails, it does not infer a cost in that transaction, and does not raise an error (ie, it infers costs where it can).

Reading variant 5 journal entries, combining cost notation and equity postings, has all the same requirements.
When reading such an entry fails, hledger raises an "unbalanced transaction" error.

## Infer cost and equity by default ?

Should `--infer-costs` and `--infer-equity` be enabled by default ?
Try using them always, eg with a shell alias:
```
alias h="hledger --infer-equity --infer-costs"
```
and let us know what problems you find.


<a name="valuation"></a>

# Value reporting

Instead of reporting amounts in their original commodity, hledger can convert them to
cost/sale amount (using the conversion rate recorded in the transaction),
and/or to market value (using some market price on a certain date).
This is controlled by the `--value=TYPE[,COMMODITY]` option, which will be described below.
We also provide the simpler `-V` and `-X COMMODITY` options, and often
one of these is all you need:

## -V: Value

The `-V/--market` flag converts amounts to market value in their
default *valuation commodity*, using the
[market prices](#p-directive) in effect on the *valuation date(s)*, if any.
More on these in a minute.

## -X: Value in specified commodity

The `-X/--exchange=COMM` option is like `-V`, except you tell it which
currency you want to convert to, and it tries to convert everything to that.

## Valuation date

Market prices can change from day to day. 
hledger will use the prices on a particular valuation date (or on more than one date).
By default hledger uses "end" dates for valuation. More specifically:

- For single period reports (including normal print and register reports):
  - If an explicit [report end date](#report-start-end-date) is specified, that is used
  - Otherwise the latest transaction date or P directive date is used (even if it's in the future)

- For [multiperiod reports](#report-intervals), each period is valued on its last day.

This can be customised with the --value option described below,
which can select either "then", "end", "now", or "custom" dates.
(Note, this has a bug in hledger-ui <=1.31: turning on valuation with
the `V` key always resets it to "end".)

## Finding market price

To convert a commodity A to its market value in another commodity B,
hledger looks for a suitable market price (exchange rate) as follows,
in this order of preference:

1. A *declared market price* or *inferred market price*:
   A's latest market price in B on or before the valuation date
   as declared by a [P directive](#p-directive), 
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
[P directives](#p-directive) in your journal.
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
you, read all of this [Value reporting](#value-reporting) section carefully,
and try adding `--debug` or `--debug=2` to troubleshoot.

`--infer-market-prices` can infer market prices from:

- multicommodity transactions with explicit prices (`@`/`@@`)

- multicommodity transactions with implicit prices (no `@`, two commodities, unbalanced).
  (With these, the order of postings matters. `hledger print -x` can be useful for troubleshooting.)

- [multicommodity transactions with equity postings](#conversion-with-equity-postings),
  if cost is inferred with [`--infer-costs`](#infer-cost-requirements).
  
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

```cli
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

- If you have [P directives](#p-directive), 
  they determine which commodities `-V` will convert, and to what.

- If you have no P directives, and use the `--infer-market-prices` flag, 
  [costs](#costs) determine it.

Amounts for which no valuation commodity can be found are not converted.

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
[market prices](#p-directive) as described above.

## Valuation examples

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
```cli
$ hledger -f t.j bal -N euros
                €100  assets:euros
```
What are they worth at end of nov 3 ?
```cli
$ hledger -f t.j bal -N euros -V -e 2016/11/4
             $110.00  assets:euros
```
What are they worth after 2016/12/21 ? (no report end date specified, defaults to today)
```cli
$ hledger -f t.j bal -N euros -V
             $103.00  assets:euros
```


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
```cli
$ hledger -f- print --cost
2000-01-01
    (a)             5 B

2000-02-01
    (a)             6 B

2000-03-01
    (a)             7 B

```

Show the value as of the last day of the report period (2000-02-29):
```cli
$ hledger -f- print --value=end date:2000/01-2000/03
2000-01-01
    (a)             2 B

2000-02-01
    (a)             2 B

```

With no report period specified, that shows the value as of the last day of the journal (2000-03-01):
```cli
$ hledger -f- print --value=end
2000-01-01
    (a)             3 B

2000-02-01
    (a)             3 B

2000-03-01
    (a)             3 B

```

Show the current value (the 2000-04-01 price is still in effect today):
```cli
$ hledger -f- print --value=now
2000-01-01
    (a)             4 B

2000-02-01
    (a)             4 B

2000-03-01
    (a)             4 B

```

Show the value on 2000/01/15:
```cli
$ hledger -f- print --value=2000-01-15
2000-01-01
    (a)             1 B

2000-02-01
    (a)             1 B

2000-03-01
    (a)             1 B

```

## Interaction of valuation and queries

When matching postings based on queries in the presence of valuation, the following happens:

1. The query is separated into two parts:
    1. the currency (`cur:`) or amount (`amt:`).
    2. all other parts.
2. The postings are matched to the currency and amount queries based on pre-valued amounts.
3. Valuation is applied to the postings.
4. The postings are matched to the other parts of the query based on post-valued amounts.

Related:
[#1625](https://github.com/simonmichael/hledger/issues/1625)


## Effect of valuation on reports

Here is a reference for how valuation is supposed to affect each part of hledger's reports.
(It's wide, you may need to scroll sideways.)
It may be useful when troubleshooting.
If you find problems, please report them, ideally with a reproducible example.
Related:
[#329](https://github.com/simonmichael/hledger/issues/329),
[#1083](https://github.com/simonmichael/hledger/issues/1083).

First, a quick glossary:

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

# PART 4: COMMANDS


<a name="commands-overview">
<a name="addons"></a><a name="add-ons"></a> <!-- see also #add-on-commands. -->

Here are the standard [commands](#commands), which you can list by running `hledger`.
If you have installed more [add-on commands](../scripts.md), they also will be listed.

<!-- keep commands & descriptions synced with Hledger.Cli.Commands.commandsList, commands.m4 -->

**[Help commands](#help-commands)**

- [help](#help)                                    - show the hledger manual with info/man/pager
- [demo](#demo)                                    - show small hledger demos in the terminal

**[User interface commands](#user-interface-commands)**

- [ui](hledger-ui.html)                            - (if installed) run hledger's terminal UI
- [web](hledger-web.html)                          - (if installed) run hledger's web UI

**[Data entry commands](#data-entry-commands)**

- [add](#add)                                      - add transactions using terminal prompts
- [import](#import)                                - add new transactions from other files, eg CSV files

**[Basic report commands](#basic-report-commands)**

- [accounts](#accounts)                            - show account names
- [codes](#codes)                                  - show transaction codes
- [commodities](#commodity-directive)              - show commodity/currency symbols
- [descriptions](#descriptions)                    - show transaction descriptions
- [files](#files)                                  - show input file paths
- [notes](#notes)                                  - show note parts of transaction descriptions
- [payees](#payees)                                - show payee parts of transaction descriptions
- [prices](#prices)                                - show market prices
- [stats](#stats)                                  - show journal statistics
- [tags](#tags-1)                                  - show tag names

**[Standard report commands](#standard-report-commands)**

- [print](#print)                                  - show transactions or export journal data
- [aregister](#aregister) (areg)                   - show transactions in a particular account
- [register](#register) (reg)                      - show postings in one or more accounts & running total
- [balancesheet](#balancesheet) (bs)               - show assets, liabilities and net worth
- [balancesheetequity](#balancesheetequity) (bse)  - show assets, liabilities and equity
- [cashflow](#cashflow) (cf)                       - show changes in liquid assets
- [incomestatement](#incomestatement) (is)         - show revenues and expenses

**[Advanced report commands](#advanced-report-commands)**

- [balance](#balance) (bal)                        - show balance changes, end balances, budgets, gains..
- [roi](#roi)                                      - show return on investments

**[Chart commands](#chart-commands)**

- [activity](#activity)                            - show bar charts of posting counts per period

**[Data generation commands](#data-generation-commands)**

- [close](#close)                                  - generate balance-zeroing/restoring transactions
- [rewrite](#rewrite)                              - generate auto postings, like print --auto

**[Maintenance commands](#maintenance-commands)**

- [check](#check)                                  - check for various kinds of error in the data
- [diff](#diff)                                    - compare account transactions in two journal files
- [test](#test)                                    - run self tests


m4_dnl XXX maybe later
m4_dnl _man_({{
m4_dnl For detailed command docs please see the appropriate man page (eg `man hledger-print`), 
m4_dnl or the info or web format of this manual.
m4_dnl }})
m4_dnl _notman_({{

Next, these commands are described in detail.

m4_dnl Include the command docs. Each starts with a level 2 heading.
m4_dnl (To change that, see Hledger/Cli/Commands/{*.md,commands.m4})
_commands_

<a name="common-tasks"></a>

# PART 5: COMMON TASKS

Here are some quick examples of how to do some basic tasks with hledger.

# Getting help

Here's how to list commands and view options and command docs:

```cli
$ hledger                # show available commands
$ hledger --help         # show common options
$ hledger CMD --help     # show CMD's options, common options and CMD's documentation
```

You can also view your hledger version's manual in several formats
by using the [help command](#help). Eg:
```cli
$ hledger help           # show the hledger manual with info, man or $PAGER (best available)
$ hledger help journal   # show the journal topic in the hledger manual
$ hledger help --help    # find out more about the help command
```

To view manuals and introductory docs on the web, visit <https://hledger.org>.
Chat and mail list support and discussion archives can be found at <https://hledger.org/support>.

# Constructing command lines

hledger has a flexible command line interface.
We strive to keep it simple and ergonomic, but if you run into one of
the sharp edges described in [OPTIONS](#options),
here are some tips that might help:

- command-specific options must go after the command (it's fine to put common options there too: `hledger CMD OPTS ARGS`)
- running add-on executables directly simplifies command line parsing (`hledger-ui OPTS ARGS`)
- enclose "problematic" args in single quotes
- if needed, also add a backslash to hide regular expression metacharacters from the shell
- to see how a misbehaving command line is being parsed, add `--debug=2`.

# Starting a journal file

hledger looks for your accounting data in a journal file, `$HOME/.hledger.journal` by default:
```cli
$ hledger stats
The hledger journal file "/Users/simon/.hledger.journal" was not found.
Please create it first, eg with "hledger add" or a text editor.
Or, specify an existing journal file with -f or LEDGER_FILE.
```

You can override this by setting the `LEDGER_FILE` environment variable (see below).
It's a good practice to keep this important file under version control,
and to start a new file each year. So you could do something like this:
```cli
$ mkdir ~/finance
$ cd ~/finance
$ git init
Initialized empty Git repository in /Users/simon/finance/.git/
$ touch 2023.journal
$ echo "export LEDGER_FILE=$HOME/finance/2023.journal" >> ~/.profile
$ source ~/.profile
$ hledger stats
Main file                : /Users/simon/finance/2023.journal
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

# Setting LEDGER_FILE

How to set `LEDGER_FILE` permanently depends on your setup:

On unix and mac, running these commands in the terminal will work for many people; adapt as needed:
```cli
$ echo 'export LEDGER_FILE=~/finance/2023.journal' >> ~/.profile
$ source ~/.profile
```

When correctly configured, in a new terminal window `env | grep LEDGER_FILE` will show your file,
and so will `hledger files`.

On mac, this additional step might be helpful for GUI applications (like Emacs started from the dock):
add an entry to `~/.MacOSX/environment.plist` like

```json
{
  "LEDGER_FILE" : "~/finance/2023.journal"
}
```
and then run `killall Dock` in a terminal window (or restart the machine).

On Windows, see <https://www.java.com/en/download/help/path.html>,
or try running these commands in a powershell window
(let us know if it persists across a reboot, and if you need to be an Administrator):
```cli
> CD
> MKDIR finance
> SETX LEDGER_FILE "C:\Users\USERNAME\finance\2023.journal"
```

# Setting opening balances

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
  2023-01-01 * opening balances
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
  ```cli
  $ hledger add
  Adding transactions to journal file /Users/simon/finance/2023.journal
  Any command line arguments will be used as defaults.
  Use tab key to complete, readline keys to edit, enter to accept defaults.
  An optional (CODE) may follow transaction dates.
  An optional ; COMMENT may follow descriptions or amounts.
  If you make a mistake, enter < at any prompt to go one step backward.
  To end a transaction, enter . when prompted.
  To quit, enter . at a date prompt or press control-d or control-c.
  Date [2023-02-07]: 2023-01-01
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
  2023-01-01 * opening balances
      assets:bank:checking                      $1000
      assets:bank:savings                       $2000
      assets:cash                                $100
      liabilities:creditcard                     $-50
      equity:opening/closing balances          $-3050
  
  Save this transaction to the journal ? [y]: 
  Saved.
  Starting the next transaction (. or ctrl-D/ctrl-C to quit)
  Date [2023-01-01]: .
  ```

If you're using version control, this could be a good time to commit the journal. Eg:
```cli
$ git commit -m 'initial balances' 2023.journal
```

# Recording transactions

As you spend or receive money, you can record these transactions
using one of the methods above (text editor, hledger add)
or by using the [hledger-iadd](#iadd) or [hledger-web](#web) add-ons,
or by using the [import command](#import) to convert CSV data downloaded from your bank.

Here are some simple transactions, see the hledger_journal(5) manual
and hledger.org for more ideas:

```journal
2023/1/10 * gift received
  assets:cash   $20
  income:gifts

2023.1.12 * farmers market
  expenses:food    $13
  assets:cash

2023-01-15 paycheck
  income:salary
  assets:bank:checking    $1000
```

# Reconciling

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
   2023-01-16 * adjust cash
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
Eg in the paycheck transaction above, insert `*` between `2023-01-15` and `paycheck`

If you're using version control, this can be another good time to commit:
```cli
$ git commit -m 'txns' 2023.journal
```

# Reporting

Here are some basic reports.

Show all transactions:
```cli
$ hledger print
2023-01-01 * opening balances
    assets:bank:checking                      $1000
    assets:bank:savings                       $2000
    assets:cash                                $100
    liabilities:creditcard                     $-50
    equity:opening/closing balances          $-3050

2023-01-10 * gift received
    assets:cash              $20
    income:gifts

2023-01-12 * farmers market
    expenses:food             $13
    assets:cash

2023-01-15 * paycheck
    income:salary
    assets:bank:checking           $1000

2023-01-16 * adjust cash
    assets:cash               $-2 = $105
    expenses:misc

```

Show account names, and their hierarchy:
```cli
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
```cli
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
```cli
$ hledger bal assets liabilities -2
               $4000  assets:bank
                $105  assets:cash
                $-50  liabilities:creditcard
--------------------
               $4055
```

Show the same thing without negative numbers, formatted as a simple balance sheet:
```cli
$ hledger bs -2
Balance Sheet 2023-01-16

                        || 2023-01-16 
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
```cli
hledger is 
Income Statement 2023-01-01-2023-01-16

               || 2023-01-01-2023-01-16 
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
```cli
$ hledger register cash
2023-01-01 opening balances     assets:cash                   $100          $100
2023-01-10 gift received        assets:cash                    $20          $120
2023-01-12 farmers market       assets:cash                   $-13          $107
2023-01-16 adjust cash          assets:cash                    $-2          $105
```

Show weekly posting counts as a bar chart:
```cli
$ hledger activity -W
2019-12-30 *****
2023-01-06 ****
2023-01-13 ****
```
# Migrating to a new file

At the end of the year, you may want to continue your journal in a new file,
so that old transactions don't slow down or clutter your reports,
and to help ensure the integrity of your accounting history.
See the [close command](#close).

If using version control, don't forget to `git add` the new file.


# BUGS

_reportbugs_

Some known issues and limitations:

The need to precede add-on command options with `--` when invoked from hledger is awkward.
(See Command options, Constructing command lines.)

A UTF-8-aware system locale must be configured to work with non-ascii data.
(See Unicode characters, Troubleshooting.)

On Microsoft Windows, depending whether you are running in a CMD window or a Cygwin/MSYS/Mintty window
and how you installed hledger,
non-ascii characters and colours may not be supported,
and the tab key may not be supported by `hledger add`.
(Running in a WSL window should resolve these.)

When processing large data files, hledger uses more memory than Ledger.

## Troubleshooting

Here are some common issues you might encounter when you run hledger,
and how to resolve them
(and remember also you can usually get quick [Support](support.md)):

**PATH issues: I get an error like "No command 'hledger' found"**\
Depending how you installed hledger, the executables may not be in your shell's PATH. 
Eg on unix systems, stack installs hledger in `~/.local/bin`
and cabal installs it in `~/.cabal/bin`.
You may need to add one of these directories to your shell's PATH,
and/or open a new terminal window.

**LEDGER_FILE issues: I configured LEDGER_FILE but hledger is not using it**\

- `LEDGER_FILE` should be a real environment variable, not just a shell variable.
  Eg on unix, the command `env | grep LEDGER_FILE` should show it.
  You may need to use `export` (see <https://stackoverflow.com/a/7411509>).
- You may need to force your shell to see the new configuration.
  A simple way is to close your terminal window and open a new one.

**LANG issues: I get errors like "Illegal byte sequence" or "Invalid or incomplete multibyte or wide character" or "commitAndReleaseBuffer: invalid argument (invalid character)"**\
Programs compiled with GHC (hledger, haskell build tools, etc.) need the system locale to be UTF-8-aware,
or they will fail when they encounter non-ascii characters.
To fix it, set the LANG environment variable to a locale which supports UTF-8
and which is installed on your system.

On unix, `locale -a` lists the installed locales.
Look for one which mentions `utf8`, `UTF-8` or similar.
Some examples: `C.UTF-8`, `en_US.utf-8`, `fr_FR.utf8`.
If necessary, use your system package manager to install one.
Then select it by setting the `LANG` environment variable.
Note, exact spelling and capitalisation of the locale name may be important:
Here's one common way to configure this permanently for your shell:

```cli
$ echo "export LANG=en_US.utf8" >>~/.profile
# close and re-open terminal window
```

If you are using Nix (not NixOS) for GHC and Hledger, you might need to set the `LOCALE_ARCHIVE` variable:
```cli
$ echo "export LOCALE_ARCHIVE=${glibcLocales}/lib/locale/locale-archive" >>~/.profile
# close and re-open terminal window
```

**COMPATIBILITY ISSUES: hledger gives an error with my Ledger file**\
Not all of Ledger's journal file syntax or feature set is supported.
See [hledger and Ledger](/ledger.md) for full details.


m4_dnl Some common markdown links.
m4_dnl These are also usable in hledger/Hledger/Cli/Commands/*.md.
m4_dnl Some are defined there also - don't remove, they are needed there for Shake cmddocs eg.
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
