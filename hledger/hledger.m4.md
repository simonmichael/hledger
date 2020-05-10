% hledger(1) hledger _version_
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

m4_dnl Show these first headings only in man pages:
_man_({{
# NAME
}})

hledger - a command-line accounting tool

_man_({{
# SYNOPSIS
}})

`hledger [-f FILE] COMMAND [OPTIONS] [ARGS]`\
`hledger [-f FILE] ADDONCMD -- [OPTIONS] [ARGS]`\
`hledger`

_man_({{
# DESCRIPTION
}})

m4_dnl Include the standard description:
_hledgerdescription_

This is hledger’s command-line interface (there are also terminal and web
interfaces). Its basic function is to read a plain text file describing
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

For more about this format, see hledger_journal(5).

Most users use a text editor to edit the journal, usually with an editor
mode such as ledger-mode for added convenience. hledger’s interactive
add command is another way to record new transactions. hledger never
changes existing transactions.

To get started, you can either save some entries like the above in
`~/.hledger.journal`, or run `hledger add` and follow the prompts. Then
try some commands like `hledger print` or `hledger balance`.
Run `hledger` with no arguments for a list of commands.

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

## Queries

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

The [print](hledger.html#print) command instead shows transactions which:

- match any of the description terms AND
- have any postings matching any of the positive account terms AND
- have no postings matching any of the negative account terms AND
- match all the other terms.

The following kinds of search terms can be used.
Remember these can also be prefixed with **`not:`**, eg to exclude a particular subaccount.

**`REGEX`, `acct:REGEX`**
: match account names by this regular expression. (With no prefix, `acct:` is assumed.)

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
PERIODEXPR is a [period expression](hledger.html#period-expressions) (with no report interval).
Examples: `date:2016`, `date:thismonth`, `date:2000/2/1-2/15`, `date:lastweek-`.
If the `--date2` command line flag is present, this matches [secondary dates](journal.html#secondary-dates) instead.

**`date2:PERIODEXPR`**
: match secondary dates within the specified period.

**`depth:N`**
: match (or display, depending on command) accounts at or above this depth

**`note:REGEX`**
: match transaction [notes](journal.html#payee-and-note)
(part of description right of `|`, or whole description when there's no `|`)

**`payee:REGEX`**
: match transaction [payee/payer names](journal.html#payee-and-note)
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

|                 |         |
|-----------------|---------|
| unescaped:      | `$`     |
| escaped:        | `\$`    |
| double-escaped: | `\\$`   |
| triple-escaped: | `\\\\$` |

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

| Reader:     | Reads:                                              | Used for file extensions:                           |
|-------------|-----------------------------------------------------|-----------------------------------------------------|
| `journal`   | hledger's journal format, also some Ledger journals | `.journal` `.j` `.hledger` `.ledger`                |
| `timeclock` | timeclock files (precise time logging)              | `.timeclock`                                        |
| `timedot`   | timedot files (approximate time logging)            | `.timedot`                                          |
| `csv`       | comma-separated values (data interchange)           | `.csv`                                              |

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
- [balance assertions](journal.html#balance-assertions) will not see any account balances from previous files

If you need those, either use the [include directive](journal.html#including-other-files),
or concatenate the files, eg: `cat a.journal b.journal | hledger -f- CMD`.

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

Some commands (print, register, the balance commands) offer a choice of output format. 
In addition to the usual plain text format (`txt`), there are
CSV (`csv`), HTML (`html`) and JSON (`json`).
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

- The JSON output from hledger commands is essentially the same as the
  JSON served by [hledger-web's JSON API](hledger-web.html#json-api),
  but pretty printed, using line breaks and indentation.
  Our pretty printer has the ability to elide data in certain cases -
  rendering non-strings as if they were strings, or displaying "FOO.."
  instead of FOO's full details. This should never happen in hledger's
  JSON output; if you see otherwise, please report as a bug.

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
#. they are [POSIX ERE][] (extended regular expressions)
#. they also support [GNU word boundaries][] (`\b`, `\B`, `\<`, `\>`)
#. they do not support [backreferences][]; if you write `\1`, it will match the digit `1`.
   Except when doing text replacement, eg in [account aliases](journal.html#regex-aliases),
   where [backreferences][] can be used in the replacement string to reference [capturing groups][] in the search regexp.
#. they do not support [mode modifiers][] (`(?s)`), character classes (`\w`, `\d`), or anything else.

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

## Smart dates

hledger's user interfaces accept a flexible "smart date" syntax (unlike dates in the journal file).
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

Most hledger reports show the full span of time represented by the journal data, by default.
So, the effective report start and end dates will be the earliest and latest transaction or posting dates found in the journal.

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

Examples:

|                   |                                                                                             |
|-------------------|---------------------------------------------------------------------------------------------|
| `-b 2016/3/17`    | begin on St. Patrick’s day 2016                                                             |
| `-e 12/1`         | end at the start of december 1st of the current year (11/30 will be the last date included) |
| `-b thismonth`    | all transactions on or after the 1st of the current month                                   |
| `-p thismonth`    | all transactions in the current month                                                       |
| `date:2016/3/17-` | the above written as queries instead                                                        |
| `date:-12/1`      |                                                                                             |
| `date:thismonth-` |                                                                                             |
| `date:thismonth`  |                                                                                             |

## Report intervals

A report interval can be specified so that commands like
[register](#register), [balance](#balance) and [activity](#activity) will divide their
reports into multiple subperiods.  The basic intervals can be
selected with one of `-D/--daily`, `-W/--weekly`, `-M/--monthly`,
`-Q/--quarterly`, or `-Y/--yearly`.  More complex intervals may be
specified with a [period expression](#period-expressions).
Report intervals can not be specified with a [query](#queries).

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

|                          |
|--------------------------|
| `-p "2009/1/1 2009/4/1"` |
| `-p2009/1/1to2009/4/1`   |
| `-p2009/1/1-2009/4/1`    |

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

|                 |                                                        |
|-----------------|--------------------------------------------------------|
| `-p "2009"`     | the year 2009; equivalent to “2009/1/1 to 2010/1/1”    |
| `-p "2009/1"`   | the month of jan; equivalent to “2009/1/1 to 2009/2/1” |
| `-p "2009/1/1"` | just that day; equivalent to “2009/1/1 to 2009/1/2”    |

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
`every <weekday>`,
`every Nth day [of month]`,
`every Nth weekday [of month]`,
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
or the full name (case insensitive) of any [tag](journal.html#tags).
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
if they have a [transaction price](journal.html#transaction-prices) specified.
This flag is equivalent to `--value=cost`, described below.

### -V: Market value

The `-V/--market` flag converts reported amounts to their market value in a default valuation commodity,
using the [market prices](journal.html#market-prices) in effect on a default valuation date.
For single period reports, the valuation date is today (equivalent to `--value=now`);
for [multiperiod reports](#report-intervals), it is the last day of each subperiod (equivalent to `--value=end`).

The default valuation commodity is the one referenced in the latest
applicable market price dated on or before the valuation date.
If most of your P declarations lead to a single home currency, this will usually be what you want.
(To specify the commodity, see -X below.)

Note that in hledger, market prices are always declared explicitly with P directives;
we do not infer them from [transaction prices](journal.html#transaction-prices) as Ledger does.

Here's a quick example of -V:

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

### -X: Market value in specified commodity

The `-X/--exchange` option is like `-V`, except it specifies the target commodity you would like to convert to.
It is equivalent to `--value=now,COMM` or `--value=end,COMM`.

### --value: Flexible valuation

*(experimental, added 201905)*

`-B`, `-V` and `-X` are special cases of the more general `--value` option:

     --value=TYPE[,COMM]  TYPE is cost, then, end, now or YYYY-MM-DD.
                          COMM is an optional commodity symbol.
                          Shows amounts converted to:
                          - cost commodity using transaction prices (then optionally to COMM using market prices at period end(s))
                          - default valuation commodity (or COMM) using market prices at posting dates
                          - default valuation commodity (or COMM) using market prices at period end(s)
                          - default valuation commodity (or COMM) using current market prices
                          - default valuation commodity (or COMM) using market prices at some date

The TYPE part basically selects either "cost", or "market value" plus a valuation date:

`--value=cost`
: Convert amounts to cost, using the prices recorded in transactions.

`--value=then`
: Convert amounts to their value in a default valuation commodity, using market prices
  on each posting's date. This is currently supported only by the 
  [print](#print) and [register](#register) commands.

`--value=end`
: Convert amounts to their value in a default valuation commodity, using market prices
  on the last day of the report period (or if unspecified, the journal's end date);
  or in multiperiod reports, market prices on the last day of each subperiod.

`--value=now`
: Convert amounts to their value in default valuation commodity using current market prices
  (as of when report is generated).

`--value=YYYY-MM-DD`
: Convert amounts to their value in default valuation commodity using market prices
  on this date.

The default valuation commodity is the commodity mentioned in the most
recent applicable market price declaration. When all your price
declarations lead to a single home currency, this will usually do what
you want.

To select a different valuation commodity, add the optional `,COMM` part:
a comma, then the target commodity's symbol. Eg: **`--value=now,EUR`**.
hledger will do its best to convert amounts to this commodity, using:

- declared prices (from source commodity to valuation commodity)
- reverse prices (declared prices from valuation to source commodity, inverted)
- indirect prices (prices calculated from the shortest chain of declared or reverse prices from source to valuation commodity)

in that order.

Here are some examples showing the effect of `--value` as seen with `print`:

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
```
P 2000-01-01 A 2B

2000-01-01
  a  1B
  b
```
```
$ hledger print -x -X A
2000-01-01
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
2000-01-01
    a           0.50A
    b          -0.50A

```

### Effect of --value on reports

Here is a reference for how `--value` currently affects each part of hledger's reports.
It's work in progress, but may be useful for troubleshooting or reporting bugs.
See also the definitions and notes below.
If you find problems, please report them, ideally with a reproducible example.
Related:
[#329](https://github.com/simonmichael/hledger/issues/329),
[#1083](https://github.com/simonmichael/hledger/issues/1083).

| Report type                                     | `-B`, `--value=cost`                          | `-V`, `-X`                                       | `--value=then`                                        | `--value=end`                                      | `--value=DATE`, `--value=now`           |
|-------------------------------------------------|-----------------------------------------------|--------------------------------------------------|-------------------------------------------------------|----------------------------------------------------|-----------------------------------------|
| **print**                                       |                                               |                                                  |                                                       |                                                    |                                         |
| posting amounts                                 | cost                                          | value at report end or today                     | value at posting date                                 | value at report or journal end                     | value at DATE/today                     |
| balance assertions / assignments                | unchanged                                     | unchanged                                        | unchanged                                             | unchanged                                          | unchanged                               |
| <br>                                            |                                               |                                                  |                                                       |                                                    |                                         |
| **register**                                    |                                               |                                                  |                                                       |                                                    |                                         |
| starting balance (with -H)                      | cost                                          | value at day before report or journal start      | not supported                                         | value at day before report or journal start        | value at DATE/today                     |
| posting amounts (no report interval)            | cost                                          | value at report end or today                     | value at posting date                                 | value at report or journal end                     | value at DATE/today                     |
| summary posting amounts (with report interval)  | summarised cost                               | value at period ends                             | sum of postings in interval, valued at interval start | value at period ends                               | value at DATE/today                     |
| running total/average                           | sum/average of displayed values               | sum/average of displayed values                  | sum/average of displayed values                       | sum/average of displayed values                    | sum/average of displayed values         |
| <br>                                            |                                               |                                                  |                                                       |                                                    |                                         |
| **balance (bs, bse, cf, is..)**                 |                                               |                                                  |                                                       |                                                    |                                         |
| balances (no report interval)                   | sums of costs                                 | value at report end or today of sums of postings | not supported                                         | value at report or journal end of sums of postings | value at DATE/today of sums of postings |
| balances (with report interval)                 | sums of costs                                 | value at period ends of sums of postings         | not supported                                         | value at period ends of sums of postings           | value at DATE/today of sums of postings |
| starting balances (with report interval and -H) | sums of costs of postings before report start | sums of postings before report start             | not supported                                         | sums of postings before report start               | sums of postings before report start    |
| budget amounts with --budget                    | like balances                                 | like balances                                    | not supported                                         | like balances                                      | like balances                           |
| grand total (no report interval)                | sum of displayed values                       | sum of displayed values                          | not supported                                         | sum of displayed values                            | sum of displayed values                 |
| row totals/averages (with report interval)      | sums/averages of displayed values             | sums/averages of displayed values                | not supported                                         | sums/averages of displayed values                  | sums/averages of displayed values       |
| column totals                                   | sums of displayed values                      | sums of displayed values                         | not supported                                         | sums of displayed values                           | sums of displayed values                |
| grand total/average                             | sum/average of column totals                  | sum/average of column totals                     | not supported                                         | sum/average of column totals                       | sum/average of column totals            |
| <br>                                            |                                               |                                                  |                                                       |                                                    |                                         |

**Additional notes**

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


### Combining -B, -V, -X, --value

The rightmost of these flags wins.

# COMMANDS

hledger provides a number of subcommands; `hledger` with no arguments
shows a list.

If you install additional `hledger-*` packages, or if you put programs
or scripts named `hledger-NAME` in your PATH, these will also be
listed as subcommands.

Run a subcommand by writing its name as first argument (eg `hledger
incomestatement`). You can also write one of the standard short aliases
displayed in parentheses in the command list (`hledger b`), or any
any unambiguous prefix of a command name (`hledger inc`).

Here are all the builtin commands in alphabetical order.
See also `hledger` for a more organised command list,
and `hledger CMD -h` for detailed command help.

## accounts

_include_(Hledger/Cli/Commands/Accounts.md)

## activity

_include_(Hledger/Cli/Commands/Activity.md)

## add

_include_(Hledger/Cli/Commands/Add.md)

## balance

_include_({{Hledger/Cli/Commands/Balance.md}})

## balancesheet

_include_({{Hledger/Cli/Commands/Balancesheet.md}})

## balancesheetequity

_include_({{Hledger/Cli/Commands/Balancesheetequity.md}})

## cashflow

_include_({{Hledger/Cli/Commands/Cashflow.md}})

## check-dates

_include_({{Hledger/Cli/Commands/Checkdates.md}})

## check-dupes

_include_({{Hledger/Cli/Commands/Checkdupes.md}})

## close

_include_({{Hledger/Cli/Commands/Close.md}})

## commodities

_include_({{Hledger/Cli/Commands/Commodities.md}})

## descriptions

_include_({{Hledger/Cli/Commands/Descriptions.md}})

## diff

_include_({{Hledger/Cli/Commands/Diff.md}})

## files

_include_({{Hledger/Cli/Commands/Files.md}})

## help

_include_({{Hledger/Cli/Commands/Help.md}})

## import

_include_({{Hledger/Cli/Commands/Import.md}})

## incomestatement

_include_({{Hledger/Cli/Commands/Incomestatement.md}})

## notes

_include_({{Hledger/Cli/Commands/Notes.md}})

## payees

_include_({{Hledger/Cli/Commands/Payees.md}})

## prices

_include_({{Hledger/Cli/Commands/Prices.md}})

## print

_include_({{Hledger/Cli/Commands/Print.md}})

## print-unique

_include_({{Hledger/Cli/Commands/Printunique.md}})

## register

_include_({{Hledger/Cli/Commands/Register.md}})

## register-match

_include_({{Hledger/Cli/Commands/Registermatch.md}})

## rewrite

_include_({{Hledger/Cli/Commands/Rewrite.md}})

## roi

_include_({{Hledger/Cli/Commands/Roi.md}})

## stats

_include_({{Hledger/Cli/Commands/Stats.md}})

## tags

_include_({{Hledger/Cli/Commands/Tags.md}})

## test

_include_({{Hledger/Cli/Commands/Test.md}})


## Add-on commands

hledger also searches for external add-on commands, and will include these in the commands list.
These are programs or scripts in your PATH whose name starts with `hledger-`
and ends with a recognised file extension
(currently: no extension, `bat`,`com`,`exe`, `hs`,`lhs`,`pl`,`py`,`rb`,`rkt`,`sh`).

Add-ons can be invoked like any hledger command, but there are a few things to be aware of.
Eg if the `hledger-web` add-on is installed,

- `hledger -h web` shows hledger's help, while `hledger web -h` shows hledger-web's help.

- Flags specific to the add-on must have a preceding `--` to hide them from hledger.
  So `hledger web --serve --port 9000` will be rejected; you must use `hledger web -- --serve --port 9000`.

- You can always run add-ons directly if preferred: `hledger-web --serve --port 9000`.

Add-ons are a relatively easy way to add local features or experiment with new ideas.
They can be written in any language, but haskell scripts have a big advantage:
they can use the same hledger (and haskell) library functions that built-in commands do,
for command-line options, journal parsing, reporting, etc.

Two important add-ons are the hledger-ui and hledger-web user interfaces.
These are maintained and released along with hledger:

### ui
[hledger-ui](hledger-ui.html) provides an efficient terminal interface.

### web
[hledger-web](hledger-web.html) provides a simple web interface.

Third party add-ons, maintained separately from hledger, include:

### iadd

[hledger-iadd](http://hackage.haskell.org/package/hledger-iadd)
is a more interactive, terminal UI replacement for the [add command](hledger.html#add).

### interest

[hledger-interest](http://hackage.haskell.org/package/hledger-interest)
generates interest transactions for an account according to various schemes.

<!-- ### autosync -->

<!-- [hledger-autosync](https://github.com/simonmichael/hledger/blob/master/bin/hledger-autosync)  -->
<!-- is a symbolic link for easily running  -->
<!-- [ledger-autosync](https://pypi.python.org/pypi/ledger-autosync) if you make a symbolic -->
<!-- ledger-autosync does deduplicating conversion of OFX data and some CSV formats, -->
<!-- and can also download the data  -->
<!-- [if your bank offers OFX Direct Connect](http://wiki.gnucash.org/wiki/OFX_Direct_Connect_Bank_Settings).  -->

A few more experimental or old add-ons can be found in hledger's bin/
directory. These are typically prototypes and not guaranteed to work.

# ENVIRONMENT

**COLUMNS**
The screen width used by the register command.
Default: the full terminal width.

m4_dnl Standard LEDGER_FILE description:
_LEDGER_FILE_

# FILES

m4_dnl Standard input files description:
Reads _files_

# LIMITATIONS

The need to precede addon command options with `--` when invoked from hledger is awkward.

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
