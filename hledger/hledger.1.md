% hledger(1) hledger 0.26.98
%
% October 2015

<div class="manonly">

# NAME

hledger - a command-line accounting tool

# SYNOPSIS

`hledger [-f FILE] COMMAND [OPTIONS] [CMDARGS]`\
`hledger [-f FILE] ADDONCMD -- [OPTIONS] [CMDARGS]`

# DESCRIPTION

hledger is a cross-platform program for tracking money, time, or any
other commodity, using double-entry accounting and a simple, editable
file format. It is inspired by and largely compatible with ledger(1).
hledger aims to be a reliable, practical tool for daily use. This man
page is a quick reference and introduction; for more complete docs, see
http://hledger.org/manual.

</div>
<div class="webonly">
* toc
</div>

This is hledger’s command-line interface (there are also curses and web
interfaces). Its basic function is to read a plain text file describing
financial transactions (in accounting terms, a general journal) and
print useful reports on standard output, or export them as CSV. hledger
can also read CSV files, converting them semi-automatically to journal
format. Additionally, hledger lists other hledger-\* executables found
in the user’s \$PATH and can invoke them as subcommands.

The journal file is `~/.hledger.journal` by default, or another file path
specified by `$LEDGER_FILE`.
(This should be a real environment variable, not a shell variable.)
You can also specify a file with `-f FILE`,
or standard input with `-f-`.

Transactions are dated movements of money between two (or more) named
accounts, and are recorded with journal entries like this:

```journal
2015/10/16 bought food
 expenses:food          $10
 assets:cash
```

For more about the format, see hledger_journal(5).

Most users use a text editor to edit the journal, usually with an editor
mode such as ledger-mode for added convenience. hledger’s interactive
add command is another way to record new transactions. hledger never
changes existing transactions.

To get started, you can either save some entries like the above in
`~/.hledger.journal`, or run `hledger add` and follow the prompts. Then
try some commands like `hledger print` or `hledger balance`.
See COMMANDS and EXAMPLES below.

# OPTIONS

To see general help and the command list: `hledger --help` or `hledger`

To see all options available with a command: `hledger COMMAND --help`

Except for the General options below, options must be written after
COMMAND, not before it.

Also, when invoking external add-on commands, their options must be
written after a double hyphen. (Or, you can invoke the external command
directly.) Eg:
```{.shell .bold}
$ hledger ui -- --register cash
$ hledger-ui --register cash
```

Options and command arguments can be intermixed. Arguments are usually
interpreted as a search query which filters the data, see QUERIES.

## General flags:

These can appear anywhere in the command line.

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

## Common reporting flags:

These are supported by most commands, where applicable.
They must be written after the command name.
Additional command-specific flags are described in COMMANDS below.

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

If a reporting option is repeated, the last one takes precedence. Eg -p jan -p
feb is equivalent to -p feb.

# COMMANDS

COMMAND selects one of hledger's subcommands; omit it to list available commands.
To save typing, some commands have a short form; any unique prefix also works.

Here is a summary (see http://hledger.org/manual#commands for the full command help):

## Data entry:

### add
prompt for transactions and add them to the journal.

This is the only hledger command that writes to the journal file.
It appends only, existing transactions are not changed.

`--no-new-accounts`
: don't allow creating new accounts; helps prevent typos when entering account names

## Reporting:

### accounts
show account names

`--tree`
: show short account names, as a tree

`--flat`
: show full account names, as a list (default)

`--drop=N`
: in flat mode: omit N leading account name parts

### activity
show an ascii barchart of posting counts per interval
(default: daily)

### balance, bal
show accounts and balances

`--tree`
: show short account names, as a tree

`--flat`
: show full account names, as a list (default)

`--drop=N`
: in flat mode: omit N leading account name parts

`--format=LINEFORMAT`
: in single-column balance reports: use this custom line format

`--no-elide`
: in tree mode: don't squash boring parent accounts

`-H --historical`
: in multicolumn mode: show historical ending balances

`--cumulative`
: in multicolumn mode: show accumulated ending balances

`-A --average`
: in multicolumn mode: show a row average column

`-T --row-total`
: in multicolumn mode: show a row total column

`-N --no-total`
: don't show the final total row

`-V --value`
: show amounts as their current market value in their default valuation commodity

`-o FILE[.FMT] --output-file=FILE[.FMT]`
: write output to FILE instead of stdout. A recognised FMT suffix influences the format.

`-O FMT --output-format=FMT     `
: select the output format. Supported formats:
txt, csv.

### balancesheet, bs
show a balance sheet

`--flat`
: show full account names, as a list (default)

`--drop=N`
: in flat mode: omit N leading account name parts

### cashflow, cf
show a cashflow statement

`--flat`
: show full account names, as a list (default)

`--drop=N`
: in flat mode: omit N leading account name parts

### incomestatement, is
show an income statement

`--flat`
: show full account names, as a list (default)

`--drop=N`
: in flat mode: omit N leading account name parts

### print
show transactions from the journal

`-m STR --match=STR             `
: show the transaction whose description is most similar to STR, and is most recent

`-o FILE[.FMT] --output-file=FILE[.FMT]`
: write output to FILE instead of stdout. A recognised FMT suffix influences the format.

`-O FMT --output-format=FMT     `
: select the output format. Supported formats:
txt, csv.

### register, reg
show postings and running total

`-H --historical`
: include prior postings in the running total

`-A --average`
: show a running average instead of the running total (implies --empty)

`-r --related`
: show postings' siblings instead

`-w N --width=N`
: set output width (default: terminal width or COLUMNS. -wN,M sets description width as well)

`-o FILE[.FMT] --output-file=FILE[.FMT]`
: write output to FILE instead of stdout. A recognised FMT suffix influences the format.

`-O FMT --output-format=FMT     `
: select the output format. Supported formats:
txt, csv.

### stats
show some journal statistics

`-o FILE[.FMT] --output-file=FILE[.FMT]`
: write output to FILE instead of stdout. A recognised FMT suffix influences the format.

## Add-on commands:

Additional commands will be available when executables or scripts
named "`hledger-`CMD" are installed in the PATH. These are often
provided by a package of the same name, or you can make your own custom scripts
(haskell scripts can use hledger-lib allowing tight integration).
Some available add-ons are:

### autosync
download OFX bank data and/or convert OFX to hledger journal format

### diff
show transactions present in one journal file but not another

### interest
generate interest transactions

### irr
calculate internal rate of return

### ui
curses-style interface, see hledger-ui(1)

### web
web interface, see hledger-web(1)

# QUERIES

Most commands accept an optional query expression, written as arguments after the command name,
to filter the data by date, account name or other criteria.
The syntax is similar to a web search:
one or more space-separated search terms,
quotes to enclose whitespace,
optional prefixes to match specific fields.
Multiple search terms are combined as follows:

All commands except print: show transactions/postings/accounts which match (or negatively match)

- any of the description terms AND
- any of the account terms AND
- all the other terms.

The print command: show transactions which

- match any of the description terms AND
- have any postings matching any of the positive account terms AND
- have no postings matching any of the negative account terms AND
- match all the other terms.

The following kinds of search terms can be used:


**`REGEX`**
: match account names by this regular expression


**`acct:REGEX`**
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
: match transaction descriptions


**`date:PERIODEXPR`**
: match dates within the specified period (which should not include a
reporting interval


**`date2:PERIODEXPR`**
: as above, but match secondary dates


**`depth:N`**
: match (or display, depending on command) accounts at or above this depth


**`real:, real:0`**
: match real or virtual postings respectively


**`status:*, status:!, status:`**
: match cleared, pending, or uncleared/pending transactions respectively


**`tag:REGEX[=REGEX]`**
: match by tag name, and optionally also by tag value.  Note a
tag: query is considered to match a transaction if it matches any of
the postings.  Also remember that postings inherit the tags of their
parent transaction.


**`not:`**
: before any of the above negates the match.

# EXAMPLES

Two simple transactions in hledger journal format:

```journal
2015/9/30 gift received
  assets:cash   $20
  income:gifts

2015/10/16 farmers market
  expenses:food    $10
  assets:cash
```

Some basic reports:

```shell
$ hledger print
2015/09/30 gift received
    assets:cash            $20
    income:gifts          $-20

2015/10/16 farmers market
    expenses:food           $10
    assets:cash            $-10
```
```shell
$ hledger accounts --tree
assets
  cash
expenses
  food
income
  gifts
```
```shell
$ hledger balance
                 $10  assets:cash
                 $10  expenses:food
                $-20  income:gifts
--------------------
                   0
```
```shell
$ hledger register cash
2015/09/30 gift received   assets:cash               $20           $20
2015/10/16 farmers market  assets:cash              $-10           $10
```

```{.shell .bold}
$ hledger                                 # show available commands
$ hledger add                             # add more transactions to the journal file
$ hledger balance                         # all accounts with aggregated balances
$ hledger balance --help                  # show help for balance command
$ hledger balance --depth 1               # only top-level accounts
$ hledger register                        # show account postings, with running total
$ hledger reg income                      # show postings to/from income accounts
$ hledger reg 'assets:some bank:checking' # show postings to/from this checking account
$ hledger print desc:shop                 # show transactions with shop in the description
$ hledger activity -W                     # show transaction counts per week as a bar chart
```

<div class="manonly">

# ENVIRONMENT

**LEDGER_FILE**
sets the default journal file path. If not set, it is `~/.hledger.journal`.

**COLUMNS**
sets the default width used by the register command (normally the full terminal width).

# FILES

Reads data from a hledger journal file (`$LEDGER_FILE` or
`~/.hledger.journal` by default), or a CSV file plus associated CSV
rules file.

# BUGS

The need to precede options with `--` when invoked from hledger is awkward.

hledger can't render non-ascii characters when run from a Windows command prompt (up to Windows 7 at least).

When input data contains non-ascii characters, a suitable system locale must be configured (or there will be an unhelpful error).
Eg on POSIX, set LANG to something other than C.

# REPORTING BUGS

Report bugs at http://bugs.hledger.org.

</div>
