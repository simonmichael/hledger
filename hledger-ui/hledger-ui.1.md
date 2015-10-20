% hledger-ui(1)
%
% October 2015

# NAME

hledger-ui - curses-style interface for the hledger accounting tool

# SYNOPSIS

`hledger-ui [OPTIONS] [QUERYARGS]`\
`hledger ui -- [OPTIONS] [QUERYARGS]`

# DESCRIPTION

hledger is a cross-platform program for tracking money, time, or any other commodity,
using double-entry accounting and a simple, editable file format.
hledger is inspired by and largely compatible with ledger(1).

hledger-ui is hledger's curses-style interface.
It reads a hledger journal file (~/.hledger.journal, $LEDGER_FILE, or -f FILE) and
provides a simple full-screen console interface for viewing account
balances and transactions. It is simpler and more efficient for
browsing than the hledger CLI, but lighter and faster than
hledger-web.

The journal file is `~/.hledger.journal`, `$LEDGER_FILE`, or another file specified with -f.
For more about the format, see hledger(1) or hledger_journal(5).

# OPTIONS

Note: if invoking hledger-ui as a hledger subcommand, write `--` before options as shown above.

Any QUERYARGS are interpreted as a hledger search query which filters the data.

`--flat`
: show full account names, unindented

`--no-elide`
: don't compress empty parent accounts on one line

`--register=ACCTREGEX`
: start in the (first) matched account's register screen

`--theme=default|terminal|greenterm`
: use this custom display theme

`-V --value`
: show amounts as their current market value in their default valuation commodity
(accounts screen only)

`-h --help`
: show help

`--version`
: show version information

## hledger options:

The following common hledger options should also work:

`-f FILE --file=FILE`
: use a different input file. For stdin, use -

`--rules-file=RULESFILE`
: Conversion rules file to use when reading CSV (default: FILE.rules)

`--alias=OLD=NEW`
: display accounts named OLD as NEW

`--ignore-assertions`
: ignore any failing balance assertions in the journal

`--debug=N`
: show debug output if N is 1-9 (default: 0)

`-b --begin=DATE`
: include postings/txns on or after this date

`-e --end=DATE`
: include postings/txns before this date

`-p --period=PERIODEXP`
: set start date, end date, and/or reporting interval all at once (overrides the flags above)

`--date2 --aux-date`
: use postings/txns' secondary dates instead

`-C --cleared`
: include only cleared postings/txns

`--pending`
: include only pending postings/txns

`-U --uncleared`
: include only uncleared (and pending) postings/txns

`-R --real`
: include only non-virtual postings

`--depth=N`
: hide accounts/postings deeper than N

`-E --empty`
: show empty/zero things which are normally omitted

`-B --cost`
: show amounts in their cost price's commodity

# ENVIRONMENT

**LEDGER_FILE**
sets the default journal file path. If not set, it is `~/.hledger.journal`.

**COLUMNS**
sets the screen width to use (normally the full terminal width).

# FILES

Reads data from a hledger journal file (`$LEDGER_FILE` or
`~/.hledger.journal` by default), or a CSV file plus associated CSV
rules file.

# BUGS

The need to precede options with `--` when invoked from hledger is awkward.

`-f-` doesn't work (hledger-ui can't read from stdin).

`-V` doesn't affect the register screen.

If you reload while in the register screen, when you return to the
accounts screen it will be showing old data, and pressing g again will
not reload it; you must adjust depth to force it (eg press 0).
