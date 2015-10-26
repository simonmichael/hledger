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

# SCREENS

Currently there are two:

## Accounts screen

This is the screen shown at startup by default.
It shows a scrollable list of accounts and their balances - all accounts, or just the matched accounts if you specified a query on the command line.
`f` toggles flat mode on and off.
You can limit the depth of accounts displayed, to reduce detail, by pressing `-` to reduce it.
`+` (or `=`) increases the depth limit again.
Or, press a number key to set a specific depth limit, eg `1` to see just top level accounts.
Use the cursor keys to move up or down, and cursor right (or enter) to view an account's transaction register.

## Register screen

This screen shows a register of transactions affecting a particular account -
all transactions, or just the matched ones if there was a query on the command line.

You can reach the register screen by pressing cursor right or enter on
the accounts screen, or jump directly to it at startup by specifying
an account with `--register ACCTREGEX` on the command line.
The cursor left key returns to the accounts screen.

The register screen shows transactions (like the register in
hledger-web, and other accounting systems), rather than postings
(like hledger's register command). This means:

- It shows transactions affecting a selected current account, rather
  than postings matching a pattern. Each line represents a whole transaction.

- It lists the other account(s) involved in the transaction, in
  abbreviated form. (As an exception, if both real and virtual
  postings are involved, only the accounts affected by real postings
  are listed.)

- The amount field shows the overall effect of the transaction on the
  current account; positive for an inflow to this account, negative
  for an outflow.

- (Not implemented yet: the balance field should usually show the
  current account's historic balance as of the transaction date, even
  if you have adjusted the report start date. Currently it always
  shows the running total).

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
