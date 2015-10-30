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
balances and transactions. It is simpler and more convenient for
browsing than the command-line interface, but lighter and faster than
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

# KEYS

Generally the cursor keys navigate; `right` (or `enter`) goes deeper, `left` returns to the previous screen,
`up`/`down`/`page up`/`page down`/`home`/`end` move up and down through lists.

`g` gets the latest data and reloads the screen (and any previous screens). There may be a noticeable pause.

`q` quits the application.

Some screens have additional key bindings, described below.

# SCREENS

## Accounts screen

This is normally the first screen displayed.
It lists accounts and their balances, like hledger's balance command.
By default, it shows all accounts and their latest ending balances.
if you specify a query on the command line, it shows just the matched accounts and the balances from matched transactions.

When not in flat mode, indentation indicates the account hierarchy. `F` toggles flat mode on and off.

By default, all subaccounts are displayed.
To see less detail, set a depth limit by pressing a number key, `1` to `9`.
Or, adjust the depth limit by pressing `-` or `+` (`=` also works).
`0` removes the depth limit.

`C` toggles cleared mode. In cleared mode, the accounts and balances
are derived only from transactions which are marked cleared (*).

Press `right` or `enter` to view an account's transactions register.

## Register screen

This screen lists all transactions affecting a particular account (like a check register).
In cleared mode (press `C`) it lists only transactions which are marked cleared.
It does not otherwise filter by query.

Note this screen shows transactions, not postings (unlike hledger's
register command). This means:

- Each line represents a whole transaction.

- For each transaction, it shows the other account(s) involved, in
  abbreviated form. (If there are both real and virtual postings, it
  shows only the accounts affected by real postings.)

- It shows the overall change to the current account's balance from
  each transaction; positive for an inflow to this account, negative
  for an outflow.

- When no query other than a date limit is in effect, it shows the
  current account's historic balance as of the transaction date.
  Otherwise it shows a running total starting from zero.  Eg, these
  will show historic balances:

    ```
    $ hledger-ui
    $ hledger-ui --begin 'this month'
    $ hledger-ui --register checking date:2015/10
    ```

    while these will show a running total, since the queries are not just date limits:

    ```
    $ hledger-ui checking
    $ hledger-ui --begin 'this month' desc:market
    $ hledger-ui --register checking --cleared
    ```

Press `right` or `enter` to view the selected transaction in full detail.

## Transaction screen

This screen shows a single transaction, as a general journal entry,
similar to hledger's print command and journal format (hledger_journal(5)).

The transaction's date(s) and any cleared flag, transaction code,
description, comments, along with all of its account postings are
shown.  Simple transactions have two postings, but there can be more
(or in certain cases, fewer).

`up` and `down` will step through all transactions listed in the
previous account register screen.  In the title bar, the numbers in
parentheses show your position within that account register. They will
vary depending on which account register you came from (remember most
transactions appear in multiple account registers). The #N number
preceding them is the transaction's position within the complete
unfiltered journal, which is a more stable id (at least until the next
reload).

## Error screen

This screen will appear if there is a problem, such as a parse error,
when you press g to reload. Once you have fixed the problem described,
press g again to reload and restore normal operation.

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

`-V` affects only the accounts screen.

When you press `g`, the current and all previous screens are
regenerated, which may cause a noticeable pause. Also there is no
visual indication that this is in progress.

The register screen's switching between historic balance and running
total based on query arguments may be confusing, and there is no
column heading to indicate which is being displayed.
