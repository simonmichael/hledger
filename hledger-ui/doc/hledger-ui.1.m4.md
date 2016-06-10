% hledger-ui(1) hledger-ui _version_
% _author_
% _monthyear_

_web_({{
_versions_({{hledger-ui}})
_toc_
<style>
.highslide img {max-width:250px; float:right; margin:0 0 1em 1em;}
.highslide-caption {color:white; background-color:black;}
</style>
<a href="images/hledger-ui/hledger-ui-sample-acc2.png" class="highslide" onclick="return hs.expand(this)"><img src="images/hledger-ui/hledger-ui-sample-acc2.png" title="Accounts screen with query and depth limit" /></a>
<a href="images/hledger-ui/hledger-ui-sample-acc.png" class="highslide" onclick="return hs.expand(this)"><img src="images/hledger-ui/hledger-ui-sample-acc.png" title="Accounts screen" /></a>
<a href="images/hledger-ui/hledger-ui-sample-acc-greenterm.png" class="highslide" onclick="return hs.expand(this)"><img src="images/hledger-ui/hledger-ui-sample-acc-greenterm.png" title="Accounts screen with greenterm theme" /></a>
<a href="images/hledger-ui/hledger-ui-sample-txn.png" class="highslide" onclick="return hs.expand(this)"><img src="images/hledger-ui/hledger-ui-sample-txn.png" title="Transaction screen" /></a>
<a href="images/hledger-ui/hledger-ui-sample-reg.png" class="highslide" onclick="return hs.expand(this)"><img src="images/hledger-ui/hledger-ui-sample-reg.png" title="Register screen" /></a>
<!-- <br clear=all> -->
<a href="images/hledger-ui/hledger-ui-bcexample-acc.png" class="highslide" onclick="return hs.expand(this)"><img src="images/hledger-ui/hledger-ui-bcexample-acc.png" title="beancount example accounts" /></a>
<a href="images/hledger-ui/hledger-ui-bcexample-acc-etrade-cash.png" class="highslide" onclick="return hs.expand(this)"><img src="images/hledger-ui/hledger-ui-bcexample-acc-etrade-cash.png" title="beancount example's etrade cash subaccount" /></a>
<a href="images/hledger-ui/hledger-ui-bcexample-acc-etrade.png" class="highslide" onclick="return hs.expand(this)"><img src="images/hledger-ui/hledger-ui-bcexample-acc-etrade.png" title="beancount example's etrade investments, all commoditiess" /></a>
}})

_man_({{
# NAME

hledger-ui - curses-style interface for the hledger accounting tool

# SYNOPSIS

`hledger-ui [OPTIONS] [QUERYARGS]`\
`hledger ui -- [OPTIONS] [QUERYARGS]`

# DESCRIPTION

_hledgerdescription_
}})

hledger-ui is hledger's curses-style interface, providing an efficient full-window text UI
for viewing accounts and transactions, and some limited data entry capability.
It is easier than hledger's command-line interface, and 
sometimes quicker and more convenient than the web interface.

Like hledger, it reads _files_
For more about this see hledger(1), hledger_journal(5) etc.

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

hledger general options:

_generaloptions_

hledger reporting options:

_reportingoptions_

# KEYS

`h` shows a help dialog listing all keys. 
(Some but not all of these also appear in the quick help at the bottom of each screen.) 
Press `h` again (or `ESCAPE`) to close it.

The cursor keys navigate: 
`right` (or `enter`) goes deeper, 
`left` returns to the previous screen,
`up`/`down`/`page up`/`page down`/`home`/`end` move up and down through lists.

`/` lets you set or change the [filter query](/hledger.html#queries),
which limits the data shown on most screens (in addition to the quick
filters described below). While editing the query you can use typical
command-line edit keys (ctrl-a/e/k), press enter to set the new filter,
or press escape to cancel.

`BACKSPACE` or `DELETE` clears any filters in effect.

`ESCAPE` removes any filters currently in effect, and jumps to the top screen.
Or, it cancels a minibuffer edit or help dialog if one is active.

`g` reloads from the data file(s) and updates the current screen and any
previous screens. (With large files, there can be a noticeable pause.)

`a` runs command-line hledger's add command, and reloads the updated file.
This allows some basic data entry. 

`q` quits the application.

Additional screen-specific keys are described below.

# SCREENS

## Accounts screen

This is normally the first screen displayed.
It lists accounts and their balances, like hledger's balance command.
By default, it shows all accounts and their latest ending balances.
if you specify a query on the command line, it shows just the matched accounts and the balances from matched transactions.

When not in flat mode, indentation indicates the account hierarchy. `F` toggles flat mode on and off.

By default, all subaccounts are displayed.
To see less detail, set a depth limit by pressing a number key, `1` to `9`.
`0` shows even less detail, collapsing all accounts to a single total.
`-` and `+` (or `=`) decrease and increase the depth limit.
To remove the depth limit, set it higher than the maximum account depth, or press `ESCAPE`.

`C` toggles cleared mode, in which
[uncleared transactions and postings](/journal.html#transactions) are
not shown. `U` toggles uncleared mode, in which only uncleared
transactions/postings are shown.

`R` toggles real mode, in which [virtual postings](/journal.html#virtual-postings) are ignored.

`E` toggles nonzero mode, in which only accounts with nonzero balances
are shown (hledger-ui shows zero items by default, unlike command-line
hledger).

Press `right` or `enter` to view an account's transactions register.

## Register screen

This screen lists all transactions affecting a particular account, like
a check register. Unlike hledger's register command (which
lists individual postings), in hledger-ui's register:

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

`C` toggles cleared mode, in which
[uncleared transactions and postings](/journal.html#transactions) are
not shown. `U` toggles uncleared mode, in which only uncleared
transactions/postings are shown.

`R` toggles real mode, in which [virtual postings](/journal.html#virtual-postings) are ignored.

`E` toggles nonzero mode, in which only transactions posting a nonzero
change are shown (hledger-ui shows zero items by default,
unlike command-line hledger).

Press `right` (or `enter`) to view the selected transaction in full
detail.

Note, filter queries which filter by account name are not very useful 
on this screen yet.

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
when you press g to reload. Once you have fixed the problem,
press g again to reload and resume normal operation.
(Or, you can press escape to cancel the reload attempt.)

_man_({{
# ENVIRONMENT

**COLUMNS**
The screen width to use. 
Default: the full terminal width.

_LEDGER_FILE_

# FILES

Reads _files_

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
}})
