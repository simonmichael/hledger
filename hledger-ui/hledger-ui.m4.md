% hledger-ui(1) hledger-ui _version_
% _author_
% _monthyear_

_web_({{
_docversionlinks_({{hledger-ui}})
}})

_man_({{
# NAME

hledger-ui - terminal interface for the hledger accounting tool

# SYNOPSIS

`hledger-ui [OPTIONS] [QUERYARGS]`\
`hledger ui -- [OPTIONS] [QUERYARGS]`

# DESCRIPTION

_hledgerdescription_
}})

_web_({{
<style>
.highslide img {max-width:200px; border:0;}
.highslide-caption {color:white; background-color:black;}
</style>
<div style="float:right; max-width:200px; text-align:right;">
<a href="_static/images/hledger-ui/hledger-ui-sample-acc2.png" class="highslide" onclick="return hs.expand(this)"><img src="_static/images/hledger-ui/hledger-ui-sample-acc2.png" title="Accounts screen with query and depth limit" /></a>
<a href="_static/images/hledger-ui/hledger-ui-sample-acc.png" class="highslide" onclick="return hs.expand(this)"><img src="_static/images/hledger-ui/hledger-ui-sample-acc.png" title="Accounts screen" /></a>
<a href="_static/images/hledger-ui/hledger-ui-sample-acc-greenterm.png" class="highslide" onclick="return hs.expand(this)"><img src="_static/images/hledger-ui/hledger-ui-sample-acc-greenterm.png" title="Accounts screen with greenterm theme" /></a>
<a href="_static/images/hledger-ui/hledger-ui-sample-txn.png" class="highslide" onclick="return hs.expand(this)"><img src="_static/images/hledger-ui/hledger-ui-sample-txn.png" title="Transaction screen" /></a>
<a href="_static/images/hledger-ui/hledger-ui-sample-reg.png" class="highslide" onclick="return hs.expand(this)"><img src="_static/images/hledger-ui/hledger-ui-sample-reg.png" title="Register screen" /></a>
<!-- <br clear=all> -->
<a href="_static/images/hledger-ui/hledger-ui-bcexample-acc.png" class="highslide" onclick="return hs.expand(this)"><img src="_static/images/hledger-ui/hledger-ui-bcexample-acc.png" title="beancount example accounts" /></a>
<a href="_static/images/hledger-ui/hledger-ui-bcexample-acc-etrade-cash.png" class="highslide" onclick="return hs.expand(this)"><img src="_static/images/hledger-ui/hledger-ui-bcexample-acc-etrade-cash.png" title="beancount example's etrade cash subaccount" /></a>
<a href="_static/images/hledger-ui/hledger-ui-bcexample-acc-etrade.png" class="highslide" onclick="return hs.expand(this)"><img src="_static/images/hledger-ui/hledger-ui-bcexample-acc-etrade.png" title="beancount example's etrade investments, all commoditiess" /></a>
</div>
}})

hledger-ui is hledger's terminal interface, providing an efficient full-window text UI
for viewing accounts and transactions, and some limited data entry capability.
It is easier than hledger's command-line interface, and 
sometimes quicker and more convenient than the web interface.

Note hledger-ui has some different defaults (experimental):

- it generates rule-based transactions and postings by default (--forecast and --auto are always on). 
- it hides transactions dated in the future by default (change this with --future or the F key).

Like hledger, it reads _files_
For more about this see hledger(1), hledger_journal(5) etc.

# OPTIONS

Note: if invoking hledger-ui as a hledger subcommand, write `--` before options as shown above.

Any QUERYARGS are interpreted as a hledger search query which filters the data.

`--watch`
: watch for data and date changes and reload automatically

`--theme=default|terminal|greenterm`
: use this custom display theme

`--register=ACCTREGEX`
: start in the (first) matched account's register screen

`--change`
: show period balances (changes) at startup instead of historical balances 

`-F --flat`
: show accounts as a list (default)

`-T --tree`
: show accounts as a tree

`--future`
: show transactions dated later than today (normally hidden)

 hledger input options:

_inputoptions_

hledger reporting options:

_reportingoptions_

hledger help options:

_helpoptions_

A @FILE argument will be expanded to the contents of FILE,
which should contain one command line option/argument per line.
(To prevent this, insert a `--` argument before.)

# KEYS

`?` shows a help dialog listing all keys. 
(Some of these also appear in the quick help at the bottom of each screen.) 
Press `?` again (or `ESCAPE`, or `LEFT`) to close it.
The following keys work on most screens:

The cursor keys navigate: 
`right` (or `enter`) goes deeper, 
`left` returns to the previous screen,
`up`/`down`/`page up`/`page down`/`home`/`end` move up and down through lists.
Vi-style (`h`/`j`/`k`/`l`) and Emacs-style (`CTRL-p`/`CTRL-n`/`CTRL-f`/`CTRL-b`) movement keys are also supported. 
A tip: movement speed is limited by your keyboard repeat rate, 
to move faster you may want to adjust it.
(If you're on a mac, the Karabiner app is one way to do that.)

With shift pressed, the cursor keys adjust the report period, 
limiting the transactions to be shown (by default, all are shown). 
`shift-down/up` steps downward and upward through these standard report period durations:
year, quarter, month, week, day.
Then, `shift-left/right` moves to the previous/next period.
`t` sets the report period to today.
With the `--watch` option, when viewing a "current" period 
(the current day, week, month, quarter, or year),
the period will move automatically to track the current date.
To set a non-standard period, you can use `/` and a `date:` query.

`/` lets you set a general filter query limiting the data shown,
using the same [query terms](hledger.html#queries) as in hledger and hledger-web.
While editing the query, you can use [CTRL-a/e/d/k, BS, cursor keys](http://hackage.haskell.org/package/brick-0.7/docs/Brick-Widgets-Edit.html#t:Editor); 
press `ENTER` to set it, or `ESCAPE`to cancel.
There are also keys for quickly adjusting some common filters like account depth and transaction status (see below).
`BACKSPACE` or `DELETE` removes all filters, showing all transactions.

As mentioned above, hledger-ui shows auto-generated periodic transactions,
and hides future transactions (auto-generated or not) by default.
`F` toggles showing and hiding these future transactions.
This is similar to using a query like `date:-tomorrow`, but more convenient.
(experimental) 

`ESCAPE` removes all filters and jumps back to the top screen.
Or, it cancels a minibuffer edit or help dialog in progress.

`CTRL-l` redraws the screen and centers the selection if possible
(selections near the top won't be centered, since we don't scroll above the top). 

`g` reloads from the data file(s) and updates the current screen and any
previous screens. (With large files, this could cause a noticeable pause.)

`I` toggles balance assertion checking. 
Disabling balance assertions temporarily can be useful for troubleshooting. 

`a` runs command-line hledger's add command, and reloads the updated file.
This allows some basic data entry. 

`A` is like `a`, but runs the [hledger-iadd](http://hackage.haskell.org/package/hledger-iadd) tool, 
which provides a terminal interface.
This key will be available if `hledger-iadd` is installed in $PATH.  

`E` runs $HLEDGER_UI_EDITOR, or $EDITOR, or a default (`emacsclient -a "" -nw`) on the journal file.
With some editors (emacs, vi), the cursor will be positioned at the current transaction
when invoked from the register and transaction screens, and at the error location (if possible)
when invoked from the error screen. 

`q` quits the application.

Additional screen-specific keys are described below.

# SCREENS

## Accounts screen

This is normally the first screen displayed.
It lists accounts and their balances, like hledger's balance command.
By default, it shows all accounts and their latest ending balances (including the balances of subaccounts).
if you specify a query on the command line, it shows just the matched accounts and the balances from matched transactions.

Account names are shown as a flat list by default. Press `T` to toggle tree mode. 
In flat mode, account balances are exclusive of subaccounts, except where subaccounts are hidden by a depth limit (see below).
In tree mode, all account balances are inclusive of subaccounts.

To see less detail, press a number key, `1` to `9`, to set a depth limit.
Or use `-` to decrease and `+`/`=` to increase the depth limit.
`0` shows even less detail, collapsing all accounts to a single total.
To remove the depth limit, set it higher than the maximum account depth, or press `ESCAPE`.

`H` toggles between showing historical balances or period balances.
Historical balances (the default) are ending balances at the end of the report period, 
taking into account all transactions before that date (filtered by the filter query if any),
including transactions before the start of the report period. In other words, historical
balances are what you would see on a bank statement for that account (unless disturbed by
a filter query). Period balances ignore transactions before the report start date, so they
show the change in balance during the report period. They are more useful eg when viewing a time log.

`U` toggles filtering by [unmarked status](journal.html#status), 
including or excluding unmarked postings in the balances. 
Similarly, `P` toggles pending postings, 
and `C` toggles cleared postings.
(By default, balances include all postings;
if you activate one or two status filters, only those postings are included; 
and if you activate all three, the filter is removed.)

`R` toggles real mode, in which [virtual postings](journal.html#virtual-postings) are ignored.

`Z` toggles nonzero mode, in which only accounts with nonzero balances
are shown (hledger-ui shows zero items by default, unlike command-line
hledger).

Press `right` or `enter` to view an account's transactions register.

## Register screen

This screen shows the transactions affecting a particular account, like a check register. 
Each line represents one transaction and shows:

- the other account(s) involved, in abbreviated form. 
  (If there are both real and virtual postings, it
  shows only the accounts affected by real postings.)

- the overall change to the current account's balance; 
  positive for an inflow to this account, negative for an outflow.

- the running historical total or period total for the current account, after the transaction.
This can be toggled with `H`.
Similar to the accounts screen, the historical total is affected by transactions
(filtered by the filter query) before the report start date, while the period total is not.
If the historical total is not disturbed by a filter query, it will be the
running historical balance you would see on a bank register for the current account. 

Transactions affecting this account's subaccounts will be included in the register
if the accounts screen is in tree mode, 
or if it's in flat mode but this account has subaccounts which are not shown due to a depth limit. 
In other words, the register always shows the transactions contributing to the balance shown on the accounts screen.  
Tree mode/flat mode can be toggled with `T` here also.

`U` toggles filtering by [unmarked status](journal.html#status), showing or hiding unmarked transactions. 
Similarly, `P` toggles pending transactions, and `C` toggles cleared transactions.
(By default, transactions with all statuses are shown;
if you activate one or two status filters, only those transactions are shown; 
and if you activate all three, the filter is removed.)

`R` toggles real mode, in which [virtual postings](journal.html#virtual-postings) are ignored.

`Z` toggles nonzero mode, in which only transactions posting a nonzero
change are shown (hledger-ui shows zero items by default,
unlike command-line hledger).

Press `right` (or `enter`) to view the selected transaction in detail.

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
regenerated, which may cause a noticeable pause with large files.
Also there is no visual indication that this is in progress.

`--watch` is not yet fully robust. It works well for normal usage, but
many file changes in a short time (eg saving the file thousands of
times with an editor macro) can cause problems at least on OSX.
Symptoms include: unresponsive UI, periodic resetting of the cursor
position, momentary display of parse errors, high CPU usage eventually
subsiding, and possibly a small but persistent build-up of CPU usage
until the program is restarted. 

Also, if you are viewing files mounted from another machine, `--watch`
requires that both machine clocks are roughly in step.


}})
