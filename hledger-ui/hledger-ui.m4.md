% hledger-ui(1)
% _author_
% _monthyear_

_notinfo_({{
# NAME
}})

hledger-ui - terminal interface (TUI) for `hledger`, a robust, friendly plain text accounting app.

_notinfo_({{
# SYNOPSIS
}})

`hledger-ui [OPTS] [QUERYARGS]`\
or\
`hledger ui [OPTS] [QUERYARGS]`

_notinfo_({{
# DESCRIPTION
}})

This manual is for hledger's terminal interface, version _version_.
See also the hledger manual for common concepts and file formats.

_hledgerdescription_

_web_({{
<div class="screenshots-right">
<a href="/images/hledger-ui/hledger-ui-sample-acc2.png" class="highslide" onclick="return hs.expand(this)"><img src="/images/hledger-ui/hledger-ui-sample-acc2.png" title="Accounts screen with query and depth limit" height="180"/></a>
<a href="/images/hledger-ui/hledger-ui-sample-acc.png" class="highslide" onclick="return hs.expand(this)"><img src="/images/hledger-ui/hledger-ui-sample-acc.png" title="Accounts screen" height="180"/></a>
<a href="/images/hledger-ui/hledger-ui-sample-acc-greenterm.png" class="highslide" onclick="return hs.expand(this)"><img src="/images/hledger-ui/hledger-ui-sample-acc-greenterm.png" title="Accounts screen with greenterm theme" height="180"/></a>
<a href="/images/hledger-ui/hledger-ui-sample-txn.png" class="highslide" onclick="return hs.expand(this)"><img src="/images/hledger-ui/hledger-ui-sample-txn.png" title="Transaction screen" height="180"/></a>
<a href="/images/hledger-ui/hledger-ui-sample-reg.png" class="highslide" onclick="return hs.expand(this)"><img src="/images/hledger-ui/hledger-ui-sample-reg.png" title="Register screen" height="180"/></a>
<a href="/images/hledger-ui/hledger-ui-bcexample-acc.png" class="highslide" onclick="return hs.expand(this)"><img src="/images/hledger-ui/hledger-ui-bcexample-acc.png" title="beancount example accounts" height="180"/></a>
<a href="/images/hledger-ui/hledger-ui-bcexample-acc-etrade-cash.png" class="highslide" onclick="return hs.expand(this)"><img src="/images/hledger-ui/hledger-ui-bcexample-acc-etrade-cash.png" title="beancount example's etrade cash subaccount" height="180"/></a>
<a href="/images/hledger-ui/hledger-ui-bcexample-acc-etrade.png" class="highslide" onclick="return hs.expand(this)"><img src="/images/hledger-ui/hledger-ui-bcexample-acc-etrade.png" title="beancount example's etrade investments, all commoditiess" height="180"/></a>
</div>
}})

hledger-ui is hledger's terminal interface, providing an efficient full-window text UI
for viewing accounts and transactions, and some limited data entry capability.
It is easier than hledger's command-line interface, and
sometimes quicker and more convenient than the web interface.

Like hledger, it _inputfileswithptr_

Unlike hledger, hledger-ui hides all future-dated transactions by default.
They can be revealed, along with any rule-generated periodic transactions,
by pressing the F key (or starting with --forecast) to enable "forecast mode".

# OPTIONS

Any arguments are interpreted as a hledger [query](hledger.md#queries) which filters the data.
hledger-ui provides the following options:

```
Flags:
  -w --watch                watch for data and date changes and reload
                            automatically
     --theme=THEME          use this custom display theme (default,
                            greenterm, terminal, dark)
     --cash                 start in the cash accounts screen
     --bs                   start in the balance sheet accounts screen
     --is                   start in the income statement accounts screen
     --all                  start in the all accounts screen
     --register=ACCTREGEX   start in the (first matched) account's register
     --change               show period balances (changes) at startup instead
                            of historical balances
  -l --flat                 show accounts as a flat list (default)
  -t --tree                 show accounts as a tree
```

and also supports many of hledger's [general options](hledger.md#options):

_generaloptions_

With hledger-ui, the `--debug` option sends debug output to a `hledger-ui.log` file in the current directory.

If you use the bash shell, you can auto-complete flags by pressing TAB in the command line.
If this is not working see [Install > Shell completions](install.html#shell-completions).

# MOUSE

In most modern terminals, you can navigate through the screens with a
mouse or touchpad:

- Use mouse wheel or trackpad to scroll up and down
- Click on list items to go deeper
- Click on the left margin (column 0) to go back.

# KEYS

Keyboard gives more control.

`?` shows a help dialog listing all keys.
(Some of these also appear in the quick help at the bottom of each screen.)
Press `?` again (or `ESCAPE`, or `LEFT`, or `q`) to close it.
The following keys work on most screens:

The cursor keys navigate:
`RIGHT` or `ENTER` goes deeper,
`LEFT` returns to the previous screen,
`UP`/`DOWN`/`PGUP`/`PGDN`/`HOME`/`END` move up and down through lists.
Emacs-style (`CTRL-p`/`CTRL-n`/`CTRL-f`/`CTRL-b`)
and VI-style (`k`,`j`,`l`,`h`) 
movement keys are also supported.

(Tip: movement speed is limited by your keyboard repeat rate, to move faster you may want to adjust it.
On a mac, the Karabiner app is one way to do that.)

`/` lets you set a general filter query limiting the data shown,
using the same [query terms](hledger.html#queries) as in hledger and hledger-web.
While editing the query, you can use [CTRL-a/e/d/k, BS, cursor keys](http://hackage.haskell.org/package/brick-0.7/docs/brick-widgets-edit.html#t:editor);
press `ENTER` to set it, or `ESCAPE`to cancel.
There are also keys for quickly adjusting some common filters like account depth and transaction status (see below).
`BACKSPACE` or `DELETE` removes all filters, showing all transactions.

As mentioned above, by default hledger-ui hides future transactions -
both ordinary transactions recorded in the journal, and periodic
transactions generated by rule. `F` toggles forecast mode, in which
future/forecasted transactions are shown.

Pressing `SHIFT-DOWN` narrows the report period, and pressing `SHIFT-UP` expands it again.
When narrowed, the current report period is displayed in the header line,
pressing `SHIFT-LEFT` or `SHIFT-RIGHT` moves to the previous or next period,
and pressing `T` sets the period to "today".
If you are using `-w/--watch` and viewing a narrowed period containing today,
the view will follow any changes in system date (moving to the period containing the new date).
(These keys work only with the standard Julian calendar year/quarter/month/week/day periods; they are not affected by a custom report interval specified at the command line.)

You can also specify a non-standard period with `/` and a `date:` query;
in this case, the period is not movable with the arrow keys.

(Tip: arrow keys with Shift do not work out of the box in all terminal software.
Eg in Apple's Terminal, the SHIFT-DOWN and SHIFT-UP keys must be configured as follows:
in Terminal's preferences, click Profiles,
select your current profile on the left,
click Keyboard on the right,
click + and add this for SHIFT-DOWN: `\033[1;2B`,
click + and add this for SHIFT-UP:   `\033[1;2A`. <!-- Press the Escape key to enter the `\033` part, you can't type it directly.) -->
In other terminals (Windows Terminal ?) you might need to configure SHIFT-RIGHT and SHIFT-LEFT
to emit `\033[1;2C` and `\033[1;2D` respectively.)

`ESCAPE` resets the UI state and jumps back to the top screen,
restoring the app's initial state at startup.
Or, it cancels minibuffer data entry or the help dialog.

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
This key will be available if `hledger-iadd` is installed in $path.

`E` runs $HLEDGER_UI_EDITOR, or $EDITOR, or a default (`emacsclient -a "" -nw`) on the journal file.
With some editors (emacs, vi), the cursor will be positioned at the current transaction
when invoked from the register and transaction screens, and at the error location (if possible)
when invoked from the error screen.

`B` toggles cost mode, showing amounts converted to their cost's commodity
(see [hledger manual > Cost reporting](hledger.md#cost-reporting).

`V` toggles value mode, showing amounts converted to their market value
(see [hledger manual > Valuation](hledger.md#valuation) flag).
More specifically, 

1. By default, the `V` key toggles showing end value (`--value=end`) on or off.
  The valuation date will be the report end date if specified, otherwise today.

2. If you started hledger-ui with some other valuation (such as `--value=then,EUR`),
  the `V` key toggles that off or on.

Cost/value tips:
- When showing end value, you can change the report end date without restarting, by pressing `/` and adding a query like ` date:..YYYY-MM-DD`.
- Either cost mode, or value mode, can be active, but not both at once. Cost mode takes precedence.
- There's not yet any visual indicator that cost or value mode is active, other than the amount values.

`q` quits the application.

Additional screen-specific keys are described below.

# SCREENS

At startup, hledger-ui shows a menu screen by default.
From here you can navigate to other screens using the cursor keys:
`UP`/`DOWN` to select, `RIGHT` to move to the selected screen, `LEFT` to return to the previous screen.
Or you can use `ESC` to return directly to the top menu screen.

You can also use a command line flag to specific a different startup screen
(`--cs`, `--bs`, `--is`, `--all`, or `--register=ACCT`).

## Menu screen

This is the top-most screen.
From here you can navigate to several screens listing accounts of various types.
Note some of these may not show anything until you have configured [account types](/hledger.html#account-types).

## Cash accounts screen

This screen shows "cash" (ie, liquid asset) accounts (like `hledger balancesheet type:c`).
It always shows balances (historical ending balances on the date shown in the title line).

## Balance sheet accounts screen

This screen shows asset, liability and equity accounts (like `hledger balancesheetequity`).
It always shows balances.

## Income statement accounts screen

This screen shows revenue and expense accounts (like `hledger incomestatement`).
It always shows changes (balance changes in the period shown in the title line).

## All accounts screen

This screen shows all accounts in your journal (unless filtered by a query; like `hledger balance`).
It shows balances by default; you can toggle showing changes with the `H` key.

## Register screen

This screen shows the transactions affecting a particular account.
Each line represents one transaction, and shows:

- the other account(s) involved, in abbreviated form.
  (If there are both real and virtual postings, it
  shows only the accounts affected by real postings.)

- the overall change to the current account's balance;
  positive for an inflow to this account, negative for an outflow.

- the running total after the transaction.
  With the `H` key you can toggle between
  - the period total, which is from just the transactions displayed
  - or the historical total, which includes any undisplayed transactions before the start of the report period (and matching the filter query if any).
    This will be the running historical balance (what you would see on a bank's website, eg) if not disturbed by a query.

Note, this screen combines each transaction's in-period postings to a
single line item, dated with the earliest in-period transaction or
posting date (like hledger's `aregister`). So custom posting dates can
cause the running balance to be temporarily inaccurate.
(See [hledger manual > aregister and posting dates](hledger.md#aregister-and-posting-dates).)

Transactions affecting this account's subaccounts will be included in the register
if the accounts screen is in tree mode,
or if it's in list mode but this account has subaccounts which are not shown due to a depth limit.
In other words, the register always shows the transactions contributing to the balance shown on the accounts screen.
Tree mode/list mode can be toggled with `t` here also.

`U` toggles filtering by [unmarked status](hledger.html#status), showing or hiding unmarked transactions.
Similarly, `P` toggles pending transactions, and `C` toggles cleared transactions.
(By default, transactions with all statuses are shown;
if you activate one or two status filters, only those transactions are shown;
and if you activate all three, the filter is removed.)

`R` toggles real mode, in which [virtual postings](hledger.html#virtual-postings) are ignored.

`z` toggles nonzero mode, in which only transactions posting a nonzero
change are shown (hledger-ui shows zero items by default,
unlike command-line hledger).

Press `RIGHT` to view the selected transaction in detail.

## Transaction screen

This screen shows a single transaction, as a general journal entry,
similar to hledger's print command and journal format (hledger_journal(5)).

The transaction's date(s) and any cleared flag, transaction code,
description, comments, along with all of its account postings are
shown.  Simple transactions have two postings, but there can be more
(or in certain cases, fewer).

`UP` and `DOWN` will step through all transactions listed in the
previous account register screen.  In the title bar, the numbers in
parentheses show your position within that account register. They will
vary depending on which account register you came from (remember most
transactions appear in multiple account registers). The #N number
preceding them is the transaction's position within the complete
unfiltered journal, which is a more stable id (at least until the next
reload).

On this screen (and the register screen), the `E` key will open your text editor
with the cursor positioned at the current transaction if possible.

## Error screen

This screen will appear if there is a problem, such as a parse error,
when you press g to reload. Once you have fixed the problem,
press g again to reload and resume normal operation.
(Or, you can press escape to cancel the reload attempt.)


# WATCH MODE

One of hledger-ui's best features is the auto-reloading `-w/--watch` mode.
With this flag, it will update the display automatically whenever changes
are saved to the data files. 

This is very useful when reconciling. A good workflow is to have
your bank's online register open in a browser window, for reference;
the journal file open in an editor window;
and hledger-ui in watch mode in a terminal window, eg:

```cli
$ hledger-ui --watch --register checking -C
```

As you mark things cleared in the editor,
you can see the effect immediately without having to context switch.
This leaves more mental bandwidth for your accounting.
Of course you can still interact with hledger-ui when needed,
eg to toggle cleared mode, or to explore the history.

## --watch problems

*However.* There are limitations/unresolved bugs with `--watch`:

- It may not work at all for you, depending on platform or system configuration.
  On some unix systems, increasing fs.inotify.max_user_watches or fs.file-max parameters in /etc/sysctl.conf might help.
  ([#836](https://github.com/simonmichael/hledger/issues/836))
- It may not detect file changes made by certain tools, such as Jetbrains IDEs or gedit.
  ([#1617](https://github.com/simonmichael/hledger/issues/1617))
- It may not detect changes made from outside a virtual machine, ie by an editor running on the host system.
- It may not detect file changes on certain less common filesystems.
- It may use increasing CPU and RAM over time, especially with large files.
  (This is probably not --watch specific, you may be able to reproduce it by pressing `g` repeatedly.)
  ([#1825](https://github.com/simonmichael/hledger/issues/1825))

Tips/workarounds:

- If --watch won't work for you, press `g` to reload data manually instead.
- If --watch is leaking resources over time, quit and restart (or suspend and resume) hledger-ui when you're not using it.
- When running hledger-ui inside a VM, also make file changes inside the VM.
- When working with files mounted from another machine, make sure the system clocks on both machines are roughly in agreement.

# ENVIRONMENT

**LEDGER_FILE**
The main journal file to use when not specified with `-f/--file`.
Default: `$HOME/.hledger.journal`.

# BUGS

_reportbugs_

Some known issues:

`-f-` doesn't work (hledger-ui can't read from stdin).

`--watch` is not robust, especially with large files (see WATCH MODE above).

If you press `g` with large files, there could be a noticeable pause with the UI unresponsive.
