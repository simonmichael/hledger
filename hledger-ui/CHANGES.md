User-visible changes in hledger-ui.
See also the hledger changelog.

# aa20f34b

- lib, cli, ui: start using Control.Monad.Fail, allow base-compat 0.11

# 1.15 2019-09-01

- allow brick >=0.47

- use hledger 1.15

# 1.14.2 2019-03-20

- support brick 0.47+ as well, to get into stackage nightly.

# 1.14.1 2019-03-20

- require brick <0.47 to fix build (#995)

- use hledger 1.14.2

# 1.14 2019-03-01

- use hledger 1.14

# 1.13.1 (2019/02/02)

- fix build issues with older brick/stack resolvers; require brick 0.23+

# 1.13 (2019/02/01)

- on posix systems, control-z suspends the program

- control-l now works everywhere and redraws more reliably

- the top status info is clearer

- use hledger 1.13

# 1.12.1 (2018/12/10)

-   avoid build issue with brick 0.44+ (#935)

# 1.12 (2018/12/02)

-   fix "Any" build error with GHC < 8.4

-   error screen: always show error position properly (#904) (Mykola Orliuk)

-   accounts screen: show correct balances when there's only periodic transactions

-   drop the --status-toggles flag

-   periodic transactions and transaction modifiers are always enabled.
    Rule-based transactions and postings are always generated
    (--forecast and --auto are always on).
    Experimental.

-   escape key resets to flat mode.
    Flat mode is the default at startup. Probably it should reset to tree
    mode if --tree was used at startup.

-   tree mode tweaks: add --tree/-T/-F flags, make flat mode the default,\
    toggle tree mode with T, ensure a visible effect on register screen

-   hide future txns by default, add --future flag, toggle with F.
    You may have transactions dated later than today, perhaps piped from
    print --forecast or recorded in the journal, which you don't want to
    see except when forecasting.

    By default, we now hide future transactions, showing "today's balance".
    This can be toggled with the F key, which is easier than setting a
    date query. --present and --future flags have been added to set the
    initial mode.

    (Experimental. Interactions with date queries have not been explored.)

-   quick help tweaks; try to show most useful info first

-   reorganise help dialog, fit content into 80x25 again

-   styling tweaks; cyan/blue -> white/yellow

-   less noisy styling in horizontal borders (#838)

-   register screen: positive amounts: green -> black
    The green/red scheme helped distinguish the changes column from the
    black/red balance column, but the default green is hard to read on
    the pale background in some terminals. Also the changes column is
    non-bold now.

-   use hledger 1.12

# 1.11.1 (2018/10/06)

-   use hledger 1.11.1

# 1.11 (2018/9/30)

-   use hledger 1.11

# 1.10.1 (2018/7/3)

-   restore support for fsnotify 0.2.1.2, as well as 0.3.x (#833)

-   fix a vty version bound & possibly build failures with old vty (#494)

# 1.10 (2018/6/30)

-   the effect of --value, --forecast, and --anon flags is now preserved on reload (#753)

-   edit-at-transaction-position is now also supported when $EDITOR is neovim

-   support/require fsnotify 0.3.0.1+

-   use hledger-lib 1.10

# 1.9.1 (2018/4/30)

-   use hledger-lib 1.9.1

# 1.9 (2018/3/31)

-   support ghc 8.4, latest deps

-   when the system text encoding is UTF-8, ignore any UTF-8 BOM prefix
    found when reading files

-   -E/--empty toggles zeroes at startup (with opposite default to cli)

# 1.5 (2017/12/31)

-   fix help -> view manual (on posix platforms) #623

-   support -V/--value, --forecast, --auto

-   remove upper bounds on all but hledger* and base (experimental)

# 1.4 (2017/9/30)

-   a @FILE argument reads flags & args from FILE, one per line

-   enable --pivot and --anon options, like hledger CLI (#474) (Jakub Zárybnický)

-   accept -NUM as a shortcut for --depth NUM

-   deps: allow ansi-terminal 0.7

-   deps: drop oldtime flag, require time 1.5+

# 1.3.1 (2017/8/25)

-   allow megaparsec 6 (#594, Simon Michael, Hans-Peter Deifel)

-   allow megaparsec-6.1 (Hans-Peter Deifel)

-   allow vty 5.17 (Felix Yan)

-   allow brick 0.24

-   restore upper bounds on hledger packages

# 1.3 (2017/6/30)

The register screen now shows transaction status marks.

The "uncleared" status, and associated UI flags and keys, have been
renamed to "unmarked" to remove ambiguity and confusion. This means
that we have dropped the `--uncleared` flag, and our `-U` flag now
matches only unmarked things and not pending ones. See the issue and
linked mail list discussion for more background. (#564)

The P key toggles pending mode, consistent with U (unmarked) and C
(cleared). There is also a temporary --status-toggles flag for testing
other toggle styles; see `hledger-ui -h`. (#564)

There is now less "warping" of selection when lists change:

-   When the selected account disappears, eg when toggling zero
    accounts, the selection moves to the alphabetically preceding item,
    instead of the first one.

-   When the selected transaction disappears, eg when toggling status
    filters, the selection moves to the nearest transaction by date (and
    if several have the same date, by journal order), instead of the
    last one.

In the accounts and register screens, you can now scroll down further
so that the last item need not always be shown at the bottom of the
screen. And we now try to show the selected item centered in the
following situations:

-   after moving to the end with Page down/End
-   after toggling filters (status, real, historical..)
-   on pressing the control-l key (should force a screen redraw, also)
-   on entering the register screen from the accounts screen (there's a
    known problem with this: it doesn't work the first time).

(Items near the top can't be centered, as we don't scroll higher than
the top of the list.)

Emacs movement keys are now supported, as well as VI keys.
hjkl and CTRL-bfnp should work wherever unmodified arrow keys work.

In the transaction screen, amounts are now better aligned, eg when
there are posting status marks or virtual postings.

Deps: allow brick 0.19 (#575, Felix Yan, Simon Michael)

# 1.2 (2017/3/31)

Fix a pattern match failure when pressing E on the transaction screen (fixes #508)

Accounts with ? in name had empty registers (fixes #498) (Bryan Richter)

Allow brick 0.16 (Joshua Chia) and brick 0.17/vty 0.15 (Peter Simons)

Allow megaparsec 5.2 (fixes #503)

Allow text-zipper 0.10

# 1.1.1 (2017/1/20)

-   allow brick 0.16 (Joshua Chia)

-   drop obsolete --no-elide flag

# 1.1 (2016/12/31)

-   with --watch, the display updates automatically to show file or date changes

    hledger-ui --watch will reload data when the journal file (or any included file) changes.
    Also, when viewing a current standard period (ie this day/week/month/quarter/year),
    the period will move as needed to track the current system date.

-   the --change flag shows period changes at startup instead of historical ending balances

-   the A key runs the hledger-iadd tool, if installed

-   always reload when g is pressed

    Previously it would check the modification time and reload only if
    it looked newer than the last reload.

-   mark hledger-ui as "stable"

-   allow brick 0.15, vty 5.14, text-zipper 0.9

# 1.0.4 (2016/11/2)

-   allow brick 0.13

# 1.0.3 (2016/10/31)

-   use brick 0.12

# 1.0.2 (2016/10/27)

-   use latest brick 0.11

# 1.0.1 (2016/10/27)

-   allow megaparsec 5.0 or 5.1

# 1.0 (2016/10/26)

## accounts screen

-   at depth 0, show accounts on one "All" line and show all transactions in the register

-   0 now sets depth limit to 0 instead of clearing it

-   always use --no-elide for a more regular accounts tree

## register screen

-   registers can now include/exclude subaccount transactions.

    The register screen now includes subaccounts' transactions if the
    accounts screen was in tree mode, or when showing an account
    which was at the depth limit. Ie, it always shows the
    transactions contributing to the balance displayed on the
    accounts screen. As on the accounts screen, F toggles between
    tree mode/subaccount txns included by default and flat
    mode/subaccount txns excluded by default. (At least, it does when
    it would make a difference.)

-   register transactions are filtered by realness and status (#354).

    Two fixes for the account transactions report when --real/--cleared/real:/status:
    are in effect, affecting hledger-ui and hledger-web:

    1.  exclude transactions which affect the current account via an excluded posting type.
        Eg when --real is in effect, a transaction posting to the current account with only
        virtual postings will not appear in the report.

    2.  when showing historical balances, don't count excluded posting types in the
        starting balance. Eg with --real, the starting balance will be the sum of only the
        non-virtual prior postings.

        This is complicated and there might be some ways to confuse it still, causing
        wrongly included/excluded transactions or wrong historical balances/running totals
        (transactions with both real and virtual postings to the current account, perhaps ?)

-   show more accurate dates when postings have their own dates.

    If postings to the register account matched by the register's
    filter query have their own dates, we show the earliest of these
    as the transaction date.

## misc

-   H toggles between showing "historical" or "period" balances (#392).

    By default hledger-ui now shows historical balances, which
    include transactions before the report start date (like hledger
    balance --historical). Use the H key to toggle to "period" mode,
    where balances start from 0 on the report start date.

-   shift arrow keys allow quick period browsing

    -   shift-down narrows to the next smaller standard period
        (year/quarter/month/week/day), shift-up does the reverse
    -   when narrowed to a standard period, shift-right/left moves to
        the next/previous period
    -   \`t\` sets the period to today.

-   a runs the add command

-   E runs $HLEDGER_UI_EDITOR or $EDITOR or a default editor (vi) on the journal file.

    When using emacs or vi, if a transaction is selected the cursor will be positioned at its journal entry.

-   / key sets the filter query; BACKSPACE/DELETE clears it

-   Z toggles display of zero items (like --empty), and they are shown by default.

    -E/--empty is now the default for hledger-ui, so accounts with 0 balance
    and transactions posting 0 change are shown by default. The Z key
    toggles this, entering "nonzero" mode which hides zero items.

-   R toggles inclusion of only real (non-virtual) postings

-   U toggles inclusion of only uncleared transactions/postings

-   I toggles balance assertions checking, useful for troubleshooting

-   vi-style movement keys are now supported (for help, you must now use ? not h) (#357)

-   ESC cancels minibuffer/help or clears the filter query and jumps to top screen

-   ENTER has been reserved for later use

-   reloading now preserves any options and modes in effect

-   reloading on the error screen now updates the message rather than entering a new error screen

-   the help dialog is more detailed, includes the hledger-ui manual, and uses the full terminal width if needed

-   the header/footer content is more efficient; historical/period and tree/flat modes are now indicated in the footer

-   date: query args on the command line now affect the report period.

    A date2: arg or --date2 flag might also affect it (untested).

-   hledger-ui now uses the quicker-building microlens

0.27.3 (2016/1/12)

-   allow brick 0.4

0.27.2 (2016/1/11)

-   allow brick 0.3.x

0.27.1 (2015/12/3)

-   allow lens 4.13
-   make reloading work on the transaction screen

0.27 (2015/10/30)

-   hledger-ui is a new curses-style UI, intended to be a standard part
    of the hledger toolset for all users (except on native MS Windows,
    where the vty lib is not yet supported).

    The UI is quite simple, allowing just browsing of accounts and
    transactions, but it has a number of improvements over the old
    hledger-vty, which it replaces:

    -   adapts to screen size
    -   handles wide characters
    -   shows multi-commodity amounts on one line
    -   manages cursor and scroll position better
    -   allows depth adjustment
    -   allows --flat toggle
    -   allows --cleared toggle
    -   allows journal reloading
    -   shows a more useful transaction register, like hledger-web
    -   offers multiple color themes
    -   includes some built-in help

    hledger-ui is built with brick, a new higher-level UI library based
    on vty, making it relatively easy to grow and maintain.
