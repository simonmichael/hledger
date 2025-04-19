# Scripts and add-ons

<div class=pagetoc>

<!-- toc -->
</div>

(This is the README in the hledger repo's `bin/` directory,
also published as the [Scripts and add-ons] page on hledger.org.)

<!-- This page can be viewed on github or hledger.org, so use absolute urls here: -->
[Scripts and add-ons]: https://hledger.org/scripts.html
[Scripting hledger]:   https://hledger.org/scripting.html
[bin]:                 https://github.com/simonmichael/hledger/tree/master/bin

A *script* is a program you can run immediately without needing to compile it first.
They are often small and defined in a single file or shell alias or shell function.
You can create your own simple or complex scripts which enhance hledger.
Eg you might script a complicated report so you don't have to remember the detailed command(s).

A hledger *add-on command* is any program whose name begins with "hledger-".
Add-on commands found in PATH will appear in the commands list (shown when you run `hledger` with no arguments).
Some of the scripts below are add-ons.
Some add-ons are written in Haskell and can use hledger's full power, like builtin commands.

Below are some existing scripts you can use or learn from.
Most of these are collected in [hledger's bin/ directory][bin],
which you can get by [cloning](https://hledger.org/scripts.html#install-scripts) the hledger source.
Compiled add-ons are also listed below, in their own section.

Note, you don't need any of these extras if you are new to hledger -
except possibly hledger-ui and hledger-web, which can be nice to have at the start.


## Related scripts

Here are some scripts which don't use hledger directly, but might be useful to hledger users.
(For more, see also: [plaintextaccounting.org > Software](https://plaintextaccounting.org#software)).


### pricehist

[pricehist](https://pypi.org/project/pricehist) is useful for downloading market prices / conversion rates; recommended.
And [`hledger-pricehist`](https://github.com/simonmichael/hledger/blob/master/bin/hledger-pricehist)
is a small script to make it show up in the hledger commands list.

### paypaljson

[`paypaljson`](https://github.com/simonmichael/hledger/blob/master/bin/paypaljson)
downloads the last 30 days of Paypal transactions (requires a free developer account & API key).

### paypaljson2csv

[`paypaljson2csv`](https://github.com/simonmichael/hledger/blob/master/bin/paypaljson2csv) (python)
converts `paypaljson`'s output to CSV, with format similar to Paypal's manually-downloaded CSV.


## hledger command line scripts

These scripts use hledger's command line interface, or process one of its output formats.

### bashrc

[`bashrc`](https://github.com/simonmichael/hledger/blob/master/bin/bashrc)
contains many example bash aliases and functions.
After installing the bin scripts: as a bash user,
```cli
# customise FINDIR and LEDGER_FILE at the top of bin/bashrc
$ . bin/bashrc
$ fin        # list the scripts available
```

### ft

[`ft`](https://github.com/simonmichael/hledger/blob/master/bin/ft)
is a way to organise your finance-related reports and scripts using standard bash.
(See also [Justfile](#justfile) below.)

```cli
$ alias f=~/src/hledger/bin/ft
$ f
--------------------------------------------------------------------------------"; }
ft - finance tool: run financial reports and finance-related scripts
Usage: ft [COMMAND [ARGS]]
Commands:
help           show this help
get-csv        download auto-downloadable CSVs (paypal)
import-dry     import new downloaded transactions to the journal, dry run
import         import new downloaded transactions to the journal, logging and not printing errors
get-prices     [PRICEHISTFETCHOPTS] - download prices for main commodities (default: today's)
bs             show balance sheet
is             show income statement
a              show assets
r              show revenues
x              show expenses
ab             show assets bar chart
rb             show revenues bar chart
xb             show expenses bar chart
al             show assets line chart
rl             show revenues line chart
xl             show expenses line chart
forecast       print transactions predicted by forecast rules from last week on
household      show a draft month-end household adjustment transaction for last month
consulting     show consulting revenue
bin            [PAT]  show all scripts in $DIR/bin/[bashrc] (default: ~/finance/)
OTHERCMD               [ARGS] run other hledger commands on the default journal

Add hledger options to customise reports.
```

### tt

[`tt`](https://github.com/simonmichael/hledger/blob/master/bin/tt)
is a similar bash multi-script for time reports.

```cli
$ alias t=~/src/hledger/bin/tt
$ t
--------------------------------------------------------------------------------
tt - time tool: run time reports and time-related scripts
Usage: tt [COMMAND [ARGS]]
Commands:
help            show this help
dash            show time dashboard, redisplaying when timelog files change
status          show current time status
what            what happened ? Show largest balances first, today and depth 1 by default
dots            print line of N dots, grouped in 4s (suitable for timedot)
x               horizontal time summary this year, monthly by default
y               vertical time summary this year, monthly by default
rweeks          recent weeks' time budgets
weeks           this and last week's time budgets
hours           show a bar chart of daily hours
accunused       show unused / undeclared accounts
accunusedcat    show unused / undeclared accounts by category
accadd          add declarations for all undeclared accounts
budgets         show monthly time budget performance this year
budgetsy        show monthly time budget performance this year, vertically
budgetsw        show weekly time budget performance this year
budgetswx       show weekly time budget performance this year, horizontally
OTHERCMD        [ARGS] run other hledger commands on $TIMELOG

Add hledger options to customise reports.
```

### Justfile

<https://github.com/casey/just> is a nice tool for organising financial reports and scripts,
similar to `make`, but more robust for this use case. I can recommend it.
See also [hledger and just](just.md).

Here is a [Justfile](https://github.com/simonmichael/hledger/blob/master/bin/Justfile)
reimplementing the `ft` and `tt` scripts more simply:

```cli
$ brew install just  # eg
$ alias j=just
$ cd ~/finance
$ cp ~/src/hledger/bin/Justfile .  # or start from scratch: just --init
$ j
Justfile commands:
    watch CMD                      # rerun the given command with watchexec whenever local files change
    get-csv                        # download auto-downloadable CSVs (paypal)
    import-dry                     # import new downloaded transactions to the main journal, dry run
    import                         # import new downloaded transactions to the journal, logging and not printing errors
    get-prices *PRICEHISTFETCHOPTS # show prices for main commodities (default: today's)
    bs *HLEDGERARGS                # show balance sheet
    is *HLEDGERARGS                # show income statement
    a *HLEDGERARGS                 # show assets
    r *HLEDGERARGS                 # show revenues
    x *HLEDGERARGS                 # show expenses
    ab *HLEDGERARGS                # show assets bar chart
    rb *HLEDGERARGS                # show revenues bar chart
    xb *HLEDGERARGS                # show expenses bar chart
    al *HLEDGERARGS                # show assets line chart
    rl *HLEDGERARGS                # show revenues line chart
    xl *HLEDGERARGS                # show expenses line chart
    forecast *HLEDGERARGS          # print transactions predicted by forecast rules from last week on
    household *HLEDGERARGS         # show a draft month-end household adjustment transaction for last month
    consulting *HLEDGERARGS        # show consulting revenue
    tdash *HLEDGERARGS             # show time dashboard, redisplaying when timelog files change
    tstatus *HLEDGERARGS           # show current time status
    twhat *HLEDGERARGS             # what happened ? Show largest time balances first, today and depth 1 by default
    tdots N                        # print line of N dots, grouped in 4s (suitable for timedot)
    tx *HLEDGERARGS                # horizontal time summary this year, monthly by default
    ty *HLEDGERARGS                # vertical time summary this year, monthly by default
    tweeks *HLEDGERARGS            # this and last week's time budgets
    tweekspast *HLEDGERARGS        # recent past weeks' time budgets
    thours *HLEDGERARGS            # show a bar chart of daily hours
    taccunused *HLEDGERARGS        # show unused / undeclared time accounts
    taccunusedcat *HLEDGERARGS     # show unused / undeclared time accounts by category
    taccadd *HLEDGERARGS           # add declarations for all undeclared time accounts
    tbudgets *HLEDGERARGS          # show monthly time budget performance this year
    tbudgetsy *HLEDGERARGS         # show monthly time budget performance this year, vertically
    tbudgetsw *HLEDGERARGS         # show weekly time budget performance this year
    tbudgetswx *HLEDGERARGS        # show weekly time budget performance this year, horizontally
```

### watchaccounts

[`watchaccounts`](https://github.com/simonmichael/hledger/blob/master/bin/watchaccounts)
shows hledger account names, updating on file change under the current directory.
Arguments are passed to the `hledger accounts` command. Useful when cleaning up accounts.
```cli
$ watchaccounts expenses -2
$ watchaccounts -f time.journal client1 date:thismonth -l
```

### sortandmergepostings

[`sortandmergepostings`](https://github.com/simonmichael/hledger/blob/master/bin/sortandmergepostings)
is an adventuresome awk script intended to clean up and merge similar postings in a transaction
(see [original discussion](https://unix.stackexchange.com/questions/526995/re-order-lines-and-merge-others-based-on-a-specific-criteria/527004)).
It sorts postings so that positive ones are first, negative ones last.
Within each sign, postings are sorted alphabetically by account name.
Lastly if there are multiple postings to the same account in the same direction, it tries to merge them (by leaving some amounts blank).
Piping the output to `hledger print` can recalculate the missing amounts.
Multiple runs might be needed to clean up all duplicates.
```cli
$ sortandmergepostings input.journal | hledger -f - print -x
```

### hledger-simplebal

[`hledger-simplebal`](https://github.com/simonmichael/hledger/blob/master/bin/hledger-simplebal)
shows how to reliably report a single machine-readable number with hledger.
This and the other "hledger-" scripts are add-on commands.

```cli
$ hledger simplebal
```

### hledger-bar

[`hledger-bar`](https://github.com/simonmichael/hledger/blob/master/bin/hledger-bar)
prints quick bar charts in the terminal.

```cli
$ hledger bar reimbursement
2023-01	++++++
2023-02	++
2023-03	++
2023-04	-------
```

```cli
$ hledger bar                                        # show help
$ hledger bar food                                   # monthly food expenses
$ hledger bar -- 1 --count food                      # monthly food posting counts
$ hledger bar -- type:c not:tag:clopen cur:\\\\$ -W  # weekly cashflow, $ only
$ hledger bar -- type:al not:tag:clopen cur:\\\\$    # monthly net worth change ($)
$ hledger bar -- type:rx --invert cur:\\\\$          # monthly profit/loss ($)
$ hledger bar -- -v 1 -f $TIMELOG -D                 # daily hours, with numbers
```

### hledger-git

[`hledger-git`](https://github.com/simonmichael/hledger/blob/master/bin/hledger-git)
provides easy version control for your journal files, using [git](https://git-scm.com).
Run it with no arguments for help.
```cli
$ hledger git log
$ hledger git status
$ hledger git record [MSG]
```

### hledger-jj

[`hledger-jj`](https://github.com/simonmichael/hledger/blob/master/bin/hledger-jj)
provides easy version control for your journal files, using [jj](https://jj-vcs.github.io)
(and a git repo).
This is newer and better than hledger-git and hledger-pijul.
Unlike most shell scripts here, it requires [ysh](https://oils.pub).
```cli
$ hledger jj log
$ hledger jj status
$ hledger jj diff
$ hledger jj commit [MSG]
```

### hledger-pijul

[`hledger-pijul`](https://github.com/simonmichael/hledger/blob/master/bin/hledger-pijul)
provides easy version control for your journal files, using the [pijul](https://pijul.org) version control system.
```cli
$ hledger pijul log
$ hledger pijul status
$ hledger pijul record [MSG]
```

### hledger-edit

The [hledger-utils python package](https://pypi.org/project/hledger-utils/) provides
a `hledger-edit` command to edit the queried transactions in your `$EDITOR` no matter what file they reside in.

Install or upgrade:
```cli
$ pip install -U hledger-utils    # might be slightly different on your system
```

Examples:
```cli
# Opens your $EDITOR or $VISUAL with only costs in Florida 
# (if you named and tagged them like that)
# edit the transactions, save and exit your editor, 
# then the changes are distributed to the original files
$ hledger edit Cost tag:location=Florida
```
```
# Automate changes by setting `$EDITOR` to a script
# (here all food we had on that one day in Florida was Fast Food 🌭 and we initially forgot to write that) 
EDITOR='perl -pi -e "s|Cost:Food|Cost:Food:Fast Food|g"' hledger edit tag:location=Florida date:2022-12-20
```
[![asciicast](https://asciinema.org/a/549559.svg)](https://asciinema.org/a/549559)

### hledger-plot

The [hledger-utils python package](https://pypi.org/project/hledger-utils/) provides
a `hledger-plot` command for generating charts with matplotlib.

Install or upgrade:
```cli
$ pip install -U hledger-utils    # might be slightly different on your system
```
Examples:
```
$ hledger-plot -h
$ hledger plot -- bal -DH ^Assets -2
```

### hledger-lots

[`hledger-lots`](https://github.com/edkedk99/hledger-lots)
shows a lots report, or generates a lot sale transaction, using FIFO strategy
(and without needing subaccounts for lots).

Install or upgrade:
```
$ pip install -U hledger-lots
```
Examples:
```cli
$ hledger lots
$ hledger lots view
$ hledger lots list
```

### hledger-report1.sh

[hledger-report1.sh](https://github.com/simonmichael/hledger/blob/master/bin/hledger-report1.sh)
is a custom compound report done in shell. See also hledger-report1.hs.


## hledger haskell scripts

These scripts are written in Haskell and use hledger's haskell API (by importing the `hledger` or `hledger-lib` haskell libraries).
They are often [stack scripts](https://docs.haskellstack.org/en/stable/topics/scripts).
They can do anything hledger's builtin commands can do, and are usually more robust than command line scripts.
Some builtin commands were first developed as standalone haskell scripts.

### hledger-script-example

[`hledger-script-example.hs`](https://github.com/simonmichael/hledger/blob/master/bin/hledger-script-example.hs)
is a template for writing your own hledger-integrated add-on command.
It has the same structure as most of the add-ons here:
- a stack script for robustness
- providing command line help
- accepting common hledger options

### hledger-swap-dates

[`hledger-swap-dates.hs`](https://github.com/simonmichael/hledger/blob/master/bin/hledger-swap-dates.hs)
prints transactions with their date and date2 fields swapped.

### hledger-check-tagfiles

[`hledger-check-tagfiles.hs`](https://github.com/simonmichael/hledger/blob/master/bin/hledger-check-tagfiles.hs)
interprets all tag values containing a `/` (forward slash) as file paths, and checks that those files exist.
[`hledger-check-tagfiles.cabal.hs`](https://github.com/simonmichael/hledger/blob/master/bin/hledger-check-tagfiles.cabal.hs)
is the same command implemented as a cabal script rather than a stack script.

### hledger-register-max

[`hledger-register-max.hs`](https://github.com/simonmichael/hledger/blob/master/bin/hledger-register-max.hs)
runs a register report and prints the posting with largest historical balance.

```cli
$ hledger-register-max -f examples/bcexample.hledger checking
2013-01-03 Hoogle | Payroll  Assets:US:BofA:Checking      1350.60 USD  8799.22 USD
$ hledger register-max -- -f examples/bcexample.hledger checking
2013-01-03 Hoogle | Payroll  Assets:US:BofA:Checking      1350.60 USD  8799.22 USD
```

### hledger-check-postable

[`hledger-check-postable.hs`](https://github.com/simonmichael/hledger/blob/master/bin/hledger-check-postable.hs)
check that no postings are made to accounts declared with a `postable:n` or `postable:no` tag.
This can be used as a workaround when you must declare a parent account to control display order,
but you don't want to allow postings to it. Eg, to allow postings to assets:cash but not assets
(remember that account tags are inherited):
```journal
account assets         ; postable:n
account assets:cash    ; postable:
```

### hledger-check-fancyassertions

[`hledger-check-fancyassertions.hs`](https://github.com/simonmichael/hledger/blob/master/bin/hledger-check-fancyassertions.hs)
checks account balances over time in more complex ways than hledger's built-in balance assertions.

### hledger-combine-balances

[`hledger-combine-balances.hs`](https://github.com/simonmichael/hledger/blob/master/bin/hledger-combine-balances.hs)
shows balance reports for two different periods side by side.

### hledger-balance-as-budget

[`hledger-balance-as-budget.hs`](https://github.com/simonmichael/hledger/blob/master/bin/hledger-balance-as-budget.hs)
uses one balance report to set budget goals for another balance report.

This is useful for compare-and-contrast reports. You can, for example, use a previous year as a budget for this year, and see
how this year spending compares to your past spending.

### hledger-balance-as-budget-multi

[`hledger-balance-as-budget.hs`](https://github.com/simonmichael/hledger/blob/master/bin/hledger-balance-as-budget-multi.hs)
uses one balance report to set budget goals for another balance report, and allows you to run multiple balance commands on them.

Like `hledger run`, this will load journals only once, and will be significantly faster than calling `hledger-balance-as-budget`
multiple times.

### hledger-smooth

[`hledger-smooth.hs`](https://github.com/simonmichael/hledger/blob/master/bin/hledger-smooth.hs)
is an incomplete attempt at automatically splitting infrequent/irregular transactions.

### hledger-move

[`hledger-move.hs`](https://github.com/simonmichael/hledger/blob/master/bin/hledger-move.hs)
helps make subaccount/cost-preserving transfers.

### hledger-report1.hs

[hledger-report1.hs](https://github.com/simonmichael/hledger/blob/master/bin/hledger-report1.hs)
is a custom compound report done in haskell. See also hledger-report1.sh.

### hledger-txnsbycat.hs

[hledger-txnsbycat.hs](https://github.com/brianhv/hledger-scripts/blob/main/hledger-txnsbycat.hs)
is a mixture of a balance report and a register report; it shows each account's transactions
under the account's balance.

## Add-ons

These are some official and third-party add-ons you can install as compiled programs:

### hledger-ui

[hledger-ui](hledger-ui.html) is hledger's official terminal UI. It allows faster browsing of your accounts and transactions.

### hledger-web

[hledger-web](hledger-web.html) is hledger's official web UI. It allows data entry and simple reports in a web browser. It's good for non-command-line users.

### hledger-iadd

[hledger-iadd](https://hackage.haskell.org/package/hledger-iadd) is a popular alternative to the builtin `add` command.

### hledger-interest

[hledger-interest](https://hackage.haskell.org/package/hledger-interest) generates interest transactions.

Notes (as of 1.6.7):

- Use the <https://github.com/peti/hledger-interest#readme>, it's better than the command line help or hackage description.
- hledger-interest silently ignores $LEDGER_FILE, so be sure to provide a file name explicitly with -f.

### hledger-sankeymatic

[hledger-sankeymatic](https://github.com/victormihalache/hledger-sankeymatic) helps export hledger data to make flow diagrams at <https://sankeymatic.com>.
See also [Charts](charts.md).

### Other

- [hledger-stockquotes](https://hackage.haskell.org/package/hledger-stockquotes) fetches market prices. Not widely used, use pricehist instead.
- [hledger-diff](https://hackage.haskell.org/package/hledger-diff) compares two journal files. It's now built in to hledger, so you don't need it.



## How to...

### Install scripts

To use these bin scripts you must ensure they are in your $PATH and runnable:

- Shell scripts: you may need [bash](https://www.gnu.org/software/bash), or to adapt the scripts for your shell.
- Python scripts: you'll need python 3 and pip. 
- Haskell scripts: you'll need [stack](http://haskellstack.org).
Or if you know how, you can make them cabal scripts, or install their dependencies manually and use runghc/ghc.

Here's a suggested install procedure:

```cli
# Go to wherever you keep financial files:
$ cd ~/finance

# Get the hledger repo
# the fast way, without version control:
$ curl -LOJ https://github.com/simonmichael/hledger/archive/refs/heads/master.zip && unzip hledger-master.zip && mv hledger-master hledger
# or the slow way, with version control for easy diffing/updating/contributing
# git clone https://github.com/simonmichael/hledger

# Make a more convenient symlink to the bin directory:
$ ln -s hledger/bin

# Add the bin directory to your PATH. Eg as a bash user:
$ echo "export PATH=$PATH:$PWD/bin" >>~/.bash_profile"
$ export PATH=$PATH:$PWD/bin

# Optionally, compile all haskell scripts for faster startup:
$ cd hledger; bin/compile.sh

# Optionally, install the python scripts:
$ pip install -U hledger-utils
$ pip install -U hledger-lots

# Check that hledger's command list now includes the bin scripts.
# Eg "check-fancyassertions" and "swap-dates" should be listed:
$ hledger
```

### Create a new script

To create a new hledger-integrated script, copy hledger-script-example.hs.
On unix, the new script should be marked executable. This should do it:

    $ cd bin
    $ cp hledger-script-example.hs hledger-cmd.hs   # replace cmd with your command name
    # edit hledger-cmd.hs, updating at least the command name and help
    $ stack install safe text     # ensure the script's dependencies are installed
    $ hledger-cmd.hs --help
    cmd [OPTIONS]
      My new cmd command.
      ...
    $ stack ghc hledger-cmd.hs  # optionally compile for faster startup/durability
    $ hledger cmd -- --help
    cmd [OPTIONS]
      My new cmd command.
      ...

### Run ghcid on a script
  
    $ stack exec --package 'safe text' -- ghcid hledger-cmd.hs 
    ...
    Ok, one module loaded.
    All good (1 module, at 10:50:48)

### Run ghci on a script

    $ stack ghci --package 'safe text' hledger-cmd.hs 
    ...
    Ok, one module loaded.
    ...
    ghci> 

### Learn more about scripting hledger

See [Scripting hledger].
