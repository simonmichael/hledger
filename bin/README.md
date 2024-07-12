# Scripts and add-ons

<div class=pagetoc>

<!-- toc -->
</div>

This document is the README in the hledger repo's [bin] directory, 
and is also published as [Scripts and add-ons] on hledger.org.

[Add-on commands](hledger.html#add-on-commands) are executable script files or compiled programs
named `hledger-*`, which show up in hledger's commands list.
Some notable add-ons are listed [in the hledger manual](https://hledger.org/dev/hledger.html#add-ons). <!--  > PART 4. COMMANDS > ADD-ONS -->

The rest of this page lists smaller scripts and add-ons which are collected in bin/,
grouped by how closely they work with hledger:

To be clear: you don't need any of these when starting out with hledger.
hledger comes with many built-in commands, and you may want to get familiar with those first.


<!-- This page can be viewed on github or hledger.org, so use absolute urls. -->
[bin]:                https://github.com/simonmichael/hledger/tree/master/bin
[Scripts and add-ons]: https://hledger.org/scripts.html
[Scripting hledger]:  https://hledger.org/scripting.html

## HLEDGER-RELATED

These scripts don't use hledger directly, but are complementary and might be useful to hledger users.
([plaintextaccounting.org](https://plaintextaccounting.org) has a longer list of PTA tools.)

### hledger-pricehist

[`hledger-pricehist`](https://github.com/simonmichael/hledger/blob/master/bin/hledger-pricehist)
is just an alias for the market price downloader [pricehist](https://pypi.org/project/pricehist),
so that it shows up in the hledger commands list.

### paypaljson

[`paypaljson`](https://github.com/simonmichael/hledger/blob/master/bin/paypaljson)
downloads the last 30 days of Paypal transactions (requires a free developer account & API key).

### paypaljson2csv

[`paypaljson2csv`](https://github.com/simonmichael/hledger/blob/master/bin/paypaljson2csv) (python)
converts `paypaljson`'s output to CSV, with format similar to Paypal's manually-downloaded CSV.


## HLEDGER-RUNNING

These scripts run hledger via its CLI,
eg to help you produce a particular report without needing to remember a complicated command line. 
They might also consume its text or CSV or JSON output.
They can be
small shell aliases or functions (typically defined in shell startup files like ~/.bashrc)
or individual script files written in shell or another language (typically kept in ~/bin/ or elsewhere in $PATH).

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

<https://github.com/casey/just> is like [make](https://en.wikipedia.org/wiki/Make_(software)), but easier and more suitable for running commands.
It is a nice tool for organising financial reports and scripts!
More on [hledger and just](just.md).

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
provides easy version control for your journal files, using git. Run it with no arguments for help.
```cli
$ hledger git log
$ hledger git status
$ hledger git record [MSG]
```

### hledger-pijul

[`hledger-pijul`](https://github.com/simonmichael/hledger/blob/master/bin/hledger-pijul)
provides the same thing using the [pijul](https://pijul.org) version control system..
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


## HLEDGER-INTEGRATED

These Haskell scripts use the hledger-lib API for maximum power and robustness;
they can do anything hledger's built-in commands can do.

### hledger-script-example

[`hledger-script-example.hs`](https://github.com/simonmichael/hledger/blob/master/bin/hledger-script-example.hs)
is a template for writing your own hledger-integrated add-on command.
It has the same structure as most of the add-ons here:
- a stack script for robustness
- providing command line help
- accepting common hledger options

### hledger-print-location

[`hledger-print-location.hs`](https://github.com/simonmichael/hledger/blob/master/bin/hledger-print-location.hs)
is a variant of hledger's `print` command
that adds the file and line number to every transaction, as a tag:
```cli
$ hledger print-location -f hledger/examples/sample.journal desc:eat
2008/06/03 * eat & shop
  ; location: /Users/simon/src/hledger/examples/sample.journal:30
  expenses:food                  $1
  expenses:supplies              $1
  assets:cash
```

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

## HOW TO

### Install scripts

To use these bin scripts you must ensure they are in your $PATH and runnable:

- Shell scripts: you may need [bash], or to adapt the scripts for your shell.
- Python scripts: you'll need python 3 and pip. 
- Haskell scripts: you'll need stack (<https://www.haskell.org/get-started>).
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

[bash]: https://www.gnu.org/software/bash
[stack]: https://haskellstack.org
[stack]: https://www.fpcomplete.com/haskell/get-started
[stack scripts]: https://docs.haskellstack.org/en/stable/GUIDE/#script-interpreter
[add-on commands]: https://hledger.org/dev/hledger.html#add-on-commands
[cabal]: https://www.haskell.org/cabal

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
