# Scripts and add-ons

<div class=pagetoc>

<!-- toc -->
</div>

This document is the README in the hledger repo's [bin] directory, 
and is also published as [Scripts] on hledger.org.

[Add-on commands](hledger.html#add-on-commands) are executable script files or compiled programs
named `hledger-*` and installed in $PATH, which show up in hledger's [commands list](hledger.html#commands).

Some larger / separately-maintained add-on commands are listed
at [hledger manual > PART 4. COMMANDS > ADD-ONS](https://hledger.org/dev/hledger.html#add-ons).

The rest of this page lists smaller scripts and add-on commands which are collected in bin/.
These are mostly ready to use, but some are just examples/inspiration for making your own.
<!-- Below is more about [installing the bin scripts](#installing-the-bin-scripts) and creating your own scripts. Contributions welcome! -->

Scripts vary in how closely they work with hledger; they can be classed as hledger-related, hledger-running and hledger-integrated.

<!-- This page can be viewed on github or hledger.org, so use absolute urls. -->
[bin]:                https://github.com/simonmichael/hledger/tree/master/bin
[Scripts]:            https://hledger.org/scripts.html
[Scripting hledger]:  https://hledger.org/scripting.html

## HLEDGER-RELATED

These scripts don't use hledger directly, but are complementary and might be useful to hledger users.
[plaintextaccounting.org](https://plaintextaccounting.org) has a longer list of non-hledger-specific PTA tools.

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
They can be:
- small shell aliases or functions, typically defined in shell startup files (eg ~/.bashrc)
- or individual script files written in shell or some other language, typically kept in ~/bin/ or other directory in $PATH.

### bashrc

[`bashrc`](https://github.com/simonmichael/hledger/blob/master/bin/bashrc)
contains many example bash aliases and functions.
After installing the bin scripts: as a bash user,
```cli
# customise FINDIR and LEDGER_FILE at the top of bin/bashrc
$ . bin/bashrc
$ fin        # list the scripts available
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
can be used to cleanup and normalize postings.
It will sort postings so that positive ones are first, negative ones last.
Inside of that it sorts postings by account name alphabetically.
Lastly it facilitates merging postings on transactions with more than one posting in the same direction on the same account.
This works by removing the duplicates and cleaning the amount field for at-most one account per run
Piping the output to `hledger print` can recalculate the missing amounts.
Subsequent runs can cleanup further duplicates.
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

### hledger-fifo

[`hledger-fifo`](https://github.com/edkedk99/hledger-fifo)
shows a lots report, or generates a lot sale transaction, using FIFO strategy
(and without needing subaccounts for lots).

Install or upgrade:
```
$ pip install -U git+https://github.com/edkedk99/hledger-fifo
```
Examples:
```cli
$ hledger fifo
$ hledger fifo lots -h
$ hledger fifo lots
$ hledger-fifo lots -c ADA -n 'closing balances'
$ hledger fifo sell
$ hledger fifo -- sell -c ADA -n 'closing balances' -b '$' -a assets:bank:checking -r 'revenues:capital gains' -d 2023-02-20 -q 100.23 -p 0.40
```

### hledger-edit

The [hledger-utils python package](https://pypi.org/project/hledger-utils/) provides
a `hledger-edit` command to edit the queried transactions in your `$EDITOR` no matter what file they reside in.

Install or upgrade:
```shell
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
```shell
$ pip install -U hledger-utils    # might be slightly different on your system
```
Examples:
```
$ hledger-plot -h
$ hledger plot -- bal -DH ^Assets -2
```

## HLEDGER-INTEGRATED

These are Haskell scripts or programs using the hledger-lib API, for maximum power/robustness.
They can use hledger's internal data types and do anything hledger's built-in commands can do.

### hledger-addon-example

[`hledger-addon-example.hs`](https://github.com/simonmichael/hledger/blob/master/bin/hledger-addon-example.hs)
is a starter template for a common type of script: a hledger-integrated add-on command.
It has the same structure as most of the other add-ons here:
- implemented as a stack script for robustness
- provides command line help
- accepts common hledger options

Further cleanup and documentation is ongoing.

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


## HOW TO

### Install the bin scripts

These scripts are not automatically installed along with hledger; 
if you want them you must download them separately. Here's a suggested method:

```cli
# go to wherever you keep financial files
$ cd ~/finance

# get the hledger repo (the fast way, without version control)
$ curl -LOJ https://github.com/simonmichael/hledger/archive/refs/heads/master.zip && unzip hledger-master.zip && mv hledger-master hledger

# (or the slow way, with version control for easy diffing/updating/contributing)
# git clone https://github.com/simonmichael/hledger

# make a more convenient symlink to the bin directory
$ ln -s hledger/bin

# add the bin directory to your PATH. Eg as a bash user:
$ echo "export PATH=$PATH:$PWD/bin" >>~/.bash_profile"
$ export PATH=$PATH:$PWD/bin

# check that hledger's command list now shows the hledger-* scripts
# (they will be listed with a + prefix):
$ hledger

# optionally compile all scripts for faster startup
$ cd hledger; bin/compile.sh
```

Scripts with no file extension are mostly [bash] scripts except where noted.
if you don't want to install bash you might have to adapt them to your shell.

Scripts with a `.hs` file extension are usually [stack scripts], requiring [stack] to run. 
If you don't want to use stack you can adapt them to be cabal scripts,
or install their required libraries yourself and run/compile them with suitable runghc/ghc commands.
Currently these stack scripts mostly use `stack runghc`; this is less robust than `stack script`,
but allows them to use the latest hledger source tree they are part of.
See the comments in hledger-check-fancyassertions.hs for more about this.

[bash]: https://www.gnu.org/software/bash
[stack]: https://haskellstack.org
[stack]: https://www.fpcomplete.com/haskell/get-started
[stack scripts]: https://docs.haskellstack.org/en/stable/GUIDE/#script-interpreter
[add-on commands]: https://hledger.org/dev/hledger.html#add-on-commands
[cabal]: https://www.haskell.org/cabal

### Create a new script

The example scripts follow a template that implements hledger's
standard command line options and help, so it's a good idea to use one
as your starting point. The hledger- naming is not required, but it
causes scripts to show up in the hledger commands list. On unix,
your new script should be marked executable. This should do it:

    $ cd hledger
    $ cp bin/hledger-swap-dates.hs bin/hledger-foo.hs  # and edit, at least the command name and help
    $ stack install string-qq     # ensure any extra script deps are installed
    $ bin/hledger-cmd.hs --help
    foo [OPTIONS]
      My new foo command.
      ...
    $ stack ghc bin/hledger-cmd.hs
    $ hledger foo -- --help
    foo [OPTIONS]
      My new foo command.
      ...

### Run ghcid on a script
  
    $ stack install string-qq     # ensure any extra script deps are installed
    $ stack exec -- ghcid bin/hledger-foo.hs 
    ...
    Ok, one module loaded.
    All good (1 module, at 10:50:48)

### Run ghci on a script

    $ stack install string-qq     # ensure any extra script deps are installed
    $ stack ghci bin/hledger-foo.hs 
    ...
    Ok, one module loaded.
    ...
    ghci> 

### Learn more about scripting hledger

See [Scripting hledger].