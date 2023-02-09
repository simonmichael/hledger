# Scripts

<div class=pagetoc>

<!-- toc -->
</div>

This document is the README in the hledger repo's [bin] directory, 
and is also published as [Scripts] on hledger.org.
Here we collect hledger scripts: additional small tools which complement hledger in some way.
These can be:

- shell aliases or functions, defined eg in your shell's startup file
- shell script files
- programs written in other languages, like Python or Haskell.
  Haskell scripts are the most powerful since they can call hledger's Haskell API (we'll call these *hledger-integrated*).
- script files or programs  named `hledger-*`, 
  which show up in hledger's [commands list](hledger.html#commands)
  as [add-on commands](https://hledger.org/dev/hledger.html#addons).

The most common types of hledger script are:

1. shell aliases/functions/scripts which run hledger with custom options and arguments, eg to produce a particular report
2. Haskell add-on command scripts implementing variants of the built-in commands, or new kinds of report.

[Scripting hledger] has more on this general topic.

The current "bin scripts" are listed in the page contents and below, categorised by how they invoke hledger.
They are either useful as is, or can be examples/inspiration for making your own.
Contributions welcome!
Following the list are [install instructions](#installing-the-bin-scripts) and other tips.

<!-- This page can be viewed on github or hledger.org, so use absolute urls. -->
[bin]:                https://github.com/simonmichael/hledger/tree/master/bin
[Scripts]:            https://hledger.org/scripts.html
[Scripting hledger]:  https://hledger.org/scripting.html

## hledger-running scripts

These run hledger via its command line interface, and perhaps process its output:

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

### hledger-utils

The [`hledger-utils` Python package](https://pypi.org/project/hledger-utils/) ships an `hledger edit` command to edit the queried transactions in your `$EDITOR` no matter what file they reside in:

```cli
# Opens your $EDITOR or $VISUAL with only costs in Florida 
# (if you named and tagged them like that)
# edit the transactions, save and exit your editor, 
# then the changes are distributed to the original files
$ hledger edit Cost tag:location=Florida

# Automate changes by setting `$EDITOR` to a script
# (here all food we had on that one day in Florida was Fast Food 🌭 and we initially forgot to write that) 
EDITOR='perl -pi -e "s|Cost:Food|Cost:Food:Fast Food|g"' hledger edit tag:location=Florida date:2022-12-20
```

📹 Screencast

[![asciicast](https://asciinema.org/a/549559.svg)](https://asciinema.org/a/549559)

And also a useful `hledger plot` command for generating charts.

To install or upgrade the hledger-utils tools:
```shell
$ pip install -U hledger-utils    # might be slightly different on your system
```

## hledger-integrated scripts

These call hledger as a Haskell library, and so must be written in
Haskell. They can use hledger's internal data types and can do
anything hledger's built-in commands can do:

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

## hledger-related scripts

These don't run hledger, but are probably related to it in some way:

### paypaljson

[`paypaljson`](https://github.com/simonmichael/hledger/blob/master/bin/paypaljson)
downloads the last 30 days of Paypal transactions (requires a free developer account & API key).

### paypaljson2csv

[`paypaljson2csv`](https://github.com/simonmichael/hledger/blob/master/bin/paypaljson2csv) (python)
converts `paypaljson`'s output to CSV, with format similar to Paypal's manually-downloaded CSV.

## More scripts

[plaintextaccounting.org](https://plaintextaccounting.org) has a longer list of PTA tools, not hledger-specific.


## Installing the bin scripts

These [bin](https://github.com/simonmichael/hledger/tree/master/bin) scripts
are not automatically installed along with hledger; 
if you want them you must download them separately. Here's a suggested method:

```cli
# go to wherever you keep financial files
$ cd ~/finance

# get the hledger repo (the fast way, without version control)
$ curl -LOJ https://github.com/simonmichael/hledger/archive/refs/heads/master.zip && unzip hledger-master.zip && mv hledger-master hledger

# (or the slow way, with version control for easy diffing/updating/contributing)
# git clone https://github.com/simonmichael/hledger.git

# make a more convenient symlink to the bin directory
$ ln -s hledger/bin

# add the bin directory to your PATH. Eg as a bash user:
$ echo "export PATH=$PATH:$PWD/bin" >>~/.bash_profile"
$ export PATH=$PATH:$PWD/bin

# check that hledger's command list now shows the hledger-* scripts
# (they will be listed with a + prefix):
$ hledger
```

Scripts with no file extension are mostly [bash] scripts except where noted.
if you don't want to install bash you might have to adapt them to your shell.

Scripts with a `.hs` file extension are usually [stack scripts], requiring [stack] to run. 
If you don't want to install stack you can adapt them to be cabal scripts,
or install their required libraries yourself and run/compile them with suitable runghc/ghc commands.
See also [Working with hledger-*.hs scripts](#working-with-hledger-hs-scripts) below.

[bash]: https://www.gnu.org/software/bash
[stack]: https://haskellstack.org
[stack scripts]: https://docs.haskellstack.org/en/stable/GUIDE/#script-interpreter

## Working with hledger-*.hs scripts

The hledger-*.hs [add-on commands] are mostly implemented as [stack]
runghc scripts. See the comments in hledger-check-fancyassertions.hs
for more about how to run or compile them.  Short version: run
bin/compile.sh to compile all scripts, and add this directory to your
$PATH so they show up in hledger's command list.

[add-on commands]: http://hledger.org/hledger.html#add-on-commands
[stack]: https://www.fpcomplete.com/haskell/get-started
[cabal]: https://www.haskell.org/cabal

How to:

### Install all add-on commands

    $ git clone https://github.com/simonmichael/hledger
    $ hledger/bin/compile.sh
    $ export PATH=$PATH:$PWD/hledger/bin

    $ hledger                           # scripts now appear in commands list
    $ hledger-print-location --help     # run a script directly
    $ hledger print-location -- --help  # or run it via hledger. -- is needed before script options

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

