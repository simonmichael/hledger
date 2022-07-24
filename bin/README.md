# Scripts

<div class=pagetoc>

<!-- toc -->
</div>

This document is the README in the hledger repo's [bin][] directory, 
and is also published as [Scripts][] on hledger.org.
Here we collect some extra scripts you can use to augment the core hledger tools.
These are either useful in themselves, or serve as examples/starting points for making your own scripts.

For a longer list of PTA tools (not hledger-specific), see [plaintextaccounting.org](https://plaintextaccounting.org).

Also, the following add-on commands are not kept in the bin directory, but can be installed by the
[hledger-install script](https://hledger.org/install.html#with-hledger-install)
or other [install methods](https://hledger.org/install.html):

- [`hledger-ui`](hledger-ui.html) is hledger's official curses-style TUI
- [`hledger-web`](hledger-web.html) is hledger's official web UI
- [`hledger-iadd`](https://hackage.haskell.org/package/hledger-iadd) is a popular alternative to hledger's `add` command.
- [`hledger-interest`](https://hackage.haskell.org/package/hledger-interest) generates interest transactions.
- [`hledger-stockquotes`](https://hackage.haskell.org/package/hledger-stockquotes) downloads market prices. (Alpha quality, needs your help.)


<!-- This page can be viewed on github or hledger.org, so use absolute urls. -->
[bin]:                https://github.com/simonmichael/hledger/tree/master/bin
[Scripts]:            https://hledger.org/scripts.html
[Scripting hledger]:  https://hledger.org/scripting.html

## About hledger scripts

(See also: [Scripting hledger][])

We are using the word "scripts" broadly here, meaning:

- shell aliases, functions, or executable shell script files
- or programs written in other languages like Python
- or programs written in Haskell, optionally compiled

which either:

- don't use hledger, but perform tasks related to it
- or run the hledger command line tools in some useful way
- or call hledger as a library, to do more powerful things
  (these must be written in Haskell).

and can be:

- local, used only by you
- or shared online for use by others
- or published in the main hledger repo's bin directory
- or published as haskell packages, and possibly system packages

And furthermore,

- a script or program which is named `hledger-something`, executable,
  and in your shell's PATH, is called an [add-on command](https://hledger.org/1.26/hledger.html#about-add-on-commands).
  These will show up in hledger's commands list (`hledger`), and can
  be invoked with (`hledger something`), much like built-in commands.

## Installing

The scripts collected here in the 
[bin](https://github.com/simonmichael/hledger/tree/master/bin) directory
are not automatically installed along with hledger; 
if you want them you must install them separately, as follows:

```cli
# suggested: go to wherever you keep financial files
$ cd ~/finance

# get the hledger repo (the fast way, without version control):
$ curl -LOJ https://github.com/simonmichael/hledger/archive/refs/heads/master.zip && unzip hledger-master.zip && mv hledger-master hledger

# (or the slow way, with version control for easy diffing/updating/contributing):
# git clone https://github.com/simonmichael/hledger.git

# symlink the bin directory (or you can copy it):
$ ln -s hledger/bin

# add this directory to your PATH. Eg as a bash user:
$ echo "export PATH=$PATH:$PWD/bin" >>~/.bash_profile"
$ export PATH=$PATH:$PWD/bin
```

Scripts with no file extension are mostly [bash] scripts except where noted.
if you don't want to install bash you might have to adapt them to your shell.

Scripts with a `.hs` file extension are usually [stack scripts][], requiring [stack][] to run. 
If you don't want to install stack you can adapt them to be cabal scripts,
or install their required libraries yourself and run/compile them with suitable runghc/ghc commands.
See also [Working with hledger-*.hs scripts](#working-with-hledger-hs-scripts) below.

[bash]: https://www.gnu.org/software/bash
[stack]: https://haskellstack.org
[stack scripts]: https://docs.haskellstack.org/en/stable/GUIDE/#script-interpreter

## The bin scripts

Here are the scripts currently collected in the bin directory:

### bashrc

[`bashrc`](https://github.com/simonmichael/hledger/blob/master/bin/bashrc)
contains many example bash aliases and functions.
After the above steps, as a bash user,
```cli
# customise FINDIR and LEDGER_FILE at the top of bin/bashrc
$ . bin/bashrc
$ fin        # list the scripts available
```

### paypaljson

[`paypaljson`](https://github.com/simonmichael/hledger/blob/master/bin/paypaljson)
downloads the last 30 days of Paypal transactions (requires a free developer account & API key).

### paypaljson2csv

[`paypaljson2csv`](https://github.com/simonmichael/hledger/blob/master/bin/paypal2csv) (python)
converts the above to CSV, with format similar to the CSV you could download manually.

### hledger-simplebal

[`hledger-simplebal`](https://github.com/simonmichael/hledger/blob/master/bin/hledger-simplebal)
tries to reliably report a single balance number from hledger.
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

### hledger-check-tagfiles

[`hledger-check-tagfiles.cabal.hs`](https://github.com/simonmichael/hledger/blob/master/bin/hledger-check-tagfiles.cabal.hs)
is the above as a cabal script.

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

