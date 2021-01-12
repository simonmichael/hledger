Miscellaneous hledger add-ons, bash scripts, example make rules, etc. 
Things in this directory can be unfinished or out of date.

## hledger scripts

The hledger-*.hs scripts here are example/experimental hledger [add-on commands].
See <https://hledger.org/scripting.html> for more about this.

They are mostly implemented as [stack] runghc scripts. See the comments in
hledger-check-fancyassertions.hs for more about how to run or compile them.
Short version: run bin/compile.sh to compile all scripts, and add this directory
to your $PATH so they show up in hledger's command list.

Scripts overview, simplest first:

- hledger-check-tagfiles.hs         - check that all tag values containing / exist as file paths
- hledger-check-tagfiles.cabal.hs   - the above as a cabal script
- hledger-swap-dates.hs             - print transactions with their date and date2 fields swapped
- hledger-print-location.hs         - add file path/line number tags to the print command
- hledger-balance-as-budget.hs      - use one balance report as budget goals for another one
- hledger-combine-balances.hs       - show balance reports for two different periods side by side
- hledger-smooth.hs                 - incomplete attempt at automatically splitting infrequent/irregular transactions
- hledger-check-fancyassertions.hs  - check more complex account balance assertions


[add-on commands]: http://hledger.org/hledger.html#add-on-commands
[stack]: https://www.fpcomplete.com/haskell/get-started
[cabal]: https://www.haskell.org/cabal

How to:

### Install all scripts as add-on commands

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
