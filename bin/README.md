Miscellaneous hledger add-ons, bash scripts, example make rules, etc. 

## hledger scripts

The hledger-*.hs scripts here are example/experimental hledger [add-on commands].
See <https://hledger.org/scripting.html> for more about this.

They are mostly implemented as [stack] scripts; if you have stack in your $PATH,
they should just work, automatically installing their dependencies if needed.
(You can also run them with [cabal], or runghc, or compile them with
ghc, if you take care of the dependencies yourself.)

Scripts overview, simplest first:

- hledger-check-tag-files.hs        - check that all tag values containing / exist as file paths
- hledger-check-tag-files.cabal.hs  - the above as a cabal script
- hledger-swap-dates.hs             - print transactions with their date and date2 fields swapped
- hledger-print-location.hs         - add file path/line number tags to the print command
- hledger-balance-as-budget.hs      - use one balance report as budget goals for another one
- hledger-combine-balances.hs       - show balance reports for two different periods side by side
- hledger-smooth.hs                 - incomplete attempt at automatically splitting infrequent/irregular transactions
- hledger-check.hs                  - check more complex account balance assertions


[add-on commands]: http://hledger.org/hledger.html#add-on-commands
[stack]: https://www.fpcomplete.com/haskell/get-started
[cabal]: https://www.haskell.org/cabal

How to:

### Install all scripts as addon commands

    $ git clone https://github.com/simonmichael/hledger
    # add hledger/bin/ to your $PATH
    $ hledger  # scripts now appear in commands list
    $ hledger-print-location.hs --help  # run script directly
    $ hledger print-location -- --help  # or run it via hledger, -- is needed before script options

### Install a single script without getting hledger source

    $ cd ~/bin  # somewhere in your $PATH
    $ curl -sO https://raw.githubusercontent.com/simonmichael/hledger/master/bin/hledger-check.hs
    $ chmod +x hledger-check.hs
    $ hledger-check.hs --help
    $ hledger check -- --help

### Create a new script

The example scripts follow a template that implements hledger's
standard command line options and help, so it's a good idea to use one
as your starting point. The hledger- naming is not required, but it
causes scripts to show up in the hledger commands list. On unix,
your new script should be marked executable. This should do it:

    # While in the hledger source directory:
    $ cp bin/hledger-swap-dates.hs bin/hledger-foo.hs
    # Customise hledger-foo.hs, at least the command name and help in cmdmode
    $ bin/hledger-foo.hs --help
    foo [OPTIONS]
      My new foo command.
      ...
    $ hledger foo -- --help
    foo [OPTIONS]
      My new foo command.
      ...

### Run ghcid on a script

    # Ensure any extra packages the script imports from are installed in the current package db
    # (running the script auto-installs them, but only in your user package db):
    $ stack install string-qq
    $ stack exec -- ghcid bin/hledger-foo.hs 
    ...
    Ok, one module loaded.
    All good (1 module, at 10:50:48)


### Run ghci on a script

    # Install any extra packages in the current package db, as above:
    $ stack install string-qq
    $ stack ghci bin/hledger-foo.hs 
    ...
    Ok, one module loaded.
    Loaded GHCi configuration from /private/var/folders/r7/f9j9c2zd1k97v47cr84j_qvc0000gn/T/haskell-stack-ghci/d0bde1be/ghci-script
    ghci> 
