Miscellaneous hledger add-ons, bash scripts, example make rules, etc. 

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

## Installing a single script

    $ curl -sO https://raw.githubusercontent.com/simonmichael/hledger/master/bin/hledger-check.hs
    $ chmod +x hledger-check.hs
    $ ./hledger-check.hs --help

If you put the script somewhere in your $PATH, it will also show up as a hledger command,
so this also works:

    $ hledger check -- --help

Note the `--`, which is required to separate script options from hledger options:

    $ hledger [HLEDGEROPTS] ADDONCMD [-- ADDONOPTS]

## Installing all scripts

    $ git clone https://github.com/simonmichael/hledger
    $ # add hledger/bin/ to your $PATH
    $ hledger  # addons appear in command list



stack exec -- ghcid bin/hledger-import-shared-expenses.hs 

stack ghci bin/hledger-import-shared-expenses.hs 
