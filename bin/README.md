Miscellaneous hledger add-ons, bash scripts, example make rules, etc. 
The Makefile may also store hledger developer binaries here.

hledger-*.hs are example/experimental hledger add-on commands, 
shipped as executable stack scripts:

- hledger-check.hs      - check more complex account balance assertions
- hledger-smooth.hs     - an attempt at automatically splitting infrequent/irregular transactions
- hledger-swap-dates.hs - print transactions with their date and date2 fields swapped

You can run them directly and they will install required dependencies 
and run in interpreted mode. 

Or you can compile them to run faster using `stack ghc SCRIPT` or
`bin/compile.sh`.

Add this directory to $PATH and they will show up in hledger's commands list.

A reminder from http://hledger.org/hledger.html#add-on-commands :
when using the main hledger executable to run add-on commands, remember
to put a -- before the add-on's options, or hledger will complain. Eg, do:

    $ hledger [HLEDGEROPTS] ADDONCMD [-- ADDONOPTS]
