#!/bin/bash

function cmd {
    if [ -x dist/build/hledger-irr/hledger-irr ]
    then
        echo ' > $ hledger-irr' $1
        eval dist/build/hledger-irr/hledger-irr $1 | sed -e 's/^/ > /'
    else
        echo "dist/build/hledger-irr/hledger-irr missing, rerun $0 to update description" >&2
    fi
}

VERSION="$(darcs show tag | head -n 1)"

rm -f hledger-irr.cabal
exec >hledger-irr.cabal

cat <<__END__
Name:                   hledger-irr
Version:                ${VERSION:-0.0}
Synopsis:               computes the internal rate of return of an investment
License:                BSD3
License-file:           LICENSE
Author:                 Joachim Breitner <mail@joachim-breitner.de>
Maintainer:             Joachim Breitner <mail@joachim-breitner.de>
Category:               Finance
Build-type:             Simple
Cabal-version:          >= 1.6
Tested-with:            GHC >= 7.4.1 && <= 7.4.1
Description:
 hledger-irr is a small command-line utility based on Simon
 Michael's hleder library. Its purpose is to compute the internal rate of
 return, also known as the effective interest rate, of a given investment.
 After specifying what account holds the investment, and what account stores
 the gains (or losses, or fees, or cost), it calculates the hypothetical
 annualy rate of fixed rate investment that would have provided the exact same
 cash flow.
 .
 As an example, consider the following irregular investment recorded in a file
 called @speculation.ledger@. The account “Speculation” holds the investment which
 could be, for example, a stock. Regularly, we make sure that the value of the
 account matches the value of the stock, by moving money from or to the account
 “Rate Gain”. It does not really matter when we adjust the price, as long as it
 is correct at the end of our reporting period.
 .
__END__
cat speculation.ledger | sed -e 's/^/ > /'
cat <<__END__
 .
 We can now calculate the rate of return for the whole time or just for parts
 of it (and be freaked out by the volatility of the investment):
 .
__END__
cmd '-f speculation.ledger -t "Rate Gain" -i Speculation -c'
cmd '-f speculation.ledger -t "Rate Gain" -i Speculation -e 2011-03-01'
cmd '-f speculation.ledger -t "Rate Gain" -i Speculation -b 2011-03-01'
cmd '-f speculation.ledger -t "Rate Gain" -i Speculation --monthly'
cat <<__END__
 .
 Running the utility with @--help@ gives a brief overview over the
 available options:
 .
__END__
cmd '--help'
cat <<__END__
 .
 Known bugs and issues:
 .
 * Currenlty, hledger-irr does not cope well with multiple commodities (e.g.
   Euro and Dollar, or shares).
 .
 * Also, interest or fees that do not pass through the account selected by
   @--investment-account@ are not taken into consideration.

Executable hledger-irr
  Main-is:              Main.hs
  Build-depends:        base >= 3 && < 5, hledger-lib == 0.20.* || == 0.21.* || == 0.22.*, time, Cabal, statistics >= 0.10
  Ghc-Options:          -Wall

source-repository head
    type:     darcs
    location: http://darcs.nomeata.de/hledger-irr

__END__

chmod -w hledger-irr.cabal
