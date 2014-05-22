cd hledger
cabal sandbox init
cabal sandbox add-source ../hledger-lib
cabal sandbox add-source ../hledger-web

cabal install --dependencies-only
cabal configure
cabal build
