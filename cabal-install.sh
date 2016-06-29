#!/usr/bin/env bash
# Run this script to install all hledger packages using cabal
# (if you prefer using cabal to stack)

# make a sandbox in this directory to avoid build problems
cabal sandbox init

# might be useful for some developers..
cabal sandbox add-source ./hledger-lib
cabal sandbox add-source ./hledger
cabal sandbox add-source ./hledger-ui
cabal sandbox add-source ./hledger-web
cabal sandbox add-source ./hledger-api

# build and install
cabal install ./hledger-lib ./hledger ./hledger-ui ./hledger-web ./hledger-api \
  && echo "hledger executables successfully installed in ./.cabal-sandbox/bin"
