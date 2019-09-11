#!/usr/bin/env bash
# Install all hledger packages using cabal (if you prefer it to stack).
# cabal.project may be a newer way.

# Ensure cabal sees the latest available packages
cabal update

# Make a local sandbox - optional, recommended for reliable installs
cabal sandbox init

# Tell sandbox about the local packages, recommended for developers
#cabal sandbox add-source ./hledger-lib
#cabal sandbox add-source ./hledger
#cabal sandbox add-source ./hledger-ui
#cabal sandbox add-source ./hledger-web

# Traditional pre-ceremony to propitiate the install gods
cabal install alex happy

# Build and install to ./cabal-sandbox-bin (or ~/.cabal/bin without a sandbox)
cabal install \
  ./hledger-lib \
  ./hledger \
  ./hledger-ui \
  ./hledger-web \
