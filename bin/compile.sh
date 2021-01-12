#!/bin/sh
# Compile all add-on scripts in this directory.
# Keep synced: compile.sh, scripts*.test, hledger-*.hs ...

cd "$(dirname "$0")" || exit

echo "building hledger libraries for scripts"
stack build hledger

echo "installing extra libraries for scripts"
stack install string-qq

echo "compiling the hledger-* scripts"
for f in `git ls-files 'hledger-*.hs'`; do stack ghc -- "$f"; done
