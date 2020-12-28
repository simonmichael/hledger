#!/bin/sh
# Compile all add-on scrips in this directory.

cd "$(dirname "$0")" || exit

echo "building hledger libraries"
stack build hledger

echo "installing extra libraries needed by scripts"
stack install string-qq

echo "compiling hledger-* scripts"
for f in hledger-*.hs; do stack ghc -- "$f"; done
  # stack script --compile would install extra libs more automatically
  # but would also run scripts, which we don't want
