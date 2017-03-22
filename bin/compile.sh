#!/bin/sh
# Run this script to compile all addons in this directory.
cd "$(dirname "$0")"
echo "building dependencies"
stack build hledger
stack install Chart Chart-diagrams colour Diff here  # additional deps needed by addons
echo "building add-on commands"
for f in hledger-*.hs; do stack ghc -- -Wall -Werror $f; done
echo "add-on commands available:"
ls -F hledger-* | grep -vE '\.(hs|hi|o|dyn_hi|dyn_o)'
