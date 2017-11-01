#!/bin/sh
# Run this script (or "make addons") to compile all addons in this directory.
cd "$(dirname "$0")"
echo "building dependencies"
stack build hledger
# additional deps needed by addons
stack install Diff here #Chart Chart-diagrams colour 
echo "building add-on commands"
for f in hledger-*.hs; do stack ghc -- -Wall -Werror $f; done
