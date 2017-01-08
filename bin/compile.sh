#!/bin/sh
cd "$(dirname "$0")"
echo "building dependencies"
stack build hledger
echo "building add-on commands"
for f in hledger-*.hs; do stack ghc $f; done
echo "add-on commands available:"
ls -F hledger-* | grep -vE '\.(hs|hi|o)'
