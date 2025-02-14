#!/usr/bin/env hledger run
# (or env -S if needed)

REPORTS 1
---------

echo
FILES:
files

echo
STATS:
stats

echo
BALANCESHEET:
bs --depth 2

echo
