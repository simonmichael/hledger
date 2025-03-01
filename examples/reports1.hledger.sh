#!/usr/bin/env hledger run
# (or env -S if needed)

echo REPORTS 1
echo ---------

echo
echo FILES:
files

echo
echo STATS:
stats

echo
echo BALANCESHEET:
bs --depth 2

echo
