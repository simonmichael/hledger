#!/usr/bin/env hledger run
# (or env -S if needed)

echo REPORTS 2
echo ---------

echo
echo List of accounts
accounts --depth 2

echo
echo Assets
balance assets --depth 2

echo
echo Liabilities
balance liabilities --depth 3

echo
