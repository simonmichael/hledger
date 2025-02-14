#!/usr/bin/env hledger run
# (or env -S if needed)

REPORTS 2
---------

echo
List of accounts
accounts --depth 2

echo
Assets
balance assets --depth 2

echo
Liabilities
balance liabilities --depth 3

echo
