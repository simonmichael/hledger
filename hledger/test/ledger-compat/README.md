Ledger compatibility tests, focussed on reading journal files for now.
See https://github.com/simonmichael/hledger/issues/1962

baseline/ and regress/ are a snapshot of Ledger's functional tests,
with all but the sample data commented, converted to shelltests which
test reading.

test runs all tests and saves a dated pretty log which can be browsed
with org mode.
