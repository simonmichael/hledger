diff\
Compares two journal files.  It looks at the transactions of a single
account and prints out the transactions which are in one journal file but not
in the other.

This is particularly useful for reconciling existing journals with bank
statements.  Many banks provide a way to export the transactions between two
given dates, which can be converted to ledger files using custom scripts or
read directly as CSV files.  With the diff command you can make sure that these
transactions from bank match up exactly with the transactions in your ledger
file, and that the resulting balance is correct.  (One possible concrete
workflow is to have one ledger file per year and export the transactions for
the current year, starting on January 1.)

This command compares the postings of a single account (which needs to have the
same name in both files), and only checks the amount of the postings (not the
name or the date of the transactions).  Postings are compared (instead of
transactions) so that you can combine multiple transactions from the bank
statement in a single transaction in the ledger file.

_FLAGS_

Examples:

```shell
$ hledger diff assets:bank:giro -f 2014.journal -f bank.journal
Unmatched transactions in the first journal:

2014/01/01 Opening Balances
    assets:bank:giro              EUR ...
    ...
    equity:opening balances       EUR -...

Unmatched transactions in the second journal:
```
