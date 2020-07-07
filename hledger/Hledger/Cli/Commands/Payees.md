payees\
List the unique payee/payer names that appear in transactions.

_FLAGS

This command lists the unique payee/payer names that appear in transactions,
in alphabetic order. 
You can add a query to select a subset of transactions.
The payee/payer is the part of the transaction description before a | character
(or if there is no |, the whole description).

Example:
```shell
$ hledger payees
Store Name
Gas Station
Person A
```
