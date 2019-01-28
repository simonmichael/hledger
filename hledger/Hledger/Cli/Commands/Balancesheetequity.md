balancesheetequity, bse\
Just like [balancesheet](#balancesheet), but also reports Equity
(which it assumes is under a top-level `equity` account).

_FLAGS_

Example:
```shell
$ hledger balancesheetequity
Balance Sheet With Equity

Assets:
                 $-2  assets
                  $1    bank:saving
                 $-3    cash
--------------------
                 $-2

Liabilities:
                  $1  liabilities:debts
--------------------
                  $1

Equity:
		  $1  equity:owner
--------------------
		  $1

Total:
--------------------
                   0
```
