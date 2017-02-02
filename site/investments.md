# Track investments

A simple example using [prices](/journal.html#prices):

```journal
2017/1/1 opening balances
  (assets:depot)  $3000

2017/1/2 buy shares at $200
  ; let's assume no fees
  assets:shares   10 TSLA @ $200  ; transaction/purchase price
  assets:depot

; market price, has jumped since yesterday's purchase!
P 2017/1/3 TSLA $250
```

Some reports.
We start with $3000.
After the 1/2 purchase, we have $1000 remaining and 10 TSLA shares:
```shell
$ hledger -f t.j bal assets --flat -HD
Ending balances (historical) in 2017/01/01-2017/01/02:

               ||  2017/01/01     2017/01/02 
===============++============================
 assets:depot  ||       $3000          $1000 
 assets:shares ||           0        10 TSLA 
---------------++----------------------------
               ||       $3000 $1000, 10 TSLA 
```

Show the shares's value at cost, with [`-B/--cost`](/hledger.html#reporting-options):
```shell
$ hledger -f t.j bal assets --flat -HD -B
Ending balances (historical) in 2017/01/01-2017/01/02:

               ||  2017/01/01  2017/01/02 
===============++=========================
 assets:depot  ||       $3000       $1000 
 assets:shares ||           0       $2000 
---------------++-------------------------
               ||       $3000       $3000 
```

Show the shares's value using the latest applicable market price, 
with [`-V/--value`](/hledger.html#market-value).
A $500 capital gain is apparent in the totals:
```shell
$ hledger -f t.j bal assets --flat -HD -V
Ending balances (historical) in 2017/01/01-2017/01/02:

               ||  2017/01/01  2017/01/02 
===============++=========================
 assets:depot  ||       $3000       $1000 
 assets:shares ||           0       $2500 
---------------++-------------------------
               ||       $3000       $3500 
```

This is about the limit of hledger's 
[value-reporting](https://github.com/simonmichael/hledger/issues/131) 
[abilities](https://github.com/simonmichael/hledger/issues/329), 
currently.
