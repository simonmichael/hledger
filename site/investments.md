# Track investments

Hledger can be used to track stock investments.
In fact, the double-entry accounting is flexible enough to support most constellations you will come across.
However, you may find that some transactions could be better supported.
Caveats are:
- hledger does not validate the cost basis during a sale.
- historical mark-to-market performance is not supported
  (but the market value at one instant, like today, can be calculated)

## Example
### Buying a stock

Let's go over a simple example using [prices](/journal.html#prices):

```journal
2017/1/1 opening balance
  (assets:depot)  $3000

2017/1/2 buy shares at $200
  ; let's assume no fees
  assets:shares   10 TSLA @ $200  ; transaction/purchase price
  assets:depot

```

Some reports.
We start with $3000.
After the 1/2 purchase, we have $1000 remaining and 10 TSLA shares:
```shell
$ hledger -f t.j bal --flat -HD
Ending balances (historical) in 2017/01/01-2017/01/02:

               ||  2017/01/01     2017/01/02
===============++============================
 assets:depot  ||       $3000          $1000
 assets:shares ||           0        10 TSLA
---------------++----------------------------
               ||       $3000 $1000, 10 TSLA
```

Show the shares' value at cost, with [`-B/--cost`](/hledger.html#reporting-options):
```shell
$ hledger -f t.j bal --flat -HD -B
Ending balances (historical) in 2017/01/01-2017/01/02:

               ||  2017/01/01  2017/01/02
===============++=========================
 assets:depot  ||       $3000       $1000
 assets:shares ||           0       $2000
---------------++-------------------------
               ||       $3000       $3000
```

## Value reporting
Add the following to the journal file.
```journal
; market price, has jumped since yesterday's purchase!
P 2017/1/3 TSLA $250
```

Show the shares's value using the latest applicable market price,
with [`-V/--value`](/hledger.html#market-value).
A $500 capital gain is apparent in the totals:
```shell
$ hledger -f t.j bal --flat -HD -V
Ending balances (historical) in 2017/01/01-2017/01/02:

               ||  2017/01/01  2017/01/02
===============++=========================
 assets:depot  ||       $3000       $1000
 assets:shares ||           0       $2500
---------------++-------------------------
               ||       $3000       $3500
```

There are still limitations in the value reporting that hledger can currently do.
More information can be found in [Github issue #131](https://github.com/simonmichael/hledger/issues/131) and [Github issue #329](https://github.com/simonmichael/hledger/issues/329).

You may want to investigate the output after adding more prices to the journal file.
```journal
P 2017/1/1 TSLA $210
P 2017/1/4 TSLA $250
P 2017/1/8 TSLA $270
```

### Selling a stock and tracking capital gains
At some point you will probably sell shares.
It may seem intuitive to model such a sale as follows.
```journal
2017/1/4 sell shares at $250      ; NOTE: You probably want to model capital gains too; see below
  assets:shares   -10 TSLA @ $250  ; sell price
  assets:depot
```

This leads to the following evolution
```shell
hledger -f t.j balance --flat -HD
Ending balances (historical) in 2017/01/01-2017/01/04:

               ||  2017/01/01     2017/01/02     2017/01/03  2017/01/04
===============++=======================================================
 assets:depot  ||       $3000          $1000          $1000       $3500
 assets:shares ||           0        10 TSLA        10 TSLA           0
---------------++-------------------------------------------------------
               ||       $3000 $1000, 10 TSLA $1000, 10 TSLA       $3500
```

You end up with the correct amount in your depot.
At some point, however, you will have to report the capital gain that you realized with your sale.
This gain is currently invisible.
In fact, we have violated the double-entry principle and created money out of nowhere.

Let's report our sale in a different way.
```journal
2017/1/4 sell shares at $250
  assets:shares         -10 TSLA @ $200  ; cost basis (must be tracked by user!)
  assets:depot          $2500            ; cash proceeds
  revenue:capital_gains                  ; deduce profit
```

Now, the new $500 are correctly balanced with the capital gains account.
```shell
hledger -f t.j balance --flat -HD
Ending balances (historical) in 2017/01/01-2017/01/04:

                       ||  2017/01/01     2017/01/02     2017/01/03  2017/01/04
=======================++=======================================================
 assets:depot          ||       $3000          $1000          $1000       $3500
 assets:shares         ||           0        10 TSLA        10 TSLA           0
 revenue:capital_gains ||           0              0              0       $-500
-----------------------++-------------------------------------------------------
                       ||       $3000 $1000, 10 TSLA $1000, 10 TSLA       $3000
```

## Further reading

- Beancount guides (general double-entry accounting advice from another tool)
    - [Cookbook][beancount_cookbook]: Account naming, basic trading transactions
    - [Trading guide][beancount_trading]: More complicated trading transactions, discussion on tricky cost basis adjustments
- [Github #624 on investment tracking](https://github.com/simonmichael/hledger/issues/624)
- [Discussion on investment modeling from the mailing list](https://groups.google.com/forum/#!topic/hledger/e8Ss7ZL4ADI)

[beancount_cookbook]: http://furius.ca/beancount/doc/cookbook
[beancount_trading]: http://furius.ca/beancount/doc/trading
