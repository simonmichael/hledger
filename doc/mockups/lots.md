# Lots, again

2023-01

I believe one could emulate most of ledger/beancount's lot tracking/selection with simpler syntax, just @.

Eg, with today's explicit lot subaccounts:

```journal
2022-01-01 buy at 10
  assets:aaa:_20220101     10 AAA @ $10
  assets:cash           -$100
    
2022-02-01 buy at 12
  assets:aaa:_20220201     10 AAA @ $12
  assets:cash           -$120
    
2022-03-01 sell at 15
  assets:aaa:_20220101    -10 AAA @ $10  ; original cost basis
  assets:aaa:_20220201     -5 AAA @ $12
  assets:cash            $225
  revenues:gains         $-65
```

Or with support for automatic or no lot subaccounts:

```journal
2022-01-01 buy at 10
  assets:aaa     10 AAA @ $10  ; creates a new lot subaccount automatically, perhaps hidden by default
  assets:cash           -$100
    
2022-02-01 buy at 12
  assets:aaa     10 AAA @ $12  ; or just a new lot internally, with special commands to show it, like ledger/beancount
  assets:cash           -$120
    
2022-03-01 sell at 15
  assets:aaa    -10 AAA @ $10  ; cost selects the lot withdrawn from
  assets:aaa     -5 AAA @ $12  ; other selectors like date/note would be possible
  assets:cash            $225
  revenues:gains         $-65
```

I don't see much need for {}.
