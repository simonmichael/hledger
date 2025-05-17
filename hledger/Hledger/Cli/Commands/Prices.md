## prices

Print the [market prices](hledger.md#p-directive) declared with P directives.
With --infer-market-prices, also show any additional prices inferred from [costs](hledger.md#costs).
With --show-reverse, also show additional prices inferred by reversing known prices.

```flags
Flags:
     --show-reverse         also show the prices inferred by reversing known
                            prices
```

Price amounts are always displayed with their full precision,
except for reverse prices which are limited to 8 decimal digits.

Prices can be filtered by a date:, cur: or amt: query.

Generally if you run this command with --infer-market-prices --show-reverse,
it will show the same prices used internally to calculate value reports.
But if in doubt, you can inspect those directly by running the value report
with --debug=2.

