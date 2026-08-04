## holdings

Show a report of investment holdings (lot-tracked assets).

```flags
Flags:
  -l --flat                 list/tree mode: show accounts as a flat list
                            (default). Amounts exclude subaccount amounts,
                            except where the account is depth-clipped.
  -t --tree                 list/tree mode: show accounts as a tree. Amounts
                            include subaccount amounts.
  -N --no-total             omit the final total row
     --round=TYPE           how much rounding or padding should be done when
                            displaying amounts ?
                            none - show original decimal digits
                            soft - just add or remove decimal zeros
                                   to match precision
                            hard - round amounts to precision (default)
                            all  - also round cost amounts to precision
```

This command is a work in progress.

It shows the assets held in lot-tracked accounts (see [Lots](#lots))
as of the report end date: one row per account, or one row per lot
with `--lots`. Columns show each holding's acquisition date and age
(when the row's lots share a single date), the quantity held,
the unit cost (or average cost, on rows aggregating multiple lots),
the total cost basis, the current market price, the market value,
and the unrealised gain (absolute and percent).

Market prices at the report date come from
[P directives](#p-directives), and from transaction costs with
`--infer-market-prices`, as usual; holdings are valued in their cost
commodity when possible. When a holding has no market price,
its Price, Value and Gain columns are left blank.
The general `-B`/`-V`/`-X`/`--value` flags are ignored.
Amounts are displayed with their commodity's display precision
(unlike lot names, which can show more precision);
`--round` can select another rounding strategy.

Not yet implemented: tree mode; output formats other than text.
