## holdings

Show a report of investment holdings (lot-tracked assets).

```flags
Flags:
  -l --flat                 list/tree mode: show accounts as a flat list
                            (default). Amounts exclude subaccount amounts,
                            except where the account is depth-clipped.
  -t --tree                 list/tree mode: show accounts as a tree. Amounts
                            include subaccount amounts.
     --no-elide             in tree mode, don't squash boring parent accounts
  -N --no-total             omit the final total row
     --round=TYPE           how much rounding or padding should be done when
                            displaying amounts ?
                            none - show original decimal digits
                            soft - just add or remove decimal zeros
                                   to match precision
                            hard - round amounts to precision (default)
                            all  - also round cost amounts to precision
  -o --output-file=FILE     write output to FILE. A file extension matching
                            one of the above formats selects that format.
```

This command is a work in progress.

It shows the assets held in lot-tracked accounts (see [Lots](#lots))
as of the report end date: one row per account, or one row per lot
with `--lots`. With `--tree`, accounts are shown as a tree, with
parent rows aggregating the lots beneath them; `--depth` limits and
aggregates the displayed rows as usual.
Columns show each holding's acquisition date and age
(when the row's lots share a single date), the quantity held,
the unit cost (or average cost, on rows aggregating multiple lots),
the total cost basis, the current market price, the market value,
and the unrealised gain (absolute and percent).

Market prices at the report date come from
[P directives](#p-directives), and from transaction costs with
`--infer-market-prices`, as usual; holdings are valued in their cost
commodity when possible. When a holding has no market price,
its Price, Value and Gain columns are left blank.

With `-V`, `-X COMM` or `--value` ([Valuation](#valuation)), holdings
are valued in the default or given valuation commodity instead, and the
cost columns are also converted to it (at the valuation date, so percent
gain is unaffected). `--value=then` is not supported, and `-B/--cost`
has no effect.
Amounts are displayed with their commodity's display precision
(unlike lot names, which can show more precision);
`--round` can select another rounding strategy.

Not yet implemented: output formats other than text.
