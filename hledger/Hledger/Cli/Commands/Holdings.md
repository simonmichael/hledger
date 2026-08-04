## holdings

Show a report of investment holdings (lot-tracked assets).

```flags
Flags:
  -l --flat                 list/tree mode: show accounts as a flat list
                            (default). Amounts exclude subaccount amounts,
                            except where the account is depth-clipped.
  -t --tree                 list/tree mode: show accounts as a tree. Amounts
                            include subaccount amounts.
```

This command is a work in progress; currently it shows a mockup of the
planned layout, with sample data.

It will show the assets held in lot-tracked accounts (see [Lots](#lots)):
one row per account (or per lot, with `--lots`), and columns showing
each holding's acquisition date, age, quantity, cost basis,
current market price, market value, and unrealised gain.
