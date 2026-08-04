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
  -S --sort-amount          sort by value (or cost) instead of account name,
                            largest first
  -N --no-total             omit the final total row
     --round=TYPE           how much rounding or padding should be done when
                            displaying amounts ?
                            none - show original decimal digits
                            soft - just add or remove decimal zeros
                                   to match precision
                            hard - round amounts to precision (default)
                            all  - also round cost amounts to precision
  -O --output-format=FMT    select the output format. Supported formats:
                            txt, csv, tsv, html, fods, json.
  -o --output-file=FILE     write output to FILE. A file extension matching
                            one of the above formats selects that format.
```

This command is a work in progress.

It shows the assets held in lot-tracked accounts (see [Lots](#lots))
as of the report end date: one row per account, or one row per lot
with `--lots`. With `--tree`, accounts are shown as a tree, with
parent rows aggregating the lots beneath them; `--depth` limits and
aggregates the displayed rows as usual.
With `-S/--sort-amount`, rows are sorted by market value (or by cost,
when unpriced), largest first.
Columns show each holding's acquisition date and age
(when the row's lots share a single date; ages are shown in days, or
from one year in years with one decimal digit, eg `44d` or `1.1y`,
approximating years as 365 days),
the quantity held,
the unit cost (or average cost, on rows aggregating multiple lots),
the total cost basis, the current market price, the market value,
the percentage of the portfolio's total value (Weight),
the unrealised gain (absolute and percent),
the realised gain from disposals so far (Rgain),
and the annualised internal rate of return (XIRR, calculated from the
account's dated cashflows and current value, like roi's IRR;
it includes realised gains).
In the totals row, Rgain and XIRR are account-level: they also include
fully disposed lots, which have no row of their own (eg with `--lots`).

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

With `-O csv` or `-O tsv`, machine-readable output is produced instead:
one record per row and commodity, with full account names, age in days,
bare quantity and gain percent numbers, gain and gain percent as
separate fields, and no totals records.
Amounts are shown without digit group marks; as in other commands'
CSV output, the decimal mark follows the commodity's display style.
(Note in tree mode, parent account records repeat the data of their
subaccounts.)

With `-O html`, an HTML table is produced: like the text table,
but with single-line cells. For styling, each cell has a css class
naming its column (`account`, `date`, `age`, `quantity`, `unitcost`,
`cost`, `price`, `value`, `gain`; totals row cells also have
`coltotal`), and each commodity amount is enclosed in a span with
class `amount` (eg allowing wrapping within amounts to be prevented).

With `-O fods`, a spreadsheet document readable by LibreOffice etc.
is produced, with the same single-line cells as the html output.

With `-O json`, a JSON array of holding objects is produced, with the
same fields as the CSV output; quantities and gain percents are
JSON number objects as in other commands' JSON output, and missing
values are null.
