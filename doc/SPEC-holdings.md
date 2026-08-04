# holdings command

Specification / design notes for the `holdings` command, which shows a standard
report of investment holdings (lotful assets).

See also
- SPEC-lots.md
- hledger manual: Lot reporting

Status: phase 1, layout mockup.

## Goal

A standard, convenient report answering "what investments do I hold right now,
what did they cost, what are they worth, how are they doing ?".

Functionally it is like a balancesheet report that is always single-period,
showing one row per holding and several useful attribute columns.
It should reuse the standard balance report machinery, supporting the usual
row-display modes: `--list` (default), `--tree`, `--depth`, `--drop`,
`--alias`, sorting, and the standard output formats (txt, csv, tsv, html,
json...).

## Rows

- Rows are the accounts holding lotful commodities (ie accounts with lot
  subaccounts), plus any query arguments to narrow them further.
  Cash and other non-lotful accounts don't appear.
- Lot subaccounts follow the standard `--lots` display toggle:
  hidden (aggregated into their base account) by default,
  shown as rows with `--lots`.
- A totals row is shown at the bottom (disable with `-N/--no-total`).

## Columns

| Column    | Meaning                                                        |
|-----------|----------------------------------------------------------------|
| Date      | the lot's acquisition date                                     |
| Age       | how long the lot has been held, as of the report date          |
| Quantity  | number of units held                                           |
| Unit cost | cost basis per unit ("Avg cost" on rows aggregating lots)      |
| Cost      | total cost basis                                               |
| Price     | current market price per unit                                  |
| Value     | current market value (Quantity x Price)                        |
| Gain      | unrealised gain: Value - Cost, absolute and percent            |

Notes:
- On rows aggregating multiple lots, Date and Age are blank,
  and Unit cost shows the average cost (column titled "Avg cost";
  it is titled "Unit cost" when `--lots` is in effect).
  (Alternatives considered for aggregated Date/Age: oldest lot's date/age,
  a date range, quantity-weighted average age.)
- Age is shown in days, eg `75d`. (Could later be humanised, eg `2m14d` or
  `1y3m`, and/or a long/short-term capital gains indicator could be added.)
- Rows with no known market price show blank Price, Value and Gain,
  rather than pretending the gain is zero.
- Amounts are displayed normalised to their commodity's display precision
  by default (unlike lot names, which can show more precision);
  `--round` can select another rounding strategy (default: hard).
- The totals row shows only the commodity-independent columns:
  Cost, Value, Gain.
- Possible future columns: portfolio weight %, realised gain, XIRR.

## Valuation

- The report date is the report end date (today by default, or set with `-e`).
- Prices are market prices at the report date, from P directives, and from
  transaction costs with `--infer-market-prices`, looked up with the standard
  price oracle. Each holding is valued in its cost commodity when possible
  (so Gain = Value - Cost is meaningful); otherwise in the default valuation
  commodity.
- Gain (absolute and percent) is shown when the value and cost are in a
  single common commodity.
- `-V`/`-X COMM`/`--value=end|now|DATE[,COMM]` select the valuation
  commodity and/or valuation date. The cost columns (Cost, Unit/Avg cost,
  and the cost side of Gain) are then also converted to the valuation
  commodity at the valuation date, so percent gain is unaffected by
  currency conversion. Costs with no market price to the valuation
  commodity are left unconverted (making Gain blank).
- `--value=then` is not supported (holdings is a snapshot report).
- `-B/--cost` has no effect; quantities always stay quantities.

## Layout mockups

Scenario: two AAPL buys in assets:broker:stocks, one MSFT buy in
assets:broker:funds, a FIFO sale of 5 AAPL, and P directives
(AAPL $72, MSFT $410) on the report date 2026-03-31.

Default (list mode, lot subaccounts hidden):

```
$ hledger holdings
Holdings on 2026-03-31

                      ||       Date  Age  Quantity  Avg cost   Cost  Price  Value           Gain
======================++========================================================================
 assets:broker:funds  || 2026-02-15  44d    5 MSFT   $400.00  $2000   $410  $2050    $50  (+2.5%)
 assets:broker:stocks ||                   15 AAPL    $56.67   $850    $72  $1080   $230 (+27.1%)
----------------------++------------------------------------------------------------------------
                      ||                                      $2850         $3130   $280  (+9.8%)
```

(assets:broker:funds holds a single lot, so its Date/Age are shown even though
lots are hidden; assets:broker:stocks aggregates two lots, so they are blank.)

With `--lots` (lot subaccounts become rows; Avg cost becomes exact Unit cost):

```
$ hledger holdings --lots
Holdings on 2026-03-31

                                        ||       Date  Age  Quantity  Unit cost   Cost  Price  Value           Gain
========================================++=========================================================================
 assets:broker:funds:{2026-02-15, $400} || 2026-02-15  44d    5 MSFT       $400  $2000   $410  $2050    $50  (+2.5%)
 assets:broker:stocks:{2026-01-15, $50} || 2026-01-15  75d    5 AAPL        $50   $250    $72   $360   $110 (+44.0%)
 assets:broker:stocks:{2026-02-01, $60} || 2026-02-01  58d   10 AAPL        $60   $600    $72   $720   $120 (+20.0%)
----------------------------------------++-------------------------------------------------------------------------
                                        ||                                       $2850         $3130   $280  (+9.8%)
```

With `--lots --tree` (parent rows aggregate; multi-commodity cells go
multi-line as in bal):

```
$ hledger holdings --lots --tree
Holdings on 2026-03-31

                          ||       Date  Age  Quantity  Unit cost   Cost  Price  Value           Gain
==========================++=========================================================================
 assets                   ||                  15 AAPL              $2850         $3130   $280  (+9.8%)
                          ||                   5 MSFT
   broker                 ||                  15 AAPL              $2850         $3130   $280  (+9.8%)
                          ||                   5 MSFT
     funds                || 2026-02-15  44d   5 MSFT       $400  $2000   $410  $2050    $50  (+2.5%)
       {2026-02-15, $400} || 2026-02-15  44d   5 MSFT       $400  $2000   $410  $2050    $50  (+2.5%)
     stocks               ||                  15 AAPL     $56.67   $850    $72  $1080   $230 (+27.1%)
       {2026-01-15, $50}  || 2026-01-15  75d   5 AAPL        $50   $250    $72   $360   $110 (+44.0%)
       {2026-02-01, $60}  || 2026-02-01  58d  10 AAPL        $60   $600    $72   $720   $120 (+20.0%)
```

With `--depth 2` (aggregation up the tree; per-unit and per-lot columns blank
where meaningless):

```
$ hledger holdings --depth 2
Holdings on 2026-03-31

                || Quantity   Cost  Value          Gain
================++=====================================
 assets:broker  ||  15 AAPL  $2850  $3130  $280 (+9.8%)
                ||   5 MSFT
```

## Implementation notes

- The holdings command receives the journal with lot detail uncollapsed,
  regardless of --lots (see maybeCollapseLotDetail in Hledger.Cli.Utils);
  it aggregates lots itself.
- Rows come from a single-period, end-balances (Historical) multiBalanceReport:
  run on the lot-detailed journal with --lots (rows are lot subaccounts),
  or on the collapsed journal otherwise (rows are the base accounts).
  Rows without lots beneath them are filtered out.
  (balanceReport was considered but it is just a thin projection of
  multiBalanceReport; MBR keeps the row structure, totals and valuation
  machinery we need.)
- Per-lot quantities are summed from the lot subaccounts' postings
  (amount arithmetic discards cost basis, so balances alone don't suffice).
  Each lot's cost basis is parsed back from the lot subaccount name, which by
  construction contains the acquisition date and unit cost.
- Rendering via Text.Tabular.AsciiWide as in Balance.hs.

## Phases

1. Layout mockup: skeleton `holdings` command printing the sample layout above. (done)
2. Real report in list mode: rows from the journal's lotful accounts, with
   Date, Age, Quantity, Unit/Avg cost and Cost columns; --lots; totals row;
   functional tests (hledger/test/holdings.test). --tree errors out. (done)
3. Valuation columns: Price, Value, Gain, with market prices from the
   standard price oracle; blank when no market price is known. (done)
4. `--tree`/`--depth` behavior.
5. Output formats, sorting, extra columns.
