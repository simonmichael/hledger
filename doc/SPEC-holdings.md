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
- The totals row shows only the commodity-independent columns:
  Cost, Value, Gain.
- Possible future columns: portfolio weight %, realised gain, XIRR.

## Valuation

- The report date is the report end date (today by default, or set with `-e`).
- Prices are market prices at the report date, from P directives and/or
  inferred from transaction costs, using the standard `--value` infrastructure.
  Value/Gain columns behave like `--value=end`.

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

- The table is a single-period MultiBalanceReport-shaped table: rows from the
  standard account-tree display machinery, but columns are per-row attributes
  instead of periods. Rendering via Text.Tabular.AsciiWide as in Balance.hs.
- Cell data comes from lot state (as computed by journalCalculateLots) plus
  market prices.

## Phases

1. Layout mockup: skeleton `holdings` command printing the sample layout above. (current)
2. Real single-attribute report: rows from the journal's lotful accounts, with
   Quantity and Cost columns.
3. Valuation columns: Price, Value, Gain.
4. Date/Age columns, `--lots`/`--tree`/`--depth` behavior, totals row.
5. Output formats, sorting, extra columns.
