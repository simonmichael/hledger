# holdings command

Specification / design notes for the `holdings` command, which shows a standard
report of investment holdings (lotful assets).

See also
- SPEC-lots.md
- hledger manual: Lot reporting

Status: implemented; see Phases below.

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
| Units     | number of units held                                           |
| Unit cost | cost basis per unit ("Avg cost" on rows aggregating lots)      |
| Cost      | total cost basis                                               |
| Price     | current market price per unit                                  |
| Value     | current market value (Units x Price)                           |
| Weight    | percentage of the portfolio's total value                      |
| UGain     | unrealised gain: Value - Cost                                  |
| UGain%    | unrealised gain as a percentage of Cost                        |
| RGain     | realised gain from disposals so far                            |
| XIRR      | annualised internal rate of return, like roi's IRR             |

Notes:
- On rows aggregating multiple lots, Date and Age are blank,
  and Unit cost shows the average cost (column titled "Avg cost";
  it is titled "Unit cost" when `--lots` is in effect).
  (Alternatives considered for aggregated Date/Age: oldest lot's date/age,
  a date range, quantity-weighted average age.)
- Age is shown in days, or from one year in years with one decimal digit,
  eg `44d`, `1.1y`, approximating years as 365 days. (A long/short-term
  capital gains indicator could be added later.) The csv/json outputs
  keep age as a number of days.
- Weight is each row's value as a percentage of the portfolio's total
  value; blank unless all displayed holdings are priced in one commodity.
- RGain sums each dispose posting's proceeds minus the cost basis of the
  disposed units, for the lots at or under the row's account. Fully
  disposed lots have no row of their own (eg with --lots), but their
  realised gains are included in the totals row, which computes RGain
  and XIRR from the displayed rows' base accounts - consistent across
  display modes. Fully disposed accounts don't appear in the report at
  all, so neither do their realised gains.
- XIRR solves for the annualised rate of return implied by the account's
  dated cashflows (acquisitions at transacted or basis cost, disposals at
  proceeds) plus its current value, like roi's IRR (using ridders,
  rate**(days/365.25)); it thus includes realised gains. Blank when
  unpriced, when cashflow commodities differ from the value commodity
  (eg under -X), or when unsolvable.
  The final cashflow is the displayed Value, treated as received at the
  report date (even if `--value` priced it at a different date) -
  consistent with the Value and UGain columns.
- Rows with no known market price show blank Price, Value and gain columns,
  rather than pretending the gain is zero.
- Amounts are displayed normalised to their commodity's display precision
  by default (unlike lot names, which can show more precision);
  `--round` can select another rounding strategy (default: hard).
- The percent columns (Weight, UGain%, XIRR) are shown with 1 decimal
  digit, or with the display style of the "%" commodity if any
  (eg by a commodity directive or `-c '0.00 %'`), so their precision and
  symbol placement can be customised. The csv/tsv/json outputs keep
  bare percent numbers.
- The totals row (shown unless -N) shows only the commodity-independent
  columns: Cost, Value, Weight (100%), UGain, UGain%, RGain, XIRR.
- Possible future columns: a long/short-term capital gains indicator.

## Valuation
- Prices are market prices at the report date, from P directives, and from
  transaction costs with `--infer-market-prices`, looked up with the standard
  price oracle. Each holding is valued in its cost commodity when possible
  (so UGain = Value - Cost is meaningful); otherwise in the default valuation
  commodity.
- UGain and UGain% are shown when the value and cost are in a
  single common commodity.
- `-V`/`-X COMM`/`--value=end|now|DATE[,COMM]` select the valuation
  commodity and/or valuation date. The cost columns (Cost, Unit/Avg cost,
  and the cost side of UGain) are then also converted to the valuation
  commodity at the valuation date, so percent gain is unaffected by
  currency conversion. Costs with no market price to the valuation
  commodity are left unconverted (making UGain blank).
- `--value=then` is not supported (holdings is a snapshot report).
- `-B/--cost` has no effect; units always stay units.

## Layout examples

Scenario: two AAPL buys in assets:broker:stocks, one MSFT buy in
assets:broker:funds, a FIFO sale of 5 AAPL at $70, and P directives
(AAPL $72, MSFT $410) on the report date 2026-03-31.

Default (list mode, lot subaccounts hidden):

```
$ hledger holdings
Holdings on 2026-03-31

                      ||       Date  Age    Units  Avg cost   Cost  Price  Value  Weight  UGain  UGain%  RGain    XIRR
======================++===============================================================================================
 assets:broker:funds  || 2026-02-15  44d   5 MSFT      $400  $2000   $410  $2050   65.5%    $50   +2.5%          22.7%
 assets:broker:stocks ||                  15 AAPL    $56.67   $850    $72  $1080   34.5%   $230  +27.1%   $100  419.4%
----------------------++-----------------------------------------------------------------------------------------------
                      ||                                     $2850         $3130  100.0%   $280   +9.8%   $100  137.8%
```

(assets:broker:funds holds a single lot, so its Date/Age are shown even though
lots are hidden; assets:broker:stocks aggregates two lots, so they are blank.
funds has no RGain because nothing was disposed from it; stocks' $100 realised
gain appears on its row and in the totals.)

With `--lots` (lot subaccounts become rows; Avg cost becomes exact Unit cost):

```
$ hledger holdings --lots
Holdings on 2026-03-31

                                        ||       Date  Age    Units  Unit cost   Cost  Price  Value  Weight  UGain  UGain%  RGain    XIRR
========================================++================================================================================================
 assets:broker:funds:{2026-02-15, $400} || 2026-02-15  44d   5 MSFT       $400  $2000   $410  $2050   65.5%    $50   +2.5%          22.7%
 assets:broker:stocks:{2026-01-15, $50} || 2026-01-15  75d   5 AAPL        $50   $250    $72   $360   11.5%   $110  +44.0%   $100  759.2%
 assets:broker:stocks:{2026-02-01, $60} || 2026-02-01  58d  10 AAPL        $60   $600    $72   $720   23.0%   $120  +20.0%         215.2%
----------------------------------------++------------------------------------------------------------------------------------------------
                                        ||                                      $2850         $3130  100.0%   $280   +9.8%   $100  137.8%
```

With `--lots --tree` (parent rows aggregate; multi-commodity cells go
multi-line as in bal; boring parents are squashed as usual):

```
$ hledger holdings --lots --tree
Holdings on 2026-03-31

                              ||       Date  Age    Units  Unit cost   Cost  Price  Value  Weight  UGain  UGain%  RGain    XIRR
==============================++================================================================================================
 assets                       ||                  15 AAPL             $2850    $72  $3130  100.0%   $280   +9.8%   $100  137.8%
                              ||                   5 MSFT                     $410
   broker                     ||                  15 AAPL             $2850    $72  $3130  100.0%   $280   +9.8%   $100  137.8%
                              ||                   5 MSFT                     $410
     funds:{2026-02-15, $400} || 2026-02-15  44d   5 MSFT       $400  $2000   $410  $2050   65.5%    $50   +2.5%          22.7%
     stocks                   ||                  15 AAPL     $56.67   $850    $72  $1080   34.5%   $230  +27.1%   $100  419.4%
       {2026-01-15, $50}      || 2026-01-15  75d   5 AAPL        $50   $250    $72   $360   11.5%   $110  +44.0%   $100  759.2%
       {2026-02-01, $60}      || 2026-02-01  58d  10 AAPL        $60   $600    $72   $720   23.0%   $120  +20.0%         215.2%
------------------------------++------------------------------------------------------------------------------------------------
                              ||                                      $2850         $3130  100.0%   $280   +9.8%   $100  137.8%
```

With `--depth 2` (aggregation up the tree):

```
$ hledger holdings --depth 2
Holdings on 2026-03-31

               || Date  Age    Units  Avg cost   Cost  Price  Value  Weight  UGain  UGain%  RGain    XIRR
===============++=========================================================================================
 assets:broker ||            15 AAPL            $2850    $72  $3130  100.0%   $280   +9.8%   $100  137.8%
               ||             5 MSFT                    $410
---------------++-----------------------------------------------------------------------------------------
               ||                               $2850         $3130  100.0%   $280   +9.8%   $100  137.8%
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
- Per-lot units are summed from the lot subaccounts' postings
  (amount arithmetic discards cost basis, so balances alone don't suffice).
  Each lot's cost basis is parsed back from the lot subaccount name, which by
  construction contains the acquisition date and unit cost.
- Rendering via Text.Tabular.AsciiWide as in Balance.hs.

## Phases

1. Layout mockup: skeleton `holdings` command printing the sample layout above. (done)
2. Real report in list mode: rows from the journal's lotful accounts, with
   Date, Age, Units, Unit/Avg cost and Cost columns; --lots; totals row;
   functional tests (hledger/test/holdings.test). --tree errors out. (done)
3. Valuation columns: Price, Value, Gain, with market prices from the
   standard price oracle; blank when no market price is known. (done)
4. `--tree`/`--depth` behavior: parent rows aggregate the lots beneath
   them, showing only lot-tracked commodities; totals sum only the topmost
   displayed rows, avoiding double counting. (done)
5. Sorting: `-S/--sort-amount` sorts rows by Value (falling back to Cost),
   largest first; tree mode sorts each subtree level, keeping subtrees
   together. (done)
6. CSV/TSV output: one record per row and commodity, with full account
   names, age in days, bare units and gain percent numbers, Gain and
   Gain% as separate fields, and no totals records. (done)
   HTML output: like the text table, but with single-line cells, via the
   spreadsheet-cell machinery. Amount cells are right-aligned; each cell
   has a css class naming its column (plus coltotal on the totals row),
   and each commodity amount is in a span with class "amount". (done)
   FODS output: the same single-line cells, via printFods. (done)
   JSON output: an array of holding objects with the CSV fields;
   units and gain percents use hledger's usual JSON number
   encoding, missing values are null. (done)
7. Extra columns: Weight (portfolio %), humanised Age (eg 1.1y),
   Rgain (realised gain), XIRR; also added to the csv/tsv/json outputs
   (with age still numeric there). (done)
8. Docs integration: mention holdings in the manual's lot reporting
   sections (First lots example, Lot subaccounts, Lot reporting example).
   (done)

## Open questions

### Future-dated postings

Without an explicit report end date (eg set by -e), 
holdings includes future-dated postings in its units
(like other hledger reports), but prices are computed as of the
valuation date, and ages as of the report end date (both are today by default).
This means future positions are typically valued/aged as of today, 
future-dated lots will show a negative age, and future P directives are invisible.

An alternative was tried (2026-08-04) and rolled back: defaulting the
report end date to today (treating a missing end date as today+1 in the
lot posting query, the internal multiBalanceReport query, and the report
date), like hledger-ui's hiding of future transactions. It was judged
not worth the inconsistency with other reports, for now; the motivating
example (examples/lots/lot-entries.journal needing -e to show prices)
was fixed instead by moving its story dates into the past. Could be
revisited if future-dated journals prove troublesome in practice.
