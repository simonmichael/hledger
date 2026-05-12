# Plan: continuous unrealised-gain tracking via generated postings

## Background

Today `equity:unrealised-gain` only receives a single posting at disposal
time, sized at the disposal gain. Conceptually it should accumulate
continuously: each market-price change should generate a synthetic
revaluation transaction, posting `Dr asset (revaluation) / Cr ugain`,
so the ugain account's history is inspectable in `print`, `register`, and
`bal` like any other account. Disposal then becomes a clean reclassification:
the existing `rgain`+`ugain` pair shape stays, but the ugain side is sized
from the disposed lot's accumulated revaluation balance rather than computed
on the spot.

This was the trade-off discussed in commit
[80b320acc](https://github.com/simonmichael/hledger/commit/80b320acc):
that commit chose not to generate revaluation postings, paying the cost of
a less inspectable ugain in exchange for less synthetic noise. The
disposal-only-gain rework documented in `PLAN-gain-checking.md` (the
parent of this doc) is forward-compatible with continuous ugain tracking
— the synthetic `rgain`+`ugain` pair shape we kept is exactly what
disposal-time reclassification would produce.

## Open design questions

1. **What's the asset-side posting representation?**
   - Pure `$` mixed into a commodity-tracked account?
   - Parallel `assets:revaluation:*` account?
   - Extension of the lot data model with a market-value field?

2. **When are revaluations triggered?**
   - Each `P` directive?
   - Each transaction whose `@`/`@@` price differs from the lot's last-known price?
   - Explicit valuation dates?
   - Period boundaries (month-end, year-end)?
   - On-demand at report time only?

3. **How does disposal-time reclassification scale with accumulated ugain?**
   - Partial disposals — what fraction of the lot's accumulated ugain transfers to rgain?
   - Interaction with FIFO/LIFO/HIFO/AVERAGE selection.
   - Lot-level vs. commodity-level vs. account-level revaluation tracking.

## What this would look like end-to-end (sketch)

```journal
2026-01-01 buy
    assets:broker     100 AAPL {$50}
    assets:cash      -$5000

P 2026-02-01 AAPL $60      ; → synthetic revaluation transaction generated:
                            ;   Dr assets:broker:{...}      $1000  ; +$1000 over basis
                            ;   Cr equity:unrealised-gain  -$1000

P 2026-03-01 AAPL $70      ; → another synthetic revaluation:
                            ;   Dr assets:broker:{...}      $1000
                            ;   Cr equity:unrealised-gain  -$1000

2026-04-01 sell
    assets:broker    -100 AAPL {$50} @ $70
    assets:cash      $7000
                            ; equity:unrealised-gain currently shows -$2000.
                            ; Disposal-time reclassification posts:
                            ;   Cr revenues:gain           -$2000
                            ;   Dr equity:unrealised-gain   $2000
                            ; ugain returns to $0 for this lot.
```

Each step is consistent at transacted-cost balance and produces a real
posting trail visible in `print`/`register`/`bal`.

## Why this is parked

Each open question above has multiple plausible answers with different
trade-offs around storage, UX, and scaling. Picking one without a real
prototype risks locking in a direction we'd regret. The current pair
shape works correctly for one-shot disposal-time gain recognition, which
covers the common reporting need; users who want continuous unrealised
tracking can use `bal -V` (market-value report) which derives the same
information at report time without persisting it.

Worth revisiting when:
- Users start asking for `register equity:unrealised-gain` to show the
  history of their paper gains, not just realisations.
- A jurisdiction-specific tax workflow makes period-end revaluation
  posting mandatory rather than optional.
- The disposal-time reclassification math turns out to need more state
  than `aquantity × (B − T)` per lot (eg cost-basis adjustments from
  corporate actions that retroactively change the gain).

Until then: out of scope.
