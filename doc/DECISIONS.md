# Decisions

A partial list of notable development decisions / design choices..

## 2022

### Replace "transaction price" terminology with "cost"

"Transaction price" never quite stuck. "Cost" is simpler, shorter, more intuitive, consistent with `--cost` and "cost reporting", and more distinct from "market price".

There is an (acceptable) ambiguity: "cost" could mean the `@ UNITCOST` price attached to the amount, or the total cost when the amount is converted (`QUANTITY * UNITCOST`).

Status: as of 2023Q1 this has been done in the manuals and is slowly ongoing in the code.

## 2023

### Plugin types

We will document and support where feasible several distinct kinds of plugin, written in haskell or other languages,
such as reader, processor, writer, formatter, command. See <https://hledger.org/scripting.html#plugin-types>.

## 2025

I think the keyword-first style for directives is right for us (`open 2025-01-01 ...`, not `2025-01-01 open ...`).
It avoids polluting/breaking transaction descriptions, it's similar to P, 
it keeps directives and transactions visually distinct,
and consistently beginning with letters and numbers respectively.

Yes we should support declaring aliases with alias: tags on account directives.

## 2026

### Release hledger 2.0 this year, with two main themes: lots and AI

hledger 2 will explore ethical AI-assisted development, 
and will leverage that to ship automated lots and gains tracking.
There will be a substantial period for preview releases, discussion and testing before the 2.0 release.

### Shift "cost" terminology to "transacted cost" or "transacted price"

To distinguish transacted costs (@) from cost basis ({}).

### Don't auto-recognise gain accounts by name

For lot tracking, hledger identifies realised gain (rgain) and unrealised gain
(ugain) postings by account types `G` and `U`.
Unlike other account types, these two are not automatically inferred from english account names
(even though the names `revenues:gain` and `equity:unrealised-gain` are built in for use as defaults).
Reasons: (a) to avoid breaking hledger 1 journals which happen to use those names
and (b) to avoid inconsistent UX for english/non-english-language users,
especially as these types can determine whether a journal entry is accepted or not.

### Compute realised gain from the disposal postings only

The synthetic `rgain`/`ugain` pair is sized from `Σ aquantity × (B − T)`
over non-acquire postings with both basis and transacted cost — not from
the entry's full cost-basis residual. This isolates real capital gain
from acquire-side bookkeeping mistakes (eg a typo'd `{B}` or a fee being
double-counted into basis).

### Don't enforce basis = transacted cost in acquisitions by default

Acquires with `{B} @ T` where `B ≠ T`, are accepted by default, for better compatibility
with other apps (hledger 1, Ledger, Beancount, rustledger, acc, etc.). 
Docs recommend users to always keep `B = T`, and to use the `basis` check to check this,
with reasons provided (prevent wrong gain caused by basis typos).
The new check might be moved into strict mode some day, but not yet.

### Don't require Gain/UnrealisedGain account declarations to balance disposals

Originally, writing an explicit gain posting in a disposal entry required
declaring its account with `type: G` (and the inferred unrealised-gain
posting required `type: U`), so hledger could recognise it pre-balance and
insert the matching balancing posting. This coupled balancing to journal
metadata, which is painful for tools that generate journals from CSV
(they'd have to emit the declarations too) and surprising to users who
expect balancing to be purely arithmetic.

Now, hledger detects the user's rgain by either of two paths:
1. **Account-type detection** — postings to accounts declared with `type: G`.
2. **Heuristic detection** — when there are no type:G postings, hledger looks
   for postings that are neither Asset/Liability/Equity nor classified as
   acquire/dispose/transfer-from/transfer-to, and assumes these are gain postings -
   provided the rest of the entry balances, so a non-gain typo isn't silently absorbed.

The heuristic path lets users write `revenues:gain  $-10`, or any other
account name, without a `type: G` declaration, and have hledger balance
the entry. The gain amount is still validated against the calculated
disposal gain after lot matching.

