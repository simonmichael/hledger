# Lot tracking

Here is the current specification for lots functionality, most of which has been implemented in the lots branch.

See also 
- hledger manual: Cost basis
- hledger manual: Lot reporting
- <https://github.com/simonmichael/hledger/blob/main/examples/lots/lot-entries.journal>
- <https://joyful.com/hledger+lot+tracking>
- <https://github.com/simonmichael/hledger/issues/1015>

## Background

Before lot tracking was added, hledger 1 users 
tracked lots manually using one subaccount per lot, 
used the `close` command and `hledger-move` script to help record complex lot movements,
and used the transaction balancing mechanism to calculate capital gain/loss.
Or, they computed gains with `balance --gain`, or the `roi` command.
These mechanisms still work but the tooling described here aims to largely subsume them.

## Lots

A lot is an amount of some commodity, acquired and held for investment purposes,
to be disposed of later, hopefully at a better price.

A lot's acquisition price and date are preserved, to 

1. help comply with tax rules, and 
2. calculate the capital gain or loss, both unrealised (before disposal), and realised (at disposal).

## Lots mode

By default, lot inference, tracking, and error checking are performed when loading a
journal, as part of journal finalising (see SPEC-finalising.md). Any journal with
lot-related content (lotful commodities/accounts, cost basis annotations, or
disposals) is validated up front. Journals with no lot activity pay near-zero cost
via an internal fast path.

This processing can be disabled with `--ignore-lots` (or its shortcut alias `-I`,
which also sets `--ignore-assertions`). When either is active, the gated lot pipeline
stages (`journalAddGainOrUGainPosting`, `journalCheckLotsTagValues`,
`journalCalculateLots`, `journalAddOrCheckGainPostings`) are skipped entirely.
Capital gains are not
inferred, lot subaccounts are not added, and lot-related errors (malformed `lots:`
tags, missing lot cost, ambiguous selectors, dispose-before-acquire, etc.) are not
raised. Classification tags and cost inference from user-written `{}` annotations
still happen, since those stages run before the `--ignore-lots` gate. Use this when
working with incomplete journal fragments (eg piping between hledger commands).

`--strict`/`-s` and `hledger check lots` both override `--ignore-lots`, restoring
full lot processing for that invocation.

The `journalCheckAcquireBasis` stage is gated separately: it runs only when the
user explicitly invokes `hledger check basis`, regardless of `--ignore-lots`,
`--strict`, or `hledger check lots`. See [Acquire basis check](#acquire-basis-check-opt-in) below.

The `--lots` general flag is a display-time toggle. It controls whether reports show

- per-lot subaccounts in the account tree, and
- the full detailed form of lot-related transactions (split postings, inferred cost
  basis annotations, synthetic balance-assertion placeholders, etc.)

When `--lots` is absent, reports show a collapsed view: lot subaccounts are hidden,
synthetic placeholder postings are dropped, and inferred gain amounts appear on the
base (parent) gain account rather than on per-lot detail accounts. Inferred gains are
visible in reports like `incomestatement` even without `--lots` — unless
`--ignore-lots` is in effect, in which case no gains were inferred in the first place.

In the journal, lot operations can be recorded

1. implicitly, with minimal notation and maximum inference;
2. partly explicitly, with any missing parts inferred;
3. or fully explicitly, requiring no inference.

A typical workflow is to use 1 primarily, or when processing/converting old journals;
and use `print` to convert to 3 when troubleshooting or reporting.

## Transacted cost and cost basis

- Transacted cost is the conversion rate used within a particular multi-commodity transaction.
  It is recorded with a @ or @@ annotation after the amount(s).

- Cost basis is the nominal acquisition cost of a lot,
  along with its acquisition date and perhaps a label.
  It is preserved (along with the lot's balance), throughout the lifetime of the lot, from acquisition through transfers to final disposal.
  It is recorded with hledger lot syntax (consolidated {} notation);
  we can also read ledger lot syntax (separate {}, [], () annotations).

A posting amount can have transacted cost, cost basis, both, or neither.
When displaying a posting with both, we show cost basis before transacted cost (like beancount).
The hledger manual has more detail.

## Lot names

A lot's cost basis also serves as the lot name. 

In hledger, a full lot name looks like {YYYY-MM-DD, "LABEL", COST} or {YYYY-MM-DD, COST}.
That is, two or three parts inside curly braces: 
- a date in strict ISO format
- an optional label in double quotes
- a single-commodity hledger amount
with a comma and space between them.
When parsing, spaces inside the braces and around the commas are optional and ignored.
This is similar to Beancount's lot syntax, except it requires DLC order (date, label, cost) and it supports hledger's flexible amount syntax.

Partial lot names are also used; these have some or all of the parts missing.
{} is a lot name/cost basis annotation with all parts missing.

Full lot names can be used, internally or explicitly in the journal, as subaccount names, to identify specific lots within a parent account.
Eg `assets:stocks:aaaa:{2026-02-10, "label", $50}`.
This means the label and the cost's commodity symbol may not contain double-quotes, colons, or semicolons.

When a lot subaccount is written explicitly in this way, it is equivalent to writing cost basis annotations after the posting amount.
(See also "Inferring cost basis from lot subaccount names" below.)
If it is written in both places, they should agree.

Lot subaccounts are ignored when checking account names, eg with `check accounts` (only the parent account needs to be declared).

## Parsing lot names

Full lot names can appear as the final part of account names, like `assets:stocks:{2026-01-01, $10}`.

When parsing, naive splitting on commas would fail because commas can appear inside:
- cost amounts as decimal separators (e.g. `€1,50`) or digit group separators (`$1,000,000.00`)
- quoted commodity symbols (e.g. `"an, odd, commodity" 1,5`)
- quoted labels (e.g. `"a, b, b"`)

Instead, parts are identified by **peeling known-format prefixes** in DLC order:

1. **Peel date**: if the first 10 characters match `YYYY-MM-DD` and are followed
   by end-of-string, comma, or whitespace, they are consumed as the date.
   The trailing comma separator (if any) and surrounding whitespace are stripped.

2. **Peel label**: if the remainder starts with `"`, scan to the next `"`.
   The quoted string is a label only if followed by a comma or end-of-string.
   If instead it is followed (after optional whitespace) by a digit, sign, or
   decimal mark, then the quoted string is a commodity symbol belonging to the
   cost amount, and it is left in place.

3. **Cost**: whatever remains is passed to the amount parser as a single string.

This approach handles all combinations of commas in dates, labels, commodity
symbols, and decimal amounts without ambiguity.

Examples:
- `{2026-01-15, "my, label", €1,50}` → date `2026-01-15`, label `my, label`, cost `€1,50`
- `{2026-01-15, "an, odd, commodity" 1,5}` → date `2026-01-15`, cost `"an, odd, commodity" 1,5` (no label; the quoted string is a commodity symbol)
- `{2026-01-15, "a, b", "an, odd, commodity" 1,5}` → date `2026-01-15`, label `a, b`, cost `"an, odd, commodity" 1,5`
- `{$100}` → cost `$100`
- `{}` → empty cost basis

## Lot ids

A lot's id is just the date and label parts.
Lot ids must be unique and ordered (per commodity), so if there are multiple lots with the same date,
labels are used to 1. disambiguate and 2. order them.
This is normally done by beginning the label with a time of day (HH:MM, or a more precise time as needed)
or an intra-day sequence number (NNNN, with enough leading zeros so that a day's lot ids sort nicely in numeric order — we'll assume four digits in total).
Labels are generated only when needed to satisfy uniqueness:
if there are multiple same-date, same-commodity acquisitions (across all accounts) with no labels,
hledger adds NNNN labels based on parse/processing order;
if such acquisitions do have user-provided labels, hledger checks that the resulting lot ids are unique
(across all accounts, to be safe) and reports an error otherwise.

Whether lot tracking is per-account or across-all-accounts depends on jurisdiction and time period.
This needs to be, and is, configurable, currently by the lots: tag's value.
Eg in the US, tax rules require that before tax year 2025, lots are tracked across all accounts,
but from tax year 2025, lots are tracked separately within each account.

## Lot selectors

A full or partial lot name/cost basis, when used in a posting with a negative amount,
selects an existing lot, rather than creating a new one.
So in this case we call it a "lot selector".

The terms "hledger lot syntax", "cost basis", "lot name", "lot selector" can sometimes be a bit interchangeable;
they all involve the same notation, which has different meanings depending on context.

## Data types

All fields are Maybe, so the same types serve for both definite and partial values:

```
data CostBasis = CostBasis { cbDate :: Maybe Day, cbLabel :: Maybe Text, cbCost :: Maybe Amount }
data LotId = LotId { lotDate :: Day, lotLabel :: Maybe Text }
```

A definite cost basis (used for a fully resolved lot) has all fields present except cbLabel which is only present when needed for uniqueness.
A partial cost basis (used as a lot selector or during inference) may have any fields missing.

## Lotful commodities and accounts

Commodities and/or accounts can be declared as lotful, by adding a "lots" tag to their declaration.
This signifies that their postings always involve a cost basis and lots, 
so these should be inferred if not written explicitly.

(In future, we may also recognise some common commodity symbols as lotful, even without the lots tag.)

## Inferring cost basis from transacted cost

In postings with a positive amount, involving a lotful commodity or account,
which have a transacted cost but no explicit cost basis annotation,
or an empty cost basis annotation (`{}`),
we infer a cost basis from the transacted cost.

## Inferring cost basis from lot subaccount names

When a posting's account name contains a lot subaccount (a final component starting with `{`),
the cost basis is parsed from the subaccount name and applied to the posting's amounts.
If the amount already has a cost basis annotation, the two are merged:
any `Nothing` fields are filled in from the other source,
and any fields present in both must agree (otherwise an error is reported).

This allows `print --lots` output (which has explicit lot subaccounts) to be re-read
without losing cost basis information, and allows users to write lot subaccounts
directly without a redundant `{}` annotation on the amount.

(journalInferBasisFromAccountNames, runs unconditionally before journalClassifyLotPostings)

## Lot postings

After inferring cost basis, we identify and classify lot postings.
A `_ptype` tag is added to each classified posting to record its type:
`acquire`, `dispose`, `transfer-from`, `transfer-to`, or `gain` (the last
applies to user-written postings on Gain-type accounts; `rgain` and
`ugain`, used on inferred postings, come later).

(`journalClassifyLotPostings` → `transactionClassifyLotPostings`)

### Classification summary

In short: a lotful commodity entering an asset account is an **acquire**.
A lotful commodity leaving an asset account is a **dispose**.
A lotful commodity moving between asset accounts is a **transfer**.
The details below handle edge cases: bare postings without `{...}`,
equity transfers, partial transfers with fees, and cost source inference.

### Classification rules

Classification proceeds in several steps. 

**1. Same-account transfer pairs.**
Within each account, negative and positive postings with the same commodity
and exact absolute quantity are paired as transfer-from / transfer-to.
When there are more of one sign than the other, the excess are left
unmatched and classified by the rules below.

**2. Postings with cost basis (`{...}`).**
These are classified regardless of account type:

- **Negative** → `dispose`, or `transfer-from` if a counterpart posting
  (same commodity and quantity, different account) exists.
- **Positive** → `acquire`, or `transfer-to` if a counterpart exists.
- **Equity transfer override**: if the posting has no transacted price
  (`@ ...`) and an equity counterpart posting (no cost basis) exists in
  the transaction, it is classified as transfer-from/to instead of
  dispose/acquire. This handles `close --clopen --lots` style equity
  transfers where lots move to/from equity in separate transactions.

**3. Bare postings on lotful asset accounts (no cost basis).**
These require an asset account type and a lotful commodity or account
(`lots:` tag). They are tried in this order:

- **Negative lotful** →
  `transfer-from` if a counterpart (same commodity, exact quantity,
  different account) exists, or if another asset account in the same
  transaction receives a positive lotful amount of the same commodity
  (transfer+fee pattern, where source qty > dest qty due to fees).
  Otherwise `dispose` if the posting has a transacted price.

- **Positive lotful, no price, with transfer-from counterpart** →
  `transfer-to`. The counterpart can match by exact quantity or by
  commodity only (for transfer+fee patterns). This handles bare
  transfer-to postings in lotful commodities/accounts that don't repeat
  the `{...}` notation.

- **Positive (any), no cost basis, with cost-basis transfer-from counterpart** →
  `transfer-to`. The counterpart can match by exact quantity or by
  commodity only (for transfer+fee patterns). This handles the receiving
  side of transfers where the sending side has `{...}` but the receiving
  side doesn't.

- **Positive lotful with a plausible cost source** →
  `acquire`. A cost source is plausible when the posting has a transacted
  price (`@ ...`), or the transaction contains a different-commodity posting
  (allowing the balancer to infer a cost), or a transfer-from counterpart
  exists. Without any of these, no lot can be created and classification is
  skipped.

Virtual (parenthesised) postings are never classified as lot postings.

**4. Gain accounts.**
Postings in accounts with type `Gain` (and not otherwise classified) get
ptype `gain`.

### Unclassified lotful postings

With `--lots`, a real posting with a nonzero lotful commodity in an asset account
that was not classified (no `_ptype` tag) is an error.
This catches lotful postings that need lot tracking but weren't recognised.

Zero-amount lotful postings (e.g. for balance assertions like `0 AAPL = 100 AAPL`)
are exempt: no lot movement occurs, so no classification is needed.
This applies regardless of whether the amount was written explicitly or left implicit.

(`isUnclassifiedLotfulPosting` in Lots.hs)

### Counterpart detection

Transfer detection uses precomputed maps keyed by (commodity, |quantity|):

- `negCBAccts`: accounts with negative postings that have cost basis
  (any account type), or are bare lotful negatives on asset accounts.
  Non-asset bare lotful negatives (e.g. revenue) are excluded.
- `posCBAccts`: accounts with positive postings that have cost basis.
- `posNoCBAccts`: accounts with positive asset postings without cost basis.

A posting has a "counterpart" when the opposite-sign map contains a
different account for the same commodity and quantity. This requires exact
quantity matching for the primary check (`hasCounterpart`,
`hasTransferFromCounterpart`).

A commodity-only fallback (`hasTransferFromCommodityMatch`) checks
`negCBAccts` for any entry with the same commodity in a different account,
ignoring quantity. This is used by `shouldClassifyLotful` and
`shouldClassifyBareTransferTo` to detect transfer-to postings in
transfer+fee patterns where the destination receives less than the source
sends.

### Main functions

- `journalClassifyLotPostings`: entry point, maps over transactions.
- `transactionClassifyLotPostings`: per-transaction classifier.
  - `sameAcctTransferSet`: precomputed set of same-account transfer pair indices.
  - `negCBAccts`, `posCBAccts`, `posNoCBAccts`: counterpart maps.
  - `hasCounterpart`, `hasTransferFromCounterpart`, `hasTransferFromCommodityMatch`: counterpart lookups.
  - `classifyAt`: per-posting dispatch.
  - `shouldClassify` → `shouldClassifyWithCostBasis`, `shouldClassifyNegativeLotful`,
    `shouldClassifyLotful`, `shouldClassifyBareTransferTo`, `shouldClassifyPositiveLotful`.
  - `postingIsLotful`: checks for `lots:` tag on commodity or account.

## Inferring transacted cost from cost basis

After classifying lot postings,
in acquire postings which have no transacted cost annotation,
we infer a transacted cost from the cost basis.

(journalInferPostingsTransactedCost)

## Lot posting effects

- An acquire posting creates a new lot, with a cost basis either specified
  or inferred from the transacted cost (or perhaps market price, in future).

- A transfer-from posting selects one or more lots to be transferred elsewhere,
  following some selection/reduction method. Either
  - it has a lot selector (a full or partial cost basis annotation),
    which must unambiguously select a single existing lot ("SPECID" method)
  - or it has no lot selector, in which case a default method is used ("FIFO" method), selecting one or more existing lots.

- A transfer-to posting mirrors a corresponding transfer-from posting in the same transaction,
  recreating its lot(s) under a new parent account.
  It doesn't need a lot annotation; if it has one, any specified fields
  (date, label, cost) must match the source lot.
  (Transfers must preserve the source lot's identity, and can't rename a lot.)
  Transfer postings (both from and to) must not have explicit transacted cost (@ or @@); this is an error.
  When the transfer-to quantity is less than the transfer-from quantity (a transfer+fee pattern),
  lots are selected for the full source quantity, then split: the transfer portion's lots are
  recreated at the destination, and the fee portion's lots are consumed from source only
  (generating from-postings on lot subaccounts with no corresponding to-postings, like a
  silent disposal with no gain).

- An equity transfer is a variant of a lot transfer that happens in two parts across
  separate transactions (e.g. a closing transaction transfers lots into equity, and an
  opening transaction transfers them back out). In the closing transaction, transfer-from
  postings reduce lots from the lot state. In the opening transaction, transfer-to
  postings re-add the lots to the lot state, preserving their original cost basis.
  The equity postings do not track lots.

- A dispose posting selects one more lots to be disposed (sold), like a transfer-from posting.
  It must also have a transacted cost, either explicit or inferred from transaction balancing
  (or from market price, in future).
  When the dispose posting has no cost basis annotation but involves a lotful commodity or account,
  the cost basis is inferred from the selected lot, and the transacted cost
  (if inferred by the balancer as @@) is normalized to unit cost (@).

## Reduction methods

The reduction method, also known as booking method, is the order in which lots are "reduced" (disposed or transferred from).
It is configurable per account and per commodity via the `lots:` tag.
(And also per posting via the `lots:` tag on a posting comment ?)

The supported reduction methods are:
SPECID (specific identification via lot selector),
FIFO (oldest first),
LIFO (newest first),
HIFO (highest cost first),
and
AVERAGE (weighted average cost basis).
If not specified, FIFO is the default.
These are per-account: they select lots and enforce/validate their order only within the posting's account.

There are also variants which consider lots across all accounts: FIFOALL, LIFOALL, HIFOALL, AVERAGEALL.
These select lots within the posting's account, but they also validate that the selected lots
would be the ones chosen if all accounts' lots were merged into a single pool.
If not, an error is raised showing which lots on other accounts have higher priority.

AVERAGE/AVERAGEALL maintain a single running per-unit cost shared by every
lot in the pool. The running cost is updated on each acquisition by the
weighted-average formula
`((poolQty × poolCost) + (acqQty × acqCost)) / (poolQty + acqQty)`,
and every existing pool lot's stored cost basis is rewritten to the new
running cost. Disposal does not change the running cost: it just consumes
quantity from one or more pool lots at the shared cost. Lots are still
selected in FIFO order so the per-acquisition dates remain meaningful for
holding-period reporting (the disposal posting fragments preserve their
original acquisition dates).

The scope of a pool is per-account for AVERAGE and global (across all
accounts holding the commodity) for AVERAGEALL.

Under AVERAGE methods the lot subaccount name omits the cost component
(`{2026-01-15}` rather than `{2026-01-15, $50}`): the running cost would
otherwise change on every acquisition, making the subaccount unstable
across acquisitions.

`print` preserves the user-written cost annotation on acquire postings.
The pool's running cost is surfaced on disposal postings (where the user
wrote `{}` and the system fills in the lot's stored cost) and on
realised-gain calculations (e.g. via `bal -B`).

## Lot transactions

Lot transactions are transactions with lot postings.
If a transaction has multiple lot postings, we (mostly ?) require that they are all of similar type: all acquire, or all transfer, or all dispose.
So a lot transaction can be broadly classified as "acquire", "transfer", or "dispose".

## Transaction balancing

All transactions, including disposals, are balanced by the ordinary
transaction-balancing rule — sum postings at transacted cost (ignoring cost
basis), sum must be zero, infer at most one missing amount per commodity.

## Disposal transactions

The realised gain/loss from a disposal is calculated as follows:
for each dispose posting in the entry whose amount has both 
a cost basis `B` and a transacted cost `T`, contribute `aquantity × (B − T)`.

### Gain postings

Conceptually, each disposal transaction has a balanced pair of postings,
representing the disposal's capital gain or loss:

- a **realised gain** posting (rgain), often to a Gain-type account (default `revenues:gain`)
- an **unrealised gain** posting (ugain) with opposite sign, usually to an UnrealisedGain-type account
  (default `equity:unrealised-gain`).

The two sum to zero so the transaction balancer accepts the disposal.
They reclassify the unrealised gain accumulated since acquisition, as realised gain.

### Gain/UnrealisedGain account types

The Gain and UnrealisedGain account types can be declared explicitly via `type:` tags:

```
account revenues:gain           ; type: G
account equity:unrealised-gain  ; type: U
```

They are also inferred from conventional English account names
(see the regex table under
[Account types](https://hledger.org/hledger.html#account-types) in the user manual).
For example `revenues:gain`, `income:capital-gains`, `equity:unrealised-gain`,
and `equity:unrealized gains` are all detected automatically.

Declaring and using these account types is not strictly required,
but they can improve error checking in disposals,
they help select an account for inferred gain postings,
and they facilitate more precise querying.

### Disposal journal entries

Disposal transactions can be written in any of these styles. The user manual's
"Recording gains" section walks through each with examples and trade-offs.

Styles are listed in the same order as the manual, from implicit to explicit.

1. **No gain postings.**
  After lot matching, hledger computes the disposal gain
  and infers realised gain and unrealised gain postings for the transaction
  (`journalAddOrCheckGainPostings`).
  The inferred amounts are rounded to the entry's local precision for the gain commodity
  (or if that is 0, and the gain has non-zero cents, decimal precision 2.
  See "Gain precision" below).

2. **Only rgain written, not using a type:G account.**
  hledger identifies rgain posting(s) heuristically: one or more postings 
  whose account type is not Asset, Liability, or Equity (or a subtype of these),
  which have not been classified as a lot movement by the lot classifier,
  and whose non-gain siblings sum to zero (or have a multi-commodity imbalance).
  Also if the lot classifier added a `_ptype:gain` tag, that indicates a gain posting
  (though currently we don't expect that without a type:G account).
  When gain postings are detected, hledger tags them with `_ptype:rgain`,
  and infers a single balancing ugain posting to the default UnrealisedGain account.
  After lot matching, the transaction's gain amount is checked against
  the calculated gain at the entry's local precision; sub-last-place-unit differences
  are tolerated (see "Gain precision" below), but larger discrepancies
  raise an error.

  When the imbalance is multi-commodity (typically because the dispose posting lacks an `@`
  transacted price), the transaction balancer fills in a balancing `@` price from the
  non-gain postings; the gain check then proceeds as above.

3. **Only rgain written, using a type:G account.**
  hledger identifies the rgain posting by the type:G account,
  and infers a balancing ugain posting. The gain amount must be written explicitly, and is checked.

4. **rgain and ugain postings written, identified by their type:G and U accounts.**
  All is explicit (including the gain amounts). No inference is needed; hledger checks the gain amount.

### Gain precision

Inferred gain amounts (cases 1, 2, 3) and the gain-validation
comparison (cases 2-4) operate at the **entry's local precision** for
the gain commodity (ie, the maximum precision seen among the
posting amounts in that commodity).

As a special case, if the local precision is 0 decimal places (or the commodity is absent),
and if the gain amount is not an integer, it is shown with 2 decimal places.

This tolerance (and non-accounting) of small imprecisions is similar to
how transaction balancing works. If you want more precision, write more
decimal places in the entry's amounts.

## Acquire basis check

`journalCheckAcquireBasis` enforces that every acquire posting has per-unit
cost basis equal to per-unit transacted cost. If `{B}` and `@T` are both
written on an acquire posting and `B ≠ T`, the check raises an error citing
the offending posting. This prevents typos in cost basis causing wrong gain
to be calculated later.

Real-world cases where basis legitimately differs from price paid (gifts
with carryover basis, NSO exercises, RSU vesting, wash-sale adjustments,
etc.) are best expressed by adding a separate income/equity/asset posting
that funds the difference.

The check uses strict Decimal equality, not the precision-tolerant
`mixedAmountLooksZero` comparison used by transaction balancing and the
recorded-gain check. Those tolerances are bounded within a single entry,
but a basis discrepancy persists in the lot store and amplifies at
disposal (gain = `(sale − basis) × qty`), and there is no downstream
"basis assertion" check that would surface accumulated drift. Strict
equality treats sub-precision inexactness as the same kind of error as
a typo.

When the per-unit basis would be a non-terminating decimal (eg
$50 / 7 = $7.142857...), record it cleanly via one of:

- `{}` — let hledger infer basis from the transacted cost (same precise
  division).
- `{{TotalCost}}` — record the total basis; hledger derives the
  per-unit value with the same precise division.
- An explicit `{$7.142857143}` at sufficient precision.

An explicit `{$7.14}` paired with `@@ $50` deliberately fails the check
— the rounded annotation forgets a per-unit fraction that would
compound across disposals.

Other PTA apps (hledger 1, Ledger, Beancount, rustledger, acc) accept entries
where `B ≠ T`, so this check is off by default to avoid interoperability
pain. It runs only when the user types `hledger check basis` (not in
default or `--strict` mode). See [DECISIONS.md](DECISIONS.md) for the
rationale.

## Balance assertions

A balance assertion on a dispose or transfer posting (eg `= 0 AAPL`) runs before `--lots` processing
(in `journalBalanceTransactions`), when the posting is still on the parent account — so it checks the
parent account's balance, as expected.

When `--lots` later splits that posting onto lot subaccounts, the assertion is removed from the lot
postings and re-attached to a new zero-amount `_generated-posting` on the original parent account,
with `bainclusive = True` (ie the `=*` syntax). This makes the assertion check the inclusive balance
of the parent plus all its lot subaccounts, which is the semantically correct interpretation when the
output is re-read later (eg after `print --lots -x`).

If the original posting's account is already an explicit lot subaccount (eg
`assets:stocks:{2026-01-15, $50}`), the assertion is left on the split posting unchanged, since it
already targets the right account.

`close --lots` does not generate balance assertions on lot subaccount postings in the
closing transaction (e.g. `assets:stocks:{2026-01-15, $50}`), because these assertions
would be invalid when the output is re-read: balance assertions run before lot calculation,
so the lot subaccounts would not yet have their expected balances. Non-lot-subaccount
postings (e.g. `assets:cash`) and opening transaction postings retain their assertions.

## Processing pipeline

Lot-related processing runs during journal finalising as a sequence of
stages. Errors (missing lot cost, ambiguous selectors, dispose before
acquire, invalid `lots:` tag values, etc.) are reported at load time.
See [SPEC-finalising.md](SPEC-finalising.md) for how this sits in the
broader pipeline.

**Always-on** (independent of `--ignore-lots`):

1. **journalInferBasisFromAccountNames** — parse cost basis from any lot subaccount
   names (`{...}` components) in posting account names.
2. **journalClassifyLotPostings** — tag postings as acquire/dispose/transfer-from/transfer-to/gain.
3. **journalInferPostingsTransactedCost** — infer `@` from `{}` on acquires (before balancing).

**Gated by `checklots`** — runs when none of `--ignore-lots` or `-I` is set, or when
`--strict`/`-s` or `hledger check lots` overrides them:

4. **journalAddGainOrUGainPosting** — if the user has written an explicit rgain or ugain
   posting without its counterpart, add the matching balancing posting (pre-balancer,
   so the ordinary balancer accepts the paired transaction).
5. **journalCheckLotsTagValues** — validate `lots:` tag values on commodity/account declarations.
6. **journalCalculateLots** — walk transactions in date order, evaluate lot selectors,
   apply reduction methods, add explicit lot subaccounts, infer cost basis for bare
   disposals, normalise transacted cost.
7. **journalCheckAcquireBasis** — *gated separately on `hledger check basis`*,
   not on `checklots`. Errors if any acquire posting has cost basis differing
   from its transacted cost (per-unit). Default mode skips this check; see
   [DECISIONS.md](DECISIONS.md) for the rationale.
8. **journalAddOrCheckGainPostings** — for disposals with no gain postings yet, add
   the rgain + ugain pair sized at the disposal gain. Also validates that any
   user-written gain amount matches the disposal gain.

The gated stages raise errors when the journal contains lot-related content that
can't be resolved (missing lot cost, ambiguous selectors, dispose before acquire,
malformed `lots:` tag values, etc.); `--ignore-lots` suppresses these by skipping
the stages entirely.

The `--lots` flag is a display toggle consumed in the report-loading layer
(`journalTransform` in `Hledger.Cli.Utils`). When absent, `journalCollapseLotDetail`
strips lot subaccount suffixes from account names, drops synthetic
`_lot-parent-assertion` postings, and merges runs of `_lotsplit-posting` fragments
(per-lot dispose/transfer splits sharing the same `poriginal`) back to a single
posting carrying the user's original amount. Posting amounts on other postings
are left alone; `print` relies on `transactionWithMostlyOriginalPostings` to revert
to `poriginal` when displaying non-explicit output.

See SPEC-finalising for more details of the implementation.

### Auto-splitting lot transfer fees

Before classification, hledger detects a common "transfer with priced fee"
pattern and rewrites it into explicit transfer + disposal postings.

If a transaction has a bare negative lotful asset posting whose absolute
quantity exceeds the matching positive quantity by some amount, and a priced
non-asset posting (typically an expense with `@` price) in the same commodity
matches that excess, the negative posting is split into two:

- a transfer portion with the matching positive quantity, and
- a dispose portion with the excess quantity and the counterpart's transacted price.

This lets natural journal entries like:

```
2026-03-09 transfer
    assets:custodial        -1 ETH
    assets:cold wallet   0.999601 ETH
    expenses:fees       0.000399 ETH @ $1,992.36
    income:gains
```

classify and balance as if the user had written:

```
2026-03-09 transfer
    assets:custodial                 -0.999601 ETH
    assets:custodial  -0.000399 ETH @ $1,992.36
    assets:cold wallet                0.999601 ETH
    expenses:fees     0.000399 ETH @ $1,992.36
    income:gains
```

The original user posting is preserved via `poriginal` on the transfer portion
(p1), and the dispose portion (p2) is tagged `_feesplit-posting`. As a result:

- Plain `print` shows the user's original entry (the `_feesplit-posting`-tagged
  portion is hidden, and the remaining portion displays at the original
  quantity via `poriginal`).
- `print -x` shows the split form explicitly (both portions visible at their
  post-split quantities).
- `print --lots` also shows the split form for auto-split transactions, so
  that the output round-trips correctly (preserving the capital gain that
  would otherwise be lost if the dispose portion were hidden). Other
  transactions still display in their mostly-original form.

### Per-lot disposal/transfer splits

A separate internal split happens when a single dispose or transfer posting
spans multiple lots: `processDisposePosting` / `processTransferPair` emit one
fragment per matched lot, each carrying its lot subaccount. Multi-fragment
results are tagged `_lotsplit-posting` (single-lot results need no tag), and
each fragment's `poriginal` points at the user's unmodified original posting.

Display behaviour:

- Plain `print` collapses the fragments via `journalCollapseLotDetail` and
  reverts to `poriginal`, so the user sees their single original posting.
- `print --lots` keeps the fragments visible and renders each with its lot
  subaccount and per-lot quantity, but with the user's original cost basis
  annotations (achieved by `transactionWithMostlyOriginalPostings` scaling
  `poriginal`'s amount to the fragment's quantity).
- `print -x` keeps the fully inferred form (no revert).

## Examples

For end-to-end walkthroughs, see the user manual's "First lots example" and "Lot reporting example" sections.

A larger collection of example entries: <https://github.com/simonmichael/hledger/blob/main/examples/lots/lot-entries.journal>

### Disposal

A minimal implicit disposal (user writes only dispose + proceeds):

```
2026-03-01 sell
    assets:stocks   -15 AAPL
    assets:cash      $900
```

or:

```
2026-03-01 sell
    assets:stocks   -15 AAPL @ $60
    assets:cash
```

Explanation:

1. The missing @ price (or missing cash amount) is inferred by the ordinary
   transaction balancer so the postings balance at transacted cost.
2. 15 AAPL are reduced from one or more existing lots, selected by
   `assets:stock`'s / `AAPL`'s / default (FIFO) reduction method.
3. `journalAddOrCheckGainPostings` computes the disposal gain
   (`aquantity × (B − T)` summed over non-acquire postings whose amounts
   carry both basis and transacted cost) and adds a realised-gain posting
   (rgain) and a matching unrealised-gain posting (ugain) with the opposite
   sign. The pair sums to zero, so the disposal stays balanced under the
   ordinary transacted-cost rule.
