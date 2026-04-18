# Lot tracking

Here is the current specification for lots functionality, most of which has been implemented in the lots branch.

See also 
- hledger manual: Cost basis
- hledger manual: Lot reporting
- <https://github.com/simonmichael/hledger/blob/main/examples/lots/lot-entries.journal>
- <https://joyful.com/hledger+lot+tracking>
- <https://github.com/simonmichael/hledger/issues/1015>


## Lots

A lot is an amount of some commodity, acquired and held for investment purposes,
to be disposed of later, hopefully at a better price.

A lot's acquisition price and date are preserved, to 

1. help comply with tax rules, and 
2. calculate the capital gain or loss, both unrealised (before disposal), and realised (at disposal).

Historically, hledger has not provided much lot-tracking assistance:
- you could track lots manually by using one subaccount per lot
- the `balance --gain` report calculates gain/loss in simple cases
- the `close` command and `hledger-move` script help record lot movements

## Lots mode

By default, lot inference, tracking, and error checking are performed when loading a
journal, as part of journal finalising (see SPEC-finalising.md). Any journal with
lot-related content (lotful commodities/accounts, cost basis annotations, or
disposals) is validated up front. Journals with no lot activity pay near-zero cost
via an internal fast path.

This processing can be disabled with `--ignore-lots` (or its shortcut alias `-I`,
which also sets `--ignore-assertions`). When either is active, the three lot pipeline
stages (`journalCheckLotsTagValues`, `journalCalculateLots`,
`journalInferAndCheckDisposalBalancing`) are skipped entirely. Capital gains are not
inferred, lot subaccounts are not added, and lot-related errors (malformed `lots:`
tags, missing lot cost, ambiguous selectors, dispose-before-acquire, etc.) are not
raised. Classification tags and cost inference from user-written `{}` annotations
still happen, since those stages run before the `--ignore-lots` gate. Use this when
working with incomplete journal fragments (eg piping between hledger commands).

`--strict`/`-s` and `hledger check lots` both override `--ignore-lots`, restoring
full lot processing for that invocation.

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

"Transacted cost" is an awkward term, overlapping with "cost basis",
but it avoids too much change from the historical "cost", and we don't have a better alternative, currently.

The hledger manual has more detail on these.

A posting amount can have transacted cost, cost basis, both, or neither.
When displaying a posting with both, we show cost basis before transacted cost (like beancount).

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
- cost amounts as decimal separators (e.g. `€1,50`)
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
Lot ids must be unique and ordered, so if there are multiple lots with the same date,
labels must be used to 1. disambiguate and 2. order them.
This is normally done by beginning the label with a time of day (HH:MM, or a more precise time as needed)
or a intra-day sequence number (NNNN, with enough leading zeros so that a day's lot ids sort nicely in numeric order; we'll assume four digits in total.)
Labels are generated only when needed to satisfy lot id uniqueness rules.
If there are multiple same-date, same-commodity acquisitions (across all accounts) with no labels,
hledger adds NNNN labels based on parse/processing order.
If such acquisitions do have user-provided labels, hledger checks that the resulting lot ids are unique
(across all accounts, to be safe) and reports an error otherwise.

What is the scope of lot ids' uniqueness and ordering ?
It is 1. per commodity (lots of different commodities do not clash), 
and 2. either per account, or across all accounts.
The latter needs to be configurable somehow, for different time periods.

Eg in the US, tax rules require that before 2025, lots are tracked across all accounts,
whereas after 2025, lots are tracked separately within each account.

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
A `ptype` tag is added to each classified posting to record its type:
acquire, dispose, transfer-from, transfer-to, or gain.

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
  It doesn't need a lot selector; if it has one, it must select the same lot as the transfer-from posting.
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

AVERAGE/AVERAGEALL reduce lots in FIFO order, but use the pool's weighted average per-unit cost as the cost basis for disposal.

## Lot transactions

Lot transactions are transactions with lot postings.
If a transaction has multiple lot postings, we (mostly ?) require that they are all of similar type: all acquire, or all transfer, or all dispose.
So a lot transaction can be broadly classified as "acquire", "transfer", or "dispose".

## Gain postings

A gain posting is a posting to an account of Gain type (a subtype of Revenue).
Gain postings appear in disposal transactions, to record the capital gain or loss.
The special account type helps hledger identify these postings during transaction balancing.

Unlike other account types, Gain can not be inferred from account names;
it must be declared explicitly with a `type:` tag.
Eg `account revenues:gain    ; type:G`.
This, and the balancing rules described next, help avoid breakage when moving between hledger 1 and 2, and between hledger 2's non-lots and lots modes.

## Transaction balancing

All transactions recorded in the journal, including lot-related ones, must pass normal transaction balancing.
Transaction balancing is not aware of lots or capital gain; it just checks and ensures that the transaction is balanced,
in the sense that the debits (positives) and credits (negatives) are equal.
It

1. sums postings using their transacted costs if any (ignoring cost basis)
2. ignores capital gain/loss postings, identified by their Gain account type.
3. checks that the postings' sum is zero;
  or if it is nonzero and there are postings with no amount, it infers the balancing amount (at most one per commodity).

Since gain postings are ignored here, an amountless gain posting will remain amountless at this stage.

## Disposal balancing

When running in lots mode, disposal transactions must pass another kind of balancing.
Disposal balancing checks and ensures that the transaction is balanced when capital gain/loss is included.
It

1. sums postings using a per-amount valuation hierarchy:
   cost basis if present (`quantity × basis_price`),
   else transacted cost (`@`/`@@`) if present,
   else the raw amount
2. checks that the postings' sum is zero;
  or if it is nonzero, assumes the imbalance is the (implicitly) recorded capital gain
3. calculates the capital gain (by comparing the lot acquisition cost(s) and the transacted disposal price)
4. checks that the recorded gain matches the calculated gain;
  or if the gain amount is missing, adds it (at most one per commodity);
  or if there's no gain posting at all, adds one
  (using the alphabetically first Gain account, or if none is declared, `revenue:gains`). <!-- `revenues:gain -->

This valuation extends transaction balancing's rule (acost or raw) by adding cost basis on top —
disposal balancing is "transaction balancing using cost basis when available."

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

Lot-related processing runs during journal finalising in two groups:

**Always-on** (independent of `--ignore-lots`):

1. **journalInferBasisFromAccountNames** — parse cost basis from any lot subaccount
   names (`{...}` components) in posting account names.
2. **journalClassifyLotPostings** — tag postings as acquire/dispose/transfer-from/transfer-to/gain.
3. **journalInferPostingsTransactedCost** — infer `@` from `{}` on acquires (before balancing).

**Gated by `checklots`** — runs when none of `--ignore-lots` or `-I` is set, or when
`--strict`/`-s` or `hledger check lots` overrides them:

4. **journalCheckLotsTagValues** — validate `lots:` tag values on commodity/account declarations.
5. **journalCalculateLots** — walk transactions in date order, evaluate lot selectors,
   apply reduction methods, add explicit lot subaccounts, infer cost basis for bare
   disposals, normalise transacted cost.
6. **journalInferAndCheckDisposalBalancing** — infer gain amounts and check disposal
   transactions balance at cost basis.

The gated stages raise errors when the journal contains lot-related content that
can't be resolved (missing lot cost, ambiguous selectors, dispose before acquire,
malformed `lots:` tag values, etc.); `--ignore-lots` suppresses these by skipping
the stages entirely.

The `--lots` flag is a display toggle consumed in the report-loading layer
(`journalTransform` in `Hledger.Cli.Utils`). When absent, `journalCollapseLotDetail`
strips lot subaccount suffixes from account names and drops synthetic `_split-posting`
and `_lot-parent-assertion` postings from each transaction. Posting amounts are left
alone; `print` relies on its existing `transactionWithMostlyOriginalPostings` logic to
revert to `poriginal` when displaying non-explicit output.

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
(p1), and the dispose portion (p2) is tagged `_split-posting`. As a result:

- Plain `print` shows the user's original entry (the `_split-posting`-tagged
  portion is hidden, and the remaining portion displays at the original
  quantity via `poriginal`).
- `print -x` shows the split form explicitly (both portions visible at their
  post-split quantities).
- `print --lots` also shows the split form for auto-split transactions, so
  that the output round-trips correctly (preserving the capital gain that
  would otherwise be lost if the dispose portion were hidden). Other
  transactions still display in their mostly-original form.

## When might cost basis differ from the transacted cost ?

In many real-world scenarios, a lot's cost basis (the value recorded for tax purposes)
can differ from the price actually paid to acquire it. These may include:

- **Gifts** — the recipient inherits the donor's original cost basis (carryover basis), not the fair market value at the time of the gift.
- **Inheritance** — inherited assets get a "stepped-up" basis to fair market value at the date of death.
- **Employee stock options (NSOs)** — the bargain element (FMV minus exercise price) is taxed as ordinary income, and cost basis becomes the FMV at exercise, not the price paid.
- **Incentive stock options (ISOs)** — cost basis is the exercise price for regular tax, but FMV at exercise for AMT, so the same lot can have two different bases depending on tax context.
- **RSUs** — cost basis is FMV at vesting; the recipient paid nothing.
- **ESPPs** — shares bought at a discount; basis treatment depends on qualifying vs disqualifying disposition.
- **Wash sales** — disallowed loss from a prior sale is added to the cost basis of the replacement shares.
- **Corporate actions** — spin-offs, mergers, and stock splits cause cost basis to be allocated or adjusted in ways unrelated to any payment.

## Examples

<https://github.com/simonmichael/hledger/blob/main/examples/lots/lot-entries.journal>

<!--
NEEDS UPDATE

### Disposal

A very implicit disposal:

```
2026-03-01 sell
    assets:stocks   -15 AAPL
    assets:cash      $900
    revenue:gains
```

or:

```
2026-03-01 sell
    assets:stocks   -15 AAPL @ $60
    assets:cash      
    revenue:gains
```

Explanation:

1. revenue:gains is recognised as a Gain account so ignored by normal transaction balancing
2. $60 sale price or $900 sale amount is inferred to balance the transaction

and when running in lots mode:

3. 15 AAPL are reduced from one or more existing lots selected with assets:stock's or AAPL's or default (FIFO) reduction method
4. disposal balancing infers the gain amount based on the reduction order, selected lot(s)' cost bases, and sale amount
-->
