# Plan for calculating lots, 2026-02

## Initial plan

For background, see SPEC-lots.md.

In journalFinalise, after transaction balancing, we'll do all lot-related processing.
We will think of lot functionality as an optional addition that happens after all normal journal validation.

There is a new step, journalCalculateLots, which can fail, something like Journal -> Either String Journal.
This will step through the journal's transactions, in date order, transforming them by adding lot information.
Specifically, 

- Where a dispose or transfer-from posting has a lot selector, that will be expanded to identify a specific lot.
  If it selects multiple lots, the posting will be replicated so that there's one for each specific lot.

- Each lotful posting will have a full lot name appended to its account name, as a subaccount.

This will require a stateful loop, accumulating a map from accounts to their lot balances.

Posting dates and secondary dates will not be used for calculating lots; only primary transaction dates.

Lots/lot postings will be modified/selected as described in "Lot posting effects". Eg,

- For acquire postings, a new lot name will be generated from the cost basis and/or posting date/transacted cost.

- For transfer-from and dispose postings, one or more existing lots will be inferred from the lot selector,
  and/or by following a lot selection method, FIFO. When multiple lots are selected, extra postings will be created.

- For transfer-to postings, the lot name will be copied from the corresponding transfer-from posting in the transaction.
  We assume that lot transfers are recorded in a single transaction.

We'll add a general --lots flag.
It will affect reports, but perhaps also journal validation, so perhaps it should be in InputOpts, like --auto or --infer-costs.

There will be two kinds of transaction balancing:

1. Normal transaction balancing, using transacted costs; all journal entries must pass this as usual.

2. Lot disposal balancing is a separate check that applies to disposal transactions only:
   it uses cost basis instead of transacted cost for balancing,
   and so it requires or infers an appropriate gain posting.

Gain postings (identifiable by Gain account type) will be ignored by normal transaction balancing,
so that both kinds of balancing can succeed with the same journal entries.
This needs some exploration and testing to assess the impact on existing journal entries
not using the new lots features.

Lot disposal balancing could be done always, for best error checking; or only when lots mode is enabled, if it helps performance.

Splitting lot postings shouldn't be a problem for transaction balancing - their aggregate effect will be unchanged.

We can enforce simplifying limitations where needed, eg for matching transfer postings.
In general it's important to get something simple working early; refinements can be added later.

Lot labels are generated when needed, to satisfy lot id uniqueness rules.
Eg if we find multiple same-date-same-commodity acquisitions (across all accounts), with no labels,
we should add NNNN labels based on parse/processing order.
If we find such acquisitions that have user-added labels, we should check that the resulting lot ids are indeed unique
(again across all accounts, to be safe) and report an error otherwise.

We are not implementing them yet, but there will be multiple lot selection methods: 
FIFO (across accounts), FIFO1 (per account), LIFO, LIFO1, and SPECID at least.
We should assume the method can be vary/be configured in a relatively fine-grained way -
per account, per commodity, per time period, perhaps even per posting.
In future we might want to add a {!METHOD, ...} part inside the lot syntax,
as another way to select lots or as an extra validation that the lot selection satisfies that method.

We will want a data structure for the calculated lot state that is relatively efficient
for implementing all the planned lot selection methods.
Suggestions include Map Commodity (Map LotId (AccountName, Amount)) for FIFO/LIFO
or Map (AccountName, Commodity) (Map LotId Amount) for FIFO1/LIFO1.
We'll want to be able to search in order of date + label.
We might find more suitable alternatives to standard Map, in the base library or on Hackage.
But it's fine to start with a simple implementation even if it's a bit slow.

In general, we should do the simplest thing that could possibly work for now, to narrow scope.
This applies generally to all unresolved questions, unless they really do need to be resolved now.

## Initial claude assessment of spec

What the spec describes

  SPEC-lots.md defines a comprehensive lot-tracking system for investment accounting. The core idea: a --lots flag enables automatic lot inference, tracking, and validation. Lots are identified by a cost basis (date + optional label + acquisition cost) which also serves as the lot name and becomes a
   subaccount (e.g. assets:stocks:aaaa:{2026-02-10, "lot1", $50}).

  Postings are classified as acquire, dispose, transfer-from, or transfer-to. Lot selection for disposals/transfers can be explicit (SPECID) or automatic (FIFO). Gain/loss is calculated by comparing acquisition cost to disposal price.

What's already implemented

Substantial infrastructure is in place:
  - CostBasis type with all-Maybe fields (no need for a separate PartialCostBasis)
  - Gain account type (subtype of Revenue)
  - journalInferPostingsCostBasis — infers cost basis from @ cost on lotful postings
  - journalClassifyLotPostings — tags postings with _ptype (acquire/dispose/transfer-from/transfer-to)
  - journalInferPostingsTransactedCost — infers @ cost from cost basis on acquire postings
  - All three are wired into the journalFinalise pipeline
  - Functional tests exist for the above

What's planned (PLAN-lot-calculation.md)

  A new journalCalculateLots function, inserted after the existing pipeline steps, that:
  1. Walks transactions in date order with stateful lot-balance tracking
  2. Expands lot selectors to specific lots (FIFO or explicit)
  3. Splits postings when multiple lots are selected
  4. Appends full lot names as subaccount names

  Gain postings and transaction balancing are explicitly deferred.

Problem areas and open questions

  1. Transaction balancing interaction — The spec acknowledges this is unclear (lines 209-213). 
  Currently balancing happens after cost inference in the pipeline but before the planned journalCalculateLots. 
  If lot calculation splits postings or adds gain postings, it may need to re-balance, or balancing may need to be deferred/split into two passes. 
  This is the biggest architectural risk.

  2. No --lots flag yet — The spec says lots mode is gated by --lots, but it doesn't exist. 
  Currently lot processing runs unconditionally based on lots: tags on commodities/accounts. 
  The plan doesn't address when/how to add the flag.

  3. Lot id uniqueness scope — The spec notes lot ids must be unique per-commodity, 
  and scope (per-account vs. global) needs to be configurable for different tax regimes (pre-2025 vs. post-2025 US rules). 
  No design for this configuration exists yet.

  4. Label generation — When no label is specified, hledger must generate one (using time-of-day or sequence number). 
  The plan doesn't detail this algorithm. 
  Getting this right matters for determinism and for lot ids to sort correctly.

  5. FIFO across accounts vs. per-account — The spec mentions FIFO (global) now and FIFO1 (per-account) later, but the plan only mentions FIFO. 
  The stateful map design needs to accommodate both from the start or risk a rewrite.

  6. Posting splitting — When a disposal/transfer selects multiple lots, postings are replicated. 
  This changes the structure of transactions after they've been parsed and partially processed, 
  which could interact poorly with other pipeline steps (auto postings, equity inference, etc.).

  7. Transfer-to matching — The plan says "we assume lot transfers are in a single transaction" and the lot name is "copied from the corresponding transfer-from".
  But matching transfer-from to transfer-to relies on the classification step's counterparty detection, 
  whose robustness in edge cases (multiple transfer pairs in one transaction, partial amounts) isn't discussed.

  8. Gain deferral — Gain calculation and gain-posting inference are deferred, but they're tightly coupled to disposal processing. 
  The disposal path will need to be revisited when gains are added, so the initial implementation should leave clear extension points.

  9. Loop style question — The plan asks whether to use a fold, scan, or State monad. 
  For this kind of stateful sequential transformation (accumulating lot balances, potentially splitting transactions), a State monad or mapAccumL would be natural in Haskell. 
  A fold would work but is less ergonomic when the output structure differs from the input.

Overall assessment

  The foundation is solid — the classification and inference pipeline is already working. 
  The plan for journalCalculateLots is reasonable and well-scoped (deferring gains is wise). 
  The main risk is the transaction balancing interaction: the current pipeline balances transactions before lot calculation, 
  but lot calculation may need to modify transactions in ways that affect balance. 
  This needs a concrete design decision before implementation begins.


## Amendments

1. LotState type: PLAN suggested Map Commodity (Map LotId (AccountName, Amount)). We use Map Commodity (Map LotId (Map AccountName Amount)) — triple-nested to support
   partial lot transfers (same LotId at two accounts).

2. selectLotsFIFO is parameterised: Takes Maybe AccountName — Nothing to search across all accounts (used for disposals), Just acct to search within one account
   (used for transfers, since the lots must come from the source account). Both use the same FIFO method; the parameter controls scope, not method.

3. reduceLotState is parameterised: Takes Maybe AccountName — Nothing for disposals (reduce across all accounts holding the lot), Just acct for transfers
   (reduce only the source account's holding). This is about where to debit quantity, not about the lot selection method.

4. Transfer pairing: PLAN said "pair by commodity". We pair by commodity, then sort by cost basis key within each commodity group to align explicit per-lot pairs.
   PLAN didn't anticipate multiple transfer pairs per commodity per transaction.

5. No transacted cost check on transfers: SPEC says "transacted costs are not expected". We removed the runtime check because {$X} sets acost via earlier pipeline
   steps, making it indistinguishable from explicit @ $X. Transaction balancing serves as the practical guard instead.

6. PLAN's "Third goal" phase 12.4 said handle transfers outside foldMPostings. We did this — transfers are partitioned and processed before foldMPostings runs on
   remaining postings.

7. Reorganised/expanded/updated lot tests.

8. Transfer transacted cost check re-added: Amendment 5 removed this check because {$X} sets acost indistinguishably from explicit @ $X. We now re-add it
   (presumably with smarter detection), erroring with "lot transfers should have no transacted cost" when an explicit @/@@ is present.

9. {} on acquire infers cost basis from implicit balancing cost: A new inference path — `{}` is treated as a wildcard requesting inference, not just from explicit
   @/@@ but also from the implicit cost determined by transaction balancing. SPEC updated: "no explicit cost basis annotation".

10. Lot posting definition broadened for transfer-to: Bare postings matched by a transfer-from in the same transaction are now classified as transfer-to, even without
    cost basis annotation and on non-lotful commodities. SPEC updated with a third criterion for lot posting identification.

11. Non-asset accounts tolerated for lot operations: Cost basis annotation forces lot detection regardless of account type (e.g. equity:opening with {$50}).
    This is a convenience for undeclared/unknown account types, not a semantically intended use — lot operations are still conceptually for asset accounts.

12. Bare/@ disposals on lotful commodities get full lot inference: FIFO lot selection, cost basis, and transacted cost are all inferred from existing lots and
    transaction balancing amounts. E.g. bare `-5 AAPL` on a lotful commodity becomes `-5 AAPL {$50} [2026-01-01] @ $55` with ptype:dispose.

13. {} on acquire postings infers cost basis from transacted cost. Previously this worked only if there was no {} annotation at all.

14. Bare/@ acquisitions on lotful commodities: Positive postings in a lotful commodity/account
    are now classified as acquire even without explicit {} notation, symmetrically with how bare
    negative lotful postings are classified as dispose (amendment 12). Cost basis is inferred from
    transacted cost (explicit @ or balancer-inferred). The removed journalInferPostingsCostBasis
    is no longer needed — classification detects bare acquires directly, and processAcquirePosting
    infers cost basis from acost at lot calculation time (after balancing).

(+ many followup improvements)

## Assessment

Lots branch: scope & complexity

Size

- 64 commits, 68 files changed
- ~8,400 insertions / ~3,800 deletions (net +4,600 lines)
- Core new module: Lots.hs at 987 lines — the single largest new artifact

Breakdown by category

| Category           | Commits | What |
|--------------------|---------|------|
| imp (improvements) | 29      | Bulk of the work — lot tracking, disposal balancing, gain inference, beancount export, print formats |
| doc                | 13      | Specs, plans, manual updates, embedded manuals |
| dev (refactoring)  | 11      | Types reorganization, pipeline cleanup, test reorganization |
| feat (features)    | 4       | Lotful commodities, cost basis classification, account lots tag, Gain type |
| fix                | 4       | Disposal matching, gain balancing, cost basis inference |
| infra              | 2       | Cabal file updates |
| test               | 1       | (plus many tests embedded in other commits) |

What it touches

- Deep plumbing: Types.hs (+208 lines — LotId, CostBasis, new posting fields), Amount.hs, Balancing.hs (disposal-aware balancing), Journal.hs
(pipeline changes), Posting.hs, Transaction.hs
- Parser: Read/Common.hs (+222 lines — new lot/cost basis syntax), JournalReader.hs
- Output: Print.hs (+68 lines), new Ledger.hs writer, Beancount.hs improvements
- New module: Lots.hs (987 lines) — acquisition, disposal FIFO/LIFO, transfers, gain inference, error reporting
- Tests: ~1,800 lines across 9 test files (lots-acquire, lots-dispose, lots-transfer, lots-methods, etc.)
- Specs/plans: ~640 lines of design documentation

Complexity assessment

High. This is a fundamental extension to hledger's data model and processing pipeline:
- Adds new fields to core types (Posting, Amount)
- Modifies transaction balancing semantics (gain postings excluded from normal balancing)
- Adds a new pipeline stage (lot calculation runs after balancing in journalFinalise)
- Multiple reduction methods (FIFO, FIFO1, LIFO, LIFO1)
- Gain posting inference (auto-creating revenue postings)
- New input syntax (consolidated lot syntax {...})
- New output format (-O ledger)
- Cross-cutting: changes span parser → types → balancing → lot calculation → output

Roughly equivalent to a medium-sized feature branch that a senior developer might produce over 2-4 weeks of focused work, or longer with the
design/spec iteration visible in the commit history.

## Impact on existing journals

 Summary

 The lots branch is safe for existing journals. Transaction balancing behavior is unchanged for journals that balanced on master. The gain posting exclusion is strictly more permissive (allows
 transactions that would have failed on master), never more restrictive.

 What's new on the lots branch (vs master)

 Already on master (no change)

 - Gain account type in Types.hs
 - gainAccountRegex in AccountName.hs — infers Gain type from names like revenue:gains, income:capital-gains, etc.
 - isAccountSubtypeOf Gain Revenue = True — Gain accounts are included in Revenue queries (income statement, type:R, etc.)
 - {...} cost basis parsing populates acostbasis field

 New on lots branch, unconditional (always runs)

 1. journalClassifyLotPostings (Common.hs:371) — adds hidden _ptype tags (acquire/dispose/transfer-from/transfer-to) to postings with cost basis {...}. Invisible in normal output; only visible with
 --verbose-tags.
 2. journalInferPostingsTransactedCost (Common.hs:372) — infers @ $X from {$X} on positive (acquire) postings only. Strictly beneficial: makes transactions work that would have failed on master.
 3. Gain posting exclusion in Balancing.hs (lines 84-88, 117-120, 286-296, 373-376) — excludes Gain-type postings from transaction balancing/inference in disposal transactions (those tagged _ptype:
 dispose).

 New on lots branch, gated by --lots

 - journalCalculateLots — lot subaccounts, splitting, FIFO/LIFO selection
 - journalInferAndCheckDisposalBalancing — cost-basis balance check, gain posting inference

 Why existing journals are safe

 The gain posting exclusion can't break balanced transactions

 The gain exclusion only triggers when ALL of:
 1. Transaction has a _ptype: dispose posting (negative amount with cost basis {...})
 2. A posting goes to a Gain-type account (revenue:gains, etc.)
 3. account_types_ is non-empty (always true in practice)

 Key insight: On master, a disposal + gain posting can NEVER balance:
 assets:broker    -5 AAPL {$50} @ $55    ; converts to -$275
 assets:cash       $275                  ; +$275
 revenue:gains    -$25                   ; -$25
                                         ; total: -$25 ≠ 0, FAILS on master
 On the lots branch, the gain posting is excluded → remaining sum is $0 → passes.

 This means the exclusion only RELAXES the rules. Transactions that balanced on master still balance (the excluded gain posting was necessarily $0, or the transaction couldn't have balanced).
 Transactions that failed on master may now succeed.

 Verified by test

 The journal above succeeds on the lots branch (tested manually) and would fail on master (no gain exclusion, sum = -$25).

 Edge case: amountless gain posting in disposal

 assets:broker    -5 AAPL {$50} @ $55
 assets:cash       $275
 revenue:gains                           ; amountless
 - Master: inferred amount = $0 (sum of others = $0)
 - Lots branch: gain posting excluded from inference, stays amountless; balance check passes (remaining postings sum to $0)
 - Effect: benign difference in internal representation; identical print output

 Specific concern: revenue:gains account

 An existing account named revenue:gains would:
 1. Already be typed as Gain on master (via gainAccountRegex)
 2. Still appear in income statement (Gain is subtype of Revenue, Type [Revenue] includes it)
 3. Still appear in type:R queries
 4. Only be excluded from balancing inside disposal transactions (those with {...} cost basis)
 5. Never get an auto-inferred gain posting without --lots

 The gainAccountRegex pattern

 Matches (case-insensitive):
 ^(income|revenue)s?:(capital[- ]?)?(gains?|loss(es)?)(:|$)
 Examples: revenue:gains, income:capital-gains, revenues:losses, income:gain:realized

 Does NOT match: revenue:other-gains, expenses:gains, revenue:gaming
 
## Next ?

- more testing with real world journals
- an amountless posting's lotful commodity is not recognised (lot postings are classified before amounts are filled)
- and declaring the amountless posting's account lotful should help, but doesn't
- infer acquire price, dispose price from market price ?
- recognise some common commodity symbols as lotful ?
- consolidate lot tests ?

Remember: don't over-engineer. Build the vision, build high quality, but most of all build what users actually need, and validate that with real users quickly.
