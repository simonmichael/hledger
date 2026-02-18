# Plan for calculating lots, 2026-02

## Original plan

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

## Next ?

key features:

- read hledger lot syntax
  - print hledger lot syntax by default
  - print ledger lot syntax in a new ledger output format

- update manual (hledger.m4.md)

robustness:
- pretty error messages
- don't add lot subaccounts if they are already explicit; and raise an error if the explicit lot subaccount is wrong
- test/improve handling of explicit ptype (or _ptype) tags
- figure out if lot subaccount names are a problem for beancount export. Just avoid --lots ? What about explicit lot subaccounts ?

more features:
- try again to infer basis even with no {} annotation ?
- recognise some common commodity symbols as lotful ?
- infer acquisition cost, disposal price from market price ?

Remember: don't over-engineer. Build the vision, build high quality, but also: build what users actually need, and validate that with real users quickly.
