# Journal finalising

After parsing, a journal goes through a **finalisation** pipeline (`journalFinalise` in
`Hledger.Read.Common`) that infers missing information, checks validity, and enriches
postings with computed metadata.

("Finalising" is not easy to say, better suggestions welcome.)

## What gets inferred, and from what

Here are the main kinds of information that finalisation infers or computes, roughly in the order they happen.

Note: "cost" means @/@@ (AKA transacted cost); "cost basis" means {} (acquisition cost & info).

| What is inferred                                            | From what                                                 | Step                                     |
|-------------------------------------------------------------|-----------------------------------------------------------|------------------------------------------|
| Account types                                               | Account declarations, account names, parent accounts      | journalAddAccountTypes                   |
| Consistent posting amount styles                            | Posting amounts written in journal                        | journalStyleAmounts                      |
| Forecast transactions                                       | Periodic transaction rules + forecast period              | journalAddForecast                       |
| Posting tags inherited from accounts                        | Account declarations                                      | journalPostingsAddAccountTags            |
| Location of conversion equity postings and costful postings | @/@@ annotations + adjacent conversion account postings   | journalTagCostsAndEquityAndMaybeInferCosts(1st)         |
| Auto postings                                               | Auto posting rules + postings in journal                  | journalAddAutoPostings                   |
| Lot posting types                                           | Cost basis + amount sign + account type + counterpostings | journalClassifyLotPostings               |
| Cost from cost basis                                        | Costless acquire postings with a cost basis               | journalInferPostingsTransactedCost       |
| Transaction-balancing amounts                               | Counterpostings (or their costs)                          | journalBalanceTransactions               |
| Transaction-balancing costs                                 | Costless two-commodity transactions                       | transactionInferBalancingCosts           |
| Balance assignment amounts                                  | Running account balances vs asserted balances             | journalBalanceTransactions               |
| Canonical commodity styles                                  | All posting amounts now present after balancing           | journalInferCommodityStyles              |
| Posting tags inherited from commodities                     | Commodity declarations                                    | journalPostingsAddCommodityTags          |
| Costs from equity postings (--infer-costs)                  | Equity conversion posting pairs                           | journalTagCostsAndEquityAndMaybeInferCosts(2nd)         |
| Equity postings from costs (--infer-equity)                 | Costful postings                                          | journalInferEquityFromCosts              |
| Market prices from costs                                    | Costful postings                                          | journalInferMarketPricesFromTransactions |
| Lot subaccounts                                             | Lot state tracking (applying FIFO etc.)                   | journalCalculateLots                     |

## Current pipeline sequence

```
journalFinalise
  -- Setup
  1.  journalSetLastReadTime
  2.  journalAddFile
  3.  journalReverse

  -- Account types and amount styles (pure, no errors)
  4.  journalAddAccountTypes            -- builds jaccounttypes map
  5.  journalStyleAmounts               -- infer preliminary commodity display styles, and apply to postings

  -- Generate forecast transactions
  6.  journalAddForecast                -- if --forecast, generate forecast transactions from periodic rules

  -- Account tags
  7.  journalPostingsAddAccountTags     -- propagate account tags to postings

  -- Pre-balancing cost/equity tagging
  8.  journalTagCostsAndEquityAndMaybeInferCosts(1st)  -- tag conversion equity postings + redundant costs (helps balancer ignore them)

  -- Generate auto postings
  9.  journalAddAutoPostings            -- if --auto, do transaction balancing (preliminary) to infer some missing amounts/costs,
                                        -- then apply auto posting rules. Calls journalBalanceTransactions.

  -- Lot classification and transacted cost inference (before balancing)
  10. journalClassifyLotPostings         -- tag lot postings as acquire/dispose/transfer-from/transfer-to
  11. journalInferPostingsTransactedCost  -- infer cost from cost basis of acquire postings 

  -- Transaction balancing (main)
  12. journalBalanceTransactions         -- infer remaining balancing amounts, balancing costs, and balance assignment amounts;
                                        -- and check transactions balanced and (unless --ignore-assertions) balance assertions satisfied.

  -- Post-balancing enrichment
  13. journalInferCommodityStyles        -- infer canonical commodity styles, now with all amounts present
  14. journalPostingsAddCommodityTags    -- propagate commodity tags to postings
  15. journalTagCostsAndEquityAndMaybeInferCosts(2nd)   -- if --infer-costs, infer costs from equity conversion postings
  16. journalInferEquityFromCosts        -- if --infer-equity, infer equity conversion postings from costs
  17. journalInferMarketPricesFromTransactions  -- infer market prices from costs
  18. journalRenumberAccountDeclarations  -- renumber account declarations for consistent ordering

  -- Lot calculation
  19. journalCalculateLots              -- with --lots: evaluate lot selectors, apply reduction methods,
                                        -- calculate lot balances, add explicit lot subaccounts
```

## Sequencing constraints

These are the known ordering requirements between steps.
An arrow A → B means "A must run before B".

### Hard constraints

- **journalAddAccountTypes → journalClassifyLotPostings**
  Classification looks up account types to identify Asset accounts.

- **journalPostingsAddAccountTags → journalClassifyLotPostings**
  Classification may need `lots:` tags inherited from account declarations (in `ptags`).

- **journalTagCostsAndEquityAndMaybeInferCosts(1st) → journalBalanceTransactions**
  The balancer needs to know which costs are redundant (equity-paired) to ignore them.

- **journalClassifyLotPostings → journalInferPostingsTransactedCost**
  Transacted cost inference skips `transfer-to` postings (which have no selling price),
  so it needs the `_ptype` tag to be present.

- **journalInferPostingsTransactedCost → journalBalanceTransactions**
  The balancer needs transacted costs to correctly infer missing amounts
  (e.g., infer `-$500` for cash, not `-10 AAPL {$50}`).

- **journalBalanceTransactions → journalPostingsAddCommodityTags**
  Balancing may infer missing posting amounts, changing their commodity from `AUTO` to a
  real commodity. Commodity tag propagation should see the real commodity so it can
  add the right tags. (In practice this is a soft constraint: `journalInferPostingsCostBasis`
  reads `jdeclaredcommoditytags` directly rather than relying on commodity tags in `ptags`.)

- **journalClassifyLotPostings → journalCalculateLots**
  Lot calculation reads `_ptype` tags to identify acquire/dispose/transfer postings.

### Design decisions

- **Acquisitions require explicit `{}` cost basis annotation.**
  Previously, `journalInferPostingsCostBasis` could infer `{$50}` from `@ $50` on lotful
  commodities/accounts. This step has been removed to allow classification to run before
  balancing. Without it, `@ $50` alone on a lotful commodity does not trigger lot classification.

- **Classification before balancing resolves the poriginal conflict.**
  Since `_ptype` tags are added before the balancer sets `poriginal`, the tags are
  naturally preserved in `poriginal` and visible in `print --verbose-tags` output.

## Key fields on Amount

These fields are central to the inference pipeline:

- **acost** — transacted cost (`@ $50` or `@@ $500`). Used by balancer, equity tagging.
  Sources: parsed from journal, inferred by balancer,
  inferred from equity postings (--infer-costs),
  inferred from cost basis.

- **acostbasis** — lot cost basis (`{$50}` or `{2024-01-15, "lot1", $50}`).
  Sources: parsed from journal (explicit `{}` required for lot tracking).
  Used by lot posting classification and lot calculation.

These two fields are sometimes both present on the same amount (both explicit, or one inferred).
`journalInferPostingsTransactedCost` and `transactionInferBalancingCosts` may infer
`acost` from `acostbasis` or from counterpostings, so in later steps you cannot assume
that the presence of `acost` means the user wrote `@ $X`.

## Key fields on Posting

- **ptags** — all tags (user-written + inherited from account/commodity + hidden computed tags).
  Tags are added by: account tag propagation, commodity tag propagation, equity tagging,
  lot classification (`_ptype`), auto posting generation (`_generated-posting`).

- **poriginal** — snapshot of the posting before amount/cost inference, used by `print`
  to show journal entries close to how they were written. Set by: `transactionInferBalancingCosts`,
  `transactionInferBalancingAmount`, balance assignment processing,
  `postingInferTransactedCost`. Since classification runs before these steps,
  `_ptype` tags are naturally included in `poriginal`.

## Conditional steps

Several steps only run with specific flags:

| Step                              | Flag              |
|-----------------------------------|-------------------|
| journalAddForecast                | --forecast        |
| journalAddAutoPostings            | --auto            |
| journalTagCostsAndEquity (2nd)    | --infer-costs     |
| journalInferEquityFromCosts       | --infer-equity    |
| journalCalculateLots              | --lots            |
