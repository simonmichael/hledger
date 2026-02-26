# Rules for detecting special postings

Here's an overview of ways hledger infers things based on certain configurations of postings.

  1. journalClassifyLotPostings (Lots.hs)

  Detects lot-related postings and tags them with _ptype. Classifications:

  ┌──────────────────┬───────────────────────────┬────────────────────────────────────────────────────────────────────────────────────────────────┐
  │     Pattern      │            Tag            │                                           Conditions                                           │
  ├──────────────────┼───────────────────────────┼────────────────────────────────────────────────────────────────────────────────────────────────┤
  │ acquire          │ _ptype:acquire            │ Positive amount + cost basis {}; OR positive lotful amount (commodity/account has lots: tag)   │
  ├──────────────────┼───────────────────────────┼────────────────────────────────────────────────────────────────────────────────────────────────┤
  │ dispose          │ _ptype:dispose            │ Negative amount + cost basis; OR negative lotful amount, with no opposite-sign counterpart in  │
  │                  │                           │ a different account                                                                            │
  ├──────────────────┼───────────────────────────┼────────────────────────────────────────────────────────────────────────────────────────────────┤
  │ transfer-from    │ _ptype:transfer-from      │ Negative amount + cost basis (or lotful), AND a matching positive counterpart in a different   │
  │                  │                           │ account with same commodity and exact quantity                                                 │
  ├──────────────────┼───────────────────────────┼────────────────────────────────────────────────────────────────────────────────────────────────┤
  │                  │                           │ Positive amount + cost basis with matching negative counterpart; OR positive lotful amount     │
  │ transfer-to      │ _ptype:transfer-to        │ with no @ cost and a matching transfer-from counterpart; OR bare positive asset posting with a │
  │                  │                           │  matching transfer-from counterpart                                                            │
  ├──────────────────┼───────────────────────────┼────────────────────────────────────────────────────────────────────────────────────────────────┤
  │ gain             │ _ptype:gain               │ Posting to a Gain-type account                                                                 │
  ├──────────────────┼───────────────────────────┼────────────────────────────────────────────────────────────────────────────────────────────────┤
  │ same-account     │ _ptype:transfer-from /    │ Within the same account, positive and negative postings with the same commodity and exact      │
  │ transfer         │ transfer-to               │ absolute quantity are paired as transfers                                                      │
  └──────────────────┴───────────────────────────┴────────────────────────────────────────────────────────────────────────────────────────────────┘

  Detection is per-transaction. Transfer detection uses precomputed maps keyed by (commodity, |quantity|) for O(n) matching.

  ---
  2. journalTagCostsAndEquityAndMaybeInferCosts (Journal.hs → Transaction.hs)

  Detects equity conversion postings and corresponding costful postings. Called twice in the pipeline (before balancing with addcosts=False to detect,
  after with addcosts=True to add costs when --infer-costs).

  ┌─────────────────────────┬─────────────────────┬───────────────────────────────────────────────────────────────────────────────────────────────┐
  │         Pattern         │         Tag         │                                          Conditions                                           │
  ├─────────────────────────┼─────────────────────┼───────────────────────────────────────────────────────────────────────────────────────────────┤
  │ conversion posting pair │ _conversion-posting │ Two adjacent postings to accounts declared as Conversion type (type V), each with a           │
  │                         │                     │ single-commodity amount and no cost                                                           │
  ├─────────────────────────┼─────────────────────┼───────────────────────────────────────────────────────────────────────────────────────────────┤
  │ costful posting         │ _cost-posting       │ A non-conversion posting whose amount matches -ca1 and whose cost matches ca2 (or vice        │
  │ matching conversions    │                     │ versa), where ca1/ca2 are the conversion pair amounts                                         │
  ├─────────────────────────┼─────────────────────┼───────────────────────────────────────────────────────────────────────────────────────────────┤
  │ costless posting        │ _cost-posting       │ (When no costful match exists) A costless single-commodity posting whose amount matches -ca1  │
  │ matching conversions    │                     │ or -ca2; must be unambiguous (no other costless posting with the same amount)                 │
  └─────────────────────────┴─────────────────────┴───────────────────────────────────────────────────────────────────────────────────────────────┘

  In addcosts=True mode, the matched costless posting also gets a TotalCost added.

  The _cost-posting tag causes the balancer to strip costs from that posting (preventing double-counting with conversion postings).

  ---
  3. journalInferEquityFromCosts (Journal.hs → Transaction.hs → Posting.hs)

  Detects costful postings that lack corresponding equity conversion postings:

  ┌────────────────────────┬───────────────────────────────────────────────┬──────────────────────────────────────────────────────────────────────┐
  │        Pattern         │                    Action                     │                              Conditions                              │
  ├────────────────────────┼───────────────────────────────────────────────┼──────────────────────────────────────────────────────────────────────┤
  │ costful posting        │ Generates a pair of _conversion-posting +     │ Posting has cost amounts (@ or @@) AND is NOT already tagged         │
  │ without conversions    │ _generated-posting tagged postings            │ _cost-posting (i.e., not already matched to existing conversion      │
  │                        │                                               │ postings)                                                            │
  └────────────────────────┴───────────────────────────────────────────────┴──────────────────────────────────────────────────────────────────────┘

  For each cost amount, two conversion postings are generated under <equityAcct>:<sorted-commodities>: with amounts that offset the cost.

  ---
  Summary of all hidden tags used

  ┌──────────────────────┬─────────────────────────────────────────────────────────────────────┬──────────────────────────────────────────────────┐
  │         Tag          │                               Set by                                │                     Meaning                      │
  ├──────────────────────┼─────────────────────────────────────────────────────────────────────┼──────────────────────────────────────────────────┤
  │ _ptype:acquire       │ journalClassifyLotPostings                                          │ Lot acquisition                                  │
  ├──────────────────────┼─────────────────────────────────────────────────────────────────────┼──────────────────────────────────────────────────┤
  │ _ptype:dispose       │ journalClassifyLotPostings                                          │ Lot disposal                                     │
  ├──────────────────────┼─────────────────────────────────────────────────────────────────────┼──────────────────────────────────────────────────┤
  │ _ptype:transfer-from │ journalClassifyLotPostings                                          │ Lot transfer source                              │
  ├──────────────────────┼─────────────────────────────────────────────────────────────────────┼──────────────────────────────────────────────────┤
  │ _ptype:transfer-to   │ journalClassifyLotPostings                                          │ Lot transfer destination                         │
  ├──────────────────────┼─────────────────────────────────────────────────────────────────────┼──────────────────────────────────────────────────┤
  │ _ptype:gain          │ journalClassifyLotPostings / journalInferAndCheckDisposalBalancing  │ Capital gain/loss posting                        │
  ├──────────────────────┼─────────────────────────────────────────────────────────────────────┼──────────────────────────────────────────────────┤
  │ _cost-posting        │ journalTagCostsAndEquityAndMaybeInferCosts /                        │ Has (or could have) cost matching conversion     │
  │                      │ journalInferEquityFromCosts                                         │ postings                                         │
  ├──────────────────────┼─────────────────────────────────────────────────────────────────────┼──────────────────────────────────────────────────┤
  │ _conversion-posting  │ journalTagCostsAndEquityAndMaybeInferCosts /                        │ Equity conversion posting                        │
  │                      │ journalInferEquityFromCosts                                         │                                                  │
  ├──────────────────────┼─────────────────────────────────────────────────────────────────────┼──────────────────────────────────────────────────┤
  │ _generated-posting   │ journalInferEquityFromCosts                                         │ Machine-generated posting                        │
  └──────────────────────┴─────────────────────────────────────────────────────────────────────┴──────────────────────────────────────────────────┘
