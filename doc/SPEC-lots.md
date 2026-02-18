# Lot tracking

Here is the current specification for lots functionality, some of which has been implemented already.

See also 
- hledger manual > Cost basis / Lot syntax
- https://joyful.com/hledger+lot+tracking older, related dev notes


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

There will be a new `--lots` flag, which will enable

- automatic lot inference, tracking and validation
- display of per-lot subaccounts and balances in all reports.

In the journal, it will be possible to record lot operations 

1. fully implicitly, with maximum inference;
2. partly explicitly, with missing parts inferred;
3. or fully explicitly, requiring no inference.

A typical workflow might be to use 1 primarily, or when processing/converting old journals;
and use `print` to convert to 3 for troubleshooting or reporting.

## Transacted cost and cost basis

- Transacted cost is the conversion rate used to balance a single transaction.
  It is recorded with @ or @@ annotations after an amount.

- Cost basis is the nominal acquisition cost of a lot,
  along with its acquisition date and perhaps a label.
  It is preserved (along with the lot's balance), throughout the lifetime of the lot, from acquisition through transfers to final disposal.
  It is recorded with hledger lot syntax (consolidated {} notation);
  we can also read ledger lot syntax (separate {}, [], () annotations).

A posting amount can have transacted cost and/or cost basis.
When displaying a posting with both, we show cost basis before transacted cost (like beancount).

"Transacted cost" is an awkward term, overlapping with "cost basis",
but it avoids too much change from the historical "cost", and we don't have a better alternative, currently.

The hledger manual has more detail on these.

## Lot names

A lot's cost basis also serves as the lot name. 

In hledger, a full lot name is rendered as either {YYYY-MM-DD, "LABEL", COST} or {YYYY-MM-DD, COST}.
That is, two or three parts inside curly braces: 
- a date in strict ISO format
- an optional label in double quotes
- a single-commodity hledger amount
with a comma and space between them.
When parsing, spaces inside the braces and around the commas are optional and ignored.
This is similar to Beancount's lot syntax, except it requires DLC order (date, label, cost) and it supports hledger's flexible amount syntax.

Full lot names may be used as subaccount names, identifying lots within a parent account.
Eg assets:stocks:aaaa:{2026-02-10, "lot1", $50}.
So the label and the cost's commodity symbol may not contain double-quotes, colons, or semicolons.

Partial lot names are also used; these have some or all of the parts missing.
The minimal partial lot name is rendered as {}.

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
type LotName = CostBasis
data LotId = LotId { lotDate :: Day, lotLabel :: Maybe Text }
```

A definite cost basis (used for a fully resolved lot) has all fields present except cbLabel which is only present when needed for uniqueness.
A partial cost basis (used as a lot selector or during inference) may have any fields missing.

## Lotful commodities and accounts

Commodities and/or accounts can be declared as lotful, by adding a "lots" tag to their declaration.
This signifies that their postings always involve a cost basis and lots, 
so these should be inferred if not written explicitly.

(In future, we will also recognise some common commodity symbols as lotful, even without the lots tag.)

## Inferring cost basis from transacted cost

In postings with a positive amount, involving a lotful commodity or account,
which have a transacted cost but no explicit cost basis annotation,
we infer a cost basis from the transacted cost.

(journalInferPostingsCostBasis)

## Lot postings

After inferring cost basis, we can identify lot postings. These are postings which

- have a cost basis annotation (any of the {}, [], () notations)
- or involve a lotful commodity or account
- or are matched by a transfer-from posting in the same transaction.

We classify lot postings, based on their amount sign and
whether they are matched by an opposite posting in the same transaction,
and add a "ptype" tag to record their type:

- Lot postings with a positive amount get ptype "acquire" or (if matched) "transfer-to".
- Lot postings with a negative amount get ptype "dispose" or (if matched) "transfer-from".

(journalClassifyLotPostings)

## Inferring transacted cost from cost basis

After classifying lot postings,
in acquire postings which have no transacted cost annotation,
we infer a transacted cost from the cost basis.

(journalInferPostingsTransactedCost)

## Lot posting effects

- An acquire posting creates a new lot, with a cost basis either specified
  or inferred from the transacted cost (or perhaps market price, in future).

- A transfer-from posting selects one or more lots to be transferred elsewhere,
  following some selection method. Either

  - it has a lot selector (a full or partial cost basis annotation),
    which must unambiguously select a single existing lot ("SPECID" method)
  - or it has no lot selector, in which case a default method is used, selecting one or more existing lots.
    Planned lot selection methods include:
    FIFO (oldest first, across all accounts), FIFO1 (oldest first, within each account),
    LIFO (newest first, across all accounts), LIFO1 (newest first, within each account),
    and SPECID (explicit selection via lot selector).
    The method should be configurable in a relatively fine-grained way —
    per account, per commodity, per time period, perhaps even per posting.
    In future, the method might be specified with an annotation like {!METHOD, ...} inside the lot syntax.

- A transfer-to posting mirrors a corresponding transfer-from posting in the same transaction,
  recreating its lot(s) under a new parent account.
  It doesn't need a lot selector; if it has one, it must select the same lot as the transfer-from posting.
  Transfer postings (both from and to) must not have explicit transacted cost (@ or @@); this is an error.

- A dispose posting selects one more lots to be disposed (sold), like a transfer-from posting.
  It must also have a transacted cost, either explicit or inferred from transaction balancing
  (or from market price, in future).
  When the dispose posting has no cost basis annotation but involves a lotful commodity or account,
  the cost basis is inferred from the selected lot, and the transacted cost
  (if inferred by the balancer as @@) is normalized to unit cost (@).

## Lot transactions

Lot transactions are transactions with lot postings.
We require that a transaction's lot postings are all of similar type: all acquire, or all transfer, or all dispose.
So lot transactions can be classified as "acquire", "transfer", or "dispose" (we don't record this explicitly).

## Gain postings

A gain posting is a posting to an account of Gain type (a subtype of Revenue).
We use this gain account to record capital gain and/or capital loss (depending on the amount sign).
The special account type helps hledger identify these postings.

## Transaction balancing

There are two kinds of transaction balancing:

1. **Normal transaction balancing**
   All journal entries, including lot-related ones, must pass this.
   When summing postings it uses their transacted costs (not cost basis), if any.
   And it excludes (ignores) capital gain/loss postings, identified by their Gain account type.

2. **Disposal balancing**
   Journal entries involving lot disposals get this additional balancing pass.
   When summing postings it uses their cost basis (not transacted cost), if any.
   And it includes gain postings, or will infer one if needed.

The different handling of gain postings allows both kinds of transaction balancing
to succeed with the same journal entries.

## Inferring/checking capital gain

A disposal transaction's total realised capital gain/loss is calculated by 
comparing the lot acquisition cost(s) for each dispose posting, and the total transacted disposal price.

If the transaction contains a gain posting (or more than one),
the recorded gain is expected to match the calculated gain.

Otherwise, a gain posting is inferred, posting the calculated gain to the alphabetically first Gain account.
This helps the transaction to pass disposal balancing.

## Processing pipeline

Lot-related processing can be thought of as an optional extra journal processing step, 
enabled by the --lots flag. 

But all the inferring conveniences make it quite interdependent with the other processing steps.
See SPEC-finalising for more details of the implementation.
