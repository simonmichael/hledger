# SPEC: print command

Notes on some of print's behaviour.

## Effects of certain output flags

### No output flags (default output)

By default, print tries to show each entry as it is written in the journal file,
except for alignment. And it shows entries in date-then-parse order.

### `--round`

Controls rounding/padding of displayed amounts:

- `none` — show original decimal digits, as in the journal (default)
- `soft` — add or remove trailing decimal zeros to match commodity precision
- `hard` — round posting amounts to commodity precision (can unbalance transactions)
- `all` — also round cost amounts to commodity precision (can unbalance transactions)

### `--verbose-tags`

Makes certain normally-hidden tags visible (in comments):

- `ptype: acquire/dispose/transfer-from/transfer-to` — lot posting classification
- `cost-tagged:` — marks postings that have or were given a transaction cost
- `conversion-tagged:` — marks equity conversion postings
- `generated-posting:` — marks auto-generated postings (from transaction modifiers or --infer-equity)
- `modified-transaction:` — marks transactions modified by auto posting rules
- `generated-transaction: <period>` — marks forecast transactions from periodic rules

Without this flag, these tags still exist internally (queryable) but don't appear in print output.

### `-x` / `--explicit`

Shows all inferred balancing amounts and balancing costs:

- Inferred amounts are shown
- Inferred costs are shown
- Balance assignment amounts are shown explicitly

### `--lots`

Triggers lot calculation, which restructures postings:

- Cost basis fields are made explicit, with missing parts filled in
- Lots acquired on the same day get uniquifying labels added if needed
- All lot postings get specific lot subaccounts added (e.g. `assets:stocks` → `assets:stocks:{2026-01-15, $50}`)
- Transfer postings and dispose postings affecting multiple lots are split into one per lot
