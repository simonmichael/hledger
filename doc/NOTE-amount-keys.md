# Amount Keys and Aggregation Design Discussion

**Date:** 2026-02-01
**Context:** Discussion about MixedAmountKey design, when amounts should combine, and the semantics of @ vs @@ notation.

## Core Questions

### 1. When Should Amount Attributes Be Combined or Discarded?

Different operations have different needs:

| Operation | Preservation Need | Current Behavior |
|-----------|------------------|------------------|
| **Parsing/Printing** | Exact fidelity - preserve everything | Preserves all: @, @@, precision, style |
| **Transaction Balancing** | Check if amounts cancel | Depends on key function |
| **Lot Tracking (FIFO/LIFO)** | Keep acquisition costs separate | Use cost basis {} |
| **Balance Reports** | Aggregate "economically equivalent" amounts | Combines by MixedAmountKey |
| **Market Valuation** | Convert to current value | Discard transacted cost, apply market price |
| **Sorting** | Deterministic ordering | Compare by commodity, then cost |

**Key Tension:** Balance reporting wants aggregation, lot tracking wants separation.

### 2. Should Transacted Cost Be in Amount Keys?

**Current Design (as of 2026-02-01):**
```haskell
-- Unit costs: quantity IS in key (amounts with different unit costs stay separate)
key (Amount "USD" 1 (UnitCost "EUR" 0.9)) = KeyUnit "USD" "EUR" 0.9

-- Total costs: quantity NOT in key (amounts with different totals combine)
key (Amount "USD" 1 (TotalCost "EUR" 90)) = KeyTotal "USD" "EUR"
```

**Implications:**
- `$1 @ €0.9` and `$1 @ €0.8` → separate (different unit costs)
- `$1 @@ €90` and `$1 @@ €80` → combine (total cost quantity ignored)
- When combining total costs, the cost quantities sum: `$1 @@ €1` + `$-2 @@ €1` = `$-1 @@ €2`

**Design Question:** What is @ and @@ notation FOR?

#### Interpretation A: Recording Only
- @ and @@ just record historical transaction cost
- NOT used for lot tracking (that's what {} is for)
- Balance reports ignore them or convert to cost
- **Key excludes all transacted costs**

#### Interpretation B: @ is Lot Marker (Current)
- @ creates implicit lots (same unit cost = same lot)
- @@ is shorthand, not for lot tracking
- Balance reports keep @ amounts separate, combine @@ amounts
- **Current design: unit cost in key, total cost not in key**

#### Interpretation C: Both Create Lots
- Both @ and @@ create lots (fine-grained separation)
- Explicit "aggregate" operation for reporting
- **Key includes all cost info**

### 3. Redundancy Between MixedAmountKey and Amount

**Current Redundancy:**
```haskell
-- Amount stores:
Amount {aCommodity = "USD", aQuantity = 100, aCost = Just (UnitCost "EUR" 0.9)}

-- Key duplicates some fields:
KeyUnit "USD" "EUR" 0.9

-- Invariant: key(amount) must match map key
-- Problem: Easy to violate, memory duplication
```

**Alternative Designs:**

#### Option A: Key is Projection (Current)
- Key derived from Amount
- Flexible but has invariant to maintain

#### Option B: Key Owns Identity
```haskell
data AmountKey = AmountKey Commodity CostKey BasisKey
data AmountValue = AmountValue Quantity Style
type MixedAmount = Map AmountKey AmountValue
```
- No redundancy, key is source of truth
- Harder to work with single amounts

#### Option C: Different Types for Different Stages
```haskell
data ParsedAmount = ... -- everything preserved
data Amount = ...       -- normalized, only cost basis matters
data MixedAmount = ...  -- aggregated by key
```
- Each type optimized for use case
- More types, conversion overhead

#### Option D: Parameterized Amount
```haskell
data Amount cost = Amount Commodity Quantity cost
type ParsedAmount = Amount (Maybe CostNotation)
type LotAmount = Amount (Maybe CostBasis)
type SimpleAmount = Amount ()
```
- Very flexible, explicit
- Complex types

## Key Insights from Greenfield Design Exercise

From designing a clean-slate implementation:

1. **The Key Function IS the Specification**
   - Answers "when do amounts combine?"
   - Everything else follows mechanically

2. **MixedAmount Forms a Commutative Monoid**
   ```haskell
   a + b = b + a                    -- commutative
   (a + b) + c = a + (b + c)        -- associative
   a + 0 = a                        -- identity
   ```
   - Essential for order-independent processing
   - Enables parallel aggregation

3. **Core Operation: add1**
   ```haskell
   add1 :: Amount -> MixedAmount -> MixedAmount
   add1 a (MixedAmount m) = MixedAmount $ M.insertWith combine (key a) a m
   ```
   - All other operations build on this
   - Simple and compositional

4. **Total Cost Semantics (Current)**
   - When amounts with total costs combine, costs sum
   - `$1 @@ €1` + `$-2 @@ €1` = `$-1 @@ €2`
   - Rationale: Accumulates cost of repeated purchases

## Proposed Design Direction

**Cleanest approach may be:**

1. **@ and @@ are RECORDING ONLY, not lot tracking**
   - Record what you paid in the transaction
   - Don't use for lot tracking (use {} explicitly)

2. **Normalize amounts after parsing**
   ```haskell
   normalize :: ParsedAmount -> Amount
   -- Strips @ and @@ (for recording only)
   -- Preserves {} (for lot tracking)
   ```

3. **MixedAmount keys only on commodity and cost basis**
   ```haskell
   key :: Amount -> Key
   key (Amount c _ (Just basis)) = KeyWithBasis c basis
   key (Amount c _ Nothing)      = KeySimple c
   -- Transacted cost not in key at all
   ```

4. **Choose aggregation level explicitly**
   ```haskell
   aggregateSimple :: MixedAmount -> Map Commodity Quantity
   aggregateLots   :: MixedAmount -> Map (Commodity, Basis) Quantity
   convertToCost   :: MixedAmount -> MixedAmount
   ```

5. **For lot tracking, REQUIRE {} notation**
   - Don't infer lots from @ or @@
   - Explicit is better than implicit

## Open Questions

1. **Should total costs sum when combining?**
   - Current: Yes (`$1 @@ €1` + `$1 @@ €1` = `$2 @@ €2`)
   - Alternative: Keep first cost (`$2 @@ €1`)

2. **What about balance assertions with costs?**
   - Should `= $100 @ €90` check the cost too?
   - Or just the commodity quantity?

3. **Conversion to cost:**
   - When displaying `$100 @ €0.9` as cost, show `€90`
   - What about `$100 @@ €88`? (unit cost would be `€0.88`, but we don't have it)

4. **Mixed @ and @@ in same transaction:**
   ```journal
   assets:cash   $100 @ €0.9
   assets:cash   $50 @@ €44
   ```
   Should these combine? Currently they don't (different keys).

## Related Files

- **Implementation:** hledger-lib/Hledger/Data/Types.hs (MixedAmountKey)
- **Operations:** hledger-lib/Hledger/Data/Amount.hs (maAddAmount, sumSimilarAmountsUsingFirstCost)
- **Tests:** hledger-lib/Hledger/Data/Amount.hs:1425 ("adding mixed amounts with total costs")
- **Other docs:**
  - doc/REFACTOR-prices-duplication.md
  - doc/CODE.md

## Language/Specification Explorations Considered

Alternative ways to model/specify the semantics:

1. **APL** - Array operations, compact but limited readability
2. **SQL** - GROUP BY semantics map perfectly to MixedAmount aggregation
3. **Equational Specification** - Mathematical laws as equations
4. **Alloy** - Constraint-based model finder for edge cases
5. **Property-Based Tests** - Laws as executable properties (QuickCheck)
6. **Prolog/Datalog** - Logic programming for rules
7. **TLA+** - Formal specification with model checking

**Most promising for hledger:**
- Equational spec in doc (concise, mathematical)
- SQL prototype (test grouping interactively)
- Property tests in Haskell (verify laws)
- Alloy for edge case discovery (optional)

## Summary

The core design choice is: **What does the key function encode?**

Current answer:
- Commodity (always)
- Unit cost with quantity (@ creates lots)
- Total cost without quantity (@@ doesn't create lots)
- Cost basis (always, {} creates lots)

Alternative answer:
- Commodity (always)
- Cost basis only (only {} creates lots)
- @ and @@ are recording notation, not semantic

The choice affects:
- How balance reports aggregate
- Whether lot tracking works implicitly or requires {}
- Memory usage and complexity

**Next steps:** Consider whether @ should be recording-only, with explicit {} required for lot tracking.
