# Typeclass Semantics for Amounts

**Date:** 2026-02-01
**Context:** Design question on whether to define amount semantics via Monoid first, then Num, or vice versa.

## Question

Is it better to define amounts' semantics in terms of standard typeclasses like Monoid first, and then define Num operations in terms of those?

## Current State

### MixedAmount (hledger-lib/Hledger/Data/Amount.hs:848-863)

```haskell
instance Semigroup MixedAmount where
  (<>) = maPlus

instance Monoid MixedAmount where
  mempty = nullmixedamt

instance Num MixedAmount where
  (+) = maPlus          -- delegates to Monoid operation
  (*) = error "..."     -- intentionally partial
  signum = error "..."  -- intentionally partial
```

### Amount (hledger-lib/Hledger/Data/Amount.hs:300-307)

```haskell
instance Num Amount where
  (+) = similarAmountsOp (+)  -- errors if commodities differ
  (*) = similarAmountsOp (*)  -- questionable semantics
  -- No Semigroup/Monoid instance at all
```

## Analysis

### MixedAmount Follows Monoid-First Design

**Strengths:**
- ✅ Core semantics defined via `maPlus :: MixedAmount -> MixedAmount -> MixedAmount` (line 905-906)
- ✅ Num instance delegates to Monoid (`(+) = maPlus`)
- ✅ Explicitly rejects nonsensical operations (multiplication, signum)
- ✅ Matches DESIGN-amounts-keys.md observation: "MixedAmount forms a commutative monoid" (line 114-121)

### Amount Does NOT

**Problems:**
- ❌ Only has Num, no Monoid/Semigroup
- ❌ Can't have a proper Monoid (no identity: `nullamt + $1` ≠ `$1` due to commodity difference)
- ❌ Num instance is **partial** (errors on mismatched commodities)
- ❌ Implements `(*)` with unclear semantics

## Recommendation: Monoid-First is Better

### Reasons

1. **Semantic Clarity**
   - The key operation is aggregation (combining amounts)
   - Monoid captures exactly this - no more, no less
   - When deciding "what goes in the key?", you're really asking "when does `<>` combine vs keep separate?"

2. **Partial Num is a Code Smell**
   - If you need `error` in Num methods, the type doesn't truly satisfy the Num contract
   - Better to not have the instance than to have a partial one

3. **Aligns with Design Documents**
   - DESIGN-amounts-keys.md explicitly identifies the commutative monoid structure as fundamental (line 114-121)
   - The core operation `add1` (line 123-129) is monoid addition

4. **Better Composition**
   - Monoid instances compose beautifully (e.g., `Map k v` is a Monoid if `v` is)
   - This helps with the `MixedAmount = Map Key Amount` design

5. **Forces Clear Thinking**
   - Monoid has simpler laws (associativity + identity)
   - Easier to verify and test
   - Num brings unnecessary baggage (multiplication, division, etc.)

### Theoretical Foundation

From abstract algebra perspective:

- **Monoid**: Just needs associativity, identity, and closure - minimal structure
- **Num**: Implies ring-like structure with multiplication - too much structure for amounts
- **For amounts**: Addition forms a commutative monoid, but multiplication is not well-defined
  - What is `$1 * $1`? `$1`? Type error? `$²1`?
  - What is `$1 * €1`? Nonsensical.

## Concrete Suggestions

### Short Term (Low Risk)
- Keep current MixedAmount design (already follows Monoid-first)
- Document that Num instance is a convenience wrapper around Monoid
- Consider deprecation warnings on partial operations

### Medium Term (Moderate Risk)
- Drop Amount's Num instance entirely
- Add clear utility functions for amount arithmetic:
  ```haskell
  amountPlus :: Amount -> Amount -> Either String Amount
  -- Returns Left if commodities don't match
  ```

### Long Term (Requires Migration)
- Consider Amount Semigroup instance IF there's a sensible (<>) that doesn't require same commodity
- Or: Keep Amount without Monoid (since no proper identity) but use Semigroup for combining
- Make MixedAmount the primary abstraction for all arithmetic

## Tradeoffs

**Lost Convenience:**
- Can't write `amount1 + amount2`
- Must write `amount1 <> amount2` or explicit function calls

**But:**
- This is already partial anyway (errors on commodity mismatch)
- Making it explicit forces handling the error case
- More honest API

## Related

- **DESIGN-amounts-keys.md** - Discusses MixedAmount monoid structure
- **hledger-lib/Hledger/Data/Amount.hs:905** - `maPlus` implementation
- **hledger-lib/Hledger/Data/Amount.hs:848** - Current Semigroup/Monoid instances

## Open Questions

1. Should Amount have a Semigroup instance? What would `<>` mean for amounts with different commodities?
2. If we remove Num from Amount, what impact on existing code?
3. Should we use a different type for "amounts that can be added" vs "amounts that must be kept separate"?
4. Could we use a phantom type to track whether an Amount is "simple" (can add) vs "complex" (need MixedAmount)?

## Conclusion

**Monoid-first design is theoretically cleaner and aligns better with amount semantics.** The current MixedAmount implementation already follows this pattern. Amount's Num instance is a historical convenience that causes more confusion than it solves.

Recommendation: Embrace Monoid as the primary abstraction for amount aggregation, with Num as a thin, well-documented convenience layer that explicitly delegates to Monoid operations.
