{-|
This is the root of the @hledger-lib@ package and the @Hledger.*@ module hierarchy.
hledger-lib is the core engine used by various hledger UIs and tools,
providing the main data types, file format parsers, reporting logic, and utilities.
-}

module Hledger (
  -- $DOCS
  module X
 ,tests_Hledger
)
where

import           Hledger.Data    as X
import           Hledger.Read    as X
import           Hledger.Reports as X
import           Hledger.Query   as X
import           Hledger.Utils   as X

tests_Hledger = testGroup "Hledger" [
   tests_Data
  ,tests_Query
  ,tests_Read
  ,tests_Reports
  ,tests_Utils
  ]


{- $DOCS

This is also the starting point for hledger's haddock docs,
describing Hledger's implementation for developers.
These can be viewed
in your code editor,
or in a web browser (eg with @make haddock@),
or on Hackage (starting at [hledger-lib:Hledger](https://hackage.haskell.org/package/hledger-lib/docs/Hledger.html)).

== See also:

- hledger:Hledger.Cli
- hledger-ui:Hledger.UI
- hledger-web:Hledger.Web
- [The README files](https://github.com/search?q=repo%3Asimonmichael%2Fhledger+path%3A**%2FREADME*&type=code&ref=advsearch)
- [The high-level developer docs](https://hledger.org/dev.html)

== Miscellaneous notes

=== Precision and rounding

Numeric amounts in hledger are represented by "Decimal",
plus additional information like a "CommoditySymbol"
and an "AmountStyle" for display.
Together these form hledger's "Amount" type.

In hledger docs, "precision" means the number of digits to the right of the decimal mark.
Amounts have several precisions we can talk about:

- __Journal precision__ - the number of decimal digits recorded in the journal file / input data.
  We accept up to 255 decimal digits there.

- __Internal precision__ - the number of decimal digits stored internally in each Decimal value (after calling normalizeDecimal).
  Decimal supports up to 255 decimal digits.
  In amounts parsed from the journal, this will be the same as their journal precision.
  In amounts generated from calculations, internal precision may increase, and will not decrease.

- __Display precision__ - the number of decimal digits to be displayed in output ("AmountPrecision");
  this is part of the AmountStyle stored within each Amount.
  Immediately after parsing, this is the same as the journal and internal precisions;
  later it gets normalised, for consistent appearance.
  When display precision is less than the internal precision, fewer, rounded decimal digits are displayed ("rounding").
  When display precision is greater than the internal precision, additional decimal zeros are displayed ("padding").

We use the term "display rounding" for applying a target display precision to an existing amount, 
This can be done more or less forcefully, according to a "display rounding strategy" (the "Rounding" type).
That rounding strategy is stored within each AmountStyle for convenience
(though, semantically speaking it is not part of the amount).
The rounding strategies are:

- none - leave the amount's display precision unchanged
- soft - add or remove trailing decimal zeros to approximate the target precision, but don't remove significant digits
- hard - use the exact target precision, possibly rounding and hiding significant digits
- all  - do hard rounding of both the main amount and its cost amount (costs are normally not display-rounded).

Here is when amount styling and display rounding happens:

1. After reading a journal,
  the standard commodity styles are applied to all amounts, except for precision (no display rounding at this stage).
  (still needed ? seems so)

2. After reading a journal, when checking each transaction for balancedness,
  the transaction amounts are hard-rounded, temporarily, before calculating their sum.
  (We'd like to update this to use transaction-local precisions only.)

3. Just before rendering a report (since 1.31).
  Most reports do hard rounding; @print@ (and maybe other print-like commands ?)
  can do any of the rounding strategies.

-}
