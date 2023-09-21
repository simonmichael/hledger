{-|

This is the root of the @hledger-lib@ package and the @Hledger.*@ module hierarchy.
hledger-lib is the core engine used by various hledger UIs and tools,
providing the main data types, file format parsers, reporting logic, and utilities.

This is also the starting point for hledger's haddock docs,
which describe Hledger's implementation for developers.
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

-}

module Hledger (
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
