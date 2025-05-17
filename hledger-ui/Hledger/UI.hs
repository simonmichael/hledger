{-|

This is the root module of the @hledger-ui@ package,
providing hledger's terminal user interface.
The main function, command-line options, and terminal themes are exported.

== See also:

- hledger-lib:Hledger
- hledger:Hledger.Cli
- [The README files](https://github.com/search?q=repo%3Asimonmichael%2Fhledger+path%3A**%2FREADME*&type=code&ref=advsearch)
- [The high-level developer docs](https://hledger.org/dev.html)

-}

module Hledger.UI (
  module Hledger.UI.Main,
  module Hledger.UI.Theme,
  module Hledger.UI.UIOptions,
)
where

import Hledger.UI.Main
import Hledger.UI.UIOptions
import Hledger.UI.Theme
