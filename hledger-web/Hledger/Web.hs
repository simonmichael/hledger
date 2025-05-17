{-|

This is the root module of the @hledger-web@ package,
providing hledger's web user interface and JSON API.
The main function and command-line options are exported.

== See also:

- hledger-lib:Hledger
- hledger:Hledger.Cli
- [The README files](https://github.com/search?q=repo%3Asimonmichael%2Fhledger+path%3A**%2FREADME*&type=code&ref=advsearch)
- [The high-level developer docs](https://hledger.org/dev.html)

-}

module Hledger.Web (
  module Hledger.Web.Main,
  module Hledger.Web.WebOptions
) where

import Hledger.Web.WebOptions
import Hledger.Web.Main
