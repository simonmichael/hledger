{-
hlint configuration for hledger

manual: http://community.haskell.org/~ndm/darcs/hlint/hlint.htm

examples:
ignore "Eta reduce"   = ""                - suppress all eta reduction suggestions.
ignore "Eta reduce"   = Data.List Prelude - suppress eta reduction hints in the Prelude and Data.List modules.
ignore                = Data.List.map     - don't give any hints in the function Data.List.map.
error                 = Data.List.map     - any hint in the function is an error.
error "Use concatMap" = ""                - the hint to use concatMap is an error.
warn "Use concatMap"  = ""                - the hint to use concatMap is a warning.

-}

import HLint.Default
ignore "Use camelCase" = ""
