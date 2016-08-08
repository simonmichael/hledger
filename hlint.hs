{-
hlint configuration for hledger

https://github.com/ndmitchell/hlint#readme
https://github.com/ndmitchell/hlint/issues/256

Examples:
ignore "Eta reduce"   = ""                - suppress all eta reduction suggestions.
ignore "Eta reduce"   = Data.List Prelude - suppress eta reduction hints in the Prelude and Data.List modules.
ignore                = Data.List.map     - don't give any hints in the function Data.List.map.
error                 = Data.List.map     - any hint in the function is an error.
error "Use concatMap" = ""                - the hint to use concatMap is an error.
warn "Use concatMap"  = ""                - the hint to use concatMap is a warning.

-}

{-# PackageImports #-}
import "hlint" HLint.Builtin.All
-- import "hlint" HLint.Default

ignore "Use camelCase" = ""

