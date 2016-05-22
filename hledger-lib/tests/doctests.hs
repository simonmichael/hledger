import Test.DocTest

main = do
  let dist = "dist"
  doctest [
    "-optP-include"
    ,"-optP" ++ dist ++ "/build/autogen/cabal_macros.h"
    ,"Hledger.hs"
    ]
