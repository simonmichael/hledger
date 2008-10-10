{-|

A 'Currency' is a symbol and a conversion rate relative to the
dollar. Currency symbols are parsed from the ledger file, rates are
currently hard-coded.

-}
module Ledger.Currency
where
import qualified Data.Map as Map
import Ledger.Utils
import Ledger.Types


currencies = 
    [
     Currency "$"   1        
    ,Currency "EUR" 0.760383 
    ,Currency "£"   0.512527 
    ,Currency "h"   60         -- hours
    ,Currency "m"   1          -- minutes
    ]

currencymap = Map.fromList [(sym, c) | c@(Currency sym rate) <- currencies]

getcurrency :: String -> Currency
getcurrency s = Map.findWithDefault (Currency s 1) s currencymap

conversionRate :: Currency -> Currency -> Double
conversionRate oldc newc = (rate newc) / (rate oldc)

-- | convenient amount constructors
dollars n = Amount (getcurrency "$") n 2
euro    n = Amount (getcurrency "EUR") n 2
pounds  n = Amount (getcurrency "£") n 2
hours   n = Amount (getcurrency "h") n 2
minutes n = Amount (getcurrency "m") n 2

