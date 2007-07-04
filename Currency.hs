module Currency
where
import Utils
import Types

currencies = 
    [
     Currency "$"   1        
    ,Currency "EUR" 0.760383 
    ,Currency "£"   0.512527 
    ,Currency "h"   60         -- hours
    ,Currency "m"   1          -- minutes
    ]

getcurrency :: String -> Currency
getcurrency s = head $ [(Currency symbol rate) | (Currency symbol rate) <- currencies, symbol==s]

conversionRate :: Currency -> Currency -> Double
conversionRate oldc newc = (rate newc) / (rate oldc)

-- convenient amount constructors
dollars n = Amount (getcurrency "$") n 2
euro    n = Amount (getcurrency "EUR") n 2
pounds  n = Amount (getcurrency "£") n 2
hours   n = Amount (getcurrency "h") n 2
minutes n = Amount (getcurrency "m") n 2

