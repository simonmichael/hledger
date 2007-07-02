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
dollars = Amount $ getcurrency "$"
euro    = Amount $ getcurrency "EUR"
pounds  = Amount $ getcurrency "£"
hours   = Amount $ getcurrency "h"
minutes = Amount $ getcurrency "m"

