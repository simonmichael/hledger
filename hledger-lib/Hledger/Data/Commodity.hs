{-|

A 'Commodity' is a symbol representing a currency or some other kind of
thing we are tracking, and some display preferences that tell how to
display 'Amount's of the commodity - is the symbol on the left or right,
are thousands separated by comma, significant decimal places and so on.

-}
module Hledger.Data.Commodity
where
import Hledger.Data.Utils
import Hledger.Data.Types


-- convenient amount and commodity constructors, for tests etc.

unknown = Commodity {symbol="",   side=L,spaced=False,comma=False,precision=0}
dollar  = Commodity {symbol="$",  side=L,spaced=False,comma=False,precision=2}
euro    = Commodity {symbol="EUR",side=L,spaced=False,comma=False,precision=2}
pound   = Commodity {symbol="£",  side=L,spaced=False,comma=False,precision=2}
hour    = Commodity {symbol="h",  side=R,spaced=False,comma=False,precision=1}

dollars n = Amount dollar n Nothing
euros n   = Amount euro n Nothing
pounds n  = Amount pound n Nothing
hours n   = Amount hour n Nothing

defaultcommodities = [dollar,  euro,  pound, hour, unknown]

-- | Look up one of the hard-coded default commodities. For use in tests.
comm :: String -> Commodity
comm sym = fromMaybe 
              (error "commodity lookup failed") 
              $ find (\(Commodity{symbol=s}) -> s==sym) defaultcommodities

-- | Find the conversion rate between two commodities. Currently returns 1.
conversionRate :: Commodity -> Commodity -> Double
conversionRate _ _ = 1

