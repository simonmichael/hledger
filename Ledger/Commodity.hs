{-|

A 'Commodity' is a symbol and a conversion rate relative to the
dollar. Commodity symbols are parsed from the ledger file, rates are
currently hard-coded.

-}
module Ledger.Commodity
where
import qualified Data.Map as Map
import Ledger.Utils
import Ledger.Types


commoditytests = TestList [
                ]

unknown = Commodity {symbol="",rate=1,side=L,spaced=False,precision=0}
dollar  = Commodity {symbol="$",rate=1,side=L,spaced=False,precision=2}
euro    = Commodity {symbol="EUR",rate=0.760383,side=L,spaced=False,precision=2}
pound   = Commodity {symbol="£",rate=0.512527,side=L,spaced=False,precision=2}
hour    = Commodity {symbol="h",rate=100,side=R,spaced=False,precision=1}

-- | convenient amount constructors
unknowns = Amount unknown
dollars  = Amount dollar
euros    = Amount euro
pounds   = Amount pound
hours    = Amount hour

defaultcommodities = [unknown, dollar,  euro,  pound, hour]

defaultcommoditiesmap :: Map.Map String Commodity
defaultcommoditiesmap = Map.fromList [(symbol c :: String, c :: Commodity) | c <- defaultcommodities]

comm :: String -> Commodity
comm symbol = Map.findWithDefault (error "commodity lookup failed") symbol defaultcommoditiesmap

conversionRate :: Commodity -> Commodity -> Double
conversionRate oldc newc = (rate newc) / (rate oldc)

