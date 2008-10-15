{-|

A 'Commodity' is a symbol representing a currency or some other kind of
thing we are tracking, and some settings that tell how to display amounts
of the commodity.  For the moment, commodities also include a hard-coded
conversion rate relative to the dollar.

-}
module Ledger.Commodity
where
import qualified Data.Map as Map
import Ledger.Utils
import Ledger.Types


commoditytests = TestList [
                ]

-- for nullamt, autoamt, etc.
unknown = Commodity {symbol="",side=L,spaced=False,comma=False,precision=0,rate=1}

-- convenient amount and commodity constructors, for tests etc.

dollar  = Commodity {symbol="$",side=L,spaced=False,comma=False,precision=2,rate=1}
euro    = Commodity {symbol="EUR",side=L,spaced=False,comma=False,precision=2,rate=0.760383}
pound   = Commodity {symbol="£",side=L,spaced=False,comma=False,precision=2,rate=0.512527}
hour    = Commodity {symbol="h",side=R,spaced=False,comma=False,precision=1,rate=100}

dollars  = Amount dollar
euros    = Amount euro
pounds   = Amount pound
hours    = Amount hour

defaultcommodities = [dollar,  euro,  pound, hour, unknown]

defaultcommoditiesmap :: Map.Map String Commodity
defaultcommoditiesmap = Map.fromList [(symbol c :: String, c :: Commodity) | c <- defaultcommodities]

comm :: String -> Commodity
comm symbol = Map.findWithDefault (error "commodity lookup failed") symbol defaultcommoditiesmap

-- | Find the conversion rate between two commodities.
conversionRate :: Commodity -> Commodity -> Double
conversionRate oldc newc = (rate newc) / (rate oldc)

