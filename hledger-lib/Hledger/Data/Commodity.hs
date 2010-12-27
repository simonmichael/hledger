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
import qualified Data.Map as Map
import Data.Map ((!))


nonsimplecommoditychars = "0123456789-.@;\n \""

quoteCommoditySymbolIfNeeded s | any (`elem` nonsimplecommoditychars) s = "\"" ++ s ++ "\""
                               | otherwise = s

-- convenient amount and commodity constructors, for tests etc.

unknown = Commodity {symbol="", side=L,spaced=False,comma=False,precision=0}
dollar  = Commodity {symbol="$",side=L,spaced=False,comma=False,precision=2}
euro    = Commodity {symbol="€",side=L,spaced=False,comma=False,precision=2}
pound   = Commodity {symbol="£",side=L,spaced=False,comma=False,precision=2}
hour    = Commodity {symbol="h",side=R,spaced=False,comma=False,precision=1}

dollars n = Amount dollar n Nothing
euros n   = Amount euro n Nothing
pounds n  = Amount pound n Nothing
hours n   = Amount hour n Nothing

defaultcommodities = [dollar,  euro,  pound, hour, unknown]

-- | Look up one of the hard-coded default commodities. For use in tests.
comm :: String -> Commodity
comm sym = fromMaybe 
              (error' "commodity lookup failed") 
              $ find (\(Commodity{symbol=s}) -> s==sym) defaultcommodities

-- | Find the conversion rate between two commodities. Currently returns 1.
conversionRate :: Commodity -> Commodity -> Double
conversionRate _ _ = 1

-- | Convert a list of commodities to a map from commodity symbols to
-- unique, display-preference-canonicalised commodities.
canonicaliseCommodities :: [Commodity] -> Map.Map String Commodity
canonicaliseCommodities cs =
    Map.fromList [(s,firstc{precision=maxp}) | s <- symbols,
                  let cs = commoditymap ! s,
                  let firstc = head cs,
                  let maxp = maximum $ map precision cs
                 ]
  where
    commoditymap = Map.fromList [(s, commoditieswithsymbol s) | s <- symbols]
    commoditieswithsymbol s = filter ((s==) . symbol) cs
    symbols = nub $ map symbol cs

tests_Hledger_Data_Commodity = TestList [
 ]

