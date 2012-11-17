{-|

A 'Commodity' is a symbol representing a currency or some other kind of
thing we are tracking, and some display preferences that tell how to
display 'Amount's of the commodity - is the symbol on the left or right,
are thousands separated by comma, significant decimal places and so on.

-}
module Hledger.Data.Commodity
where
import Data.List
import Data.Map ((!))
import Data.Maybe
import Test.HUnit
import qualified Data.Map as Map

import Hledger.Data.Types
import Hledger.Utils


-- characters than can't be in a non-quoted commodity symbol
nonsimplecommoditychars = "0123456789-.@;\n \"{}" :: String

quoteCommoditySymbolIfNeeded s | any (`elem` nonsimplecommoditychars) s = "\"" ++ s ++ "\""
                               | otherwise = s

-- convenient amount and commodity constructors, for tests etc.

unknown = Commodity {symbol="", side=L,spaced=False,decimalpoint='.',precision=0,separator=',',separatorpositions=[]}
dollar  = Commodity {symbol="$",side=L,spaced=False,decimalpoint='.',precision=2,separator=',',separatorpositions=[]}
euro    = Commodity {symbol="€",side=L,spaced=False,decimalpoint='.',precision=2,separator=',',separatorpositions=[]}
pound   = Commodity {symbol="£",side=L,spaced=False,decimalpoint='.',precision=2,separator=',',separatorpositions=[]}
hour    = Commodity {symbol="h",side=R,spaced=False,decimalpoint='.',precision=1,separator=',',separatorpositions=[]}

dollars n = Amount dollar n Nothing
euros n   = Amount euro   n Nothing
pounds n  = Amount pound  n Nothing
hours n   = Amount hour   n Nothing

defaultcommodities = [dollar, euro, pound, hour, unknown]

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

