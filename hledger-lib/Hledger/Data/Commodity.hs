{-|

A 'Commodity' is a symbol representing a currency or some other kind of
thing we are tracking, and some display preferences that tell how to
display 'Amount's of the commodity - is the symbol on the left or right,
are thousands separated by comma, significant decimal places and so on.

-}

{-# LANGUAGE OverloadedStrings #-}

module Hledger.Data.Commodity
where
import Control.Applicative (liftA2)
import Data.Char (isDigit)
import Data.List
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
-- import qualified Data.Map as M

import Hledger.Data.Types
import Hledger.Utils

-- Show space-containing commodity symbols quoted, as they are in a journal.
showCommoditySymbol = textQuoteIfNeeded

-- characters that may not be used in a non-quoted commodity symbol
isNonsimpleCommodityChar :: Char -> Bool
isNonsimpleCommodityChar = liftA2 (||) isDigit isOther
  where
    otherChars = "-+.@*;\t\n \"{}=" :: T.Text
    isOther c = T.any (==c) otherChars

quoteCommoditySymbolIfNeeded :: T.Text -> T.Text
quoteCommoditySymbolIfNeeded s
  | T.any isNonsimpleCommodityChar s = "\"" <> s <> "\""
  | otherwise = s

commodity = ""

-- handy constructors for tests
-- unknown = commodity
-- usd     = "$"
-- eur     = "€"
-- gbp     = "£"
-- hour    = "h"

-- Some sample commodity' names and symbols, for use in tests..
commoditysymbols =
  [("unknown","")
  ,("usd","$")
  ,("eur","€")
  ,("gbp","£")
  ,("hour","h")
  ]

-- | Look up one of the sample commodities' symbol by name.
comm :: String -> CommoditySymbol
comm name = snd $ fromMaybe
              (error' "commodity lookup failed")  -- PARTIAL:
              (find (\n -> fst n == name) commoditysymbols)

-- | Find the conversion rate between two commodities. Currently returns 1.
conversionRate :: CommoditySymbol -> CommoditySymbol -> Double
conversionRate _ _ = 1

-- -- | Convert a list of commodities to a map from commodity symbols to
-- -- unique, display-preference-canonicalised commodities.
-- canonicaliseCommodities :: [CommoditySymbol] -> Map.Map String CommoditySymbol
-- canonicaliseCommodities cs =
--     Map.fromList [(s,firstc{precision=maxp}) | s <- symbols,
--                   let cs = commoditymap ! s,
--                   let firstc = head cs,
--                   let maxp = maximum $ map precision cs
--                  ]
--   where
--     commoditymap = Map.fromList [(s, commoditieswithsymbol s) | s <- symbols]
--     commoditieswithsymbol s = filter ((s==) . symbol) cs
--     symbols = nub $ map symbol cs

