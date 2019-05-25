{-|

Find historical exchange rates between two commodities.

-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Hledger.Data.Prices (
   Prices
  ,nullPrices
  ,toPrices
  ,priceLookup
  ,tests_Prices
)
where

import Data.List
import qualified Data.Text as T
import Data.Time.Calendar (Day)

import Hledger.Utils
import Hledger.Data.Types

-- | A database of historical market prices for multiple commodites,
-- allowing fast lookup of exchange rates between commodity pairs on a
-- given date.
data Prices = Prices {
  prPrices :: [MarketPrice]  -- ^ For now, just a list of price declarations sorted by date then parse order.
  }

nullPrices = toPrices []

-- | Convert a list of market prices in declaration order to a 'Prices' db.
toPrices :: [MarketPrice] -> Prices
toPrices declaredprices = Prices{prPrices = reverse $ sortOn mpdate declaredprices}

-- | Find the market value of one unit of the given commodity on the
-- given date in its default valuation commodity (the commodity of the
-- latest applicable price before the valuation date).
-- Returns Nothing if there's no applicable price.
priceLookup :: Prices -> Day -> CommoditySymbol -> Maybe Amount
priceLookup Prices{..} valuationdate c =
  case filter (\MarketPrice{..} -> mpcommodity==c && mpdate<=valuationdate) prPrices of
    []  -> dbg Nothing
    ps' -> dbg $ Just $ mpamount $ head ps'
  where
    dbg = dbg8 ("using market price for "++T.unpack c)

tests_Prices = tests "Prices" [
  ]

