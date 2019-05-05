{-|

A 'MarketPrice' represents a historical exchange rate between two
commodities. (Ledger calls them historical prices.) For example, prices
published by a stock exchange or the foreign exchange market.  Some
commands (balance, currently) can use this information to show the market
value of things at a given date.

-}

{-# LANGUAGE RecordWildCards #-}

module Hledger.Data.MarketPrice (
   showMarketPrice
  ,postingValueAtDate
)
where

import Data.List
import qualified Data.Text as T
import Data.Time.Calendar

import Hledger.Data.Amount
import Hledger.Data.Dates
import Hledger.Data.Types

-- | Get the string representation of an market price, based on its
-- commodity's display settings.
showMarketPrice :: MarketPrice -> String
showMarketPrice mp = unwords
    [ "P"
    , showDate (mpdate mp)
    , T.unpack (mpcommodity mp)
    , (showAmount . setAmountPrecision maxprecision) (mpamount mp)
    ]

-- | Convert this posting's amount to its value on the given date in
-- its default valuation commodity, using market prices from the given journal.
postingValueAtDate :: Journal -> Day -> Posting -> Posting
postingValueAtDate j d p@Posting{..} = p{pamount=mixedAmountValue prices d pamount}
  where
    -- prices are in parse order - sort into date then parse order,
    -- & reversed for quick lookup of the latest price.
    prices = reverse $ sortOn mpdate $ jmarketprices j
