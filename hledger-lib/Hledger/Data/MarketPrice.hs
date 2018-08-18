{-|

A 'MarketPrice' represents a historical exchange rate between two
commodities. (Ledger calls them historical prices.) For example, prices
published by a stock exchange or the foreign exchange market.  Some
commands (balance, currently) can use this information to show the market
value of things at a given date.

-}

{-# LANGUAGE OverloadedStrings, LambdaCase #-}

module Hledger.Data.MarketPrice
where
import qualified Data.Text as T

import Hledger.Data.Amount
import Hledger.Data.Dates
import Hledger.Data.Types
import Hledger.Utils.Test

-- | Get the string representation of an market price, based on its
-- commodity's display settings.
showMarketPrice :: MarketPrice -> String
showMarketPrice mp = unwords
    [ "P"
    , showDate (mpdate mp)
    , T.unpack (mpcommodity mp)
    , (showAmount . setAmountPrecision maxprecision) (mpamount mp)
    ]

tests_Hledger_Data_MarketPrice = TestList []
