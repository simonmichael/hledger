{-|

A 'MarketPrice' represents a historical exchange rate between two
commodities. (Ledger calls them historical prices.) For example, prices
published by a stock exchange or the foreign exchange market.  Some
commands (balance, currently) can use this information to show the market
value of things at a given date.

thing we are tracking, and some display preferences that tell how to
display 'Amount's of the commodity - is the symbol on the left or right,
are thousands separated by comma, significant decimal places and so on.

-}

{-# LANGUAGE LambdaCase #-}

module Hledger.Data.MarketPrice
where
import Data.Time.Format
import qualified Data.Text as T
-- import Test.HUnit

import Hledger.Data.Amount
import Hledger.Data.Types
import Hledger.Utils

showMarketPrice :: MarketPrice -> String
showMarketPrice mp = unwords
    [ "P"
    , formatTime defaultTimeLocale "%0Y/%m/%d" (mpdate mp)
    , T.unpack (mpcommodity mp)
    , (showAmount . setAmountPrecision maxprecision) (mpamount mp)
    ]
