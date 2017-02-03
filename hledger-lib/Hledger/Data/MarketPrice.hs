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

{-# LANGUAGE OverloadedStrings, LambdaCase #-}

module Hledger.Data.MarketPrice
where
import qualified Data.Text as T
import Test.HUnit

import Hledger.Data.Amount
import Hledger.Data.Dates
import Hledger.Data.Types
-- import Hledger.Read.JournalReader
import Hledger.Utils
import Hledger.Utils.Parse

-- | Get the string representation of an market price, based on its
-- commodity's display settings.
showMarketPrice :: MarketPrice -> String
showMarketPrice mp = unwords
    [ "P"
    , showDate (mpdate mp)
    , T.unpack (mpcommodity mp)
    , (showAmount . setAmountPrecision maxprecision) (mpamount mp)
    ]

tests_Hledger_Data_MarketPrice = TestList $
  [
    -- this test needs Hledger.Read.JournalReader, which causes cyclic imports?
    -- "showParsedMarketPrice" ~: do
    --   let mp = parseWithState mempty marketpricedirectivep "P 2017/01/30 BTC $922.83"
    --       mpString = (fmap . fmap) showMarketPrice mp
    --   mpString `is` (Just (Right "P 2017/01/30 BTC $922.83"))
  ]
