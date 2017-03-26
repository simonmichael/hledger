#!/usr/bin/env stack
{- stack runghc --verbosity info
   --package hledger
   --package here
   --package text
   --package time
-}

{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# LANGUAGE QuasiQuotes #-}

import Data.Maybe
import Data.String.Here
import Data.Time
import qualified Data.Text as T
import Control.Monad (when)
import Hledger.Cli

------------------------------------------------------------------------------
cmdmode = hledgerCommandMode
  [here| prices
Print all prices from the journal.
  |]
  [flagNone ["costs"] (setboolopt "costs") "print transaction prices from postings instead of market prices"]
  [generalflagsgroup1]
  []
  ([], Nothing)
------------------------------------------------------------------------------

showPrice :: MarketPrice -> String
showPrice mp = unwords ["P", show $ mpdate mp, T.unpack . quoteCommoditySymbolIfNeeded $ mpcommodity mp, showAmountWithZeroCommodity $ mpamount mp]

divideAmount' :: Amount -> Quantity -> Amount
divideAmount' a d = a' where
    a' = (a `divideAmount` d) { astyle = style' }
    style' = (astyle a) { asprecision = precision' }
    extPrecision = (1+) . floor . logBase 10 $ (realToFrac d :: Double)
    precision' = extPrecision + asprecision (astyle a)

amountCost :: Day -> Amount -> Maybe MarketPrice
amountCost d a =
    case aprice a of
        NoPrice -> Nothing
        UnitPrice pa -> Just
            MarketPrice { mpdate = d, mpcommodity = acommodity a, mpamount = pa }
        TotalPrice pa -> Just
            MarketPrice { mpdate = d, mpcommodity = acommodity a, mpamount = pa `divideAmount'` abs (aquantity a) }

postingCosts :: Posting -> [MarketPrice]
postingCosts p = mapMaybe (amountCost date) . amounts $ pamount p  where
   date = fromMaybe (tdate . fromJust $ ptransaction p) $ pdate p

allPostsings :: Journal -> [Posting]
allPostsings = concatMap tpostings . jtxns

main :: IO ()
main = do
    opts <- getHledgerCliOpts cmdmode
    withJournalDo opts{ ignore_assertions_ = True } $ \_ j -> do
        let cprices = concatMap postingCosts . allPostsings $ j
            printPrices = mapM_ (putStrLn . showPrice)
        when (boolopt "costs" $ rawopts_ opts) $ printPrices cprices
        printPrices $ jmarketprices j
