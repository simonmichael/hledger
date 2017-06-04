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
import Data.List
import Data.String.Here
import Data.Time
import qualified Data.Text as T
import Hledger.Cli

------------------------------------------------------------------------------
cmdmode = hledgerCommandMode
  [here| prices
Print all market prices from the journal.
  |]
  [flagNone ["costs"] (setboolopt "costs") "print transaction prices from postings"
  ,flagNone ["inverted-costs"] (setboolopt "inverted-costs") "print transaction inverted prices from postings also"]
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

invertPrice :: Amount -> Amount
invertPrice a =
    case aprice a of
        NoPrice -> a
        UnitPrice pa -> invertPrice
            -- normalize to TotalPrice
            a { aprice = TotalPrice pa' } where
                pa' = (pa `divideAmount` (1 / aquantity a)) { aprice = NoPrice }
        TotalPrice pa ->
            a { aquantity = aquantity pa * signum (aquantity a), acommodity = acommodity pa, aprice = TotalPrice pa' } where
                pa' = pa { aquantity = abs $ aquantity a, acommodity = acommodity a, aprice = NoPrice, astyle = astyle a }

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

allPostings :: Journal -> [Posting]
allPostings = concatMap tpostings . jtxns

mapAmount :: (Amount -> Amount) -> [Posting] -> [Posting]
mapAmount f = map pf where
    pf p = p { pamount = mf (pamount p) }
    mf = mixed . map f . amounts

main :: IO ()
main = do
    opts <- getHledgerCliOpts cmdmode
    withJournalDo opts{ ignore_assertions_ = True } $ \_ j -> do
        let cprices = concatMap postingCosts . allPostings $ j
            icprices = concatMap postingCosts . mapAmount invertPrice . allPostings $ j
            printPrices = mapM_ (putStrLn . showPrice)
            forBoolOpt opt | boolopt opt $ rawopts_ opts = id
                           | otherwise = const []
            allPrices = sortOn mpdate . concat $
                [ jmarketprices j
                , forBoolOpt "costs" cprices
                , forBoolOpt "inverted-costs" icprices
                ]

        printPrices allPrices
