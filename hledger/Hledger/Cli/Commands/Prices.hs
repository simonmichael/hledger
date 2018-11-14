{-# LANGUAGE QuasiQuotes #-}

module Hledger.Cli.Commands.Prices (
  pricesmode
 ,prices
) 
where

import Data.Maybe
import Data.List
import Data.String.Here
import qualified Data.Text as T
import Data.Time
import Hledger
import Hledger.Cli.CliOptions
import System.Console.CmdArgs.Explicit

pricesmode = hledgerCommandMode
  [here| prices
Print market price directives from the journal.
With --costs, also print synthetic market prices based on transaction prices.
With --inverted-costs, also print inverse prices based on transaction prices.
Prices (and postings providing prices) can be filtered by a query.
  |]
  [flagNone ["costs"] (setboolopt "costs") "print transaction prices from postings"
  ,flagNone ["inverted-costs"] (setboolopt "inverted-costs") "print transaction inverted prices from postings also"]
  [generalflagsgroup1]
  []
  ([], Just $ argsFlag "[QUERY]")

-- XXX the original hledger-prices script always ignored assertions 
prices opts j = do
  d <- getCurrentDay
  let
    q          = queryFromOpts d (reportopts_ opts)
    ps         = filter (matchesPosting q) $ allPostings j
    mprices    = jmarketprices j
    cprices    = concatMap postingCosts ps
    icprices   = concatMap postingCosts . mapAmount invertPrice $ ps
    allprices  = mprices ++ ifBoolOpt "costs" cprices ++ ifBoolOpt "inverted-costs" icprices
  mapM_ (putStrLn . showPrice) $
    sortOn mpdate $
    filter (matchesMarketPrice q) $
    allprices
  where
    ifBoolOpt opt | boolopt opt $ rawopts_ opts = id
                  | otherwise = const []

showPrice :: MarketPrice -> String
showPrice mp = unwords ["P", show $ mpdate mp, T.unpack . quoteCommoditySymbolIfNeeded $ mpcommodity mp, showAmountWithZeroCommodity $ mpamount mp]

divideAmount' :: Quantity -> Amount -> Amount
divideAmount' n a = a' where
    a' = (n `divideAmount` a) { astyle = style' }
    style' = (astyle a) { asprecision = precision' }
    extPrecision = (1+) . floor . logBase 10 $ (realToFrac n :: Double)
    precision' = extPrecision + asprecision (astyle a)

invertPrice :: Amount -> Amount
invertPrice a =
    case aprice a of
        NoPrice -> a
        UnitPrice pa -> invertPrice
            -- normalize to TotalPrice
            a { aprice = TotalPrice pa' } where
                pa' = ((1 / aquantity a) `divideAmount` pa) { aprice = NoPrice }
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
            MarketPrice { mpdate = d, mpcommodity = acommodity a, mpamount = abs (aquantity a) `divideAmount'` pa }

postingCosts :: Posting -> [MarketPrice]
postingCosts p = mapMaybe (amountCost date) . amounts $ pamount p  where
   date = fromMaybe (tdate . fromJust $ ptransaction p) $ pdate p

allPostings :: Journal -> [Posting]
allPostings = concatMap tpostings . jtxns

mapAmount :: (Amount -> Amount) -> [Posting] -> [Posting]
mapAmount f = map pf where
    pf p = p { pamount = mf (pamount p) }
    mf = mixed . map f . amounts
