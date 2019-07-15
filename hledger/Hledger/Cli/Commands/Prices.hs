{-# LANGUAGE TemplateHaskell #-}

module Hledger.Cli.Commands.Prices (
  pricesmode
 ,prices
)
where

import Data.Maybe
import Data.List
import qualified Data.Text as T
import Data.Time
import Hledger
import Hledger.Cli.CliOptions
import System.Console.CmdArgs.Explicit

pricesmode = hledgerCommandMode
  $(embedFileRelative "Hledger/Cli/Commands/Prices.txt")
  [flagNone ["costs"] (setboolopt "costs") "print transaction prices from postings"
  ,flagNone ["inverted-costs"] (setboolopt "inverted-costs") "print transaction inverted prices from postings also"]
  [generalflagsgroup1]
  hiddenflags
  ([], Just $ argsFlag "[QUERY]")

-- XXX the original hledger-prices script always ignored assertions
prices opts j = do
  d <- getCurrentDay
  let
    q          = queryFromOpts d (reportopts_ opts)
    ps         = filter (matchesPosting q) $ allPostings j
    mprices    = jpricedirectives j
    cprices    = concatMap postingCosts ps
    icprices   = concatMap postingCosts . mapAmount invertPrice $ ps
    allprices  = mprices ++ ifBoolOpt "costs" cprices ++ ifBoolOpt "inverted-costs" icprices
  mapM_ (putStrLn . showPriceDirective) $
    sortOn pddate $
    filter (matchesPriceDirective q) $
    allprices
  where
    ifBoolOpt opt | boolopt opt $ rawopts_ opts = id
                  | otherwise = const []

showPriceDirective :: PriceDirective -> String
showPriceDirective mp = unwords ["P", show $ pddate mp, T.unpack . quoteCommoditySymbolIfNeeded $ pdcommodity mp, showAmountWithZeroCommodity $ pdamount mp]

divideAmount' :: Quantity -> Amount -> Amount
divideAmount' n a = a' where
    a' = (n `divideAmount` a) { astyle = style' }
    style' = (astyle a) { asprecision = precision' }
    extPrecision = (1+) . floor . logBase 10 $ (realToFrac n :: Double)
    precision' = extPrecision + asprecision (astyle a)

-- XXX

-- | Invert an amount's price for --invert-cost, somehow ? Unclear.
invertPrice :: Amount -> Amount
invertPrice a =
    case aprice a of
        Nothing -> a
        Just (UnitPrice pa) -> invertPrice
            -- normalize to TotalPrice
            a { aprice = Just $ TotalPrice pa' } where
                pa' = ((1 / aquantity a) `divideAmount` pa) { aprice = Nothing }
        Just (TotalPrice pa) ->
            a { aquantity = aquantity pa * signum (aquantity a), acommodity = acommodity pa, aprice = Just $ TotalPrice pa' } where
                pa' = pa { aquantity = abs $ aquantity a, acommodity = acommodity a, aprice = Nothing, astyle = astyle a }

amountCost :: Day -> Amount -> Maybe PriceDirective
amountCost d a =
    case aprice a of
        Nothing -> Nothing
        Just (UnitPrice pa) -> Just
            PriceDirective { pddate = d, pdcommodity = acommodity a, pdamount = pa }
        Just (TotalPrice pa) -> Just
            PriceDirective { pddate = d, pdcommodity = acommodity a, pdamount = abs (aquantity a) `divideAmount'` pa }

postingCosts :: Posting -> [PriceDirective]
postingCosts p = mapMaybe (amountCost date) . amounts $ pamount p  where
   date = fromMaybe (tdate . fromJust $ ptransaction p) $ pdate p

allPostings :: Journal -> [Posting]
allPostings = concatMap tpostings . jtxns

mapAmount :: (Amount -> Amount) -> [Posting] -> [Posting]
mapAmount f = map pf where
    pf p = p { pamount = mf (pamount p) }
    mf = mixed . map f . amounts
