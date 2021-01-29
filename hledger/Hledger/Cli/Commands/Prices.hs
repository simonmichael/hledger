{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Hledger.Cli.Commands.Prices (
  pricesmode
 ,prices
)
where

import qualified Data.Map as M
import Data.Maybe
import Data.List
import qualified Data.Text as T
import qualified Data.Text.IO as T
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
  let
    styles     = journalCommodityStyles j
    q          = rsQuery $ reportspec_ opts
    ps         = filter (matchesPosting q) $ allPostings j
    mprices    = jpricedirectives j
    cprices    = map (stylePriceDirectiveExceptPrecision styles) $ concatMap postingsPriceDirectivesFromCosts ps
    icprices   = map (stylePriceDirectiveExceptPrecision styles) $ concatMap postingsPriceDirectivesFromCosts $ map (postingTransformAmount $ mapMixedAmount invertPrice) ps
    allprices  = mprices ++ ifBoolOpt "costs" cprices ++ ifBoolOpt "inverted-costs" icprices
  mapM_ (T.putStrLn . showPriceDirective) $
    sortOn pddate $
    filter (matchesPriceDirective q) $
    allprices
  where
    ifBoolOpt opt | boolopt opt $ rawopts_ opts = id
                  | otherwise = const []

showPriceDirective :: PriceDirective -> T.Text
showPriceDirective mp = T.unwords ["P", T.pack . show $ pddate mp, quoteCommoditySymbolIfNeeded $ pdcommodity mp, wbToText . showAmountB noColour{displayZeroCommodity=True} $ pdamount mp]

divideAmount' :: Quantity -> Amount -> Amount
divideAmount' n a = a' where
    a' = (n `divideAmount` a) { astyle = style' }
    style' = (astyle a) { asprecision = precision' }
    extPrecision = (1+) . floor . logBase 10 $ (realToFrac n :: Double)
    precision' = case asprecision (astyle a) of
                      NaturalPrecision -> NaturalPrecision
                      Precision p      -> Precision $ extPrecision + p

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

postingsPriceDirectivesFromCosts :: Posting -> [PriceDirective]
postingsPriceDirectivesFromCosts p = mapMaybe (amountPriceDirectiveFromCost date) . amountsRaw $ pamount p
  where date = fromMaybe (tdate . fromJust $ ptransaction p) $ pdate p

amountPriceDirectiveFromCost :: Day -> Amount -> Maybe PriceDirective
amountPriceDirectiveFromCost d a =
    case aprice a of
        Nothing -> Nothing
        Just (UnitPrice pa) -> Just
            PriceDirective { pddate = d, pdcommodity = acommodity a, pdamount = pa }
        Just (TotalPrice pa) -> Just
            PriceDirective { pddate = d, pdcommodity = acommodity a, pdamount = abs (aquantity a) `divideAmount'` pa }

-- | Given a map of standard amount display styles, apply the
-- appropriate one, if any, to this price directive's amount.
-- But keep the number of decimal places unchanged.
stylePriceDirectiveExceptPrecision :: M.Map CommoditySymbol AmountStyle -> PriceDirective -> PriceDirective
stylePriceDirectiveExceptPrecision styles pd@PriceDirective{pdamount=a} =
  pd{pdamount = styleAmountExceptPrecision styles a}

allPostings :: Journal -> [Posting]
allPostings = concatMap tpostings . jtxns
