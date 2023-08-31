{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Hledger.Cli.Commands.Prices (
  pricesmode
 ,prices
)
where

import qualified Data.Map as M
import Data.List
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Hledger
import Hledger.Cli.CliOptions
import System.Console.CmdArgs.Explicit

pricesmode = hledgerCommandMode
  $(embedFileRelative "Hledger/Cli/Commands/Prices.txt")
  [flagNone ["infer-reverse-prices"] (setboolopt "infer-reverse-prices") "also show prices obtained by inverting transaction prices"
  ]
  [generalflagsgroup1]
  (hiddenflags ++
  [flagNone ["costs"]          (setboolopt "infer-market-prices") "deprecated, use --infer-market-prices instead"
  ,flagNone ["inverted-costs"] (setboolopt "infer-reverse-prices")      "deprecated, use --infer-reverse-prices instead"
  ])
  ([], Just $ argsFlag "[QUERY]")

-- XXX the original hledger-prices script always ignored assertions
prices opts j = do
  let
    styles     = journalCommodityStyles j
    q          = _rsQuery $ reportspec_ opts
    ps         = filter (matchesPosting q) $ allPostings j
    mprices    = jpricedirectives j
    cprices    =
      map (stylePriceDirectiveExceptPrecision styles) $
      concatMap postingPriceDirectivesFromCost ps
    rcprices   =
      map (stylePriceDirectiveExceptPrecision styles) $
      concatMap (postingPriceDirectivesFromCost . postingTransformAmount (mapMixedAmount invertPrice))
      ps
    allprices  =
      mprices
      ++ ifBoolOpt "infer-market-prices" cprices
      ++ ifBoolOpt "infer-reverse-prices" rcprices  -- TODO: shouldn't this show reversed P prices also ? valuation will use them

  mapM_ (T.putStrLn . showPriceDirective) $
    sortOn pddate $
    filter (matchesPriceDirective q) $
    allprices
  where
    ifBoolOpt opt | boolopt opt $ rawopts_ opts = id
                  | otherwise = const []

showPriceDirective :: PriceDirective -> T.Text
showPriceDirective mp = T.unwords ["P", T.pack . show $ pddate mp, quoteCommoditySymbolIfNeeded $ pdcommodity mp, wbToText . showAmountB noColour{displayZeroCommodity=True} $ pdamount mp]

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
            a { aquantity = aquantity pa * nonZeroSignum (aquantity a), acommodity = acommodity pa, aprice = Just $ TotalPrice pa' } where
                pa' = pa { aquantity = abs $ aquantity a, acommodity = acommodity a, aprice = Nothing, astyle = astyle a }
  where
    nonZeroSignum x = if x < 0 then -1 else 1

-- | Given a map of standard amount display styles, apply the
-- appropriate one, if any, to this price directive's amount.
-- But keep the number of decimal places unchanged.
stylePriceDirectiveExceptPrecision :: M.Map CommoditySymbol AmountStyle -> PriceDirective -> PriceDirective
stylePriceDirectiveExceptPrecision styles pd@PriceDirective{pdamount=a} =
  pd{pdamount = amountSetStylesExceptPrecision styles a}

allPostings :: Journal -> [Posting]
allPostings = concatMap tpostings . jtxns
