{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE NamedFieldPuns #-}

module Hledger.Cli.Commands.Prices (
  pricesmode
 ,prices
)
where

import Data.List
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Hledger
import Hledger.Cli.CliOptions
import System.Console.CmdArgs.Explicit
import Data.Maybe (mapMaybe)
import Data.Function ((&))

pricesmode = hledgerCommandMode
  $(embedFileRelative "Hledger/Cli/Commands/Prices.txt")
  [flagNone ["show-reverse"] (setboolopt "show-reverse")
    "also show the prices inferred by reversing known prices"
  ]
  cligeneralflagsgroups1
  (hiddenflags ++
  [flagNone ["costs"]                (setboolopt "infer-market-prices") "deprecated, use --infer-market-prices instead"
  ,flagNone ["inverted-costs"]       (setboolopt "show-reverse") "deprecated, use --show-reverse instead"
  ,flagNone ["infer-reverse-prices"] (setboolopt "show-reverse") "deprecated, use --show-reverse instead"
  ])
  ([], Just $ argsFlag "[QUERY]")

instance HasAmounts PriceDirective where
  styleAmounts styles pd = pd{pdamount=styleAmounts styles $ pdamount pd}

-- List market prices.
prices opts j = do
  let
    styles = journalCommodityStyles j
    q      = _rsQuery $ reportspec_ opts

    -- XXX duplicates logic in Hledger.Data.Valuation.makePriceGraph, keep synced

    declaredprices =
      -- dbg0 "declaredprices" $
      jpricedirectives j

    pricesfromcosts =
      -- dbg0 "pricesfromcosts" $
      concatMap postingPriceDirectivesFromCost $
      journalPostings j

    forwardprices =
      -- dbg0 "forwardprices" $
      if boolopt "infer-market-prices" (rawopts_ opts)
      then declaredprices `mergePriceDirectives` pricesfromcosts
      else declaredprices

    reverseprices =
      -- dbg0 "reverseprices" $
      mapMaybe reversePriceDirective forwardprices

    allprices =
      -- dbg0 "allprices" $
      if boolopt "show-reverse" (rawopts_ opts)
      then forwardprices `mergePriceDirectives` reverseprices
      else forwardprices

    filteredprices =
      -- dbg0 "filtered unsorted" $
      filter (matchesPriceDirective q) allprices

  mapM_ (T.putStrLn . showPriceDirective . styleAmounts styles) $
    sortOn pddate filteredprices

-- XXX performance
-- | Append any new price directives (with different from commodity,
-- to commodity, or date) from the second list to the first.
-- (Does not remove redundant prices from the first; just avoids adding more.)
mergePriceDirectives :: [PriceDirective] -> [PriceDirective] -> [PriceDirective]
mergePriceDirectives pds1 pds2 =
  pds1 ++ [ pd | pd <- pds2 , pdid pd `notElem` pds1ids ]
  where
    pds1ids = map pdid pds1
    pdid PriceDirective{pddate,pdcommodity,pdamount} = (pddate, pdcommodity, acommodity pdamount)

showPriceDirective :: PriceDirective -> T.Text
showPriceDirective mp = T.unwords [
  "P",
  T.pack . show $ pddate mp,
  quoteCommoditySymbolIfNeeded $ pdcommodity mp,
  wbToText . showAmountB defaultFmt{displayZeroCommodity=True} $ pdamount mp
  ]

-- | Convert a market price directive to a corresponding one in the
-- opposite direction, if possible. (A price directive with a zero
-- price can't be reversed.)
--
-- The price's display precision will be set to show all significant
-- decimal digits (or if they appear infinite, a smaller default precision (8).
-- This is visible eg in the prices command's output.
--
reversePriceDirective :: PriceDirective -> Maybe PriceDirective
reversePriceDirective pd@PriceDirective{pdcommodity=c, pdamount=a}
  | amountIsZero a = Nothing
  | otherwise      = Just pd{pdcommodity=acommodity a, pdamount=a'}
    where
      lbl = lbl_ "reversePriceDirective"
      a' =
        amountSetFullPrecisionUpTo (Just defaultMaxPrecision) $
        invertAmount a{acommodity=c}
        & dbg9With (lbl "calculated reverse price".showAmount)
        -- & dbg9With (lbl "precision of reverse price".show.amountDisplayPrecision)
