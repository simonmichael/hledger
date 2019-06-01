{-|

Find historical market prices (exchange rates) between commodities,
convert amounts to value in various ways.

-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}

module Hledger.Data.Prices (
   Prices
  ,nullPrices
  ,toPrices
  ,priceLookup
  ,amountValueAtDate
  ,amountApplyValuation
  ,mixedAmountValueAtDate
  ,mixedAmountApplyValuation
  ,tests_Prices
)
where

import Control.Applicative ((<|>))
import Data.List
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Time.Calendar (Day)
import Safe (headMay)

import Hledger.Utils
import Hledger.Data.Types
import Hledger.Data.Amount
import Hledger.Data.Dates (parsedate)

-- | A database of historical market prices for multiple commodites,
-- allowing fast lookup of exchange rates between commodity pairs on a
-- given date.
data Prices = Prices {
  prPrices :: [MarketPrice]  -- ^ For now, just a list of price declarations,
                             --   sorted by date then parse order, then reversed.
  }

nullPrices = toPrices []

-- | Convert a list of market prices in declaration order to a 'Prices' db.
toPrices :: [MarketPrice] -> Prices
toPrices declaredprices = Prices{prPrices = reverse $ sortOn mpdate declaredprices}

-- | Reverse a market price from A to B, so that it becomes an equivalent price from B to A.
marketPriceInvert :: MarketPrice -> MarketPrice
marketPriceInvert p@MarketPrice{mpcommodity, mpamount} =  
  p{ mpcommodity = acommodity mpamount
   , mpamount    = setMinimalPrecision mpamount{acommodity=mpcommodity, aquantity=1 / aquantity mpamount}
   }

tests_marketPriceInvert = tests "marketPriceInvert" [
  marketPriceInvert (MarketPrice{mpdate=d "2019-06-01", mpcommodity="A", mpamount=amt "B" 2})
    `is`            (MarketPrice{mpdate=d "2019-06-01", mpcommodity="B", mpamount=amt "A" 0.5 `withPrecision` 1})
  ]

d = parsedate
amt c q = nullamt{acommodity=c, aquantity=q}

-- | Using the market prices in effect at the given date, find the
-- market value of one unit of a given commodity, in a different
-- specified valuation commodity, defaulting to the commodity of the
-- most recent applicable price.
--
-- When the valuation commodity is specified, this looks for, in order:
--
-- - a direct price, giving the exchange rate from source commodity to
--   valuation commodity.
--
-- - a reverse direct price, giving the exchange rate from valuation
--   commodity to source commodity, which is inverted.
--
-- - (TODO: the shortest chain of prices leading from source commodity
--   to valuation commodity, which is collapsed into a single
--   synthetic exchange rate.)
--
-- When the valuation commodity is not specified, this looks for the
-- latest applicable market price, and converts to the commodity
-- mentioned in that price. Note when valuing amounts over multiple
-- periods, this default valuation commodity may vary, since it
-- depends on the presence and parse order of market price
-- declarations in each period.
-- 
-- If no applicable market price or chain of prices can be found, or
-- if the source commodity and the valuation commodity are the same,
-- this returns Nothing.
--
priceLookup :: Prices -> Day -> Maybe CommoditySymbol -> CommoditySymbol -> Maybe Amount
priceLookup Prices{prPrices} d mto from
  | mto == Just from = Nothing
  | otherwise        = mdirectprice <|> mreverseprice
  where
    dbgprice lbl =
      dbg4With ( ((lbl++" for "++T.unpack from++" at "++show d++": ") ++)
                 . maybe "none" showAmount )

    -- Key to commodity symbols:
    -- from  - commodity we are converting from (looking up a price for)
    -- mto   - commodity we want to convert to, or Nothing meaning use default
    -- pfrom - commodity that this market price converts from
    -- pto   - commodity that this market price converts to

    -- prPrices is sorted by date then parse order, reversed. So the
    -- first price on or before the valuation date is the effective one.

    mdirectprice =
      dbgprice "direct  market price" $
      headMay [mpamount | MarketPrice{mpdate, mpcommodity=pfrom, mpamount} <- prPrices
                        , let pto = acommodity mpamount
                        , mpdate <= d
                        , pfrom == from
                        , maybe True (== pto) mto
                        ]
    mreverseprice =
      dbgprice "reverse market price" $
      headMay [ priceamt
              | mp@MarketPrice{mpdate, mpcommodity=pfrom, mpamount} <- prPrices
              , let pto = acommodity mpamount
              , mpdate <= d
              , pto == from
              , maybe False (== pfrom) mto  -- use reverse prices only when target commodity is explicitly specified
              , let MarketPrice{mpamount=priceamt} = marketPriceInvert mp
              ]

tests_priceLookup = tests "priceLookup" [
   priceLookup (Prices []) (d "2019-06-01") Nothing "" `is` Nothing
  ]
   
-- Apply a specified valuation to this mixed amount, using the provided
-- prices db, commodity styles, period-end/current dates, 
-- and whether this is for a multiperiod report or not.
-- Currently ignores the specified valuation commodity and always uses
-- the default valuation commodity.
mixedAmountApplyValuation :: Prices -> M.Map CommoditySymbol AmountStyle -> Day -> Day -> Bool -> ValuationType -> MixedAmount -> MixedAmount
mixedAmountApplyValuation prices styles periodend today ismultiperiod v (Mixed as) =
  Mixed $ map (amountApplyValuation prices styles periodend today ismultiperiod v) as

-- | Find the market value of each component amount in the given
-- commodity, or its default valuation commodity, at the given
-- valuation date, using the given market prices.
-- When market prices available on that date are not sufficient to
-- calculate the value, amounts are left unchanged.
mixedAmountValueAtDate :: Prices -> Maybe CommoditySymbol -> Day -> MixedAmount -> MixedAmount
mixedAmountValueAtDate prices mc d (Mixed as) = Mixed $ map (amountValueAtDate prices mc d) as

-- | Apply a specified valuation to this amount, using the provided
-- prices db, commodity styles, period-end/current dates, 
-- and whether this is for a multiperiod report or not.
amountApplyValuation :: Prices -> M.Map CommoditySymbol AmountStyle -> Day -> Day -> Bool -> ValuationType -> Amount -> Amount
amountApplyValuation prices styles periodend today ismultiperiod v a =
  case v of
    AtCost    Nothing            -> amountToCost styles a
    AtCost    mc                 -> amountValueAtDate prices mc periodend $ amountToCost styles a
    AtEnd     mc                 -> amountValueAtDate prices mc periodend a
    AtNow     mc                 -> amountValueAtDate prices mc today     a
    AtDefault mc | ismultiperiod -> amountValueAtDate prices mc periodend a
    AtDefault mc                 -> amountValueAtDate prices mc today     a
    AtDate d  mc                 -> amountValueAtDate prices mc d         a

-- | Find the market value of this amount in the given valuation
-- commodity if any, otherwise the default valuation commodity, at the
-- given valuation date. (The default valuation commodity is the
-- commodity of the latest applicable market price before the
-- valuation date.)
-- If the market prices available on that date are not sufficient to
-- calculate this value, the amount is left unchanged.
amountValueAtDate :: Prices -> Maybe CommoditySymbol -> Day -> Amount -> Amount
amountValueAtDate prices mc d a =
  case priceLookup prices d mc (acommodity a) of
    Just v  -> v{aquantity=aquantity v * aquantity a}
    Nothing -> a

tests_Prices = tests "Prices" [
   tests_marketPriceInvert
  ,tests_priceLookup
  ]

