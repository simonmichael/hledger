{-|

Convert amounts to some related value in various ways. This involves
looking up historical market prices (exchange rates) between commodities.

-}

{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}

module Hledger.Data.Valuation (
   ValuationType(..)
  ,PriceOracle
  ,journalPriceOracle
  ,unsupportedValueThenError
  -- ,amountApplyValuation
  -- ,amountValueAtDate
  ,mixedAmountApplyValuation
  ,mixedAmountValueAtDate
  ,marketPriceReverse
  ,priceDirectiveToMarketPrice
  -- ,priceLookup
  ,tests_Valuation
)
where

import Control.Applicative ((<|>))
import Data.Foldable (asum)
import Data.Function ((&), on)
import Data.List ( (\\), sortBy )
import Data.List.Extra (nubSortBy)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe ( fromMaybe )
import qualified Data.Text as T
import Data.Time.Calendar (Day, fromGregorian)
import Data.MemoUgly (memo)
import GHC.Generics (Generic)
import Safe (lastMay)

import Hledger.Utils
import Hledger.Data.Types
import Hledger.Data.Amount
import Hledger.Data.Dates (nulldate)


------------------------------------------------------------------------------
-- Types

-- | What kind of value conversion should be done on amounts ?
-- CLI: --value=cost|then|end|now|DATE[,COMM]
data ValuationType =
    AtCost     (Maybe CommoditySymbol)  -- ^ convert to cost commodity using transaction prices, then optionally to given commodity using market prices at posting date
  | AtThen     (Maybe CommoditySymbol)  -- ^ convert to default or given valuation commodity, using market prices at each posting's date
  | AtEnd      (Maybe CommoditySymbol)  -- ^ convert to default or given valuation commodity, using market prices at period end(s)
  | AtNow      (Maybe CommoditySymbol)  -- ^ convert to default or given valuation commodity, using current market prices
  | AtDate Day (Maybe CommoditySymbol)  -- ^ convert to default or given valuation commodity, using market prices on some date
  | AtDefault  (Maybe CommoditySymbol)  -- ^ works like AtNow in single period reports, like AtEnd in multiperiod reports
  deriving (Show,Eq)

-- | A price oracle is a magic memoising function that efficiently
-- looks up market prices (exchange rates) from one commodity to
-- another (or if unspecified, to a default valuation commodity) on a
-- given date.
type PriceOracle = (Day, CommoditySymbol, Maybe CommoditySymbol) -> Maybe (CommoditySymbol, Quantity)

-- | Generate a price oracle (memoising price lookup function) from a
-- journal's directive-declared and transaction-inferred market
-- prices. For best performance, generate this only once per journal,
-- reusing it across reports if there are more than one, as
-- compoundBalanceCommand does.
-- The boolean argument is whether to infer market prices from
-- transactions or not.
journalPriceOracle :: Bool -> Journal -> PriceOracle
journalPriceOracle infer Journal{jpricedirectives, jinferredmarketprices} =
  let
    declaredprices = map priceDirectiveToMarketPrice jpricedirectives
    inferredprices = if infer then jinferredmarketprices else []
    makepricegraph = memo $ makePriceGraph declaredprices inferredprices
  in
    memo $ uncurry3 $ priceLookup makepricegraph

priceDirectiveToMarketPrice :: PriceDirective -> MarketPrice
priceDirectiveToMarketPrice PriceDirective{..} =
  MarketPrice{ mpdate = pddate
             , mpfrom = pdcommodity
             , mpto   = acommodity pdamount
             , mprate = aquantity pdamount
             }

------------------------------------------------------------------------------
-- Converting things to value

-- | Apply a specified valuation to this mixed amount, using the
-- provided price oracle, commodity styles, reference dates, and
-- whether this is for a multiperiod report or not.
-- See amountApplyValuation.
mixedAmountApplyValuation :: PriceOracle -> M.Map CommoditySymbol AmountStyle -> Day -> Maybe Day -> Day -> Bool -> ValuationType -> MixedAmount -> MixedAmount
mixedAmountApplyValuation priceoracle styles periodlast mreportlast today ismultiperiod v (Mixed as) =
  Mixed $ map (amountApplyValuation priceoracle styles periodlast mreportlast today ismultiperiod v) as

-- | Apply a specified valuation to this amount, using the provided
-- price oracle, reference dates, and whether this is for a
-- multiperiod report or not. Also fix up its display style using the
-- provided commodity styles.
--
-- When the valuation requires converting to another commodity, a
-- valuation (conversion) date is chosen based on the valuation type,
-- the provided reference dates, and whether this is for a
-- single-period or multi-period report. It will be one of:
--
-- - a fixed date specified by the ValuationType itself
--   (--value=DATE).
-- 
-- - the provided "period end" date - this is typically the last day
--   of a subperiod (--value=end with a multi-period report), or of
--   the specified report period or the journal (--value=end with a
--   single-period report).
--
-- - the provided "report end" date - the last day of the specified
--   report period, if any (-V/-X with a report end date).
--
-- - the provided "today" date - (--value=now, or -V/X with no report
--   end date).
-- 
-- Note --value=then is not supported by this function, and will cause an error;
-- use postingApplyValuation for that.
-- 
-- This is all a bit complicated. See the reference doc at
-- https://hledger.org/hledger.html#effect-of-valuation-on-reports
-- (hledger_options.m4.md "Effect of valuation on reports"), and #1083.
--
amountApplyValuation :: PriceOracle -> M.Map CommoditySymbol AmountStyle -> Day -> Maybe Day -> Day -> Bool -> ValuationType -> Amount -> Amount
amountApplyValuation priceoracle styles periodlast mreportlast today ismultiperiod v a =
  case v of
    AtCost    Nothing            -> styleAmount styles $ amountCost a
    AtCost    mc                 -> amountValueAtDate priceoracle styles mc periodlast $ styleAmount styles $ amountCost a
    AtThen    _mc                -> error' unsupportedValueThenError  -- PARTIAL:
                                 -- amountValueAtDate priceoracle styles mc periodlast a  -- posting date unknown, handle like AtEnd
    AtEnd     mc                 -> amountValueAtDate priceoracle styles mc periodlast a
    AtNow     mc                 -> amountValueAtDate priceoracle styles mc today a
    AtDefault mc | ismultiperiod -> amountValueAtDate priceoracle styles mc periodlast a
    AtDefault mc                 -> amountValueAtDate priceoracle styles mc (fromMaybe today mreportlast) a
    AtDate d  mc                 -> amountValueAtDate priceoracle styles mc d a

-- | Standard error message for a report not supporting --value=then.
unsupportedValueThenError :: String
unsupportedValueThenError = "Sorry, --value=then is not yet supported for this kind of report."

-- | Find the market value of each component amount in the given
-- commodity, or its default valuation commodity, at the given
-- valuation date, using the given market price oracle.
-- When market prices available on that date are not sufficient to
-- calculate the value, amounts are left unchanged.
mixedAmountValueAtDate :: PriceOracle -> M.Map CommoditySymbol AmountStyle -> Maybe CommoditySymbol -> Day -> MixedAmount -> MixedAmount
mixedAmountValueAtDate priceoracle styles mc d (Mixed as) = Mixed $ map (amountValueAtDate priceoracle styles mc d) as

-- | Find the market value of this amount in the given valuation
-- commodity if any, otherwise the default valuation commodity, at the
-- given valuation date. (The default valuation commodity is the
-- commodity of the latest applicable market price before the
-- valuation date.)
--
-- The returned amount will have its commodity's canonical style applied,
-- but with the precision adjusted to show all significant decimal digits
-- up to a maximum of 8. (experimental)
--
-- If the market prices available on that date are not sufficient to
-- calculate this value, the amount is left unchanged.
amountValueAtDate :: PriceOracle -> M.Map CommoditySymbol AmountStyle -> Maybe CommoditySymbol -> Day -> Amount -> Amount
amountValueAtDate priceoracle styles mto d a =
  case priceoracle (d, acommodity a, mto) of
    Nothing           -> a
    Just (comm, rate) ->
      -- setNaturalPrecisionUpTo 8 $  -- XXX force higher precision in case amount appears to be zero ?
                                      -- Make default display style use precision 2 instead of 0 ?
                                      -- Leave as is for now; mentioned in manual.
      styleAmount styles
      amount{acommodity=comm, aquantity=rate * aquantity a}

------------------------------------------------------------------------------
-- Market price lookup

-- | Given a memoising price graph generator, a valuation date, a
-- source commodity and an optional valuation commodity, find the
-- value on that date of one unit of the source commodity in the
-- valuation commodity, or in a default valuation commodity. Returns
-- the valuation commodity that was specified or chosen, and the
-- quantity of it that one unit of the source commodity is worth. Or
-- if no applicable market price can be found or calculated, or if the
-- source commodity and the valuation commodity are the same, returns
-- Nothing.
--
-- See makePriceGraph for how prices are determined.
-- Note that both market prices and default valuation commodities can
-- vary with valuation date, since that determines which market prices
-- are visible.
--
priceLookup :: (Day -> PriceGraph) -> Day -> CommoditySymbol -> Maybe CommoditySymbol -> Maybe (CommoditySymbol, Quantity)
priceLookup makepricegraph d from mto =
  -- trace ("priceLookup ("++show d++", "++show from++", "++show mto++")") $
  let
    PriceGraph{pgEdges=forwardprices
              ,pgEdgesRev=allprices
              ,pgDefaultValuationCommodities=defaultdests
              } =
      traceAt 1 ("valuation date: "++show d) $ makepricegraph d
    mto' = mto <|> mdefaultto
      where
        mdefaultto = dbg1 ("default valuation commodity for "++T.unpack from) $
                     M.lookup from defaultdests
  in
    case mto' of
      Nothing            -> Nothing
      Just to | to==from -> Nothing
      Just to            ->
        -- We have a commodity to convert to. Find the most direct price available,
        -- according to the rules described in makePriceGraph.
        case 
          pricesShortestPath forwardprices from to <|> 
          pricesShortestPath allprices     from to 
        of
          Nothing -> Nothing
          Just [] -> Nothing
          Just ps -> Just (mpto $ last ps, product $ map mprate ps)

tests_priceLookup =
  let
    p y m d from q to = MarketPrice{mpdate=fromGregorian y m d, mpfrom=from, mpto=to, mprate=q}
    ps1 = [
       p 2000 01 01 "A" 10 "B"
      ,p 2000 01 01 "B" 10 "C"
      ,p 2000 01 01 "C" 10 "D"
      ,p 2000 01 01 "E"  2 "D"
      ,p 2001 01 01 "A" 11 "B"
      ]
    makepricegraph = makePriceGraph ps1 []
  in test "priceLookup" $ do
    priceLookup makepricegraph (fromGregorian 1999 01 01) "A" Nothing    @?= Nothing
    priceLookup makepricegraph (fromGregorian 2000 01 01) "A" Nothing    @?= Just ("B",10)
    priceLookup makepricegraph (fromGregorian 2000 01 01) "B" (Just "A") @?= Just ("A",0.1)
    priceLookup makepricegraph (fromGregorian 2000 01 01) "A" (Just "E") @?= Just ("E",500)

------------------------------------------------------------------------------
-- Market price graph

type Edge = MarketPrice
type Path = [Edge]

data PriceGraph = PriceGraph {
   pgDate :: Day
    -- ^ The date on which these prices are in effect.
  ,pgEdges :: [Edge]
    -- ^ "Forward" exchange rates between commodity pairs, either
    --   declared by P directives or inferred from transaction prices,
    --   forming the edges of a directed graph.  
  ,pgEdgesRev :: [Edge]
    -- ^ The same edges, plus any additional edges that can be
    --   inferred by reversing them and inverting the rates.
    --
    --   In both of these there will be at most one edge between each
    --   directed pair of commodities, eg there can be one USD->EUR and one EUR->USD.
  ,pgDefaultValuationCommodities :: M.Map CommoditySymbol CommoditySymbol
    -- ^ The default valuation commodity for each source commodity.
    --   These are used when a valuation commodity is not specified
    --   (-V). They are the destination commodity of each source commodity's
    --   latest (declared or inferred, but not reverse) market price
    --   (on the date of this graph).
  }
  deriving (Show,Generic)

-- | Find the shortest path and corresponding conversion rate, if any, 
-- from one commodity to another using the provided market prices which
-- form the edges of a directed graph. There should be at most one edge
-- between each directed pair of commodities, eg there can be one
-- USD->EUR price and one EUR->USD price.
pricesShortestPath :: [Edge] -> CommoditySymbol -> CommoditySymbol -> Maybe Path
pricesShortestPath edges start end =
  dbg1 ("shortest price path for "++T.unpack start++" -> "++T.unpack end) $ 
  asum $ map (findPath end edgesremaining) initialpaths
  where
    initialpaths = dbg9 "initial price paths" $ [[p] | p <- edges, mpfrom p == start]
    edgesremaining = dbg9 "initial edges remaining" $ edges \\ concat initialpaths

-- Helper: breadth-first search for a continuation of the given path
-- using zero or more of the given edges, to the specified end commodity.
-- Returns the first & shortest complete path found, or Nothing.
findPath :: CommoditySymbol -> [Edge] -> Path -> Maybe Path
findPath end _ path | mpathend == Just end = Just path  -- path is complete
  where 
    mpathend = mpto <$> lastMay path
findPath _ [] _ = Nothing   -- no more edges are available
findPath end edgesremaining path =   -- try continuing with all the remaining edges
  asum [ 
      findPath end edgesremaining' path'
    | e <- nextedges
    , let path' = path++[e]
    , let edgesremaining' = filter (/=e) edgesremaining
    ]
  where
    nextedges = [ e | e <- edgesremaining, Just (mpfrom e) == mpathend ]
      where
        mpathend = mpto <$> lastMay path

-- | A snapshot of the known exchange rates between commodity pairs at a given date.
-- This is a home-made version, more tailored to our needs.
-- | Build the graph of commodity conversion prices for a given day.
-- Converts a list of declared market prices in parse order, and a
-- list of transaction-inferred market prices in parse order, to:
--
-- 1. a graph of all known exchange rates declared or inferred from 
-- one commodity to another in effect on that day
--
-- 2. a second graph which includes any additional exchange rates
-- that can be inferred by reversing known rates
--
-- 3. a map of each commodity's default valuation commodity, if any.
--
-- These allow price lookup and valuation to be performed as
-- described in hledger.m4.md -> Valuation:
--
-- "hledger looks for a market price (exchange rate) from commodity A
-- to commodity B in one or more of these ways, in this order of
-- preference:
--
-- 1. A *declared market price* or *inferred market price*:
--    A's latest market price in B on or before the valuation date
--    as declared by a P directive, or (with the `--infer-value` flag)
--    inferred from transaction prices.
--   
-- 2. A *reverse market price*:
--    the inverse of a declared or inferred market price from B to A.
-- 
-- 3. A *a forward chain of market prices*:
--    a synthetic price formed by combining the shortest chain of
--    "forward" (only 1 above) market prices, leading from A to B.
--
-- 4. A *any chain of market prices*:
--    a chain of any market prices, including both forward and
--    reverse prices (1 and 2 above), leading from A to B."
--
-- and: "For each commodity A, hledger picks a default valuation
-- commodity as follows, in this order of preference:
--
-- 1. The price commodity from the latest declared market price for A
--    on or before valuation date.
--
-- 2. The price commodity from the latest declared market price for A
--    on any date. (Allows conversion to proceed if there are inferred
--    prices before the valuation date.)
--
-- 3. If there are no P directives at all (any commodity or date), and
--    the `--infer-value` flag is used, then the price commodity from
--    the latest transaction price for A on or before valuation date."
--
makePriceGraph :: [MarketPrice] -> [MarketPrice] -> Day -> PriceGraph
makePriceGraph alldeclaredprices allinferredprices d =
  dbg9 ("makePriceGraph "++show d) $
  PriceGraph{
     pgDate = d
    ,pgEdges=forwardprices
    ,pgEdgesRev=allprices
    ,pgDefaultValuationCommodities=defaultdests
    }
  where
    -- prices in effect on date d, either declared or inferred
    visibledeclaredprices = dbg2 "visibledeclaredprices" $ filter ((<=d).mpdate) alldeclaredprices
    visibleinferredprices = dbg2 "visibleinferredprices" $ filter ((<=d).mpdate) allinferredprices
    forwardprices = effectiveMarketPrices visibledeclaredprices visibleinferredprices

    -- infer any additional reverse prices not already declared or inferred
    reverseprices = dbg2 "additional reverse prices" $
      [p | p@MarketPrice{..} <- map marketPriceReverse forwardprices
         , not $ (mpfrom,mpto) `S.member` forwardpairs
      ]
      where
        forwardpairs = S.fromList [(mpfrom,mpto) | MarketPrice{..} <- forwardprices]
    allprices = forwardprices ++ reverseprices

    -- determine a default valuation commodity for each source commodity
    -- somewhat but not quite like effectiveMarketPrices
    defaultdests = M.fromList [(mpfrom,mpto) | MarketPrice{..} <- pricesfordefaultcomms]
      where
        pricesfordefaultcomms = dbg2 "prices for choosing default valuation commodities, by date then parse order" $
          ps
          & zip [1..]  -- label items with their parse order
          & sortBy (compare `on` (\(parseorder,MarketPrice{..})->(mpdate,parseorder)))  -- sort by increasing date then increasing parse order
          & map snd    -- discard labels
          where
            ps | not $ null visibledeclaredprices = visibledeclaredprices
               | not $ null alldeclaredprices     = alldeclaredprices
               | otherwise                        = visibleinferredprices  -- will be null without --infer-value

-- | Given a list of P-declared market prices in parse order and a
-- list of transaction-inferred market prices in parse order, select
-- just the latest prices that are in effect for each commodity pair.
-- That is, for each commodity pair, the latest price by date then
-- parse order, with declared prices having precedence over inferred
-- prices on the same day.
effectiveMarketPrices :: [MarketPrice] -> [MarketPrice] -> [MarketPrice]
effectiveMarketPrices declaredprices inferredprices =
  let
    -- label each item with its same-day precedence, then parse order
    declaredprices' = [(1, i, p) | (i,p) <- zip [1..] declaredprices]
    inferredprices' = [(0, i, p) | (i,p) <- zip [1..] inferredprices]
  in
    dbg2 "effective forward prices" $
    -- combine
    declaredprices' ++ inferredprices'
    -- sort by decreasing date then decreasing precedence then decreasing parse order
    & sortBy (flip compare `on` (\(precedence,parseorder,mp)->(mpdate mp,precedence,parseorder)))
    -- discard the sorting labels
    & map third3
    -- keep only the first (ie the newest, highest precedence, latest parsed) price for each pair
    & nubSortBy (compare `on` (\(MarketPrice{..})->(mpfrom,mpto)))

marketPriceReverse :: MarketPrice -> MarketPrice
marketPriceReverse mp@MarketPrice{..} = 
  mp{mpfrom=mpto, mpto=mpfrom, mprate=if mprate==0 then 0 else 1/mprate}  -- PARTIAL: /

nullmarketprice :: MarketPrice
nullmarketprice = MarketPrice {
   mpdate=nulldate
  ,mpfrom=""
  ,mpto=""
  ,mprate=0
  }

------------------------------------------------------------------------------

tests_Valuation = tests "Valuation" [
   tests_priceLookup
  ,test "marketPriceReverse" $ do
    marketPriceReverse nullmarketprice{mprate=2} @?= nullmarketprice{mprate=0.5}
    marketPriceReverse nullmarketprice @?= nullmarketprice  -- the reverse of a 0 price is a 0 price


  ]
