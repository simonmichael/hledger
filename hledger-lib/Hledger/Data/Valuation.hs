{-|

Convert amounts to some related value in various ways. This involves
looking up historical market prices (exchange rates) between commodities.

-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}

module Hledger.Data.Valuation (
   Costing(..)
  ,ValuationType(..)
  ,PriceOracle
  ,journalPriceOracle
  ,mixedAmountToCost
  ,mixedAmountApplyValuation
  ,mixedAmountValueAtDate
  ,marketPriceReverse
  ,priceDirectiveToMarketPrice
  -- ,priceLookup
  ,tests_Valuation
)
where

import Control.Applicative ((<|>))
import Data.Function ((&), on)
import Data.List (partition, intercalate, sortBy)
import Data.List.Extra (nubSortBy)
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Time.Calendar (Day, fromGregorian)
import Data.MemoUgly (memo)
import GHC.Generics (Generic)
import Safe (headMay, lastMay)

import Hledger.Utils
import Hledger.Data.Types
import Hledger.Data.Amount
import Hledger.Data.Dates (nulldate)
import Hledger.Data.Commodity (showCommoditySymbol)
import Data.Maybe (fromMaybe)
import Text.Printf (printf)


------------------------------------------------------------------------------
-- Types

-- | Whether to convert amounts to cost.
data Costing = Cost | NoCost
  deriving (Show,Eq)

-- | What kind of value conversion should be done on amounts ?
-- CLI: --value=then|end|now|DATE[,COMM]
data ValuationType =
    AtThen     (Maybe CommoditySymbol)  -- ^ convert to default or given valuation commodity, using market prices at each posting's date
  | AtEnd      (Maybe CommoditySymbol)  -- ^ convert to default or given valuation commodity, using market prices at period end(s)
  | AtNow      (Maybe CommoditySymbol)  -- ^ convert to default or given valuation commodity, using current market prices
  | AtDate Day (Maybe CommoditySymbol)  -- ^ convert to default or given valuation commodity, using market prices on some date
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

-- | Convert all component amounts to cost/selling price if requested, and style them.
mixedAmountToCost :: Costing -> M.Map CommoditySymbol AmountStyle -> MixedAmount -> MixedAmount
mixedAmountToCost cost styles = mapMixedAmount (amountToCost cost styles)

-- | Apply a specified valuation to this mixed amount, using the
-- provided price oracle, commodity styles, and reference dates.
-- See amountApplyValuation.
mixedAmountApplyValuation :: PriceOracle -> M.Map CommoditySymbol AmountStyle -> Day -> Day -> Day -> ValuationType -> MixedAmount -> MixedAmount
mixedAmountApplyValuation priceoracle styles periodlast today postingdate v =
  mapMixedAmount (amountApplyValuation priceoracle styles periodlast today postingdate v)

-- | Convert an Amount to its cost if requested, and style it appropriately.
amountToCost :: Costing -> M.Map CommoditySymbol AmountStyle -> Amount -> Amount
amountToCost NoCost _      = id
amountToCost Cost   styles = styleAmount styles . amountCost

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
-- This is all a bit complicated. See the reference doc at
-- https://hledger.org/hledger.html#effect-of-valuation-on-reports
-- (hledger_options.m4.md "Effect of valuation on reports"), and #1083.
--
amountApplyValuation :: PriceOracle -> M.Map CommoditySymbol AmountStyle -> Day -> Day -> Day -> ValuationType -> Amount -> Amount
amountApplyValuation priceoracle styles periodlast today postingdate v a =
  case v of
    AtThen    mc      -> amountValueAtDate priceoracle styles mc postingdate a
    AtEnd     mc      -> amountValueAtDate priceoracle styles mc periodlast a
    AtNow     mc      -> amountValueAtDate priceoracle styles mc today a
    AtDate d  mc      -> amountValueAtDate priceoracle styles mc d a

-- | Find the market value of each component amount in the given
-- commodity, or its default valuation commodity, at the given
-- valuation date, using the given market price oracle.
-- When market prices available on that date are not sufficient to
-- calculate the value, amounts are left unchanged.
mixedAmountValueAtDate :: PriceOracle -> M.Map CommoditySymbol AmountStyle -> Maybe CommoditySymbol -> Day -> MixedAmount -> MixedAmount
mixedAmountValueAtDate priceoracle styles mc d = mapMixedAmount (amountValueAtDate priceoracle styles mc d)

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
        let msg = printf "seeking %s to %s price" (showCommoditySymbol from) (showCommoditySymbol to)
        in case 
          (traceAt 2 (msg++" using forward prices") $ 
            pricesShortestPath from to forwardprices)
          <|> 
          (traceAt 2 (msg++" using forward and reverse prices") $ 
            pricesShortestPath from to allprices)
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
-- built directly with MarketPrices for now, probably space-inefficient

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
pricesShortestPath :: CommoditySymbol -> CommoditySymbol -> [Edge] -> Maybe Path
pricesShortestPath start end edges =
  -- at --debug=2 +, print the pretty path and also the detailed prices
  let label = printf "shortest path from %s to %s: " (showCommoditySymbol start) (showCommoditySymbol end) in
  fmap (dbg2With (("price chain:\n"++).pshow)) $ 
  dbg2With ((label++).(maybe "none found" (pshowpath ""))) $

  find [([],edges)]

  where
    -- Find the first and shortest complete path using a breadth-first search.
    find :: [(Path,[Edge])] -> Maybe Path
    find paths =
      case concatMap extend paths of
        [] -> Nothing 
        _ | pathlength > maxpathlength -> 
          trace ("gave up searching for a price chain at length "++show maxpathlength++", please report a bug")
          Nothing
          where 
            pathlength = 2 + maybe 0 (length . fst) (headMay paths)
            maxpathlength = 1000
        paths' -> 
          case completepaths of
                p:_ -> Just p  -- the left-most complete path at this length
                []  -> find paths'
          where completepaths = [p | (p,_) <- paths', (mpto <$> lastMay p) == Just end]

    -- Use all applicable edges from those provided to extend this path by one step,
    -- returning zero or more new (path, remaining edges) pairs.
    extend :: (Path,[Edge]) -> [(Path,[Edge])]
    extend (path,unusededges) =
      let
        pathnodes = start : map mpto path
        pathend = fromMaybe start $ mpto <$> lastMay path
        (nextedges,remainingedges) = partition ((==pathend).mpfrom) unusededges
      in
        [ (path', remainingedges')
        | e <- nextedges
        , let path' = dbgpath "trying" $ path ++ [e]  -- PERF prepend ?
        , let pathnodes' = mpto e : pathnodes
        , let remainingedges' = [r | r <- remainingedges, not $ mpto r `elem` pathnodes' ]
        ]

-- debug helpers
dbgpath  label = dbg2With (pshowpath label)
-- dbgedges label = dbg2With (pshowedges label)
pshowpath label = \case
  []      -> prefix label ""
  p@(e:_) -> prefix label $ pshownode (mpfrom e) ++ ">" ++ intercalate ">" (map (pshownode . mpto) p)
-- pshowedges label = prefix label . intercalate ", " . map (pshowedge "")
-- pshowedge label MarketPrice{..} = pshowedge' label mpfrom mpto
-- pshowedge' label from to = prefix label $ pshownode from ++ ">" ++ pshownode to
pshownode = T.unpack . showCommoditySymbol
prefix l = if null l then (""++) else ((l++": ")++)

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
--    as declared by a P directive, or (with the `--infer-market-price` flag)
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
--    the `--infer-market-price` flag is used, then the price commodity from
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
    visibledeclaredprices = dbg9 "visibledeclaredprices" $ filter ((<=d).mpdate) alldeclaredprices
    visibleinferredprices = dbg9 "visibleinferredprices" $ filter ((<=d).mpdate) allinferredprices
    forwardprices = effectiveMarketPrices visibledeclaredprices visibleinferredprices

    -- infer any additional reverse prices not already declared or inferred
    reverseprices = dbg9 "additional reverse prices" $
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
        pricesfordefaultcomms = dbg9 "prices for choosing default valuation commodities, by date then parse order" $
          ps
          & zip [1..]  -- label items with their parse order
          & sortBy (compare `on` (\(parseorder,MarketPrice{..})->(mpdate,parseorder)))  -- sort by increasing date then increasing parse order
          & map snd    -- discard labels
          where
            ps | not $ null visibledeclaredprices = visibledeclaredprices
               | not $ null alldeclaredprices     = alldeclaredprices
               | otherwise                        = visibleinferredprices  -- will be null without --infer-market-price

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
    dbg9 "effective forward prices" $
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
