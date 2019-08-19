{-|

Convert amounts to some related value in various ways. This involves
looking up historical market prices (exchange rates) between commodities.

-}

{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Hledger.Data.Valuation (
   journalPriceOracle
  -- ,amountValueAtDate
  -- ,amountApplyValuation
  ,mixedAmountValueAtDate
  ,mixedAmountApplyValuation
  ,marketPriceReverse
  ,priceDirectiveToMarketPrice
  ,priceLookup
  ,tests_Valuation
)
where

import Control.Applicative ((<|>))
import Data.Decimal (roundTo)
import Data.Function (on)
import Data.Graph.Inductive  (Gr, Node, NodeMap, mkMapGraph, mkNode, lab, out, sp)
import Data.List
import Data.List.Extra (nubSortBy)
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Text as T
import Data.Time.Calendar (Day)
import Data.MemoUgly (memo)
import Safe (headMay)

import Hledger.Utils
import Hledger.Data.Types
import Hledger.Data.Amount
import Hledger.Data.Dates (parsedate)


tests_Valuation = tests "Valuation" [
   tests_priceLookup
  ]



------------------------------------------------------------------------------
-- Valuation

-- | Apply a specified valuation to this mixed amount, using the provided
-- price oracle, commodity styles, period-end/current dates,
-- and whether this is for a multiperiod report or not.
mixedAmountApplyValuation :: PriceOracle -> M.Map CommoditySymbol AmountStyle -> Day -> Day -> Bool -> ValuationType -> MixedAmount -> MixedAmount
mixedAmountApplyValuation priceoracle styles periodend today ismultiperiod v (Mixed as) =
  Mixed $ map (amountApplyValuation priceoracle styles periodend today ismultiperiod v) as

-- | Apply a specified valuation to this amount, using the provided
-- price oracle, commodity styles, period-end/current dates,
-- and whether this is for a multiperiod report or not.
amountApplyValuation :: PriceOracle -> M.Map CommoditySymbol AmountStyle -> Day -> Day -> Bool -> ValuationType -> Amount -> Amount
amountApplyValuation priceoracle styles periodend today ismultiperiod v a =
  case v of
    AtCost    Nothing            -> amountToCost styles a
    AtCost    mc                 -> amountValueAtDate priceoracle styles mc periodend $ amountToCost styles a
    AtEnd     mc                 -> amountValueAtDate priceoracle styles mc periodend a
    AtNow     mc                 -> amountValueAtDate priceoracle styles mc today     a
    AtDefault mc | ismultiperiod -> amountValueAtDate priceoracle styles mc periodend a
    AtDefault mc                 -> amountValueAtDate priceoracle styles mc today     a
    AtDate d  mc                 -> amountValueAtDate priceoracle styles mc d         a

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

-- From a journal's market price directives, generate a memoising function
-- that efficiently looks up exchange rates between commodities on any date.
-- For best results, you should generate this only once per journal, reusing it
-- across reports if there are more than one (as in compoundBalanceCommand).
journalPriceOracle :: Journal -> PriceOracle
journalPriceOracle Journal{jpricedirectives} =
  -- traceStack "journalPriceOracle" $
  let
    pricesatdate =
      memo $
      pricesAtDate jpricedirectives
  in
    memo $
    uncurry3 $
    priceLookup pricesatdate

-- | Given a list of price directives in parse order, find the market
-- value at the given date of one unit of a given source commodity, in
-- a different specified valuation commodity, or a default valuation
-- commodity.
--
-- When the valuation commodity is specified, this looks for, in order:
--
-- - a price declaration giving the exchange rate from source
--   commodity to valuation commodity ("declared price").
--
-- - a price declaration from valuation to source commodity, which
--   gets inverted ("reverse price").
--
-- - the shortest chain of prices (declared or reverse) leading from
--   source commodity to valuation commodity, which gets collapsed
--   into a single synthetic exchange rate ("indirect price").
--
-- When the valuation commodity is not specified, this looks for the
-- latest applicable declared price, and converts to the commodity
-- mentioned in that price (the default valuation commodity).
--
-- Note this default valuation commodity can vary across successive
-- calls for different dates, since it depends on the price
-- declarations in each period.
--
-- This returns the valuation commodity that was specified or
-- inferred, and the quantity of it that one unit of the source
-- commodity is worth. Or if no applicable market price or chain of
-- prices can be found, or the source commodity and the valuation
-- commodity are the same, returns Nothing.
--
priceLookup :: (Day -> PriceGraph) -> Day -> CommoditySymbol -> Maybe CommoditySymbol -> Maybe (CommoditySymbol, Quantity)
priceLookup pricesatdate d from mto =
  -- trace ("priceLookup ("++show d++", "++show from++", "++show mto++")") $
  let
    -- build a graph of the commodity exchange rates in effect on this day
    -- XXX should hide these fgl details better
    PriceGraph{prGraph=g, prNodemap=m, prDeclaredPairs=dps} = pricesatdate d
    fromnode = node m from
    mto' = mto <|> mdefaultto
      where
        -- If to is unspecified, try to pick a default valuation commodity from declared prices (only).
        -- XXX how to choose ? Take lowest sorted ?
        -- Take first, hoping current order is useful ?       <-
        -- Keep parse order in label and take latest parsed ?
        mdefaultto =
          dbg4 ("default valuation commodity for "++T.unpack from) $
          headMay [t | (f,t,_) <- out g fromnode, (f,t) `elem` dps] >>= lab g
  in
    case mto' of
      Nothing            -> Nothing
      Just to | to==from -> Nothing
      Just to            ->
        -- We have a commodity to convert to. Find the most direct price available.
        case mindirectprice of
          Nothing -> Nothing
          Just q  -> Just (to, q)
        where
          tonode = node m to
          mindirectprice :: Maybe Quantity =
            -- Find the shortest path, if any, between from and to.
            case sp fromnode tonode g :: Maybe [Node] of
              Nothing    -> Nothing
              Just nodes ->
                dbg ("market price "++intercalate "->" (map T.unpack comms)) $
                Just $ product $ pathEdgeLabels g nodes  -- convert to a single exchange rate
                where comms = catMaybes $ map (lab g) nodes

          -- log a message and a Maybe Quantity, hiding Just/Nothing and limiting decimal places
          dbg msg = dbg4With (((msg++": ")++) . maybe "" (show . roundTo 8))

tests_priceLookup =
  let
    d = parsedate
    a q c = amount{acommodity=c, aquantity=q}
    p date from q to = PriceDirective{pddate=d date, pdcommodity=from, pdamount=a q to}
    ps1 = [
       p "2000/01/01" "A" 10 "B"
      ,p "2000/01/01" "B" 10 "C"
      ,p "2000/01/01" "C" 10 "D"
      ,p "2000/01/01" "E"  2 "D"
      ,p "2001/01/01" "A" 11 "B"
      ]
    pricesatdate = pricesAtDate ps1
  in tests "priceLookup" [
     priceLookup pricesatdate (d "1999/01/01") "A" Nothing    `is` Nothing
    ,priceLookup pricesatdate (d "2000/01/01") "A" Nothing    `is` Just ("B",10)
    ,priceLookup pricesatdate (d "2000/01/01") "B" (Just "A") `is` Just ("A",0.1)
    ,priceLookup pricesatdate (d "2000/01/01") "A" (Just "E") `is` Just ("E",500)
    ]

------------------------------------------------------------------------------
-- Building the price graph (network of commodity conversions) on a given day.

-- | Convert a list of market price directives in parse order to a
-- graph of all prices in effect on a given day, allowing efficient
-- lookup of exchange rates between commodity pairs.
pricesAtDate :: [PriceDirective] -> Day -> PriceGraph
pricesAtDate pricedirectives d =
  -- trace ("pricesAtDate ("++show d++")") $
  PriceGraph{prGraph=g, prNodemap=m, prDeclaredPairs=dps}
  where
    declaredprices = latestPriceForEachPairOn pricedirectives d

    -- infer additional reverse prices where not already declared
    reverseprices =
      dbg5 "reverseprices" $
      map marketPriceReverse declaredprices \\ declaredprices

    -- build the graph and associated node map
    (g, m) =
      mkMapGraph
      (dbg5 "g nodelabels" $ sort allcomms) -- this must include all nodes mentioned in edges
      (dbg5 "g edges"      $ [(mpfrom, mpto, mprate) | MarketPrice{..} <- prices])
      :: (Gr CommoditySymbol Quantity, NodeMap CommoditySymbol)
      where
        prices   = declaredprices ++ reverseprices
        allcomms = map mpfrom prices

    -- remember which edges correspond to declared prices
    dps = [(node m mpfrom, node m mpto) | MarketPrice{..} <- declaredprices ]

-- From a list of price directives in parse order, get the latest
-- price declared on or before date d for each commodity pair.
latestPriceForEachPairOn :: [PriceDirective] -> Day -> [MarketPrice]
latestPriceForEachPairOn pricedirectives d =
  dbg5 "latestPriceForEachPairOn" $
  nubSortBy (compare `on` (\(MarketPrice{..})->(mpfrom,mpto))) $  -- keep only the first (ie newest and latest parsed) price for each pair
  map snd $  -- discard the parse order label
  sortBy (flip compare `on` (\(parseorder,mp)->(mpdate mp,parseorder))) $  -- sort with newest dates and latest parse order first
  zip [1..] $  -- label with parse order
  map priceDirectiveToMarketPrice $
  filter ((<=d).pddate) pricedirectives  -- consider only price declarations up to the valuation date

priceDirectiveToMarketPrice :: PriceDirective -> MarketPrice
priceDirectiveToMarketPrice PriceDirective{..} =
  MarketPrice{ mpdate = pddate
             , mpfrom = pdcommodity
             , mpto   = acommodity pdamount
             , mprate = aquantity pdamount
             }

marketPriceReverse :: MarketPrice -> MarketPrice
marketPriceReverse mp@MarketPrice{..} = mp{mpfrom=mpto, mpto=mpfrom, mprate=1/mprate}

------------------------------------------------------------------------------
-- fgl helpers

-- | Look up an existing graph node by its label.
-- (If the node does not exist, a new one will be generated, but not
-- persisted in the nodemap.)
node :: Ord a => NodeMap a -> a -> Node
node m = fst . fst . mkNode m

-- | Convert a valid path within the given graph to the corresponding
-- edge labels. When there are multiple edges between two nodes, the
-- lowest-sorting label is used.
pathEdgeLabels :: (Show b, Ord b) => Gr a b -> [Node] -> [b]
pathEdgeLabels g = map frommaybe . map (nodesEdgeLabel g) . pathEdges
  where frommaybe = fromMaybe (error' "pathEdgeLabels: expected no Nothings here")

-- | Convert a path to node pairs representing the path's edges.
pathEdges :: [Node] -> [(Node,Node)]
pathEdges p = [(f,t) | f:t:_ <- tails p]

-- | Get the label of a graph edge from one node to another.
-- When there are multiple such edges, the lowest-sorting label is used.
nodesEdgeLabel :: Ord b => Gr a b -> (Node, Node) -> Maybe b
nodesEdgeLabel g (from,to) = headMay $ sort [l | (_,t,l) <- out g from, t==to]

------------------------------------------------------------------------------
