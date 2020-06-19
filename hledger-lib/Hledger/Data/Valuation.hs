{-|

Convert amounts to some related value in various ways. This involves
looking up historical market prices (exchange rates) between commodities.

-}

{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}

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
import Control.DeepSeq (NFData)
import Data.Data
import Data.Decimal (roundTo)
import Data.Function ((&), on)
import Data.Graph.Inductive  (Gr, Node, NodeMap, mkMapGraph, mkNode, lab, out, sp)
import Data.List
import Data.List.Extra (nubSortBy)
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Text as T
import Data.Time.Calendar (Day)
import Data.MemoUgly (memo)
import GHC.Generics (Generic)
import Safe (headMay)

import Hledger.Utils
import Hledger.Data.Types
import Hledger.Data.Amount
import Hledger.Data.Dates (parsedate)


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
  deriving (Show,Data,Eq) -- Typeable

-- | A snapshot of the known exchange rates between commodity pairs at a given date,
-- as a graph allowing fast lookup and path finding, along with some helper data.
data PriceGraph = PriceGraph {
   prGraph   :: Gr CommoditySymbol Quantity
    -- ^ A directed graph of exchange rates between commodity pairs.
    -- Node labels are commodities and edge labels are exchange rates,
    -- which were either:
    -- declared by P directives,
    -- inferred from transaction prices,
    -- inferred by reversing a declared rate,
    -- or inferred by reversing a transaction-inferred rate.
    -- There will be at most one edge between each directed pair of commodities,
    -- eg there can be one USD->EUR and one EUR->USD.
  ,prNodemap :: NodeMap CommoditySymbol
    -- ^ Mapping of graph node ids to commodity symbols.
  ,prDefaultValuationCommodities :: M.Map CommoditySymbol CommoditySymbol
    -- ^ The default valuation commodity for each source commodity.
    --   These are used when a valuation commodity is not specified
    --   (-V). They are the destination commodity of the latest
    --   (declared or inferred, but not reverse) each
    --   source commodity's latest market price (on the date of this
    --   graph).
  }
  deriving (Show,Generic)

instance NFData PriceGraph

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
journalPriceOracle :: Journal -> PriceOracle
journalPriceOracle Journal{jpricedirectives, jinferredmarketprices} =
  let
    declaredprices = map priceDirectiveToMarketPrice jpricedirectives
    makepricegraph = memo $ makePriceGraph declaredprices jinferredmarketprices
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
    AtThen    _mc                -> error' unsupportedValueThenError  -- TODO
                                 -- amountValueAtDate priceoracle styles mc periodlast a  -- posting date unknown, handle like AtEnd
    AtEnd     mc                 -> amountValueAtDate priceoracle styles mc periodlast a
    AtNow     mc                 -> amountValueAtDate priceoracle styles mc today a
    AtDefault mc | ismultiperiod -> amountValueAtDate priceoracle styles mc periodlast a
    AtDefault mc                 -> amountValueAtDate priceoracle styles mc (fromMaybe today mreportlast) a
    AtDate d  mc                 -> amountValueAtDate priceoracle styles mc d a

-- | Standard error message for a report not supporting --value=then.
unsupportedValueThenError :: String
unsupportedValueThenError = "Sorry, --value=then is not yet implemented for this kind of report."

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
    -- build a graph of the commodity exchange rates in effect on this day
    -- XXX should hide these fgl details better
    PriceGraph{prGraph=g, prNodemap=m, prDefaultValuationCommodities=defaultdests} = makepricegraph d
    fromnode = node m from
    mto' = mto <|> mdefaultto
      where
        mdefaultto = dbg1 ("default valuation commodity for "++T.unpack from) $
                     M.lookup from defaultdests
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
                dbg ("market price for "++intercalate " -> " (map T.unpack comms)) $
                Just $ product $ pathEdgeLabels g nodes  -- convert to a single exchange rate
                where comms = catMaybes $ map (lab g) nodes

          -- log a message and a Maybe Quantity, hiding Just/Nothing and limiting decimal places
          dbg msg = dbg1With (((msg++": ")++) . maybe "" (show . roundTo 8))

tests_priceLookup =
  let
    d = parsedate
    p date from q to = MarketPrice{mpdate=d date, mpfrom=from, mpto=to, mprate=q}
    ps1 = [
       p "2000/01/01" "A" 10 "B"
      ,p "2000/01/01" "B" 10 "C"
      ,p "2000/01/01" "C" 10 "D"
      ,p "2000/01/01" "E"  2 "D"
      ,p "2001/01/01" "A" 11 "B"
      ]
    makepricegraph = makePriceGraph ps1 []
  in test "priceLookup" $ do
    priceLookup makepricegraph (d "1999/01/01") "A" Nothing    @?= Nothing
    priceLookup makepricegraph (d "2000/01/01") "A" Nothing    @?= Just ("B",10)
    priceLookup makepricegraph (d "2000/01/01") "B" (Just "A") @?= Just ("A",0.1)
    priceLookup makepricegraph (d "2000/01/01") "A" (Just "E") @?= Just ("E",500)

-- | Build the graph of commodity conversion prices for a given day.
-- Converts a list of declared market prices in parse order, and a
-- list of transaction-inferred market prices in parse order, to a
-- graph of all known exchange rates between commodity pairs in effect
-- on that day. Cf hledger.m4.md -> Valuation:
--
-- hledger looks for a market price (exchange rate) from commodity A
-- to commodity B in one or more of these ways, in this order of
-- preference:
--
-- 1. A *declared market price* or *inferred market price*:
--    A's latest market price in B on or before the valuation date
--    as declared by a P directive, or (with the `--value-infer` flag)
--    inferred from transaction prices.
--   
-- 2. A *reverse market price*:
--    the inverse of a declared or inferred market price from B to A.
-- 
-- 3. A *chained market price*:
--    a synthetic price formed by combining the shortest chain of market
--    prices (any of the above types) leading from A to B.
--
-- 1 and 2 form the edges of the price graph, and we can query it for
-- 3 (which is the reason we use a graph).
--
-- We also identify each commodity's default valuation commodity, if
-- any. For each commodity A, hledger picks a default valuation
-- commodity as follows:
--
-- 1. The price commodity from the latest (on or before valuation
--    date) declared market price for A.
--
-- 2. If there are no P directives at all (any commodity, any date),
--    and the `--value-infer` flag is used, then the price commodity
--    from the latest (on or before valuation date) transaction price
--    for A.
--
makePriceGraph :: [MarketPrice] -> [MarketPrice] -> Day -> PriceGraph
makePriceGraph alldeclaredprices allinferredprices d =
  dbg9 ("makePriceGraph "++show d) $
  PriceGraph{prGraph=g, prNodemap=m, prDefaultValuationCommodities=defaultdests}
  where
    -- prices in effect on date d, either declared or inferred
    declaredandinferredprices = dbg2 "declaredandinferredprices" $
      declaredOrInferredPricesOn alldeclaredprices allinferredprices d

    -- infer any additional reverse prices not already declared or inferred
    reverseprices = dbg2 "reverseprices" $
      map marketPriceReverse declaredandinferredprices \\ declaredandinferredprices

    -- build the graph and associated node map
    (g, m) =
      mkMapGraph
      (dbg9 "price graph labels" $ sort allcomms) -- this must include all nodes mentioned in edges
      (dbg9 "price graph edges" $ [(mpfrom, mpto, mprate) | MarketPrice{..} <- prices])
      :: (Gr CommoditySymbol Quantity, NodeMap CommoditySymbol)
      where
        prices   = declaredandinferredprices ++ reverseprices
        allcomms = map mpfrom prices

    -- determine a default valuation commodity D for each source commodity S:
    -- the price commodity in the latest declared market price for S (on any date)
    defaultdests = M.fromList [(mpfrom,mpto) | MarketPrice{..} <- alldeclaredprices]

-- | From a list of directive-declared market prices in parse order,
-- and a list of transaction-inferred market prices in parse order,
-- get the effective price on the given date for each commodity pair.
-- That is, the latest (by date then parse order) declared price or
-- inferred price, on or before that date, If there is both a declared
-- and inferred price on the same day, declared takes precedence.
declaredOrInferredPricesOn :: [MarketPrice] -> [MarketPrice] -> Day -> [MarketPrice]
declaredOrInferredPricesOn declaredprices inferredprices d =
  let
    -- keeping only prices on or before the valuation date, label each
    -- item with its same-day precedence (declared above inferred) and
    -- then parse order
    declaredprices' = [(1, i, p) | (i,p@MarketPrice{mpdate}) <- zip [1..] declaredprices, mpdate<=d]
    inferredprices' = [(0, i, p) | (i,p@MarketPrice{mpdate}) <- zip [1..] inferredprices, mpdate<=d]
  in
    -- combine
    declaredprices' ++ inferredprices'
    -- sort by newest date then highest precedence then latest parse order
    & sortBy (flip compare `on` (\(precedence,parseorder,mp)->(mpdate mp,precedence,parseorder)))
    -- discard the sorting labels
    & map third3
    -- keep only the first (ie the newest, highest precedence, latest parsed) price for each pair
    -- XXX or use a Map ?
    & nubSortBy (compare `on` (\(MarketPrice{..})->(mpfrom,mpto)))

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

tests_Valuation = tests "Valuation" [
   tests_priceLookup
  ]
