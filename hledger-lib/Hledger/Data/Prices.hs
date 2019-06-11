{-|

Convert amounts to some related value in various ways. This involves
looking up historical market prices (exchange rates) between commodities.

-}

{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Hledger.Data.Prices (
   amountValueAtDate
  ,amountApplyValuation
  ,mixedAmountValueAtDate
  ,mixedAmountApplyValuation
  ,priceLookup
  ,priceDirectiveToMarketPrice
  ,tests_Prices
)
where

import Control.Applicative ((<|>))
import Data.Decimal (roundTo)
import Data.Function (on)
import Data.Graph.Inductive  (Gr, Node, NodeMap, mkMapGraph, mkNode, lab, out, suc, sp)
import Data.List
import Data.List.Extra
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Text as T
import Data.Time.Calendar (Day)
import Safe (headMay)

import Hledger.Utils
import Hledger.Data.Types
import Hledger.Data.Amount
import Hledger.Data.Dates (parsedate)


tests_Prices = tests "Prices" [
   tests_priceLookup
  ]

------------------------------------------------------------------------------
-- Valuation
                     
-- Apply a specified valuation to this mixed amount, using the provided
-- prices db, commodity styles, period-end/current dates, 
-- and whether this is for a multiperiod report or not.
mixedAmountApplyValuation :: [PriceDirective] -> M.Map CommoditySymbol AmountStyle -> Day -> Day -> Bool -> ValuationType -> MixedAmount -> MixedAmount
mixedAmountApplyValuation prices styles periodend today ismultiperiod v (Mixed as) =
  Mixed $ map (amountApplyValuation prices styles periodend today ismultiperiod v) as

-- | Find the market value of each component amount in the given
-- commodity, or its default valuation commodity, at the given
-- valuation date, using the given market prices.
-- When market prices available on that date are not sufficient to
-- calculate the value, amounts are left unchanged.
mixedAmountValueAtDate :: [PriceDirective] -> M.Map CommoditySymbol AmountStyle -> Maybe CommoditySymbol -> Day -> MixedAmount -> MixedAmount
mixedAmountValueAtDate prices styles mc d (Mixed as) = Mixed $ map (amountValueAtDate prices styles mc d) as

-- | Apply a specified valuation to this amount, using the provided
-- prices db, commodity styles, period-end/current dates, 
-- and whether this is for a multiperiod report or not.
amountApplyValuation :: [PriceDirective] -> M.Map CommoditySymbol AmountStyle -> Day -> Day -> Bool -> ValuationType -> Amount -> Amount
amountApplyValuation prices styles periodend today ismultiperiod v a =
  case v of
    AtCost    Nothing            -> amountToCost styles a
    AtCost    mc                 -> amountValueAtDate prices styles mc periodend $ amountToCost styles a
    AtEnd     mc                 -> amountValueAtDate prices styles mc periodend a
    AtNow     mc                 -> amountValueAtDate prices styles mc today     a
    AtDefault mc | ismultiperiod -> amountValueAtDate prices styles mc periodend a
    AtDefault mc                 -> amountValueAtDate prices styles mc today     a
    AtDate d  mc                 -> amountValueAtDate prices styles mc d         a

-- | Find the market value of this amount in the given valuation
-- commodity if any, otherwise the default valuation commodity, at the
-- given valuation date. (The default valuation commodity is the
-- commodity of the latest applicable market price before the
-- valuation date.)
-- If the market prices available on that date are not sufficient to
-- calculate this value, the amount is left unchanged.
amountValueAtDate :: [PriceDirective] -> M.Map CommoditySymbol AmountStyle -> Maybe CommoditySymbol -> Day -> Amount -> Amount
amountValueAtDate pricedirectives styles mto d a =
  case priceLookup pricedirectives d (acommodity a) mto of
    Nothing           -> a
    Just (comm, rate) -> styleAmount styles $ amount{acommodity=comm, aquantity=rate * aquantity a}

------------------------------------------------------------------------------
-- Market price lookup
                     
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
  in tests "priceLookup" [
     priceLookup ps1 (d "1999/01/01") "A" Nothing    `is` Nothing
    ,priceLookup ps1 (d "2000/01/01") "A" Nothing    `is` Just ("B",10)
    ,priceLookup ps1 (d "2000/01/01") "B" (Just "A") `is` Just ("A",0.1)
    ,priceLookup ps1 (d "2000/01/01") "A" (Just "E") `is` Just ("E",500)
    ]

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
-- latest applicable market price, and converts to the commodity
-- mentioned in that price (default valuation commodity).
--
-- Note when calling this repeatedly for different periods, the
-- default valuation commodity can vary, since it depends on the
-- presence and parse order of market price declarations in each
-- period.
--
-- This returns the valuation commodity that was specified or
-- inferred, and the quantity of it that one unit of the source
-- commodity is worth. Or if no applicable market price or chain of
-- prices can be found, or the source commodity and the valuation
-- commodity are the same, returns Nothing.
--
-- A 'Prices' database (price graphs) is built each time this is
-- called, which is probably wasteful when looking up multiple prices
-- on the same day; it could be built at a higher level, or memoised.
--
priceLookup :: [PriceDirective] -> Day -> CommoditySymbol -> Maybe CommoditySymbol -> Maybe (CommoditySymbol, Quantity)
priceLookup pricedirectives d from mto =
  let
    -- build a graph of the commodity exchange rates in effect on this day
    -- XXX should hide these fgl details better
    Prices{prNodemap=m, prDeclaredPrices=g, prWithReversePrices=gr} = pricesAtDate pricedirectives d
    fromnode = node m from
    -- if to is unspecified, try to find a default valuation commodity based on available prices
    mto' = mto <|> mdefaultto
      where
        -- the default valuation commodity, if we could find one.
        -- XXX how to choose ? Take lowest sorted ?
        -- Take first, hoping current order is useful ?       <- 
        -- Keep parse order in label and take latest parsed ?
        mdefaultto = headMay (suc g fromnode) >>= lab g
  in
    case mto' of
      Nothing            -> Nothing
      Just to | to==from -> Nothing
      Just to            ->
        -- We have a commodity to convert to. Find the most direct price available.
        case
          -- These seem unnecessary, and we can avoid building one of the graphs
          -- mdeclaredprice <|> mreverseprice <|>
          mindirectprice of
            Nothing -> Nothing
            Just q  -> Just (to, q)
        where
          tonode = node m to
          -- mdeclaredprice :: Maybe Quantity =
          --   dbg ("declared market price "++T.unpack from++"->"++T.unpack to) $
          --   nodesEdgeLabel g (fromnode,tonode)
          -- mreverseprice  :: Maybe Quantity =
          --   dbg ("reverse  market price "++T.unpack from++"->"++T.unpack to) $
          --   ((1 /) <$> nodesEdgeLabel g (tonode,fromnode))
          mindirectprice :: Maybe Quantity =
            -- Find the shortest path, if any, between from and to.
            -- This time use gr which includes both declared and reverse prices.
            case sp fromnode tonode gr :: Maybe [Node] of
              Nothing    -> Nothing
              Just nodes ->
                dbg ("market price "++intercalate "->" (map T.unpack comms)) $
                Just $ product $ pathEdgeLabels gr nodes  -- convert to a single exchange rate
                where comms = catMaybes $ map (lab g) nodes

          -- log a message and a Maybe Quantity, hiding Just/Nothing and limiting decimal places
          dbg msg = dbg4With (((msg++": ")++) . maybe "" (show . roundTo 8))

-- | Convert a list of market price directives in parse order to
-- a database of market prices in effect on a given day,
-- allowing efficient lookup of exchange rates between commodity pairs.
pricesAtDate :: [PriceDirective] -> Day -> Prices
pricesAtDate pricedirectives d = Prices{
   prNodemap = m
  ,prDeclaredPrices = g
  ,prWithReversePrices = gr
  }
  where
    -- get the latest (before d) declared price for each commodity pair
    latestdeclaredprices :: [MarketPrice] =
      dbg5 "latestdeclaredprices" $
      nubSortBy (compare `on` (\(MarketPrice{..})->(mpfrom,mpto))) $  -- keep only the first (ie newest and latest parsed) price for each pair
      map snd $  -- discard the parse order label
      sortBy (flip compare `on` (\(parseorder,mp)->(mpdate mp,parseorder))) $  -- sort with newest dates and latest parse order first
      zip [1..] $  -- label with parse order
      map priceDirectiveToMarketPrice $
      filter ((<=d).pddate) pricedirectives  -- consider only price declarations up to the valuation date
    -- and the latest declared or reverse price for each commodity pair
    latestdeclaredandreverseprices =
      latestdeclaredprices `union` map marketPriceReverse latestdeclaredprices
        -- XXX hopefully this prioritises the declared prices, test
    allcomms = sort $ map mpfrom latestdeclaredandreverseprices
    (g :: PriceGraph, m :: NodeMap CommoditySymbol) = mkMapGraph
      (dbg5 "g nodelabels" allcomms) -- this must include all nodes mentioned in edges
      (dbg5 "g edges"      [(mpfrom, mpto, mprate) | MarketPrice{..} <- latestdeclaredprices])
    (gr, _) = mkMapGraph
      (dbg5 "gr nodelabels" allcomms) -- this must include all nodes mentioned in edges
      (dbg5 "gr edges"      [(mpfrom, mpto, mprate) | MarketPrice{..} <- latestdeclaredandreverseprices])

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
