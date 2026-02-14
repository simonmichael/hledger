{-|
Lot calculation for investment tracking.

This module implements journalCalculateLots, which processes acquire postings
by generating lot names from cost basis and appending them as subaccounts.
It accumulates lot state (a map from commodities to their lots) as it walks
through transactions in date order.

For background, see doc\/SPEC-lots.md and doc\/PLAN-lots.md.
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Hledger.Data.Lots (
  journalCalculateLots,
  showLotName,
  -- LotState,
) where

import Control.Applicative ((<|>))
import Control.Monad (foldM, when)
import Data.List (sortOn)
#if !MIN_VERSION_base(4,20,0)
import Data.List (foldl')
#endif
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe)
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Time.Calendar (Day)
import Text.Printf (printf)

import Hledger.Data.Amount (amountsRaw, noCostFmt, showAmountWith)
import Hledger.Data.Journal (journalTieTransactions)
import Hledger.Data.Types
import Hledger.Utils (SourcePos, sourcePosPretty)

-- | Map from commodity to (map from lot id to (account name, lot balance)).
-- Keyed by commodity at the top level so lots of different commodities don't clash.
-- The inner Map LotId is ordered by date then label, supporting FIFO/LIFO naturally.
type LotState = M.Map CommoditySymbol (M.Map LotId (AccountName, Amount))

-- | Render a lot name in the consolidated hledger format for use as a subaccount name.
-- Format: @{YYYY-MM-DD, COST}@ or @{YYYY-MM-DD, \"LABEL\", COST}@.
showLotName :: CostBasis -> T.Text
showLotName CostBasis{cbDate, cbLabel, cbCost} =
  "{" <> datePart <> labelPart <> costPart <> "}"
  where
    datePart  = maybe "" (T.pack . show) cbDate
    labelPart = maybe "" (\l -> ", \"" <> l <> "\"") cbLabel
    costPart  = maybe "" (\a -> ", " <> T.pack (showAmountWith noCostFmt a)) cbCost

-- | Calculate detailed lot movements by walking transactions in date order.
-- Currently handles acquire postings only: generates lot names and appends
-- them as subaccounts. Dispose and transfer postings are left unmodified.
journalCalculateLots :: Journal -> Either String Journal
journalCalculateLots j
  | not $ any (any isAcquirePosting . tpostings) txns = Right j
  | otherwise = do
      validateUserLabels txns
      let needsLabels = findDatesNeedingLabels txns
      (_, txns') <- foldM (processTransaction needsLabels) (M.empty, []) (sortOn tdate txns)
      Right $ journalTieTransactions $ j{jtxns = reverse txns'}
  where
    txns = jtxns j

-- | Check if a posting is an acquire posting (has _ptype:acquire tag).
isAcquirePosting :: Posting -> Bool
isAcquirePosting p = ("_ptype", "acquire") `elem` ptags p

-- | Find (commodity, date) pairs that have multiple acquisitions and thus need labels.
-- Only counts acquisitions that don't already have a user-provided label.
findDatesNeedingLabels :: [Transaction] -> S.Set (CommoditySymbol, Day)
findDatesNeedingLabels txns =
    M.keysSet $ M.filter (> 1) counts
  where
    counts = foldl' countAcquire M.empty
      [ (acommodity a, getLotDate t cb)
      | t <- txns
      , p <- tpostings t
      , isAcquirePosting p
      , a <- amountsRaw (pamount p)
      , Just cb <- [acostbasis a]
      ]
    countAcquire m (c, d) = M.insertWith (+) (c, d) (1 :: Int) m

-- | Validate that user-provided labels don't create duplicate lot ids.
validateUserLabels :: [Transaction] -> Either String ()
validateUserLabels txns =
    case M.toList duplicates of
      [] -> Right ()
      (((c, d, l), _):_) -> Left $ "lot id is not unique: commodity " ++ T.unpack c
                              ++ ", date " ++ show d
                              ++ ", label \"" ++ T.unpack l ++ "\""
  where
    labeled = [ (acommodity a, getLotDate t cb, l)
              | t <- txns
              , p <- tpostings t
              , isAcquirePosting p
              , a <- amountsRaw (pamount p)
              , Just cb <- [acostbasis a]
              , Just l <- [cbLabel cb]
              ]
    counts = foldl' (\m k -> M.insertWith (+) k (1 :: Int) m) M.empty labeled
    duplicates = M.filter (> 1) counts

-- | Get the lot date from cost basis, falling back to the transaction date.
getLotDate :: Transaction -> CostBasis -> Day
getLotDate t cb = fromMaybe (tdate t) (cbDate cb)

-- | Process a single transaction: transform its acquire postings.
-- Accumulates (LotState, [Transaction]) — transactions in reverse order.
processTransaction :: S.Set (CommoditySymbol, Day) -> (LotState, [Transaction]) -> Transaction
                   -> Either String (LotState, [Transaction])
processTransaction needsLabels (ls, acc) t = do
    (ls', ps') <- foldMPostings ls [] (tpostings t)
    return (ls', t{tpostings = reverse ps'} : acc)
  where
    txnDate = tdate t
    txnPos = fst (tsourcepos t)

    foldMPostings :: LotState -> [Posting] -> [Posting] -> Either String (LotState, [Posting])
    foldMPostings st acc' [] = Right (st, acc')
    foldMPostings st acc' (p:ps)
      | isAcquirePosting p = do
          (st', p') <- processAcquirePosting needsLabels txnDate txnPos st p
          foldMPostings st' (p':acc') ps
      | otherwise =
          foldMPostings st (p:acc') ps

-- | Process a single acquire posting: generate a lot name and append it as a subaccount.
processAcquirePosting :: S.Set (CommoditySymbol, Day) -> Day -> SourcePos -> LotState -> Posting
                      -> Either String (LotState, Posting)
processAcquirePosting needsLabels txnDate txnPos lotState p = do
    let lotAmts = [(a, cb) | a <- amountsRaw (pamount p), Just cb <- [acostbasis a]]
    (lotAmt, cb) <- case lotAmts of
      []  -> Left $ showPos ++ "acquire posting has no cost basis"
      [x] -> Right x
      _   -> Left $ showPos ++ "acquire posting has multiple cost basis amounts (not yet supported)"

    let commodity = acommodity lotAmt
        date      = fromMaybe txnDate (cbDate cb)

    lotCost <- maybe (Left $ showPos ++ "acquire posting has no lot cost") Right (cbCost cb)

    let needsLabel = S.member (commodity, date) needsLabels
        lotLabel'  = cbLabel cb <|> if needsLabel then Just (generateLabel commodity date lotState) else Nothing
        lotId      = LotId date lotLabel'
        fullCb     = CostBasis{cbDate = Just date, cbLabel = lotLabel', cbCost = Just lotCost}
        lotName    = showLotName fullCb

    let existingLots = M.findWithDefault M.empty commodity lotState
    when (M.member lotId existingLots) $
      Left $ showPos ++ "duplicate lot id: " ++ T.unpack lotName
              ++ " for commodity " ++ T.unpack commodity

    let p' = p{paccount = paccount p <> ":" <> lotName}
    let lotState' = M.insertWith M.union commodity
                      (M.singleton lotId (paccount p, lotAmt)) lotState
    return (lotState', p')
  where
    showPos = sourcePosPretty txnPos ++ ":\n"

-- | Generate a label for a lot that needs one (due to same-date collision).
-- Uses a sequence number formatted as four (or more) digits: "0001", "0002", etc.
-- Uses O(log n) Map operations to count same-date lots efficiently.
generateLabel :: CommoditySymbol -> Day -> LotState -> T.Text
generateLabel commodity date lotState =
    T.pack $ printf "%04d" nextNum
  where
    existingLots = M.findWithDefault M.empty commodity lotState
    -- Use takeWhileAntitone/dropWhileAntitone for O(log n) range extraction.
    -- LotId is ordered by date first, so same-date lots form a contiguous range.
    sameDate = M.takeWhileAntitone (\(LotId d _) -> d == date)
             $ M.dropWhileAntitone (\(LotId d _) -> d < date) existingLots
    nextNum = M.size sameDate + 1 :: Int
