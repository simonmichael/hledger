{-|
Lot calculation for investment tracking.

This module implements journalCalculateLots, which walks transactions in
date order, accumulating a map from commodities to their lots, and:

- For acquire postings: generates lot names from cost basis and appends
  them to the account name as subaccounts.

- For dispose postings: selects an existing lot subaccount matched by
  the posting's lot selector, using FIFO (oldest first).  If needed and
  if the lot selector permits it, selects multiple lots, splitting
  the posting into one per lot.
  Dispose postings are required to have a transacted cost (the selling price).

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
import Data.Maybe (fromMaybe, isNothing)
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Time.Calendar (Day)
import Text.Printf (printf)

import Hledger.Data.Amount (amountsRaw, mixedAmount, noCostFmt, showAmountWith)
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
-- Handles acquire postings (generating lot names as subaccounts) and
-- dispose postings (matching to existing lots using FIFO, splitting if needed).
-- Transfer postings are left unmodified for now.
journalCalculateLots :: Journal -> Either String Journal
journalCalculateLots j
  | not $ any (any isLotPosting . tpostings) txns = Right j
  | otherwise = do
      validateUserLabels txns
      let needsLabels = findDatesNeedingLabels txns
      (_, txns') <- foldM (processTransaction needsLabels) (M.empty, []) (sortOn tdate txns)
      Right $ journalTieTransactions $ j{jtxns = reverse txns'}
  where
    txns = jtxns j

-- | Check if a posting has any lot-related ptype tag.
isLotPosting :: Posting -> Bool
isLotPosting p = isAcquirePosting p || isDisposePosting p

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

-- | Process a single transaction: transform its acquire and dispose postings.
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
      | isDisposePosting p = do
          (st', newPs) <- processDisposePosting txnPos st p
          foldMPostings st' (reverse newPs ++ acc') ps
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

-- | Process a dispose posting: match to existing lots using FIFO,
-- split into multiple postings if the disposal spans multiple lots.
-- Returns the list of resulting postings (one per matched lot).
processDisposePosting :: SourcePos -> LotState -> Posting
                      -> Either String (LotState, [Posting])
processDisposePosting txnPos lotState p = do
    let lotAmts = [(a, cb) | a <- amountsRaw (pamount p), Just cb <- [acostbasis a]]
    (lotAmt, cb) <- case lotAmts of
      []  -> Left $ showPos ++ "dispose posting has no cost basis"
      [x] -> Right x
      _   -> Left $ showPos ++ "dispose posting has multiple cost basis amounts (not yet supported)"

    let commodity = acommodity lotAmt
        disposeQty = aquantity lotAmt

    when (isNothing (acost lotAmt)) $
      Left $ showPos ++ "dispose posting has no transacted cost (selling price) for " ++ T.unpack commodity

    when (disposeQty >= 0) $
      Left $ showPos ++ "dispose posting has non-negative quantity for " ++ T.unpack commodity

    let posQty = negate disposeQty
    selected <- selectLotsFIFO showPos commodity posQty cb lotState

    let mkPosting (lotId, _acctName, storedAmt, consumedQty) = do
          lotCost <- case acostbasis storedAmt >>= cbCost of
            Just c  -> Right c
            Nothing -> Left $ showPos ++ "lot " ++ T.unpack (T.pack (show lotId))
                                ++ " for commodity " ++ T.unpack commodity
                                ++ " has no cost basis (internal error)"
          let lotCb = CostBasis
                { cbDate  = Just (lotDate lotId)
                , cbLabel = lotLabel lotId
                , cbCost  = Just lotCost
                }
              lotName = showLotName lotCb
              -- Build the dispose amount: negative consumed quantity,
              -- keeping the original amount's commodity, style, cost, and cost basis.
              disposeAmt = lotAmt{aquantity = negate consumedQty, acostbasis = Just lotCb}
          Right p{ paccount = paccount p <> ":" <> lotName
                 , pamount  = mixedAmount disposeAmt
                 }

    newPostings <- mapM mkPosting selected
    let consumed = [(lotId, qty) | (lotId, _, _, qty) <- selected]
        lotState' = reduceLotState commodity consumed lotState

    return (lotState', newPostings)
  where
    showPos = sourcePosPretty txnPos ++ ":\n"

-- | Check if a posting is a dispose posting (has _ptype:dispose tag).
isDisposePosting :: Posting -> Bool
isDisposePosting p = ("_ptype", "dispose") `elem` ptags p

-- | Select lots to consume using FIFO (oldest first, across all accounts).
-- Given a commodity, a positive quantity to dispose, and a lot selector,
-- walks matching lots in key order (oldest first by date, then label).
-- The lot selector filters which lots are eligible: each non-Nothing field
-- in the selector must match the corresponding field in the lot's cost basis.
-- An all-Nothing selector (from @{}@) matches all lots.
-- Returns a list of (lot id, account name, lot amount, quantity consumed from this lot).
-- Errors if total available quantity in matching lots is insufficient.
selectLotsFIFO :: String -> CommoditySymbol -> Quantity -> CostBasis -> LotState
               -> Either String [(LotId, AccountName, Amount, Quantity)]
selectLotsFIFO posStr commodity qty selector lotState = do
    let allLots = M.findWithDefault M.empty commodity lotState
        matchingLots = M.filter (lotMatchesSelector selector) allLots
    when (M.null matchingLots) $
      Left $ posStr ++ "no lots available for commodity " ++ T.unpack commodity
    let available = sum [aquantity a | (_, a) <- M.elems matchingLots]
    when (available < qty) $
      Left $ posStr ++ "insufficient lots for commodity " ++ T.unpack commodity
              ++ ": need " ++ show qty ++ " but only " ++ show available ++ " available"
    Right $ go qty (M.toAscList matchingLots)
  where
    go 0 _ = []
    go _ [] = []  -- shouldn't happen after the check above
    go remaining ((lotId, (acctName, lotAmt)):rest)
      | remaining >= lotBal = (lotId, acctName, lotAmt, lotBal) : go (remaining - lotBal) rest
      | otherwise           = [(lotId, acctName, lotAmt, remaining)]
      where lotBal = aquantity lotAmt

-- | Does a lot match a lot selector?
-- Each non-Nothing field in the selector must match the lot's stored cost basis.
lotMatchesSelector :: CostBasis -> (AccountName, Amount) -> Bool
lotMatchesSelector selector (_, a) =
    case acostbasis a of
      Nothing    -> False
      Just lotCb -> matchCost (cbCost selector) (cbCost lotCb)
                 && matchField cbDate selector lotCb
                 && matchField cbLabel selector lotCb
  where
    matchField :: Eq b => (CostBasis -> Maybe b) -> CostBasis -> CostBasis -> Bool
    matchField f sel lot = case f sel of
      Nothing -> True   -- selector doesn't constrain this field
      Just v  -> f lot == Just v
    -- Compare costs by commodity and quantity, ignoring style differences.
    matchCost :: Maybe Amount -> Maybe Amount -> Bool
    matchCost Nothing    _          = True
    matchCost (Just _)   Nothing    = False
    matchCost (Just sel) (Just lot) = acommodity sel == acommodity lot
                                   && aquantity sel == aquantity lot

-- | Subtract consumed quantities from LotState. Removes lots whose balance reaches zero.
reduceLotState :: CommoditySymbol -> [(LotId, Quantity)] -> LotState -> LotState
reduceLotState commodity consumed = M.adjust adjustCommodity commodity
  where
    adjustCommodity lots = foldl' reduceLot lots consumed
    reduceLot lots (lotId, qty) = M.update shrink lotId lots
      where
        shrink (acctName, a)
          | aquantity a <= qty = Nothing  -- fully consumed, remove
          | otherwise          = Just (acctName, a{aquantity = aquantity a - qty})

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
