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

- For transfer postings: selects lots from the source account (like
  dispose) and recreates them under the destination account. The lot's
  cost basis is preserved through the transfer.
  Multi-lot transfers are supported (eg via @{}@ to transfer all lots).

For background, see doc\/SPEC-lots.md and doc\/PLAN-lots.md.
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Hledger.Data.Lots (
  journalCalculateLots,
  journalClassifyLotPostings,
  transactionClassifyLotPostings,
  showLotName,
  -- LotState,
) where

import Control.Applicative ((<|>))
import Control.Monad (foldM, guard, when)
import Data.List (sortOn)
#if !MIN_VERSION_base(4,20,0)
import Data.List (foldl')
#endif
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe, isJust, isNothing)
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time.Calendar (Day)
import Text.Printf (printf)

import Hledger.Data.AccountType (isAssetType)
import Hledger.Data.Amount (amountsRaw, isNegativeAmount, mapMixedAmount, mixedAmount, noCostFmt, showAmountWith)
import Hledger.Data.Journal (journalAccountType, journalCommodityUsesLots, journalMapTransactions, journalTieTransactions)
import Hledger.Data.Posting (originalPosting, postingAddHiddenAndMaybeVisibleTag)
import Hledger.Data.Types
import Hledger.Utils (SourcePos, dbg5, sourcePosPretty)

-- | Map from commodity to (map from lot id to (map from account name to lot balance)).
-- Keyed by commodity at the top level so lots of different commodities don't clash.
-- The inner Map LotId is ordered by date then label, supporting FIFO/LIFO naturally.
-- The innermost Map AccountName allows the same lot to exist at multiple accounts
-- (eg after a partial lot transfer).
type LotState = M.Map CommoditySymbol (M.Map LotId (M.Map AccountName Amount))

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
-- Handles acquire postings (generating lot names as subaccounts),
-- dispose postings (matching to existing lots using FIFO, splitting if needed),
-- and transfer postings (moving lots between accounts, preserving cost basis).
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
             || isTransferFromPosting p || isTransferToPosting p

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

-- | Process a single transaction: transform its acquire, dispose, and transfer postings.
-- Transfer pairs are processed first (so that transferred lots are available for
-- subsequent disposals in the same transaction), then acquire and dispose postings.
-- Accumulates (LotState, [Transaction]) — transactions in reverse order.
processTransaction :: S.Set (CommoditySymbol, Day) -> (LotState, [Transaction]) -> Transaction
                   -> Either String (LotState, [Transaction])
processTransaction needsLabels (ls, acc) t = do
    -- Partition postings into transfer pairs and others
    let (transferFroms, transferTos, otherPs) = partitionTransferPostings (tpostings t)
    pairs <- pairTransferPostings txnPos transferFroms transferTos
    -- Process transfer pairs first, accumulating lot state changes
    (ls', transferPs) <- foldM processOnePair (ls, []) pairs
    -- Process remaining postings (acquire, dispose, passthrough)
    (ls'', otherPs') <- foldMPostings ls' [] otherPs
    -- Combine: transfer postings first, then others, preserving relative order
    let allPs = reverse transferPs ++ reverse otherPs'
    return (ls'', t{tpostings = allPs} : acc)
  where
    txnDate = tdate t
    txnPos = fst (tsourcepos t)

    processOnePair (st, psAcc) (fromP, toP) = do
      (st', fromPs, toPs) <- processTransferPair txnPos st fromP toP
      return (st', reverse toPs ++ reverse fromPs ++ psAcc)

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

-- | Partition a transaction's postings into transfer-from, transfer-to, and others.
partitionTransferPostings :: [Posting] -> ([Posting], [Posting], [Posting])
partitionTransferPostings = go [] [] []
  where
    go froms tos others [] = (reverse froms, reverse tos, reverse others)
    go froms tos others (p:ps)
      | isTransferFromPosting p = go (p:froms) tos others ps
      | isTransferToPosting p   = go froms (p:tos) others ps
      | otherwise               = go froms tos (p:others) ps

-- | Pair transfer-from and transfer-to postings by commodity.
-- Within each commodity group, froms and tos are sorted by cost basis fields
-- (date, label, cost) so that explicit per-lot pairs align correctly even when
-- interleaved. Cost basis mismatches are caught later by validation, not here.
pairTransferPostings :: SourcePos -> [Posting] -> [Posting]
                     -> Either String [(Posting, Posting)]
pairTransferPostings _ [] [] = Right []
pairTransferPostings txnPos froms tos = do
    fromGroups <- groupByCommodity "transfer-from" froms
    toGroups   <- groupByCommodity "transfer-to" tos
    let allComms = S.union (M.keysSet fromGroups) (M.keysSet toGroups)
    concat <$> mapM (matchCommodityGroup fromGroups toGroups) (S.toList allComms)
  where
    showPos = sourcePosPretty txnPos ++ ":\n"

    -- Group postings by their lotful commodity (the one with cost basis).
    groupByCommodity :: String -> [Posting] -> Either String (M.Map CommoditySymbol [Posting])
    groupByCommodity label ps = do
      tagged <- mapM (\p -> (,p) <$> postingCommodity label p) ps
      Right $ M.map reverse $ M.fromListWith (++) [(c, [p]) | (c, p) <- tagged]

    postingCommodity :: String -> Posting -> Either String CommoditySymbol
    postingCommodity label p =
      case [acommodity a | a <- amountsRaw (pamount p), isJust (acostbasis a)] of
        [c] -> Right c
        -- Transfer-to postings without {} have no cost basis; use the raw commodity.
        _   -> case [acommodity a | a <- amountsRaw (pamount p)] of
                 [c] -> Right c
                 _   -> Left $ showPos ++ label ++ " posting has no lotful commodity"

    -- Sort key for aligning pairs within a commodity group.
    -- Extracts (date, label, normalized cost) from a posting's cost basis.
    postingSortKey :: Posting -> (Maybe Day, Maybe T.Text, Maybe (CommoditySymbol, Quantity))
    postingSortKey p =
      case [cb | a <- amountsRaw (pamount p), Just cb <- [acostbasis a]] of
        [cb] -> (cbDate cb, cbLabel cb,
                 fmap (\a -> (acommodity a, aquantity a)) (cbCost cb))
        _    -> (Nothing, Nothing, Nothing)

    matchCommodityGroup fromGroups toGroups comm = do
      let fs = M.findWithDefault [] comm fromGroups
          ts = M.findWithDefault [] comm toGroups
      case (fs, ts) of
        ([], _) -> Left $ showPos ++ "transfer-to posting for " ++ T.unpack comm
                            ++ " has no matching transfer-from posting"
        (_, []) -> Left $ showPos ++ "transfer-from posting for " ++ T.unpack comm
                            ++ " has no matching transfer-to posting"
        _ -> do
          when (length fs /= length ts) $
            Left $ showPos ++ "mismatched transfer postings for commodity " ++ T.unpack comm
                       ++ ": " ++ show (length fs) ++ " transfer-from but "
                       ++ show (length ts) ++ " transfer-to"
          let sortedFs = sortOn postingSortKey fs
              sortedTs = sortOn postingSortKey ts
          Right $ zip sortedFs sortedTs

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

    -- Check if the original (pre-balancing) amount had an explicit transacted cost.
    -- If so, {} with @ is an error; {} alone can infer cost basis from balancing.
    let origAmt = case poriginal p of
          Just orig -> case [a | a <- amountsRaw (pamount orig), acommodity a == commodity] of
                         (a:_) -> a
                         []    -> lotAmt
          Nothing   -> lotAmt

    lotBasis <- case cbCost cb of
      Just c  -> Right c
      Nothing
        | isJust (acost origAmt) -> Left $ showPos ++ "acquire posting has no lot cost"
        | otherwise -> case acost lotAmt of
            Just (UnitCost c)  -> Right c
            Just (TotalCost c) -> Right c{aquantity = aquantity c / aquantity lotAmt}
            _                  -> Left $ showPos ++ "acquire posting has no lot cost"

    let cbInferred = isNothing (cbCost cb)
        needsLabel = S.member (commodity, date) needsLabels
        lotLabel'  = cbLabel cb <|> if needsLabel then Just (generateLabel commodity date lotState) else Nothing
        lotId      = LotId date lotLabel'
        fullCb     = CostBasis{cbDate = Just date, cbLabel = lotLabel', cbCost = Just lotBasis}
        lotName    = showLotName fullCb
        -- When cost basis was inferred, fill it in on the user's original cb;
        -- the full cb (with date) is used only for the lot subaccount name and lot state key.
        filledCb   = cb{cbCost = Just lotBasis}
        lotAmt'    = if cbInferred then lotAmt{acostbasis = Just filledCb} else lotAmt

    let existingLots = M.findWithDefault M.empty commodity lotState
    when (M.member lotId existingLots) $
      Left $ showPos ++ "duplicate lot id: " ++ T.unpack lotName
              ++ " for commodity " ++ T.unpack commodity

    let p' = if cbInferred
             then let -- Update the original posting's cost basis too, so print shows {$50} not {}
                      origP  = originalPosting p
                      origP' = origP{pamount = mapMixedAmount updateCb $ pamount origP}
                      updateCb a | acommodity a == commodity = a{acostbasis = Just filledCb}
                                 | otherwise                 = a
                  in p{paccount = paccount p <> ":" <> lotName
                      ,pamount  = mixedAmount lotAmt'
                      ,poriginal = Just origP'}
             else p{paccount = paccount p <> ":" <> lotName}
    let lotState' = M.insertWith (M.unionWith M.union) commodity
                      (M.singleton lotId (M.singleton (paccount p) lotAmt')) lotState
    return (lotState', p')
  where
    showPos = sourcePosPretty txnPos ++ ":\n"

-- | Process a dispose posting: match to existing lots using FIFO,
-- split into multiple postings if the disposal spans multiple lots.
-- Returns the list of resulting postings (one per matched lot).
processDisposePosting :: SourcePos -> LotState -> Posting
                      -> Either String (LotState, [Posting])
processDisposePosting txnPos lotState p = do
    -- Extract lotful amount and lot selector. When cost basis is present, use it directly.
    -- When absent (bare dispose on a lotful commodity), use a wildcard selector.
    let lotAmts = [(a, cb) | a <- amountsRaw (pamount p), Just cb <- [acostbasis a]]
    (lotAmt, cb, isBare) <- case lotAmts of
      [x] -> Right (fst x, snd x, False)
      _   -> do
        let bareAmts = [a | a <- amountsRaw (pamount p), isNegativeAmount a]
        case bareAmts of
          [a] -> Right (a, CostBasis Nothing Nothing Nothing, True)
          _   -> Left $ showPos ++ "dispose posting has no cost basis"

    let commodity = acommodity lotAmt
        disposeQty = aquantity lotAmt

    when (isNothing (acost lotAmt)) $
      Left $ showPos ++ "dispose posting has no transacted cost (selling price) for " ++ T.unpack commodity

    when (disposeQty >= 0) $
      Left $ showPos ++ "dispose posting has non-negative quantity for " ++ T.unpack commodity

    let posQty = negate disposeQty
    selected <- selectLotsFIFO showPos Nothing commodity posQty cb lotState

    let mkPosting (lotId, storedAmt, consumedQty) = do
          lotBasis <- case acostbasis storedAmt >>= cbCost of
            Just c  -> Right c
            Nothing -> Left $ showPos ++ "lot " ++ T.unpack (T.pack (show lotId))
                                ++ " for commodity " ++ T.unpack commodity
                                ++ " has no cost basis (internal error)"
          let lotCb = CostBasis
                { cbDate  = Just (lotDate lotId)
                , cbLabel = lotLabel lotId
                , cbCost  = Just lotBasis
                }
              lotName = showLotName lotCb
              -- Build the dispose amount: negative consumed quantity,
              -- keeping the original amount's commodity, style, cost, and cost basis.
              disposeAmt = lotAmt{aquantity = negate consumedQty, acostbasis = Just lotCb}
          if isBare
            then do
              -- Normalize transacted cost to UnitCost and update poriginal
              -- so print shows the inferred cost basis and cost.
              let normalizedCost = case acost lotAmt of
                    Just (TotalCost c) ->
                      Just $ UnitCost c{aquantity = abs (aquantity c) / abs (aquantity lotAmt)}
                    other -> other
                  disposeAmt' = disposeAmt{acost = normalizedCost}
                  origP  = originalPosting p
                  origP' = origP{pamount = mapMixedAmount updateAmt $ pamount origP}
                  updateAmt a | acommodity a == commodity =
                                  a{acostbasis = Just lotCb, acost = normalizedCost}
                              | otherwise = a
              Right p{ paccount  = paccount p <> ":" <> lotName
                     , pamount   = mixedAmount disposeAmt'
                     , poriginal = Just origP'
                     }
            else
              Right p{ paccount = paccount p <> ":" <> lotName
                     , pamount  = mixedAmount disposeAmt
                     }

    newPostings <- mapM mkPosting selected
    let consumed = [(lotId, qty) | (lotId, _, qty) <- selected]
        lotState' = reduceLotState Nothing commodity consumed lotState

    return (lotState', newPostings)
  where
    showPos = sourcePosPretty txnPos ++ ":\n"

-- | Check if a posting is a dispose posting (has _ptype:dispose tag).
isDisposePosting :: Posting -> Bool
isDisposePosting p = ("_ptype", "dispose") `elem` ptags p

-- | Check if a posting is a transfer-from posting (has _ptype:transfer-from tag).
isTransferFromPosting :: Posting -> Bool
isTransferFromPosting p = ("_ptype", "transfer-from") `elem` ptags p

-- | Check if a posting is a transfer-to posting (has _ptype:transfer-to tag).
isTransferToPosting :: Posting -> Bool
isTransferToPosting p = ("_ptype", "transfer-to") `elem` ptags p

-- | Process a transfer pair: select lots from the source account (transfer-from)
-- and recreate them under the destination account (transfer-to).
-- Returns updated LotState and two lists of expanded postings (from, to).
processTransferPair :: SourcePos -> LotState -> Posting -> Posting
                    -> Either String (LotState, [Posting], [Posting])
processTransferPair txnPos lotState fromP toP = do
    -- Extract lotful amount and lot selector from transfer-from
    let fromAmts = [(a, cb) | a <- amountsRaw (pamount fromP), Just cb <- [acostbasis a]]
    (fromAmt, fromCb) <- case fromAmts of
      []  -> Left $ showPos ++ "transfer-from posting has no cost basis"
      [x] -> Right x
      _   -> Left $ showPos ++ "transfer-from posting has multiple cost basis amounts (not yet supported)"

    let commodity = acommodity fromAmt
        transferQty = aquantity fromAmt

    let toAmts = amountsRaw (pamount toP)

    -- Check that neither transfer posting has explicit transacted cost (@ or @@).
    -- Use originalPosting to distinguish user-written @ from pipeline-inferred acost.
    let origFromAmts = amountsRaw $ pamount $ originalPosting fromP
        origToAmts   = amountsRaw $ pamount $ originalPosting toP
    when (any (isJust . acost) origFromAmts || any (isJust . acost) origToAmts) $
      Left $ showPos ++ "lot transfers should have no transacted cost"

    -- Validate transfer-from has negative quantity
    when (transferQty >= 0) $
      Left $ showPos ++ "transfer-from posting has non-negative quantity for " ++ T.unpack commodity

    let posQty = negate transferQty

    -- Select lots from source using FIFO
    selected <- selectLotsFIFO showPos (Just (paccount fromP)) commodity posQty fromCb lotState

    -- Extract the transfer-to cost basis for optional validation
    let toCb = case [(a, cb) | a <- toAmts, Just cb <- [acostbasis a]] of
                 [(_, cb)] -> Just cb
                 _         -> Nothing

    -- For each selected lot, generate from and to postings
    (fromPs, toPs) <- fmap unzip $ mapM (mkTransferPostings fromAmt toCb commodity) selected

    -- Update lot state: remove from source, add to destination
    let consumed = [(lotId, qty) | (lotId, _, qty) <- selected]
        lotState' = reduceLotState (Just (paccount fromP)) commodity consumed lotState
        lotState'' = foldl' (addTransferredLot commodity (paccount toP)) lotState' selected

    return (lotState'', fromPs, toPs)
  where
    showPos = sourcePosPretty txnPos ++ ":\n"

    mkTransferPostings fromAmt toCb commodity (lotId, storedAmt, consumedQty) = do
        lotBasis <- case acostbasis storedAmt >>= cbCost of
          Just c  -> Right c
          Nothing -> Left $ showPos ++ "lot " ++ show lotId
                              ++ " for commodity " ++ T.unpack commodity
                              ++ " has no cost basis (internal error)"
        let lotCb = CostBasis
              { cbDate  = Just (lotDate lotId)
              , cbLabel = lotLabel lotId
              , cbCost  = Just lotBasis
              }
            lotName = showLotName lotCb

        -- Validate transfer-to cost basis if it has specific fields
        validateToCb toCb lotCb commodity

        let fromAmt' = fromAmt{aquantity = negate consumedQty, acostbasis = Just lotCb}
            toAmt'   = fromAmt'{aquantity = consumedQty}
            fromP' = fromP{ paccount = paccount fromP <> ":" <> lotName
                          , pamount  = mixedAmount fromAmt'
                          }
            toP'   = toP{ paccount = paccount toP <> ":" <> lotName
                        , pamount  = mixedAmount toAmt'
                        }
        Right (fromP', toP')

    -- Validate that transfer-to cost basis (if specified with concrete fields)
    -- matches the lot's cost basis.
    validateToCb Nothing _ _ = Right ()
    validateToCb (Just toCb') lotCb commodity = do
      case cbCost toCb' of
        Just toCost | Just lotBasis <- cbCost lotCb ->
          when (acommodity toCost /= acommodity lotBasis || aquantity toCost /= aquantity lotBasis) $
            Left $ showPos ++ "lot cost basis " ++ T.unpack (showLotName lotCb)
                     ++ " does not match transfer-to cost basis " ++ T.unpack (showLotName toCb')
                     ++ " for commodity " ++ T.unpack commodity
        _ -> Right ()

    -- Re-add a transferred lot to LotState under the destination account.
    addTransferredLot commodity destAcct ls (lotId, storedAmt, consumedQty) =
      let amt = storedAmt{aquantity = consumedQty}
      in M.insertWith (M.unionWith M.union) commodity
           (M.singleton lotId (M.singleton destAcct amt)) ls

-- | Select lots to consume using FIFO (oldest first).
-- When account is @Nothing@, selects globally across all accounts (for disposals).
-- When account is @Just acct@, selects only from that account (for transfers).
-- The lot selector filters which lots are eligible: each non-Nothing field
-- in the selector must match the corresponding field in the lot's cost basis.
-- An all-Nothing selector (from @{}@) matches all lots.
-- Returns a list of (lot id, lot amount, quantity consumed from this lot).
-- Errors if total available quantity in matching lots is insufficient.
selectLotsFIFO :: String -> Maybe AccountName -> CommoditySymbol -> Quantity -> CostBasis -> LotState
               -> Either String [(LotId, Amount, Quantity)]
selectLotsFIFO posStr maccount commodity qty selector lotState = do
    let allLots = M.findWithDefault M.empty commodity lotState
        -- Flatten to (LotId, Amount) pairs, optionally filtering by account.
        -- For global selection, sum quantities across accounts per lot.
        -- For per-account selection, take only the specified account's balance.
        flatLots = case maccount of
          Nothing   -> M.mapMaybe pickAny allLots
          Just acct -> M.mapMaybe (M.lookup acct) allLots
        matchingLots = M.filter (lotMatchesSelector selector) flatLots
    when (M.null matchingLots) $
      Left $ posStr ++ "no lots available for commodity " ++ T.unpack commodity
    let available = sum [aquantity a | a <- M.elems matchingLots]
    when (available < qty) $
      Left $ posStr ++ "insufficient lots for commodity " ++ T.unpack commodity
              ++ ": need " ++ show qty ++ " but only " ++ show available ++ " available"
    Right $ go qty (M.toAscList matchingLots)
  where
    -- For global selection, pick any account's Amount as representative
    -- (they all share the same cost basis) and sum the total quantity.
    pickAny acctMap = case M.elems acctMap of
      []    -> Nothing
      (a:_) -> Just a{aquantity = sum [aquantity x | x <- M.elems acctMap]}

    go 0 _ = []
    go _ [] = []  -- shouldn't happen after the check above
    go remaining ((lotId, lotAmt):rest)
      | remaining >= lotBal = (lotId, lotAmt, lotBal) : go (remaining - lotBal) rest
      | otherwise           = [(lotId, lotAmt, remaining)]
      where lotBal = aquantity lotAmt

-- | Does a lot match a lot selector?
-- Each non-Nothing field in the selector must match the lot's stored cost basis.
lotMatchesSelector :: CostBasis -> Amount -> Bool
lotMatchesSelector selector a =
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

-- | Subtract consumed quantities from LotState.
-- When account is @Nothing@, reduces globally across all accounts for each lot
-- (consuming from accounts in map order until the quantity is fulfilled).
-- When account is @Just acct@, reduces only from that specific account.
-- Removes lot-account entries whose balance reaches zero.
-- Removes the lot entirely if no accounts remain.
reduceLotState :: Maybe AccountName -> CommoditySymbol -> [(LotId, Quantity)] -> LotState -> LotState
reduceLotState maccount commodity consumed = M.adjust adjustCommodity commodity
  where
    adjustCommodity lots = foldl' reduceLot lots consumed
    reduceLot lots (lotId, qty) = M.update shrinkLot lotId lots
      where
        shrinkLot acctMap =
          let acctMap' = case maccount of
                Just acct -> M.update (shrinkAmt qty) acct acctMap
                Nothing   -> reduceAcrossAccounts qty acctMap
          in if M.null acctMap' then Nothing else Just acctMap'
        shrinkAmt q a
          | aquantity a <= q = Nothing
          | otherwise        = Just a{aquantity = aquantity a - q}
        -- Reduce across all accounts, consuming from each in map order.
        reduceAcrossAccounts 0 m = m
        reduceAcrossAccounts remaining m = case M.minViewWithKey m of
          Nothing -> m  -- shouldn't happen
          Just ((acct, a), rest)
            | aquantity a <= remaining -> reduceAcrossAccounts (remaining - aquantity a) rest
            | otherwise -> M.insert acct a{aquantity = aquantity a - remaining} rest

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

-- | Classify lot-related postings by adding ptype tags.
-- Must be called after journalAddAccountTypes so account types are available.
-- The verbosetags parameter controls whether the ptype tags will be made visible in comments.
journalClassifyLotPostings :: Bool -> Journal -> Journal
journalClassifyLotPostings verbosetags j = journalMapTransactions (transactionClassifyLotPostings verbosetags lookupType commodityIsLotful) j
  where
    lookupType = journalAccountType j
    commodityIsLotful = journalCommodityUsesLots j

-- | Classify lot-related postings by adding a ptype tag.
-- For each posting with a cost basis (any account type):
-- - determine if it's of type "acquire" or "dispose" (based on amount sign)
-- - or "transfer-from" or "transfer-to" (if counterposting with same commodity exists in a different account).
-- For asset postings without cost basis:
-- - a lotful posting can be classified as "transfer-to" (see shouldClassifyLotful)
-- - a bare positive posting can be classified as "transfer-to" if there's a
--   matching transfer-from counterpart (see shouldClassifyBareTransferTo).
-- The lookupAccountType function should typically be `journalAccountType journal`.
-- The commodityIsLotful function should typically be `journalCommodityUsesLots journal`.
-- The verbosetags parameter controls whether the tags are made visible in comments.
transactionClassifyLotPostings :: Bool -> (AccountName -> Maybe AccountType) -> (CommoditySymbol -> Bool) -> Transaction -> Transaction
transactionClassifyLotPostings verbosetags lookupAccountType commodityIsLotful t@Transaction{tpostings=ps}
  | not (any hasLotRelevantAmount ps) = dbg5 "classifyLotPostings: no lot-relevant amounts found, skipping" t
  | otherwise = dbg5 "classifyLotPostings: classifying" $ t{tpostings=map classifyPosting ps}
  where
    hasCostBasis :: Posting -> Bool
    hasCostBasis p = let amts = amountsRaw (pamount p)
                         result = any (isJust . acostbasis) amts
                     in dbg5 ("classifyLotPostings: hasCostBasis " ++ show (paccount p) ++ " amts=" ++ show (length amts)) result

    hasLotRelevantAmount :: Posting -> Bool
    hasLotRelevantAmount p = hasCostBasis p || hasNegativeLotfulAmount p

    hasNegativeLotfulAmount :: Posting -> Bool
    hasNegativeLotfulAmount p =
      let amts = amountsRaw (pamount p)
      in not (any (isJust . acostbasis) amts)
         && any isNegativeAmount amts
         && postingIsLotful p amts

    -- Precompute per-commodity transfer counterpart info in a single pass over all postings (O(n)).
    -- For each commodity, which accounts have:
    --   negative postings with cost basis  (transfer-from candidates; any account type)
    --   positive postings with cost basis  (transfer-to candidates, standard path; any account type)
    --   positive asset postings without cost basis  (transfer-to candidates, bare path; asset only)
    -- Account lists are deduplicated (typically 1-2 accounts per commodity).
    negCBAccts, posCBAccts, posNoCBAccts :: M.Map CommoditySymbol [AccountName]
    (negCBAccts, posCBAccts, posNoCBAccts) = foldl' collect (M.empty, M.empty, M.empty) ps
      where
        collect (!neg, !pos, !noCB) p =
              let isAsset  = maybe False isAssetType (lookupAccountType (paccount p))
                  amts     = amountsRaw (pamount p)
                  acct     = paccount p
                  isNeg    = any isNegativeAmount amts
                  hasCB    = any (isJust . acostbasis) amts
                  cbComms  = [acommodity a | a <- amts, isJust (acostbasis a)]
                  allComms = [acommodity a | a <- amts]
                  neg'     = if isNeg && hasCB
                             then foldl' (addAcct acct) neg cbComms else neg
                  pos'     = if not isNeg && hasCB
                             then foldl' (addAcct acct) pos cbComms else pos
                  noCB'    = if not isNeg && isAsset && not hasCB
                             then foldl' (addAcct acct) noCB allComms else noCB
              in (neg', pos', noCB')
        -- Add an account to a commodity's account list, deduplicating.
        -- The lists are short (bounded by distinct accounts, typically 1-2).
        addAcct acct m c = M.insertWith (\_ old -> if acct `elem` old then old else acct : old) c [acct] m

    -- Is there a transfer counterpart for a posting in this account, with this sign,
    -- in this commodity?  O(k) where k = distinct accounts per commodity (typically 1-2).
    hasCounterpart :: AccountName -> Bool -> CommoditySymbol -> Bool
    hasCounterpart acct isNeg c
      | isNeg     = anyOtherAcct posCBAccts || anyOtherAcct posNoCBAccts
      | otherwise = anyOtherAcct negCBAccts
      where anyOtherAcct m = any (/= acct) (M.findWithDefault [] c m)

    -- Is there a transfer-from counterpart (negative asset with cost basis) for this commodity?
    hasTransferFromCounterpart :: AccountName -> CommoditySymbol -> Bool
    hasTransferFromCounterpart acct c = any (/= acct) (M.findWithDefault [] c negCBAccts)

    classifyPosting :: Posting -> Posting
    classifyPosting p =
      case dbg5 ("classifyLotPostings: classifyPosting " ++ show (paccount p) ++ " result") $ shouldClassify p of
        Nothing -> p
        Just classification ->
          let tag = ("ptype", classification)
          in postingAddHiddenAndMaybeVisibleTag verbosetags (toHiddenTag tag) p

    -- Check if posting should be classified and return the classification:
    -- one of acquire, dispose, transfer-from, transfer-to.
    shouldClassify :: Posting -> Maybe Text
    shouldClassify p = do
      let amts = amountsRaw $ pamount p
      if any (isJust . acostbasis) amts
        -- Cost basis present: classify regardless of account type (fix A)
        then dbg5 ("classifyLotPostings: shouldClassify " ++ show (paccount p) ++ " withCostBasis") $
             shouldClassifyWithCostBasis p amts
        else do
          -- No cost basis: require asset account type
          acctType <- dbg5 ("classifyLotPostings: shouldClassify " ++ show (paccount p) ++ " acctType") $
                      lookupAccountType (paccount p)
          guard $ isAssetType acctType
          dbg5 ("classifyLotPostings: shouldClassify " ++ show (paccount p) ++ " lotful/bare") $
            shouldClassifyNegativeLotful p amts <|> shouldClassifyLotful p amts <|> shouldClassifyBareTransferTo p amts

    -- Classify a posting that has cost basis: acquire, dispose, transfer-from, or transfer-to.
    shouldClassifyWithCostBasis :: Posting -> [Amount] -> Maybe Text
    shouldClassifyWithCostBasis p amts = do
      let
        isNeg = any isNegativeAmount amts
        primaryType = if isNeg then "dispose" else "acquire"
        pCommodities = [acommodity a | a <- amts, isJust (acostbasis a)]
        isTransfer = any (hasCounterpart (paccount p) isNeg) pCommodities
      return $ if isTransfer
        then if isNeg then "transfer-from" else "transfer-to"
        else primaryType

    -- Classify a negative lotful posting without cost basis as dispose or transfer-from.
    shouldClassifyNegativeLotful :: Posting -> [Amount] -> Maybe Text
    shouldClassifyNegativeLotful p amts = do
      guard $ postingIsLotful p amts
      guard $ any isNegativeAmount amts
      let pCommodities = [acommodity a | a <- amts]
          isTransfer = any (hasCounterpart (paccount p) True) pCommodities
      return $ if isTransfer then "transfer-from" else "dispose"

    -- Classify a lotful posting without cost basis.
    -- A positive posting in a lotful commodity/account, with no transacted cost,
    -- and with a matching transfer-from counterpart, is classified as transfer-to.
    shouldClassifyLotful :: Posting -> [Amount] -> Maybe Text
    shouldClassifyLotful p amts = do
      guard $ postingIsLotful p amts
      guard $ not $ any isNegativeAmount amts
      guard $ not $ any (isJust . acost) amts
      let pCommodities = [acommodity a | a <- amts]
      guard $ any (hasTransferFromCounterpart (paccount p)) pCommodities
      return "transfer-to"

    -- Classify a bare positive asset posting (no cost basis, not necessarily lotful)
    -- as transfer-to if there's a matching transfer-from counterpart (fix C).
    shouldClassifyBareTransferTo :: Posting -> [Amount] -> Maybe Text
    shouldClassifyBareTransferTo p amts = do
      guard $ not $ any isNegativeAmount amts
      let pCommodities = [acommodity a | a <- amts]
      guard $ any (hasTransferFromCounterpart (paccount p)) pCommodities
      return "transfer-to"

    -- Check if a posting is lotful: its commodity or account has a lots: tag.
    postingIsLotful :: Posting -> [Amount] -> Bool
    postingIsLotful p amts =
      any ((== "lots") . T.toLower . fst) (ptags p)  -- account lots: tag (inherited via ptags)
      || any (commodityIsLotful . acommodity) amts     -- commodity lots: tag
