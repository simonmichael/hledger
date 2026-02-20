{-|
Lot tracking for investment accounting.

This module implements two pipeline stages (see doc\/SPEC-finalising.md):

1. Classification ('journalClassifyLotPostings'): identifies lot postings
   and tags them as acquire, dispose, transfer-from, or transfer-to.

2. Calculation ('journalCalculateLots'): walks transactions in date order,
   accumulating a map from commodities to their lots, and:

   - For acquire postings: generates lot names from cost basis and appends
     them to the account name as subaccounts.

   - For dispose postings: selects an existing lot subaccount matched by
     the posting's lot selector, using the configured reduction method
     (FIFO by default, configurable per account\/commodity via @lots:@ tag).
     If needed and if the lot selector permits it, selects multiple lots,
     splitting the posting into one per lot.
     Dispose postings are required to have a transacted price (the selling price).

   - For transfer postings: selects lots from the source account (like
     dispose) and recreates them under the destination account. The lot's
     cost basis is preserved through the transfer.
     Multi-lot transfers are supported (eg via @{}@ to transfer all lots).

For background, see doc\/SPEC-lots.md and doc\/PLAN-lots.md.

== Errors

User-visible errors from this module. See also Hledger.Data.Errors and doc/ERRORS.md.

journalCalculateLots:

* validateUserLabels:
  "lot id is not unique: commodity X, date D, label L"

* processAcquirePosting:
  "acquire posting has no cost basis",
  "...has multiple cost basis amounts",
  "...has no lot cost",
  "duplicate lot id: {...} for commodity X"

* processDisposePosting:
  "dispose posting has no cost basis",
  "...has no transacted price (selling price)",
  "...has non-negative quantity",
  "SPECID requires a lot selector",
  "lot ... has no cost basis (internal error)",
  "lot subaccount ... does not match resolved lot"

* pairTransferPostings:
  "transfer-to/from posting ... has no matching ... posting",
  "mismatched transfer postings for commodity X",
  "... posting has no lotful commodity"

* processTransferPair:
  "transfer-from posting has no cost basis",
  "...has multiple cost basis amounts",
  "lot transfers should have no transacted price",
  "transfer-from posting has non-negative quantity",
  "lot ... has no cost basis (internal error)",
  "lot cost basis ... does not match transfer-to cost basis"

* selectLots:
  "SPECID requires an explicit lot selector",
  "no lots available for commodity X",
  "lot selector is ambiguous, matches N lots",
  "insufficient lots for commodity X"

* poolWeightedAvgCost:
  "no lots with cost basis available for averaging",
  "cannot average lots with different cost commodities"

* journalInferAndCheckDisposalBalancing:
  "This disposal transaction has multiple amountless gain postings",
  "This disposal transaction is unbalanced at cost basis"

-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Hledger.Data.Lots (
  journalClassifyLotPostings,
  journalCalculateLots,
  journalInferAndCheckDisposalBalancing,
  isGainPosting,
  showLotName,
) where

import Control.Applicative ((<|>))
import Control.Monad (foldM, guard, unless, when)
import Data.List (sort, sortOn)
import Data.Ord (Down(..))
#if !MIN_VERSION_base(4,20,0)
import Data.List (foldl')
#endif
import Data.Map.Strict qualified as M
import Data.Maybe (catMaybes, fromMaybe, isJust, isNothing)
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time.Calendar (Day)
import Text.Printf (printf)

import Hledger.Data.AccountName (accountNameType)
import Hledger.Data.AccountType (isAssetType)
import Hledger.Data.Amount (amountsRaw, isNegativeAmount, maNegate, mapMixedAmount, mixedAmount, mixedAmountLooksZero, nullmixedamt, noCostFmt, showAmountWith, showMixedAmountOneLine)
import Hledger.Data.Errors (makeTransactionErrorExcerpt)
import Hledger.Data.Journal (journalAccountType, journalCommodityLotsMethod, journalCommodityUsesLots, journalMapTransactions, journalTieTransactions, postingLotsMethod)
import Hledger.Data.Posting (hasAmount, nullposting, originalPosting, postingAddHiddenAndMaybeVisibleTag)
import Hledger.Data.Transaction (txnTieKnot)
import Hledger.Data.Types
import Hledger.Utils (dbg5)

-- Types

-- | Map from commodity to (map from lot id to (map from account name to lot balance)).
-- Keyed by commodity at the top level so lots of different commodities don't clash.
-- The inner Map LotId is ordered by date then label, supporting FIFO/LIFO naturally.
-- The innermost Map AccountName allows the same lot to exist at multiple accounts
-- (eg after a partial lot transfer).
type LotState = M.Map CommoditySymbol (M.Map LotId (M.Map AccountName Amount))

-- | Resolve which reduction method to use for a posting.
-- Checks the posting's account tags first (inherited via ptags), then commodity tags, defaulting to FIFO.
resolveReductionMethod :: Journal -> Posting -> CommoditySymbol -> ReductionMethod
resolveReductionMethod j p commodity =
  fromMaybe FIFO $
    postingLotsMethod p
    <|> journalCommodityLotsMethod j commodity

-- | Whether a reduction method uses per-account scope (FIFO1/LIFO1/HIFO1/AVERAGE1) vs global.
methodIsPerAccount :: ReductionMethod -> Bool
methodIsPerAccount FIFO1    = True
methodIsPerAccount LIFO1    = True
methodIsPerAccount HIFO1    = True
methodIsPerAccount AVERAGE1 = True
methodIsPerAccount _        = False

-- | Strip any trailing lot subaccount (a component starting with '{') from an account name.
-- E.g., @\"assets:broker:{2026-01-15, $50}\"@ becomes @\"assets:broker\"@.
-- Handles the case where a user writes the lot subaccount explicitly in a journal entry.
lotBaseAccount :: AccountName -> AccountName
lotBaseAccount a =
  let (prefix, lastComp) = T.breakOnEnd ":" a
  in if not (T.null prefix) && "{" `T.isPrefixOf` lastComp
     then T.init prefix  -- strip the trailing ':'
     else a

-- | Render a lot name in the consolidated hledger format for use as a subaccount name.
-- Format: @{YYYY-MM-DD, COST}@ or @{YYYY-MM-DD, \"LABEL\", COST}@.
showLotName :: CostBasis -> T.Text
showLotName CostBasis{cbDate, cbLabel, cbCost} =
  "{" <> T.intercalate ", " parts <> "}"
  where
    parts = catMaybes
      [ fmap (T.pack . show) cbDate
      , fmap (\l -> "\"" <> l <> "\"") cbLabel
      , fmap (T.pack . showAmountWith noCostFmt) cbCost
      ]

-- Classification (pipeline stage 1)

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
  | not (any hasLotRelevantAmount ps) && not (any isGainAcct ps)
    = dbg5 "classifyLotPostings: no lot-relevant amounts found, skipping" t
  | otherwise = dbg5 "classifyLotPostings: classifying" $ t{tpostings=zipWith classifyAt [0..] ps}
  where
    hasCostBasis :: Posting -> Bool
    hasCostBasis p = let amts = amountsRaw (pamount p)
                         result = any (isJust . acostbasis) amts
                     in dbg5 ("classifyLotPostings: hasCostBasis " ++ show (paccount p) ++ " amts=" ++ show (length amts)) result

    hasLotRelevantAmount :: Posting -> Bool
    hasLotRelevantAmount p = isReal p && (hasCostBasis p || hasNegativeLotfulAmount p || hasPositiveLotfulAmount p)

    isReal :: Posting -> Bool
    isReal p = preal p == RealPosting

    hasNegativeLotfulAmount :: Posting -> Bool
    hasNegativeLotfulAmount p =
      let amts = amountsRaw (pamount p)
      in not (any (isJust . acostbasis) amts)
         && any isNegativeAmount amts
         && postingIsLotful p amts

    hasPositiveLotfulAmount :: Posting -> Bool
    hasPositiveLotfulAmount p =
      let amts = amountsRaw (pamount p)
      in not (any (isJust . acostbasis) amts)
         && not (any isNegativeAmount amts)
         && any (\a -> aquantity a > 0) amts  -- has a strictly positive amount (not zero or amountless)
         && postingIsLotful p amts

    -- Same-account transfer pairs: within each account, match positive and negative
    -- postings with the same commodity and absolute quantity as transfer pairs.
    -- When there are more of one sign than the other, the excess are left unmatched
    -- (and will be classified normally as acquire/dispose).
    sameAcctTransferSet :: S.Set Int
    sameAcctTransferSet = S.fromList $ concatMap matchPairs $ M.elems grouped
      where
        grouped :: M.Map (AccountName, CommoditySymbol, Quantity) ([Int], [Int])
        grouped = foldl' addPosting M.empty (zip [0..] ps)
        addPosting m (i, p)
          | not (isReal p) = m
          | not (hasLotRelevantAmount p) = m
          | otherwise = foldl' (addAmt i (paccount p)) m (amountsRaw (pamount p))
        addAmt i acct m a
          | q < 0     = M.insertWith mergePair (acct, acommodity a, negate q) ([i], []) m
          | q > 0     = M.insertWith mergePair (acct, acommodity a, q)        ([], [i]) m
          | otherwise = m
          where q = aquantity a
        mergePair (n1, p1) (n2, p2) = (n1++n2, p1++p2)
        matchPairs (negs, poss) =
          let n = min (length negs) (length poss)
          in take n negs ++ take n poss

    -- Precompute per-commodity, per-quantity transfer counterpart info (O(n)).
    -- Keyed by (commodity, |quantity|) so that transfer detection requires exact quantity matching.
    -- For each key, which accounts have:
    --   negative postings with cost basis  (transfer-from candidates; any account type)
    --   positive postings with cost basis  (transfer-to candidates, standard path; any account type)
    --   positive asset postings without cost basis  (transfer-to candidates, bare path; asset only)
    -- Account lists are deduplicated (typically 1-2 accounts per commodity).
    negCBAccts, posCBAccts, posNoCBAccts :: M.Map (CommoditySymbol, Quantity) [AccountName]
    (negCBAccts, posCBAccts, posNoCBAccts) = foldl' collect (M.empty, M.empty, M.empty) (zip [0..] ps)
      where
        collect (!neg, !pos, !noCB) (i, p)
              | not (isReal p) = (neg, pos, noCB)  -- skip virtual postings
              | i `S.member` sameAcctTransferSet = (neg, pos, noCB)  -- skip same-account transfer pairs
              | otherwise =
              let isAsset  = maybe False isAssetType (lookupAccountType (paccount p))
                  amts     = amountsRaw (pamount p)
                  acct     = paccount p
                  isNeg    = any isNegativeAmount amts
                  hasCB    = any (isJust . acostbasis) amts
                  isLotful = postingIsLotful p amts
                  cbKeys   = [(acommodity a, abs (aquantity a)) | a <- amts, isJust (acostbasis a)]
                  allKeys  = [(acommodity a, abs (aquantity a)) | a <- amts]
                  -- Include both cost-basis and bare lotful negatives, so
                  -- hasTransferFromCounterpart sees bare transfer-from postings.
                  neg'     = if isNeg && (hasCB || isLotful)
                             then foldl' (addAcct acct) neg (if hasCB then cbKeys else allKeys) else neg
                  pos'     = if not isNeg && hasCB
                             then foldl' (addAcct acct) pos cbKeys else pos
                  noCB'    = if not isNeg && isAsset && not hasCB
                             then foldl' (addAcct acct) noCB allKeys else noCB
              in (neg', pos', noCB')
        -- Add an account to a (commodity, quantity) key's account list, deduplicating.
        addAcct acct m k = M.insertWith (\_ old -> if acct `elem` old then old else acct : old) k [acct] m

    -- Is there a transfer counterpart for a posting in this account, with this sign,
    -- in this commodity and quantity?
    hasCounterpart :: AccountName -> Bool -> CommoditySymbol -> Quantity -> Bool
    hasCounterpart acct isNeg c q
      | isNeg     = anyOtherAcct posCBAccts || anyOtherAcct posNoCBAccts
      | otherwise = anyOtherAcct negCBAccts
      where anyOtherAcct m = any (/= acct) (M.findWithDefault [] (c, abs q) m)

    -- Is there a transfer-from counterpart (negative with cost basis or lotful)
    -- for this commodity and quantity?
    hasTransferFromCounterpart :: AccountName -> CommoditySymbol -> Quantity -> Bool
    hasTransferFromCounterpart acct c q = any (/= acct) (M.findWithDefault [] (c, abs q) negCBAccts)

    isGainAcct :: Posting -> Bool
    isGainAcct p = lookupAccountType (paccount p) == Just Gain

    classifyAt :: Int -> Posting -> Posting
    classifyAt i p
      | not (isReal p) = p  -- skip virtual (parenthesised) postings
      | i `S.member` sameAcctTransferSet =
          let amts = amountsRaw (pamount p)
              cls = if any isNegativeAmount amts then "transfer-from" else "transfer-to"
          in addTag cls p
      | otherwise =
      case dbg5 ("classifyLotPostings: classifyPosting " ++ show (paccount p) ++ " result") $ shouldClassify p of
        Just classification -> addTag classification p
        Nothing
          | isGainAcct p -> addTag "gain" p
          | otherwise    -> p
      where addTag cls = postingAddHiddenAndMaybeVisibleTag verbosetags (toHiddenTag ("ptype", cls))

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
            shouldClassifyNegativeLotful p amts <|> shouldClassifyLotful p amts <|> shouldClassifyBareTransferTo p amts <|> shouldClassifyPositiveLotful p amts

    -- Classify a posting that has cost basis: acquire, dispose, transfer-from, or transfer-to.
    shouldClassifyWithCostBasis :: Posting -> [Amount] -> Maybe Text
    shouldClassifyWithCostBasis p amts = do
      let
        isNeg = any isNegativeAmount amts
        primaryType = if isNeg then "dispose" else "acquire"
        cbAmts = [(acommodity a, aquantity a) | a <- amts, isJust (acostbasis a)]
        isTransfer = any (\(c, q) -> hasCounterpart (paccount p) isNeg c q) cbAmts
      return $ if isTransfer
        then if isNeg then "transfer-from" else "transfer-to"
        else primaryType

    -- Classify a negative lotful posting without cost basis as dispose or transfer-from.
    shouldClassifyNegativeLotful :: Posting -> [Amount] -> Maybe Text
    shouldClassifyNegativeLotful p amts = do
      guard $ postingIsLotful p amts
      guard $ any isNegativeAmount amts
      let amtPairs = [(acommodity a, aquantity a) | a <- amts]
          isTransfer = any (\(c, q) -> hasCounterpart (paccount p) True c q) amtPairs
      return $ if isTransfer then "transfer-from" else "dispose"

    -- Classify a lotful posting without cost basis.
    -- A positive posting in a lotful commodity/account, with no transacted price,
    -- and with a matching transfer-from counterpart, is classified as transfer-to.
    shouldClassifyLotful :: Posting -> [Amount] -> Maybe Text
    shouldClassifyLotful p amts = do
      guard $ postingIsLotful p amts
      guard $ not $ any isNegativeAmount amts
      guard $ not $ any (isJust . acost) amts
      let amtPairs = [(acommodity a, aquantity a) | a <- amts]
      guard $ any (\(c, q) -> hasTransferFromCounterpart (paccount p) c q) amtPairs
      return "transfer-to"

    -- Classify a bare positive asset posting (no cost basis, not necessarily lotful)
    -- as transfer-to if there's a matching transfer-from counterpart (fix C).
    shouldClassifyBareTransferTo :: Posting -> [Amount] -> Maybe Text
    shouldClassifyBareTransferTo p amts = do
      guard $ not $ any isNegativeAmount amts
      let amtPairs = [(acommodity a, aquantity a) | a <- amts]
      guard $ any (\(c, q) -> hasTransferFromCounterpart (paccount p) c q) amtPairs
      return "transfer-to"

    -- Classify a positive lotful posting without cost basis as acquire.
    -- This is the fallback for positive lotful postings that aren't transfer-to.
    -- The cost basis will be inferred later from transacted cost (explicit or balancer-inferred).
    -- If cost can't be inferred, the classification is removed at lot calculation time.
    shouldClassifyPositiveLotful :: Posting -> [Amount] -> Maybe Text
    shouldClassifyPositiveLotful p amts = do
      guard $ postingIsLotful p amts
      guard $ any (\a -> aquantity a > 0) amts
      return "acquire"

    -- Check if a posting is lotful: its commodity or account has a lots: tag.
    postingIsLotful :: Posting -> [Amount] -> Bool
    postingIsLotful p amts =
      any ((== "lots") . T.toLower . fst) (ptags p)  -- account lots: tag (inherited via ptags)
      || any (commodityIsLotful . acommodity) amts     -- commodity lots: tag

-- Lot calculation (pipeline stage 2)

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
      (_, txns') <- foldM (processTransaction j needsLabels) (M.empty, []) (sortOn tdate txns)
      Right $ journalTieTransactions $ j{jtxns = reverse txns'}
  where
    txns = jtxns j

-- Disposal balancing

-- | For each disposal transaction, infer any amountless gain posting's amount from cost basis,
-- then check that the transaction is balanced at cost basis.
-- This runs after journalCalculateLots, which has filled in cost basis info on dispose postings.
-- The verbosetags parameter controls whether the ptype tag will be made visible in comments.
journalInferAndCheckDisposalBalancing :: Bool -> Journal -> Either String Journal
journalInferAndCheckDisposalBalancing verbosetags j = do
    txns' <- mapM inferGainInTransaction (jtxns j)
    Right j{jtxns = txns'}
  where
    atypes = jaccounttypes j

    isGain :: Posting -> Bool
    isGain p = accountNameType atypes (paccount p) == Just Gain

    gainAccount = case sort [a | (a, Gain) <- M.toList atypes] of
      []    -> "revenue:gains"
      (a:_) -> a

    tagGain :: Posting -> Posting
    tagGain = postingAddHiddenAndMaybeVisibleTag verbosetags (toHiddenTag ("ptype", "gain"))

    inferGainInTransaction t
      | not (any isDisposePosting (tpostings t)) = Right t
      | otherwise = do
          let (gainPs, otherPs) = partition' isGain (tpostings t)
              (amountfulGainPs, amountlessGainPs) = partition' hasAmount gainPs
          case amountlessGainPs of
            -- No amountless gain postings
            []
              -- No gain postings at all: create one if residual is nonzero
              | null gainPs -> do
                  let residual = foldMap postingCostBasisAmount (tpostings t)
                  if mixedAmountLooksZero residual
                    then Right t
                    else do
                      let inferredAmt = maNegate residual
                          gp = tagGain nullposting{paccount = gainAccount, pamount = inferredAmt}
                          t' = txnTieKnot $ t{tpostings = tpostings t ++ [gp]}
                      checkBalance t'
                      Right t'
              -- Has amountful gain postings, just check balance
              | otherwise -> do
                  checkBalance t
                  Right t
            -- One amountless gain posting: infer its amount, then check
            [gp] -> do
              let otherSum = foldMap postingCostBasisAmount (otherPs ++ amountfulGainPs)
                  inferredAmt = maNegate otherSum
                  gp' = gp{ pamount   = inferredAmt
                          , poriginal = Just $ originalPosting gp
                          }
                  t' = t{tpostings = map (\p -> if p == gp then gp' else p) (tpostings t)}
              checkBalance t'
              Right t'
            -- Multiple amountless gain postings: error
            _ -> Left $ txnErrPrefix t
                     ++ "This disposal transaction has multiple amountless gain postings.\n"
                     ++ "At most one gain posting may have its amount inferred."

    checkBalance t =
      let costBasisSum = foldMap postingCostBasisAmount (tpostings t)
      in unless (mixedAmountLooksZero costBasisSum) $
           Left $ disposalBalanceError t costBasisSum

    -- Value a posting at cost basis: if it has a cost basis, use quantity * basis cost;
    -- otherwise use the raw amount. Amountless postings contribute nothing.
    postingCostBasisAmount :: Posting -> MixedAmount
    postingCostBasisAmount p
      | not (hasAmount p) = nullmixedamt
      | otherwise         = foldMap amountCostBasisValue (amountsRaw (pamount p))

    amountCostBasisValue :: Amount -> MixedAmount
    amountCostBasisValue a = case acostbasis a >>= cbCost of
      Just basisCost -> mixedAmount basisCost{aquantity = aquantity a * aquantity basisCost}
      Nothing        -> mixedAmount a

    disposalBalanceError :: Transaction -> MixedAmount -> String
    disposalBalanceError t residual =
      txnErrPrefix t
      ++ "This disposal transaction is unbalanced at cost basis.\n"
      ++ "The gain/loss posting may be missing or incorrect.\n"
      ++ "Residual: " ++ showMixedAmountOneLine residual

    -- Like Data.List.partition but preserves the type for the predicate
    partition' :: (a -> Bool) -> [a] -> ([a], [a])
    partition' _ [] = ([], [])
    partition' f (x:xs)
      | f x       = (x:yes, no)
      | otherwise  = (yes, x:no)
      where (yes, no) = partition' f xs

-- Posting type predicates

-- | Remove the _ptype tag from a posting, declassifying it as a lot posting.
-- Used when a bare lotful posting was tentatively classified but cost/price
-- can't be inferred at lot calculation time.
stripPtypeTag :: Posting -> Posting
stripPtypeTag p = p{ptags = filter (\(k,_) -> k /= "_ptype") (ptags p)}

-- | Check if a posting has any lot-related ptype tag.
isLotPosting :: Posting -> Bool
isLotPosting p = isAcquirePosting p || isDisposePosting p
             || isTransferFromPosting p || isTransferToPosting p
             || isGainPosting p

-- | Check if a posting is an acquire posting (has _ptype:acquire tag).
isAcquirePosting :: Posting -> Bool
isAcquirePosting p = ("_ptype", "acquire") `elem` ptags p

-- | Check if a posting is a dispose posting (has _ptype:dispose tag).
isDisposePosting :: Posting -> Bool
isDisposePosting p = ("_ptype", "dispose") `elem` ptags p

-- | Check if a posting is a transfer-from posting (has _ptype:transfer-from tag).
isTransferFromPosting :: Posting -> Bool
isTransferFromPosting p = ("_ptype", "transfer-from") `elem` ptags p

-- | Check if a posting is a transfer-to posting (has _ptype:transfer-to tag).
isTransferToPosting :: Posting -> Bool
isTransferToPosting p = ("_ptype", "transfer-to") `elem` ptags p

-- | Check if a posting is a gain posting (has _ptype:gain tag).
isGainPosting :: Posting -> Bool
isGainPosting p = ("_ptype", "gain") `elem` ptags p

-- Validation and label generation

-- | Validate that user-provided labels don't create duplicate lot ids.
validateUserLabels :: [Transaction] -> Either String ()
validateUserLabels txns =
    case M.toList duplicates of
      [] -> Right ()
      (((c, d, l), t2:_):_) ->
        let (f, line, _, ex) = makeTransactionErrorExcerpt t2 (const Nothing)
        in Left $ printf (unlines [
              "%s:%d:"
             ,"%s"
             ,"lot id is not unique: commodity %s, date %s, label \"%s\""
             ]) f line ex (T.unpack c) (show d) (T.unpack l)
      _ -> Right ()  -- shouldn't happen
  where
    labeled = [ ((acommodity a, getLotDate t cb, l), t)
              | t <- txns
              , p <- tpostings t
              , isAcquirePosting p
              , a <- amountsRaw (pamount p)
              , Just cb <- [acostbasis a]
              , Just l  <- [cbLabel cb]
              ]
    txnsByKey = foldl' (\m (k, t) -> M.insertWith (++) k [t] m) M.empty labeled
    duplicates = M.filter (\ts -> length ts > 1) txnsByKey

-- | Find (commodity, date) pairs that have multiple acquisitions and thus need labels.
-- Only counts acquisitions that don't already have a user-provided label.
-- Includes bare acquire postings (no acostbasis), which use the transaction date.
findDatesNeedingLabels :: [Transaction] -> S.Set (CommoditySymbol, Day)
findDatesNeedingLabels txns =
    M.keysSet $ M.filter (> 1) counts
  where
    counts = foldl' countAcquire M.empty
      [ (acommodity a, acquireDate t a)
      | t <- txns
      , p <- tpostings t
      , isAcquirePosting p
      , a <- amountsRaw (pamount p)
      ]
    countAcquire m (c, d) = M.insertWith (+) (c, d) (1 :: Int) m
    acquireDate t a = case acostbasis a of
      Just cb -> getLotDate t cb
      Nothing -> tdate t

-- | Format a verbose error prefix for a transaction: "file:line:\nexcerpt\n\n".
-- Prepend to an error message to show source position and a transaction excerpt.
txnErrPrefix :: Transaction -> String
txnErrPrefix t = printf "%s:%d:\n%s\n" f line ex
  where (f, line, _, ex) = makeTransactionErrorExcerpt t (const Nothing)

-- | Get the lot date from cost basis, falling back to the transaction date.
getLotDate :: Transaction -> CostBasis -> Day
getLotDate t cb = fromMaybe (tdate t) (cbDate cb)

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

-- Transaction dispatch

-- | Process a single transaction: transform its acquire, dispose, and transfer postings.
-- Transfer pairs are processed first (so that transferred lots are available for
-- subsequent disposals in the same transaction), then acquire and dispose postings.
-- Accumulates (LotState, [Transaction]) — transactions in reverse order.
processTransaction :: Journal -> S.Set (CommoditySymbol, Day) -> (LotState, [Transaction]) -> Transaction
                   -> Either String (LotState, [Transaction])
processTransaction j needsLabels (ls, acc) t = do
    -- Partition postings into transfer pairs and others
    let (transferFroms, transferTos, otherPs) = partitionTransferPostings (tpostings t)
    pairs <- pairTransferPostings t transferFroms transferTos
    -- Process transfer pairs first, accumulating lot state changes
    (ls', transferPs) <- foldM processOnePair (ls, []) pairs
    -- Process remaining postings (acquire, dispose, passthrough)
    (ls'', otherPs') <- foldMPostings ls' [] otherPs
    -- Combine: transfer postings first, then others, preserving relative order
    let allPs = reverse transferPs ++ reverse otherPs'
    return (ls'', t{tpostings = allPs} : acc)
  where
    txnDate = tdate t

    processOnePair (st, psAcc) (fromP, toP) = do
      (st', fromPs, toPs) <- processTransferPair j t st fromP toP
      return (st', reverse toPs ++ reverse fromPs ++ psAcc)

    foldMPostings :: LotState -> [Posting] -> [Posting] -> Either String (LotState, [Posting])
    foldMPostings st acc' [] = Right (st, acc')
    foldMPostings st acc' (p:ps)
      | isAcquirePosting p = do
          (st', p') <- processAcquirePosting needsLabels txnDate t st p
          foldMPostings st' (p':acc') ps
      | isDisposePosting p = do
          (st', newPs) <- processDisposePosting j t st p
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
pairTransferPostings :: Transaction -> [Posting] -> [Posting]
                     -> Either String [(Posting, Posting)]
pairTransferPostings _ [] [] = Right []
pairTransferPostings t froms tos = do
    fromGroups <- groupByCommodity "transfer-from" froms
    toGroups   <- groupByCommodity "transfer-to" tos
    let allComms = S.union (M.keysSet fromGroups) (M.keysSet toGroups)
    concat <$> mapM (matchCommodityGroup fromGroups toGroups) (S.toList allComms)
  where
    showPos = txnErrPrefix t

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

-- | Extract a per-unit cost Amount from an AmountCost, normalising TotalCost by quantity.
-- If quantity is zero, returns the TotalCost amount as-is (avoiding division by zero).
amountCostToUnitCost :: Quantity -> AmountCost -> Amount
amountCostToUnitCost _   (UnitCost c)  = c
amountCostToUnitCost qty (TotalCost c)
  | qty == 0  = c
  | otherwise = c{aquantity = aquantity c / qty}

-- Per-type posting processing

-- | Process a single acquire posting: generate a lot name and append it as a subaccount.
processAcquirePosting :: S.Set (CommoditySymbol, Day) -> Day -> Transaction -> LotState -> Posting
                      -> Either String (LotState, Posting)
processAcquirePosting needsLabels txnDate t lotState p = do
    let lotAmts = [(a, cb) | a <- amountsRaw (pamount p), Just cb <- [acostbasis a]]
    (lotAmt, cb, isBare) <- case lotAmts of
      [x] -> Right (fst x, snd x, False)
      _   -> do
        let bareAmts = [a | a <- amountsRaw (pamount p), not (isNegativeAmount a)]
        case bareAmts of
          [a] -> Right (a, CostBasis Nothing Nothing Nothing, True)
          _   -> Left $ showPos ++ "acquire posting has no cost basis"

    let commodity = acommodity lotAmt
        date      = fromMaybe txnDate (cbDate cb)

    -- Get the original (pre-balancing) amount to check for explicit transacted price.
    -- A unit cost on the original can be used directly as the cost basis;
    -- a total cost on the balanced amount is normalised to a per-unit cost basis.
    let origAmt = case poriginal p of
          Just orig -> case [a | a <- amountsRaw (pamount orig), acommodity a == commodity] of
                         (a:_) -> a
                         []    -> lotAmt
          Nothing   -> lotAmt

    let maybeLotBasis = case cbCost cb of
          Just c  -> Just c
          Nothing
            | Just (UnitCost c) <- acost origAmt -> Just c
            | Just cost <- acost lotAmt -> Just $ amountCostToUnitCost (aquantity lotAmt) cost
            | otherwise                 -> Nothing

    case maybeLotBasis of
      -- Bare acquire with no inferable cost: silently declassify and pass through.
      Nothing | isBare    -> Right (lotState, stripPtypeTag p)
              | otherwise -> Left $ showPos ++ "acquire posting has no lot cost"
      Just lotBasis -> do
        let cbInferred = isNothing (cbCost cb)
            needsLabel = S.member (commodity, date) needsLabels
            lotLabel'  = cbLabel cb <|> if needsLabel then Just (generateLabel commodity date lotState) else Nothing
            lotId      = LotId date lotLabel'
            fullCb     = CostBasis{cbDate = Just date, cbLabel = lotLabel', cbCost = Just lotBasis}
            lotName    = showLotName fullCb
            -- When cost basis was inferred, fill it in on the user's original cb
            -- so that print shows {$50} not {}.
            filledCb   = cb{cbCost = Just lotBasis}
            -- The lot state always stores the full cost basis (with date/label/cost)
            -- so that disposal selectors like {2026-01-15, $50} can match.
            lotStateAmt = lotAmt{acostbasis = Just fullCb}
            -- The posting amount preserves the user's original cost basis fields
            -- (only filling in cost when inferred) for print output fidelity.
            postingAmt  = if cbInferred then lotAmt{acostbasis = Just filledCb} else lotAmt

        let existingLots = M.findWithDefault M.empty commodity lotState
        when (M.member lotId existingLots) $
          Left $ showPos ++ "duplicate lot id: " ++ T.unpack lotName
                  ++ " for commodity " ++ T.unpack commodity

        let p' = if cbInferred
                 then let -- Update the original posting's cost basis too, so print shows {$50} not {}
                          -- For bare acquires, also normalize transacted cost to UnitCost
                          normalizedCost = fmap (UnitCost . amountCostToUnitCost (aquantity lotAmt)) (acost lotAmt)
                          origP  = originalPosting p
                          origP' = origP{pamount = mapMixedAmount updateCb $ pamount origP}
                          updateCb a
                            | acommodity a == commodity =
                                if isBare then a{acostbasis = Just filledCb, acost = normalizedCost}
                                else a{acostbasis = Just filledCb}
                            | otherwise = a
                      in p{paccount = paccount p <> ":" <> lotName
                          ,pamount  = mixedAmount $ if isBare then postingAmt{acost = normalizedCost} else postingAmt
                          ,poriginal = Just origP'}
                 else p{paccount = paccount p <> ":" <> lotName}
        let lotState' = M.insertWith (M.unionWith M.union) commodity
                          (M.singleton lotId (M.singleton (paccount p) lotStateAmt)) lotState
        return (lotState', p')
  where
    showPos = txnErrPrefix t

-- | Process a dispose posting: match to existing lots using the resolved reduction method,
-- split into multiple postings if the disposal spans multiple lots.
-- Returns the list of resulting postings (one per matched lot).
processDisposePosting :: Journal -> Transaction -> LotState -> Posting
                      -> Either String (LotState, [Posting])
processDisposePosting j t lotState p = do
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

    -- Bare dispose with no transacted price: silently declassify and pass through.
    -- Non-bare dispose (explicit {}) without price is still an error.
    case acost lotAmt of
      Nothing | isBare    -> return (lotState, [stripPtypeTag p])
              | otherwise -> Left $ showPos ++ "dispose posting has no transacted price (selling price) for " ++ T.unpack commodity
      Just _ -> do

        when (disposeQty >= 0) $
          Left $ showPos ++ "dispose posting has non-negative quantity for " ++ T.unpack commodity

        let posQty = negate disposeQty
            method = resolveReductionMethod j p commodity
            -- Per-account methods (FIFO1/LIFO1) scope to the posting's base account
            -- (stripping any explicit lot subaccount the user may have written).
            scopeAcct = if methodIsPerAccount method
                        then Just (lotBaseAccount (paccount p))
                        else Nothing

        when (isBare && method == SPECID) $
          Left $ showPos ++ "SPECID requires a lot selector on dispose postings"

        selected <- selectLots method showPos scopeAcct commodity posQty cb lotState

        -- For AVERAGE methods, compute the weighted average cost across the entire pool
        -- (not just the selected lots). The pool is scoped the same way as lot selection.
        mavgCost <- if methodIsAverage method
          then do
            let allLots = M.findWithDefault M.empty commodity lotState
                flatLots = case scopeAcct of
                  Nothing   -> M.mapMaybe lotPickAny allLots
                  Just acct -> M.mapMaybe (M.lookup acct) allLots
            fmap Just $ poolWeightedAvgCost showPos flatLots
          else Right Nothing

        let baseAcct = lotBaseAccount (paccount p)
            hasExplicitLotAcct = baseAcct /= paccount p
            mkPosting (lotId, storedAmt, consumedQty) = do
              -- The original lot's cost basis is always needed for the lot subaccount name.
              origBasis <- case acostbasis storedAmt >>= cbCost of
                Just c  -> Right c
                Nothing -> Left $ showPos ++ "lot " ++ T.unpack (T.pack (show lotId))
                                    ++ " for commodity " ++ T.unpack commodity
                                    ++ " has no cost basis (internal error)"
              -- For AVERAGE methods, use the weighted average cost for the disposal amount.
              -- For all other methods, use the original lot's cost.
              let disposalBasis = fromMaybe origBasis mavgCost
                  -- Lot name uses the original cost (so the subaccount matches the acquisition).
                  lotCb = CostBasis
                    { cbDate  = Just (lotDate lotId)
                    , cbLabel = lotLabel lotId
                    , cbCost  = Just origBasis
                    }
                  -- Disposal cost basis uses average cost when applicable.
                  dispCb = lotCb{cbCost = Just disposalBasis}
                  lotName = showLotName lotCb
                  expectedAcct = baseAcct <> ":" <> lotName
              -- If the user wrote an explicit lot subaccount, check it matches the resolved lot.
              when (hasExplicitLotAcct && paccount p /= expectedAcct) $
                Left $ showPos ++ "lot subaccount " ++ T.unpack (paccount p)
                        ++ " does not match the resolved lot " ++ T.unpack expectedAcct
              let acctWithLot = expectedAcct
                  -- Build the dispose amount: negative consumed quantity,
                  -- keeping the original amount's commodity, style, cost, and cost basis.
                  disposeAmt = lotAmt{aquantity = negate consumedQty, acostbasis = Just dispCb}
              if isBare
                then do
                  -- Normalize transacted price to UnitCost and update poriginal
                  -- so print shows the inferred cost basis and cost.
                  let normalizedCost = fmap (UnitCost . amountCostToUnitCost (aquantity lotAmt)) (acost lotAmt)
                      disposeAmt' = disposeAmt{acost = normalizedCost}
                      origP  = originalPosting p
                      origP' = origP{pamount = mapMixedAmount updateAmt $ pamount origP}
                      updateAmt a | acommodity a == commodity =
                                      a{aquantity = negate consumedQty, acostbasis = Just dispCb, acost = normalizedCost}
                                  | otherwise = a
                  Right p{ paccount  = acctWithLot
                         , pamount   = mixedAmount disposeAmt'
                         , poriginal = Just origP'
                         }
                else
                  Right p{ paccount = acctWithLot
                         , pamount  = mixedAmount disposeAmt
                         }

        newPostings <- mapM mkPosting selected
        let consumed = [(lotId, qty) | (lotId, _, qty) <- selected]
            lotState' = reduceLotState scopeAcct commodity consumed lotState

        return (lotState', newPostings)
  where
    showPos = txnErrPrefix t

-- | Process a transfer pair: select lots from the source account (transfer-from)
-- and recreate them under the destination account (transfer-to).
-- Returns updated LotState and two lists of expanded postings (from, to).
processTransferPair :: Journal -> Transaction -> LotState -> Posting -> Posting
                    -> Either String (LotState, [Posting], [Posting])
processTransferPair j t lotState fromP toP = do
    -- Extract lotful amount and lot selector from transfer-from.
    -- When cost basis is present, use it directly as the lot selector.
    -- When absent (bare transfer on a lotful commodity), use a wildcard selector.
    let fromAmts = [(a, cb) | a <- amountsRaw (pamount fromP), Just cb <- [acostbasis a]]
    (fromAmt, fromCb, isBare) <- case fromAmts of
      [x] -> Right (fst x, snd x, False)
      _   -> do
        let bareAmts = [a | a <- amountsRaw (pamount fromP), isNegativeAmount a]
        case bareAmts of
          [a] -> Right (a, CostBasis Nothing Nothing Nothing, True)
          _   -> Left $ showPos ++ "transfer-from posting has no cost basis"

    let commodity = acommodity fromAmt
        transferQty = aquantity fromAmt

    let toAmts = amountsRaw (pamount toP)

    -- Check that neither transfer posting has explicit transacted price (@ or @@).
    -- Use originalPosting to distinguish user-written @ from pipeline-inferred acost.
    let origFromAmts = amountsRaw $ pamount $ originalPosting fromP
        origToAmts   = amountsRaw $ pamount $ originalPosting toP
    when (any (isJust . acost) origFromAmts || any (isJust . acost) origToAmts) $
      Left $ showPos ++ "lot transfers should have no transacted price"

    -- Validate transfer-from has negative quantity
    when (transferQty >= 0) $
      Left $ showPos ++ "transfer-from posting has non-negative quantity for " ++ T.unpack commodity

    let posQty = negate transferQty
        -- Transfers are always per-account (scoped to source), but ordering follows the method.
        method = resolveReductionMethod j fromP commodity

    -- Select lots from source account using the resolved method's ordering
    selected <- selectLots method showPos (Just (lotBaseAccount (paccount fromP))) commodity posQty fromCb lotState

    -- Extract the transfer-to cost basis for optional validation
    let toCb = case [(a, cb) | a <- toAmts, Just cb <- [acostbasis a]] of
                 [(_, cb)] -> Just cb
                 _         -> Nothing

    -- For each selected lot, generate from and to postings
    (fromPs, toPs) <- fmap unzip $ mapM (mkTransferPostings isBare fromAmt toCb commodity) selected

    -- Update lot state: remove from source, add to destination
    let consumed = [(lotId, qty) | (lotId, _, qty) <- selected]
        lotState' = reduceLotState (Just (paccount fromP)) commodity consumed lotState
        lotState'' = foldl' (addTransferredLot commodity (paccount toP)) lotState' selected

    return (lotState'', fromPs, toPs)
  where
    showPos = txnErrPrefix t

    mkTransferPostings isBare' fromAmt toCb commodity (lotId, storedAmt, consumedQty) = do
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
        if isBare'
          then do
            -- For bare transfers, update poriginal so print shows the inferred cost basis.
            let origFromP = originalPosting fromP
                origFromP' = origFromP{pamount = mapMixedAmount (updateBare commodity lotCb (negate consumedQty)) (pamount origFromP)}
                origToP = originalPosting toP
                origToP' = origToP{pamount = mapMixedAmount (updateBare commodity lotCb consumedQty) (pamount origToP)}
                fromP' = fromP{ paccount = paccount fromP <> ":" <> lotName
                              , pamount  = mixedAmount fromAmt'
                              , poriginal = Just origFromP'
                              }
                toP'   = toP{ paccount = paccount toP <> ":" <> lotName
                            , pamount  = mixedAmount toAmt'
                            , poriginal = Just origToP'
                            }
            Right (fromP', toP')
          else do
            let fromP' = fromP{ paccount = paccount fromP <> ":" <> lotName
                              , pamount  = mixedAmount fromAmt'
                              }
                toP'   = toP{ paccount = paccount toP <> ":" <> lotName
                            , pamount  = mixedAmount toAmt'
                            }
            Right (fromP', toP')

    updateBare commodity lotCb qty a
      | acommodity a == commodity = a{aquantity = qty, acostbasis = Just lotCb}
      | otherwise = a

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

-- Lot state operations

-- | Select lots to consume using the given reduction method.
-- The method controls two things:
--   1. Ordering: FIFO\/FIFO1 use oldest-first; LIFO\/LIFO1 use newest-first;
--      HIFO\/HIFO1 use highest per-unit cost first; AVERAGE\/AVERAGE1 use FIFO order.
--   2. Scope: FIFO\/LIFO\/HIFO\/AVERAGE select globally across all accounts;
--      FIFO1\/LIFO1\/HIFO1\/AVERAGE1 select only from the specified account.
-- When @maccount@ is @Just acct@, that account is used for per-account methods
-- and for transfers (which are always per-account regardless of method).
-- The lot selector filters which lots are eligible: each non-Nothing field
-- in the selector must match the corresponding field in the lot's cost basis.
-- An all-Nothing selector (from @{}@) matches all lots.
-- Returns a list of (lot id, lot amount, quantity consumed from this lot).
-- Errors if total available quantity in matching lots is insufficient.
selectLots :: ReductionMethod -> String -> Maybe AccountName -> CommoditySymbol
           -> Quantity -> CostBasis -> LotState
           -> Either String [(LotId, Amount, Quantity)]
selectLots method posStr maccount commodity qty selector lotState = do
    when (method == SPECID && isWildcardSelector selector) $
      Left $ posStr ++ "SPECID requires an explicit lot selector"
    let allLots = M.findWithDefault M.empty commodity lotState
        -- Flatten to (LotId, Amount) pairs, optionally filtering by account.
        -- For global selection, sum quantities across accounts per lot.
        -- For per-account selection, take only the specified account's balance.
        flatLots = case maccount of
          Nothing   -> M.mapMaybe lotPickAny allLots
          Just acct -> M.mapMaybe (M.lookup acct) allLots
        matchingLots = M.filter (lotMatchesSelector selector) flatLots
    when (M.null matchingLots) $
      Left $ posStr ++ "no lots available for commodity " ++ T.unpack commodity
    when (method == SPECID && M.size matchingLots > 1) $
      Left $ posStr ++ "lot selector is ambiguous, matches " ++ show (M.size matchingLots) ++ " lots"
    let available = sum [aquantity a | a <- M.elems matchingLots]
    when (available < qty) $
      Left $ posStr ++ "insufficient lots for commodity " ++ T.unpack commodity
              ++ ": need " ++ show qty ++ " but only " ++ show available ++ " available"
    let orderedLots = case method of
          FIFO     -> M.toAscList matchingLots
          FIFO1    -> M.toAscList matchingLots
          LIFO     -> M.toDescList matchingLots
          LIFO1    -> M.toDescList matchingLots
          HIFO     -> sortOn (Down . lotPerUnitCost) (M.toList matchingLots)
          HIFO1    -> sortOn (Down . lotPerUnitCost) (M.toList matchingLots)
          AVERAGE  -> M.toAscList matchingLots
          AVERAGE1 -> M.toAscList matchingLots
          SPECID   -> M.toAscList matchingLots
    Right $ go qty orderedLots
  where
    go 0 _ = []
    go _ [] = []  -- shouldn't happen after the check above
    go remaining ((lotId, lotAmt):rest)
      | remaining >= lotBal = (lotId, lotAmt, lotBal) : go (remaining - lotBal) rest
      | otherwise           = [(lotId, lotAmt, remaining)]
      where lotBal = aquantity lotAmt

-- | For global lot selection, pick any account's Amount as representative
-- (they all share the same cost basis) and sum the total quantity.
lotPickAny :: M.Map AccountName Amount -> Maybe Amount
lotPickAny acctMap = case M.elems acctMap of
  []    -> Nothing
  (a:_) -> Just a{aquantity = sum [aquantity x | x <- M.elems acctMap]}

-- | Extract the per-unit cost quantity from a lot entry, for HIFO sorting.
lotPerUnitCost :: (LotId, Amount) -> Quantity
lotPerUnitCost (_, a) = maybe 0 aquantity (acostbasis a >>= cbCost)

-- | Whether a reduction method uses weighted average cost basis for disposals.
methodIsAverage :: ReductionMethod -> Bool
methodIsAverage AVERAGE  = True
methodIsAverage AVERAGE1 = True
methodIsAverage _        = False

-- | Compute the weighted average per-unit cost across all lots in a pool.
-- All lots must have cost basis in the same commodity.
-- Returns a representative cost Amount with the weighted average quantity.
poolWeightedAvgCost :: String -> M.Map LotId Amount -> Either String Amount
poolWeightedAvgCost posStr lots = do
    let costs = [(aquantity a, c) | a <- M.elems lots, Just cb <- [acostbasis a], Just c <- [cbCost cb]]
    case costs of
      [] -> Left $ posStr ++ "no lots with cost basis available for averaging"
      ((_, firstCost):rest)
        | any (\(_, c) -> acommodity c /= acommodity firstCost) rest ->
            Left $ posStr ++ "cannot average lots with different cost commodities"
        | otherwise ->
            let totalQty  = sum [q | (q, _) <- costs]
                totalCost = sum [q * aquantity c | (q, c) <- costs]
            in Right firstCost{aquantity = totalCost / totalQty}

-- | Is this an all-Nothing (wildcard) lot selector, i.e. from @{}@?
isWildcardSelector :: CostBasis -> Bool
isWildcardSelector (CostBasis Nothing Nothing Nothing) = True
isWildcardSelector _ = False

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
