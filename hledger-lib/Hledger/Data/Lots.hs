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
     Dispose postings with a transacted price (selling price) generate gain postings.
     Bare disposes without a price (e.g. fee deductions) get lot subaccounts but no gain.

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

* pairIndexedTransferPostings:
  "transfer-to/from posting ... has no matching ... posting",
  -- "mismatched transfer postings for commodity X",
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
  "no lots available for commodity X in account Y",
  "lot selector is ambiguous, matches N lots in account Y",
  "insufficient lots for commodity X in account Y"

* validateGlobalCompliance:
  "METHOD: lot(s) on other account(s) have higher priority than the lots in ACCT"

* poolWeightedAvgCost:
  "no lots with cost basis available for averaging",
  "cannot average lots with different cost commodities",
  "cannot average lots with zero total quantity"

* foldMPostings (--lots-warn only):
  "X is declared lotful ... but this posting was not classified"

* journalInferAndCheckDisposalBalancing:
  "This disposal transaction has multiple amountless gain postings",
  "This disposal transaction is unbalanced at cost basis"

-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}

module Hledger.Data.Lots (
  journalClassifyLotPostings,
  journalCalculateLots,
  journalCalculateLotsQuiet,
  journalInferAndCheckDisposalBalancing,
  isGainPosting,
  lotBaseAccount,
  lotSubaccountName,
  mergeCostBasis,
  parseLotName,
  showLotName,
) where

import Control.Applicative ((<|>))
import Data.Bifunctor (first)
import Control.Monad (foldM, guard, unless, when)
import Data.List (intercalate, sort, sortOn)
import Data.Ord (Down(..))
#if !MIN_VERSION_base(4,20,0)
import Data.List (foldl')
#endif
import Data.Map.Strict qualified as M
import Data.Maybe (catMaybes, fromMaybe, isJust, isNothing)
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import Data.Char (isDigit)
import Data.Time.Calendar (Day, addDays, fromGregorianValid)
import Text.Printf (printf)

import Hledger.Data.AccountName (accountNameType)
import Hledger.Data.Dates (showDate)
import Hledger.Data.AccountType (isAssetType, isEquityType)
import Hledger.Data.Amount (amountSetQuantity, amountsRaw, isNegativeAmount, maNegate, mapMixedAmount, mixedAmount, mixedAmountLooksZero, nullmixedamt, noCostFmt, showAmountWith, showMixedAmountOneLine)
import Hledger.Data.Errors (makePostingErrorExcerpt, makeTransactionErrorExcerpt)
import Hledger.Data.Journal (journalAccountType, journalCommodityLotsMethod, journalCommodityUsesLots, journalFilePaths, journalInheritedAccountTags, journalMapTransactions, journalTieTransactions, parseReductionMethod)
import Hledger.Data.Posting (generatedPostingTagName, hasAmount, isReal, nullposting, originalPosting, postingAddHiddenAndMaybeVisibleTag, postingStripCosts)
import Hledger.Data.Transaction (txnTieKnot)
import Hledger.Data.Types
import Hledger.Utils (dbg5, dbg5With, warn)

-- Types

-- | Map from commodity to (map from lot id to (map from account name to lot balance)).
-- Keyed by commodity at the top level so lots of different commodities don't clash.
-- The inner Map LotId is ordered by date then label, supporting FIFO/LIFO naturally.
-- The innermost Map AccountName allows the same lot to exist at multiple accounts
-- (eg after a partial lot transfer).
type LotState = M.Map CommoditySymbol (M.Map LotId (M.Map AccountName Amount))

-- | Resolve which reduction method to use for a posting, and where it came from.
-- Checks account-inherited tags first, then commodity tags, defaulting to FIFO.
-- Since commodity tags are propagated to ptags, we distinguish by checking the
-- commodity's own declared tags separately.
resolveReductionMethodWithSource :: Journal -> Posting -> CommoditySymbol -> (ReductionMethod, String)
resolveReductionMethodWithSource j p commodity =
  case accountLotsMethod of
    Just m  -> (m, "from account tag on " ++ T.unpack (paccount p))
    Nothing -> case journalCommodityLotsMethod j commodity of
      Just m  -> (m, "from commodity tag on " ++ T.unpack commodity)
      Nothing -> (FIFO, "default")
  where
    -- Check account-inherited tags only (excluding commodity-propagated tags).
    accountLotsMethod =
      let acctTags = journalInheritedAccountTags j (paccount p)
      in case [v | (k, v) <- acctTags, T.toLower k == "lots"] of
           (v:_) -> parseReductionMethod v
           []    -> Nothing

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

-- | Extract the lot subaccount name (the @{...}@ component) from an account name,
-- or @Nothing@ if there is none.
-- E.g., @\"assets:broker:{2026-01-15, $50}\"@ returns @Just \"{2026-01-15, $50}\"@.
lotSubaccountName :: AccountName -> Maybe Text
lotSubaccountName a =
  let (prefix, lastComp) = T.breakOnEnd ":" a
  in if not (T.null prefix) && "{" `T.isPrefixOf` lastComp
     then Just lastComp
     else Nothing

-- | Parse a lot name (as produced by 'showLotName') back into a 'CostBasis'.
-- The input should be the full @{...}@ string including braces.
-- Parts are comma-separated and all optional: date (@YYYY-MM-DD@),
-- label (@\"LABEL\"@), and cost (parsed by the supplied callback).
-- The callback avoids an import cycle (Lots.hs cannot import Read-layer parsers).
parseLotName :: (String -> Maybe Amount) -> Text -> Either String CostBasis
parseLotName parseAmt t = do
  inner <- case T.stripPrefix "{" t >>= T.stripSuffix "}" of
    Just s  -> Right (T.strip s)
    Nothing -> Left $ "lot name must be enclosed in braces: " ++ T.unpack t
  if T.null inner
    then Right $ CostBasis Nothing Nothing Nothing
    else do
      let parts = map T.strip $ splitOnCommas inner
      parseParts parts
  where
    parseParts parts = go parts Nothing Nothing Nothing
      where
        go [] d l c = Right $ CostBasis d l c
        go (p:ps) d l c
          | isDatePart p = case parseDate p of
              Just day -> go ps (Just day) l c
              Nothing  -> Left $ "invalid date in lot name: " ++ T.unpack p
          | isLabelPart p = go ps d (Just (T.drop 1 (T.dropEnd 1 p))) c
          | otherwise = case parseAmt (T.unpack p) of
              Just amt -> go ps d l (Just amt)
              Nothing  -> Left $ "cannot parse lot name part: " ++ T.unpack p

    isDatePart p = T.length p == 10 && T.all (\c -> isDigit c || c == '-') p
    isLabelPart p = "\"" `T.isPrefixOf` p && "\"" `T.isSuffixOf` p && T.length p >= 2

    parseDate p = do
      let s = T.unpack p
      case s of
        [y1,y2,y3,y4,'-',m1,m2,'-',d1,d2] ->
          fromGregorianValid
            (read [y1,y2,y3,y4])
            (read [m1,m2])
            (read [d1,d2])
        _ -> Nothing

    -- Split on commas, but not within double-quoted strings.
    -- E.g. @"2026-01-15, \"my, label\", $50"@ -> @["2026-01-15", "\"my, label\"", "$50"]@.
    splitOnCommas :: Text -> [Text]
    splitOnCommas s
      | T.null s  = []
      | otherwise = let (part, rest) = takeField s
                    in part : if T.null rest then [] else splitOnCommas rest

    -- Consume one comma-separated field (respecting double quotes), returning
    -- (field, remaining-after-comma).
    takeField :: Text -> (Text, Text)
    takeField = go T.empty False
      where
        go acc inQuote s
          | T.null s             = (acc, T.empty)
          | c == '"'             = go (T.snoc acc c) (not inQuote) cs
          | c == ',' && not inQuote = (acc, cs)
          | otherwise            = go (T.snoc acc c) inQuote cs
          where (c, cs) = (T.head s, T.tail s)

-- | Merge two 'CostBasis' values. For each field, if both are @Just@, they
-- must agree (returns error if not); otherwise takes whichever is @Just@.
-- The first argument is typically the account-name basis (more complete),
-- the second is the amount's existing basis.
mergeCostBasis :: CostBasis -> CostBasis -> Either String CostBasis
mergeCostBasis a b = do
  d <- mergeField "date"  show     (cbDate a) (cbDate b)
  l <- mergeField "label" T.unpack (cbLabel a) (cbLabel b)
  c <- mergeCostField (cbCost a) (cbCost b)
  Right $ CostBasis d l c
  where
    mergeField :: Eq a => String -> (a -> String) -> Maybe a -> Maybe a -> Either String (Maybe a)
    mergeField _ _ Nothing  y       = Right y
    mergeField _ _ x       Nothing  = Right x
    mergeField name showVal (Just x) (Just y)
      | x == y    = Right (Just x)
      | otherwise = Left $ "conflicting cost basis " ++ name
                      ++ ": account name has " ++ showVal x
                      ++ " but amount has " ++ showVal y

    mergeCostField :: Maybe Amount -> Maybe Amount -> Either String (Maybe Amount)
    mergeCostField Nothing  y       = Right y
    mergeCostField x       Nothing  = Right x
    mergeCostField (Just x) (Just y)
      | acommodity x == acommodity y && aquantity x == aquantity y = Right (Just x)
      | otherwise = Left $ "conflicting cost basis cost"
                      ++ ": account name has " ++ showAmountWith noCostFmt x
                      ++ " but amount has " ++ showAmountWith noCostFmt y

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
--
-- For more detail on classification rules, please see doc/SPEC-lots.md > Lot postings.
--
transactionClassifyLotPostings :: Bool -> (AccountName -> Maybe AccountType) -> (CommoditySymbol -> Bool) -> Transaction -> Transaction
transactionClassifyLotPostings verbosetags lookupAccountType commodityIsLotful t@Transaction{tpostings=ps}
  | not (any hasLotRelevantAmount ps) && not (any isGainAcct ps)
    = lotDbg t "no lot-relevant amounts, skipping" t
  | otherwise = lotDbg t "classifying" $ t{tpostings=zipWith classifyAt [0..] ps}
  where
    hasCostBasis :: Posting -> Bool
    hasCostBasis p = let amts = amountsRaw (pamount p)
                         result = any (isJust . acostbasis) amts
                     in dbg5 ("classifyLotPostings: hasCostBasis " ++ show (paccount p) ++ " amts=" ++ show (length amts)) result

    hasLotRelevantAmount :: Posting -> Bool
    hasLotRelevantAmount p = isReal p && (hasCostBasis p || hasNegativeLotfulAmount p || hasPositiveLotfulAmount p)

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
          | otherwise = foldl' (addAmt i (lotBaseAccount (paccount p))) m (amountsRaw (pamount p))
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
              let baseAcct = lotBaseAccount (paccount p)
                  isAsset  = maybe False isAssetType (lookupAccountType baseAcct)
                  amts     = amountsRaw (pamount p)
                  acct     = baseAcct
                  isNeg    = any isNegativeAmount amts
                  hasCB    = any (isJust . acostbasis) amts
                  isLotful = postingIsLotful p amts
                  cbKeys   = [(acommodity a, abs (aquantity a)) | a <- amts, isJust (acostbasis a)]
                  allKeys  = [(acommodity a, abs (aquantity a)) | a <- amts]
                  -- Include cost-basis negatives (any account type) and bare lotful
                  -- negatives on asset accounts. Non-asset bare lotful negatives
                  -- (e.g. revenue) are excluded — they shouldn't be transfer counterparts.
                  -- Equity transfers are handled separately via hasEquityCounterpart.
                  neg'     = if isNeg && (hasCB || (isLotful && isAsset))
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

    -- Commodity-only transfer-from check (ignores quantity).
    -- Fallback for transfer+fee patterns where quantities don't match exactly.
    hasTransferFromCommodityMatch :: AccountName -> CommoditySymbol -> Bool
    hasTransferFromCommodityMatch acct c =
      any (\((c', _), accts) -> c' == c && any (/= acct) accts)
          (M.toList negCBAccts)

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
      where addTag cls = postingAddHiddenAndMaybeVisibleTag True verbosetags (toHiddenTag ("ptype", cls))

    -- Check if posting should be classified and return the classification:
    -- one of acquire, dispose, transfer-from, transfer-to.
    shouldClassify :: Posting -> Maybe Text
    shouldClassify p = do
      let amts = amountsRaw $ pamount p
          baseAcct = lotBaseAccount (paccount p)
      if any (isJust . acostbasis) amts
        -- Cost basis present: classify regardless of account type (fix A)
        then dbg5 ("classifyLotPostings: shouldClassify " ++ show (paccount p) ++ " withCostBasis") $
             shouldClassifyWithCostBasis p amts
        else do
          -- No cost basis: require asset account type
          acctType <- dbg5 ("classifyLotPostings: shouldClassify " ++ show (paccount p) ++ " acctType") $
                      lookupAccountType baseAcct
          guard $ isAssetType acctType
          dbg5 ("classifyLotPostings: shouldClassify " ++ show (paccount p) ++ " lotful/bare") $
            shouldClassifyNegativeLotful p amts <|> shouldClassifyLotful p amts <|> shouldClassifyBareTransferTo p amts <|> shouldClassifyPositiveLotful p amts

    -- True when the transaction has an equity posting with no explicit cost-basis amounts.
    -- This indicates an equity transfer: lots move to/from equity in two parts
    -- (e.g. close --clopen --lots generates a closing txn transferring lots into equity,
    -- and an opening txn transferring them back out), allowing the negative lot postings
    -- to be classified as transfer-from rather than dispose.
    hasEquityCounterpart :: Bool
    hasEquityCounterpart = any isEquityNonLotPosting ps
      where
        isEquityNonLotPosting q =
          maybe False isEquityType (lookupAccountType (lotBaseAccount (paccount q)))
          && not (any (isJust . acostbasis) (amountsRaw (pamount q)))

    -- Classify a posting that has cost basis: acquire, dispose, transfer-from, or transfer-to.
    shouldClassifyWithCostBasis :: Posting -> [Amount] -> Maybe Text
    shouldClassifyWithCostBasis p amts = do
      let
        baseAcct = lotBaseAccount (paccount p)
        isNeg = any isNegativeAmount amts
        primaryType = if isNeg then "dispose" else "acquire"
        cbAmts = [(acommodity a, aquantity a) | a <- amts, isJust (acostbasis a)]
        isTransfer = any (\(c, q) -> hasCounterpart baseAcct isNeg c q) cbAmts
        -- Also treat as equity transfer when: no transacted price written,
        -- and an equity counterpart posting is present. This handles lots moving
        -- to/from equity (e.g. close --clopen --lots generates a closing txn with
        -- negative lot postings and an opening txn with positive lot postings).
        isEquityTransfer = not (any (isJust . acost) amts) && hasEquityCounterpart
      if isTransfer || isEquityTransfer
        then return $ if isNeg then "transfer-from" else "transfer-to"
        else do
          -- Don't classify income statement accounts (Revenue, Expense, Gain) as acquire/dispose.
          -- These are flow accounts that should not track lots or get lot subaccounts.
          -- E.g. expenses:fees 0.1 ETSY {$80} @ $90 in a stock-fee disposal.
          guard $ not $ maybe False isIncomeStatementAccountType (lookupAccountType baseAcct)
          return primaryType

    -- Classify a negative lotful posting without cost basis as dispose or transfer-from.
    -- If the posting has no transacted price and another asset account in the same
    -- transaction receives a positive lotful amount of the same commodity (even at
    -- different quantity), skip classification — it's a transfer+fee pattern and
    -- global FIFO will handle the lot reduction when the destination account trades.
    shouldClassifyNegativeLotful :: Posting -> [Amount] -> Maybe Text
    shouldClassifyNegativeLotful p amts = do
      guard $ postingIsLotful p amts
      guard $ any isNegativeAmount amts
      let baseAcct = lotBaseAccount (paccount p)
          hasPrice = any (isJust . acost) amts
          negAmts = [a | a <- amts, isNegativeAmount a]
          negCommodities = S.fromList [acommodity a | a <- negAmts]
          amtPairs = [(acommodity a, aquantity a) | a <- amts]
          isTransfer = any (\(c, q) -> hasCounterpart baseAcct True c q) amtPairs
          -- Check for positive lotful amounts in other asset accounts (same commodity).
          otherAssetReceives = any isOtherAssetWithLotful (filter (/= p) ps)
          isOtherAssetWithLotful q =
            let qAmts = amountsRaw (pamount q)
                qBase = lotBaseAccount (paccount q)
            in qBase /= baseAcct
               && maybe False isAssetType (lookupAccountType qBase)
               && postingIsLotful q qAmts
               && any (\a -> aquantity a > 0 && acommodity a `S.member` negCommodities) qAmts
          -- Does a non-asset posting receive exactly this commodity+quantity?
          -- If so, this posting is likely a fee/dispose (e.g. paired with expenses:fees),
          -- not part of a transfer to another asset account.
          hasFeeCounterpart = any isFeeCounterpart (filter (/= p) ps)
          isFeeCounterpart q =
            let qAmts = amountsRaw (pamount q)
                qBase = lotBaseAccount (paccount q)
            in not (maybe False isAssetType (lookupAccountType qBase))
               && any (\a -> aquantity a > 0
                          && any (\na -> acommodity na == acommodity a
                                      && abs (aquantity na) == aquantity a) negAmts) qAmts
      if isTransfer || (otherAssetReceives && not hasFeeCounterpart)
        then return "transfer-from"
        else do
          guard $ hasPrice || hasFeeCounterpart || not otherAssetReceives
          return "dispose"

    -- Classify a lotful posting without cost basis.
    -- A positive posting in a lotful commodity/account, with no transacted price,
    -- and with a matching transfer-from counterpart, is classified as transfer-to.
    shouldClassifyLotful :: Posting -> [Amount] -> Maybe Text
    shouldClassifyLotful p amts = do
      guard $ postingIsLotful p amts
      guard $ not $ any isNegativeAmount amts
      guard $ not $ any (isJust . acost) amts
      let baseAcct = lotBaseAccount (paccount p)
          amtPairs = [(acommodity a, aquantity a) | a <- amts]
      guard $ any (\(c, q) -> hasTransferFromCounterpart baseAcct c q) amtPairs
           || any (\(c, _) -> hasTransferFromCommodityMatch baseAcct c) amtPairs
      return "transfer-to"

    -- Classify a bare positive asset posting (no cost basis, not necessarily lotful)
    -- as transfer-to if there's a matching transfer-from counterpart (fix C).
    shouldClassifyBareTransferTo :: Posting -> [Amount] -> Maybe Text
    shouldClassifyBareTransferTo p amts = do
      guard $ not $ any isNegativeAmount amts
      let baseAcct = lotBaseAccount (paccount p)
          amtPairs = [(acommodity a, aquantity a) | a <- amts]
      guard $ any (\(c, q) -> hasTransferFromCounterpart baseAcct c q) amtPairs
           || any (\(c, _) -> hasTransferFromCommodityMatch baseAcct c) amtPairs
      return "transfer-to"

    -- Classify a positive lotful posting without cost basis as acquire.
    -- This is the fallback for positive lotful postings that aren't transfer-to.
    -- Requires a plausible cost source: transacted price, different-commodity posting
    -- (for balancer inference), or a transfer-from counterpart (whose lot cost is inherited).
    -- Without any of these, no lot can be created so we skip classification.
    shouldClassifyPositiveLotful :: Posting -> [Amount] -> Maybe Text
    shouldClassifyPositiveLotful p amts = do
      guard $ postingIsLotful p amts
      guard $ any (\a -> aquantity a > 0) amts
      let commodities = S.fromList [acommodity a | a <- amts]
          hasPrice = any (isJust . acost) amts
          hasDiffCommodity = any (\q -> any ((`S.notMember` commodities) . acommodity) (amountsRaw (pamount q)))
                               (filter (\q -> q /= p && hasAmount q) ps)
          baseAcct = lotBaseAccount (paccount p)
          hasTransferFrom = any (\(c, q) -> hasTransferFromCounterpart baseAcct c q)
                              [(acommodity a, aquantity a) | a <- amts]
      guard $ hasPrice || hasDiffCommodity || hasTransferFrom
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
-- The verbosetags parameter controls whether generated-posting tags are made visible in comments.
-- The lotswarn parameter controls whether lot selection errors are warnings (True) or fatal (False).
journalCalculateLots :: Bool -> Bool -> Journal -> Either String Journal
journalCalculateLots = journalCalculateLotsImpl warn

-- | Like 'journalCalculateLots' but suppresses all warning output (no trace/stderr).
-- Useful for a first pass when you only want to detect hard errors without printing warnings.
journalCalculateLotsQuiet :: Bool -> Bool -> Journal -> Either String Journal
journalCalculateLotsQuiet = journalCalculateLotsImpl (\_ x -> x)

-- | Internal implementation of lot calculation, parameterised by the warn function.
journalCalculateLotsImpl :: (forall a. String -> a -> a) -> Bool -> Bool -> Journal -> Either String Journal
journalCalculateLotsImpl warnFn verbosetags lotswarn j
  | not $ any (any isLotPosting . tpostings) txns = warnUnclassified $ Right j
  | otherwise = do
      validateUserLabels txns
      let needsLabels = findDatesNeedingLabels txns
      (_, txns') <- foldM (processTransaction warnFn verbosetags lotswarn j needsLabels) (M.empty, []) (sortOn tdate txns)
      Right $ journalTieTransactions $ j{jtxns = reverse txns'}
  where
    txns = jtxns j
    -- When lotswarn is active, warn about unclassified lotful postings even on the early-exit path
    -- (when no postings were classified at all, foldMPostings never runs).
    warnUnclassified
      | lotswarn  = foldr (\p -> warnFn (unclassifiedLotWarning j p)) `flip`
                      [p | t <- txns, p <- tpostings t, isUnclassifiedLotfulPosting j p]
      | otherwise = id

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
    tagGain = postingAddHiddenAndMaybeVisibleTag True verbosetags (toHiddenTag ("ptype", "gain"))

    disposeHasPrice p = isDisposePosting p && any (isJust . acost) (amountsRaw (pamount p))

    inferGainInTransaction t
      | not (any isDisposePosting (tpostings t)) = Right t
      | not (any disposeHasPrice (tpostings t))  = Right t
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
                          gp = postingAddHiddenAndMaybeVisibleTag False verbosetags (generatedPostingTagName, "")
                                 $ tagGain nullposting{paccount = gainAccount, pamount = inferredAmt}
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

-- | When an implicit-lot-subaccount posting (one whose account was a plain account,
-- not already a lot subaccount) is converted to explicit lot subaccount posting(s),
-- and it had a balance assertion, move the assertion to a new zero-amount generated
-- posting on the original parent account, making it subaccount-inclusive (=* style).
-- This preserves the assertion's meaning when the output is re-read without --lots:
-- the assertion checks the total of all lot subaccounts rather than the (empty)
-- direct balance of the parent.
-- If the original account was already a lot subaccount, the split postings are
-- returned unchanged (the assertion already targets the right account).
preserveParentAssertion :: Bool -> AccountName -> Maybe BalanceAssertion -> [Posting] -> [Posting]
preserveParentAssertion _           _        Nothing  ps = ps
preserveParentAssertion _           origAcct (Just _) ps
    | lotBaseAccount origAcct /= origAcct = ps  -- already an explicit lot subaccount; leave as-is
preserveParentAssertion verbosetags origAcct (Just ba) ps =
    map (\p -> p{pbalanceassertion = Nothing}) ps
    ++ [ postingAddHiddenAndMaybeVisibleTag False verbosetags (generatedPostingTagName, "")
           nullposting
             { paccount          = origAcct
             , pamount           = mixedAmount (baamount ba){aquantity = 0}
             , pbalanceassertion = Just ba{bainclusive = True}
             } ]

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

-- | True if this posting involves a lotful commodity/account in an asset account
-- but has no _ptype tag (wasn't classified as acquire/dispose/transfer/gain).
isUnclassifiedLotfulPosting :: Journal -> Posting -> Bool
isUnclassifiedLotfulPosting j p =
  isReal p
  && hasAmount p
  && any ((/= 0) . aquantity) amts
  && not (isLotPosting p)
  && (any ((== "lots") . T.toLower . fst) (ptags p)
      || any (journalCommodityUsesLots j . acommodity) amts)
  && maybe False isAssetType (journalAccountType j (lotBaseAccount (paccount p)))
  where amts = amountsRaw (pamount p)

-- | Build a warning message for an unclassified lotful posting.
unclassifiedLotWarning :: Journal -> Posting -> String
unclassifiedLotWarning j p =
  let amts = amountsRaw (pamount p)
      lotfulCommodities = [acommodity a | a <- amts, journalCommodityUsesLots j (acommodity a)]
      hasAccountTag = any ((== "lots") . T.toLower . fst) (ptags p)
      source = case (lotfulCommodities, hasAccountTag) of
        (c:_, _)   -> T.unpack c ++ " is declared lotful (commodity lots: tag)"
        ([], True)  -> T.unpack (paccount p) ++ " is declared lotful (account lots: tag)"
        _           -> "posting involves a lotful commodity or account"
  in postingErrPrefix p
     ++ source ++ " but this posting was not classified as\n"
     ++ "acquire, dispose, or transfer. Lot state will not be updated.\n"
     ++ "Possible fixes: add a cost basis ({$X}), a price (@ $X),\n"
     ++ "or check the account type declaration."

-- | Warning for a bare acquire posting that was declassified because no cost basis
-- could be inferred. This catches e.g. prices accidentally left inside comments.
declassifiedAcquireWarning :: Journal -> Posting -> String
declassifiedAcquireWarning j p =
  let amts = amountsRaw (pamount p)
      lotfulCommodities = [acommodity a | a <- amts, journalCommodityUsesLots j (acommodity a)]
      commodity = case lotfulCommodities of
        (c:_) -> T.unpack c
        _     -> "?"
  in postingErrPrefix p
     ++ commodity ++ " is lotful but this acquire posting has no cost basis or price.\n"
     ++ "No lot will be created. If a price was intended, check it is not\n"
     ++ "inside a comment (after ;)."

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

-- | Format a verbose error prefix for a posting: "file:line:\nexcerpt\n\n".
-- Like txnErrPrefix but highlights the specific posting line.
-- Strips costs from the posting before lookup, since makePostingErrorExcerpt
-- matches by comparing cost-stripped transaction postings against the argument.
postingErrPrefix :: Posting -> String
postingErrPrefix p = printf "%s:%d:\n%s\n" f line ex
  where (f, line, _, ex) = makePostingErrorExcerpt (postingStripCosts p) (\_ _ _ -> Nothing)

-- | Emit a dbg5 trace for a lot operation: "lots: FILE:LINE DATE DESC: message".
lotDbg :: Transaction -> String -> a -> a
lotDbg t msg = dbg5With (\_ -> "lots: " ++ txnDbgPrefix t ++ ": " ++ msg)

-- | Format a one-line transaction summary for debug traces: "FILE:LINE DATE DESC".
txnDbgPrefix :: Transaction -> String
txnDbgPrefix t = printf "%s:%d %s %s" f line (show (tdate t)) (T.unpack (tdescription t))
  where (f, line, _, _) = makeTransactionErrorExcerpt t (const Nothing)

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
-- Parameterised by a warn function, allowing callers to suppress or redirect warning output.
processTransaction :: (forall a. String -> a -> a) -> Bool -> Bool -> Journal -> S.Set (CommoditySymbol, Day) -> (LotState, [Transaction]) -> Transaction
                       -> Either String (LotState, [Transaction])
processTransaction warnFn verbosetags lotswarn j needsLabels (ls, acc) t = do
    -- Partition postings into transfer pairs and others
    let (transferFroms, transferTos, otherPs) = partitionTransferPostings (tpostings t)
        hasEquityOther = any (isEquityPosting j) otherPs
    -- Closing equity transfer: transfer-from postings with no transfer-to counterpart,
    -- where an equity posting receives the lots (e.g. close --clopen --lots).
    -- Reduce lots from state; pass all postings through unchanged (equity does not track lots).
    if not (null transferFroms) && null transferTos && hasEquityOther
      then do
        ls' <- if lotswarn
               then foldM (\st p -> case reduceLotTransferToEquity j t st p of
                              Right st' -> Right st'
                              Left err  -> warnFn err $ Right st
                           ) ls transferFroms
               else foldM (reduceLotTransferToEquity j t) ls transferFroms
        return (ls', t : acc)
    -- Opening equity transfer: transfer-to postings with no transfer-from counterpart,
    -- where an equity posting is the source (e.g. opening balances from close --clopen --lots).
    -- Process transfer-to postings as acquires to add lots to the state.
    else if null transferFroms && not (null transferTos) && hasEquityOther
      then do
        -- Build map of processed transfer-to postings, then reconstruct in original order.
        let indexedTos = [(i, p) | (i, p) <- zip [0..] (tpostings t), isTransferToPosting p]
        (ls', toMap) <- foldM (\(st, m) (i, p) -> do
            (st', p') <- processAcquirePosting needsLabels txnDate t st p
            return (st', M.insert i p' m)
          ) (ls, M.empty) indexedTos
        let allPs = [maybe p id (M.lookup i toMap) | (i, p) <- zip [0..] (tpostings t)]
        return (ls', t{tpostings = allPs} : acc)
    else do
        let indexedFroms = [(i, p) | (i, p) <- zip [0..] (tpostings t), isTransferFromPosting p]
            indexedTos   = [(i, p) | (i, p) <- zip [0..] (tpostings t), isTransferToPosting p]
        case pairIndexedTransferPostings t indexedFroms indexedTos of
          Left err | lotswarn -> warnFn err $ Right (ls, t : acc)
          Left err -> Left err
          Right pairs -> do
            -- Process transfer pairs first, building an IntMap from original index to expanded postings.
            (ls', transferMap) <- foldM processOnePair (ls, M.empty) pairs
            -- Walk all postings in original order, substituting expanded results.
            (ls'', allPs) <- foldMPostings ls' [] (zip [0..] (tpostings t)) transferMap
            return (ls'', t{tpostings = reverse allPs} : acc)
  where
    txnDate = tdate t

    -- Process a transfer pair; record expanded postings keyed by original index.
    processOnePair (st, m) (fromIdx, fromP, toIdx, toP) =
      case processTransferPair verbosetags j t st fromP toP of
        Right (st', fromPs, toPs) ->
          let m' = M.insert fromIdx fromPs $ M.insert toIdx toPs m
          in Right (st', m')
        Left err
          | lotswarn ->
              let m' = M.insert fromIdx [fromP] $ M.insert toIdx [toP] m
              in warnFn err $ Right (st, m')
          | otherwise -> Left err

    -- Walk postings in original order, looking up transfer results or processing normally.
    foldMPostings :: LotState -> [Posting] -> [(Int, Posting)] -> M.Map Int [Posting]
                  -> Either String (LotState, [Posting])
    foldMPostings st acc' [] _ = Right (st, acc')
    foldMPostings st acc' ((i,p):ps) tmap
      | Just expanded <- M.lookup i tmap =
          foldMPostings st (reverse expanded ++ acc') ps tmap
      | isAcquirePosting p = do
          (st', p') <- processAcquirePosting needsLabels txnDate t st p
          let go = foldMPostings st' (p':acc') ps tmap
          -- Warn when a bare acquire was declassified because no cost could be inferred.
          -- This catches e.g. prices accidentally left inside comments.
          if lotswarn && not (isAcquirePosting p')
            then warnFn (declassifiedAcquireWarning j p) go
            else go
      | isDisposePosting p =
          case processDisposePosting verbosetags j t st p of
            Right (st', newPs) ->
              foldMPostings st' (reverse newPs ++ acc') ps tmap
            Left err
              | lotswarn ->
                  warnFn err $
                    foldMPostings st (p:acc') ps tmap
              | otherwise -> Left err
      | otherwise =
          let go = foldMPostings st (p:acc') ps tmap
          in if lotswarn && isUnclassifiedLotfulPosting j p
             then warnFn (unclassifiedLotWarning j p) go
             else go

-- | True if the posting is in an equity account.
isEquityPosting :: Journal -> Posting -> Bool
isEquityPosting j p = maybe False isEquityType (journalAccountType j (lotBaseAccount (paccount p)))

-- | Reduce lots from the lot state for a transfer-from posting going to an equity account.
-- Used when lots are transferred to equity (e.g. close --clopen --lots): reduces the lots
-- without requiring a matching transfer-to posting, since equity does not track lots.
reduceLotTransferToEquity :: Journal -> Transaction -> LotState -> Posting -> Either String LotState
reduceLotTransferToEquity j t ls p =
    case [(a, cb) | a <- amountsRaw (pamount p), Just cb <- [acostbasis a], isNegativeAmount a] of
      [(a, cb)] -> do
        let commodity = acommodity a
            qty       = negate (aquantity a)
            acct      = lotBaseAccount (paccount p)
            (method, methodSource) = resolveReductionMethodWithSource j p commodity
        selected <- first (enrichLotError method methodSource acct commodity (tdate t) (journalFilePaths j))
                  $ selectLots method (postingErrPrefix p) acct commodity qty cb ls
        let consumed = [(lotId, qty') | (lotId, _, qty') <- selected]
        return $ lotDbg t ("equity-transfer " ++ show qty ++ " " ++ T.unpack commodity
                           ++ " from " ++ T.unpack acct
                           ++ " (lots: " ++ showSelectedLots selected ++ ")")
               $ reduceLotState acct commodity consumed ls
      _ -> Right ls  -- no single lot amount (e.g. cash posting): pass through

-- | Partition a transaction's postings into transfer-from, transfer-to, and others.
partitionTransferPostings :: [Posting] -> ([Posting], [Posting], [Posting])
partitionTransferPostings = go [] [] []
  where
    go froms tos others [] = (reverse froms, reverse tos, reverse others)
    go froms tos others (p:ps)
      | isTransferFromPosting p = go (p:froms) tos others ps
      | isTransferToPosting p   = go froms (p:tos) others ps
      | otherwise               = go froms tos (p:others) ps

-- | Pair indexed transfer-from and transfer-to postings by commodity.
-- Within each commodity group, froms and tos are sorted by cost basis fields
-- (date, label, cost) so that explicit per-lot pairs align correctly even when
-- interleaved. Cost basis mismatches are caught later by validation, not here.
-- Returns (fromIndex, fromPosting, toIndex, toPosting) tuples.
pairIndexedTransferPostings :: Transaction -> [(Int, Posting)] -> [(Int, Posting)]
                            -> Either String [(Int, Posting, Int, Posting)]
pairIndexedTransferPostings _ [] [] = Right []
pairIndexedTransferPostings t froms tos = do
    fromGroups <- groupByCommodity "transfer-from" froms
    toGroups   <- groupByCommodity "transfer-to" tos
    let allComms = S.union (M.keysSet fromGroups) (M.keysSet toGroups)
    concat <$> mapM (matchCommodityGroup fromGroups toGroups) (S.toList allComms)
  where
    showPos = txnErrPrefix t

    -- Group indexed postings by their lotful commodity.
    groupByCommodity :: String -> [(Int, Posting)] -> Either String (M.Map CommoditySymbol [(Int, Posting)])
    groupByCommodity label ips = do
      tagged <- mapM (\ip -> (,ip) <$> postingCommodity label (snd ip)) ips
      Right $ M.map reverse $ M.fromListWith (++) [(c, [ip]) | (c, ip) <- tagged]

    postingCommodity :: String -> Posting -> Either String CommoditySymbol
    postingCommodity label p =
      case [acommodity a | a <- amountsRaw (pamount p), isJust (acostbasis a)] of
        [c] -> Right c
        -- Transfer-to postings without {} have no cost basis; use the raw commodity.
        _   -> case [acommodity a | a <- amountsRaw (pamount p)] of
                 [c] -> Right c
                 _   -> Left $ showPos ++ label ++ " posting has no lotful commodity"

    -- Sort key for aligning pairs within a commodity group.
    postingSortKey :: (Int, Posting) -> (Maybe Day, Maybe T.Text, Maybe (CommoditySymbol, Quantity))
    postingSortKey (_, p) =
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
          -- when (length fs /= length ts) $
          --   Left $ showPos ++ "mismatched transfer postings for commodity " ++ T.unpack comm
          --              ++ ": " ++ show (length fs) ++ " transfer-from but "
          --              ++ show (length ts) ++ " transfer-to"
          let sortedFs = sortOn postingSortKey fs
              sortedTs = sortOn postingSortKey ts
          Right [(fi, fp, ti, tp) | ((fi, fp), (ti, tp)) <- zip sortedFs sortedTs]

-- | Extract a per-unit cost Amount from an AmountCost, normalising TotalCost by quantity.
-- If quantity is zero, returns the TotalCost amount as-is (avoiding division by zero).
amountCostToUnitCost :: Quantity -> AmountCost -> Amount
amountCostToUnitCost _   (UnitCost c)  = c
amountCostToUnitCost qty (TotalCost c)
  | qty == 0  = c
  | otherwise = c{aquantity = aquantity c / qty}

-- | Normalize an amount's transacted cost to UnitCost form (converting TotalCost by dividing by quantity).
-- Returns Nothing if the amount has no transacted cost.
amountNormalizeCostToUnit :: Amount -> Maybe AmountCost
amountNormalizeCostToUnit a = fmap (UnitCost . amountCostToUnitCost (aquantity a)) (acost a)

-- | Set the quantity of an amount matching the given commodity; leave others unchanged.
amountSetQuantityOf :: CommoditySymbol -> Quantity -> Amount -> Amount
amountSetQuantityOf c q a
  | acommodity a == c = amountSetQuantity q a
  | otherwise         = a

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
            -- If the lot id already exists (e.g. an equity transfer-to on the same date
            -- as a regular acquire, not predicted by findDatesNeedingLabels), auto-generate
            -- a label to disambiguate.
            existingLots = M.findWithDefault M.empty commodity lotState
            lotId0     = LotId date lotLabel'
            (lotId, lotLabel'')
              | isNothing (cbLabel cb) && M.member lotId0 existingLots
                = let l = generateLabel commodity date lotState
                  in (LotId date (Just l), Just l)
              | otherwise = (lotId0, lotLabel')
            fullCb     = CostBasis{cbDate = Just date, cbLabel = lotLabel'', cbCost = Just lotBasis}
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

        let baseAcct = lotBaseAccount (paccount p)
            hasExplicitLotAcct = baseAcct /= paccount p
            expectedAcct = baseAcct <> ":" <> lotName

        -- If the user wrote an explicit lot subaccount, check it matches the resolved lot.
        when (hasExplicitLotAcct && paccount p /= expectedAcct) $
          Left $ showPos ++ "lot subaccount " ++ T.unpack (paccount p)
                  ++ " does not match the resolved lot " ++ T.unpack expectedAcct

        when (M.member lotId existingLots) $
          Left $ showPos ++ "duplicate lot id: " ++ T.unpack lotName
                  ++ " for commodity " ++ T.unpack commodity

        let -- For bare acquires with inferred CB, normalize transacted cost to UnitCost
            -- in the posting's pamount (not in poriginal — that preserves what the user wrote).
            p' = p{paccount = expectedAcct
                   ,pamount  = mixedAmount $ if isBare && cbInferred then postingAmt{acost = amountNormalizeCostToUnit lotAmt} else postingAmt
                   ,poriginal = Just (originalPosting p)}
        let lotState' = addLotState commodity lotId baseAcct lotStateAmt lotState
        return $ lotDbg t ("acquired " ++ show (aquantity lotAmt) ++ " "
                           ++ T.unpack commodity ++ " " ++ T.unpack lotName
                           ++ " on " ++ T.unpack baseAcct)
               (lotState', p')
  where
    showPos = txnErrPrefix t

-- | Process a dispose posting: match to existing lots using the resolved reduction method,
-- split into multiple postings if the disposal spans multiple lots.
-- Returns the list of resulting postings (one per matched lot).
processDisposePosting :: Bool -> Journal -> Transaction -> LotState -> Posting
                      -> Either String (LotState, [Posting])
processDisposePosting verbosetags j t lotState p = do
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

    -- Non-bare dispose (explicit {}) without price is an error.
    -- Bare dispose without price proceeds to lot matching (but skips gain generation).
    case acost lotAmt of
      Nothing | not isBare -> Left $ showPos ++ "dispose posting has no transacted price (selling price) for " ++ T.unpack commodity
      _ -> do

        when (disposeQty >= 0) $
          Left $ showPos ++ "dispose posting has non-negative quantity for " ++ T.unpack commodity

        let posQty = negate disposeQty
            (method, methodSource) = resolveReductionMethodWithSource j p commodity
            -- All methods are per-account, scoped to the posting's base account
            -- (stripping any explicit lot subaccount the user may have written).
            scopeAcct = lotBaseAccount (paccount p)

        when (isBare && method == SPECID) $
          Left $ showPos ++ "SPECID requires a lot selector on dispose postings"

        selected <- first (enrichLotError method methodSource scopeAcct commodity (tdate t) (journalFilePaths j))
                  $ selectLots method (postingErrPrefix p) scopeAcct commodity posQty cb lotState

        -- For AVERAGE methods, compute the weighted average cost across the pool.
        -- AVERAGEALL uses the global pool (all accounts); AVERAGE uses per-account scope.
        mavgCost <- if methodIsAverage method
          then do
            let allLots = M.findWithDefault M.empty commodity lotState
                flatLots = if methodIsGlobal method
                           then flattenAllAccountLots allLots
                           else M.mapMaybe (M.lookup scopeAcct) allLots
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
                  disposeAmt = (amountSetQuantity (negate consumedQty) lotAmt){acostbasis = Just dispCb}
              let -- For bare disposes with a price, normalize transacted cost to UnitCost.
                  -- For bare disposes without a price (e.g. fee deductions), keep no cost.
                  disposeAmt' | isNothing (acost lotAmt) = disposeAmt{acost = Nothing}
                              | isBare    = disposeAmt{acost = amountNormalizeCostToUnit lotAmt}
                              | otherwise = disposeAmt
                  -- poriginal preserves the user's original annotations, only updating quantity.
                  origP  = originalPosting p
                  origP' = origP{pamount = mapMixedAmount (amountSetQuantityOf commodity (negate consumedQty)) $ pamount origP}
              Right p{ paccount  = acctWithLot
                     , pamount   = mixedAmount disposeAmt'
                     , poriginal = Just origP'
                     }

        newPostings <- mapM mkPosting selected
        let consumed  = [(lotId, qty) | (lotId, _, qty) <- selected]
            lotState' = reduceLotState scopeAcct commodity consumed lotState

        return $ lotDbg t ("disposed " ++ show posQty ++ " " ++ T.unpack commodity
                           ++ " from " ++ T.unpack scopeAcct
                           ++ " (" ++ show method ++ ", lots: " ++ showSelectedLots selected ++ ")")
               (lotState', preserveParentAssertion verbosetags (paccount p) (pbalanceassertion p) newPostings)
  where
    showPos = txnErrPrefix t

-- | Process a transfer pair: select lots from the source account (transfer-from)
-- and recreate them under the destination account (transfer-to).
-- Returns updated LotState and two lists of expanded postings (from, to).
processTransferPair :: Bool -> Journal -> Transaction -> LotState -> Posting -> Posting
                    -> Either String (LotState, [Posting], [Posting])
processTransferPair verbosetags j t lotState fromP toP = do
    -- Extract lotful amount and lot selector from transfer-from.
    -- When cost basis is present, use it directly as the lot selector.
    -- When absent (bare transfer on a lotful commodity), use a wildcard selector.
    let fromAmts = [(a, cb) | a <- amountsRaw (pamount fromP), Just cb <- [acostbasis a]]
    (fromAmt, fromCb) <- case fromAmts of
      [x] -> Right x
      _   -> do
        let bareAmts = [a | a <- amountsRaw (pamount fromP), isNegativeAmount a]
        case bareAmts of
          [a] -> Right (a, CostBasis Nothing Nothing Nothing)
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

    let fromQty = negate transferQty
        -- Detect fee: if transfer-to has less qty than transfer-from, the difference is a fee.
        toQty = case [aquantity a | a <- toAmts, acommodity a == commodity, aquantity a > 0] of
                  [q] | q < fromQty -> q
                  _                 -> fromQty
        feeQty = fromQty - toQty
        -- Transfers are always per-account (scoped to source), but ordering follows the method.
        (method, methodSource) = resolveReductionMethodWithSource j fromP commodity
        fromBaseAcct = lotBaseAccount (paccount fromP)

    -- Select lots from source account for the full fromQty
    selected <- first (enrichLotError method methodSource fromBaseAcct commodity (tdate t) (journalFilePaths j))
              $ selectLots method (postingErrPrefix fromP) fromBaseAcct commodity fromQty fromCb lotState

    -- Split selected lots into transfer portion and fee portion
    let (transferLots, feeLots) = if feeQty > 0
                                  then splitLotsAt toQty selected
                                  else (selected, [])

    -- Extract the transfer-to cost basis for optional validation
    let toCb = case [(a, cb) | a <- toAmts, Just cb <- [acostbasis a]] of
                 [(_, cb)] -> Just cb
                 _         -> Nothing

    -- For each transfer lot, generate from and to postings
    (transferFromPs, toPs) <- fmap unzip $ mapM (mkTransferPostings fromAmt toCb commodity) transferLots

    -- For fee lots, generate from-only postings (lots consumed from source, no destination)
    feeFromPs <- mapM (mkFeeFromPosting fromAmt commodity) feeLots

    -- Update lot state: remove full fromQty from source, add only transfer portion to destination
    let toBaseAcct   = lotBaseAccount (paccount toP)
        consumed   = [(lotId, qty) | (lotId, _, qty) <- selected]
        lotState'  = reduceLotState fromBaseAcct commodity consumed lotState
        lotState'' = foldl' (addTransferredLot commodity toBaseAcct) lotState' transferLots

    return $ lotDbg t ("transferred " ++ show fromQty ++ " " ++ T.unpack commodity
                        ++ " from " ++ T.unpack fromBaseAcct ++ " to " ++ T.unpack toBaseAcct
                        ++ " (lots: " ++ showSelectedLots selected
                        ++ if feeQty > 0 then "; fee: " ++ show feeQty ++ ")" else ")")
           ( lotState''
           , preserveParentAssertion verbosetags (paccount fromP) (pbalanceassertion fromP) (transferFromPs ++ feeFromPs)
           , preserveParentAssertion verbosetags (paccount toP)   (pbalanceassertion toP)   toPs
           )
  where
    showPos = txnErrPrefix t

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
            -- Use base accounts to avoid double-appending lot subaccounts.
            fromAcct = lotBaseAccount (paccount fromP) <> ":" <> lotName
            toAcct   = lotBaseAccount (paccount toP)   <> ":" <> lotName

        -- Validate transfer-to cost basis if it has specific fields
        validateToCb toCb lotCb commodity

        let fromAmt' = (amountSetQuantity (negate consumedQty) fromAmt){acostbasis = Just lotCb}
            toAmt'   = amountSetQuantity consumedQty fromAmt'
            -- poriginal preserves the user's original annotations, only updating quantity.
            origFromP = originalPosting fromP
            origFromP' = origFromP{pamount = mapMixedAmount (amountSetQuantityOf commodity (negate consumedQty)) (pamount origFromP)}
            origToP = originalPosting toP
            origToP' = origToP{pamount = mapMixedAmount (amountSetQuantityOf commodity consumedQty) (pamount origToP)}
            fromP' = fromP{ paccount = fromAcct
                          , pamount  = mixedAmount fromAmt'
                          , poriginal = Just origFromP'
                          }
            toP'   = toP{ paccount = toAcct
                        , pamount  = mixedAmount toAmt'
                        , poriginal = Just origToP'
                        }
        Right (fromP', toP')

    -- Generate a from-only posting for a fee-consumed lot (no destination).
    mkFeeFromPosting fromAmt commodity (lotId, storedAmt, consumedQty) = do
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
            fromAcct = lotBaseAccount (paccount fromP) <> ":" <> lotName
            fromAmt' = (amountSetQuantity (negate consumedQty) fromAmt){acostbasis = Just lotCb, acost = Nothing}
            origFromP = originalPosting fromP
            origFromP' = origFromP{pamount = mapMixedAmount (amountSetQuantityOf commodity (negate consumedQty)) (pamount origFromP)}
        Right fromP{ paccount  = fromAcct
                   , pamount   = mixedAmount fromAmt'
                   , poriginal = Just origFromP'
                   }

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
      in addLotState commodity lotId destAcct amt ls

-- Lot state operations

-- | Add an amount to LotState for a specific account/commodity/lot.
-- If the lot already exists on that account, quantities are summed (not overwritten).
-- This can happen when the same lot is transferred to the same destination account
-- by multiple transactions (e.g. two transfers on the same date both move portions
-- of the same lot).
addLotState :: CommoditySymbol -> LotId -> AccountName -> Amount -> LotState -> LotState
addLotState commodity lotId account amt =
  M.insertWith (M.unionWith (M.unionWith addQty)) commodity
    (M.singleton lotId (M.singleton account amt))
  where addQty a1 a2 = a1{aquantity = aquantity a1 + aquantity a2}

-- | Enrich a selectLots error with reduction method info and a review hint.
enrichLotError :: ReductionMethod -> String -> AccountName -> CommoditySymbol
               -> Day -> [FilePath] -> String -> String
enrichLotError method methodSource account commodity txnDate files err =
  err ++ "\nUsing " ++ show method ++ " (" ++ methodSource ++ ")."
      ++ "\nTo review lot movements: hledger"
      ++ concatMap (" -f " ++) files
      ++ " reg " ++ T.unpack account ++ " cur:" ++ T.unpack commodity
      ++ " --lots-warn -e " ++ T.unpack (showDate (addDays 1 txnDate))
      ++ " --verbose-tags"

-- | Select lots to consume using the given reduction method.
-- All methods select from the specified account only.
-- Ordering: FIFO\/FIFOALL oldest-first; LIFO\/LIFOALL newest-first;
-- HIFO\/HIFOALL highest per-unit cost first; AVERAGE\/AVERAGEALL FIFO order;
-- SPECID requires an explicit selector matching one lot.
-- The *ALL variants additionally validate that the selected lots would also be
-- chosen first if all accounts' lots were considered together (see 'validateGlobalCompliance').
-- The lot selector filters which lots are eligible: each non-Nothing field
-- in the selector must match the corresponding field in the lot's cost basis.
-- An all-Nothing selector (from @{}@) matches all lots.
-- Returns a list of (lot id, lot amount, quantity consumed from this lot).
-- Errors if total available quantity in matching lots is insufficient.
selectLots :: ReductionMethod -> String -> AccountName -> CommoditySymbol
           -> Quantity -> CostBasis -> LotState
           -> Either String [(LotId, Amount, Quantity)]
selectLots method posStr account commodity qty selector lotState = do
    when (method == SPECID && isWildcardSelector selector) $
      Left $ posStr ++ "SPECID requires an explicit lot selector"
    let allLots = M.findWithDefault M.empty commodity lotState
        -- Flatten to (LotId, Amount) pairs, taking only the specified account's balance.
        flatLots = M.mapMaybe (M.lookup account) allLots
        matchingLots = M.filter (lotMatchesSelector selector) flatLots
    when (M.null matchingLots) $
      Left $ posStr ++ "no lots available for commodity " ++ T.unpack commodity
              ++ " in account " ++ T.unpack account
              ++ showOtherAccountLots allLots
    when (method == SPECID && M.size matchingLots > 1) $
      Left $ posStr ++ "lot selector is ambiguous, matches " ++ show (M.size matchingLots)
              ++ " lots in account " ++ T.unpack account ++ ":"
              ++ showLotList matchingLots
    let available = sum [aquantity a | a <- M.elems matchingLots]
    when (available < qty) $
      Left $ posStr ++ "insufficient lots for commodity " ++ T.unpack commodity
              ++ " in account " ++ T.unpack account
              ++ ": need " ++ show qty ++ " but only " ++ show available ++ " available"
              ++ "\nAvailable lots:" ++ showLotList matchingLots
              ++ showOtherAccountLots allLots
    let base = methodBaseOrdering method
        orderedLots = case base of
          FIFO    -> M.toAscList matchingLots
          LIFO    -> M.toDescList matchingLots
          HIFO    -> sortOn (Down . lotPerUnitCost) (M.toList matchingLots)
          AVERAGE -> M.toAscList matchingLots
          SPECID  -> M.toAscList matchingLots
          _       -> M.toAscList matchingLots  -- unreachable after methodBaseOrdering
        selected = go qty orderedLots
    when (methodIsGlobal method) $
      validateGlobalCompliance method posStr account commodity qty selector lotState selected
    Right selected
  where
    go 0 _ = []
    go _ [] = []  -- shouldn't happen after the check above
    go remaining ((lotId, lotAmt):rest)
      | remaining >= lotBal = (lotId, lotAmt, lotBal) : go (remaining - lotBal) rest
      | otherwise           = [(lotId, lotAmt, remaining)]
      where lotBal = aquantity lotAmt

    showLotList :: M.Map LotId Amount -> String
    showLotList lots = concatMap fmt (M.toAscList lots)
      where fmt (lid, a) = "\n  " ++ T.unpack (showLotName (lotIdToCb lid a))
                            ++ "  " ++ show (aquantity a)

    showOtherAccountLots :: M.Map LotId (M.Map AccountName Amount) -> String
    showOtherAccountLots allLots' =
      let others = [(acct, lid, a) | (lid, acctMap) <- M.toAscList allLots'
                                    , (acct, a) <- M.toList acctMap, acct /= account]
          byAcct = M.fromListWith (++) [(acct, [(lid, a)]) | (acct, lid, a) <- others]
      in if M.null byAcct then ""
         else "\nLots of " ++ T.unpack commodity ++ " on other accounts:"
           ++ concatMap fmtAcct (M.toAscList byAcct)
      where fmtAcct (acct, lots) = "\n  " ++ T.unpack acct ++ ": "
              ++ intercalate ", " [T.unpack (showLotName (lotIdToCb lid a)) ++ " " ++ show (aquantity a)
                                  | (lid, a) <- lots]

-- | Split a list of selected lots at a quantity boundary.
-- Returns (lots for the first portion, lots for the remainder).
-- Used to separate transfer and fee portions when transfer qty < source qty.
splitLotsAt :: Quantity -> [(LotId, Amount, Quantity)]
            -> ([(LotId, Amount, Quantity)], [(LotId, Amount, Quantity)])
splitLotsAt 0 lots = ([], lots)
splitLotsAt _ [] = ([], [])
splitLotsAt remaining ((lid, amt, qty):rest)
  | remaining >= qty = let (a, b) = splitLotsAt (remaining - qty) rest
                       in ((lid, amt, qty):a, b)
  | otherwise = ([(lid, amt, remaining)], (lid, amt, qty - remaining):rest)

-- | Extract the per-unit cost quantity from a lot entry, for HIFO sorting.
lotPerUnitCost :: (LotId, Amount) -> Quantity
lotPerUnitCost (_, a) = maybe 0 aquantity (acostbasis a >>= cbCost)

-- | Whether a reduction method uses weighted average cost basis for disposals.
methodIsAverage :: ReductionMethod -> Bool
methodIsAverage AVERAGE    = True
methodIsAverage AVERAGEALL = True
methodIsAverage _          = False

-- | Whether a reduction method requires global validation across all accounts.
methodIsGlobal :: ReductionMethod -> Bool
methodIsGlobal FIFOALL    = True
methodIsGlobal LIFOALL    = True
methodIsGlobal HIFOALL    = True
methodIsGlobal AVERAGEALL = True
methodIsGlobal _          = False

-- | Map a *ALL method to its base ordering, or return the method unchanged.
methodBaseOrdering :: ReductionMethod -> ReductionMethod
methodBaseOrdering FIFOALL    = FIFO
methodBaseOrdering LIFOALL    = LIFO
methodBaseOrdering HIFOALL    = HIFO
methodBaseOrdering AVERAGEALL = AVERAGE
methodBaseOrdering m          = m

-- | Flatten lots across all accounts, summing quantities for shared lot IDs.
-- From @Map LotId (Map AccountName Amount)@ to @Map LotId Amount@,
-- combining quantities across accounts (taking the first Amount's metadata).
flattenAllAccountLots :: M.Map LotId (M.Map AccountName Amount) -> M.Map LotId Amount
flattenAllAccountLots = M.mapMaybe flattenAccts
  where
    flattenAccts acctMap =
      case M.elems acctMap of
        []       -> Nothing
        (a:rest) -> Just a{aquantity = aquantity a + sum (map aquantity rest)}

-- | Validate that the per-account selected lots would also be chosen first
-- under a global ordering across all accounts. Errors if lots on other accounts
-- have higher priority than the selected lots.
validateGlobalCompliance :: ReductionMethod -> String -> AccountName -> CommoditySymbol
                         -> Quantity -> CostBasis -> LotState
                         -> [(LotId, Amount, Quantity)] -> Either String ()
validateGlobalCompliance method posStr account commodity qty selector lotState selected = do
    let allLots = M.findWithDefault M.empty commodity lotState
        globalFlat = flattenAllAccountLots allLots
        globalMatching = M.filter (lotMatchesSelector selector) globalFlat
        base = methodBaseOrdering method
        globalOrdered = case base of
          FIFO    -> M.toAscList globalMatching
          LIFO    -> M.toDescList globalMatching
          HIFO    -> sortOn (Down . lotPerUnitCost) (M.toList globalMatching)
          AVERAGE -> M.toAscList globalMatching
          _       -> M.toAscList globalMatching
        -- Greedily consume qty from the globally-ordered list
        globalSelectedIds = S.fromList $ map fst3 $ goConsume qty globalOrdered
        selectedIds = S.fromList [lid | (lid, _, _) <- selected]
        -- Lot IDs that would be globally selected but are NOT in the per-account selection
        -- (i.e. they exist on other accounts and have higher priority)
        higherPriorityElsewhere = S.difference globalSelectedIds selectedIds
    unless (S.null higherPriorityElsewhere) $ do
      -- Build detailed error showing which accounts hold the higher-priority lots
      let otherLots = [(acct, lid, a)
                      | lid <- S.toList higherPriorityElsewhere
                      , Just acctMap <- [M.lookup lid allLots]
                      , (acct, a) <- M.toList acctMap]
          byAcct = M.fromListWith (++) [(acct, [(lid, a)]) | (acct, lid, a) <- otherLots]
          fmtAcct (acct, lots) = "\n  " ++ T.unpack acct ++ ": "
            ++ intercalate ", " [T.unpack (showLotName (lotIdToCb lid a)) ++ "  " ++ show (aquantity a)
                                | (lid, a) <- lots]
          fmtSelected = concatMap (\(lid, a, q) -> "\n  " ++ T.unpack (showLotName (lotIdToCb lid a))
                                                    ++ "  " ++ show q) selected
      Left $ posStr ++ show method ++ ": lot(s) on other account(s) have higher priority than the lots in "
              ++ T.unpack account ++ ":"
              ++ concatMap fmtAcct (M.toAscList byAcct)
              ++ "\nSelected from " ++ T.unpack account ++ ":"
              ++ fmtSelected
              ++ "\nConsider disposing from the account(s) listed above first, or use "
              ++ show (methodBaseOrdering method) ++ " for per-account scope."
  where
    goConsume 0 _ = []
    goConsume _ [] = []
    goConsume remaining ((lotId, lotAmt):rest)
      | remaining >= aquantity lotAmt = (lotId, lotAmt, aquantity lotAmt) : goConsume (remaining - aquantity lotAmt) rest
      | otherwise = [(lotId, lotAmt, remaining)]
    fst3 (x, _, _) = x

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
            in if totalQty == 0
               then Left $ posStr ++ "cannot average lots with zero total quantity"
               else Right firstCost{aquantity = totalCost / totalQty}

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

-- | Subtract consumed quantities from LotState for a specific account.
-- Removes lot-account entries whose balance reaches zero.
-- Removes the lot entirely if no accounts remain.
reduceLotState :: AccountName -> CommoditySymbol -> [(LotId, Quantity)] -> LotState -> LotState
reduceLotState account commodity consumed = M.adjust adjustCommodity commodity
  where
    adjustCommodity lots = foldl' reduceLot lots consumed
    reduceLot lots (lotId, qty) = M.update shrinkLot lotId lots
      where
        shrinkLot acctMap =
          let acctMap' = M.update (shrinkAmt qty) account acctMap
          in if M.null acctMap' then Nothing else Just acctMap'
        shrinkAmt q a
          | aquantity a <= q = Nothing
          | otherwise        = Just a{aquantity = aquantity a - q}

-- Debug trace helpers

-- | Reconstruct a CostBasis from a LotId and a stored Amount (for display in trace messages).
lotIdToCb :: LotId -> Amount -> CostBasis
lotIdToCb lid a = CostBasis (Just (lotDate lid)) (lotLabel lid) (acostbasis a >>= cbCost)

-- | Show selected lots for trace messages: "{2026-01-15, $50} 5, {2026-02-01, $60} 3"
showSelectedLots :: [(LotId, Amount, Quantity)] -> String
showSelectedLots = intercalate ", " . map fmt
  where fmt (lid, a, qty) = T.unpack (showLotName (lotIdToCb lid a)) ++ " " ++ show qty
