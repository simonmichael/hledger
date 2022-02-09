{-|

A 'Transaction' represents a movement of some commodity(ies) between two
or more accounts. It consists of multiple account 'Posting's which balance
to zero, a date, and optional extras like description, cleared status, and
tags.

-}

{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module Hledger.Data.Transaction
( -- * Transaction
  nulltransaction
, transaction
, txnTieKnot
, txnUntieKnot
  -- * operations
, hasRealPostings
, realPostings
, assignmentPostings
, virtualPostings
, balancedVirtualPostings
, transactionsPostings
, transactionTransformPostings
, transactionApplyValuation
, transactionToCost
, transactionAddInferredEquityPostings
, transactionAddPricesFromEquity
, transactionApplyAliases
, transactionMapPostings
, transactionMapPostingAmounts
  -- nonzerobalanceerror
  -- * date operations
, transactionDate2
, transactionDateOrDate2
  -- * transaction description parts
, transactionPayee
, transactionNote
  -- payeeAndNoteFromDescription
  -- * rendering
, showTransaction
, showTransactionOneLineAmounts
, showTransactionLineFirstPart
, transactionFile
  -- * transaction errors
, annotateErrorWithTransaction
  -- * tests
, tests_Transaction
) where

import Control.Monad.Trans.State (StateT(..), evalStateT)
import Data.Bifunctor (first)
import Data.Foldable (foldrM)
import Data.Maybe (fromMaybe, isJust, mapMaybe)
import Data.Semigroup (Endo(..))
import Data.Text (Text)
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TB
import Data.Time.Calendar (Day, fromGregorian)

import Hledger.Utils
import Hledger.Data.Types
import Hledger.Data.Dates
import Hledger.Data.Posting
import Hledger.Data.Amount
import Hledger.Data.Valuation


nulltransaction :: Transaction
nulltransaction = Transaction {
                    tindex=0,
                    tsourcepos=nullsourcepos,
                    tdate=nulldate,
                    tdate2=Nothing,
                    tstatus=Unmarked,
                    tcode="",
                    tdescription="",
                    tcomment="",
                    ttags=[],
                    tpostings=[],
                    tprecedingcomment=""
                  }

-- | Make a simple transaction with the given date and postings.
transaction :: Day -> [Posting] -> Transaction
transaction day ps = txnTieKnot $ nulltransaction{tdate=day, tpostings=ps}

transactionPayee :: Transaction -> Text
transactionPayee = fst . payeeAndNoteFromDescription . tdescription

transactionNote :: Transaction -> Text
transactionNote = snd . payeeAndNoteFromDescription . tdescription

-- | Parse a transaction's description into payee and note (aka narration) fields,
-- assuming a convention of separating these with | (like Beancount).
-- Ie, everything up to the first | is the payee, everything after it is the note.
-- When there's no |, payee == note == description.
payeeAndNoteFromDescription :: Text -> (Text,Text)
payeeAndNoteFromDescription t
  | T.null n = (t, t)
  | otherwise = (T.strip p, T.strip $ T.drop 1 n)
  where
    (p, n) = T.span (/= '|') t

{-|
Render a journal transaction as text similar to the style of Ledger's print command.

Adapted from Ledger 2.x and 3.x standard format:

@
yyyy-mm-dd[ *][ CODE] description.........          [  ; comment...............]
    account name 1.....................  ...$amount1[  ; comment...............]
    account name 2.....................  ..$-amount1[  ; comment...............]

pcodewidth    = no limit -- 10          -- mimicking ledger layout.
pdescwidth    = no limit -- 20          -- I don't remember what these mean,
pacctwidth    = 35 minimum, no maximum  -- they were important at the time.
pamtwidth     = 11
pcommentwidth = no limit -- 22
@

The output will be parseable journal syntax.
To facilitate this, postings with explicit multi-commodity amounts
are displayed as multiple similar postings, one per commodity.
(Normally does not happen with this function).
-}
showTransaction :: Transaction -> Text
showTransaction = TL.toStrict . TB.toLazyText . showTransactionHelper False

-- | Like showTransaction, but explicit multi-commodity amounts
-- are shown on one line, comma-separated. In this case the output will
-- not be parseable journal syntax.
showTransactionOneLineAmounts :: Transaction -> Text
showTransactionOneLineAmounts = TL.toStrict . TB.toLazyText . showTransactionHelper True

-- | Helper for showTransaction*.
showTransactionHelper :: Bool -> Transaction -> TB.Builder
showTransactionHelper onelineamounts t =
      TB.fromText descriptionline <> newline
    <> foldMap ((<> newline) . TB.fromText) newlinecomments
    <> foldMap ((<> newline) . TB.fromText) (postingsAsLines onelineamounts $ tpostings t)
    <> newline
  where
    descriptionline = T.stripEnd $ showTransactionLineFirstPart t <> T.concat [desc, samelinecomment]
    desc = if T.null d then "" else " " <> d where d = tdescription t
    (samelinecomment, newlinecomments) =
      case renderCommentLines (tcomment t) of []   -> ("",[])
                                              c:cs -> (c,cs)
    newline = TB.singleton '\n'

-- Useful when rendering error messages.
showTransactionLineFirstPart t = T.concat [date, status, code]
  where
    date = showDate (tdate t) <> maybe "" (("="<>) . showDate) (tdate2 t)
    status | tstatus t == Cleared = " *"
           | tstatus t == Pending = " !"
           | otherwise            = ""
    code = if T.null (tcode t) then "" else wrap " (" ")" $ tcode t

hasRealPostings :: Transaction -> Bool
hasRealPostings = not . null . realPostings

realPostings :: Transaction -> [Posting]
realPostings = filter isReal . tpostings

assignmentPostings :: Transaction -> [Posting]
assignmentPostings = filter hasBalanceAssignment . tpostings

virtualPostings :: Transaction -> [Posting]
virtualPostings = filter isVirtual . tpostings

balancedVirtualPostings :: Transaction -> [Posting]
balancedVirtualPostings = filter isBalancedVirtual . tpostings

transactionsPostings :: [Transaction] -> [Posting]
transactionsPostings = concatMap tpostings

-- Get a transaction's secondary date, or the primary date if there is none.
transactionDate2 :: Transaction -> Day
transactionDate2 t = fromMaybe (tdate t) $ tdate2 t

-- Get a transaction's primary or secondary date, as specified.
transactionDateOrDate2 :: WhichDate -> Transaction -> Day
transactionDateOrDate2 PrimaryDate   = tdate
transactionDateOrDate2 SecondaryDate = transactionDate2

-- | Ensure a transaction's postings refer back to it, so that eg
-- relatedPostings works right.
txnTieKnot :: Transaction -> Transaction
txnTieKnot t@Transaction{tpostings=ps} = t' where
    t' = t{tpostings=map (postingSetTransaction t') ps}

-- | Ensure a transaction's postings do not refer back to it, so that eg
-- recursiveSize and GHCI's :sprint work right.
txnUntieKnot :: Transaction -> Transaction
txnUntieKnot t@Transaction{tpostings=ps} = t{tpostings=map (\p -> p{ptransaction=Nothing}) ps}

-- | Set a posting's parent transaction.
postingSetTransaction :: Transaction -> Posting -> Posting
postingSetTransaction t p = p{ptransaction=Just t}

-- | Apply a transform function to this transaction's amounts.
transactionTransformPostings :: (Posting -> Posting) -> Transaction -> Transaction
transactionTransformPostings f t@Transaction{tpostings=ps} = t{tpostings=map f ps}

-- | Apply a specified valuation to this transaction's amounts, using
-- the provided price oracle, commodity styles, and reference dates.
-- See amountApplyValuation.
transactionApplyValuation :: PriceOracle -> M.Map CommoditySymbol AmountStyle -> Day -> Day -> ValuationType -> Transaction -> Transaction
transactionApplyValuation priceoracle styles periodlast today v =
  transactionTransformPostings (postingApplyValuation priceoracle styles periodlast today v)

-- | Maybe convert this 'Transaction's amounts to cost and apply the
-- appropriate amount styles.
transactionToCost :: M.Map CommoditySymbol AmountStyle -> ConversionOp -> Transaction -> Transaction
transactionToCost styles cost t = t{tpostings = mapMaybe (postingToCost styles cost) $ tpostings t}

-- | Add inferred equity postings to a 'Transaction' using transaction prices.
transactionAddInferredEquityPostings :: AccountName -> Transaction -> Transaction
transactionAddInferredEquityPostings equityAcct t =
    t{tpostings=concatMap (postingAddInferredEquityPostings equityAcct) $ tpostings t}

-- | Add inferred transaction prices from equity postings. For every adjacent
-- pair of conversion postings, it will first search the postings with
-- transaction prices to see if any match. If so, it will tag it as matched.
-- If no postings with transaction prices match, it will then search the
-- postings without transaction prices, and will match the first such posting
-- which matches one of the conversion amounts. If it finds a match, it will
-- add a transaction price and then tag it.
type IdxPosting = (Int, Posting)
transactionAddPricesFromEquity :: M.Map AccountName AccountType -> Transaction -> Either String Transaction
transactionAddPricesFromEquity acctTypes t = first (annotateErrorWithTransaction t . T.unpack) $ do
    (conversionPairs, stateps) <- partitionPs npostings
    f <- transformIndexedPostingsF addPricesToPostings conversionPairs stateps
    return t{tpostings = map (snd . f) npostings}
  where
    -- Include indices for postings
    npostings = zip [0..] $ tpostings t
    transformIndexedPostingsF f = evalStateT . fmap (appEndo . foldMap Endo) . traverse f

    -- Sort postings into pairs of conversion postings, transaction price postings, and other postings
    partitionPs = fmap fst . foldrM select (([], ([], [])), Nothing)
    select np@(_, p) ((cs, others@(ps, os)), Nothing)
      | isConversion p = Right ((cs, others),      Just np)
      | hasPrice p     = Right ((cs, (np:ps, os)), Nothing)
      | otherwise      = Right ((cs, (ps, np:os)), Nothing)
    select np@(_, p) ((cs, others), Just last)
      | isConversion p = Right (((last, np):cs, others), Nothing)
      | otherwise      = Left "Conversion postings must occur in adjacent pairs"

    -- Given a pair of indexed conversion postings, and a state consisting of lists of
    -- priced and unpriced non-conversion postings, create a function which adds transaction
    -- prices to the posting which matches the conversion postings if necessary, and tags
    -- the conversion and matched postings. Then update the state by removing the matched
    -- postings. If there are no matching postings or too much ambiguity, return an error
    -- string annotated with the conversion postings.
    addPricesToPostings :: (IdxPosting, IdxPosting)
                        -> StateT ([IdxPosting], [IdxPosting]) (Either Text) (IdxPosting -> IdxPosting)
    addPricesToPostings ((n1, cp1), (n2, cp2)) = StateT $ \(priceps, otherps) -> do
        -- Get the two conversion posting amounts, if possible
        ca1 <- postingAmountNoPrice cp1
        ca2 <- postingAmountNoPrice cp2
        let -- The function to add transaction prices and tag postings in the indexed list of postings
            transformPostingF np pricep = \(n, p) ->
                (n, if | n == np            -> pricep `postingAddTags` [("_price-matched","")]
                       | n == n1 || n == n2 -> p      `postingAddTags` [("_conversion-matched","")]
                       | otherwise          -> p)
            -- All priced postings which match the conversion posting pair
            matchingPricePs = mapMaybe (mapM $ pricedPostingIfMatchesBothAmounts ca1 ca2) priceps
            -- All other postings which match at least one of the conversion posting pair
            matchingOtherPs = mapMaybe (mapM $ addPriceIfMatchesOneAmount ca1 ca2) otherps

        -- Annotate any errors with the conversion posting pair
        first (annotateWithPostings [cp1, cp2]) $
            if -- If a single transaction price posting matches the conversion postings,
               -- delete it from the list of priced postings in the state, delete the
               -- first matching unpriced posting from the list of non-priced postings
               -- in the state, and return the transformation function with the new state.
               | [(np, (pricep, _))] <- matchingPricePs
               , Just newpriceps <- deleteIdx np priceps
                   -> Right (transformPostingF np pricep, (newpriceps, otherps))
               -- If no transaction price postings match the conversion postings, but some
               -- of the unpriced postings match, check that the first such posting has a
               -- different amount from all the others, and if so add a transaction price to
               -- it, then delete it from the list of non-priced postings in the state, and
               -- return the transformation function with the new state.
               | [] <- matchingPricePs
               , (np, (pricep, amt)):nps <- matchingOtherPs
               , not $ any (amountMatches amt . snd . snd) nps
               , Just newotherps <- deleteIdx np otherps
                   -> Right (transformPostingF np pricep, (priceps, newotherps))
               -- Otherwise it's too ambiguous to make a guess, so return an error.
               | otherwise -> Left "There is not a unique posting which matches the conversion posting pair:"

    -- If a posting with transaction price matches both the conversion amounts, return it along
    -- with the matching amount which must be present in another non-conversion posting.
    pricedPostingIfMatchesBothAmounts :: Amount -> Amount -> Posting -> Maybe (Posting, Amount)
    pricedPostingIfMatchesBothAmounts a1 a2 p = do
        a@Amount{aprice=Just _} <- postingSingleAmount p
        if | amountMatches (-a1) a && amountMatches a2 (amountCost a) -> Just (p, -a2)
           | amountMatches (-a2) a && amountMatches a1 (amountCost a) -> Just (p, -a1)
           | otherwise -> Nothing

    -- Add a transaction price to a posting if it matches (negative) one of the
    -- supplied conversion amounts, adding the other amount as the price
    addPriceIfMatchesOneAmount :: Amount -> Amount -> Posting -> Maybe (Posting, Amount)
    addPriceIfMatchesOneAmount a1 a2 p = do
        a <- postingSingleAmount p
        let newp price = p{pamount = mixedAmount a{aprice = Just $ TotalPrice price}}
        if | amountMatches (-a1) a -> Just (newp a2, a2)
           | amountMatches (-a2) a -> Just (newp a1, a1)
           | otherwise             -> Nothing

    hasPrice p = isJust $ aprice =<< postingSingleAmount p
    postingAmountNoPrice p = case postingSingleAmount p of
        Just a@Amount{aprice=Nothing} -> Right a
        _ -> Left $ annotateWithPostings [p] "The posting must only have a single amount with no transaction price"
    postingSingleAmount p = case amountsRaw (pamount p) of
        [a] -> Just a
        _   -> Nothing

    amountMatches a b = acommodity a == acommodity b && aquantity a == aquantity b
    isConversion p = M.lookup (paccount p) acctTypes == Just Conversion

    -- Delete a posting from the indexed list of postings based on either its
    -- index or its posting amount.
    -- Note: traversing the whole list to delete a single match is generally not efficient,
    -- but given that a transaction probably doesn't have more than four postings, it should
    -- still be more efficient than using a Map or another data structure. Even monster
    -- transactions with up to 10 postings, which are generally not a good
    -- idea, are still too small for there to be an advantage.
    deleteIdx n = deleteUniqueMatch ((n==) . fst)
    deleteUniqueMatch p (x:xs) | p x       = if any p xs then Nothing else Just xs
                               | otherwise = (x:) <$> deleteUniqueMatch p xs
    deleteUniqueMatch _ []                 = Nothing
    annotateWithPostings xs str = T.unlines $ str : postingsAsLines False xs

-- | Apply some account aliases to all posting account names in the transaction, as described by accountNameApplyAliases.
-- This can fail due to a bad replacement pattern in a regular expression alias.
transactionApplyAliases :: [AccountAlias] -> Transaction -> Either RegexError Transaction
transactionApplyAliases aliases t =
  case mapM (postingApplyAliases aliases) $ tpostings t of
    Right ps -> Right $ txnTieKnot $ t{tpostings=ps}
    Left err -> Left err

-- | Apply a transformation to a transaction's postings.
transactionMapPostings :: (Posting -> Posting) -> Transaction -> Transaction
transactionMapPostings f t@Transaction{tpostings=ps} = t{tpostings=map f ps}

-- | Apply a transformation to a transaction's posting amounts.
transactionMapPostingAmounts :: (MixedAmount -> MixedAmount) -> Transaction -> Transaction
transactionMapPostingAmounts f  = transactionMapPostings (postingTransformAmount f)

-- | The file path from which this transaction was parsed.
transactionFile :: Transaction -> FilePath
transactionFile Transaction{tsourcepos} = sourceName $ fst tsourcepos

-- Add transaction information to an error message.
annotateErrorWithTransaction :: Transaction -> String -> String
annotateErrorWithTransaction t s =
  unlines [ sourcePosPairPretty $ tsourcepos t, s
          , T.unpack . T.stripEnd $ showTransaction t
          ]

-- tests

tests_Transaction :: TestTree
tests_Transaction =
  testGroup "Transaction" [

      testGroup "showPostingLines" [
          testCase "null posting" $ showPostingLines nullposting @?= ["                   0"]
        , testCase "non-null posting" $
           let p =
                posting
                  { pstatus = Cleared
                  , paccount = "a"
                  , pamount = mixed [usd 1, hrs 2]
                  , pcomment = "pcomment1\npcomment2\n  tag3: val3  \n"
                  , ptype = RegularPosting
                  , ptags = [("ptag1", "val1"), ("ptag2", "val2")]
                  }
           in showPostingLines p @?=
              [ "    * a         $1.00  ; pcomment1"
              , "    ; pcomment2"
              , "    ;   tag3: val3  "
              , "    * a         2.00h  ; pcomment1"
              , "    ; pcomment2"
              , "    ;   tag3: val3  "
              ]
        ]

    , let
        -- one implicit amount
        timp = nulltransaction {tpostings = ["a" `post` usd 1, "b" `post` missingamt]}
        -- explicit amounts, balanced
        texp = nulltransaction {tpostings = ["a" `post` usd 1, "b" `post` usd (-1)]}
        -- explicit amount, only one posting
        texp1 = nulltransaction {tpostings = ["(a)" `post` usd 1]}
        -- explicit amounts, two commodities, explicit balancing price
        texp2 = nulltransaction {tpostings = ["a" `post` usd 1, "b" `post` (hrs (-1) `at` usd 1)]}
        -- explicit amounts, two commodities, implicit balancing price
        texp2b = nulltransaction {tpostings = ["a" `post` usd 1, "b" `post` hrs (-1)]}
        -- one missing amount, not the last one
        t3 = nulltransaction {tpostings = ["a" `post` usd 1, "b" `post` missingamt, "c" `post` usd (-1)]}
        -- unbalanced amounts when precision is limited (#931)
        -- t4 = nulltransaction {tpostings = ["a" `post` usd (-0.01), "b" `post` usd (0.005), "c" `post` usd (0.005)]}
      in testGroup "postingsAsLines" [
              testCase "null-transaction" $ postingsAsLines False (tpostings nulltransaction) @?= []
            , testCase "implicit-amount" $ postingsAsLines False (tpostings timp) @?=
                  [ "    a           $1.00"
                  , "    b" -- implicit amount remains implicit
                  ]
            , testCase "explicit-amounts" $ postingsAsLines False (tpostings texp) @?=
                  [ "    a           $1.00"
                  , "    b          $-1.00"
                  ]
            , testCase "one-explicit-amount" $ postingsAsLines False (tpostings texp1) @?=
                  [ "    (a)           $1.00"
                  ]
            , testCase "explicit-amounts-two-commodities" $ postingsAsLines False (tpostings texp2) @?=
                  [ "    a             $1.00"
                  , "    b    -1.00h @ $1.00"
                  ]
            , testCase "explicit-amounts-not-explicitly-balanced" $ postingsAsLines False (tpostings texp2b) @?=
                  [ "    a           $1.00"
                  , "    b          -1.00h"
                  ]
            , testCase "implicit-amount-not-last" $ postingsAsLines False (tpostings t3) @?=
                  ["    a           $1.00", "    b", "    c          $-1.00"]
            -- , testCase "ensure-visibly-balanced" $
            --    in postingsAsLines False (tpostings t4) @?=
            --       ["    a          $-0.01", "    b           $0.005", "    c           $0.005"]

            ]

    , testGroup "showTransaction" [
          testCase "null transaction" $ showTransaction nulltransaction @?= "0000-01-01\n\n"
        , testCase "non-null transaction" $ showTransaction
            nulltransaction
              { tdate = fromGregorian 2012 05 14
              , tdate2 = Just $ fromGregorian 2012 05 15
              , tstatus = Unmarked
              , tcode = "code"
              , tdescription = "desc"
              , tcomment = "tcomment1\ntcomment2\n"
              , ttags = [("ttag1", "val1")]
              , tpostings =
                  [ nullposting
                      { pstatus = Cleared
                      , paccount = "a"
                      , pamount = mixed [usd 1, hrs 2]
                      , pcomment = "\npcomment2\n"
                      , ptype = RegularPosting
                      , ptags = [("ptag1", "val1"), ("ptag2", "val2")]
                      }
                  ]
              } @?=
          T.unlines
            [ "2012-05-14=2012-05-15 (code) desc  ; tcomment1"
            , "    ; tcomment2"
            , "    * a         $1.00"
            , "    ; pcomment2"
            , "    * a         2.00h"
            , "    ; pcomment2"
            , ""
            ]
        , testCase "show a balanced transaction" $
          (let t =
                 Transaction
                   0
                   ""
                   nullsourcepos
                   (fromGregorian 2007 01 28)
                   Nothing
                   Unmarked
                   ""
                   "coopportunity"
                   ""
                   []
                   [ posting {paccount = "expenses:food:groceries", pamount = mixedAmount (usd 47.18), ptransaction = Just t}
                   , posting {paccount = "assets:checking", pamount = mixedAmount (usd (-47.18)), ptransaction = Just t}
                   ]
            in showTransaction t) @?=
          (T.unlines
             [ "2007-01-28 coopportunity"
             , "    expenses:food:groceries          $47.18"
             , "    assets:checking                 $-47.18"
             , ""
             ])
        , testCase "show an unbalanced transaction, should not elide" $
          (showTransaction
             (txnTieKnot $
              Transaction
                0
                ""
                nullsourcepos
                (fromGregorian 2007 01 28)
                Nothing
                Unmarked
                ""
                "coopportunity"
                ""
                []
                [ posting {paccount = "expenses:food:groceries", pamount = mixedAmount (usd 47.18)}
                , posting {paccount = "assets:checking", pamount = mixedAmount (usd (-47.19))}
                ])) @?=
          (T.unlines
             [ "2007-01-28 coopportunity"
             , "    expenses:food:groceries          $47.18"
             , "    assets:checking                 $-47.19"
             , ""
             ])
        , testCase "show a transaction with one posting and a missing amount" $
          (showTransaction
             (txnTieKnot $
              Transaction
                0
                ""
                nullsourcepos
                (fromGregorian 2007 01 28)
                Nothing
                Unmarked
                ""
                "coopportunity"
                ""
                []
                [posting {paccount = "expenses:food:groceries", pamount = missingmixedamt}])) @?=
          (T.unlines ["2007-01-28 coopportunity", "    expenses:food:groceries", ""])
        , testCase "show a transaction with a priced commodityless amount" $
          (showTransaction
             (txnTieKnot $
              Transaction
                0
                ""
                nullsourcepos
                (fromGregorian 2010 01 01)
                Nothing
                Unmarked
                ""
                "x"
                ""
                []
                [ posting {paccount = "a", pamount = mixedAmount $ num 1 `at` (usd 2 `withPrecision` Precision 0)}
                , posting {paccount = "b", pamount = missingmixedamt}
                ])) @?=
          (T.unlines ["2010-01-01 x", "    a          1 @ $2", "    b", ""])
        ]
    ]
