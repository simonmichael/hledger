{-|

A 'Transaction' represents a movement of some commodity(ies) between two
or more accounts. It consists of multiple account 'Posting's which balance
to zero, a date, and optional extras like description, cleared status, and
tags.

-}

{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

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
, transactionInferCostsFromEquity
, transactionApplyAliases
, transactionMapPostings
, transactionMapPostingAmounts
, partitionAndCheckConversionPostings
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
import Data.Bifunctor (first, second)
import Data.Foldable (foldlM)
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
import Data.Decimal (normalizeDecimal, decimalPlaces)
import Data.Functor ((<&>))


instance HasAmounts Transaction where
  styleAmounts styles t = t{tpostings=styleAmounts styles $ tpostings t}

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
transactionAddInferredEquityPostings :: Bool -> AccountName -> Transaction -> Transaction
transactionAddInferredEquityPostings verbosetags equityAcct t =
    t{tpostings=concatMap (postingAddInferredEquityPostings verbosetags equityAcct) $ tpostings t}

type IdxPosting = (Int, Posting)

-- XXX Warning: The following code - for analysing equity conversion postings,
-- inferring missing costs and ignoring redundant costs -
-- is twisty and hard to follow.

label s = ((s <> ": ")++)

-- | Add costs inferred from equity postings in this transaction.
-- For every adjacent pair of conversion postings, it will first search the postings
-- with costs to see if any match. If so, it will tag these as matched.
-- If no postings with costs match, it will then search the postings without costs,
-- and will match the first such posting which matches one of the conversion amounts.
-- If it finds a match, it will add a cost and then tag it.
-- If the first argument is true, do a dry run instead: identify and tag
-- the costful and conversion postings, but don't add costs.
transactionInferCostsFromEquity :: Bool -> M.Map AccountName AccountType -> Transaction -> Either String Transaction
transactionInferCostsFromEquity dryrun acctTypes t = first (annotateErrorWithTransaction t . T.unpack) $ do
  -- number the postings
  let npostings = zip [0..] $ tpostings t

  -- Identify all pairs of conversion postings and all other postings (with and without costs) in the transaction.
  (conversionPairs, otherps) <- partitionAndCheckConversionPostings False acctTypes npostings

  -- Generate a pure function that can be applied to each of this transaction's postings,
  -- possibly modifying it, to produce the following end result:
  -- 1. each pair of conversion postings, and the corresponding postings which balance them, are tagged for easy identification
  -- 2. each pair of balancing postings which did't have an explicit cost, have had a cost calculated and added to one of them
  -- 3. if any ambiguous situation was detected, an informative error is raised
  processposting <- transformIndexedPostingsF (addCostsToPostings dryrun) conversionPairs otherps

  -- And if there was no error, use it to modify the transaction's postings.
  return t{tpostings = map (snd . processposting) npostings}

  where

    -- Generate the tricksy processposting function,
    -- which when applied to each posting in turn, rather magically has the effect of
    -- applying addCostsToPostings to each pair of conversion postings in the transaction,
    -- matching them with the other postings, tagging them and perhaps adding cost information to the other postings.
    -- General type:
    -- transformIndexedPostingsF :: (Monad m, Foldable t, Traversable t) =>
    --   (a -> StateT s m (a1 -> a1)) ->
    --   t a ->
    --   s ->
    --   m (a1 -> a1)
    -- Concrete type:
    transformIndexedPostingsF ::
      ((IdxPosting, IdxPosting) -> StateT ([IdxPosting],[IdxPosting]) (Either Text) (IdxPosting -> IdxPosting)) ->  -- state update function (addCostsToPostings with the bool applied)
      [(IdxPosting, IdxPosting)] ->   -- initial state: the pairs of adjacent conversion postings in the transaction
      ([IdxPosting],[IdxPosting]) ->  -- initial state: the other postings in the transaction, separated into costful and costless
      (Either Text (IdxPosting -> IdxPosting))  -- returns an error message or a posting transform function
    transformIndexedPostingsF updatefn = evalStateT . fmap (appEndo . foldMap Endo) . traverse (updatefn)

    -- A tricksy state update helper for processposting/transformIndexedPostingsF.
    -- Approximately: given a pair of conversion postings to match,
    -- and lists of the remaining unmatched costful and costless other postings,
    -- 1. find (and consume) two other postings which match the two conversion postings
    -- 2. add identifying tags to the four postings
    -- 3. add an explicit cost, if missing, to one of the matched other postings
    -- 4. or if there is a problem, raise an informative error or do nothing as appropriate.
    -- Or, if the first argument is true:
    -- do a dry run instead: find and consume, add tags, but don't add costs
    -- (and if there are no costful postings at all, do nothing).
    addCostsToPostings :: Bool -> (IdxPosting, IdxPosting) -> StateT ([IdxPosting], [IdxPosting]) (Either Text) (IdxPosting -> IdxPosting)
    addCostsToPostings dryrun' ((n1, cp1), (n2, cp2)) = StateT $ \(costps, otherps) -> do
      -- Get the two conversion posting amounts, if possible
      ca1 <- conversionPostingAmountNoCost cp1
      ca2 <- conversionPostingAmountNoCost cp2
      let 
        -- All costful postings which match the conversion posting pair
        matchingCostPs =
          dbg7With (label "matched costful postings".show.length) $ 
          mapMaybe (mapM $ costfulPostingIfMatchesBothAmounts ca1 ca2) costps

        -- All other single-commodity postings whose amount matches at least one of the conversion postings,
        -- with an explicit cost added. Or in dry run mode, all other single-commodity postings.
        matchingOtherPs =
          dbg7With (label "matched costless postings".show.length) $
          if dryrun'
          then [(n,(p, a)) | (n,p) <- otherps, let Just a = postingSingleAmount p]
          else mapMaybe (mapM $ addCostIfMatchesOneAmount ca1 ca2) otherps

        -- A function that adds a cost and/or tag to a numbered posting if appropriate.
        postingAddCostAndOrTag np costp (n,p) =
          (n, if | n == np            -> costp `postingAddTags` [("_price-matched","")]
                 | n == n1 || n == n2 -> p     `postingAddTags` [("_conversion-matched","")]
                 | otherwise          -> p)

      -- Annotate any errors with the conversion posting pair
      first (annotateWithPostings [cp1, cp2]) $
        if
          -- If a single costful posting matches the conversion postings,
          -- delete it from the list of costful postings in the state, delete the
          -- first matching costless posting from the list of costless postings
          -- in the state, and return the transformation function with the new state.
          | [(np, costp)] <- matchingCostPs
          , Just newcostps <- deleteIdx np costps
              -> Right (postingAddCostAndOrTag np costp, (if dryrun' then costps else newcostps, otherps))

          -- If no costful postings match the conversion postings, but some
          -- of the costless postings match, check that the first such posting has a
          -- different amount from all the others, and if so add a cost to it,
          -- then delete it from the list of costless postings in the state,
          -- and return the transformation function with the new state.
          | [] <- matchingCostPs
          , (np, (costp, amt)):nps <- matchingOtherPs
          , not $ any (amountsMatch amt . snd . snd) nps
          , Just newotherps <- deleteIdx np otherps
              -> Right (postingAddCostAndOrTag np costp, (costps, if dryrun' then otherps else newotherps))

          -- Otherwise, do nothing, leaving the transaction unchanged.
          -- We don't want to be over-zealous reporting problems here
          -- since this is always called at least in dry run mode by
          -- journalFinalise > journalMarkRedundantCosts. (#2045)
          | otherwise -> Right (id, (costps, otherps))

    -- If a posting with cost matches both the conversion amounts, return it along
    -- with the matching amount which must be present in another non-conversion posting.
    costfulPostingIfMatchesBothAmounts :: Amount -> Amount -> Posting -> Maybe Posting
    costfulPostingIfMatchesBothAmounts a1 a2 costfulp = do
        a@Amount{aprice=Just _} <- postingSingleAmount costfulp
        if
           | dbgamtmatch 1 a1 a (amountsMatch (-a1) a)  &&  dbgcostmatch 2 a2 a (amountsMatch a2 (amountCost a)) -> Just costfulp
           | dbgamtmatch 2 a2 a (amountsMatch (-a2) a)  &&  dbgcostmatch 1 a1 a (amountsMatch a1 (amountCost a)) -> Just costfulp
           | otherwise -> Nothing
           where
            dbgamtmatch  n a b = dbg7 ("conversion posting "     <>show n<>" "<>showAmount a<>" balances amount "<>showAmountWithoutPrice b <>" of costful posting "<>showAmount b<>" at precision "<>dbgShowAmountPrecision a<>" ?")
            dbgcostmatch n a b = dbg7 ("and\nconversion posting "<>show n<>" "<>showAmount a<>" matches cost "   <>showAmount (amountCost b)<>" of costful posting "<>showAmount b<>" at precision "<>dbgShowAmountPrecision a<>" ?") 

    -- Add a cost to a posting if it matches (negative) one of the
    -- supplied conversion amounts, adding the other amount as the cost.
    addCostIfMatchesOneAmount :: Amount -> Amount -> Posting -> Maybe (Posting, Amount)
    addCostIfMatchesOneAmount a1 a2 p = do
        a <- postingSingleAmount p
        let newp cost = p{pamount = mixedAmount a{aprice = Just $ TotalPrice cost}}
        if
           | amountsMatch (-a1) a -> Just (newp a2, a2)
           | amountsMatch (-a2) a -> Just (newp a1, a1)
           | otherwise            -> Nothing

    -- Get the single-commodity costless amount from a conversion posting, or raise an error.
    conversionPostingAmountNoCost p = case postingSingleAmount p of
        Just a@Amount{aprice=Nothing} -> Right a
        Just Amount{aprice=Just _} -> Left $ annotateWithPostings [p] "Conversion postings must not have a cost:"
        Nothing                    -> Left $ annotateWithPostings [p] "Conversion postings must have a single-commodity amount:"

    -- Do these amounts look the same when compared at the first's display precision ?
    -- (Or if that's unset, compare as-is)
    amountsMatch a b =
      case asprecision $ astyle a of
        Just p  -> amountLooksZero $ amountSetPrecision p $ a - b
        Nothing -> amountLooksZero $ a - b

    -- Delete a posting from the indexed list of postings based on either its
    -- index or its posting amount.
    -- Note: traversing the whole list to delete a single match is generally not efficient,
    -- but given that a transaction probably doesn't have more than four postings, it should
    -- still be more efficient than using a Map or another data structure. Even monster
    -- transactions with up to 10 postings, which are generally not a good
    -- idea, are still too small for there to be an advantage.
    -- XXX shouldn't assume transactions have few postings
    deleteIdx n = deleteUniqueMatch ((n==) . fst)
    deleteUniqueMatch p (x:xs) | p x       = if any p xs then Nothing else Just xs
                               | otherwise = (x:) <$> deleteUniqueMatch p xs
    deleteUniqueMatch _ []                 = Nothing
    annotateWithPostings xs str = T.unlines $ str : postingsAsLines False xs

dbgShowAmountPrecision a =
  case asprecision $ astyle a of
    Just (Precision n)    -> show n
    Just NaturalPrecision -> show $ decimalPlaces $ normalizeDecimal $ aquantity a
    Nothing               -> "unset"

-- Using the provided account types map, sort the given indexed postings
-- into three lists of posting numbers (stored in two pairs), like so:
-- (conversion postings, (costful other postings, costless other postings)).
-- A true first argument activates its secondary function: check that all
-- conversion postings occur in adjacent pairs, otherwise return an error.
partitionAndCheckConversionPostings :: Bool -> M.Map AccountName AccountType -> [IdxPosting] -> Either Text ( [(IdxPosting, IdxPosting)], ([IdxPosting], [IdxPosting]) )
partitionAndCheckConversionPostings check acctTypes =
  -- Left fold processes postings in parse order, so that eg inferred costs
  -- will be added to the first (top-most) posting, not the last one.
  foldlM select (([], ([], [])), Nothing)
    -- The costless other postings are somehow reversed still; "second (second reverse" fixes that.
    <&> fmap (second (second reverse) . fst)
  where
    select ((cs, others@(ps, os)), Nothing) np@(_, p)
      | isConversion p = Right ((cs, others),      Just np)
      | hasCost p      = Right ((cs, (np:ps, os)), Nothing)
      | otherwise      = Right ((cs, (ps, np:os)), Nothing)
    select ((cs, others@(ps,os)), Just lst) np@(_, p)
      | isConversion p = Right (((lst, np):cs, others), Nothing)
      | check          = Left "Conversion postings must occur in adjacent pairs"
      | otherwise      = Right ((cs, (ps, np:os)), Nothing)
    isConversion p = M.lookup (paccount p) acctTypes == Just Conversion
    hasCost p = isJust $ aprice =<< postingSingleAmount p

-- | Get a posting's amount if it is single-commodity.
postingSingleAmount :: Posting -> Maybe Amount
postingSingleAmount p = case amountsRaw (pamount p) of
  [a] -> Just a
  _   -> Nothing

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
