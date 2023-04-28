{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-|

A 'TransactionModifier' is a rule that modifies certain 'Transaction's,
typically adding automated postings to them.

-}
module Hledger.Data.TransactionModifier (
   modifyTransactions
)
where

import Prelude hiding (Applicative(..))
import Control.Applicative (Applicative(..), (<|>))
import qualified Data.Map as M
import Data.Maybe (catMaybes)
import qualified Data.Text as T
import Data.Time.Calendar (Day)
import Hledger.Data.Types
import Hledger.Data.Amount
import Hledger.Data.Dates
import Hledger.Data.Transaction (txnTieKnot)
import Hledger.Query (Query, filterQuery, matchesAmount, matchesPostingExtra,
                      parseQuery, queryIsAmt, queryIsSym, simplifyQuery)
import Hledger.Data.Posting (commentJoin, commentAddTag, postingAddTags, postingApplyCommodityStyles)
import Hledger.Utils (wrap)
import Hledger.Utils.Debug

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Hledger.Data.Posting
-- >>> import Hledger.Data.Transaction
-- >>> import Hledger.Data.Journal

-- | Apply all the given transaction modifiers, in turn, to each transaction
-- for which the given predicate is true.
-- Or if any of them fails to be parsed, return the first error. A reference
-- date is provided to help interpret relative dates in transaction modifier
-- queries.
modifyTransactions :: (Transaction -> Bool)
                   -> (AccountName -> Maybe AccountType)
                   -> (AccountName -> [Tag])
                   -> M.Map CommoditySymbol AmountStyle
                   -> Day -> [TransactionModifier] -> [Transaction]
                   -> Either String [Transaction]
modifyTransactions predfn atypes atags styles d tmods ts = do
  fs <- mapM (transactionModifierToFunction atypes atags styles d) tmods  -- convert modifiers to functions, or return a parse error
  let
    maybemodifytxn t = if predfn t then t'' else t
      where
        t' = foldr (flip (.)) id fs t  -- apply each function in turn
        t'' = if t' == t  -- and add some tags if it was changed
              then t'
              else t'{tcomment=tcomment t' `commentAddTag` ("modified",""), ttags=("modified","") : ttags t'}
  Right $ map maybemodifytxn ts

-- | Converts a 'TransactionModifier' to a 'Transaction'-transforming function
-- which applies the modification(s) specified by the TransactionModifier.
-- Or, returns the error message there is a problem parsing the TransactionModifier's query.
-- A reference date is provided to help interpret relative dates in the query.
--
-- The postings of the transformed transaction will reference it in the usual
-- way (ie, 'txnTieKnot' is called).
--
-- Currently the only kind of modification possible is adding automated
-- postings when certain other postings are present.
--
-- >>> import qualified Data.Text.IO as T
-- >>> t = nulltransaction{tpostings=["ping" `post` usd 1]}
-- >>> tmpost acc amt = TMPostingRule (acc `post` amt) False
-- >>> test = either putStr (T.putStr.showTransaction) . fmap ($ t) . transactionModifierToFunction (const Nothing) (const []) mempty nulldate
-- >>> test $ TransactionModifier "" ["pong" `tmpost` usd 2]
-- 0000-01-01
--     ping           $1.00
--     pong           $2.00  ; generated-posting: =
-- <BLANKLINE>
-- >>> test $ TransactionModifier "miss" ["pong" `tmpost` usd 2]
-- 0000-01-01
--     ping           $1.00
-- <BLANKLINE>
-- >>> test $ TransactionModifier "ping" [("pong" `tmpost` nullamt{aquantity=3}){tmprIsMultiplier=True}]
-- 0000-01-01
--     ping           $1.00
--     pong           $3.00  ; generated-posting: = ping
-- <BLANKLINE>
--
transactionModifierToFunction :: (AccountName -> Maybe AccountType)
                              -> (AccountName -> [Tag])
                              -> M.Map CommoditySymbol AmountStyle
                              -> Day -> TransactionModifier
                              -> Either String (Transaction -> Transaction)
transactionModifierToFunction atypes atags styles refdate TransactionModifier{tmquerytxt, tmpostingrules} = do
  q <- simplifyQuery . fst <$> parseQuery refdate tmquerytxt
  let
    fs = map (\tmpr -> addAccountTags . tmPostingRuleToFunction styles q tmquerytxt tmpr) tmpostingrules
    addAccountTags p = p `postingAddTags` atags (paccount p)
    generatePostings p = p : map ($ p) (if matchesPostingExtra atypes q p then fs else [])
  Right $ \t@(tpostings -> ps) -> txnTieKnot t{tpostings=concatMap generatePostings ps}

-- | Converts a 'TransactionModifier''s posting rule to a 'Posting'-generating function,
-- which will be used to make a new posting based on the old one (an "automated posting").
-- The new posting's amount can optionally be the old posting's amount multiplied by a constant.
-- If the old posting had a total-priced amount, the new posting's multiplied amount will be unit-priced.
-- The new posting will have two tags added: a normal generated-posting: tag which also appears in the comment,
-- and a hidden _generated-posting: tag which does not.
-- The TransactionModifier's query text is also provided, and saved
-- as the tags' value.
tmPostingRuleToFunction :: M.Map CommoditySymbol AmountStyle -> Query -> T.Text -> TMPostingRule -> (Posting -> Posting)
tmPostingRuleToFunction styles query querytxt tmpr =
  \p -> postingApplyCommodityStyles styles . renderPostingCommentDates $ pr
      { pdate    = pdate  pr <|> pdate  p
      , pdate2   = pdate2 pr <|> pdate2 p
      , pamount  = amount' p
      , pcomment = pcomment pr `commentAddTag` ("generated-posting",qry)
      , ptags    = ("generated-posting", qry) :
                   ("_generated-posting",qry) :
                   ptags pr
      }
  where
    pr = tmprPosting tmpr
    qry = "= " <> querytxt
    symq = filterQuery (liftA2 (||) queryIsSym queryIsAmt) query
    amount' = case postingRuleMultiplier tmpr of
        Nothing -> const $ pamount pr
        Just n  -> \p ->
          -- Multiply the old posting's amount by the posting rule's multiplier.
          let
            pramount = dbg6 "pramount" . head . amountsRaw $ pamount pr
            matchedamount = dbg6 "matchedamount" . filterMixedAmount (symq `matchesAmount`) $ pamount p
            -- Handle a matched amount with a total price carefully so as to keep the transaction balanced (#928).
            -- Approach 1: convert to a unit price and increase the display precision slightly
            -- Mixed as = dbg6 "multipliedamount" $ n `multiplyMixedAmount` mixedAmountTotalPriceToUnitPrice matchedamount
            -- Approach 2: multiply the total price (keeping it positive) as well as the quantity
            as = dbg6 "multipliedamount" $ multiplyMixedAmount n matchedamount
          in
            case acommodity pramount of
              "" -> as
              -- TODO multipliers with commodity symbols are not yet a documented feature.
              -- For now: in addition to multiplying the quantity, it also replaces the
              -- matched amount's commodity, display style, and price with those of the posting rule.
              c  -> mapMixedAmount (\a -> a{acommodity = c, astyle = astyle pramount, aprice = aprice pramount}) as

postingRuleMultiplier :: TMPostingRule -> Maybe Quantity
postingRuleMultiplier tmpr = case amountsRaw . pamount $ tmprPosting tmpr of
    [a] | tmprIsMultiplier tmpr -> Just $ aquantity a
    _                           -> Nothing

renderPostingCommentDates :: Posting -> Posting
renderPostingCommentDates p = p { pcomment = comment' }
    where
        dates = T.concat $ catMaybes [showDate <$> pdate p, ("=" <>) . showDate <$> pdate2 p]
        comment'
            | T.null dates = pcomment p
            | otherwise    = (wrap "[" "]" dates) `commentJoin` pcomment p
