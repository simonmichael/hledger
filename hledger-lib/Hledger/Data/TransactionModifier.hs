{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE CPP #-}
{-|

A 'TransactionModifier' is a rule that modifies certain 'Transaction's,
typically adding automated postings to them.

-}
module Hledger.Data.TransactionModifier (
   modifyTransactions
)
where

import Control.Applicative ((<|>))
import Data.Maybe
#if !(MIN_VERSION_base(4,11,0))
import Data.Monoid ((<>))
#endif
import qualified Data.Text as T
import Data.Time.Calendar
import Hledger.Data.Types
import Hledger.Data.Dates
import Hledger.Data.Amount
import Hledger.Data.Transaction
import Hledger.Query
import Hledger.Data.Posting (commentJoin, commentAddTag)
import Hledger.Utils.UTF8IOCompat (error')
import Hledger.Utils.Debug

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Hledger.Data.Posting
-- >>> import Hledger.Data.Transaction
-- >>> import Hledger.Data.Journal

-- | Apply all the given transaction modifiers, in turn, to each transaction.
modifyTransactions :: [TransactionModifier] -> [Transaction] -> [Transaction]
modifyTransactions tmods = map applymods
  where
    applymods t = taggedt'
      where
        t' = foldr (flip (.) . transactionModifierToFunction) id tmods t
        taggedt'
          -- PERF: compares txns to see if any modifier had an effect, inefficient ?
          | t' /= t   = t'{tcomment = tcomment t' `commentAddTag` ("modified","")
                          ,ttags    = ("modified","") : ttags t'
                          }
          | otherwise = t'

-- | Converts a 'TransactionModifier' to a 'Transaction'-transforming function,
-- which applies the modification(s) specified by the TransactionModifier.
-- Currently this means adding automated postings when certain other postings are present.
-- The postings of the transformed transaction will reference it in the usual
-- way (ie, 'txnTieKnot' is called).
--
-- >>> putStr $ showTransaction $ transactionModifierToFunction (TransactionModifier "" ["pong" `post` usd 2]) nulltransaction{tpostings=["ping" `post` usd 1]}
-- 0000/01/01
--     ping           $1.00
--     pong           $2.00  ; generated-posting: =
-- <BLANKLINE>
-- >>> putStr $ showTransaction $ transactionModifierToFunction (TransactionModifier "miss" ["pong" `post` usd 2]) nulltransaction{tpostings=["ping" `post` usd 1]}
-- 0000/01/01
--     ping           $1.00
-- <BLANKLINE>
-- >>> putStr $ showTransaction $ transactionModifierToFunction (TransactionModifier "ping" ["pong" `post` amount{aismultiplier=True, aquantity=3}]) nulltransaction{tpostings=["ping" `post` usd 2]}
-- 0000/01/01
--     ping           $2.00
--     pong           $6.00  ; generated-posting: = ping
-- <BLANKLINE>
--
transactionModifierToFunction :: TransactionModifier -> (Transaction -> Transaction)
transactionModifierToFunction mt =
  \t@(tpostings -> ps) -> txnTieKnot t{ tpostings=generatePostings ps }
  where
    q = simplifyQuery $ tmParseQuery mt (error' "a transaction modifier's query cannot depend on current date")
    mods = map (tmPostingRuleToFunction (tmquerytxt mt)) $ tmpostingrules mt
    generatePostings ps = [p' | p <- ps
                              , p' <- if q `matchesPosting` p then p:[ m p | m <- mods] else [p]]

-- | Parse the 'Query' from a 'TransactionModifier's 'tmquerytxt',
-- and return it as a function requiring the current date.
--
-- >>> tmParseQuery (TransactionModifier "" []) undefined
-- Any
-- >>> tmParseQuery (TransactionModifier "ping" []) undefined
-- Acct "ping"
-- >>> tmParseQuery (TransactionModifier "date:2016" []) undefined
-- Date (DateSpan 2016)
-- >>> tmParseQuery (TransactionModifier "date:today" []) (read "2017-01-01")
-- Date (DateSpan 2017/01/01)
tmParseQuery :: TransactionModifier -> (Day -> Query)
tmParseQuery mt = fst . flip parseQuery (tmquerytxt mt)

-- | Converts a 'TransactionModifier''s posting rule to a 'Posting'-generating function,
-- which will be used to make a new posting based on the old one (an "automated posting").
-- The new posting's amount can optionally be the old posting's amount multiplied by a constant.
-- If the old posting had a total-priced amount, the new posting's multiplied amount will be unit-priced.
-- The new posting will have two tags added: a normal generated-posting: tag which also appears in the comment,
-- and a hidden _generated-posting: tag which does not.
-- The TransactionModifier's query text is also provided, and saved
-- as the tags' value.
tmPostingRuleToFunction :: T.Text -> TMPostingRule -> (Posting -> Posting)
tmPostingRuleToFunction querytxt pr =
  \p -> renderPostingCommentDates $ pr
      { pdate    = pdate  pr <|> pdate  p
      , pdate2   = pdate2 pr <|> pdate2 p
      , pamount  = amount' p
      , pcomment = pcomment pr `commentAddTag` ("generated-posting",qry)
      , ptags    = ("generated-posting", qry) :
                   ("_generated-posting",qry) :
                   ptags pr
      }
  where
    qry = "= " <> querytxt
    amount' = case postingRuleMultiplier pr of
        Nothing -> const $ pamount pr
        Just n  -> \p ->
          -- Multiply the old posting's amount by the posting rule's multiplier.
          let
            pramount = dbg6 "pramount" $ head $ amounts $ pamount pr
            matchedamount = dbg6 "matchedamount" $ pamount p
            -- Handle a matched amount with a total price carefully so as to keep the transaction balanced (#928).
            -- Approach 1: convert to a unit price and increase the display precision slightly
            -- Mixed as = dbg6 "multipliedamount" $ n `multiplyMixedAmount` mixedAmountTotalPriceToUnitPrice matchedamount
            -- Approach 2: multiply the total price (keeping it positive) as well as the quantity
            Mixed as = dbg6 "multipliedamount" $ n `multiplyMixedAmountAndPrice` matchedamount
          in
            case acommodity pramount of
              "" -> Mixed as
              -- TODO multipliers with commodity symbols are not yet a documented feature.
              -- For now: in addition to multiplying the quantity, it also replaces the
              -- matched amount's commodity, display style, and price with those of the posting rule.
              c  -> Mixed [a{acommodity = c, astyle = astyle pramount, aprice = aprice pramount} | a <- as]

postingRuleMultiplier :: TMPostingRule -> Maybe Quantity
postingRuleMultiplier p =
    case amounts $ pamount p of
        [a] | aismultiplier a -> Just $ aquantity a
        _                   -> Nothing

renderPostingCommentDates :: Posting -> Posting
renderPostingCommentDates p = p { pcomment = comment' }
    where
        dates = T.concat $ catMaybes [T.pack . showDate <$> pdate p, ("=" <>) . T.pack . showDate <$> pdate2 p]
        comment'
            | T.null dates = pcomment p
            | otherwise    = ("[" <> dates <> "]") `commentJoin` pcomment p
