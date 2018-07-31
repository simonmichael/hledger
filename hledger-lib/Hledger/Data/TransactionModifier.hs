{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE CPP #-}
{-|

A 'TransactionModifier' is a rule that modifies certain 'Transaction's,
typically adding automated postings to them. 

-}
module Hledger.Data.TransactionModifier (
    transactionModifierToFunction
)
where

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
import Hledger.Utils.UTF8IOCompat (error')
-- import Hledger.Utils.Debug

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Hledger.Data.Posting
-- >>> import Hledger.Data.Transaction
-- >>> import Hledger.Data.Journal

-- | Converts a 'TransactionModifier' and a 'Query' to a 
-- 'Transaction'-transforming function, which applies the modification(s)
-- specified by the TransactionModifier. Currently this means adding automated
-- postings when certain other postings - specified by the TransactionModifier,
-- and additionally limited by the extra query, if it's not 'Any' - are present.
-- The postings of the transformed transaction will reference it, as usual 
-- ('txnTieKnot').
--
-- >>> transactionModifierToFunction Any (TransactionModifier "" ["pong" `post` usd 2]) nulltransaction{tpostings=["ping" `post` usd 1]}
-- 0000/01/01
--     ping           $1.00
--     pong           $2.00
-- <BLANKLINE>
-- <BLANKLINE>
-- >>> transactionModifierToFunction Any (TransactionModifier "miss" ["pong" `post` usd 2]) nulltransaction{tpostings=["ping" `post` usd 1]}
-- 0000/01/01
--     ping           $1.00
-- <BLANKLINE>
-- <BLANKLINE>
-- >>> transactionModifierToFunction None (TransactionModifier "" ["pong" `post` usd 2]) nulltransaction{tpostings=["ping" `post` usd 1]}
-- 0000/01/01
--     ping           $1.00
-- <BLANKLINE>
-- <BLANKLINE>
-- >>> transactionModifierToFunction Any (TransactionModifier "ping" ["pong" `post` amount{amultiplier=True, aquantity=3}]) nulltransaction{tpostings=["ping" `post` usd 2]}
-- 0000/01/01
--     ping           $2.00
--     pong           $6.00
-- <BLANKLINE>
-- <BLANKLINE>
transactionModifierToFunction :: Query -> TransactionModifier -> (Transaction -> Transaction)
transactionModifierToFunction q mt = 
  \t@(tpostings -> ps) -> txnTieKnot t{ tpostings=generatePostings ps } -- TODO add modifier txn comment/tags ?
  where
    q' = simplifyQuery $ And [q, tmParseQuery mt (error' "a transaction modifier's query cannot depend on current date")]
    mods = map tmPostingToFunction $ tmpostings mt
    generatePostings ps = [p' | p <- ps
                              , p' <- if q' `matchesPosting` p then p:[ m p | m <- mods] else [p]]
    
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

---- | 'DateSpan' of all dates mentioned in 'Journal'
----
---- >>> jdatespan nulljournal
---- DateSpan -
---- >>> jdatespan nulljournal{jtxns=[nulltransaction{tdate=read "2016-01-01"}] }
---- DateSpan 2016/01/01
---- >>> jdatespan nulljournal{jtxns=[nulltransaction{tdate=read "2016-01-01", tpostings=[nullposting{pdate=Just $ read "2016-02-01"}]}] }
---- DateSpan 2016/01/01-2016/02/01
--jdatespan :: Journal -> DateSpan
--jdatespan j
--        | null dates = nulldatespan
--        | otherwise = DateSpan (Just $ minimum dates) (Just $ 1 `addDays` maximum dates)
--    where
--        dates = concatMap tdates $ jtxns j

---- | 'DateSpan' of all dates mentioned in 'Transaction'
----
---- >>> tdates nulltransaction
---- [0000-01-01]
--tdates :: Transaction -> [Day]
--tdates t = tdate t : concatMap pdates (tpostings t) ++ maybeToList (tdate2 t) where
--    pdates p = catMaybes [pdate p, pdate2 p]

postingScale :: Posting -> Maybe Quantity
postingScale p =
    case amounts $ pamount p of
        [a] | amultiplier a -> Just $ aquantity a
        _ -> Nothing

-- | Converts a 'TransactionModifier''s posting to a 'Posting'-generating function,
-- which will be used to make a new posting based on the old one (an "automated posting").
tmPostingToFunction :: Posting -> (Posting -> Posting)
tmPostingToFunction p' = 
  \p -> renderPostingCommentDates $ p'
      { pdate = pdate p
      , pdate2 = pdate2 p
      , pamount = amount' p
      }
  where
    amount' = case postingScale p' of
        Nothing -> const $ pamount p'
        Just n -> \p -> withAmountType (head $ amounts $ pamount p') $ pamount p `divideMixedAmount` (1/n)
    withAmountType amount (Mixed as) = case acommodity amount of
        "" -> Mixed as
        c -> Mixed [a{acommodity = c, astyle = astyle amount, aprice = aprice amount} | a <- as]

renderPostingCommentDates :: Posting -> Posting
renderPostingCommentDates p = p { pcomment = comment' }
    where
        datesComment = T.concat $ catMaybes [T.pack . showDate <$> pdate p, ("=" <>) . T.pack . showDate <$> pdate2 p]
        comment'
            | T.null datesComment = pcomment p
            | otherwise = T.intercalate "\n" $ filter (not . T.null) [T.strip $ pcomment p, "[" <> datesComment <> "]"]
