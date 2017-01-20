{-# LANGUAGE OverloadedStrings, ViewPatterns #-}
{-|

This module provides utilities for applying automated transactions like
'ModifierTransaction' and 'PeriodicTransaction'.

-}
module Hledger.Data.AutoTransaction
    (
    -- * Transaction processors
      runModifierTransaction

    -- * Accessors
    , mtvaluequery
    )
where

import Data.Maybe
import Data.Monoid ((<>))
import Data.Time.Calendar
import qualified Data.Text as T
import Hledger.Data.Types
import Hledger.Data.Dates
import Hledger.Data.Amount
import Hledger.Query

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Hledger.Data.Posting
-- >>> import Hledger.Data.Transaction

-- | Builds a 'Transaction' transformer based on 'ModifierTransaction'.
--
-- 'Query' parameter allows injection of additional restriction on posting
-- match. Don't forget to call 'txnTieKnot'.
--
-- >>> runModifierTransaction Any (ModifierTransaction "" ["pong" `post` usd 2]) nulltransaction{tpostings=["ping" `post` usd 1]}
-- 0000/01/01
--     ping         $1.00
--     pong         $2.00
-- <BLANKLINE>
-- <BLANKLINE>
-- >>> runModifierTransaction Any (ModifierTransaction "miss" ["pong" `post` usd 2]) nulltransaction{tpostings=["ping" `post` usd 1]}
-- 0000/01/01
--     ping         $1.00
-- <BLANKLINE>
-- <BLANKLINE>
-- >>> runModifierTransaction None (ModifierTransaction "" ["pong" `post` usd 2]) nulltransaction{tpostings=["ping" `post` usd 1]}
-- 0000/01/01
--     ping         $1.00
-- <BLANKLINE>
-- <BLANKLINE>
-- >>> runModifierTransaction Any (ModifierTransaction "ping" ["pong" `post` amount{acommodity="*", aquantity=3}]) nulltransaction{tpostings=["ping" `post` usd 2]}
-- 0000/01/01
--     ping         $2.00
--     pong         $6.00
-- <BLANKLINE>
-- <BLANKLINE>
runModifierTransaction :: Query -> ModifierTransaction -> (Transaction -> Transaction)
runModifierTransaction q mt = modifier where
    q' = simplifyQuery $ And [q, mtvaluequery mt (error "query cannot depend on current time")]
    mods = map runModifierPosting $ mtpostings mt
    generatePostings ps = [m p | p <- ps, q' `matchesPosting` p, m <- mods]
    modifier t@(tpostings -> ps) = t { tpostings = ps ++ generatePostings ps }

-- | Extract 'Query' equivalent of 'mtvalueexpr' from 'ModifierTransaction'
--
-- >>> mtvaluequery (ModifierTransaction "" []) undefined
-- Any
-- >>> mtvaluequery (ModifierTransaction "ping" []) undefined
-- Acct "ping"
-- >>> mtvaluequery (ModifierTransaction "date:2016" []) undefined
-- Date (DateSpan 2016)
-- >>> mtvaluequery (ModifierTransaction "date:today" []) (read "2017-01-01")
-- Date (DateSpan 2017/01/01)
mtvaluequery :: ModifierTransaction -> (Day -> Query)
mtvaluequery mt = fst . flip parseQuery (mtvalueexpr mt)

postingScale :: Posting -> Maybe Quantity
postingScale p =
    case amounts $ pamount p of
        [a] | acommodity a == "*" -> Just $ aquantity a
        _ -> Nothing

runModifierPosting :: Posting -> (Posting -> Posting)
runModifierPosting p' = modifier where
    modifier p = renderPostingCommentDates $ p'
        { pdate = pdate p
        , pdate2 = pdate2 p
        , pamount = amount' p
        }
    amount' =
        case postingScale p' of
            Nothing -> const $ pamount p'
            Just n -> \p -> pamount p `divideMixedAmount` (1/n)

renderPostingCommentDates :: Posting -> Posting
renderPostingCommentDates p = p { pcomment = comment' }
    where
        datesComment = T.concat $ catMaybes [T.pack . showDate <$> pdate p, ("=" <>) . T.pack . showDate <$> pdate2 p]
        comment'
            | T.null datesComment = pcomment p
            | otherwise = T.intercalate "\n" $ filter (not . T.null) [T.strip $ pcomment p, "[" <> datesComment <> "]"]
