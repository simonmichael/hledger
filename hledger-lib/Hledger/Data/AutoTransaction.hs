{-# LANGUAGE OverloadedStrings, ViewPatterns #-}
{-|

This module provides utilities for applying automated transactions like
'ModifierTransaction' and 'PeriodicTransaction'.

-}
module Hledger.Data.AutoTransaction
    (
    -- * Transaction processors
      runModifierTransaction
    , runPeriodicTransaction

    -- * Accessors
    , mtvaluequery
    , jdatespan
    )
where

import Data.Maybe
import Data.Monoid ((<>))
import Data.Time.Calendar
import qualified Data.Text as T
import Hledger.Data.Types
import Hledger.Data.Dates
import Hledger.Data.Amount
import Hledger.Data.Transaction
import Hledger.Utils.Parse
import Hledger.Utils.UTF8IOCompat (error')
import Hledger.Query

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Hledger.Data.Posting
-- >>> import Hledger.Data.Journal

-- | Builds a 'Transaction' transformer based on 'ModifierTransaction'.
--
-- 'Query' parameter allows injection of additional restriction on posting
-- match. Don't forget to call 'txnTieKnot'.
--
-- >>> runModifierTransaction Any (ModifierTransaction "" "" False ["pong" `post` usd 2]) nulltransaction{tpostings=["ping" `post` usd 1]}
-- 0000/01/01
--     ping           $1.00
--     pong           $2.00
-- <BLANKLINE>
-- <BLANKLINE>
-- >>> runModifierTransaction Any (ModifierTransaction "miss" "" False ["pong" `post` usd 2]) nulltransaction{tpostings=["ping" `post` usd 1]}
-- 0000/01/01
--     ping           $1.00
-- <BLANKLINE>
-- <BLANKLINE>
-- >>> runModifierTransaction None (ModifierTransaction "" "" False ["pong" `post` usd 2]) nulltransaction{tpostings=["ping" `post` usd 1]}
-- 0000/01/01
--     ping           $1.00
-- <BLANKLINE>
-- <BLANKLINE>
-- >>> runModifierTransaction Any (ModifierTransaction "ping" "" False ["pong" `post` amount{amultiplier=True, aquantity=3}]) nulltransaction{tpostings=["ping" `post` usd 2]}
-- 0000/01/01
--     ping           $2.00
--     pong           $6.00
-- <BLANKLINE>
-- <BLANKLINE>
-- >>> runModifierTransaction Any (ModifierTransaction "ping" "" True ["pong" `post` amount{amultiplier=True, aquantity=1}]) nulltransaction{tpostings=["ping" `post` usd 2]}
-- 0000/01/01
--     pong           $2.00
-- <BLANKLINE>
-- <BLANKLINE>
-- >>> runModifierTransaction Any (ModifierTransaction "ping" "review" False []) nulltransaction{tpostings=["ping" `post` usd 2, "ping" `post` usd 1]}
-- 0000/01/01
--     ; review
--     ping           $2.00
--     ping           $1.00
-- <BLANKLINE>
-- <BLANKLINE>
runModifierTransaction :: Query -> ModifierTransaction -> (Transaction -> Transaction)
runModifierTransaction q mt = modifier where
    q' = simplifyQuery $ And [q, mtvaluequery mt (error "query cannot depend on current time")]
    mods = map runModifierPosting $ mtpostings mt
    modifyComment | T.null $ mtcomment mt = id
                  | otherwise = \c -> T.unlines [T.stripEnd c, mtcomment mt]
    filterPostings | mtreplace mt = filter (not . (q `matchesPosting`))
                   | otherwise = id
    generatePostings ps = [m p | p <- ps, q' `matchesPosting` p, m <- mods]
    modifier t@(tpostings -> ps) = t { tpostings = filterPostings ps ++
                                                     generatePostings ps
                                     , tcomment = modifyComment $ tcomment t
                                     }

-- | Extract 'Query' equivalent of 'mtvalueexpr' from 'ModifierTransaction'
--
-- >>> mtvaluequery (ModifierTransaction "" undefined undefined []) undefined
-- Any
-- >>> mtvaluequery (ModifierTransaction "ping" undefined undefined []) undefined
-- Acct "ping"
-- >>> mtvaluequery (ModifierTransaction "date:2016" undefined undefined []) undefined
-- Date (DateSpan 2016)
-- >>> mtvaluequery (ModifierTransaction "date:today" undefined undefined []) (read "2017-01-01")
-- Date (DateSpan 2017/01/01)
mtvaluequery :: ModifierTransaction -> (Day -> Query)
mtvaluequery mt = fst . flip parseQuery (mtvalueexpr mt)

-- | 'DateSpan' of all dates mentioned in 'Journal'
--
-- >>> jdatespan nulljournal
-- DateSpan -
-- >>> jdatespan nulljournal{jtxns=[nulltransaction{tdate=read "2016-01-01"}] }
-- DateSpan 2016/01/01
-- >>> jdatespan nulljournal{jtxns=[nulltransaction{tdate=read "2016-01-01", tpostings=[nullposting{pdate=Just $ read "2016-02-01"}]}] }
-- DateSpan 2016/01/01-2016/02/01
jdatespan :: Journal -> DateSpan
jdatespan j
        | null dates = nulldatespan
        | otherwise = DateSpan (Just $ minimum dates) (Just $ 1 `addDays` maximum dates)
    where
        dates = concatMap tdates $ jtxns j

-- | 'DateSpan' of all dates mentioned in 'Transaction'
--
-- >>> tdates nulltransaction
-- [0000-01-01]
tdates :: Transaction -> [Day]
tdates t = tdate t : concatMap pdates (tpostings t) ++ maybeToList (tdate2 t) where
    pdates p = catMaybes [pdate p, pdate2 p]

postingScale :: Posting -> Maybe Quantity
postingScale p =
    case amounts $ pamount p of
        [a] | amultiplier a -> Just $ aquantity a
        _ -> Nothing

runModifierPosting :: Posting -> (Posting -> Posting)
runModifierPosting p' = modifier where
    modifier p = renderPostingCommentDates $ p'
        { pdate = pdate p
        , pdate2 = pdate2 p
        , pamount = amount' p
        }
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

-- | Generate transactions from 'PeriodicTransaction' within a 'DateSpan'
--
-- Note that new transactions require 'txnTieKnot' post-processing.
--
-- >>> mapM_ (putStr . show) $ runPeriodicTransaction (PeriodicTransaction "monthly from 2017/1 to 2017/4" ["hi" `post` usd 1]) nulldatespan
-- 2017/01/01
--     hi           $1.00
-- <BLANKLINE>
-- 2017/02/01
--     hi           $1.00
-- <BLANKLINE>
-- 2017/03/01
--     hi           $1.00
-- <BLANKLINE>
runPeriodicTransaction :: PeriodicTransaction -> (DateSpan -> [Transaction])
runPeriodicTransaction pt = generate where
    base = nulltransaction { tpostings = ptpostings pt }
    periodExpr = ptperiodicexpr pt
    errCurrent = error' $ "Current date cannot be referenced in " ++ show (T.unpack periodExpr)
    (interval, effectspan) =
        case parsePeriodExpr errCurrent periodExpr of
            Left e -> error' $ "Failed to parse " ++ show (T.unpack periodExpr) ++ ": " ++ showDateParseError e
            Right x -> x
    generate jspan = [base {tdate=date} | span <- interval `splitSpan` spanIntersect effectspan jspan, let Just date = spanStart span]
