{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE CPP #-}
{-|

This module provides helpers for applying 'TransactionModifier's 
(eg generating automated postings) and generating 'PeriodicTransaction's.

-}
module Hledger.Data.AutoTransaction
    (
    -- * Transaction modifiers
      transactionModifierToFunction

    -- * Periodic transactions
    , runPeriodicTransaction
    , checkPeriodicTransactionStartDate

    -- -- * Accessors, seemingly unused
    -- , tmParseQuery
    -- , jdatespan
    -- , periodTransactionInterval
    )
where

import Data.Maybe
#if !(MIN_VERSION_base(4,11,0))
import Data.Monoid ((<>))
#endif
import Data.Time.Calendar
import qualified Data.Text as T
import Hledger.Data.Types
import Hledger.Data.Dates
import Hledger.Data.Amount
import Hledger.Data.Posting (post)
import Hledger.Data.Transaction
import Hledger.Utils.UTF8IOCompat (error')
import Hledger.Query
-- import Hledger.Utils.Debug

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Hledger.Data.Posting
-- >>> import Hledger.Data.Journal

-- | Converts a 'TransactionModifier' and a 'Query' to a 
-- 'Transaction'-transforming function. The query allows injection of
-- additional restrictions on which postings to modify. 
-- The transformer function will not call 'txnTieKnot', you will
-- probably want to call that after using it.
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
  \t@(tpostings -> ps) -> t { tpostings = generatePostings ps } -- TODO add modifier txn comment/tags ?
  where
    q' = simplifyQuery $ And [q, tmParseQuery mt (error' "a transaction modifier's query cannot depend on current date")]
    mods = map tmPostingToFunction $ tmpostings mt
    generatePostings ps = [p' | p <- ps
                              , p' <- if q' `matchesPosting` p then p:[ m p | m <- mods] else [p]]
    
-- | Parse the 'Query' from a 'TransactionModifier's 'tmquerytxt', 
-- and return it as a function requiring the current date. 
--
-- >>> mtvaluequery (TransactionModifier "" []) undefined
-- Any
-- >>> mtvaluequery (TransactionModifier "ping" []) undefined
-- Acct "ping"
-- >>> mtvaluequery (TransactionModifier "date:2016" []) undefined
-- Date (DateSpan 2016)
-- >>> mtvaluequery (TransactionModifier "date:today" []) (read "2017-01-01")
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

-- doctest helper, too much hassle to define in the comment
-- XXX duplicates some logic in periodictransactionp
_ptgen str = do
  let 
    t = T.pack str
    (i,s) = parsePeriodExpr' nulldate t
  case checkPeriodicTransactionStartDate i s t of
    Just e  -> error' e
    Nothing ->
      mapM_ (putStr . show) $
        runPeriodicTransaction
          nullperiodictransaction{ ptperiodexpr=t , ptspan=s, ptinterval=i, ptpostings=["a" `post` usd 1] } 
          nulldatespan

-- | Generate transactions from 'PeriodicTransaction' within a 'DateSpan'
--
-- Note that new transactions require 'txnTieKnot' post-processing.
--
-- >>> _ptgen "monthly from 2017/1 to 2017/4"
-- 2017/01/01
--     ; recur: monthly from 2017/1 to 2017/4
--     a           $1.00
-- <BLANKLINE>
-- 2017/02/01
--     ; recur: monthly from 2017/1 to 2017/4
--     a           $1.00
-- <BLANKLINE>
-- 2017/03/01
--     ; recur: monthly from 2017/1 to 2017/4
--     a           $1.00
-- <BLANKLINE>
--
-- >>> _ptgen "monthly from 2017/1 to 2017/5"
-- 2017/01/01
--     ; recur: monthly from 2017/1 to 2017/5
--     a           $1.00
-- <BLANKLINE>
-- 2017/02/01
--     ; recur: monthly from 2017/1 to 2017/5
--     a           $1.00
-- <BLANKLINE>
-- 2017/03/01
--     ; recur: monthly from 2017/1 to 2017/5
--     a           $1.00
-- <BLANKLINE>
-- 2017/04/01
--     ; recur: monthly from 2017/1 to 2017/5
--     a           $1.00
-- <BLANKLINE>
--
-- >>> _ptgen "every 2nd day of month from 2017/02 to 2017/04"
-- 2017/01/02
--     ; recur: every 2nd day of month from 2017/02 to 2017/04
--     a           $1.00
-- <BLANKLINE>
-- 2017/02/02
--     ; recur: every 2nd day of month from 2017/02 to 2017/04
--     a           $1.00
-- <BLANKLINE>
-- 2017/03/02
--     ; recur: every 2nd day of month from 2017/02 to 2017/04
--     a           $1.00
-- <BLANKLINE>
--
-- >>> _ptgen "every 30th day of month from 2017/1 to 2017/5"
-- 2016/12/30
--     ; recur: every 30th day of month from 2017/1 to 2017/5
--     a           $1.00
-- <BLANKLINE>
-- 2017/01/30
--     ; recur: every 30th day of month from 2017/1 to 2017/5
--     a           $1.00
-- <BLANKLINE>
-- 2017/02/28
--     ; recur: every 30th day of month from 2017/1 to 2017/5
--     a           $1.00
-- <BLANKLINE>
-- 2017/03/30
--     ; recur: every 30th day of month from 2017/1 to 2017/5
--     a           $1.00
-- <BLANKLINE>
-- 2017/04/30
--     ; recur: every 30th day of month from 2017/1 to 2017/5
--     a           $1.00
-- <BLANKLINE>
--
-- >>> _ptgen "every 2nd Thursday of month from 2017/1 to 2017/4"
-- 2016/12/08
--     ; recur: every 2nd Thursday of month from 2017/1 to 2017/4
--     a           $1.00
-- <BLANKLINE>
-- 2017/01/12
--     ; recur: every 2nd Thursday of month from 2017/1 to 2017/4
--     a           $1.00
-- <BLANKLINE>
-- 2017/02/09
--     ; recur: every 2nd Thursday of month from 2017/1 to 2017/4
--     a           $1.00
-- <BLANKLINE>
-- 2017/03/09
--     ; recur: every 2nd Thursday of month from 2017/1 to 2017/4
--     a           $1.00
-- <BLANKLINE>
--
-- >>> _ptgen "every nov 29th from 2017 to 2019"
-- 2016/11/29
--     ; recur: every nov 29th from 2017 to 2019
--     a           $1.00
-- <BLANKLINE>
-- 2017/11/29
--     ; recur: every nov 29th from 2017 to 2019
--     a           $1.00
-- <BLANKLINE>
-- 2018/11/29
--     ; recur: every nov 29th from 2017 to 2019
--     a           $1.00
-- <BLANKLINE>
--
-- >>> _ptgen "2017/1"
-- 2017/01/01
--     ; recur: 2017/1
--     a           $1.00
-- <BLANKLINE>
--
-- >>> _ptgen ""
-- *** Exception: failed to parse...
-- ...
--
-- >>> _ptgen "weekly from 2017"
-- *** Exception: Unable to generate transactions according to "weekly from 2017" because 2017-01-01 is not a first day of the week
--
-- >>> _ptgen "monthly from 2017/5/4"
-- *** Exception: Unable to generate transactions according to "monthly from 2017/5/4" because 2017-05-04 is not a first day of the month        
--
-- >>> _ptgen "every quarter from 2017/1/2"
-- *** Exception: Unable to generate transactions according to "every quarter from 2017/1/2" because 2017-01-02 is not a first day of the quarter        
--
-- >>> _ptgen "yearly from 2017/1/14"
-- *** Exception: Unable to generate transactions according to "yearly from 2017/1/14" because 2017-01-14 is not a first day of the year        
--
-- >>> let reportperiod="daily from 2018/01/03" in let (i,s) = parsePeriodExpr' nulldate reportperiod in runPeriodicTransaction (nullperiodictransaction{ptperiodexpr=reportperiod, ptspan=s, ptinterval=i, ptpostings=["a" `post` usd 1]}) (DateSpan (Just $ parsedate "2018-01-01") (Just $ parsedate "2018-01-03"))
-- []
--
runPeriodicTransaction :: PeriodicTransaction -> DateSpan -> [Transaction]
runPeriodicTransaction PeriodicTransaction{..} requestedspan =
    [ t{tdate=d} | (DateSpan (Just d) _) <- ptinterval `splitSpan` spantofill ]
  where
    spantofill = spanIntervalIntersect ptinterval ptspan requestedspan
    t = nulltransaction{
           tstatus      = ptstatus
          ,tcode        = ptcode
          ,tdescription = ptdescription 
          ,tcomment     = (if T.null ptcomment then "\n" else ptcomment) <> "recur: " <> ptperiodexpr
          ,ttags        = ("recur", ptperiodexpr) : pttags 
          ,tpostings    = ptpostings
          }

-- | Check that this date span begins at a boundary of this interval, 
-- or return an explanatory error message including the provided period expression
-- (from which the span and interval are derived).
checkPeriodicTransactionStartDate :: Interval -> DateSpan -> T.Text -> Maybe String 
checkPeriodicTransactionStartDate i s periodexpr = 
  case (i, spanStart s) of
    (Weeks _,    Just d) -> checkStart d "week"
    (Months _,   Just d) -> checkStart d "month"
    (Quarters _, Just d) -> checkStart d "quarter"
    (Years _,    Just d) -> checkStart d "year"
    _                    -> Nothing 
    where
      checkStart d x =
        let firstDate = fixSmartDate d ("","this",x) 
        in   
         if d == firstDate 
         then Nothing
         else Just $
          "Unable to generate transactions according to "++show (T.unpack periodexpr)
          ++" because "++show d++" is not a first day of the "++x

---- | What is the interval of this 'PeriodicTransaction's period expression, if it can be parsed ?
--periodTransactionInterval :: PeriodicTransaction -> Maybe Interval
--periodTransactionInterval pt =
--  let
--    expr = ptperiodexpr pt
--    err  = error' $ "Current date cannot be referenced in " ++ show (T.unpack expr)
--  in
--    case parsePeriodExpr err expr of
--      Left _      -> Nothing
--      Right (i,_) -> Just i
