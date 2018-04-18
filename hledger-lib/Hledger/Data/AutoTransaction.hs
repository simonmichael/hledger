{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE CPP #-}
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
    , periodTransactionInterval
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
import Hledger.Data.Transaction
import Hledger.Utils.Parse
import Hledger.Utils.UTF8IOCompat (error')
import Hledger.Query
-- import Hledger.Utils.Debug

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Hledger.Data.Posting
-- >>> import Hledger.Data.Journal

-- | Builds a 'Transaction' transformer based on 'ModifierTransaction'.
--
-- 'Query' parameter allows injection of additional restriction on posting
-- match. Don't forget to call 'txnTieKnot'.
--
-- >>> runModifierTransaction Any (ModifierTransaction "" ["pong" `post` usd 2]) nulltransaction{tpostings=["ping" `post` usd 1]}
-- 0000/01/01
--     ping           $1.00
--     pong           $2.00
-- <BLANKLINE>
-- <BLANKLINE>
-- >>> runModifierTransaction Any (ModifierTransaction "miss" ["pong" `post` usd 2]) nulltransaction{tpostings=["ping" `post` usd 1]}
-- 0000/01/01
--     ping           $1.00
-- <BLANKLINE>
-- <BLANKLINE>
-- >>> runModifierTransaction None (ModifierTransaction "" ["pong" `post` usd 2]) nulltransaction{tpostings=["ping" `post` usd 1]}
-- 0000/01/01
--     ping           $1.00
-- <BLANKLINE>
-- <BLANKLINE>
-- >>> runModifierTransaction Any (ModifierTransaction "ping" ["pong" `post` amount{amultiplier=True, aquantity=3}]) nulltransaction{tpostings=["ping" `post` usd 2]}
-- 0000/01/01
--     ping           $2.00
--     pong           $6.00
-- <BLANKLINE>
-- <BLANKLINE>
runModifierTransaction :: Query -> ModifierTransaction -> (Transaction -> Transaction)
runModifierTransaction q mt = modifier where
    q' = simplifyQuery $ And [q, mtvaluequery mt (error "query cannot depend on current time")]
    mods = map runModifierPosting $ mtpostings mt
    generatePostings ps = [p' | p <- ps
                              , p' <- if q' `matchesPosting` p then p:[ m p | m <- mods] else [p]]
    modifier t@(tpostings -> ps) = t { tpostings = generatePostings ps }

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
-- >>> let gen str = mapM_ (putStr . show) $ runPeriodicTransaction (PeriodicTransaction str ["hi" `post` usd 1]) nulldatespan
-- >>> gen "monthly from 2017/1 to 2017/4"
-- 2017/01/01 Forecast transaction (monthly from 2017/1 to 2017/4)
--     hi           $1.00
-- <BLANKLINE>
-- 2017/02/01 Forecast transaction (monthly from 2017/1 to 2017/4)
--     hi           $1.00
-- <BLANKLINE>
-- 2017/03/01 Forecast transaction (monthly from 2017/1 to 2017/4)
--     hi           $1.00
-- <BLANKLINE>
-- >>> gen "monthly from 2017/1 to 2017/5"
-- 2017/01/01 Forecast transaction (monthly from 2017/1 to 2017/5)
--     hi           $1.00
-- <BLANKLINE>
-- 2017/02/01 Forecast transaction (monthly from 2017/1 to 2017/5)
--     hi           $1.00
-- <BLANKLINE>
-- 2017/03/01 Forecast transaction (monthly from 2017/1 to 2017/5)
--     hi           $1.00
-- <BLANKLINE>
-- 2017/04/01 Forecast transaction (monthly from 2017/1 to 2017/5)
--     hi           $1.00
-- <BLANKLINE>
-- >>> gen "every 2nd day of month from 2017/02 to 2017/04"
-- 2017/01/02 Forecast transaction (every 2nd day of month from 2017/02 to 2017/04)
--     hi           $1.00
-- <BLANKLINE>
-- 2017/02/02 Forecast transaction (every 2nd day of month from 2017/02 to 2017/04)
--     hi           $1.00
-- <BLANKLINE>
-- 2017/03/02 Forecast transaction (every 2nd day of month from 2017/02 to 2017/04)
--     hi           $1.00
-- <BLANKLINE>
-- >>> gen "every 30th day of month from 2017/1 to 2017/5"
-- 2016/12/30 Forecast transaction (every 30th day of month from 2017/1 to 2017/5)
--     hi           $1.00
-- <BLANKLINE>
-- 2017/01/30 Forecast transaction (every 30th day of month from 2017/1 to 2017/5)
--     hi           $1.00
-- <BLANKLINE>
-- 2017/02/28 Forecast transaction (every 30th day of month from 2017/1 to 2017/5)
--     hi           $1.00
-- <BLANKLINE>
-- 2017/03/30 Forecast transaction (every 30th day of month from 2017/1 to 2017/5)
--     hi           $1.00
-- <BLANKLINE>
-- 2017/04/30 Forecast transaction (every 30th day of month from 2017/1 to 2017/5)
--     hi           $1.00
-- <BLANKLINE>
-- >>> gen "every 2nd Thursday of month from 2017/1 to 2017/4"
-- 2016/12/08 Forecast transaction (every 2nd Thursday of month from 2017/1 to 2017/4)
--     hi           $1.00
-- <BLANKLINE>
-- 2017/01/12 Forecast transaction (every 2nd Thursday of month from 2017/1 to 2017/4)
--     hi           $1.00
-- <BLANKLINE>
-- 2017/02/09 Forecast transaction (every 2nd Thursday of month from 2017/1 to 2017/4)
--     hi           $1.00
-- <BLANKLINE>
-- 2017/03/09 Forecast transaction (every 2nd Thursday of month from 2017/1 to 2017/4)
--     hi           $1.00
-- <BLANKLINE>
-- >>> gen "every nov 29th from 2017 to 2019"
-- 2016/11/29 Forecast transaction (every nov 29th from 2017 to 2019)
--     hi           $1.00
-- <BLANKLINE>
-- 2017/11/29 Forecast transaction (every nov 29th from 2017 to 2019)
--     hi           $1.00
-- <BLANKLINE>
-- 2018/11/29 Forecast transaction (every nov 29th from 2017 to 2019)
--     hi           $1.00
-- <BLANKLINE>
-- >>> gen "2017/1"
-- 2017/01/01 Forecast transaction (2017/1)
--     hi           $1.00
-- <BLANKLINE>
-- >>> gen ""
-- ... Failed to parse ...
-- >>> gen "weekly from 2017"
-- *** Exception: Unable to generate transactions according to "weekly from 2017" as 2017-01-01 is not a first day of the week
-- >>> gen "monthly from 2017/5/4"
-- *** Exception: Unable to generate transactions according to "monthly from 2017/5/4" as 2017-05-04 is not a first day of the month        
-- >>> gen "every quarter from 2017/1/2"
-- *** Exception: Unable to generate transactions according to "every quarter from 2017/1/2" as 2017-01-02 is not a first day of the quarter        
-- >>> gen "yearly from 2017/1/14"
-- *** Exception: Unable to generate transactions according to "yearly from 2017/1/14" as 2017-01-14 is not a first day of the year        
--
-- >>> let reportperiod="daily from 2018/01/03" in runPeriodicTransaction (PeriodicTransaction reportperiod [post "a" (usd 1)]) (DateSpan (Just $ parsedate "2018-01-01") (Just $ parsedate "2018-01-03"))
-- []
runPeriodicTransaction :: PeriodicTransaction -> (DateSpan -> [Transaction])
runPeriodicTransaction pt =
  \requestedspan ->
    let fillspan = ptspan `spanIntersect` requestedspan
    in  [ t{tdate=d} | (DateSpan (Just d) _) <- ptinterval `splitSpan` fillspan ]
  where
    descr = T.pack $ "Forecast transaction (" ++ T.unpack periodexpr ++ ")"
    t = nulltransaction { tpostings = ptpostings pt, tdescription = descr }
    periodexpr = ptperiodicexpr pt
    currentdateerr = error' $ "Current date cannot be referenced in " ++ show (T.unpack periodexpr)
    (ptinterval, ptspan) =
        case parsePeriodExpr currentdateerr periodexpr of
            Left e  -> error' $ "Failed to parse " ++ show (T.unpack periodexpr) ++ ": " ++ showDateParseError e
            Right x -> checkPeriodTransactionStartDate periodexpr x

checkPeriodTransactionStartDate :: T.Text -> (Interval, DateSpan) -> (Interval, DateSpan)
checkPeriodTransactionStartDate periodexpr (i,s) = 
  case (i,spanStart s) of
    (Weeks _,    Just d) -> checkStart d "week"
    (Months _,   Just d) -> checkStart d "month"
    (Quarters _, Just d) -> checkStart d "quarter"
    (Years _,    Just d) -> checkStart d "year"
    _                    -> (i,s) 
    where
      checkStart d x =
        let firstDate = fixSmartDate d ("","this",x) 
        in   
         if d == firstDate then (i,s)
         else error' $ "Unable to generate transactions according to "++(show periodexpr)++" as "++(show d)++" is not a first day of the "++x

-- | What is the interval of this 'PeriodicTransaction's period expression, if it can be parsed ?
periodTransactionInterval :: PeriodicTransaction -> Maybe Interval
periodTransactionInterval pt =
  let
    expr = ptperiodicexpr pt
    err  = error' $ "Current date cannot be referenced in " ++ show (T.unpack expr)
  in
    case parsePeriodExpr err expr of
      Left _      -> Nothing
      Right (i,_) -> Just i
