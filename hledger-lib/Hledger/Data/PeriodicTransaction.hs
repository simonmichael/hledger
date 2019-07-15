{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-|

A 'PeriodicTransaction' is a rule describing recurring transactions.

-}
module Hledger.Data.PeriodicTransaction (
    runPeriodicTransaction
  , checkPeriodicTransactionStartDate
)
where

#if !(MIN_VERSION_base(4,11,0))
import Data.Monoid ((<>))
#endif
import qualified Data.Text as T
import Text.Printf

import Hledger.Data.Types
import Hledger.Data.Dates
import Hledger.Data.Amount
import Hledger.Data.Posting (post)
import Hledger.Data.Transaction
import Hledger.Utils.UTF8IOCompat (error')
-- import Hledger.Utils.Debug

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Hledger.Data.Posting
-- >>> import Hledger.Data.Journal

-- doctest helper, too much hassle to define in the comment
-- XXX duplicates some logic in periodictransactionp
_ptgen str = do
  let
    t = T.pack str
    (i,s) = parsePeriodExpr' nulldate t
  case checkPeriodicTransactionStartDate i s t of
    Just e  -> error' e
    Nothing ->
      mapM_ (putStr . showTransaction) $
        runPeriodicTransaction
          nullperiodictransaction{ ptperiodexpr=t , ptspan=s, ptinterval=i, ptpostings=["a" `post` usd 1] }
          nulldatespan


--deriving instance Show PeriodicTransaction
-- for better pretty-printing:
instance Show PeriodicTransaction where
  show PeriodicTransaction{..} =
    printf "PeriodicTransactionPP {%s, %s, %s, %s, %s, %s, %s, %s, %s}"
      ("ptperiodexpr=" ++ show ptperiodexpr)
      ("ptinterval=" ++ show ptinterval)
      ("ptspan=" ++ show (show ptspan))
      ("ptstatus=" ++ show (show ptstatus))
      ("ptcode=" ++ show ptcode)
      ("ptdescription=" ++ show ptdescription)
      ("ptcomment=" ++ show ptcomment)
      ("pttags=" ++ show pttags)
      ("ptpostings=" ++ show ptpostings)

-- A basic human-readable rendering.
--showPeriodicTransaction t = "~ " ++ T.unpack (ptperiodexpr t) ++ "\n" ++ unlines (map show (ptpostings t))

--nullperiodictransaction is defined in Types.hs

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
