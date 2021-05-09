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

import qualified Data.Text as T
import qualified Data.Text.IO as T
import Text.Printf

import Hledger.Data.Types
import Hledger.Data.Dates
import Hledger.Data.Amount
import Hledger.Data.Posting (post, commentAddTagNextLine)
import Hledger.Data.Transaction
import Hledger.Utils.UTF8IOCompat (error')
import Hledger.Utils.Debug

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
    Just e  -> error' e  -- PARTIAL:
    Nothing ->
      mapM_ (T.putStr . showTransaction) $
        runPeriodicTransaction
          nullperiodictransaction{ ptperiodexpr=t , ptspan=s, ptinterval=i, ptpostings=["a" `post` usd 1] }
          nulldatespan

_ptgenspan str span = do
  let
    t = T.pack str
    (i,s) = parsePeriodExpr' nulldate t
  case checkPeriodicTransactionStartDate i s t of
    Just e  -> error' e  -- PARTIAL:
    Nothing ->
      mapM_ (T.putStr . showTransaction) $
        runPeriodicTransaction
          nullperiodictransaction{ ptperiodexpr=t , ptspan=s, ptinterval=i, ptpostings=["a" `post` usd 1] }
          span

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
-- The new transactions will have three tags added: 
-- - a recur:PERIODICEXPR tag whose value is the generating periodic expression
-- - a generated-transaction: tag
-- - a hidden _generated-transaction: tag which does not appear in the comment. 
--
-- >>> import Data.Time (fromGregorian)
-- >>> _ptgen "monthly from 2017/1 to 2017/4"
-- 2017-01-01
--     ; generated-transaction: ~ monthly from 2017/1 to 2017/4
--     a           $1.00
-- <BLANKLINE>
-- 2017-02-01
--     ; generated-transaction: ~ monthly from 2017/1 to 2017/4
--     a           $1.00
-- <BLANKLINE>
-- 2017-03-01
--     ; generated-transaction: ~ monthly from 2017/1 to 2017/4
--     a           $1.00
-- <BLANKLINE>
--
-- >>> _ptgen "monthly from 2017/1 to 2017/5"
-- 2017-01-01
--     ; generated-transaction: ~ monthly from 2017/1 to 2017/5
--     a           $1.00
-- <BLANKLINE>
-- 2017-02-01
--     ; generated-transaction: ~ monthly from 2017/1 to 2017/5
--     a           $1.00
-- <BLANKLINE>
-- 2017-03-01
--     ; generated-transaction: ~ monthly from 2017/1 to 2017/5
--     a           $1.00
-- <BLANKLINE>
-- 2017-04-01
--     ; generated-transaction: ~ monthly from 2017/1 to 2017/5
--     a           $1.00
-- <BLANKLINE>
--
-- >>> _ptgen "every 2nd day of month from 2017/02 to 2017/04"
-- 2017-01-02
--     ; generated-transaction: ~ every 2nd day of month from 2017/02 to 2017/04
--     a           $1.00
-- <BLANKLINE>
-- 2017-02-02
--     ; generated-transaction: ~ every 2nd day of month from 2017/02 to 2017/04
--     a           $1.00
-- <BLANKLINE>
-- 2017-03-02
--     ; generated-transaction: ~ every 2nd day of month from 2017/02 to 2017/04
--     a           $1.00
-- <BLANKLINE>
--
-- >>> _ptgen "every 30th day of month from 2017/1 to 2017/5"
-- 2016-12-30
--     ; generated-transaction: ~ every 30th day of month from 2017/1 to 2017/5
--     a           $1.00
-- <BLANKLINE>
-- 2017-01-30
--     ; generated-transaction: ~ every 30th day of month from 2017/1 to 2017/5
--     a           $1.00
-- <BLANKLINE>
-- 2017-02-28
--     ; generated-transaction: ~ every 30th day of month from 2017/1 to 2017/5
--     a           $1.00
-- <BLANKLINE>
-- 2017-03-30
--     ; generated-transaction: ~ every 30th day of month from 2017/1 to 2017/5
--     a           $1.00
-- <BLANKLINE>
-- 2017-04-30
--     ; generated-transaction: ~ every 30th day of month from 2017/1 to 2017/5
--     a           $1.00
-- <BLANKLINE>
--
-- >>> _ptgen "every 2nd Thursday of month from 2017/1 to 2017/4"
-- 2016-12-08
--     ; generated-transaction: ~ every 2nd Thursday of month from 2017/1 to 2017/4
--     a           $1.00
-- <BLANKLINE>
-- 2017-01-12
--     ; generated-transaction: ~ every 2nd Thursday of month from 2017/1 to 2017/4
--     a           $1.00
-- <BLANKLINE>
-- 2017-02-09
--     ; generated-transaction: ~ every 2nd Thursday of month from 2017/1 to 2017/4
--     a           $1.00
-- <BLANKLINE>
-- 2017-03-09
--     ; generated-transaction: ~ every 2nd Thursday of month from 2017/1 to 2017/4
--     a           $1.00
-- <BLANKLINE>
--
-- >>> _ptgen "every nov 29th from 2017 to 2019"
-- 2016-11-29
--     ; generated-transaction: ~ every nov 29th from 2017 to 2019
--     a           $1.00
-- <BLANKLINE>
-- 2017-11-29
--     ; generated-transaction: ~ every nov 29th from 2017 to 2019
--     a           $1.00
-- <BLANKLINE>
-- 2018-11-29
--     ; generated-transaction: ~ every nov 29th from 2017 to 2019
--     a           $1.00
-- <BLANKLINE>
--
-- >>> _ptgen "2017/1"
-- 2017-01-01
--     ; generated-transaction: ~ 2017/1
--     a           $1.00
-- <BLANKLINE>
--
-- >>> _ptgen ""
-- *** Exception: failed to parse...
-- ...
--
-- >>> _ptgen "weekly from 2017"
-- *** Exception: Unable to generate transactions according to "weekly from 2017" because 2017-01-01 is not a first day of the Week
--
-- >>> _ptgen "monthly from 2017/5/4"
-- *** Exception: Unable to generate transactions according to "monthly from 2017/5/4" because 2017-05-04 is not a first day of the Month
--
-- >>> _ptgen "every quarter from 2017/1/2"
-- *** Exception: Unable to generate transactions according to "every quarter from 2017/1/2" because 2017-01-02 is not a first day of the Quarter
--
-- >>> _ptgen "yearly from 2017/1/14"
-- *** Exception: Unable to generate transactions according to "yearly from 2017/1/14" because 2017-01-14 is not a first day of the Year
--
-- >>> let reportperiod="daily from 2018/01/03" in let (i,s) = parsePeriodExpr' nulldate reportperiod in runPeriodicTransaction (nullperiodictransaction{ptperiodexpr=reportperiod, ptspan=s, ptinterval=i, ptpostings=["a" `post` usd 1]}) (DateSpan (Just $ fromGregorian 2018 01 01) (Just $ fromGregorian 2018 01 03))
-- []
--
-- >>> _ptgenspan "every 3 months from 2019-05" (DateSpan (Just $ fromGregorian 2020 01 01) (Just $ fromGregorian 2020 02 01))
--
-- >>> _ptgenspan "every 3 months from 2019-05" (DateSpan (Just $ fromGregorian 2020 02 01) (Just $ fromGregorian 2020 03 01))
-- 2020-02-01
--     ; generated-transaction: ~ every 3 months from 2019-05
--     a           $1.00
-- <BLANKLINE>
-- >>> _ptgenspan "every 3 days from 2018" (DateSpan (Just $ fromGregorian 2018 01 01) (Just $ fromGregorian 2018 01 05))
-- 2018-01-01
--     ; generated-transaction: ~ every 3 days from 2018
--     a           $1.00
-- <BLANKLINE>
-- 2018-01-04
--     ; generated-transaction: ~ every 3 days from 2018
--     a           $1.00
-- <BLANKLINE>
-- >>> _ptgenspan "every 3 days from 2018" (DateSpan (Just $ fromGregorian 2018 01 02) (Just $ fromGregorian 2018 01 05))
-- 2018-01-04
--     ; generated-transaction: ~ every 3 days from 2018
--     a           $1.00
-- <BLANKLINE>

runPeriodicTransaction :: PeriodicTransaction -> DateSpan -> [Transaction]
runPeriodicTransaction PeriodicTransaction{..} requestedspan =
    [ t{tdate=d} | (DateSpan (Just d) _) <- alltxnspans, spanContainsDate requestedspan d ]
  where
    t = nulltransaction{
           tstatus      = ptstatus
          ,tcode        = ptcode
          ,tdescription = ptdescription
          ,tcomment     = ptcomment
                          `commentAddTagNextLine` ("generated-transaction",period)
          ,ttags        = ("_generated-transaction",period) :
                          ("generated-transaction" ,period) :
                          pttags
          ,tpostings    = ptpostings
          }
    period = "~ " <> ptperiodexpr
    -- All spans described by this periodic transaction, where spanStart is event date.
    -- If transaction does not have start/end date, we set them to start/end of requested span,
    -- to avoid generating (infinitely) many events. 
    alltxnspans = dbg3 "alltxnspans" $ ptinterval `splitSpan` (spanDefaultsFrom ptspan requestedspan)

-- | Check that this date span begins at a boundary of this interval,
-- or return an explanatory error message including the provided period expression
-- (from which the span and interval are derived).
checkPeriodicTransactionStartDate :: Interval -> DateSpan -> T.Text -> Maybe String
checkPeriodicTransactionStartDate i s periodexpr =
  case (i, spanStart s) of
    (Weeks _,    Just d) -> checkStart d Week
    (Months _,   Just d) -> checkStart d Month
    (Quarters _, Just d) -> checkStart d Quarter
    (Years _,    Just d) -> checkStart d Year
    _                    -> Nothing
    where
      checkStart d x =
        let firstDate = fixSmartDate d $ SmartRelative This x
        in
         if d == firstDate
         then Nothing
         else Just $
          "Unable to generate transactions according to "++show (T.unpack periodexpr)
          ++" because "++show d++" is not a first day of the "++show x

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
