{-|

Manipulate the time periods typically used for reports with Period,
a richer abstraction that will probably replace DateSpan.
See also Types and Dates.

-}

module Hledger.Data.Period
where

import Data.Time.Calendar
import Data.Time.Calendar.WeekDate
import Data.Time.Format
import Text.Printf

import Hledger.Data.Types

-- | Convert Periods to DateSpans.
--
-- >>> periodAsDateSpan (MonthPeriod 2000 1) == DateSpan (Just $ fromGregorian 2000 1 1) (Just $ fromGregorian 2000 2 1)
-- True
periodAsDateSpan :: Period -> DateSpan
periodAsDateSpan (DayPeriod d) = DateSpan (Just d) (Just $ addDays 1 d)
periodAsDateSpan (WeekPeriod b) = DateSpan (Just b) (Just $ addDays 7 b)
periodAsDateSpan (MonthPeriod y m) = DateSpan (Just $ fromGregorian y m 1) (Just $ fromGregorian y' m' 1)
  where
    (y',m') | m==12     = (y+1,1)
            | otherwise = (y,m+1)
periodAsDateSpan (QuarterPeriod y q) = DateSpan (Just $ fromGregorian y m 1) (Just $ fromGregorian y' m' 1)
  where
    (y', q') | q==4      = (y+1,1)
             | otherwise = (y,q+1)
    quarterAsMonth q = (q-1) * 3 + 1
    m  = quarterAsMonth q
    m' = quarterAsMonth q'
periodAsDateSpan (YearPeriod y) = DateSpan (Just $ fromGregorian y 1 1) (Just $ fromGregorian (y+1) 1 1)
periodAsDateSpan (PeriodBetween b e) = DateSpan (Just b) (Just e)
periodAsDateSpan (PeriodFrom b) = DateSpan (Just b) Nothing
periodAsDateSpan (PeriodTo e) = DateSpan Nothing (Just e)
periodAsDateSpan (PeriodAll) = DateSpan Nothing Nothing

-- | Convert DateSpans to Periods.
--
-- >>> dateSpanAsPeriod $ DateSpan (Just $ fromGregorian 2000 1 1) (Just $ fromGregorian 2000 2 1)
-- MonthPeriod 2000 1
dateSpanAsPeriod :: DateSpan -> Period
dateSpanAsPeriod (DateSpan (Just b) (Just e)) = simplifyPeriod $ PeriodBetween b e
dateSpanAsPeriod (DateSpan (Just b) Nothing) = PeriodFrom b
dateSpanAsPeriod (DateSpan Nothing (Just e)) = PeriodTo e
dateSpanAsPeriod (DateSpan Nothing Nothing) = PeriodAll

-- | Convert PeriodBetweens to a more abstract period where possible.
--
-- >>> simplifyPeriod $ PeriodBetween (fromGregorian 1 1 1) (fromGregorian 2 1 1)
-- YearPeriod 1
-- >>> simplifyPeriod $ PeriodBetween (fromGregorian 2000 10 1) (fromGregorian 2001 1 1)
-- QuarterPeriod 2000 4
-- >>> simplifyPeriod $ PeriodBetween (fromGregorian 2000 2 1) (fromGregorian 2000 3 1)
-- MonthPeriod 2000 2
-- >>> simplifyPeriod $ PeriodBetween (fromGregorian 2016 7 25) (fromGregorian 2016 8 1)
-- WeekPeriod 2016-07-25
-- >>> simplifyPeriod $ PeriodBetween (fromGregorian 2000 1 1) (fromGregorian 2000 1 2)
-- DayPeriod 2000-01-01
-- >>> simplifyPeriod $ PeriodBetween (fromGregorian 2000 2 28) (fromGregorian 2000 3 1)
-- PeriodBetween 2000-02-28 2000-03-01
-- >>> simplifyPeriod $ PeriodBetween (fromGregorian 2000 2 29) (fromGregorian 2000 3 1)
-- DayPeriod 2000-02-29
-- >>> simplifyPeriod $ PeriodBetween (fromGregorian 2000 12 31) (fromGregorian 2001 1 1)
-- DayPeriod 2000-12-31
--
simplifyPeriod :: Period -> Period
simplifyPeriod (PeriodBetween b e) =
  case (toGregorian b, toGregorian e) of
    -- a year
    ((by,1,1), (ey,1,1))   | by+1==ey           -> YearPeriod by
    -- a half-year
    -- ((by,1,1), (ey,7,1))   | by==ey             ->
    -- ((by,7,1), (ey,1,1))   | by+1==ey           ->
    -- a quarter
    ((by,1,1), (ey,4,1))   | by==ey             -> QuarterPeriod by 1
    ((by,4,1), (ey,7,1))   | by==ey             -> QuarterPeriod by 2
    ((by,7,1), (ey,10,1))  | by==ey             -> QuarterPeriod by 3
    ((by,10,1), (ey,1,1))  | by+1==ey           -> QuarterPeriod by 4
    -- a month
    ((by,bm,1), (ey,em,1)) | by==ey && bm+1==em -> MonthPeriod by bm
    ((by,12,1), (ey,1,1))  | by+1==ey           -> MonthPeriod by 12
    -- a week (two successive mondays),
    -- YYYYwN ("week N of year YYYY")
    -- _ | let ((by,bw,bd), (ey,ew,ed)) = (toWeekDate from, toWeekDate to) in by==ey && fw+1==tw && bd==1 && ed==1 ->
    -- a week starting on a monday
    _ | let ((by,bw,bd), (ey,ew,ed)) = (toWeekDate b, toWeekDate (addDays (-1) e))
        in by==ey && bw==ew && bd==1 && ed==7   -> WeekPeriod b
    -- a day
    ((by,bm,bd), (ey,em,ed)) |
        (by==ey && bm==em && bd+1==ed) ||
        (by+1==ey && bm==12 && em==1 && bd==31 && ed==1) || -- crossing a year boundary
        (by==ey && bm+1==em && isLastDayOfMonth by bm bd && ed==1) -- crossing a month boundary
         -> DayPeriod b
    _ -> PeriodBetween b e
simplifyPeriod p = p

isLastDayOfMonth y m d =
  case m of
    1 -> d==31
    2 | isLeapYear y -> d==29
      | otherwise    -> d==28
    3 -> d==31
    4 -> d==30
    5 -> d==31
    6 -> d==30
    7 -> d==31
    8 -> d==31
    9 -> d==30
    10 -> d==31
    11 -> d==30
    12 -> d==31
    _ -> False

-- | Render a period as a compact display string suitable for user output.
--
-- >>> showPeriod (WeekPeriod (fromGregorian 2016 7 25))
-- "2016/07/25w30"
showPeriod (DayPeriod b)       = formatTime defaultTimeLocale "%0C%y/%m/%dd" b    -- DATEd
showPeriod (WeekPeriod b)      = formatTime defaultTimeLocale "%0C%y/%m/%dw%V" b  -- STARTDATEwYEARWEEK
showPeriod (MonthPeriod y m)   = printf "%04d/%02d" y m                           -- YYYY/MM
showPeriod (QuarterPeriod y q) = printf "%04dq%d" y q                             -- YYYYqN
showPeriod (YearPeriod y)      = printf "%04d" y                                  -- YYYY
showPeriod (PeriodBetween b e) = formatTime defaultTimeLocale "%0C%y/%m/%d" b
                                 ++ formatTime defaultTimeLocale "-%0C%y/%m/%d" (addDays (-1) e) -- STARTDATE-INCLUSIVEENDDATE
showPeriod (PeriodFrom b)      = formatTime defaultTimeLocale "%0C%y/%m/%d-" b                   -- STARTDATE-
showPeriod (PeriodTo e)        = formatTime defaultTimeLocale "-%0C%y/%m/%d" (addDays (-1) e)    -- -INCLUSIVEENDDATE
showPeriod PeriodAll           = "-"

