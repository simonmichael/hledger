{-|

Manipulate the time periods typically used for reports with Period,
a richer abstraction than DateSpan. See also Types and Dates.

-}

module Hledger.Data.Period
where

import Data.Time.Calendar
import Data.Time.Calendar.MonthDay
import Data.Time.Calendar.OrdinalDate
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
showPeriod (DayPeriod b)       = formatTime defaultTimeLocale "%0C%y/%m/%d" b     -- DATE
showPeriod (WeekPeriod b)      = formatTime defaultTimeLocale "%0C%y/%m/%dw%V" b  -- STARTDATEwYEARWEEK
showPeriod (MonthPeriod y m)   = printf "%04d/%02d" y m                           -- YYYY/MM
showPeriod (QuarterPeriod y q) = printf "%04dq%d" y q                             -- YYYYqN
showPeriod (YearPeriod y)      = printf "%04d" y                                  -- YYYY
showPeriod (PeriodBetween b e) = formatTime defaultTimeLocale "%0C%y/%m/%d" b
                                 ++ formatTime defaultTimeLocale "-%0C%y/%m/%d" (addDays (-1) e) -- STARTDATE-INCLUSIVEENDDATE
showPeriod (PeriodFrom b)      = formatTime defaultTimeLocale "%0C%y/%m/%d-" b                   -- STARTDATE-
showPeriod (PeriodTo e)        = formatTime defaultTimeLocale "-%0C%y/%m/%d" (addDays (-1) e)    -- -INCLUSIVEENDDATE
showPeriod PeriodAll           = "-"

periodStart :: Period -> Maybe Day
periodStart p = mb
  where
    DateSpan mb _ = periodAsDateSpan p

periodEnd :: Period -> Maybe Day
periodEnd p = me
  where
    DateSpan _ me = periodAsDateSpan p

-- | Move a period to the following period of same duration.
periodNext :: Period -> Period
periodNext (DayPeriod b) = DayPeriod (addDays 1 b)
periodNext (WeekPeriod b) = WeekPeriod (addDays 7 b)
periodNext (MonthPeriod y 12) = MonthPeriod (y+1) 1
periodNext (MonthPeriod y m) = MonthPeriod y (m+1)
periodNext (QuarterPeriod y 4) = QuarterPeriod (y+1) 1
periodNext (QuarterPeriod y q) = QuarterPeriod y (q+1)
periodNext (YearPeriod y) = YearPeriod (y+1)
periodNext p = p

-- | Move a period to the preceding period of same duration.
periodPrevious :: Period -> Period
periodPrevious (DayPeriod b) = DayPeriod (addDays (-1) b)
periodPrevious (WeekPeriod b) = WeekPeriod (addDays (-7) b)
periodPrevious (MonthPeriod y 1) = MonthPeriod (y-1) 12
periodPrevious (MonthPeriod y m) = MonthPeriod y (m-1)
periodPrevious (QuarterPeriod y 1) = QuarterPeriod (y-1) 4
periodPrevious (QuarterPeriod y q) = QuarterPeriod y (q-1)
periodPrevious (YearPeriod y) = YearPeriod (y-1)
periodPrevious p = p

-- | Move a period to the following period of same duration, staying within enclosing dates.
periodNextIn :: DateSpan -> Period -> Period
periodNextIn (DateSpan _ (Just e)) p =
  case mb of
    Just b -> if b < e then p' else p
    _      -> p
  where
    p' = periodNext p
    mb = periodStart p'
periodNextIn _ p = periodNext p

-- | Move a period to the preceding period of same duration, staying within enclosing dates.
periodPreviousIn :: DateSpan -> Period -> Period
periodPreviousIn (DateSpan (Just b) _) p =
  case me of
    Just e -> if e > b then p' else p
    _      -> p
  where
    p' = periodPrevious p
    me = periodEnd p'
periodPreviousIn _ p = periodPrevious p

-- | Enlarge a standard period to the next larger enclosing standard period, if there is one.
-- Eg, a day becomes the enclosing week.
-- A week becomes whichever month the week's thursday falls into.
-- A year becomes all (unlimited).
-- Growing an unlimited period, or a non-standard period (arbitrary dates) has no effect.
periodGrow :: Period -> Period
periodGrow (DayPeriod b) = WeekPeriod $ mondayBefore b
periodGrow (WeekPeriod b) = MonthPeriod y m
  where (y,m) = yearMonthContainingWeekStarting b
periodGrow (MonthPeriod y m) = QuarterPeriod y (quarterContainingMonth m)
periodGrow (QuarterPeriod y _) = YearPeriod y
periodGrow (YearPeriod _) = PeriodAll
periodGrow p = p

-- | Shrink a period to the next smaller standard period inside it,
-- choosing the subperiod which contains today's date if possible,
-- otherwise the first subperiod. It goes like this:
-- unbounded periods and nonstandard periods (between two arbitrary dates) ->
-- current year ->
-- current quarter if it's in selected year, otherwise first quarter of selected year ->
-- current month if it's in selected quarter, otherwise first month of selected quarter ->
-- current week if it's in selected month, otherwise first week of selected month ->
-- today if it's in selected week, otherwise first day of selected week,
--  unless that's in previous month, in which case first day of month containing selected week.
-- Shrinking a day has no effect.
periodShrink :: Day -> Period -> Period
periodShrink _     p@(DayPeriod _) = p
periodShrink today (WeekPeriod b)
  | today >= b && diffDays today b < 7 = DayPeriod today
  | m /= weekmonth                     = DayPeriod $ fromGregorian weekyear weekmonth 1
  | otherwise                          = DayPeriod b
  where
    (_,m,_) = toGregorian b
    (weekyear,weekmonth) = yearMonthContainingWeekStarting b
periodShrink today (MonthPeriod y m)
  | (y',m') == (y,m) = WeekPeriod $ mondayBefore today
  | otherwise        = WeekPeriod $ startOfFirstWeekInMonth y m
  where (y',m',_) = toGregorian today
periodShrink today (QuarterPeriod y q)
  | quarterContainingMonth thismonth == q = MonthPeriod y thismonth
  | otherwise                             = MonthPeriod y (firstMonthOfQuarter q)
  where (_,thismonth,_) = toGregorian today
periodShrink today (YearPeriod y)
  | y == thisyear = QuarterPeriod y thisquarter
  | otherwise     = QuarterPeriod y 1
  where
    (thisyear,thismonth,_) = toGregorian today
    thisquarter = quarterContainingMonth thismonth
periodShrink today _ = YearPeriod y
  where (y,_,_) = toGregorian today

mondayBefore d = addDays (fromIntegral (1 - wd)) d
  where
    (_,_,wd) = toWeekDate d

yearMonthContainingWeekStarting weekstart = (y,m)
  where
    thu = addDays 3 weekstart
    (y,yd) = toOrdinalDate thu
    (m,_) = dayOfYearToMonthAndDay (isLeapYear y) yd

quarterContainingMonth m = (m-1) `div` 3 + 1

firstMonthOfQuarter q = (q-1)*3 + 1

startOfFirstWeekInMonth y m
  | monthstartday <= 4 = mon
  | otherwise          = addDays 7 mon  -- month starts with a fri/sat/sun
  where
    monthstart = fromGregorian y m 1
    mon = mondayBefore monthstart
    (_,_,monthstartday) = toWeekDate monthstart

