{-# LANGUAGE CPP #-}
{-# LANGUAGE NoMonoLocalBinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PackageImports #-}
{-|

Date parsing and utilities for hledger.

For date and time values, we use the standard Day and UTCTime types.

A 'SmartDate' is a date which may be partially-specified or relative.
Eg 2008\/12\/31, but also 2008\/12, 12\/31, tomorrow, last week, next year.
We represent these as a triple of strings like (\"2008\",\"12\",\"\"),
(\"\",\"\",\"tomorrow\"), (\"\",\"last\",\"week\").

A 'DateSpan' is the span of time between two specific calendar dates, or
an open-ended span where one or both dates are unspecified. (A date span
with both ends unspecified matches all dates.)

An 'Interval' is ledger's \"reporting interval\" - weekly, monthly,
quarterly, etc.

'Period' will probably replace DateSpan in due course.

-}

-- XXX fromGregorian silently clips bad dates, use fromGregorianValid instead ?

module Hledger.Data.Dates (
  -- * Misc date handling utilities
  getCurrentDay,
  getCurrentMonth,
  getCurrentYear,
  nulldate,
  spanContainsDate,
  periodContainsDate,
  parsedateM,
  parsedate,
  showDate,
  showDateSpan,
  showDateSpanMonthAbbrev,
  elapsedSeconds,
  prevday,
  periodexprp,
  parsePeriodExpr,
  parsePeriodExpr',
  nulldatespan,
  emptydatespan,
  failIfInvalidYear,
  failIfInvalidMonth,
  failIfInvalidDay,
  datesepchar,
  datesepchars,
  isDateSepChar,
  spanStart,
  spanEnd,
  spansSpan,
  spanIntersect,
  spansIntersect,
  spanDefaultsFrom,
  spanUnion,
  spansUnion,
  smartdate,
  splitSpan,
  fixSmartDate,
  fixSmartDateStr,
  fixSmartDateStrEither,
  fixSmartDateStrEither',
  daysInSpan,
  maybePeriod,
  mkdatespan,
)
where

import Prelude ()
import "base-compat-batteries" Prelude.Compat hiding (fail)
import qualified "base-compat-batteries" Control.Monad.Fail.Compat as Fail (MonadFail, fail)
import Control.Applicative.Permutations
import Control.Monad (unless)
import "base-compat-batteries" Data.List.Compat
import Data.Default
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
#if MIN_VERSION_time(1,5,0)
import Data.Time.Format hiding (months)
#else
import Data.Time.Format
import System.Locale (TimeLocale, defaultTimeLocale)
#endif
import Data.Time.Calendar
import Data.Time.Calendar.OrdinalDate
import Data.Time.Clock
import Data.Time.LocalTime
import Safe (headMay, lastMay, readMay)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Custom
import Text.Printf

import Hledger.Data.Types
import Hledger.Data.Period
import Hledger.Utils


-- Help ppShow parse and line-wrap DateSpans better in debug output.
instance Show DateSpan where
    show s = "DateSpan " ++ showDateSpan s
    -- show s = "DateSpan \"" ++ showDateSpan s ++ "\"" -- quotes to help pretty-show

showDate :: Day -> String
showDate = formatTime defaultTimeLocale "%0C%y/%m/%d"

-- | Render a datespan as a display string, abbreviating into a
-- compact form if possible.
showDateSpan :: DateSpan -> String
showDateSpan = showPeriod . dateSpanAsPeriod

-- | Like showDateSpan, but show month spans as just the abbreviated month name
-- in the current locale.
showDateSpanMonthAbbrev :: DateSpan -> String
showDateSpanMonthAbbrev = showPeriodMonthAbbrev . dateSpanAsPeriod

-- | Get the current local date.
getCurrentDay :: IO Day
getCurrentDay = localDay . zonedTimeToLocalTime <$> getZonedTime

-- | Get the current local month number.
getCurrentMonth :: IO Int
getCurrentMonth = second3 . toGregorian <$> getCurrentDay

-- | Get the current local year.
getCurrentYear :: IO Integer
getCurrentYear = first3 . toGregorian <$> getCurrentDay

elapsedSeconds :: Fractional a => UTCTime -> UTCTime -> a
elapsedSeconds t1 = realToFrac . diffUTCTime t1

spanStart :: DateSpan -> Maybe Day
spanStart (DateSpan d _) = d

spanEnd :: DateSpan -> Maybe Day
spanEnd (DateSpan _ d) = d

-- might be useful later: http://en.wikipedia.org/wiki/Allen%27s_interval_algebra

-- | Get overall span enclosing multiple sequentially ordered spans.
spansSpan :: [DateSpan] -> DateSpan
spansSpan spans = DateSpan (maybe Nothing spanStart $ headMay spans) (maybe Nothing spanEnd $ lastMay spans)

-- | Split a DateSpan into consecutive whole spans of the specified interval
-- which fully encompass the original span (and a little more when necessary).
-- If no interval is specified, the original span is returned.
-- If the original span is the null date span, ie unbounded, the null date span is returned.
-- If the original span is empty, eg if the end date is <= the start date, no spans are returned.
--
--
-- ==== Examples:
-- >>> let t i d1 d2 = splitSpan i $ mkdatespan d1 d2
-- >>> t NoInterval "2008/01/01" "2009/01/01"
-- [DateSpan 2008]
-- >>> t (Quarters 1) "2008/01/01" "2009/01/01"
-- [DateSpan 2008q1,DateSpan 2008q2,DateSpan 2008q3,DateSpan 2008q4]
-- >>> splitSpan (Quarters 1) nulldatespan
-- [DateSpan -]
-- >>> t (Days 1) "2008/01/01" "2008/01/01"  -- an empty datespan
-- []
-- >>> t (Quarters 1) "2008/01/01" "2008/01/01"
-- []
-- >>> t (Months 1) "2008/01/01" "2008/04/01"
-- [DateSpan 2008/01,DateSpan 2008/02,DateSpan 2008/03]
-- >>> t (Months 2) "2008/01/01" "2008/04/01"
-- [DateSpan 2008/01/01-2008/02/29,DateSpan 2008/03/01-2008/04/30]
-- >>> t (Weeks 1) "2008/01/01" "2008/01/15"
-- [DateSpan 2007/12/31w01,DateSpan 2008/01/07w02,DateSpan 2008/01/14w03]
-- >>> t (Weeks 2) "2008/01/01" "2008/01/15"
-- [DateSpan 2007/12/31-2008/01/13,DateSpan 2008/01/14-2008/01/27]
-- >>> t (DayOfMonth 2) "2008/01/01" "2008/04/01"
-- [DateSpan 2007/12/02-2008/01/01,DateSpan 2008/01/02-2008/02/01,DateSpan 2008/02/02-2008/03/01,DateSpan 2008/03/02-2008/04/01]
-- >>> t (WeekdayOfMonth 2 4) "2011/01/01" "2011/02/15"
-- [DateSpan 2010/12/09-2011/01/12,DateSpan 2011/01/13-2011/02/09,DateSpan 2011/02/10-2011/03/09]
-- >>> t (DayOfWeek 2) "2011/01/01" "2011/01/15"
-- [DateSpan 2010/12/28-2011/01/03,DateSpan 2011/01/04-2011/01/10,DateSpan 2011/01/11-2011/01/17]
-- >>> t (DayOfYear 11 29) "2011/10/01" "2011/10/15"
-- [DateSpan 2010/11/29-2011/11/28]
-- >>> t (DayOfYear 11 29) "2011/12/01" "2012/12/15"
-- [DateSpan 2011/11/29-2012/11/28,DateSpan 2012/11/29-2013/11/28]
--
splitSpan :: Interval -> DateSpan -> [DateSpan]
splitSpan _ (DateSpan Nothing Nothing) = [DateSpan Nothing Nothing]
splitSpan _ s | isEmptySpan s = []
splitSpan NoInterval     s = [s]
splitSpan (Days n)       s = splitspan startofday     (applyN n nextday)     s
splitSpan (Weeks n)      s = splitspan startofweek    (applyN n nextweek)    s
splitSpan (Months n)     s = splitspan startofmonth   (applyN n nextmonth)   s
splitSpan (Quarters n)   s = splitspan startofquarter (applyN n nextquarter) s
splitSpan (Years n)      s = splitspan startofyear    (applyN n nextyear)    s
splitSpan (DayOfMonth n) s = splitspan (nthdayofmonthcontaining n) (nthdayofmonth n . nextmonth) s
splitSpan (WeekdayOfMonth n wd) s = splitspan (nthweekdayofmonthcontaining n wd) (advancetonthweekday n wd . nextmonth) s
splitSpan (DayOfWeek n)  s = splitspan (nthdayofweekcontaining n)  (applyN (n-1) nextday . nextweek)  s
splitSpan (DayOfYear m n) s = splitspan (nthdayofyearcontaining m n) (applyN (n-1) nextday . applyN (m-1) nextmonth . nextyear) s
-- splitSpan (WeekOfYear n)    s = splitspan startofweek    (applyN n nextweek)    s
-- splitSpan (MonthOfYear n)   s = splitspan startofmonth   (applyN n nextmonth)   s
-- splitSpan (QuarterOfYear n) s = splitspan startofquarter (applyN n nextquarter) s

-- Split the given span using the provided helper functions:
-- start is applied to the span's start date to get the first sub-span's start date
-- next is applied to a sub-span's start date to get the next sub-span's start date
splitspan :: (Day -> Day) -> (Day -> Day) -> DateSpan -> [DateSpan]
splitspan _ _ (DateSpan Nothing Nothing) = []
splitspan start next (DateSpan Nothing (Just e)) = splitspan start next (DateSpan (Just $ start e) (Just $ next $ start e))
splitspan start next (DateSpan (Just s) Nothing) = splitspan start next (DateSpan (Just $ start s) (Just $ next $ start s))
splitspan start next span@(DateSpan (Just s) (Just e))
    | s == e = [span]
    | otherwise = splitspan' start next span
    where
      splitspan' start next (DateSpan (Just s) (Just e))
          | s >= e = []
          | otherwise = DateSpan (Just subs) (Just sube) : splitspan' start next (DateSpan (Just sube) (Just e))
          where subs = start s
                sube = next subs
      splitspan' _ _ _ = error' "won't happen, avoids warnings"

-- | Count the days in a DateSpan, or if it is open-ended return Nothing.
daysInSpan :: DateSpan -> Maybe Integer
daysInSpan (DateSpan (Just d1) (Just d2)) = Just $ diffDays d2 d1
daysInSpan _ = Nothing

-- | Is this an empty span, ie closed with the end date on or before the start date ?
isEmptySpan :: DateSpan -> Bool
isEmptySpan s = case daysInSpan s of
                  Just n  -> n < 1
                  Nothing -> False

-- | Does the span include the given date ?
spanContainsDate :: DateSpan -> Day -> Bool
spanContainsDate (DateSpan Nothing Nothing)   _ = True
spanContainsDate (DateSpan Nothing (Just e))  d = d < e
spanContainsDate (DateSpan (Just b) Nothing)  d = d >= b
spanContainsDate (DateSpan (Just b) (Just e)) d = d >= b && d < e

-- | Does the period include the given date ?
-- (Here to avoid import cycle).
periodContainsDate :: Period -> Day -> Bool
periodContainsDate p = spanContainsDate (periodAsDateSpan p)

-- | Calculate the intersection of a number of datespans.
spansIntersect [] = nulldatespan
spansIntersect [d] = d
spansIntersect (d:ds) = d `spanIntersect` (spansIntersect ds)

-- | Calculate the intersection of two datespans.
--
-- For non-intersecting spans, gives an empty span beginning on the second's start date:
-- >>> mkdatespan "2018-01-01" "2018-01-03" `spanIntersect` mkdatespan "2018-01-03" "2018-01-05"
-- DateSpan 2018/01/03-2018/01/02
spanIntersect (DateSpan b1 e1) (DateSpan b2 e2) = DateSpan b e
    where
      b = latest b1 b2
      e = earliest e1 e2

-- | Fill any unspecified dates in the first span with the dates from
-- the second one. Sort of a one-way spanIntersect.
spanDefaultsFrom (DateSpan a1 b1) (DateSpan a2 b2) = DateSpan a b
    where a = if isJust a1 then a1 else a2
          b = if isJust b1 then b1 else b2

-- | Calculate the union of a number of datespans.
spansUnion [] = nulldatespan
spansUnion [d] = d
spansUnion (d:ds) = d `spanUnion` (spansUnion ds)

-- | Calculate the union of two datespans.
spanUnion (DateSpan b1 e1) (DateSpan b2 e2) = DateSpan b e
    where
      b = earliest b1 b2
      e = latest e1 e2

latest d Nothing = d
latest Nothing d = d
latest (Just d1) (Just d2) = Just $ max d1 d2

earliest d Nothing = d
earliest Nothing d = d
earliest (Just d1) (Just d2) = Just $ min d1 d2

-- | Parse a period expression to an Interval and overall DateSpan using
-- the provided reference date, or return a parse error.
parsePeriodExpr
  :: Day -> Text -> Either (ParseErrorBundle Text CustomErr) (Interval, DateSpan)
parsePeriodExpr refdate s = parsewith (periodexprp refdate <* eof) (T.toLower s)

-- | Like parsePeriodExpr, but call error' on failure.
parsePeriodExpr' :: Day -> Text -> (Interval, DateSpan)
parsePeriodExpr' refdate s =
  either (error' . ("failed to parse:" ++) . customErrorBundlePretty) id $
  parsePeriodExpr refdate s

maybePeriod :: Day -> Text -> Maybe (Interval,DateSpan)
maybePeriod refdate = either (const Nothing) Just . parsePeriodExpr refdate

-- | Show a DateSpan as a human-readable pseudo-period-expression string.
-- dateSpanAsText :: DateSpan -> String
-- dateSpanAsText (DateSpan Nothing Nothing)   = "all"
-- dateSpanAsText (DateSpan Nothing (Just e))  = printf "to %s" (show e)
-- dateSpanAsText (DateSpan (Just b) Nothing)  = printf "from %s" (show b)
-- dateSpanAsText (DateSpan (Just b) (Just e)) = printf "%s to %s" (show b) (show e)

-- | Convert a single smart date string to a date span using the provided
-- reference date, or raise an error.
-- spanFromSmartDateString :: Day -> String -> DateSpan
-- spanFromSmartDateString refdate s = spanFromSmartDate refdate sdate
--     where
--       sdate = fromparse $ parsewith smartdateonly s

spanFromSmartDate :: Day -> SmartDate -> DateSpan
spanFromSmartDate refdate sdate = DateSpan (Just b) (Just e)
    where
      (ry,rm,_) = toGregorian refdate
      (b,e) = span sdate
      span :: SmartDate -> (Day,Day)
      span ("","","today")       = (refdate, nextday refdate)
      span ("","this","day")     = (refdate, nextday refdate)
      span ("","","yesterday")   = (prevday refdate, refdate)
      span ("","last","day")     = (prevday refdate, refdate)
      span ("","","tomorrow")    = (nextday refdate, addDays 2 refdate)
      span ("","next","day")     = (nextday refdate, addDays 2 refdate)
      span ("","last","week")    = (prevweek refdate, thisweek refdate)
      span ("","this","week")    = (thisweek refdate, nextweek refdate)
      span ("","next","week")    = (nextweek refdate, startofweek $ addDays 14 refdate)
      span ("","last","month")   = (prevmonth refdate, thismonth refdate)
      span ("","this","month")   = (thismonth refdate, nextmonth refdate)
      span ("","next","month")   = (nextmonth refdate, startofmonth $ addGregorianMonthsClip 2 refdate)
      span ("","last","quarter") = (prevquarter refdate, thisquarter refdate)
      span ("","this","quarter") = (thisquarter refdate, nextquarter refdate)
      span ("","next","quarter") = (nextquarter refdate, startofquarter $ addGregorianMonthsClip 6 refdate)
      span ("","last","year")    = (prevyear refdate, thisyear refdate)
      span ("","this","year")    = (thisyear refdate, nextyear refdate)
      span ("","next","year")    = (nextyear refdate, startofyear $ addGregorianYearsClip 2 refdate)
      span ("","",d)             = (day, nextday day) where day = fromGregorian ry rm (read d)
      span ("",m,"")             = (startofmonth day, nextmonth day) where day = fromGregorian ry (read m) 1
      span ("",m,d)              = (day, nextday day) where day = fromGregorian ry (read m) (read d)
      span (y,"","")             = (startofyear day, nextyear day) where day = fromGregorian (read y) 1 1
      span (y,m,"")              = (startofmonth day, nextmonth day) where day = fromGregorian (read y) (read m) 1
      span (y,m,d)               = (day, nextday day) where day = fromGregorian (read y) (read m) (read d)

-- showDay :: Day -> String
-- showDay day = printf "%04d/%02d/%02d" y m d where (y,m,d) = toGregorian day

-- | Convert a smart date string to an explicit yyyy\/mm\/dd string using
-- the provided reference date, or raise an error.
fixSmartDateStr :: Day -> Text -> String
fixSmartDateStr d s =
  either (error' . printf "could not parse date %s %s" (show s) . show) id $
  (fixSmartDateStrEither d s :: Either (ParseErrorBundle Text CustomErr) String)

-- | A safe version of fixSmartDateStr.
fixSmartDateStrEither :: Day -> Text -> Either (ParseErrorBundle Text CustomErr) String
fixSmartDateStrEither d = fmap showDate . fixSmartDateStrEither' d

fixSmartDateStrEither'
  :: Day -> Text -> Either (ParseErrorBundle Text CustomErr) Day
fixSmartDateStrEither' d s = case parsewith smartdateonly (T.toLower s) of
                               Right sd -> Right $ fixSmartDate d sd
                               Left e -> Left e

-- | Convert a SmartDate to an absolute date using the provided reference date.
--
-- ==== Examples:
-- >>> :set -XOverloadedStrings
-- >>> let t = fixSmartDateStr (parsedate "2008/11/26")
-- >>> t "0000-01-01"
-- "0000/01/01"
-- >>> t "1999-12-02"
-- "1999/12/02"
-- >>> t "1999.12.02"
-- "1999/12/02"
-- >>> t "1999/3/2"
-- "1999/03/02"
-- >>> t "19990302"
-- "1999/03/02"
-- >>> t "2008/2"
-- "2008/02/01"
-- >>> t "0020/2"
-- "0020/02/01"
-- >>> t "1000"
-- "1000/01/01"
-- >>> t "4/2"
-- "2008/04/02"
-- >>> t "2"
-- "2008/11/02"
-- >>> t "January"
-- "2008/01/01"
-- >>> t "feb"
-- "2008/02/01"
-- >>> t "today"
-- "2008/11/26"
-- >>> t "yesterday"
-- "2008/11/25"
-- >>> t "tomorrow"
-- "2008/11/27"
-- >>> t "this day"
-- "2008/11/26"
-- >>> t "last day"
-- "2008/11/25"
-- >>> t "next day"
-- "2008/11/27"
-- >>> t "this week"  -- last monday
-- "2008/11/24"
-- >>> t "last week"  -- previous monday
-- "2008/11/17"
-- >>> t "next week"  -- next monday
-- "2008/12/01"
-- >>> t "this month"
-- "2008/11/01"
-- >>> t "last month"
-- "2008/10/01"
-- >>> t "next month"
-- "2008/12/01"
-- >>> t "this quarter"
-- "2008/10/01"
-- >>> t "last quarter"
-- "2008/07/01"
-- >>> t "next quarter"
-- "2009/01/01"
-- >>> t "this year"
-- "2008/01/01"
-- >>> t "last year"
-- "2007/01/01"
-- >>> t "next year"
-- "2009/01/01"
--
-- t "last wed"
-- "2008/11/19"
-- t "next friday"
-- "2008/11/28"
-- t "next january"
-- "2009/01/01"
--
fixSmartDate :: Day -> SmartDate -> Day
fixSmartDate refdate = fix
  where
    fix :: SmartDate -> Day
    fix ("", "", "today") = fromGregorian ry rm rd
    fix ("", "this", "day") = fromGregorian ry rm rd
    fix ("", "", "yesterday") = prevday refdate
    fix ("", "last", "day") = prevday refdate
    fix ("", "", "tomorrow") = nextday refdate
    fix ("", "next", "day") = nextday refdate
    fix ("", "last", "week") = prevweek refdate
    fix ("", "this", "week") = thisweek refdate
    fix ("", "next", "week") = nextweek refdate
    fix ("", "last", "month") = prevmonth refdate
    fix ("", "this", "month") = thismonth refdate
    fix ("", "next", "month") = nextmonth refdate
    fix ("", "last", "quarter") = prevquarter refdate
    fix ("", "this", "quarter") = thisquarter refdate
    fix ("", "next", "quarter") = nextquarter refdate
    fix ("", "last", "year") = prevyear refdate
    fix ("", "this", "year") = thisyear refdate
    fix ("", "next", "year") = nextyear refdate
    fix ("", "", d) = fromGregorian ry rm (read d)
    fix ("", m, "") = fromGregorian ry (read m) 1
    fix ("", m, d) = fromGregorian ry (read m) (read d)
    fix (y, "", "") = fromGregorian (read y) 1 1
    fix (y, m, "") = fromGregorian (read y) (read m) 1
    fix (y, m, d) = fromGregorian (read y) (read m) (read d)
    (ry, rm, rd) = toGregorian refdate

prevday :: Day -> Day
prevday = addDays (-1)
nextday = addDays 1
startofday = id

thisweek = startofweek
prevweek = startofweek . addDays (-7)
nextweek = startofweek . addDays 7
startofweek day = fromMondayStartWeek y w 1
    where
      (y,_,_) = toGregorian day
      (w,_) = mondayStartWeek day

thismonth = startofmonth
prevmonth = startofmonth . addGregorianMonthsClip (-1)
nextmonth = startofmonth . addGregorianMonthsClip 1
startofmonth day = fromGregorian y m 1 where (y,m,_) = toGregorian day
nthdayofmonth d day = fromGregorian y m d where (y,m,_) = toGregorian day

thisquarter = startofquarter
prevquarter = startofquarter . addGregorianMonthsClip (-3)
nextquarter = startofquarter . addGregorianMonthsClip 3
startofquarter day = fromGregorian y (firstmonthofquarter m) 1
    where
      (y,m,_) = toGregorian day
      firstmonthofquarter m = ((m-1) `div` 3) * 3 + 1

thisyear = startofyear
prevyear = startofyear . addGregorianYearsClip (-1)
nextyear = startofyear . addGregorianYearsClip 1
startofyear day = fromGregorian y 1 1 where (y,_,_) = toGregorian day

-- | For given date d find year-long interval that starts on given
-- MM/DD of year and covers it.
-- The given MM and DD should be basically valid (1-12 & 1-31),
-- or an error is raised.
--
-- Examples: lets take 2017-11-22. Year-long intervals covering it that
-- starts before Nov 22 will start in 2017. However
-- intervals that start after Nov 23rd should start in 2016:
-- >>> let wed22nd = parsedate "2017-11-22"
-- >>> nthdayofyearcontaining 11 21 wed22nd
-- 2017-11-21
-- >>> nthdayofyearcontaining 11 22 wed22nd
-- 2017-11-22
-- >>> nthdayofyearcontaining 11 23 wed22nd
-- 2016-11-23
-- >>> nthdayofyearcontaining 12 02 wed22nd
-- 2016-12-02
-- >>> nthdayofyearcontaining 12 31 wed22nd
-- 2016-12-31
-- >>> nthdayofyearcontaining 1 1 wed22nd
-- 2017-01-01
nthdayofyearcontaining :: Month -> MonthDay -> Day -> Day
nthdayofyearcontaining m md date
  | not (validMonth $ show m)  = error' $ "nthdayofyearcontaining: invalid month "++show m
  | not (validDay   $ show md) = error' $ "nthdayofyearcontaining: invalid day "  ++show md
  | mmddOfSameYear <= date = mmddOfSameYear
  | otherwise = mmddOfPrevYear
  where mmddOfSameYear = addDays (fromIntegral md-1) $ applyN (m-1) nextmonth s
        mmddOfPrevYear = addDays (fromIntegral md-1) $ applyN (m-1) nextmonth $ prevyear s
        s = startofyear date

-- | For given date d find month-long interval that starts on nth day of month
-- and covers it.
-- The given day of month should be basically valid (1-31), or an error is raised.
--
-- Examples: lets take 2017-11-22. Month-long intervals covering it that
-- start on 1st-22nd of month will start in Nov. However
-- intervals that start on 23rd-30th of month should start in Oct:
-- >>> let wed22nd = parsedate "2017-11-22"
-- >>> nthdayofmonthcontaining 1 wed22nd
-- 2017-11-01
-- >>> nthdayofmonthcontaining 12 wed22nd
-- 2017-11-12
-- >>> nthdayofmonthcontaining 22 wed22nd
-- 2017-11-22
-- >>> nthdayofmonthcontaining 23 wed22nd
-- 2017-10-23
-- >>> nthdayofmonthcontaining 30 wed22nd
-- 2017-10-30
nthdayofmonthcontaining :: MonthDay -> Day -> Day
nthdayofmonthcontaining md date
  | not (validDay $ show md) = error' $ "nthdayofmonthcontaining: invalid day "  ++show md
  | nthOfSameMonth <= date = nthOfSameMonth
  | otherwise = nthOfPrevMonth
  where nthOfSameMonth = nthdayofmonth md s
        nthOfPrevMonth = nthdayofmonth md $ prevmonth s
        s = startofmonth date

-- | For given date d find week-long interval that starts on nth day of week
-- and covers it.
--
-- Examples: 2017-11-22 is Wed. Week-long intervals that cover it and
-- start on Mon, Tue or Wed will start in the same week. However
-- intervals that start on Thu or Fri should start in prev week:
-- >>> let wed22nd = parsedate "2017-11-22"
-- >>> nthdayofweekcontaining 1 wed22nd
-- 2017-11-20
-- >>> nthdayofweekcontaining 2 wed22nd
-- 2017-11-21
-- >>> nthdayofweekcontaining 3 wed22nd
-- 2017-11-22
-- >>> nthdayofweekcontaining 4 wed22nd
-- 2017-11-16
-- >>> nthdayofweekcontaining 5 wed22nd
-- 2017-11-17
nthdayofweekcontaining :: WeekDay -> Day -> Day
nthdayofweekcontaining n d | nthOfSameWeek <= d = nthOfSameWeek
                           | otherwise = nthOfPrevWeek
    where nthOfSameWeek = addDays (fromIntegral n-1) s
          nthOfPrevWeek = addDays (fromIntegral n-1) $ prevweek s
          s = startofweek d

-- | For given date d find month-long interval that starts on nth weekday of month
-- and covers it.
--
-- Examples: 2017-11-22 is 3rd Wed of Nov. Month-long intervals that cover it and
-- start on 1st-4th Wed will start in Nov. However
-- intervals that start on 4th Thu or Fri or later should start in Oct:
-- >>> let wed22nd = parsedate "2017-11-22"
-- >>> nthweekdayofmonthcontaining 1 3 wed22nd
-- 2017-11-01
-- >>> nthweekdayofmonthcontaining 3 2 wed22nd
-- 2017-11-21
-- >>> nthweekdayofmonthcontaining 4 3 wed22nd
-- 2017-11-22
-- >>> nthweekdayofmonthcontaining 4 4 wed22nd
-- 2017-10-26
-- >>> nthweekdayofmonthcontaining 4 5 wed22nd
-- 2017-10-27
nthweekdayofmonthcontaining :: Int -> WeekDay -> Day -> Day
nthweekdayofmonthcontaining n wd d | nthWeekdaySameMonth <= d  = nthWeekdaySameMonth
                                   | otherwise = nthWeekdayPrevMonth
    where nthWeekdaySameMonth = advancetonthweekday n wd $ startofmonth d
          nthWeekdayPrevMonth = advancetonthweekday n wd $ prevmonth d

-- | Advance to nth weekday wd after given start day s
advancetonthweekday :: Int -> WeekDay -> Day -> Day
advancetonthweekday n wd s =
  maybe err (addWeeks (n-1)) $ firstMatch (>=s) $ iterate (addWeeks 1) $ firstweekday s
  where
    err = error' "advancetonthweekday: should not happen"
    addWeeks k = addDays (7 * fromIntegral k)
    firstMatch p = headMay . dropWhile (not . p)
    firstweekday = addDays (fromIntegral wd-1) . startofweek

----------------------------------------------------------------------
-- parsing

-- -- | Parse a couple of date-time string formats to a time type.
-- parsedatetimeM :: String -> Maybe LocalTime
-- parsedatetimeM s = firstJust [
--     parseTime defaultTimeLocale "%Y/%m/%d %H:%M:%S" s,
--     parseTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" s
--     ]

parsetime :: ParseTime t => TimeLocale -> String -> String -> Maybe t
parsetime =
#if MIN_VERSION_time(1,5,0)
     parseTimeM True
#else
     parseTime
#endif


-- | Parse a couple of date string formats to a time type.
parsedateM :: String -> Maybe Day
parsedateM s = firstJust [
     parsetime defaultTimeLocale "%Y/%m/%d" s,
     parsetime defaultTimeLocale "%Y-%m-%d" s
     ]


-- -- | Parse a date-time string to a time type, or raise an error.
-- parsedatetime :: String -> LocalTime
-- parsedatetime s = fromMaybe (error' $ "could not parse timestamp \"" ++ s ++ "\"")
--                             (parsedatetimeM s)

-- | Parse a YYYY-MM-DD or YYYY/MM/DD date string to a Day, or raise an error. For testing/debugging.
--
-- >>> parsedate "2008/02/03"
-- 2008-02-03
parsedate :: String -> Day
parsedate s =  fromMaybe (error' $ "could not parse date \"" ++ s ++ "\"")
                         (parsedateM s)
-- doctests I haven't been able to make compatible with both GHC 7 and 8
-- -- >>> parsedate "2008/02/03/"
-- -- *** Exception: could not parse date "2008/02/03/"
-- #if MIN_VERSION_base(4,9,0)
-- -- ...
-- #endif
-- #if MIN_VERSION_time(1,6,0)
-- -- >>> parsedate "2008/02/30"  -- with time >= 1.6, invalid dates are rejected
-- -- *** Exception: could not parse date "2008/02/30"
-- #if MIN_VERSION_base(4,9,0)
-- -- ...
-- #endif
-- #else
-- -- >>> parsedate "2008/02/30"  -- with time < 1.6, they are silently adjusted
-- -- 2008-02-29
-- #endif

{-|
Parse a date in any of the formats allowed in Ledger's period expressions, and some others.
Assumes any text in the parse stream has been lowercased.
Returns a SmartDate, to be converted to a full date later (see fixSmartDate).

Examples:

> 2004                                        (start of year, which must have 4+ digits)
> 2004/10                                     (start of month, which must be 1-12)
> 2004/10/1                                   (exact date, day must be 1-31)
> 10/1                                        (month and day in current year)
> 21                                          (day in current month)
> october, oct                                (start of month in current year)
> yesterday, today, tomorrow                  (-1, 0, 1 days from today)
> last/this/next day/week/month/quarter/year  (-1, 0, 1 periods from the current period)
> 20181201                                    (8 digit YYYYMMDD with valid year month and day)
> 201812                                      (6 digit YYYYMM with valid year and month)

Note malformed digit sequences might give surprising results:

> 201813                                      (6 digits with an invalid month is parsed as start of 6-digit year)
> 20181301                                    (8 digits with an invalid month is parsed as start of 8-digit year)
> 20181232                                    (8 digits with an invalid day gives an error)
> 201801012                                   (9+ digits beginning with a valid YYYYMMDD gives an error)

Eg:

YYYYMMDD is parsed as year-month-date if those parts are valid
(>=4 digits, 1-12, and 1-31 respectively):
>>> parsewith (smartdate <* eof) "20181201"
Right ("2018","12","01")

YYYYMM is parsed as year-month-01 if year and month are valid:
>>> parsewith (smartdate <* eof) "201804"
Right ("2018","04","01")

With an invalid month, it's parsed as a year:
>>> parsewith (smartdate <* eof) "201813"
Right ("201813","","")

A 9+ digit number beginning with valid YYYYMMDD gives an error:
>>> parsewith (smartdate <* eof) "201801012"
Left (...)

Big numbers not beginning with a valid YYYYMMDD are parsed as a year:
>>> parsewith (smartdate <* eof) "201813012"
Right ("201813012","","")

-}
smartdate :: TextParser m SmartDate
smartdate = do
  -- XXX maybe obscures date errors ? see ledgerdate
  (y,m,d) <- choice' [yyyymmdd, yyyymm, ymd, ym, md, y, d, month, mon, today, yesterday, tomorrow, lastthisnextthing]
  return (y,m,d)

-- | Like smartdate, but there must be nothing other than whitespace after the date.
smartdateonly :: TextParser m SmartDate
smartdateonly = do
  d <- smartdate
  skipMany spacenonewline
  eof
  return d

datesepchars :: String
datesepchars = "/-."

datesepchar :: TextParser m Char
datesepchar = satisfy isDateSepChar

isDateSepChar :: Char -> Bool
isDateSepChar c = c == '/' || c == '-' || c == '.'

validYear, validMonth, validDay :: String -> Bool
validYear s = length s >= 4 && isJust (readMay s :: Maybe Year)
validMonth s = maybe False (\n -> n>=1 && n<=12) $ readMay s
validDay s = maybe False (\n -> n>=1 && n<=31) $ readMay s

failIfInvalidYear, failIfInvalidMonth, failIfInvalidDay :: (Fail.MonadFail m) => String -> m ()
failIfInvalidYear s  = unless (validYear s)  $ Fail.fail $ "bad year number: " ++ s
failIfInvalidMonth s = unless (validMonth s) $ Fail.fail $ "bad month number: " ++ s
failIfInvalidDay s   = unless (validDay s)   $ Fail.fail $ "bad day number: " ++ s

yyyymmdd :: TextParser m SmartDate
yyyymmdd = do
  y <- count 4 digitChar
  m <- count 2 digitChar
  failIfInvalidMonth m
  d <- count 2 digitChar
  failIfInvalidDay d
  return (y,m,d)

yyyymm :: TextParser m SmartDate
yyyymm = do
  y <- count 4 digitChar
  m <- count 2 digitChar
  failIfInvalidMonth m
  return (y,m,"01")

ymd :: TextParser m SmartDate
ymd = do
  y <- some digitChar
  failIfInvalidYear y
  sep <- datesepchar
  m <- some digitChar
  failIfInvalidMonth m
  char sep
  d <- some digitChar
  failIfInvalidDay d
  return $ (y,m,d)

ym :: TextParser m SmartDate
ym = do
  y <- some digitChar
  failIfInvalidYear y
  datesepchar
  m <- some digitChar
  failIfInvalidMonth m
  return (y,m,"")

y :: TextParser m SmartDate
y = do
  y <- some digitChar
  failIfInvalidYear y
  return (y,"","")

d :: TextParser m SmartDate
d = do
  d <- some digitChar
  failIfInvalidDay d
  return ("","",d)

md :: TextParser m SmartDate
md = do
  m <- some digitChar
  failIfInvalidMonth m
  datesepchar
  d <- some digitChar
  failIfInvalidDay d
  return ("",m,d)

-- These are compared case insensitively, and should all be kept lower case.
months         = ["january","february","march","april","may","june",
                  "july","august","september","october","november","december"]
monthabbrevs   = ["jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec"]
weekdays       = ["monday","tuesday","wednesday","thursday","friday","saturday","sunday"]
weekdayabbrevs = ["mon","tue","wed","thu","fri","sat","sun"]

-- | Convert a case insensitive english month name to a month number.
monthIndex name = maybe 0 (+1) $ T.toLower name `elemIndex` months

-- | Convert a case insensitive english three-letter month abbreviation to a month number.
monIndex   name = maybe 0 (+1) $ T.toLower name `elemIndex` monthabbrevs

month :: TextParser m SmartDate
month = do
  m <- choice $ map (try . string') months
  let i = monthIndex m
  return ("",show i,"")

mon :: TextParser m SmartDate
mon = do
  m <- choice $ map (try . string') monthabbrevs
  let i = monIndex m
  return ("",show i,"")

weekday :: TextParser m Int
weekday = do
  wday <- T.toLower <$> (choice . map string' $ weekdays ++ weekdayabbrevs)
  case catMaybes $ [wday `elemIndex` weekdays, wday `elemIndex` weekdayabbrevs] of
    (i:_) -> return (i+1)
    []    -> Fail.fail $ "weekday: should not happen: attempted to find " <>
                         show wday <> " in " <> show (weekdays ++ weekdayabbrevs)

today,yesterday,tomorrow :: TextParser m SmartDate
today     = string' "today"     >> return ("","","today")
yesterday = string' "yesterday" >> return ("","","yesterday")
tomorrow  = string' "tomorrow"  >> return ("","","tomorrow")

lastthisnextthing :: TextParser m SmartDate
lastthisnextthing = do
  r <- choice $ map string' [
        "last"
       ,"this"
       ,"next"
      ]
  skipMany spacenonewline  -- make the space optional for easier scripting
  p <- choice $ map string' [
        "day"
       ,"week"
       ,"month"
       ,"quarter"
       ,"year"
      ]
-- XXX support these in fixSmartDate
--       ++ (map string' $ months ++ monthabbrevs ++ weekdays ++ weekdayabbrevs)

  return ("", T.unpack r, T.unpack p)

-- | Parse a period expression, specifying a date span and optionally
-- a reporting interval. Requires a reference "today" date for
-- resolving any relative start/end dates (only; it is not needed for
-- parsing the reporting interval).
--
-- >>> let p = parsePeriodExpr (parsedate "2008/11/26")
-- >>> p "from Aug to Oct"
-- Right (NoInterval,DateSpan 2008/08/01-2008/09/30)
-- >>> p "aug to oct"
-- Right (NoInterval,DateSpan 2008/08/01-2008/09/30)
-- >>> p "every 3 days in Aug"
-- Right (Days 3,DateSpan 2008/08)
-- >>> p "daily from aug"
-- Right (Days 1,DateSpan 2008/08/01-)
-- >>> p "every week to 2009"
-- Right (Weeks 1,DateSpan -2008/12/31)
-- >>> p "every 2nd day of month"
-- Right (DayOfMonth 2,DateSpan -)
-- >>> p "every 2nd day"
-- Right (DayOfMonth 2,DateSpan -)
-- >>> p "every 2nd day 2009-"
-- Right (DayOfMonth 2,DateSpan 2009/01/01-)
-- >>> p "every 29th Nov"
-- Right (DayOfYear 11 29,DateSpan -)
-- >>> p "every 29th nov -2009"
-- Right (DayOfYear 11 29,DateSpan -2008/12/31)
-- >>> p "every nov 29th"
-- Right (DayOfYear 11 29,DateSpan -)
-- >>> p "every Nov 29th 2009-"
-- Right (DayOfYear 11 29,DateSpan 2009/01/01-)
-- >>> p "every 11/29 from 2009"
-- Right (DayOfYear 11 29,DateSpan 2009/01/01-)
-- >>> p "every 2nd Thursday of month to 2009"
-- Right (WeekdayOfMonth 2 4,DateSpan -2008/12/31)
-- >>> p "every 1st monday of month to 2009"
-- Right (WeekdayOfMonth 1 1,DateSpan -2008/12/31)
-- >>> p "every tue"
-- Right (DayOfWeek 2,DateSpan -)
-- >>> p "every 2nd day of week"
-- Right (DayOfWeek 2,DateSpan -)
-- >>> p "every 2nd day of month"
-- Right (DayOfMonth 2,DateSpan -)
-- >>> p "every 2nd day"
-- Right (DayOfMonth 2,DateSpan -)
-- >>> p "every 2nd day 2009-"
-- Right (DayOfMonth 2,DateSpan 2009/01/01-)
-- >>> p "every 2nd day of month 2009-"
-- Right (DayOfMonth 2,DateSpan 2009/01/01-)
periodexprp :: Day -> TextParser m (Interval, DateSpan)
periodexprp rdate = do
  skipMany spacenonewline
  choice $ map try [
                    intervalanddateperiodexprp rdate,
                    (,) NoInterval <$> periodexprdatespanp rdate
                   ]

-- Parse a reporting interval and a date span.
intervalanddateperiodexprp :: Day -> TextParser m (Interval, DateSpan)
intervalanddateperiodexprp rdate = do
  i <- reportingintervalp
  s <- option def . try $ do
      skipMany spacenonewline
      periodexprdatespanp rdate
  return (i,s)

-- Parse a reporting interval.
reportingintervalp :: TextParser m Interval
reportingintervalp = choice' [
                       tryinterval "day"     "daily"     Days,
                       tryinterval "week"    "weekly"    Weeks,
                       tryinterval "month"   "monthly"   Months,
                       tryinterval "quarter" "quarterly" Quarters,
                       tryinterval "year"    "yearly"    Years,
                       do string' "biweekly"
                          return $ Weeks 2,
                       do string' "bimonthly"
                          return $ Months 2,
                       do string' "every"
                          skipMany spacenonewline
                          n <- nth
                          skipMany spacenonewline
                          string' "day"
                          of_ "week"
                          return $ DayOfWeek n,
                       do string' "every"
                          skipMany spacenonewline
                          DayOfWeek <$> weekday,
                       do string' "every"
                          skipMany spacenonewline
                          n <- nth
                          skipMany spacenonewline
                          string' "day"
                          optOf_ "month"
                          return $ DayOfMonth n,
                       do string' "every"
                          let mnth = choice' [month, mon] >>= \(_,m,_) -> return (read m)
                          d_o_y <- runPermutation $
                            DayOfYear <$> toPermutation (try (skipMany spacenonewline *> mnth))
                                      <*> toPermutation (try (skipMany spacenonewline *> nth))
                          optOf_ "year"
                          return d_o_y,
                       do string' "every"
                          skipMany spacenonewline
                          ("",m,d) <- md
                          optOf_ "year"
                          return $ DayOfYear (read m) (read d),
                       do string' "every"
                          skipMany spacenonewline
                          n <- nth
                          skipMany spacenonewline
                          wd <- weekday
                          optOf_ "month"
                          return $ WeekdayOfMonth n wd
                    ]
    where
      of_ period = do
        skipMany spacenonewline
        string' "of"
        skipMany spacenonewline
        string' period

      optOf_ period = optional $ try $ of_ period

      nth = do n <- some digitChar
               choice' $ map string' ["st","nd","rd","th"]
               return $ read n

      -- Parse any of several variants of a basic interval, eg "daily", "every day", "every N days".
      tryinterval :: String -> String -> (Int -> Interval) -> TextParser m Interval
      tryinterval singular compact intcons =
        choice' [
          do string' compact'
             return $ intcons 1,
          do string' "every"
             skipMany spacenonewline
             string' singular'
             return $ intcons 1,
          do string' "every"
             skipMany spacenonewline
             n <- read <$> some digitChar
             skipMany spacenonewline
             string' plural'
             return $ intcons n
          ]
        where
          compact'  = T.pack compact
          singular' = T.pack singular
          plural'   = T.pack $ singular ++ "s"

periodexprdatespanp :: Day -> TextParser m DateSpan
periodexprdatespanp rdate = choice $ map try [
                            doubledatespanp rdate,
                            fromdatespanp rdate,
                            todatespanp rdate,
                            justdatespanp rdate
                           ]

-- |
-- -- >>> parsewith (doubledatespan (parsedate "2018/01/01") <* eof) "20180101-201804"
-- Right DateSpan 2018/01/01-2018/04/01
doubledatespanp :: Day -> TextParser m DateSpan
doubledatespanp rdate = do
  optional (string' "from" >> skipMany spacenonewline)
  b <- smartdate
  skipMany spacenonewline
  optional (choice [string' "to", string' "-"] >> skipMany spacenonewline)
  DateSpan (Just $ fixSmartDate rdate b) . Just . fixSmartDate rdate <$> smartdate

fromdatespanp :: Day -> TextParser m DateSpan
fromdatespanp rdate = do
  b <- choice [
    do
      string' "from" >> skipMany spacenonewline
      smartdate
    ,
    do
      d <- smartdate
      string' "-"
      return d
    ]
  return $ DateSpan (Just $ fixSmartDate rdate b) Nothing

todatespanp :: Day -> TextParser m DateSpan
todatespanp rdate = do
  choice [string' "to", string' "-"] >> skipMany spacenonewline
  DateSpan Nothing . Just . fixSmartDate rdate <$> smartdate

justdatespanp :: Day -> TextParser m DateSpan
justdatespanp rdate = do
  optional (string' "in" >> skipMany spacenonewline)
  spanFromSmartDate rdate <$> smartdate

-- | Make a datespan from two valid date strings parseable by parsedate
-- (or raise an error). Eg: mkdatespan \"2011/1/1\" \"2011/12/31\".
mkdatespan :: String -> String -> DateSpan
mkdatespan b = DateSpan (Just $ parsedate b) . Just . parsedate

nulldatespan :: DateSpan
nulldatespan = DateSpan Nothing Nothing

-- | A datespan of zero length, that matches no date.
emptydatespan :: DateSpan
emptydatespan = DateSpan (Just $ addDays 1 nulldate) (Just nulldate)

nulldate :: Day
nulldate = fromGregorian 0 1 1
