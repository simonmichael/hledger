{-# LANGUAGE CPP #-}
{-# LANGUAGE NoMonoLocalBinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
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

-}

-- XXX fromGregorian silently clips bad dates, use fromGregorianValid instead ?

module Hledger.Data.Dates (
  -- * Misc date handling utilities
  getCurrentDay,
  getCurrentMonth,
  getCurrentYear,
  nulldate,
  spanContainsDate,
  parsedateM,
  parsedate,
  showDate,
  showDateSpan,
  elapsedSeconds,
  prevday,
  parsePeriodExpr,
  nulldatespan,
  failIfInvalidYear,
  failIfInvalidMonth,
  failIfInvalidDay,
  datesepchar,
  datesepchars,
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
import Prelude.Compat
import Control.Monad
import Data.List.Compat
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
import Data.Time.Calendar.WeekDate
import Data.Time.Clock
import Data.Time.LocalTime
import Safe (headMay, lastMay, readMay)
import Text.Megaparsec
import Text.Megaparsec.Text
import Text.Printf

import Hledger.Data.Types
import Hledger.Utils


-- Help ppShow parse and line-wrap DateSpans better in debug output.
instance Show DateSpan where
    show s = "DateSpan " ++ showDateSpan s
    -- show s = "DateSpan \"" ++ showDateSpan s ++ "\"" -- quotes to help pretty-show

showDate :: Day -> String
showDate = formatTime defaultTimeLocale "%0C%y/%m/%d"

-- XXX review for more boundary crossing issues
-- | Render a datespan as a display string, abbreviating into a
-- compact form if possible.
showDateSpan ds@(DateSpan (Just from) (Just to)) =
  case (toGregorian from, toGregorian to) of
    -- special cases we can abbreviate:
    -- a year, YYYY
    ((fy,1,1), (ty,1,1))   | fy+1==ty           -> formatTime defaultTimeLocale "%0C%y" from
    -- a half, YYYYhN
    ((fy,1,1), (ty,7,1))   | fy==ty             -> formatTime defaultTimeLocale "%0C%yh1" from
    ((fy,7,1), (ty,1,1))   | fy+1==ty           -> formatTime defaultTimeLocale "%0C%yh2" from
    -- a quarter, YYYYqN
    ((fy,1,1), (ty,4,1))   | fy==ty             -> formatTime defaultTimeLocale "%0C%yq1" from
    ((fy,4,1), (ty,7,1))   | fy==ty             -> formatTime defaultTimeLocale "%0C%yq2" from
    ((fy,7,1), (ty,10,1))  | fy==ty             -> formatTime defaultTimeLocale "%0C%yq3" from
    ((fy,10,1), (ty,1,1))  | fy+1==ty           -> formatTime defaultTimeLocale "%0C%yq4" from
    -- a month, YYYY/MM
    ((fy,fm,1), (ty,tm,1)) | fy==ty && fm+1==tm -> formatTime defaultTimeLocale "%0C%y/%m" from
    ((fy,12,1), (ty,1,1))  | fy+1==ty           -> formatTime defaultTimeLocale "%0C%y/%m" from
    -- a week (two successive mondays),
    -- YYYYwN ("week N of year YYYY")
    -- _ | let ((fy,fw,fd), (ty,tw,td)) = (toWeekDate from, toWeekDate to) in fy==ty && fw+1==tw && fd==1 && td==1
    --                                             -> formatTime defaultTimeLocale "%0f%gw%V" from
    -- YYYY/MM/DDwN ("week N, starting on YYYY/MM/DD")
    _ | let ((fy,fw,fd), (ty,tw,td)) = (toWeekDate from, toWeekDate (addDays (-1) to)) in fy==ty && fw==tw && fd==1 && td==7
                                                -> formatTime defaultTimeLocale "%0C%y/%m/%dw%V" from
    -- a day, YYYY/MM/DDd (d suffix is to distinguish from a regular date in register)
    ((fy,fm,fd), (ty,tm,td)) | fy==ty && fm==tm && fd+1==td -> formatTime defaultTimeLocale "%0C%y/%m/%dd" from
    -- ((fy,fm,fd), (ty,tm,td)) | fy==ty && fm==tm && fd+1==td -> formatTime defaultTimeLocale "%0C%y/%m/%d" from -- try without the d
    -- crossing a year boundary
    ((fy,fm,fd), (ty,tm,td)) | fy+1==ty && fm==12 && tm==1 && fd==31 && td==1 -> formatTime defaultTimeLocale "%0C%y/%m/%dd" from
    -- crossing a month boundary XXX wrongly shows LEAPYEAR/2/28-LEAPYEAR/3/1 as LEAPYEAR/2/28
    ((fy,fm,fd), (ty,tm,td)) | fy==ty && fm+1==tm && fd `elem` fromMaybe [] (lookup fm lastdayofmonth) && td==1 -> formatTime defaultTimeLocale "%0C%y/%m/%dd" from
    -- otherwise, YYYY/MM/DD-YYYY/MM/DD
    _                                           -> showDateSpan' ds
  where lastdayofmonth = [(1,[31])
                         ,(2,[28,29])
                         ,(3,[31])
                         ,(4,[30])
                         ,(5,[31])
                         ,(6,[30])
                         ,(7,[31])
                         ,(8,[31])
                         ,(9,[30])
                         ,(10,[31])
                         ,(11,[30])
                         ,(12,[31])
                         ]

showDateSpan ds = showDateSpan' ds

-- | Render a datespan as a display string like [START]-[ENDINCL]
-- (optional start date, hyphen, optional inclusive end date).
showDateSpan' (DateSpan from to) =
  concat
    [maybe "" showDate from
    ,"-"
    ,maybe "" (showDate . prevday) to
    ]

-- | Get the current local date.
getCurrentDay :: IO Day
getCurrentDay = do
    t <- getZonedTime
    return $ localDay (zonedTimeToLocalTime t)

-- | Get the current local month number.
getCurrentMonth :: IO Int
getCurrentMonth = do
  (_,m,_) <- toGregorian `fmap` getCurrentDay
  return m

-- | Get the current local year.
getCurrentYear :: IO Integer
getCurrentYear = do
  (y,_,_) <- toGregorian `fmap` getCurrentDay
  return y

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

-- | Split a DateSpan into one or more consecutive whole spans of the specified length which enclose it.
-- If no interval is specified, the original span is returned.
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
-- [DateSpan 2008/01/01-2007/12/31]
-- >>> t (Quarters 1) "2008/01/01" "2008/01/01"
-- [DateSpan 2008/01/01-2007/12/31]
-- >>> t (Months 1) "2008/01/01" "2008/04/01"
-- [DateSpan 2008/01,DateSpan 2008/02,DateSpan 2008/03]
-- >>> t (Months 2) "2008/01/01" "2008/04/01"
-- [DateSpan 2008/01/01-2008/02/29,DateSpan 2008/03/01-2008/04/30]
-- >>> t (Weeks 1) "2008/01/01" "2008/01/15"
-- [DateSpan 2007/12/31w01,DateSpan 2008/01/07w02,DateSpan 2008/01/14w03]
-- >>> t (Weeks 2) "2008/01/01" "2008/01/15"
-- [DateSpan 2007/12/31-2008/01/13,DateSpan 2008/01/14-2008/01/27]
-- >>> t (DayOfMonth 2) "2008/01/01" "2008/04/01"
-- [DateSpan 2008/01/02-2008/02/01,DateSpan 2008/02/02-2008/03/01,DateSpan 2008/03/02-2008/04/01]
-- >>> t (DayOfWeek 2) "2011/01/01" "2011/01/15"
-- [DateSpan 2011/01/04-2011/01/10,DateSpan 2011/01/11-2011/01/17]
--
splitSpan :: Interval -> DateSpan -> [DateSpan]
splitSpan _ (DateSpan Nothing Nothing) = [DateSpan Nothing Nothing]
splitSpan NoInterval     s = [s]
splitSpan (Days n)       s = splitspan startofday     (applyN n nextday)     s
splitSpan (Weeks n)      s = splitspan startofweek    (applyN n nextweek)    s
splitSpan (Months n)     s = splitspan startofmonth   (applyN n nextmonth)   s
splitSpan (Quarters n)   s = splitspan startofquarter (applyN n nextquarter) s
splitSpan (Years n)      s = splitspan startofyear    (applyN n nextyear)    s
splitSpan (DayOfMonth n) s = splitspan (nthdayofmonthcontaining n) (applyN (n-1) nextday . nextmonth) s
splitSpan (DayOfWeek n)  s = splitspan (nthdayofweekcontaining n)  (applyN (n-1) nextday . nextweek)  s
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

-- | Does the span include the given date ?
spanContainsDate :: DateSpan -> Day -> Bool
spanContainsDate (DateSpan Nothing Nothing)   _ = True
spanContainsDate (DateSpan Nothing (Just e))  d = d < e
spanContainsDate (DateSpan (Just b) Nothing)  d = d >= b
spanContainsDate (DateSpan (Just b) (Just e)) d = d >= b && d < e

-- | Calculate the intersection of a number of datespans.
spansIntersect [] = nulldatespan
spansIntersect [d] = d
spansIntersect (d:ds) = d `spanIntersect` (spansIntersect ds)

-- | Calculate the intersection of two datespans.
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
parsePeriodExpr :: Day -> Text -> Either (ParseError Char Dec) (Interval, DateSpan)
parsePeriodExpr refdate = parsewith (periodexpr refdate <* eof)

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
fixSmartDateStr d s = either
                       (\e->error' $ printf "could not parse date %s %s" (show s) (show e))
                       id
                       $ (fixSmartDateStrEither d s :: Either (ParseError Char Dec) String)

-- | A safe version of fixSmartDateStr.
fixSmartDateStrEither :: Day -> Text -> Either (ParseError Char Dec) String
fixSmartDateStrEither d = either Left (Right . showDate) . fixSmartDateStrEither' d

fixSmartDateStrEither' :: Day -> Text -> Either (ParseError Char Dec) Day
fixSmartDateStrEither' d s = case parsewith smartdateonly (T.toLower s) of
                               Right sd -> Right $ fixSmartDate d sd
                               Left e -> Left e

-- | Convert a SmartDate to an absolute date using the provided reference date.
--
-- ==== Examples:
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
fixSmartDate refdate sdate = fix sdate
    where
      fix :: SmartDate -> Day
      fix ("","","today")       = fromGregorian ry rm rd
      fix ("","this","day")     = fromGregorian ry rm rd
      fix ("","","yesterday")   = prevday refdate
      fix ("","last","day")     = prevday refdate
      fix ("","","tomorrow")    = nextday refdate
      fix ("","next","day")     = nextday refdate
      fix ("","last","week")    = prevweek refdate
      fix ("","this","week")    = thisweek refdate
      fix ("","next","week")    = nextweek refdate
      fix ("","last","month")   = prevmonth refdate
      fix ("","this","month")   = thismonth refdate
      fix ("","next","month")   = nextmonth refdate
      fix ("","last","quarter") = prevquarter refdate
      fix ("","this","quarter") = thisquarter refdate
      fix ("","next","quarter") = nextquarter refdate
      fix ("","last","year")    = prevyear refdate
      fix ("","this","year")    = thisyear refdate
      fix ("","next","year")    = nextyear refdate
      fix ("","",d)             = fromGregorian ry rm (read d)
      fix ("",m,"")             = fromGregorian ry (read m) 1
      fix ("",m,d)              = fromGregorian ry (read m) (read d)
      fix (y,"","")             = fromGregorian (read y) 1 1
      fix (y,m,"")              = fromGregorian (read y) (read m) 1
      fix (y,m,d)               = fromGregorian (read y) (read m) (read d)
      (ry,rm,rd) = toGregorian refdate

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

nthdayofmonthcontaining n d | d1 >= d    = d1
                            | otherwise = d2
    where d1 = addDays (fromIntegral n-1) s
          d2 = addDays (fromIntegral n-1) $ nextmonth s
          s = startofmonth d

nthdayofweekcontaining n d | d1 >= d    = d1
                           | otherwise = d2
    where d1 = addDays (fromIntegral n-1) s
          d2 = addDays (fromIntegral n-1) $ nextweek s
          s = startofweek d

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

-- | Parse a time string to a time type using the provided pattern, or
-- return the default.
_parsetimewith :: ParseTime t => String -> String -> t -> t
_parsetimewith pat s def = fromMaybe def $ parsetime defaultTimeLocale pat s

{-|
Parse a date in any of the formats allowed in ledger's period expressions,
and maybe some others:

> 2004
> 2004/10
> 2004/10/1
> 10/1
> 21
> october, oct
> yesterday, today, tomorrow
> this/next/last week/day/month/quarter/year

Returns a SmartDate, to be converted to a full date later (see fixSmartDate).
Assumes any text in the parse stream has been lowercased.
-}
smartdate :: Parser SmartDate
smartdate = do
  -- XXX maybe obscures date errors ? see ledgerdate
  (y,m,d) <- choice' [yyyymmdd, ymd, ym, md, y, d, month, mon, today, yesterday, tomorrow, lastthisnextthing]
  return (y,m,d)

-- | Like smartdate, but there must be nothing other than whitespace after the date.
smartdateonly :: Parser SmartDate
smartdateonly = do
  d <- smartdate
  many spacenonewline
  eof
  return d

datesepchars :: [Char]
datesepchars = "/-."
datesepchar :: TextParser m Char
datesepchar = oneOf datesepchars

validYear, validMonth, validDay :: String -> Bool
validYear s = length s >= 4 && isJust (readMay s :: Maybe Year)
validMonth s = maybe False (\n -> n>=1 && n<=12) $ readMay s
validDay s = maybe False (\n -> n>=1 && n<=31) $ readMay s

failIfInvalidYear, failIfInvalidMonth, failIfInvalidDay :: (Monad m) => String -> m ()
failIfInvalidYear s  = unless (validYear s)  $ fail $ "bad year number: " ++ s
failIfInvalidMonth s = unless (validMonth s) $ fail $ "bad month number: " ++ s
failIfInvalidDay s   = unless (validDay s)   $ fail $ "bad day number: " ++ s

yyyymmdd :: Parser SmartDate
yyyymmdd = do
  y <- count 4 digitChar
  m <- count 2 digitChar
  failIfInvalidMonth m
  d <- count 2 digitChar
  failIfInvalidDay d
  return (y,m,d)

ymd :: Parser SmartDate
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

ym :: Parser SmartDate
ym = do
  y <- some digitChar
  failIfInvalidYear y
  datesepchar
  m <- some digitChar
  failIfInvalidMonth m
  return (y,m,"")

y :: Parser SmartDate
y = do
  y <- some digitChar
  failIfInvalidYear y
  return (y,"","")

d :: Parser SmartDate
d = do
  d <- some digitChar
  failIfInvalidDay d
  return ("","",d)

md :: Parser SmartDate
md = do
  m <- some digitChar
  failIfInvalidMonth m
  datesepchar
  d <- some digitChar
  failIfInvalidDay d
  return ("",m,d)

months         = ["january","february","march","april","may","june",
                  "july","august","september","october","november","december"]
monthabbrevs   = ["jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec"]
-- weekdays       = ["monday","tuesday","wednesday","thursday","friday","saturday","sunday"]
-- weekdayabbrevs = ["mon","tue","wed","thu","fri","sat","sun"]

monthIndex s = maybe 0 (+1) $ lowercase s `elemIndex` months
monIndex s   = maybe 0 (+1) $ lowercase s `elemIndex` monthabbrevs

month :: Parser SmartDate
month = do
  m <- choice $ map (try . string) months
  let i = monthIndex m
  return ("",show i,"")

mon :: Parser SmartDate
mon = do
  m <- choice $ map (try . string) monthabbrevs
  let i = monIndex m
  return ("",show i,"")

today,yesterday,tomorrow :: Parser SmartDate
today     = string "today"     >> return ("","","today")
yesterday = string "yesterday" >> return ("","","yesterday")
tomorrow  = string "tomorrow"  >> return ("","","tomorrow")

lastthisnextthing :: Parser SmartDate
lastthisnextthing = do
  r <- choice [
        string "last"
       ,string "this"
       ,string "next"
      ]
  many spacenonewline  -- make the space optional for easier scripting
  p <- choice [
        string "day"
       ,string "week"
       ,string "month"
       ,string "quarter"
       ,string "year"
      ]
-- XXX support these in fixSmartDate
--       ++ (map string $ months ++ monthabbrevs ++ weekdays ++ weekdayabbrevs)

  return ("",r,p)

-- |
-- >>> let p = parsewith (periodexpr (parsedate "2008/11/26")) :: T.Text -> Either (ParseError Char Dec) (Interval, DateSpan)
-- >>> p "from aug to oct"
-- Right (NoInterval,DateSpan 2008/08/01-2008/09/30)
-- >>> p "aug to oct"
-- Right (NoInterval,DateSpan 2008/08/01-2008/09/30)
-- >>> p "every 3 days in aug"
-- Right (Days 3,DateSpan 2008/08)
-- >>> p "daily from aug"
-- Right (Days 1,DateSpan 2008/08/01-)
-- >>> p "every week to 2009"
-- Right (Weeks 1,DateSpan -2008/12/31)
periodexpr :: Day -> Parser (Interval, DateSpan)
periodexpr rdate = choice $ map try [
                    intervalanddateperiodexpr rdate,
                    intervalperiodexpr,
                    dateperiodexpr rdate,
                    (return (NoInterval,DateSpan Nothing Nothing))
                   ]

intervalanddateperiodexpr :: Day -> Parser (Interval, DateSpan)
intervalanddateperiodexpr rdate = do
  many spacenonewline
  i <- reportinginterval
  many spacenonewline
  s <- periodexprdatespan rdate
  return (i,s)

intervalperiodexpr :: Parser (Interval, DateSpan)
intervalperiodexpr = do
  many spacenonewline
  i <- reportinginterval
  return (i, DateSpan Nothing Nothing)

dateperiodexpr :: Day -> Parser (Interval, DateSpan)
dateperiodexpr rdate = do
  many spacenonewline
  s <- periodexprdatespan rdate
  return (NoInterval, s)

-- Parse a reporting interval.
reportinginterval :: Parser Interval
reportinginterval = choice' [
                       tryinterval "day"     "daily"     Days,
                       tryinterval "week"    "weekly"    Weeks,
                       tryinterval "month"   "monthly"   Months,
                       tryinterval "quarter" "quarterly" Quarters,
                       tryinterval "year"    "yearly"    Years,
                       do string "biweekly"
                          return $ Weeks 2,
                       do string "bimonthly"
                          return $ Months 2,
                       do string "every"
                          many spacenonewline
                          n <- fmap read $ some digitChar
                          thsuffix
                          many spacenonewline
                          string "day"
                          many spacenonewline
                          string "of"
                          many spacenonewline
                          string "week"
                          return $ DayOfWeek n,
                       do string "every"
                          many spacenonewline
                          n <- fmap read $ some digitChar
                          thsuffix
                          many spacenonewline
                          string "day"
                          optional $ do
                            many spacenonewline
                            string "of"
                            many spacenonewline
                            string "month"
                          return $ DayOfMonth n
                    ]
    where

      thsuffix = choice' $ map string ["st","nd","rd","th"]

      -- Parse any of several variants of a basic interval, eg "daily", "every day", "every N days".
      tryinterval :: String -> String -> (Int -> Interval) -> Parser Interval
      tryinterval singular compact intcons =
          choice' [
           do string compact
              return $ intcons 1,
           do string "every"
              many spacenonewline
              string singular
              return $ intcons 1,
           do string "every"
              many spacenonewline
              n <- fmap read $ some digitChar
              many spacenonewline
              string plural
              return $ intcons n
           ]
          where plural = singular ++ "s"

periodexprdatespan :: Day -> Parser DateSpan
periodexprdatespan rdate = choice $ map try [
                            doubledatespan rdate,
                            fromdatespan rdate,
                            todatespan rdate,
                            justdatespan rdate
                           ]

doubledatespan :: Day -> Parser DateSpan
doubledatespan rdate = do
  optional (string "from" >> many spacenonewline)
  b <- smartdate
  many spacenonewline
  optional (choice [string "to", string "-"] >> many spacenonewline)
  e <- smartdate
  return $ DateSpan (Just $ fixSmartDate rdate b) (Just $ fixSmartDate rdate e)

fromdatespan :: Day -> Parser DateSpan
fromdatespan rdate = do
  b <- choice [
    do
      string "from" >> many spacenonewline
      smartdate
    ,
    do
      d <- smartdate
      string "-"
      return d
    ]
  return $ DateSpan (Just $ fixSmartDate rdate b) Nothing

todatespan :: Day -> Parser DateSpan
todatespan rdate = do
  choice [string "to", string "-"] >> many spacenonewline
  e <- smartdate
  return $ DateSpan Nothing (Just $ fixSmartDate rdate e)

justdatespan :: Day -> Parser DateSpan
justdatespan rdate = do
  optional (string "in" >> many spacenonewline)
  d <- smartdate
  return $ spanFromSmartDate rdate d

-- | Make a datespan from two valid date strings parseable by parsedate
-- (or raise an error). Eg: mkdatespan \"2011/1/1\" \"2011/12/31\".
mkdatespan :: String -> String -> DateSpan
mkdatespan b = DateSpan (Just $ parsedate b) . Just . parsedate

nulldatespan :: DateSpan
nulldatespan = DateSpan Nothing Nothing

nulldate :: Day
nulldate = fromGregorian 0 1 1
