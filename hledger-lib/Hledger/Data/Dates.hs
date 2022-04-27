{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoMonoLocalBinds    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeFamilies        #-}
{-|

Date parsing and utilities for hledger.

For date and time values, we use the standard Day and UTCTime types.

A 'SmartDate' is a date which may be partially-specified or relative.
Eg 2008\/12\/31, but also 2008\/12, 12\/31, tomorrow, last week, next year,
in 5 days, in -3 quarters.
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
  datesepchar,
  datesepchars,
  isDateSepChar,
  spanStart,
  spanEnd,
  spanStartYear,
  spanEndYear,
  spanYears,
  spansSpan,
  spanIntersect,
  spansIntersect,
  spanDefaultsFrom,
  spanUnion,
  spansUnion,
  daysSpan,
  latestSpanContaining,
  smartdate,
  splitSpan,
  spansFromBoundaries,
  groupByDateSpan,
  fixSmartDate,
  fixSmartDateStr,
  fixSmartDateStrEither,
  fixSmartDateStrEither',
  yearp,
  daysInSpan,

  tests_Dates
)
where

import qualified Control.Monad.Fail as Fail (MonadFail, fail)
import Control.Applicative (liftA2)
import Control.Applicative.Permutations
import Control.Monad (guard, unless)
import Data.Char (digitToInt, isDigit, ord)
import Data.Default (def)
import Data.Foldable (asum)
import Data.Function (on)
import Data.Functor (($>))
import Data.List (elemIndex, group, sort, sortBy)
import Data.Maybe (catMaybes, fromMaybe, isJust, mapMaybe)
import Data.Ord (comparing)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Format hiding (months)
import Data.Time.Calendar
    (Day, addDays, addGregorianYearsClip, addGregorianMonthsClip, diffDays,
     fromGregorian, fromGregorianValid, toGregorian)
import Data.Time.Calendar.OrdinalDate (fromMondayStartWeek, mondayStartWeek)
import Data.Time.Clock (UTCTime, diffUTCTime)
import Data.Time.LocalTime (getZonedTime, localDay, zonedTimeToLocalTime)
import Safe (headMay, lastMay, maximumMay, minimumMay)
import Text.Megaparsec
import Text.Megaparsec.Char (char, char', digitChar, string, string')
import Text.Megaparsec.Char.Lexer (decimal, signed)
import Text.Megaparsec.Custom (customErrorBundlePretty, HledgerParseErrors)
import Text.Printf (printf)

import Hledger.Data.Types
import Hledger.Data.Period
import Hledger.Utils


-- Help ppShow parse and line-wrap DateSpans better in debug output.
instance Show DateSpan where
    show s = "DateSpan " ++ T.unpack (showDateSpan s)

showDate :: Day -> Text
showDate = T.pack . show

-- | Render a datespan as a display string, abbreviating into a
-- compact form if possible.
showDateSpan :: DateSpan -> Text
showDateSpan = showPeriod . dateSpanAsPeriod

-- | Like showDateSpan, but show month spans as just the abbreviated month name
-- in the current locale.
showDateSpanMonthAbbrev :: DateSpan -> Text
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

spanStartYear :: DateSpan -> Maybe Year
spanStartYear (DateSpan d _) = fmap (first3 . toGregorian) d

spanEndYear :: DateSpan -> Maybe Year
spanEndYear (DateSpan d _) = fmap (first3 . toGregorian) d

-- | Get the 0-2 years mentioned explicitly in a DateSpan.
spanYears :: DateSpan -> [Year]
spanYears (DateSpan ma mb) = mapMaybe (fmap (first3 . toGregorian)) [ma,mb]

-- might be useful later: http://en.wikipedia.org/wiki/Allen%27s_interval_algebra

-- | Get overall span enclosing multiple sequentially ordered spans.
spansSpan :: [DateSpan] -> DateSpan
spansSpan spans = DateSpan (spanStart =<< headMay spans) (spanEnd =<< lastMay spans)

-- | Split a DateSpan into consecutive whole spans of the specified interval
-- which fully encompass the original span (and a little more when necessary).
-- If no interval is specified, the original span is returned.
-- If the original span is the null date span, ie unbounded, the null date span is returned.
-- If the original span is empty, eg if the end date is <= the start date, no spans are returned.
--
--
-- ==== Examples:
-- >>> let t i y1 m1 d1 y2 m2 d2 = splitSpan i $ DateSpan (Just $ fromGregorian y1 m1 d1) (Just $ fromGregorian y2 m2 d2)
-- >>> t NoInterval 2008 01 01 2009 01 01
-- [DateSpan 2008]
-- >>> t (Quarters 1) 2008 01 01 2009 01 01
-- [DateSpan 2008Q1,DateSpan 2008Q2,DateSpan 2008Q3,DateSpan 2008Q4]
-- >>> splitSpan (Quarters 1) nulldatespan
-- [DateSpan ..]
-- >>> t (Days 1) 2008 01 01 2008 01 01  -- an empty datespan
-- []
-- >>> t (Quarters 1) 2008 01 01 2008 01 01
-- []
-- >>> t (Months 1) 2008 01 01 2008 04 01
-- [DateSpan 2008-01,DateSpan 2008-02,DateSpan 2008-03]
-- >>> t (Months 2) 2008 01 01 2008 04 01
-- [DateSpan 2008-01-01..2008-02-29,DateSpan 2008-03-01..2008-04-30]
-- >>> t (Weeks 1) 2008 01 01 2008 01 15
-- [DateSpan 2007-12-31W01,DateSpan 2008-01-07W02,DateSpan 2008-01-14W03]
-- >>> t (Weeks 2) 2008 01 01 2008 01 15
-- [DateSpan 2007-12-31..2008-01-13,DateSpan 2008-01-14..2008-01-27]
-- >>> t (DayOfMonth 2) 2008 01 01 2008 04 01
-- [DateSpan 2007-12-02..2008-01-01,DateSpan 2008-01-02..2008-02-01,DateSpan 2008-02-02..2008-03-01,DateSpan 2008-03-02..2008-04-01]
-- >>> t (WeekdayOfMonth 2 4) 2011 01 01 2011 02 15
-- [DateSpan 2010-12-09..2011-01-12,DateSpan 2011-01-13..2011-02-09,DateSpan 2011-02-10..2011-03-09]
-- >>> t (DaysOfWeek [2]) 2011 01 01 2011 01 15
-- [DateSpan 2010-12-28..2011-01-03,DateSpan 2011-01-04..2011-01-10,DateSpan 2011-01-11..2011-01-17]
-- >>> t (DayOfYear 11 29) 2011 10 01 2011 10 15
-- [DateSpan 2010-11-29..2011-11-28]
-- >>> t (DayOfYear 11 29) 2011 12 01 2012 12 15
-- [DateSpan 2011-11-29..2012-11-28,DateSpan 2012-11-29..2013-11-28]
--
splitSpan :: Interval -> DateSpan -> [DateSpan]
splitSpan _ (DateSpan Nothing Nothing) = [DateSpan Nothing Nothing]
splitSpan _ ds | isEmptySpan ds = []
splitSpan _ ds@(DateSpan (Just s) (Just e)) | s == e = [ds]
splitSpan NoInterval      ds = [ds]
splitSpan (Days n)        ds = splitspan startofday     addDays n                    ds
splitSpan (Weeks n)       ds = splitspan startofweek    addDays (7*n)                ds
splitSpan (Months n)      ds = splitspan startofmonth   addGregorianMonthsClip n     ds
splitSpan (Quarters n)    ds = splitspan startofquarter addGregorianMonthsClip (3*n) ds
splitSpan (Years n)       ds = splitspan startofyear    addGregorianYearsClip n      ds
splitSpan (DayOfMonth n)  ds = splitspan (nthdayofmonthcontaining n)  addGregorianMonthsClip 1 ds
splitSpan (DayOfYear m n) ds = splitspan (nthdayofyearcontaining m n) addGregorianYearsClip 1 ds
splitSpan (WeekdayOfMonth n wd) ds = splitspan (nthweekdayofmonthcontaining n wd) advancemonths 1 ds
  where
    advancemonths 0 = id
    advancemonths w = advancetonthweekday n wd . startofmonth . addGregorianMonthsClip w
splitSpan (DaysOfWeek [])         ds = [ds]
splitSpan (DaysOfWeek days@(n:_)) ds = spansFromBoundaries e bdrys
  where
    (s, e) = dateSpanSplitLimits (nthdayofweekcontaining n) nextday ds
    bdrys = concatMap (flip map starts . addDays) [0,7..]
    -- The first representative of each weekday
    starts = map (\d -> addDays (toInteger $ d - n) $ nthdayofweekcontaining n s) days

-- Split the given span using the provided helper functions:
-- start is applied to the span's start date to get the first sub-span's start date
-- addInterval is applied to an integer n (multiplying it by mult) and the span's start date to get the nth sub-span's start date
splitspan :: (Day -> Day) -> (Integer -> Day -> Day) -> Int -> DateSpan -> [DateSpan]
splitspan start addInterval mult ds = spansFromBoundaries e bdrys
  where
    (s, e) = dateSpanSplitLimits start (addInterval $ toInteger mult) ds
    bdrys = mapM (addInterval . toInteger) [0,mult..] $ start s

-- | Fill in missing endpoints for calculating 'splitSpan'.
dateSpanSplitLimits :: (Day -> Day) -> (Day -> Day) -> DateSpan -> (Day, Day)
dateSpanSplitLimits start _    (DateSpan (Just s) (Just e)) = (start s, e)
dateSpanSplitLimits start next (DateSpan (Just s) Nothing)  = (start s, next $ start s)
dateSpanSplitLimits start next (DateSpan Nothing  (Just e)) = (start e, next $ start e)
dateSpanSplitLimits _     _    (DateSpan Nothing   Nothing) = error "dateSpanSplitLimits: Should not be nulldatespan"  -- PARTIAL: This case should have been handled in splitSpan

-- | Construct a list of 'DateSpan's from a list of boundaries, which fit within a given range.
spansFromBoundaries :: Day -> [Day] -> [DateSpan]
spansFromBoundaries e bdrys = zipWith (DateSpan `on` Just) (takeWhile (< e) bdrys) $ drop 1 bdrys

-- | Count the days in a DateSpan, or if it is open-ended return Nothing.
daysInSpan :: DateSpan -> Maybe Integer
daysInSpan (DateSpan (Just d1) (Just d2)) = Just $ diffDays d2 d1
daysInSpan _ = Nothing

-- | Is this an empty span, ie closed with the end date on or before the start date ?
isEmptySpan :: DateSpan -> Bool
isEmptySpan (DateSpan (Just s) (Just e)) = e <= s
isEmptySpan _                            = False

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

-- | Group elements based on where they fall in a list of 'DateSpan's without
-- gaps. The precondition is not checked.
groupByDateSpan :: Bool -> (a -> Day) -> [DateSpan] -> [a] -> [(DateSpan, [a])]
groupByDateSpan showempty date colspans =
      groupByCols colspans
    . dropWhile (beforeStart . fst)
    . sortBy (comparing fst)
    . map (\x -> (date x, x))
  where
    groupByCols []     _  = []
    groupByCols (c:cs) [] = if showempty then (c, []) : groupByCols cs [] else []
    groupByCols (c:cs) ps = (c, map snd matches) : groupByCols cs later
      where (matches, later) = span ((spanEnd c >) . Just . fst) ps

    beforeStart = maybe (const True) (>) $ spanStart =<< headMay colspans

-- | Calculate the intersection of a number of datespans.
spansIntersect [] = nulldatespan
spansIntersect [d] = d
spansIntersect (d:ds) = d `spanIntersect` (spansIntersect ds)

-- | Calculate the intersection of two datespans.
--
-- For non-intersecting spans, gives an empty span beginning on the second's start date:
-- >>> DateSpan (Just $ fromGregorian 2018 01 01) (Just $ fromGregorian 2018 01 03) `spanIntersect` DateSpan (Just $ fromGregorian 2018 01 03) (Just $ fromGregorian 2018 01 05)
-- DateSpan 2018-01-03..2018-01-02
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

-- | Calculate the minimal DateSpan containing all of the given Days (in the
-- usual exclusive-end-date sense: beginning on the earliest, and ending on
-- the day after the latest).
daysSpan :: [Day] -> DateSpan
daysSpan ds = DateSpan (minimumMay ds) (addDays 1 <$> maximumMay ds)

-- | Select the DateSpan containing a given Day, if any, from a given list of
-- DateSpans.
--
-- If the DateSpans are non-overlapping, this returns the unique containing
-- DateSpan, if it exists. If the DateSpans are overlapping, it will return the
-- containing DateSpan with the latest start date, and then latest end date.

-- Note: This will currently return `DateSpan (Just s) (Just e)` before it will
-- return `DateSpan (Just s) Nothing`. It's unclear which behaviour is desired.
-- This is irrelevant at the moment as it's never applied to any list with
-- overlapping DateSpans.
latestSpanContaining :: [DateSpan] -> Day -> Maybe DateSpan
latestSpanContaining datespans = go
  where
    go day = do
        span <- Set.lookupLT supSpan spanSet
        guard $ spanContainsDate span day
        return span
      where
        -- The smallest DateSpan larger than any DateSpan containing day.
        supSpan = DateSpan (Just $ addDays 1 day) Nothing

    spanSet = Set.fromList $ filter (not . isEmptySpan) datespans

-- | Parse a period expression to an Interval and overall DateSpan using
-- the provided reference date, or return a parse error.
parsePeriodExpr
  :: Day -> Text -> Either HledgerParseErrors (Interval, DateSpan)
parsePeriodExpr refdate s = parsewith (periodexprp refdate <* eof) (T.toLower s)

-- | Like parsePeriodExpr, but call error' on failure.
parsePeriodExpr' :: Day -> Text -> (Interval, DateSpan)
parsePeriodExpr' refdate s =
  either (error' . ("failed to parse:" ++) . customErrorBundlePretty) id $  -- PARTIAL:
  parsePeriodExpr refdate s

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
      span (SmartCompleteDate day)       = (day, nextday day)
      span (SmartAssumeStart y Nothing)  = (startofyear day, nextyear day) where day = fromGregorian y 1 1
      span (SmartAssumeStart y (Just m)) = (startofmonth day, nextmonth day) where day = fromGregorian y m 1
      span (SmartFromReference m d)      = (day, nextday day) where day = fromGregorian ry (fromMaybe rm m) d
      span (SmartMonth m)                = (startofmonth day, nextmonth day) where day = fromGregorian ry m 1
      span (SmartRelative n Day)         = (addDays n refdate, addDays (n+1) refdate)
      span (SmartRelative n Week)        = (addDays (7*n) d, addDays (7*n+7) d) where d = thisweek refdate
      span (SmartRelative n Month)       = (addGregorianMonthsClip n d, addGregorianMonthsClip (n+1) d) where d = thismonth refdate
      span (SmartRelative n Quarter)     = (addGregorianMonthsClip (3*n) d, addGregorianMonthsClip (3*n+3) d) where d = thisquarter refdate
      span (SmartRelative n Year)        = (addGregorianYearsClip n d, addGregorianYearsClip (n+1) d) where d = thisyear refdate

-- showDay :: Day -> String
-- showDay day = printf "%04d/%02d/%02d" y m d where (y,m,d) = toGregorian day

-- | Convert a smart date string to an explicit yyyy\/mm\/dd string using
-- the provided reference date, or raise an error.
fixSmartDateStr :: Day -> Text -> Text
fixSmartDateStr d s =
  either (error' . printf "could not parse date %s %s" (show s) . show) id $  -- PARTIAL:
  (fixSmartDateStrEither d s :: Either HledgerParseErrors Text)

-- | A safe version of fixSmartDateStr.
fixSmartDateStrEither :: Day -> Text -> Either HledgerParseErrors Text
fixSmartDateStrEither d = fmap showDate . fixSmartDateStrEither' d

fixSmartDateStrEither'
  :: Day -> Text -> Either HledgerParseErrors Day
fixSmartDateStrEither' d s = case parsewith smartdateonly (T.toLower s) of
                               Right sd -> Right $ fixSmartDate d sd
                               Left e -> Left e

-- | Convert a SmartDate to an absolute date using the provided reference date.
--
-- ==== Examples:
-- >>> :set -XOverloadedStrings
-- >>> let t = fixSmartDateStr (fromGregorian 2008 11 26)
-- >>> t "0000-01-01"
-- "0000-01-01"
-- >>> t "1999-12-02"
-- "1999-12-02"
-- >>> t "1999.12.02"
-- "1999-12-02"
-- >>> t "1999/3/2"
-- "1999-03-02"
-- >>> t "19990302"
-- "1999-03-02"
-- >>> t "2008/2"
-- "2008-02-01"
-- >>> t "0020/2"
-- "0020-02-01"
-- >>> t "1000"
-- "1000-01-01"
-- >>> t "4/2"
-- "2008-04-02"
-- >>> t "2"
-- "2008-11-02"
-- >>> t "January"
-- "2008-01-01"
-- >>> t "feb"
-- "2008-02-01"
-- >>> t "today"
-- "2008-11-26"
-- >>> t "yesterday"
-- "2008-11-25"
-- >>> t "tomorrow"
-- "2008-11-27"
-- >>> t "this day"
-- "2008-11-26"
-- >>> t "last day"
-- "2008-11-25"
-- >>> t "next day"
-- "2008-11-27"
-- >>> t "this week"  -- last monday
-- "2008-11-24"
-- >>> t "last week"  -- previous monday
-- "2008-11-17"
-- >>> t "next week"  -- next monday
-- "2008-12-01"
-- >>> t "this month"
-- "2008-11-01"
-- >>> t "last month"
-- "2008-10-01"
-- >>> t "next month"
-- "2008-12-01"
-- >>> t "this quarter"
-- "2008-10-01"
-- >>> t "last quarter"
-- "2008-07-01"
-- >>> t "next quarter"
-- "2009-01-01"
-- >>> t "this year"
-- "2008-01-01"
-- >>> t "last year"
-- "2007-01-01"
-- >>> t "next year"
-- "2009-01-01"
--
-- t "last wed"
-- "2008-11-19"
-- t "next friday"
-- "2008-11-28"
-- t "next january"
-- "2009-01-01"
--
-- >>> t "in 5 days"
-- "2008-12-01"
-- >>> t "in 7 months"
-- "2009-06-01"
-- >>> t "in -2 weeks"
-- "2008-11-10"
-- >>> t "1 quarter ago"
-- "2008-07-01"
-- >>> t "1 week ahead"
-- "2008-12-01"
fixSmartDate :: Day -> SmartDate -> Day
fixSmartDate refdate = fix
  where
    fix :: SmartDate -> Day
    fix (SmartCompleteDate d)     = d
    fix (SmartAssumeStart y m)    = fromGregorian y (fromMaybe 1 m) 1
    fix (SmartFromReference m d)  = fromGregorian ry (fromMaybe rm m) d
    fix (SmartMonth m)            = fromGregorian ry m 1
    fix (SmartRelative n Day)     = addDays n refdate
    fix (SmartRelative n Week)    = addDays (7*n) $ thisweek refdate
    fix (SmartRelative n Month)   = addGregorianMonthsClip n $ thismonth refdate
    fix (SmartRelative n Quarter) = addGregorianMonthsClip (3*n) $ thisquarter refdate
    fix (SmartRelative n Year)    = addGregorianYearsClip n $ thisyear refdate
    (ry, rm, _) = toGregorian refdate

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
-- >>> let wed22nd = fromGregorian 2017 11 22
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
  -- PARTIAL:
  | not (validMonth m)  = error' $ "nthdayofyearcontaining: invalid month "++show m
  | not (validDay   md) = error' $ "nthdayofyearcontaining: invalid day "  ++show md
  | mmddOfSameYear <= date = mmddOfSameYear
  | otherwise = mmddOfPrevYear
  where mmddOfSameYear = addDays (toInteger md-1) $ applyN (m-1) nextmonth s
        mmddOfPrevYear = addDays (toInteger md-1) $ applyN (m-1) nextmonth $ prevyear s
        s = startofyear date

-- | For given date d find month-long interval that starts on nth day of month
-- and covers it.
-- The given day of month should be basically valid (1-31), or an error is raised.
--
-- Examples: lets take 2017-11-22. Month-long intervals covering it that
-- start on 1st-22nd of month will start in Nov. However
-- intervals that start on 23rd-30th of month should start in Oct:
-- >>> let wed22nd = fromGregorian 2017 11 22
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
  -- PARTIAL:
  | not (validDay md) = error' $ "nthdayofmonthcontaining: invalid day "  ++show md
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
-- >>> let wed22nd = fromGregorian 2017 11 22
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
    where nthOfSameWeek = addDays (toInteger n-1) s
          nthOfPrevWeek = addDays (toInteger n-1) $ prevweek s
          s = startofweek d

-- | For given date d find month-long interval that starts on nth weekday of month
-- and covers it.
--
-- Examples: 2017-11-22 is 3rd Wed of Nov. Month-long intervals that cover it and
-- start on 1st-4th Wed will start in Nov. However
-- intervals that start on 4th Thu or Fri or later should start in Oct:
-- >>> let wed22nd = fromGregorian 2017 11 22
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
-- Can call error.
advancetonthweekday :: Int -> WeekDay -> Day -> Day
advancetonthweekday n wd s =
  -- PARTIAL:
  maybe err (addWeeks (n-1)) $ firstMatch (>=s) $ iterate (addWeeks 1) $ firstweekday s
  where
    err = error' "advancetonthweekday: should not happen"
    addWeeks k = addDays (7 * toInteger k)
    firstMatch p = headMay . dropWhile (not . p)
    firstweekday = addDays (toInteger wd-1) . startofweek

----------------------------------------------------------------------
-- parsing

-- -- | Parse a couple of date-time string formats to a time type.
-- parsedatetimeM :: String -> Maybe LocalTime
-- parsedatetimeM s = asum [
--     parseTimeM TruedefaultTimeLocale "%Y/%m/%d %H:%M:%S" s,
--     parseTimeM TruedefaultTimeLocale "%Y-%m-%d %H:%M:%S" s
--     ]

-- | Try to parse a couple of date string formats:
-- `YYYY-MM-DD`, `YYYY/MM/DD` or `YYYY.MM.DD`, with leading zeros required.
-- For internal use, not quite the same as the journal's "simple dates".
-- >>> parsedateM "2008/02/03"
-- Just 2008-02-03
-- >>> parsedateM "2008/02/03/"
-- Nothing
-- >>> parsedateM "2008/02/30"
-- Nothing
parsedateM :: String -> Maybe Day
parsedateM s = asum [
     parseTimeM True defaultTimeLocale "%Y-%m-%d" s,
     parseTimeM True defaultTimeLocale "%Y/%m/%d" s,
     parseTimeM True defaultTimeLocale "%Y.%m.%d" s
     ]

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
> in n days/weeks/months/quarters/years       (n periods from the current period)
> n days/weeks/months/quarters/years ago      (-n periods from the current period)
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
Right (SmartCompleteDate 2018-12-01)

YYYYMM is parsed as year-month-01 if year and month are valid:
>>> parsewith (smartdate <* eof) "201804"
Right (SmartAssumeStart 2018 (Just 4))

With an invalid month, it's parsed as a year:
>>> parsewith (smartdate <* eof) "201813"
Right (SmartAssumeStart 201813 Nothing)

A 9+ digit number beginning with valid YYYYMMDD gives an error:
>>> parsewith (smartdate <* eof) "201801012"
Left (...)

Big numbers not beginning with a valid YYYYMMDD are parsed as a year:
>>> parsewith (smartdate <* eof) "201813012"
Right (SmartAssumeStart 201813012 Nothing)

-}
smartdate :: TextParser m SmartDate
smartdate = choice'
  -- XXX maybe obscures date errors ? see ledgerdate
    [ relativeP
    , yyyymmdd, ymd
    , (\(m,d) -> SmartFromReference (Just m) d) <$> md
    , failIfInvalidDate . SmartFromReference Nothing =<< decimal
    , SmartMonth <$> (month <|> mon)
    , SmartRelative 0    Day <$ string' "today"
    , SmartRelative (-1) Day <$ string' "yesterday"
    , SmartRelative 1    Day <$ string' "tomorrow"
    ]
  where
    relativeP = do
        optional $ string' "in" <* skipNonNewlineSpaces
        num      <- seqP <* skipNonNewlineSpaces
        interval <- intervalP <* skipNonNewlineSpaces
        sign     <- choice [negate <$ string' "ago", id <$ string' "ahead", pure id]
        return $ SmartRelative (sign num) interval

    seqP = choice [ 0 <$ string' "this", -1 <$ string' "last", 1 <$ string' "next", signed skipNonNewlineSpaces decimal ]
    intervalP = choice [ Day <$ string' "day", Week <$ string' "week", Month <$ string' "month"
                       , Quarter <$ string' "quarter", Year <$ string' "year" ] <* optional (char' 's')

-- | Like smartdate, but there must be nothing other than whitespace after the date.
smartdateonly :: TextParser m SmartDate
smartdateonly = smartdate <* skipNonNewlineSpaces <* eof

datesepchars :: String
datesepchars = "/-."

datesepchar :: TextParser m Char
datesepchar = satisfy isDateSepChar

isDateSepChar :: Char -> Bool
isDateSepChar c = c == '-' || c == '/' || c == '.'

validMonth, validDay :: Int -> Bool
validMonth n = n >= 1 && n <= 12
validDay n = n >= 1 && n <= 31

failIfInvalidDate :: Fail.MonadFail m => SmartDate -> m SmartDate
failIfInvalidDate s = unless isValid (Fail.fail $ "bad smart date: " ++ show s) $> s
  where isValid = case s of
            SmartAssumeStart _ (Just m) -> validMonth m
            SmartFromReference mm d     -> isJust $ fromGregorianValid 2004 (fromMaybe 1 mm) d
            SmartMonth m                -> validMonth m
            _                           -> True

showBadDate :: Integer -> Int -> Int -> String
showBadDate y m d = "bad smart date: " ++ show y ++ "-" ++ show m ++ "-" ++ show d

yyyymmdd :: TextParser m SmartDate
yyyymmdd = do
  y <- read <$> count 4 digitChar
  m <- read <$> count 2 digitChar
  md <- optional $ read <$> count 2 digitChar
  case md of
      Nothing -> failIfInvalidDate $ SmartAssumeStart y (Just m)
      Just d  -> maybe (Fail.fail $ showBadDate y m d) (return . SmartCompleteDate) $
                   fromGregorianValid y m d

ymd :: TextParser m SmartDate
ymd = do
    y <- yearp
    emd <- optional . try $ do
        sep <- datesepchar
        m <- decimal
        unless (validMonth m) $ Fail.fail ("Bad month " <> show m)
        option (Left m) . try $ Right <$> do
            _ <- char sep
            d <- decimal
            maybe (Fail.fail $ showBadDate y m d) return $ fromGregorianValid y m d
    return $ case emd of
        Nothing          -> SmartAssumeStart y Nothing
        Just (Left m)    -> SmartAssumeStart y (Just m)
        Just (Right day) -> SmartCompleteDate day

md :: TextParser m (Month, MonthDay)
md = do
  m <- decimal
  datesepchar
  d <- decimal
  _ <- failIfInvalidDate $ SmartFromReference (Just m) d
  return (m, d)

-- | Parse a year number from a Text, making sure that at least four digits are
-- used.
yearp :: TextParser m Integer
yearp = do
  year <- takeWhile1P (Just "year") isDigit
  unless (T.length year >= 4) . Fail.fail $ "Year must contain at least 4 digits: " <> T.unpack year
  return $ readDecimal year

-- These are compared case insensitively, and should all be kept lower case.
months         = ["january","february","march","april","may","june",
                  "july","august","september","october","november","december"]
monthabbrevs   = ["jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec"]
weekdays       = ["monday","tuesday","wednesday","thursday","friday","saturday","sunday"]
weekdayabbrevs = ["mon","tue","wed","thu","fri","sat","sun"]

month, mon :: TextParser m Month
month = choice $ zipWith (\i m -> i <$ string' m) [1..12] months
mon   = choice $ zipWith (\i m -> i <$ string' m) [1..12] monthabbrevs

weekday :: TextParser m Int
weekday = do
  wday <- T.toLower <$> (choice . map string' $ weekdays ++ weekdayabbrevs)
  case catMaybes $ [wday `elemIndex` weekdays, wday `elemIndex` weekdayabbrevs] of
    (i:_) -> return (i+1)
    []    -> Fail.fail $ "weekday: should not happen: attempted to find " <>
                         show wday <> " in " <> show (weekdays ++ weekdayabbrevs)

weekdaysp :: TextParser m [Int]
weekdaysp = fmap head . group . sort <$> sepBy1 weekday (string' ",")

-- | Parse a period expression, specifying a date span and optionally
-- a reporting interval. Requires a reference "today" date for
-- resolving any relative start/end dates (only; it is not needed for
-- parsing the reporting interval).
--
-- >>> let p = parsePeriodExpr (fromGregorian 2008 11 26)
-- >>> p "from Aug to Oct"
-- Right (NoInterval,DateSpan 2008-08-01..2008-09-30)
-- >>> p "aug to oct"
-- Right (NoInterval,DateSpan 2008-08-01..2008-09-30)
-- >>> p "2009q2"
-- Right (NoInterval,DateSpan 2009Q2)
-- >>> p "Q3"
-- Right (NoInterval,DateSpan 2008Q3)
-- >>> p "every 3 days in Aug"
-- Right (Days 3,DateSpan 2008-08)
-- >>> p "daily from aug"
-- Right (Days 1,DateSpan 2008-08-01..)
-- >>> p "every week to 2009"
-- Right (Weeks 1,DateSpan ..2008-12-31)
-- >>> p "every 2nd day of month"
-- Right (DayOfMonth 2,DateSpan ..)
-- >>> p "every 2nd day"
-- Right (DayOfMonth 2,DateSpan ..)
-- >>> p "every 2nd day 2009.."
-- Right (DayOfMonth 2,DateSpan 2009-01-01..)
-- >>> p "every 2nd day 2009-"
-- Right (DayOfMonth 2,DateSpan 2009-01-01..)
-- >>> p "every 29th Nov"
-- Right (DayOfYear 11 29,DateSpan ..)
-- >>> p "every 29th nov ..2009"
-- Right (DayOfYear 11 29,DateSpan ..2008-12-31)
-- >>> p "every nov 29th"
-- Right (DayOfYear 11 29,DateSpan ..)
-- >>> p "every Nov 29th 2009.."
-- Right (DayOfYear 11 29,DateSpan 2009-01-01..)
-- >>> p "every 11/29 from 2009"
-- Right (DayOfYear 11 29,DateSpan 2009-01-01..)
-- >>> p "every 2nd Thursday of month to 2009"
-- Right (WeekdayOfMonth 2 4,DateSpan ..2008-12-31)
-- >>> p "every 1st monday of month to 2009"
-- Right (WeekdayOfMonth 1 1,DateSpan ..2008-12-31)
-- >>> p "every tue"
-- Right (DaysOfWeek [2],DateSpan ..)
-- >>> p "every 2nd day of week"
-- Right (DaysOfWeek [2],DateSpan ..)
-- >>> p "every 2nd day of month"
-- Right (DayOfMonth 2,DateSpan ..)
-- >>> p "every 2nd day"
-- Right (DayOfMonth 2,DateSpan ..)
-- >>> p "every 2nd day 2009.."
-- Right (DayOfMonth 2,DateSpan 2009-01-01..)
-- >>> p "every 2nd day of month 2009.."
-- Right (DayOfMonth 2,DateSpan 2009-01-01..)
periodexprp :: Day -> TextParser m (Interval, DateSpan)
periodexprp rdate = do
  skipNonNewlineSpaces
  choice' [ intervalanddateperiodexprp rdate
          , (,) NoInterval <$> periodexprdatespanp rdate
          ]

-- Parse a reporting interval and a date span.
intervalanddateperiodexprp :: Day -> TextParser m (Interval, DateSpan)
intervalanddateperiodexprp rdate = do
  i <- reportingintervalp
  s <- option def . try $ do
      skipNonNewlineSpaces
      periodexprdatespanp rdate
  return (i,s)

-- Parse a reporting interval.
reportingintervalp :: TextParser m Interval
reportingintervalp = choice'
    [ tryinterval "day"     "daily"     Days
    , tryinterval "month"   "monthly"   Months
    , tryinterval "quarter" "quarterly" Quarters
    , tryinterval "year"    "yearly"    Years
    , Weeks 2 <$ string' "biweekly"
    , Weeks 2 <$ string' "fortnightly"
    , Months 2 <$ string' "bimonthly"
    , string' "every" *> skipNonNewlineSpaces *> choice'
        [ DaysOfWeek . pure <$> (nth <* skipNonNewlineSpaces <* string' "day" <* of_ "week")
        , DayOfMonth <$> (nth <* skipNonNewlineSpaces <* string' "day" <* optOf_ "month")
        , liftA2 WeekdayOfMonth nth $ skipNonNewlineSpaces *> weekday <* optOf_ "month"
        , uncurry DayOfYear <$> (md <* optOf_ "year")
        , DaysOfWeek <$> weekdaysp
        , DaysOfWeek [1..5] <$ string' "weekday"
        , DaysOfWeek [6..7] <$ string' "weekendday"
        , d_o_y <* optOf_ "year"
        ]
    -- NB: the ordering is important here since the parse for `every weekday`
    -- would match the `tryinterval` first and then error on `d`. Perhaps it
    -- would be clearer to factor some of this into the `every` choice or other
    -- left-factorings.
    , tryinterval "week"    "weekly"    Weeks
    ]
  where
    of_ period =
      skipNonNewlineSpaces *> string' "of" *> skipNonNewlineSpaces *> string' period

    optOf_ period = optional . try $ of_ period

    nth = decimal <* choice (map string' ["st","nd","rd","th"])
    d_o_y = runPermutation $ liftA2 DayOfYear (toPermutation $ (month <|> mon) <* skipNonNewlineSpaces)
                                              (toPermutation $ nth <* skipNonNewlineSpaces)

    -- Parse any of several variants of a basic interval, eg "daily", "every day", "every N days".
    tryinterval :: Text -> Text -> (Int -> Interval) -> TextParser m Interval
    tryinterval singular compact intcons = intcons <$> choice'
        [ 1 <$ string' compact
        , string' "every" *> skipNonNewlineSpaces *> choice
            [ 1 <$ string' singular
            , decimal <* skipNonNewlineSpaces <* string' (singular <> "s")
            ]
        ]

periodexprdatespanp :: Day -> TextParser m DateSpan
periodexprdatespanp rdate = choice $ map try [
                            doubledatespanp rdate,
                            quarterdatespanp rdate,
                            fromdatespanp rdate,
                            todatespanp rdate,
                            justdatespanp rdate
                           ]

-- |
-- >>> parsewith (doubledatespanp (fromGregorian 2018 01 01) <* eof) "20180101-201804"
-- Right DateSpan 2018Q1
-- >>> parsewith (doubledatespanp (fromGregorian 2018 01 01) <* eof) "2017..2018"
-- Right DateSpan 2017
-- >>> parsewith (doubledatespanp (fromGregorian 2018 01 01) <* eof) "2017-2018"
-- Right DateSpan 2017
-- >>> parsewith (doubledatespanp (fromGregorian 2018 01 01) <* eof) "2017-01-2018"
-- Right DateSpan 2017
-- >>> parsewith (doubledatespanp (fromGregorian 2018 01 01) <* eof) "2017-01-01-2018"
-- Right DateSpan 2017
doubledatespanp :: Day -> TextParser m DateSpan
doubledatespanp rdate = liftA2 fromToSpan
    (optional (string' "from" *> skipNonNewlineSpaces) *> smartdate)
    (skipNonNewlineSpaces *> choice [string' "to", string "..", string "-"]
    *> skipNonNewlineSpaces *> smartdate)
  where
    fromToSpan = DateSpan `on` (Just . fixSmartDate rdate)

-- |
-- >>> parsewith (quarterdatespanp (fromGregorian 2018 01 01) <* eof) "q1"
-- Right DateSpan 2018Q1
-- >>> parsewith (quarterdatespanp (fromGregorian 2018 01 01) <* eof) "Q1"
-- Right DateSpan 2018Q1
-- >>> parsewith (quarterdatespanp (fromGregorian 2018 01 01) <* eof) "2020q4"
-- Right DateSpan 2020Q4
quarterdatespanp :: Day -> TextParser m DateSpan
quarterdatespanp rdate = do
    y <- yearp <|> pure (first3 $ toGregorian rdate)
    q <- char' 'q' *> satisfy is4Digit
    return . periodAsDateSpan $ QuarterPeriod y (digitToInt q)
  where
    is4Digit c = (fromIntegral (ord c - ord '1') :: Word) <= 3

fromdatespanp :: Day -> TextParser m DateSpan
fromdatespanp rdate = fromSpan <$> choice
    [ string' "from" *> skipNonNewlineSpaces *> smartdate
    , smartdate <* choice [string "..", string "-"]
    ]
  where
    fromSpan b = DateSpan (Just $ fixSmartDate rdate b) Nothing

todatespanp :: Day -> TextParser m DateSpan
todatespanp rdate =
    choice [string' "to", string' "until", string "..", string "-"]
    *> skipNonNewlineSpaces
    *> (DateSpan Nothing . Just . fixSmartDate rdate <$> smartdate)

justdatespanp :: Day -> TextParser m DateSpan
justdatespanp rdate =
    optional (string' "in" *> skipNonNewlineSpaces)
    *> (spanFromSmartDate rdate <$> smartdate)

nulldatespan :: DateSpan
nulldatespan = DateSpan Nothing Nothing

-- | A datespan of zero length, that matches no date.
emptydatespan :: DateSpan
emptydatespan = DateSpan (Just $ addDays 1 nulldate) (Just nulldate)

nulldate :: Day
nulldate = fromGregorian 0 1 1


-- tests

tests_Dates = testGroup "Dates"
  [ testCase "weekday" $ do
      splitSpan (DaysOfWeek [1..5]) (DateSpan (Just $ fromGregorian 2021 07 01) (Just $ fromGregorian 2021 07 08))
        @?= [ (DateSpan (Just $ fromGregorian 2021 06 28) (Just $ fromGregorian 2021 06 29))
            , (DateSpan (Just $ fromGregorian 2021 06 29) (Just $ fromGregorian 2021 06 30))
            , (DateSpan (Just $ fromGregorian 2021 06 30) (Just $ fromGregorian 2021 07 01))
            , (DateSpan (Just $ fromGregorian 2021 07 01) (Just $ fromGregorian 2021 07 02))
            , (DateSpan (Just $ fromGregorian 2021 07 02) (Just $ fromGregorian 2021 07 05))
            -- next week
            , (DateSpan (Just $ fromGregorian 2021 07 05) (Just $ fromGregorian 2021 07 06))
            , (DateSpan (Just $ fromGregorian 2021 07 06) (Just $ fromGregorian 2021 07 07))
            , (DateSpan (Just $ fromGregorian 2021 07 07) (Just $ fromGregorian 2021 07 08))
            ]

      splitSpan (DaysOfWeek [1, 5]) (DateSpan (Just $ fromGregorian 2021 07 01) (Just $ fromGregorian 2021 07 08))
        @?= [ (DateSpan (Just $ fromGregorian 2021 06 28) (Just $ fromGregorian 2021 07 02))
            , (DateSpan (Just $ fromGregorian 2021 07 02) (Just $ fromGregorian 2021 07 05))
            -- next week
            , (DateSpan (Just $ fromGregorian 2021 07 05) (Just $ fromGregorian 2021 07 09))
            ]

  , testCase "match dayOfWeek" $ do
      let dayofweek n s = splitspan (nthdayofweekcontaining n) (\w -> (if w == 0 then id else applyN (n-1) nextday . applyN (fromInteger w) nextweek)) 1 s
          match ds day = splitSpan (DaysOfWeek [day]) ds @?= dayofweek day ds
          ys2021 = fromGregorian 2021 01 01
          ye2021 = fromGregorian 2021 12 31
          ys2022 = fromGregorian 2022 01 01
      mapM_ (match (DateSpan (Just ys2021) (Just ye2021))) [1..7]
      mapM_ (match (DateSpan (Just ys2021) (Just ys2022))) [1..7]
      mapM_ (match (DateSpan (Just ye2021) (Just ys2022))) [1..7]

      mapM_ (match (DateSpan (Just ye2021) Nothing)) [1..7]
      mapM_ (match (DateSpan (Just ys2022) Nothing)) [1..7]

      mapM_ (match (DateSpan Nothing (Just ye2021))) [1..7]
      mapM_ (match (DateSpan Nothing (Just ys2022))) [1..7]

  ]
