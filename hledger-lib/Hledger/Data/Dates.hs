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

module Hledger.Data.Dates
where

import Control.Monad
import Data.List
import Data.Maybe
import Data.Time.Format
import Data.Time.Calendar
import Data.Time.Calendar.OrdinalDate
import Data.Time.Clock
import Data.Time.LocalTime
import Safe (readMay)
import System.Locale (defaultTimeLocale)
import Test.HUnit
import Text.ParserCombinators.Parsec
import Text.Printf

import Hledger.Data.Types
import Hledger.Utils


showDate :: Day -> String
showDate = formatTime defaultTimeLocale "%C%y/%m/%d"

-- | Get the current local date.
getCurrentDay :: IO Day
getCurrentDay = do
    t <- getZonedTime
    return $ localDay (zonedTimeToLocalTime t)

-- | Get the current local year.
getCurrentYear :: IO Integer
getCurrentYear = do
  (y,_,_) <- toGregorian `fmap` getCurrentDay
  return y

elapsedSeconds :: Fractional a => UTCTime -> UTCTime -> a
elapsedSeconds t1 = realToFrac . diffUTCTime t1

-- | Split a DateSpan into one or more consecutive spans at the specified interval.
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
    
-- | Combine two datespans, filling any unspecified dates in the first
-- with dates from the second.
orDatesFrom (DateSpan a1 b1) (DateSpan a2 b2) = DateSpan a b
    where a = if isJust a1 then a1 else a2
          b = if isJust b1 then b1 else b2

-- | Parse a period expression to an Interval and overall DateSpan using
-- the provided reference date, or return a parse error.
parsePeriodExpr :: Day -> String -> Either ParseError (Interval, DateSpan)
parsePeriodExpr refdate = parsewith (periodexpr refdate)

maybePeriod :: Day -> String -> Maybe (Interval,DateSpan)
maybePeriod refdate = either (const Nothing) Just . parsePeriodExpr refdate

-- | Show a DateSpan as a human-readable pseudo-period-expression string.
dateSpanAsText :: DateSpan -> String
dateSpanAsText (DateSpan Nothing Nothing)   = "all"
dateSpanAsText (DateSpan Nothing (Just e))  = printf "to %s" (show e)
dateSpanAsText (DateSpan (Just b) Nothing)  = printf "from %s" (show b)
dateSpanAsText (DateSpan (Just b) (Just e)) = printf "%s to %s" (show b) (show e)
    
-- | Convert a single smart date string to a date span using the provided
-- reference date, or raise an error.
spanFromSmartDateString :: Day -> String -> DateSpan
spanFromSmartDateString refdate s = spanFromSmartDate refdate sdate
    where
      sdate = fromparse $ parsewith smartdateonly s

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

showDay :: Day -> String
showDay day = printf "%04d/%02d/%02d" y m d where (y,m,d) = toGregorian day

-- | Convert a smart date string to an explicit yyyy\/mm\/dd string using
-- the provided reference date, or raise an error.
fixSmartDateStr :: Day -> String -> String
fixSmartDateStr d s = either (\e->error' $ printf "could not parse date %s %s" (show s) (show e)) id $ fixSmartDateStrEither d s

-- | A safe version of fixSmartDateStr.
fixSmartDateStrEither :: Day -> String -> Either ParseError String
fixSmartDateStrEither t s = case parsewith smartdateonly (lowercase s) of
                              Right sd -> Right $ showDay $ fixSmartDate t sd
                              Left e -> Left e

-- | Convert a SmartDate to an absolute date using the provided reference date.
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

firstJust ms = case dropWhile (==Nothing) ms of
    [] -> Nothing
    (md:_) -> md

-- | Parse a couple of date-time string formats to a time type.
parsedatetimeM :: String -> Maybe LocalTime
parsedatetimeM s = firstJust [
    parseTime defaultTimeLocale "%Y/%m/%d %H:%M:%S" s,
    parseTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" s
    ]

-- | Parse a couple of date string formats to a time type.
parsedateM :: String -> Maybe Day
parsedateM s = firstJust [ 
     parseTime defaultTimeLocale "%Y/%m/%d" s,
     parseTime defaultTimeLocale "%Y-%m-%d" s 
     ]

-- | Parse a date-time string to a time type, or raise an error.
parsedatetime :: String -> LocalTime
parsedatetime s = fromMaybe (error' $ "could not parse timestamp \"" ++ s ++ "\"")
                            (parsedatetimeM s)

-- | Parse a date string to a time type, or raise an error.
parsedate :: String -> Day
parsedate s =  fromMaybe (error' $ "could not parse date \"" ++ s ++ "\"")
                         (parsedateM s)

-- | Parse a time string to a time type using the provided pattern, or
-- return the default.
parsetimewith :: ParseTime t => String -> String -> t -> t
parsetimewith pat s def = fromMaybe def $ parseTime defaultTimeLocale pat s

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
smartdate :: GenParser Char st SmartDate
smartdate = do
  -- XXX maybe obscures date errors ? see ledgerdate
  (y,m,d) <- choice' [yyyymmdd, ymd, ym, md, y, d, month, mon, today, yesterday, tomorrow, lastthisnextthing]
  return (y,m,d)

-- | Like smartdate, but there must be nothing other than whitespace after the date.
smartdateonly :: GenParser Char st SmartDate
smartdateonly = do
  d <- smartdate
  many spacenonewline
  eof
  return d

datesepchars = "/-."
datesepchar = oneOf datesepchars

validYear, validMonth, validDay :: String -> Bool
validYear s = length s >= 4 && isJust (readMay s :: Maybe Int)
validMonth s = maybe False (\n -> n>=1 && n<=12) $ readMay s
validDay s = maybe False (\n -> n>=1 && n<=31) $ readMay s

failIfInvalidYear, failIfInvalidMonth, failIfInvalidDay :: (Monad m) => String -> m ()
failIfInvalidYear s  = unless (validYear s)  $ fail $ "bad year number: " ++ s
failIfInvalidMonth s = unless (validMonth s) $ fail $ "bad month number: " ++ s
failIfInvalidDay s   = unless (validDay s)   $ fail $ "bad day number: " ++ s

yyyymmdd :: GenParser Char st SmartDate
yyyymmdd = do
  y <- count 4 digit
  m <- count 2 digit
  failIfInvalidMonth m
  d <- count 2 digit
  failIfInvalidDay d
  return (y,m,d)

ymd :: GenParser Char st SmartDate
ymd = do
  y <- many1 digit
  failIfInvalidYear y
  datesepchar
  m <- many1 digit
  failIfInvalidMonth m
  datesepchar
  d <- many1 digit
  failIfInvalidDay d
  return $ (y,m,d)

ym :: GenParser Char st SmartDate
ym = do
  y <- many1 digit
  failIfInvalidYear y
  datesepchar
  m <- many1 digit
  failIfInvalidMonth m
  return (y,m,"")

y :: GenParser Char st SmartDate
y = do
  y <- many1 digit
  failIfInvalidYear y
  return (y,"","")

d :: GenParser Char st SmartDate
d = do
  d <- many1 digit
  failIfInvalidDay d
  return ("","",d)

md :: GenParser Char st SmartDate
md = do
  m <- many1 digit
  failIfInvalidMonth m
  datesepchar
  d <- many1 digit
  failIfInvalidDay d
  return ("",m,d)

months         = ["january","february","march","april","may","june",
                  "july","august","september","october","november","december"]
monthabbrevs   = ["jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec"]
weekdays       = ["monday","tuesday","wednesday","thursday","friday","saturday","sunday"]
weekdayabbrevs = ["mon","tue","wed","thu","fri","sat","sun"]

monthIndex s = maybe 0 (+1) $ lowercase s `elemIndex` months
monIndex s   = maybe 0 (+1) $ lowercase s `elemIndex` monthabbrevs

month :: GenParser Char st SmartDate
month = do
  m <- choice $ map (try . string) months
  let i = monthIndex m
  return ("",show i,"")

mon :: GenParser Char st SmartDate
mon = do
  m <- choice $ map (try . string) monthabbrevs
  let i = monIndex m
  return ("",show i,"")

today,yesterday,tomorrow :: GenParser Char st SmartDate
today     = string "today"     >> return ("","","today")
yesterday = string "yesterday" >> return ("","","yesterday")
tomorrow  = string "tomorrow"  >> return ("","","tomorrow")

lastthisnextthing :: GenParser Char st SmartDate
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

periodexpr :: Day -> GenParser Char st (Interval, DateSpan)
periodexpr rdate = choice $ map try [
                    intervalanddateperiodexpr rdate,
                    intervalperiodexpr,
                    dateperiodexpr rdate,
                    (return (NoInterval,DateSpan Nothing Nothing))
                   ]

intervalanddateperiodexpr :: Day -> GenParser Char st (Interval, DateSpan)
intervalanddateperiodexpr rdate = do
  many spacenonewline
  i <- reportinginterval
  many spacenonewline
  s <- periodexprdatespan rdate
  return (i,s)

intervalperiodexpr :: GenParser Char st (Interval, DateSpan)
intervalperiodexpr = do
  many spacenonewline
  i <- reportinginterval
  return (i, DateSpan Nothing Nothing)

dateperiodexpr :: Day -> GenParser Char st (Interval, DateSpan)
dateperiodexpr rdate = do
  many spacenonewline
  s <- periodexprdatespan rdate
  return (NoInterval, s)

-- Parse a reporting interval.
reportinginterval :: GenParser Char st Interval
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
                          n <- fmap read $ many1 digit
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
                          n <- fmap read $ many1 digit
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
      tryinterval :: String -> String -> (Int -> Interval) -> GenParser Char st Interval
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
              n <- fmap read $ many1 digit
              many spacenonewline
              string plural
              return $ intcons n
           ]
          where plural = singular ++ "s"

periodexprdatespan :: Day -> GenParser Char st DateSpan
periodexprdatespan rdate = choice $ map try [
                            doubledatespan rdate,
                            fromdatespan rdate,
                            todatespan rdate,
                            justdatespan rdate
                           ]

doubledatespan :: Day -> GenParser Char st DateSpan
doubledatespan rdate = do
  optional (string "from" >> many spacenonewline)
  b <- smartdate
  many spacenonewline
  optional (string "to" >> many spacenonewline)
  e <- smartdate
  return $ DateSpan (Just $ fixSmartDate rdate b) (Just $ fixSmartDate rdate e)

fromdatespan :: Day -> GenParser Char st DateSpan
fromdatespan rdate = do
  string "from" >> many spacenonewline
  b <- smartdate
  return $ DateSpan (Just $ fixSmartDate rdate b) Nothing

todatespan :: Day -> GenParser Char st DateSpan
todatespan rdate = do
  string "to" >> many spacenonewline
  e <- smartdate
  return $ DateSpan Nothing (Just $ fixSmartDate rdate e)

justdatespan :: Day -> GenParser Char st DateSpan
justdatespan rdate = do
  optional (string "in" >> many spacenonewline)
  d <- smartdate
  return $ spanFromSmartDate rdate d

mkdatespan :: String -> String -> DateSpan
mkdatespan b = DateSpan (Just $ parsedate b) . Just . parsedate

nulldatespan = DateSpan Nothing Nothing

nulldate = parsedate "1900/01/01"

tests_Hledger_Data_Dates = TestList
 [

   "parsedate" ~: do
    let date1 = parsedate "2008/11/26"
    parsedate "2008/02/03" `is` parsetimewith "%Y/%m/%d" "2008/02/03" date1
    parsedate "2008-02-03" `is` parsetimewith "%Y/%m/%d" "2008/02/03" date1

  ,"period expressions" ~: do
    let todaysdate = parsedate "2008/11/26"
    let str `gives` result = show (parsewith (periodexpr todaysdate) str) `is` ("Right " ++ result)
    "from aug to oct"           `gives` "(NoInterval,DateSpan (Just 2008-08-01) (Just 2008-10-01))"
    "aug to oct"                `gives` "(NoInterval,DateSpan (Just 2008-08-01) (Just 2008-10-01))"
    "every 3 days in aug"       `gives` "(Days 3,DateSpan (Just 2008-08-01) (Just 2008-09-01))"
    "daily from aug"            `gives` "(Days 1,DateSpan (Just 2008-08-01) Nothing)"
    "every week to 2009"        `gives` "(Weeks 1,DateSpan Nothing (Just 2009-01-01))"

  ,"splitSpan" ~: do
    let gives (interval, span) = (splitSpan interval span `is`)
    (NoInterval,mkdatespan "2008/01/01" "2009/01/01") `gives`
     [mkdatespan "2008/01/01" "2009/01/01"]
    (Quarters 1,mkdatespan "2008/01/01" "2009/01/01") `gives`
     [mkdatespan "2008/01/01" "2008/04/01"
     ,mkdatespan "2008/04/01" "2008/07/01"
     ,mkdatespan "2008/07/01" "2008/10/01"
     ,mkdatespan "2008/10/01" "2009/01/01"
     ]
    (Quarters 1,nulldatespan) `gives`
     [nulldatespan]
    (Days 1,mkdatespan "2008/01/01" "2008/01/01") `gives`
     [mkdatespan "2008/01/01" "2008/01/01"]
    (Quarters 1,mkdatespan "2008/01/01" "2008/01/01") `gives`
     [mkdatespan "2008/01/01" "2008/01/01"]
    (Months 1,mkdatespan "2008/01/01" "2008/04/01") `gives`
     [mkdatespan "2008/01/01" "2008/02/01"
     ,mkdatespan "2008/02/01" "2008/03/01"
     ,mkdatespan "2008/03/01" "2008/04/01"
     ]
    (Months 2,mkdatespan "2008/01/01" "2008/04/01") `gives`
     [mkdatespan "2008/01/01" "2008/03/01"
     ,mkdatespan "2008/03/01" "2008/05/01"
     ]
    (Weeks 1,mkdatespan "2008/01/01" "2008/01/15") `gives`
     [mkdatespan "2007/12/31" "2008/01/07"
     ,mkdatespan "2008/01/07" "2008/01/14"
     ,mkdatespan "2008/01/14" "2008/01/21"
     ]
    (Weeks 2,mkdatespan "2008/01/01" "2008/01/15") `gives`
     [mkdatespan "2007/12/31" "2008/01/14"
     ,mkdatespan "2008/01/14" "2008/01/28"
     ]
    (DayOfMonth 2,mkdatespan "2008/01/01" "2008/04/01") `gives`
     [mkdatespan "2008/01/02" "2008/02/02"
     ,mkdatespan "2008/02/02" "2008/03/02"
     ,mkdatespan "2008/03/02" "2008/04/02"
     ]
    (DayOfWeek 2,mkdatespan "2011/01/01" "2011/01/15") `gives`
     [mkdatespan "2011/01/04" "2011/01/11"
     ,mkdatespan "2011/01/11" "2011/01/18"
     ]

  ,"fixSmartDateStr" ~: do
    let gives = is . fixSmartDateStr (parsedate "2008/11/26")
    "1999-12-02"   `gives` "1999/12/02"
    "1999.12.02"   `gives` "1999/12/02"
    "1999/3/2"     `gives` "1999/03/02"
    "19990302"     `gives` "1999/03/02"
    "2008/2"       `gives` "2008/02/01"
    "0020/2"       `gives` "0020/02/01"
    "1000"         `gives` "1000/01/01"
    "4/2"          `gives` "2008/04/02"
    "2"            `gives` "2008/11/02"
    "January"      `gives` "2008/01/01"
    "feb"          `gives` "2008/02/01"
    "today"        `gives` "2008/11/26"
    "yesterday"    `gives` "2008/11/25"
    "tomorrow"     `gives` "2008/11/27"
    "this day"     `gives` "2008/11/26"
    "last day"     `gives` "2008/11/25"
    "next day"     `gives` "2008/11/27"
    "this week"    `gives` "2008/11/24" -- last monday
    "last week"    `gives` "2008/11/17" -- previous monday
    "next week"    `gives` "2008/12/01" -- next monday
    "this month"   `gives` "2008/11/01"
    "last month"   `gives` "2008/10/01"
    "next month"   `gives` "2008/12/01"
    "this quarter" `gives` "2008/10/01"
    "last quarter" `gives` "2008/07/01"
    "next quarter" `gives` "2009/01/01"
    "this year"    `gives` "2008/01/01"
    "last year"    `gives` "2007/01/01"
    "next year"    `gives` "2009/01/01"
--     "last wed"     `gives` "2008/11/19"
--     "next friday"  `gives` "2008/11/28"
--     "next january" `gives` "2009/01/01"

 ]
