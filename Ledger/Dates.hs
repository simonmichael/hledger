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

An 'Interval' is ledger's "reporting interval" - weekly, monthly,
quarterly, etc.

-}

module Ledger.Dates
where

import Data.Time.Format
import Data.Time.Calendar.OrdinalDate
import Locale (defaultTimeLocale)
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Char
import Text.ParserCombinators.Parsec.Combinator
import Ledger.Types
import Ledger.Utils


showDate :: Day -> String
showDate = formatTime defaultTimeLocale "%Y/%m/%d"

getCurrentDay :: IO Day
getCurrentDay = do
    t <- getZonedTime
    return $ localDay (zonedTimeToLocalTime t)

elapsedSeconds :: Fractional a => UTCTime -> UTCTime -> a
elapsedSeconds t1 = realToFrac . diffUTCTime t1

-- | Split a DateSpan into one or more consecutive spans at the specified interval.
splitSpan :: Interval -> DateSpan -> [DateSpan]
splitSpan _ (DateSpan Nothing Nothing) = [DateSpan Nothing Nothing]
splitSpan NoInterval s = [s]
splitSpan Daily s      = splitspan startofday     nextday     s
splitSpan Weekly s     = splitspan startofweek    nextweek    s
splitSpan Monthly s    = splitspan startofmonth   nextmonth   s
splitSpan Quarterly s  = splitspan startofquarter nextquarter s
splitSpan Yearly s     = splitspan startofyear    nextyear    s

splitspan :: (Day -> Day) -> (Day -> Day) -> DateSpan -> [DateSpan]
splitspan _ _ (DateSpan Nothing Nothing) = []
splitspan start next (DateSpan Nothing (Just e)) = [DateSpan (Just $ start e) (Just $ next $ start e)]
splitspan start next (DateSpan (Just b) Nothing) = [DateSpan (Just $ start b) (Just $ next $ start b)]
splitspan start next span@(DateSpan (Just b) (Just e))
    | b == e = [span]
    | otherwise = splitspan' start next span
    where
      splitspan' start next (DateSpan (Just b) (Just e))
          | b >= e = []
          | otherwise = DateSpan (Just s) (Just n)
                        : splitspan' start next (DateSpan (Just n) (Just e))
          where s = start b
                n = next s
      splitspan' _ _ _ = error "won't happen, avoids warnings"

-- | Count the days in a DateSpan, or if it is open-ended return Nothing.
daysInSpan :: DateSpan -> Maybe Integer
daysInSpan (DateSpan (Just d1) (Just d2)) = Just $ diffDays d2 d1
daysInSpan _ = Nothing
    
-- | Parse a period expression to an Interval and overall DateSpan using
-- the provided reference date.
parsePeriodExpr :: Day -> String -> (Interval, DateSpan)
parsePeriodExpr refdate expr = (interval,span)
    where (interval,span) = fromparse $ parsewith (periodexpr refdate) expr
    
-- | Convert a single smart date string to a date span using the provided
-- reference date.
spanFromSmartDateString :: Day -> String -> DateSpan
spanFromSmartDateString refdate s = spanFromSmartDate refdate sdate
    where
      sdate = fromparse $ parsewith smartdate s

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

-- | Convert a smart date string to an explicit yyyy\/mm\/dd string using
-- the provided reference date.
fixSmartDateStr :: Day -> String -> String
fixSmartDateStr t s = printf "%04d/%02d/%02d" y m d
    where
      (y,m,d) = toGregorian $ fixSmartDate t sdate
      sdate = fromparse $ parsewith smartdate $ lowercase s

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

----------------------------------------------------------------------
-- parsing

firstJust ms = case dropWhile (==Nothing) ms of
    [] -> Nothing
    (md:_) -> md

parsedatetimeM :: String -> Maybe LocalTime
parsedatetimeM s = firstJust [
    parseTime defaultTimeLocale "%Y/%m/%d %H:%M:%S" s,
    parseTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" s
    ]

-- | Parse a date-time string to a time type, or raise an error.
parsedatetime :: String -> LocalTime
parsedatetime s = fromMaybe (error $ "could not parse timestamp \"" ++ s ++ "\"")
                            (parsedatetimeM s)

-- | Parse a date string to a time type, or raise an error.
parsedateM :: String -> Maybe Day
parsedateM s = firstJust [ 
     parseTime defaultTimeLocale "%Y/%m/%d" s,
     parseTime defaultTimeLocale "%Y-%m-%d" s 
     ]

-- | Parse a date string to a time type, or raise an error.
parsedate :: String -> Day
parsedate s =  fromMaybe (error $ "could not parse date \"" ++ s ++ "\"")
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
> (not yet) this/next/last week/day/month/quarter/year

Returns a SmartDate, to be converted to a full date later (see fixSmartDate).
Assumes any text in the parse stream has been lowercased.
-}
smartdate :: GenParser Char st SmartDate
smartdate = do
  let dateparsers = [yyyymmdd, ymd, ym, md, y, d, month, mon, today, yesterday, tomorrow,
                     lastthisnextthing
                    ]
  (y,m,d) <- choice $ map try dateparsers
  return (y,m,d)

datesepchar = oneOf "/-."

yyyymmdd :: GenParser Char st SmartDate
yyyymmdd = do
  y <- count 4 digit
  m <- count 2 digit
  guard (read m <= 12)
  d <- count 2 digit
  guard (read d <= 31)
  return (y,m,d)

ymd :: GenParser Char st SmartDate
ymd = do
  y <- many1 digit
  datesepchar
  m <- try (count 2 digit) <|> count 1 digit
  guard (read m >= 1 && (read m <= 12))
  -- when (read m < 1 || (read m > 12)) $ fail "bad month number specified"
  datesepchar
  d <- try (count 2 digit) <|> count 1 digit
  when (read d < 1 || (read d > 31)) $ fail "bad day number specified"
  return $ (y,m,d)

ym :: GenParser Char st SmartDate
ym = do
  y <- many1 digit
  guard (read y > 12)
  datesepchar
  m <- try (count 2 digit) <|> count 1 digit
  guard (read m >= 1 && (read m <= 12))
  return (y,m,"")

y :: GenParser Char st SmartDate
y = do
  y <- many1 digit
  guard (read y >= 1000)
  return (y,"","")

d :: GenParser Char st SmartDate
d = do
  d <- many1 digit
  guard (read d <= 31)
  return ("","",d)

md :: GenParser Char st SmartDate
md = do
  m <- try (count 2 digit) <|> count 1 digit
  guard (read m >= 1 && (read m <= 12))
  datesepchar
  d <- try (count 2 digit) <|> count 1 digit
  when (read d < 1 || (read d > 31)) $ fail "bad day number specified"
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
  i <- periodexprinterval
  many spacenonewline
  s <- periodexprdatespan rdate
  return (i,s)

intervalperiodexpr :: GenParser Char st (Interval, DateSpan)
intervalperiodexpr = do
  many spacenonewline
  i <- periodexprinterval
  return (i, DateSpan Nothing Nothing)

dateperiodexpr :: Day -> GenParser Char st (Interval, DateSpan)
dateperiodexpr rdate = do
  many spacenonewline
  s <- periodexprdatespan rdate
  return (NoInterval, s)

periodexprinterval :: GenParser Char st Interval
periodexprinterval = 
    choice $ map try [
                tryinterval "day" "daily" Daily,
                tryinterval "week" "weekly" Weekly,
                tryinterval "month" "monthly" Monthly,
                tryinterval "quarter" "quarterly" Quarterly,
                tryinterval "year" "yearly" Yearly
               ]
    where
      tryinterval s1 s2 v = 
          choice [try (string $ "every "++s1), try (string s2)] >> return v

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

nulldatespan = DateSpan Nothing Nothing

mkdatespan b = DateSpan (Just $ parsedate b) . Just . parsedate

nulldate = parsedate "1900/01/01"