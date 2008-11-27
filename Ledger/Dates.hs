{-|

A 'SmartDate' is a date which may be partially-specified or relative.
Eg 2008/12/31, but also 2008/12, 12/31, tomorrow, last week, next year.
We represent these as a triple of strings like ("2008","12",""),
("","","tomorrow"), ("","last","week").

-}

module Ledger.Dates
where

import Data.Time.Clock
import Data.Time.Format
import Data.Time.Calendar
import Data.Time.Calendar.MonthDay
import Data.Time.Calendar.OrdinalDate
import Data.Time.Calendar.WeekDate
import Data.Time.LocalTime
import System.Locale (defaultTimeLocale)
import Text.Printf
import Data.Maybe
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Char
import Text.ParserCombinators.Parsec.Combinator
import Ledger.Types
import Ledger.Utils


showDate :: Day -> String
showDate d = formatTime defaultTimeLocale "%Y/%m/%d" d

mkUTCTime :: Day -> TimeOfDay -> UTCTime
mkUTCTime day tod = localTimeToUTC utc (LocalTime day tod)

today :: IO Day
today = do
    t <- getZonedTime
    return $ localDay (zonedTimeToLocalTime t)

now :: IO UTCTime
now = getCurrentTime 

elapsedSeconds :: Fractional a => UTCTime -> UTCTime -> a
elapsedSeconds t1 t2 = realToFrac $ diffUTCTime t1 t2

dayToUTC :: Day -> UTCTime
dayToUTC d = localTimeToUTC utc (LocalTime d midnight)

-- | Convert a fuzzy date string to an explicit yyyy/mm/dd string using
-- the provided date as reference point.
fixSmartDateStr :: Day -> String -> String
fixSmartDateStr t s = printf "%04d/%02d/%02d" y m d
    where
      (y,m,d) = toGregorian $ fixSmartDate t sdate
      sdate = fromparse $ parsewith smartdate $ map toLower s

-- | Convert a SmartDate to an absolute date using the provided date as
-- reference point.
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
      fix ("",m,d)              = fromGregorian ry (read m) (read d)
      fix (y,m,d)               = fromGregorian (read y) (read m) (read d)
      (ry,rm,rd) = toGregorian refdate

prevday :: Day -> Day
prevday = addDays (-1)
nextday = addDays 1

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

-- | Parse a date-time string to a time type, or raise an error.
parsedatetime :: String -> UTCTime
parsedatetime s = 
    parsetimewith "%Y/%m/%d %H:%M:%S" s $
    error $ printf "could not parse timestamp \"%s\"" s

-- | Parse a date string to a time type, or raise an error.
parsedate :: String -> Day
parsedate s =  
    parsetimewith "%Y/%m/%d" s $
    error $ printf "could not parse date \"%s\"" s

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
smartdate :: Parser SmartDate
smartdate = do
  let dateparsers = [ymd, ym, md, y, d, month, mon, today', yesterday, tomorrow,
                     lastthisnextthing
                    ]
  (y,m,d) <- choice $ map try dateparsers
  return $ (y,m,d)

datesepchar = oneOf "/-."

ymd :: Parser SmartDate
ymd = do
  y <- many1 digit
  datesepchar
  m <- many1 digit
  guard (read m <= 12)
  datesepchar
  d <- many1 digit
  guard (read d <= 31)
  return (y,m,d)

ym :: Parser SmartDate
ym = do
  y <- many1 digit
  guard (read y > 12)
  datesepchar
  m <- many1 digit
  guard (read m <= 12)
  return (y,m,"1")

y :: Parser SmartDate
y = do
  y <- many1 digit
  guard (read y >= 1000)
  return (y,"1","1")

d :: Parser SmartDate
d = do
  d <- many1 digit
  guard (read d <= 31)
  return ("","",d)

md :: Parser SmartDate
md = do
  m <- many1 digit
  guard (read m <= 12)
  datesepchar
  d <- many1 digit
  guard (read d <= 31)
  return ("",m,d)

months = ["january","february","march","april","may","june",
          "july","august","september","october","november","december"]

mons = ["jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec"]

month :: Parser SmartDate
month = do
  m <- choice $ map string months
  let i = maybe 0 (+1) $ (map toLower m) `elemIndex` months
  return ("",show i,"1")

mon :: Parser SmartDate
mon = do
  m <- choice $ map string mons
  let i = maybe 0 (+1) $ (map toLower m) `elemIndex` mons
  return ("",show i,"1")

today',yesterday,tomorrow :: Parser SmartDate
today'    = string "today"     >> return ("","","today")
yesterday = string "yesterday" >> return ("","","yesterday")
tomorrow  = string "tomorrow"  >> return ("","","tomorrow")

lastthisnextthing :: Parser SmartDate
lastthisnextthing = do
  r <- choice [
        string "last"
       ,string "this"
       ,string "next"
      ]
  --many1 spacenonewline
  many spacenonewline  -- allow the space to be omitted for easier scripting
  p <- choice [
        string "day"
       ,string "week"
       ,string "month"
       ,string "quarter"
       ,string "year"
      ]
  return ("",r,p)

