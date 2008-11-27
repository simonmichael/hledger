{-|

'Date' and 'DateTime' are a helper layer on top of the standard UTCTime,
Day etc.

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
import Data.Time.LocalTime
import System.Locale (defaultTimeLocale)
import Text.Printf
import Data.Maybe
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Char
import Text.ParserCombinators.Parsec.Combinator
import Ledger.Types
import Ledger.Utils


instance Show Date where
   show (Date t) = formatTime defaultTimeLocale "%Y/%m/%d" t

instance Show DateTime where 
   show (DateTime t) = formatTime defaultTimeLocale "%Y/%m/%d %H:%M:%S" t

mkDate :: Day -> Date
mkDate day = Date (localTimeToUTC utc (LocalTime day midnight))

mkDateTime :: Day -> TimeOfDay -> DateTime
mkDateTime day tod = DateTime (localTimeToUTC utc (LocalTime day tod))

today :: IO Date
today = do
    t <- getZonedTime
    return (mkDate (localDay (zonedTimeToLocalTime t)))

now :: IO DateTime
now = fmap DateTime getCurrentTime 

datetimeToDate :: DateTime -> Date
datetimeToDate (DateTime (UTCTime{utctDay=day})) = Date (UTCTime day 0)

elapsedSeconds :: Fractional a => DateTime -> DateTime -> a
elapsedSeconds (DateTime dt1) (DateTime dt2) = realToFrac $ diffUTCTime dt1 dt2

dateToUTC :: Date -> UTCTime
dateToUTC (Date u) = u

dateComponents :: Date -> (Integer,Int,Int)
dateComponents = toGregorian . utctDay . dateToUTC

-- dateDay :: Date -> Day
dateDay date = d where (_,_,d) = dateComponents date

-- dateMonth :: Date -> Day
dateMonth date = m where (_,m,_) = dateComponents date

-- | Convert a fuzzy date string to an explicit yyyy/mm/dd string using
-- the provided date as reference point.
fixSmartDateStr :: Date -> String -> String
fixSmartDateStr t s = printf "%04d/%02d/%02d" y m d
    where
      pdate = fromparse $ parsewith smartdate $ map toLower s
      (y,m,d) = dateComponents $ fixSmartDate t pdate

-- | Convert a SmartDate to an absolute date using the provided date as
-- reference point.
fixSmartDate :: Date -> SmartDate -> Date
fixSmartDate refdate sdate = mkDate $ fromGregorian y m d
    where
      (y,m,d) = fix sdate
      fix :: SmartDate -> (Integer,Int,Int)
      fix ("","","today")     = (ry, rm, rd)
      fix ("","this","day")   = (ry, rm, rd)
      fix ("","","yesterday") = dateComponents $ lastday refdate
      fix ("","last","day")   = dateComponents $ lastday refdate
      fix ("","","tomorrow")  = dateComponents $ nextday refdate
      fix ("","next","day")   = dateComponents $ nextday refdate
      fix ("","last","week")  = dateComponents $ lastweek refdate
      fix ("","this","week")  = dateComponents $ thisweek refdate
      fix ("","next","week")  = dateComponents $ nextweek refdate
      fix ("","",d)           = (ry, rm, read d)
      fix ("",m,d)            = (ry, read m, read d)
      fix (y,m,d)             = (read y, read m, read d)
      (ry,rm,rd) = dateComponents refdate

lastday, nextday :: Date -> Date
lastday = mkDate . (addDays (-1)) . utctDay . dateToUTC
nextday = mkDate . (addDays 1) . utctDay . dateToUTC
lastweek = mkDate . (addDays (-7)) . utctDay . dateToUTC
thisweek = mkDate . (addDays 0) . utctDay . dateToUTC
nextweek = mkDate . (addDays 7) . utctDay . dateToUTC

----------------------------------------------------------------------
-- parsing

-- | Parse a date-time string to a time type, or raise an error.
parsedatetime :: String -> DateTime
parsedatetime s = DateTime $
    parsetimewith "%Y/%m/%d %H:%M:%S" s $
    error $ printf "could not parse timestamp \"%s\"" s

-- | Parse a date string to a time type, or raise an error.
parsedate :: String -> Date
parsedate s =  Date $
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
  let dateparsers = [ymd, ym, md, y, d, month, mon, today', yesterday, tomorrow
                     -- lastthisnextthing
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
  many1 spacenonewline
  p <- choice [
        string "day"
       ,string "week"
       ,string "month"
       ,string "quarter"
       ,string "year"
      ]
  return ("",r,p)

