{-|

Types for Dates and DateTimes, implemented in terms of UTCTime

-}

module Ledger.Dates
--(
--     Date,                    
--     DateTime,
--     mkDate,
--     mkDateTime,
--     parsedatetime,
--     parsedate,
--     datetimeToDate,
--     elapsedSeconds,
--     today
--    ) 
where

import Data.Time.Clock
import Data.Time.Format
import Data.Time.Calendar
import Data.Time.LocalTime
import System.Locale (defaultTimeLocale)
import Text.Printf
import Data.Maybe

newtype Date = Date UTCTime
    deriving (Ord, Eq)

newtype DateTime = DateTime UTCTime
    deriving (Ord, Eq)

instance Show Date where
   show (Date t) = formatTime defaultTimeLocale "%Y/%m/%d" t

instance Show DateTime where 
   show (DateTime t) = formatTime defaultTimeLocale "%Y/%m/%d %H:%M:%S" t

mkDate :: Day -> Date
mkDate day = Date (localTimeToUTC utc (LocalTime day midnight))

mkDateTime :: Day -> TimeOfDay -> DateTime
mkDateTime day tod = DateTime (localTimeToUTC utc (LocalTime day tod))

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

datetimeToDate :: DateTime -> Date
datetimeToDate (DateTime (UTCTime{utctDay=day})) = Date (UTCTime day 0)

elapsedSeconds :: Fractional a => DateTime -> DateTime -> a
elapsedSeconds (DateTime dt1) (DateTime dt2) = realToFrac $ diffUTCTime dt1 dt2

today :: IO Date
today = getCurrentTime >>= return . Date

dateToUTC :: Date -> UTCTime
dateToUTC (Date u) = u

dateComponents :: Date -> (Integer,Int,Int)
dateComponents = toGregorian . utctDay . dateToUTC

-- dateDay :: Date -> Day
dateDay date = d where (_,_,d) = dateComponents date

-- dateMonth :: Date -> Day
dateMonth date = m where (_,m,_) = dateComponents date