{-|

A 'TimeclockEntry' is a clock-in, clock-out, or other directive in a timeclock
file (see timeclock.el or the command-line version). These can be
converted to 'Transactions' and queried like a ledger.

-}

{-# LANGUAGE CPP, OverloadedStrings #-}

module Hledger.Data.Timeclock
where
import Data.Maybe
-- import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.Format
import Data.Time.LocalTime
#if !(MIN_VERSION_time(1,5,0))
import System.Locale (defaultTimeLocale)
#endif
import Test.HUnit
import Text.Printf

import Hledger.Utils
import Hledger.Data.Types
import Hledger.Data.Dates
import Hledger.Data.Amount
import Hledger.Data.Posting
import Hledger.Data.Transaction

instance Show TimeclockEntry where
    show t = printf "%s %s %s  %s" (show $ tlcode t) (show $ tldatetime t) (tlaccount t) (tldescription t)

instance Show TimeclockCode where
    show SetBalance = "b"
    show SetRequiredHours = "h"
    show In = "i"
    show Out = "o"
    show FinalOut = "O"

instance Read TimeclockCode where
    readsPrec _ ('b' : xs) = [(SetBalance, xs)]
    readsPrec _ ('h' : xs) = [(SetRequiredHours, xs)]
    readsPrec _ ('i' : xs) = [(In, xs)]
    readsPrec _ ('o' : xs) = [(Out, xs)]
    readsPrec _ ('O' : xs) = [(FinalOut, xs)]
    readsPrec _ _ = []

-- | Convert time log entries to journal transactions. When there is no
-- clockout, add one with the provided current time. Sessions crossing
-- midnight are split into days to give accurate per-day totals.
timeclockEntriesToTransactions :: LocalTime -> [TimeclockEntry] -> [Transaction]
timeclockEntriesToTransactions _ [] = []
timeclockEntriesToTransactions now [i]
    | odate > idate = entryFromTimeclockInOut i o' : timeclockEntriesToTransactions now [i',o]
    | otherwise = [entryFromTimeclockInOut i o]
    where
      o = TimeclockEntry (tlsourcepos i) Out end "" ""
      end = if itime > now then itime else now
      (itime,otime) = (tldatetime i,tldatetime o)
      (idate,odate) = (localDay itime,localDay otime)
      o' = o{tldatetime=itime{localDay=idate, localTimeOfDay=TimeOfDay 23 59 59}}
      i' = i{tldatetime=itime{localDay=addDays 1 idate, localTimeOfDay=midnight}}
timeclockEntriesToTransactions now (i:o:rest)
    | odate > idate = entryFromTimeclockInOut i o' : timeclockEntriesToTransactions now (i':o:rest)
    | otherwise = entryFromTimeclockInOut i o : timeclockEntriesToTransactions now rest
    where
      (itime,otime) = (tldatetime i,tldatetime o)
      (idate,odate) = (localDay itime,localDay otime)
      o' = o{tldatetime=itime{localDay=idate, localTimeOfDay=TimeOfDay 23 59 59}}
      i' = i{tldatetime=itime{localDay=addDays 1 idate, localTimeOfDay=midnight}}

-- | Convert a timeclock clockin and clockout entry to an equivalent journal
-- transaction, representing the time expenditure. Note this entry is  not balanced,
-- since we omit the \"assets:time\" transaction for simpler output.
entryFromTimeclockInOut :: TimeclockEntry -> TimeclockEntry -> Transaction
entryFromTimeclockInOut i o
    | otime >= itime = t
    | otherwise =
        error' $ "clock-out time less than clock-in time in:\n" ++ showTransaction t
    where
      t = Transaction {
            tindex       = 0,
            tsourcepos   = tlsourcepos i,
            tdate        = idate,
            tdate2       = Nothing,
            tstatus      = Cleared,
            tcode        = "",
            tdescription = desc,
            tcomment     = "",
            ttags        = [],
            tpostings    = ps,
            tpreceding_comment_lines=""
          }
      itime    = tldatetime i
      otime    = tldatetime o
      itod     = localTimeOfDay itime
      otod     = localTimeOfDay otime
      idate    = localDay itime
      desc     | T.null (tldescription i) = T.pack $ showtime itod ++ "-" ++ showtime otod
               | otherwise                = tldescription i
      showtime = take 5 . show
      hours    = elapsedSeconds (toutc otime) (toutc itime) / 3600 where toutc = localTimeToUTC utc
      acctname = tlaccount i
      amount   = Mixed [hrs hours]
      ps       = [posting{paccount=acctname, pamount=amount, ptype=VirtualPosting, ptransaction=Just t}]


tests_Hledger_Data_Timeclock = TestList [

   "timeclockEntriesToTransactions" ~: do
     today <- getCurrentDay
     now' <- getCurrentTime
     tz <- getCurrentTimeZone
     let now = utcToLocalTime tz now'
         nowstr = showtime now
         yesterday = prevday today
         clockin = TimeclockEntry nullsourcepos In
         mktime d = LocalTime d . fromMaybe midnight .
#if MIN_VERSION_time(1,5,0)
                    parseTimeM True defaultTimeLocale "%H:%M:%S"
#else
                    parseTime defaultTimeLocale "%H:%M:%S"
#endif
         showtime = formatTime defaultTimeLocale "%H:%M"
         assertEntriesGiveStrings name es ss = assertEqual name ss (map (T.unpack . tdescription) $ timeclockEntriesToTransactions now es)

     assertEntriesGiveStrings "started yesterday, split session at midnight"
                                  [clockin (mktime yesterday "23:00:00") "" ""]
                                  ["23:00-23:59","00:00-"++nowstr]
     assertEntriesGiveStrings "split multi-day sessions at each midnight"
                                  [clockin (mktime (addDays (-2) today) "23:00:00") "" ""]
                                  ["23:00-23:59","00:00-23:59","00:00-"++nowstr]
     assertEntriesGiveStrings "auto-clock-out if needed"
                                  [clockin (mktime today "00:00:00") "" ""]
                                  ["00:00-"++nowstr]
     let future = utcToLocalTime tz $ addUTCTime 100 now'
         futurestr = showtime future
     assertEntriesGiveStrings "use the clockin time for auto-clockout if it's in the future"
                                  [clockin future "" ""]
                                  [printf "%s-%s" futurestr futurestr]

 ]
