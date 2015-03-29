{-# LANGUAGE CPP #-}
{-|

A 'TimeLogEntry' is a clock-in, clock-out, or other directive in a timelog
file (see timeclock.el or the command-line version). These can be
converted to 'Transactions' and queried like a ledger.

-}

module Hledger.Data.TimeLog
where
import Data.Maybe
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

instance Show TimeLogEntry where
    show t = printf "%s %s %s" (show $ tlcode t) (show $ tldatetime t) (tlcomment t)

instance Show TimeLogCode where
    show SetBalance = "b"
    show SetRequiredHours = "h"
    show In = "i"
    show Out = "o"
    show FinalOut = "O"

instance Read TimeLogCode where
    readsPrec _ ('b' : xs) = [(SetBalance, xs)]
    readsPrec _ ('h' : xs) = [(SetRequiredHours, xs)]
    readsPrec _ ('i' : xs) = [(In, xs)]
    readsPrec _ ('o' : xs) = [(Out, xs)]
    readsPrec _ ('O' : xs) = [(FinalOut, xs)]
    readsPrec _ _ = []

-- | Convert time log entries to journal transactions. When there is no
-- clockout, add one with the provided current time. Sessions crossing
-- midnight are split into days to give accurate per-day totals.
timeLogEntriesToTransactions :: LocalTime -> [TimeLogEntry] -> [Transaction]
timeLogEntriesToTransactions _ [] = []
timeLogEntriesToTransactions now [i]
    | odate > idate = entryFromTimeLogInOut i o' : timeLogEntriesToTransactions now [i',o]
    | otherwise = [entryFromTimeLogInOut i o]
    where
      o = TimeLogEntry (tlsourcepos i) Out end ""
      end = if itime > now then itime else now
      (itime,otime) = (tldatetime i,tldatetime o)
      (idate,odate) = (localDay itime,localDay otime)
      o' = o{tldatetime=itime{localDay=idate, localTimeOfDay=TimeOfDay 23 59 59}}
      i' = i{tldatetime=itime{localDay=addDays 1 idate, localTimeOfDay=midnight}}
timeLogEntriesToTransactions now (i:o:rest)
    | odate > idate = entryFromTimeLogInOut i o' : timeLogEntriesToTransactions now (i':o:rest)
    | otherwise = entryFromTimeLogInOut i o : timeLogEntriesToTransactions now rest
    where
      (itime,otime) = (tldatetime i,tldatetime o)
      (idate,odate) = (localDay itime,localDay otime)
      o' = o{tldatetime=itime{localDay=idate, localTimeOfDay=TimeOfDay 23 59 59}}
      i' = i{tldatetime=itime{localDay=addDays 1 idate, localTimeOfDay=midnight}}

-- | Convert a timelog clockin and clockout entry to an equivalent journal
-- transaction, representing the time expenditure. Note this entry is  not balanced,
-- since we omit the \"assets:time\" transaction for simpler output.
entryFromTimeLogInOut :: TimeLogEntry -> TimeLogEntry -> Transaction
entryFromTimeLogInOut i o
    | otime >= itime = t
    | otherwise =
        error' $ "clock-out time less than clock-in time in:\n" ++ showTransaction t
    where
      t = Transaction {
            tsourcepos   = tlsourcepos i,
            tdate        = idate,
            tdate2       = Nothing,
            tstatus      = True,
            tcode        = "",
            tdescription = showtime itod ++ "-" ++ showtime otod,
            tcomment     = "",
            ttags        = [],
            tpostings    = ps,
            tpreceding_comment_lines=""
          }
      showtime = take 5 . show
      acctname = tlcomment i
      itime    = tldatetime i
      otime    = tldatetime o
      itod     = localTimeOfDay itime
      otod     = localTimeOfDay otime
      idate    = localDay itime
      hours    = elapsedSeconds (toutc otime) (toutc itime) / 3600 where toutc = localTimeToUTC utc
      amount   = Mixed [hrs hours]
      ps       = [posting{paccount=acctname, pamount=amount, ptype=VirtualPosting, ptransaction=Just t}]


tests_Hledger_Data_TimeLog = TestList [

   "timeLogEntriesToTransactions" ~: do
     today <- getCurrentDay
     now' <- getCurrentTime
     tz <- getCurrentTimeZone
     let now = utcToLocalTime tz now'
         nowstr = showtime now
         yesterday = prevday today
         clockin = TimeLogEntry nullsourcepos In
         mktime d = LocalTime d . fromMaybe midnight .
#if MIN_VERSION_time(1,5,0)
                    parseTimeM True defaultTimeLocale "%H:%M:%S"
#else
                    parseTime defaultTimeLocale "%H:%M:%S"
#endif
         showtime = formatTime defaultTimeLocale "%H:%M"
         assertEntriesGiveStrings name es ss = assertEqual name ss (map tdescription $ timeLogEntriesToTransactions now es)

     assertEntriesGiveStrings "started yesterday, split session at midnight"
                                  [clockin (mktime yesterday "23:00:00") ""]
                                  ["23:00-23:59","00:00-"++nowstr]
     assertEntriesGiveStrings "split multi-day sessions at each midnight"
                                  [clockin (mktime (addDays (-2) today) "23:00:00") ""]
                                  ["23:00-23:59","00:00-23:59","00:00-"++nowstr]
     assertEntriesGiveStrings "auto-clock-out if needed"
                                  [clockin (mktime today "00:00:00") ""]
                                  ["00:00-"++nowstr]
     let future = utcToLocalTime tz $ addUTCTime 100 now'
         futurestr = showtime future
     assertEntriesGiveStrings "use the clockin time for auto-clockout if it's in the future"
                                  [clockin future ""]
                                  [printf "%s-%s" futurestr futurestr]

 ]
