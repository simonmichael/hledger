{-|

A 'TimeLogEntry' is a clock-in, clock-out, or other directive in a timelog
file (see timeclock.el or the command-line version). These can be
converted to 'LedgerTransactions' and queried like a ledger.

-}

module Ledger.TimeLog
where
import Ledger.Utils
import Ledger.Types
import Ledger.Dates
import Ledger.Commodity
import Ledger.LedgerTransaction

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

-- | Convert time log entries to ledger transactions. When there is no
-- clockout, add one with the provided current time. Sessions crossing
-- midnight are split into days to give accurate per-day totals.
entriesFromTimeLogEntries :: LocalTime -> [TimeLogEntry] -> [LedgerTransaction]
entriesFromTimeLogEntries _ [] = []
entriesFromTimeLogEntries now [i]
    | odate > idate = entryFromTimeLogInOut i o' : entriesFromTimeLogEntries now [i',o]
    | otherwise = [entryFromTimeLogInOut i o]
    where
      o = TimeLogEntry Out end ""
      end = if itime > now then itime else now
      (itime,otime) = (tldatetime i,tldatetime o)
      (idate,odate) = (localDay itime,localDay otime)
      o' = o{tldatetime=itime{localDay=idate, localTimeOfDay=TimeOfDay 23 59 59}}
      i' = i{tldatetime=itime{localDay=addDays 1 idate, localTimeOfDay=midnight}}
entriesFromTimeLogEntries now (i:o:rest)
    | odate > idate = entryFromTimeLogInOut i o' : entriesFromTimeLogEntries now (i':o:rest)
    | otherwise = entryFromTimeLogInOut i o : entriesFromTimeLogEntries now rest
    where
      (itime,otime) = (tldatetime i,tldatetime o)
      (idate,odate) = (localDay itime,localDay otime)
      o' = o{tldatetime=itime{localDay=idate, localTimeOfDay=TimeOfDay 23 59 59}}
      i' = i{tldatetime=itime{localDay=addDays 1 idate, localTimeOfDay=midnight}}

-- | Convert a timelog clockin and clockout entry to an equivalent ledger
-- entry, representing the time expenditure. Note this entry is  not balanced,
-- since we omit the \"assets:time\" transaction for simpler output.
entryFromTimeLogInOut :: TimeLogEntry -> TimeLogEntry -> LedgerTransaction
entryFromTimeLogInOut i o
    | otime >= itime = t
    | otherwise = 
        error $ "clock-out time less than clock-in time in:\n" ++ showLedgerTransaction t
    where
      t = LedgerTransaction {
            ltdate         = idate,
            lteffectivedate = Nothing,
            ltstatus       = True,
            ltcode         = "",
            ltdescription  = showtime itod ++ "-" ++ showtime otod,
            ltcomment      = "",
            ltpostings = ps,
            ltpreceding_comment_lines=""
          }
      showtime = take 5 . show
      acctname = tlcomment i
      itime    = tldatetime i
      otime    = tldatetime o
      itod     = localTimeOfDay itime
      otod     = localTimeOfDay otime
      idate    = localDay itime
      hrs      = elapsedSeconds (toutc otime) (toutc itime) / 3600 where toutc = localTimeToUTC utc
      amount   = Mixed [hours hrs]
      ps       = [Posting False acctname amount "" RegularPosting
                 --,Posting "assets:time" (-amount) "" RegularPosting
                 ]
