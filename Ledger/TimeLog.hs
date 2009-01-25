{-|

A 'TimeLog' is a parsed timelog file (see timeclock.el or the command-line
version) containing zero or more 'TimeLogEntry's. It can be converted to a
'RawLedger' for querying.

-}

module Ledger.TimeLog
where
import Ledger.Utils
import Ledger.Types
import Ledger.Dates
import Ledger.Commodity
import Ledger.Amount
import Ledger.Entry

instance Show TimeLogEntry where 
    show t = printf "%s %s %s" (show $ tlcode t) (show $ tldatetime t) (tlcomment t)

instance Show TimeLog where
    show tl = printf "TimeLog with %d entries" $ length $ timelog_entries tl

-- | Convert time log entries to ledger entries. When there is no clockout,
-- add one with the provided current time.
entriesFromTimeLogEntries :: LocalTime -> [TimeLogEntry] -> [Entry]
entriesFromTimeLogEntries _ [] = []
entriesFromTimeLogEntries t [i] = [entryFromTimeLogInOut i (TimeLogEntry 'o' t "")]
entriesFromTimeLogEntries t (i:o:rest) = [entryFromTimeLogInOut i o] ++ entriesFromTimeLogEntries t rest

-- | Convert a timelog clockin and clockout entry to an equivalent ledger
-- entry, representing the time expenditure. Note this entry is  not balanced,
-- since we omit the \"assets:time\" transaction for simpler output.
entryFromTimeLogInOut :: TimeLogEntry -> TimeLogEntry -> Entry
entryFromTimeLogInOut i o
    | otime >= itime = e
    | otherwise = 
        error $ "clock-out time less than clock-in time in:\n" ++ showEntry e
    where
      e = Entry {
            edate         = odate, -- like ledger
            estatus       = True,
            ecode         = "",
            edescription  = showtime itod ++ "-" ++ showtime otod,
            ecomment      = "",
            etransactions = txns,
            epreceding_comment_lines=""
          }
      showtime = take 5 . show
      acctname = tlcomment i
      itime    = tldatetime i
      otime    = tldatetime o
      itod     = localTimeOfDay itime
      otod     = localTimeOfDay otime
      idate    = localDay itime
      odate    = localDay otime
      hrs      = elapsedSeconds (toutc otime) (toutc itime) / 3600 where toutc = localTimeToUTC utc
      amount   = Mixed [hours hrs]
      txns     = [RawTransaction acctname amount "" RegularTransaction
                 --,RawTransaction "assets:time" (-amount) "" RegularTransaction
                 ]
