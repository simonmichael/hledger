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

instance Show TimeLogEntry where 
    show t = printf "%s %s %s" (show $ tlcode t) (show $ tldatetime t) (tlcomment t)

instance Show TimeLog where
    show tl = printf "TimeLog with %d entries" $ length $ timelog_entries tl

-- | Convert time log entries to ledger entries.
entriesFromTimeLogEntries :: [TimeLogEntry] -> [Entry]
entriesFromTimeLogEntries [] = []
entriesFromTimeLogEntries [i] = entriesFromTimeLogEntries [i, clockoutFor i]
entriesFromTimeLogEntries (i:o:rest) = [entryFromTimeLogInOut i o] ++ entriesFromTimeLogEntries rest

-- | When there is a trailing clockin entry, provide the missing clockout.
-- An entry for now is what we want but this requires IO so for now use
-- the clockin time, ie don't count the current clocked-in period.
clockoutFor :: TimeLogEntry -> TimeLogEntry
clockoutFor (TimeLogEntry _ t _) = TimeLogEntry 'o' t ""

-- | Convert a timelog clockin and clockout entry to an equivalent ledger
-- entry, representing the time expenditure. Note this entry is  not balanced,
-- since we omit the \"assets:time\" transaction for simpler output.
entryFromTimeLogInOut :: TimeLogEntry -> TimeLogEntry -> Entry
entryFromTimeLogInOut i o =
    Entry {
      edate         = indate, -- ledger uses outdate
      estatus       = True,
      ecode         = "",
      edescription  = "",
      ecomment      = "",
      etransactions = txns,
      epreceding_comment_lines=""
    }
    where
      acctname = tlcomment i
      indate   = utctDay intime
      outdate  = utctDay outtime
      intime   = tldatetime i
      outtime  = tldatetime o
      amount   = Mixed [hours $ elapsedSeconds outtime intime / 3600]
      txns     = [RawTransaction acctname amount "" RegularTransaction
                 --,RawTransaction "assets:time" (-amount) "" RegularTransaction
                 ]
