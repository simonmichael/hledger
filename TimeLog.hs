module TimeLog
where
import Utils
import Types
import Currency
import Amount
import LedgerTransaction
import LedgerEntry
import RawLedger

instance Show TimeLogEntry where 
    show t = printf "%s %s %s" (show $ tlcode t) (tldatetime t) (tlcomment t)

instance Show TimeLog where
    show tl = printf "TimeLog with %d entries" $ length $ timelog_entries tl

ledgerFromTimeLog :: TimeLog -> RawLedger
ledgerFromTimeLog tl = 
    RawLedger [] [] (entriesFromTimeLogEntries $ timelog_entries tl) ""

entriesFromTimeLogEntries :: [TimeLogEntry] -> [LedgerEntry]

entriesFromTimeLogEntries [clockin] = 
    entriesFromTimeLogEntries [clockin, clockoutNowEntry]

entriesFromTimeLogEntries [clockin,clockout] =
    [
     LedgerEntry {
       edate         = indate,
       estatus       = True,
       ecode         = "",
       edescription  = accountname,
       ecomment      = "",
       etransactions = [
        LedgerTransaction accountname amount "",
        LedgerTransaction "TIME" (-amount) ""
       ],
       epreceding_comment_lines=""}
    ]
    where
      accountname = tlcomment clockin
      intime      = tldatetime clockin
      indate      = dateFrom $ tldatetime clockin
      outtime     = tldatetime clockout
      amount      = hours 0 -- read $ outtime - intime

entriesFromTimeLogEntries many =
    (entriesFromTimeLogEntries $ take 2 many) ++
    (entriesFromTimeLogEntries $ drop 2 many)

clockoutNowEntry = TimeLogEntry ' ' "" ""
dateFrom = id
