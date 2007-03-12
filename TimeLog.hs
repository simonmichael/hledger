module TimeLog
where
import Utils
import BasicTypes
import Amount
import Transaction
import Entry
import Ledger

data TimeLogEntry = TimeLogEntry {
                    tcode :: Char,
                    tdatetime :: DateTime,
                    tcomment :: String
                   } deriving (Eq,Ord)

instance Show TimeLogEntry where 
    show t = printf "%s %s %s" (show $ tcode t) (tdatetime t) (tcomment t)

data TimeLog = TimeLog {
      timelog_entries :: [TimeLogEntry]
    } deriving (Eq)

instance Show TimeLog where
    show tl = printf "TimeLog with %d entries" $ length $ timelog_entries tl

ledgerFromTimeLog :: TimeLog -> Ledger
ledgerFromTimeLog tl = 
    Ledger [] [] (entriesFromTimeLogEntries $ timelog_entries tl)

entriesFromTimeLogEntries :: [TimeLogEntry] -> [Entry]

entriesFromTimeLogEntries [clockin] = 
    entriesFromTimeLogEntries [clockin, clockoutNowEntry]

entriesFromTimeLogEntries [clockin,clockout] =
    [
     Entry {
       edate         = indate,
       estatus       = True,
       ecode         = "",
       edescription  = accountname,
       etransactions = [
        Transaction accountname amount,
        Transaction "TIME" (-amount)
       ]}
    ]
    where
      accountname = tcomment clockin
      intime      = tdatetime clockin
      indate      = dateFrom $ tdatetime clockin
      outtime     = tdatetime clockout
      amount      = hours 0 -- read $ outtime - intime

entriesFromTimeLogEntries many =
    (entriesFromTimeLogEntries $ take 2 many) ++
    (entriesFromTimeLogEntries $ drop 2 many)

clockoutNowEntry = TimeLogEntry ' ' "" ""
dateFrom = id
