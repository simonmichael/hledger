
module Entry
where
import Utils
import BasicTypes
import Transaction


-- a register entry is displayed as two or more lines like this:
-- date       description          account                 amount       balance
-- DDDDDDDDDD dddddddddddddddddddd aaaaaaaaaaaaaaaaaaaaaa  AAAAAAAAAAA AAAAAAAAAAAA
--                                 aaaaaaaaaaaaaaaaaaaaaa  AAAAAAAAAAA AAAAAAAAAAAA
--                                 ...                     ...         ...
-- dateWidth = 10
-- descWidth = 20
-- acctWidth = 22
-- amtWidth  = 11
-- balWidth  = 12

data Entry = Entry {
                    edate :: Date,
                    estatus :: Status,
                    ecode :: String,
                    edescription :: String,
                    etransactions :: [Transaction]
                   } deriving (Eq,Ord)

instance Show Entry where show = showEntry

showEntry e = (showDate $ edate e) ++ " " ++ (showDescription $ edescription e) ++ " "
showDate d = printf "%-10s" d
showDescription s = printf "%-20s" (elideRight 20 s)

isEntryBalanced :: Entry -> Bool
isEntryBalanced e = (sumTransactions . etransactions) e == 0

autofillEntry :: Entry -> Entry
autofillEntry e = 
    Entry (edate e) (estatus e) (ecode e) (edescription e)
              (autofillTransactions (etransactions e))

-- modifier & periodic entries

data ModifierEntry = ModifierEntry { -- aka "automated entry"
                    valueexpr :: String,
                    m_transactions :: [Transaction]
                   } deriving (Eq)

instance Show ModifierEntry where 
    show e = "= " ++ (valueexpr e) ++ "\n" ++ unlines (map show (m_transactions e))

data PeriodicEntry = PeriodicEntry {
                    periodexpr :: String,
                    p_transactions :: [Transaction]
                   } deriving (Eq)

instance Show PeriodicEntry where 
    show e = "~ " ++ (periodexpr e) ++ "\n" ++ unlines (map show (p_transactions e))

