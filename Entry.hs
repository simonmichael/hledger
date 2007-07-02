
module Entry
where
import Utils
import Types
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

instance Show ModifierEntry where 
    show e = "= " ++ (valueexpr e) ++ "\n" ++ unlines (map show (m_transactions e))

instance Show PeriodicEntry where 
    show e = "~ " ++ (periodexpr e) ++ "\n" ++ unlines (map show (p_transactions e))

