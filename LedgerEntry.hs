module LedgerEntry
where
import Utils
import Types
import LedgerTransaction
import Amount


instance Show LedgerEntry where show = showEntryDescription

-- for register report
--
-- a register entry is displayed as two or more lines like this:
-- date       description          account                 amount       balance
-- DDDDDDDDDD dddddddddddddddddddd aaaaaaaaaaaaaaaaaaaaaa  AAAAAAAAAAA AAAAAAAAAAAA
--                                 aaaaaaaaaaaaaaaaaaaaaa  AAAAAAAAAAA AAAAAAAAAAAA
--                                 ...                     ...         ...
-- datewidth = 10
-- descwidth = 20
-- acctwidth = 22
-- amtwidth  = 11
-- balwidth  = 12

showEntryDescription e = (showDate $ edate e) ++ " " ++ (showDescription $ edescription e) ++ " "
showDate d = printf "%-10s" d
showDescription s = printf "%-20s" (elideRight 20 s)

isEntryBalanced :: LedgerEntry -> Bool
isEntryBalanced e = (sumLedgerTransactions . etransactions) e == 0

autofillEntry :: LedgerEntry -> LedgerEntry
autofillEntry e = 
    LedgerEntry (edate e) (estatus e) (ecode e) (edescription e) (ecomment e)
              (autofillTransactions (etransactions e))

-- the print command shows cleaned up ledger file entries, something like:
--
-- yyyy/mm/dd[ *][ CODE] description.........          [  ; comment.............]
--     account name 1.....................  ...$amount1[  ; comment.............]
--     account name 2.....................  ..$-amount1[  ; comment.............]
--
-- codewidth    = 10
-- descwidth    = 20
-- acctwidth    = 35
-- amtwidth     = 11
-- commentwidth = 20

showEntry :: LedgerEntry -> String
showEntry e = 
    unlines $ ["", description] ++ (showtxns $ etransactions e)
    where
      description = concat [date, status, code, desc, comment]
      date = showDate $ edate e
      status = if estatus e then " *" else ""
      code = if (length $ ecode e) > 0 then " "++(printf "%-10s" $ ecode e) else ""
      desc = " " ++ (elideRight 20 $ edescription e)
      comment = if (length $ ecomment e) > 0 then "  ; "++(printf "%-20s" $ ecomment e) else ""
      showtxns (t1:t2:[]) = [showtxn t1, showtxnnoamt t2]
      showtxns ts = map showtxn ts
      showtxn t = showacct t ++ "  " ++ (showamount $ tamount t) ++ (showcomment $ tcomment t)
      showtxnnoamt t = showacct t ++ "             " ++ (showcomment $ tcomment t)
      showacct t = "    " ++ (showaccountname $ taccount t)
      showamount = printf "%11s" . showAmountRounded
      showaccountname = printf "%-35s" . elideRight 35
      showcomment s = if (length s) > 0 then "  ; "++(printf "%-20s" $ elideRight 20 s) else ""

showEntries :: [LedgerEntry] -> String
showEntries = concatMap showEntry

entrySetPrecision :: Int -> LedgerEntry -> LedgerEntry
entrySetPrecision p (LedgerEntry d s c desc comm ts) = 
    LedgerEntry d s c desc comm $ map (ledgerTransactionSetPrecision p) ts
                

-- modifier & periodic entries

instance Show ModifierEntry where 
    show e = "= " ++ (valueexpr e) ++ "\n" ++ unlines (map show (m_transactions e))

instance Show PeriodicEntry where 
    show e = "~ " ++ (periodexpr e) ++ "\n" ++ unlines (map show (p_transactions e))

