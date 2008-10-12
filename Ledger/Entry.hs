{-|

An 'Entry' represents a normal entry in the ledger file. It normally
contains two or more 'RawTransaction's which balance.

-}

module Ledger.Entry
where
import Ledger.Utils
import Ledger.Types
import Ledger.RawTransaction
import Ledger.Amount


entrytests = TestList [
             ]

instance Show Entry where show = showEntryDescription

{-
Helpers for the register report. A register entry is displayed as two
or more lines like this:

@
date       description          account                 amount       balance
DDDDDDDDDD dddddddddddddddddddd aaaaaaaaaaaaaaaaaaaaaa  AAAAAAAAAAA AAAAAAAAAAAA
                                aaaaaaaaaaaaaaaaaaaaaa  AAAAAAAAAAA AAAAAAAAAAAA
                                ...                     ...         ...

datewidth = 10
descwidth = 20
acctwidth = 22
amtwidth  = 11
balwidth  = 12
@
-}

showEntryDescription e = 
    (showDate $ edate e) ++ " " ++ (showDescription $ edescription e) ++ " "
showDate d = printf "%-10s" d
showDescription s = printf "%-20s" (elideRight 20 s)

isEntryBalanced :: Entry -> Bool
isEntryBalanced = isZeroAmount . sumLedgerTransactions . etransactions

autofillEntry :: Entry -> Entry
autofillEntry e@(Entry _ _ _ _ _ ts _) =
    let e' = e{etransactions=autofillTransactions ts} in
    case (isEntryBalanced e') of
      True -> e'
      False -> (error $ "transactions don't balance in " ++ show e)

{-|
Helper for the print command which shows cleaned up ledger file
entries, something like:

@
yyyy/mm/dd[ *][ CODE] description.........          [  ; comment...............]
    account name 1.....................  ...$amount1[  ; comment...............]
    account name 2.....................  ..$-amount1[  ; comment...............]

pcodewidth    = no limit -- 10
pdescwidth    = no limit -- 20
pacctwidth    = 35 minimum, no maximum
pamtwidth     = 11
pcommentwidth = no limit -- 22
@
-}
showEntry :: Entry -> String
showEntry e = 
    unlines $ [precedingcomment ++ description] ++ (showtxns $ etransactions e) ++ [""]
    where
      precedingcomment = epreceding_comment_lines e
      description = concat [date, status, code, desc] -- , comment]
      date = showDate $ edate e
      status = if estatus e then " *" else ""
      code = if (length $ ecode e) > 0 then (printf " (%s)" $ ecode e) else ""
      desc = " " ++ edescription e
      comment = if (length $ ecomment e) > 0 then "  ; "++(ecomment e) else ""
      showtxns (t1:t2:[]) = [showtxn t1, showtxnnoamt t2]
      showtxns ts = map showtxn ts
      showtxn t = showacct t ++ "  " ++ (showamount $ tamount t) ++ (showcomment $ tcomment t)
      showtxnnoamt t = showacct t ++ "              " ++ (showcomment $ tcomment t)
      showacct t = "    " ++ (showaccountname $ taccount t)
      showamount = printf "%12s" . showAmountRounded
      showaccountname s = printf "%-34s" s
      showcomment s = if (length s) > 0 then "  ; "++s else ""

entrySetPrecision :: Int -> Entry -> Entry
entrySetPrecision p (Entry d s c desc comm ts prec) = 
    Entry d s c desc comm (map (ledgerTransactionSetPrecision p) ts) prec
                

-- modifier & periodic entries

instance Show ModifierEntry where 
    show e = "= " ++ (valueexpr e) ++ "\n" ++ unlines (map show (m_transactions e))

instance Show PeriodicEntry where 
    show e = "~ " ++ (periodexpr e) ++ "\n" ++ unlines (map show (p_transactions e))

