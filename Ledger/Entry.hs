{-|

An 'Entry' represents a regular entry in the ledger file. It normally
contains two or more balanced 'RawTransaction's.

-}

module Ledger.Entry
where
import Ledger.Utils
import Ledger.Types
import Ledger.RawTransaction
import Ledger.Amount


instance Show Entry where show = showEntry

instance Show ModifierEntry where 
    show e = "= " ++ (valueexpr e) ++ "\n" ++ unlines (map show (m_transactions e))

instance Show PeriodicEntry where 
    show e = "~ " ++ (periodexpr e) ++ "\n" ++ unlines (map show (p_transactions e))

nullentry = Entry {
              edate=parsedate "1900/1/1", 
              estatus=False, 
              ecode="", 
              edescription="", 
              ecomment="",
              etransactions=[],
              epreceding_comment_lines=""
            }

{-|
Show a ledger entry, formatted for the print command. ledger 2.x's
standard format looks like this:

@
yyyy/mm/dd[ *][ CODE] description.........          [  ; comment...............]
    account name 1.....................  ...$amount1[  ; comment...............]
    account name 2.....................  ..$-amount1[  ; comment...............]

pcodewidth    = no limit -- 10          -- mimicking ledger layout.
pdescwidth    = no limit -- 20          -- I don't remember what these mean,
pacctwidth    = 35 minimum, no maximum  -- they were important at the time.
pamtwidth     = 11
pcommentwidth = no limit -- 22
@
-}
showEntry :: Entry -> String
showEntry e = 
    unlines $ [{-precedingcomment ++ -}description] ++ (showtxns $ etransactions e) ++ [""]
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
      showamount = printf "%12s" . showMixedAmount
      showaccountname s = printf "%-34s" s
      showcomment s = if (length s) > 0 then "  ; "++s else ""

showDate d = printf "%-10s" (show d)

isEntryBalanced :: Entry -> Bool
isEntryBalanced (Entry {etransactions=ts}) = 
    isZeroMixedAmount $ sum $ map tamount $ filter isReal ts

-- | Fill in a missing balance in this entry, if we have enough
-- information to do that. Excluding virtual transactions, there should be
-- at most one missing balance. Otherwise, raise an error.
-- The new balance will be converted to cost basis if possible.
balanceEntry :: Entry -> Entry
balanceEntry e@Entry{etransactions=ts} = e{etransactions=ts'}
    where 
      (withamounts, missingamounts) = partition hasAmount $ filter isReal ts
      ts' = case (length missingamounts) of
              0 -> ts
              1 -> map balance ts
              otherwise -> error $ "could not balance this entry, too many missing amounts:\n" ++ show e
      otherstotal = sum $ map tamount withamounts
      balance t
          | isReal t && not (hasAmount t) = t{tamount = costOfMixedAmount (-otherstotal)}
          | otherwise = t
