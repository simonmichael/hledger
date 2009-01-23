{-|

An 'Entry' represents a regular entry in the ledger file. It normally
contains two or more balanced 'RawTransaction's.

-}

module Ledger.Entry
where
import Ledger.Utils
import Ledger.Types
import Ledger.Dates
import Ledger.RawTransaction
import Ledger.Amount


instance Show Entry where show = showEntry

instance Show ModifierEntry where 
    show e = "= " ++ (valueexpr e) ++ "\n" ++ unlines (map show (m_transactions e))

instance Show PeriodicEntry where 
    show e = "~ " ++ (periodicexpr e) ++ "\n" ++ unlines (map show (p_transactions e))

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
showEntry = showEntry' True

showEntryUnelided :: Entry -> String
showEntryUnelided = showEntry' False

showEntry' :: Bool -> Entry -> String
showEntry' elide e = 
    unlines $ [{-precedingcomment ++ -}description] ++ (showtxns $ etransactions e) ++ [""]
    where
      precedingcomment = epreceding_comment_lines e
      description = concat [date, status, code, desc] -- , comment]
      date = showdate $ edate e
      status = if estatus e then " *" else ""
      code = if (length $ ecode e) > 0 then (printf " (%s)" $ ecode e) else ""
      desc = " " ++ edescription e
      comment = if (length $ ecomment e) > 0 then "  ; "++(ecomment e) else ""
      showtxns ts
          | elide && length ts == 2 = [showtxn (ts !! 0), showtxnnoamt (ts !! 1)]
          | otherwise = map showtxn ts
      showtxn t = showacct t ++ "  " ++ (showamount $ tamount t) ++ (showcomment $ tcomment t)
      showtxnnoamt t = showacct t ++ "              " ++ (showcomment $ tcomment t)
      showacct t = "    " ++ showstatus t ++ (showaccountname $ taccount t)
      showamount = printf "%12s" . showMixedAmount
      showaccountname s = printf "%-34s" s
      showcomment s = if (length s) > 0 then "  ; "++s else ""
      showdate d = printf "%-10s" (showDate d)
      showstatus t = case tstatus t of
                       True -> "* "
                       False -> ""

isEntryBalanced :: Entry -> Bool
isEntryBalanced (Entry {etransactions=ts}) = 
    isZeroMixedAmount $ costOfMixedAmount $ sum $ map tamount $ filter isReal ts

-- | Ensure that this entry is balanced, possibly auto-filling a missing
-- amount first. We can auto-fill if there is just one non-virtual
-- transaction without an amount. The auto-filled balance will be
-- converted to cost basis if possible. If the entry can not be balanced,
-- return an error message instead.
balanceEntry :: Entry -> Either String Entry
balanceEntry e@Entry{etransactions=ts}
    | length missingamounts > 1 = Left $ showerr "could not balance this entry, too many missing amounts"
    | not $ isEntryBalanced e' = Left $ showerr "could not balance this entry, amounts do not balance"
    | otherwise = Right e'
    where
      (withamounts, missingamounts) = partition hasAmount $ filter isReal ts
      e' = e{etransactions=ts'}
      ts' | length missingamounts == 1 = map balance ts
          | otherwise = ts
          where 
            balance t | isReal t && not (hasAmount t) = t{tamount = costOfMixedAmount (-otherstotal)}
                      | otherwise = t
                      where otherstotal = sum $ map tamount withamounts
      showerr s = printf "%s:\n%s" s (showEntryUnelided e)
