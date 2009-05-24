{-|

A 'LedgerTransaction' represents a regular transaction in the ledger
file. It normally contains two or more balanced 'Posting's.

-}

module Ledger.LedgerTransaction
where
import Ledger.Utils
import Ledger.Types
import Ledger.Dates
import Ledger.Posting
import Ledger.Amount


instance Show LedgerTransaction where show = showLedgerTransaction

instance Show ModifierTransaction where 
    show t = "= " ++ (mtvalueexpr t) ++ "\n" ++ unlines (map show (mtpostings t))

instance Show PeriodicTransaction where 
    show t = "~ " ++ (ptperiodicexpr t) ++ "\n" ++ unlines (map show (ptpostings t))

nullledgertxn :: LedgerTransaction
nullledgertxn = LedgerTransaction {
              ltdate=parsedate "1900/1/1", 
              ltstatus=False, 
              ltcode="", 
              ltdescription="", 
              ltcomment="",
              ltpostings=[],
              ltpreceding_comment_lines=""
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
showLedgerTransaction :: LedgerTransaction -> String
showLedgerTransaction = showLedgerTransaction' True

showLedgerTransactionUnelided :: LedgerTransaction -> String
showLedgerTransactionUnelided = showLedgerTransaction' False

showLedgerTransaction' :: Bool -> LedgerTransaction -> String
showLedgerTransaction' elide t = 
    unlines $ [{-precedingcomment ++ -}description] ++ (showpostings $ ltpostings t) ++ [""]
    where
      precedingcomment = ltpreceding_comment_lines t
      description = concat [date, status, code, desc] -- , comment]
      date = showdate $ ltdate t
      status = if ltstatus t then " *" else ""
      code = if (length $ ltcode t) > 0 then (printf " (%s)" $ ltcode t) else ""
      desc = " " ++ ltdescription t
      comment = if (length $ ltcomment t) > 0 then "  ; "++(ltcomment t) else ""
      showdate d = printf "%-10s" (showDate d)
      showpostings ps
          | elide && length ps > 1 && isLedgerTransactionBalanced t
              = map showposting (init ps) ++ [showpostingnoamt (last ps)]
          | otherwise = map showposting ps
          where
            showposting p = showacct p ++ "  " ++ (showamount $ pamount p) ++ (showcomment $ pcomment p)
            showpostingnoamt p = rstrip $ showacct p ++ "              " ++ (showcomment $ pcomment p)
            showacct p = "    " ++ showstatus p ++ (printf "%-34s" $ showAccountName (Just 34) (ptype p) (paccount p))
            showamount = printf "%12s" . showMixedAmount
            showcomment s = if (length s) > 0 then "  ; "++s else ""
            showstatus p = if pstatus p then "* " else ""

-- | Show an account name, clipped to the given width if any, and
-- appropriately bracketed/parenthesised for the given posting type.
showAccountName :: Maybe Int -> PostingType -> AccountName -> String
showAccountName w = fmt
    where
      fmt RegularPosting = take w'
      fmt VirtualPosting = parenthesise . reverse . take (w'-2) . reverse
      fmt BalancedVirtualPosting = bracket . reverse . take (w'-2) . reverse
      w' = fromMaybe 999999 w
      parenthesise s = "("++s++")"
      bracket s = "["++s++"]"

isLedgerTransactionBalanced :: LedgerTransaction -> Bool
isLedgerTransactionBalanced (LedgerTransaction {ltpostings=ps}) = 
    all (isReallyZeroMixedAmount . costOfMixedAmount . sum . map pamount)
            [filter isReal ps, filter isBalancedVirtual ps]

-- | Ensure that this entry is balanced, possibly auto-filling a missing
-- amount first. We can auto-fill if there is just one non-virtual
-- transaction without an amount. The auto-filled balance will be
-- converted to cost basis if possible. If the entry can not be balanced,
-- return an error message instead.
balanceLedgerTransaction :: LedgerTransaction -> Either String LedgerTransaction
balanceLedgerTransaction t@LedgerTransaction{ltpostings=ps}
    | length missingamounts > 1 = Left $ printerr "could not balance this transaction, too many missing amounts"
    | not $ isLedgerTransactionBalanced t' = Left $ printerr nonzerobalanceerror
    | otherwise = Right t'
    where
      (withamounts, missingamounts) = partition hasAmount $ filter isReal ps
      t' = t{ltpostings=ps'}
      ps' | length missingamounts == 1 = map balance ps
          | otherwise = ps
          where 
            balance p | isReal p && not (hasAmount p) = p{pamount = costOfMixedAmount (-otherstotal)}
                      | otherwise = p
                      where otherstotal = sum $ map pamount withamounts
      printerr s = printf "%s:\n%s" s (showLedgerTransactionUnelided t)

nonzerobalanceerror = "could not balance this transaction, amounts do not add up to zero"