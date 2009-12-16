{-|

A 'Posting' represents a 'MixedAmount' being added to or subtracted from a
single 'Account'.  Each 'Transaction' contains two or more postings
which should add up to 0.  

Generally, we use these with the ledger transaction's date and description
added, which we call a 'LedgerPosting'.

-}

module Ledger.Posting
where
import Ledger.Utils
import Ledger.Types
import Ledger.Amount
import Ledger.AccountName


instance Show Posting where show = showPosting

nullrawposting = Posting False "" nullmixedamt "" RegularPosting

showPosting :: Posting -> String
showPosting (Posting _ a amt com lptype) = 
    concatTopPadded [showaccountname a ++ " ", showamount amt, comment]
    where
      ledger3ishlayout = False
      acctnamewidth = if ledger3ishlayout then 25 else 22
      showaccountname = printf ("%-"++(show acctnamewidth)++"s") . bracket . elideAccountName width
      (bracket,width) = case lptype of
                          BalancedVirtualPosting -> (\s -> "["++s++"]", acctnamewidth-2)
                          VirtualPosting -> (\s -> "("++s++")", acctnamewidth-2)
                          _ -> (id,acctnamewidth)
      showamount = padleft 12 . showMixedAmountOrZero
      comment = if null com then "" else "  ; " ++ com
-- XXX refactor
showPostingWithoutPrice (Posting _ a amt com lptype) =
    concatTopPadded [showaccountname a ++ " ", showamount amt, comment]
    where
      ledger3ishlayout = False
      acctnamewidth = if ledger3ishlayout then 25 else 22
      showaccountname = printf ("%-"++(show acctnamewidth)++"s") . bracket . elideAccountName width
      (bracket,width) = case lptype of
                          BalancedVirtualPosting -> (\s -> "["++s++"]", acctnamewidth-2)
                          VirtualPosting -> (\s -> "("++s++")", acctnamewidth-2)
                          _ -> (id,acctnamewidth)
      showamount = padleft 12 . showMixedAmountOrZeroWithoutPrice
      comment = if null com then "" else "  ; " ++ com

isReal :: Posting -> Bool
isReal p = ptype p == RegularPosting

isVirtual :: Posting -> Bool
isVirtual p = ptype p == VirtualPosting

isBalancedVirtual :: Posting -> Bool
isBalancedVirtual p = ptype p == BalancedVirtualPosting

hasAmount :: Posting -> Bool
hasAmount = (/= missingamt) . pamount

postingTypeFromAccountName a
    | head a == '[' && last a == ']' = BalancedVirtualPosting
    | head a == '(' && last a == ')' = VirtualPosting
    | otherwise = RegularPosting

