{-|

A 'Posting' represents a 'MixedAmount' being added to or subtracted from a
single 'Account'.  Each 'LedgerTransaction' contains two or more postings
which should add up to 0.  

Generally, we use these with the ledger transaction's date and description
added, which we call a 'Transaction'.

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
showPosting (Posting s a amt _ ttype) = 
    concatTopPadded [showaccountname a ++ " ", showamount amt]
    where
      showaccountname = printf "%-22s" . bracket . elideAccountName width
      (bracket,width) = case ttype of
                      BalancedVirtualPosting -> (\s -> "["++s++"]", 20)
                      VirtualPosting -> (\s -> "("++s++")", 20)
                      otherwise -> (id,22)
      showamount = padleft 12 . showMixedAmountOrZero

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

