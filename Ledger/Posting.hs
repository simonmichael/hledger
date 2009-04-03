{-|

A 'Posting' represents a single transaction line within a ledger
entry. We call it raw to distinguish from the cached 'Transaction'.

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

hasAmount :: Posting -> Bool
hasAmount = (/= missingamt) . pamount
