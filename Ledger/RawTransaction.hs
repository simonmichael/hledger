{-|

A 'RawTransaction' represents a single transaction line within a ledger
entry. We call it raw to distinguish from the cached 'Transaction'.

-}

module Ledger.RawTransaction
where
import Ledger.Utils
import Ledger.Types
import Ledger.Amount
import Ledger.AccountName


instance Show RawTransaction where show = showRawTransaction

nullrawtxn = RawTransaction "" nullamt "" RegularTransaction

showRawTransaction :: RawTransaction -> String
showRawTransaction (RawTransaction a amt _ ttype) = 
    showaccountname a ++ " " ++ (showamount amt) 
    where
      showaccountname = printf "%-22s" . bracket . elideAccountName width
      showamount = printf "%12s" . showMixedAmountOrZero
      (bracket,width) = case ttype of
                      BalancedVirtualTransaction -> (\s -> "["++s++"]", 20)
                      VirtualTransaction -> (\s -> "("++s++")", 20)
                      otherwise -> (id,22)

isReal :: RawTransaction -> Bool
isReal t = rttype t == RegularTransaction

hasAmount :: RawTransaction -> Bool
hasAmount = (/= missingamt) . tamount
