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

nullrawtxn = RawTransaction False "" nullmixedamt "" RegularTransaction

showRawTransaction :: RawTransaction -> String
showRawTransaction (RawTransaction s a amt _ ttype) = 
    concatTopPadded [showaccountname a ++ " ", showamount amt]
    where
      showaccountname = printf "%-22s" . bracket . elideAccountName width
      (bracket,width) = case ttype of
                      BalancedVirtualTransaction -> (\s -> "["++s++"]", 20)
                      VirtualTransaction -> (\s -> "("++s++")", 20)
                      otherwise -> (id,22)
      showamount = padleft 12 . showMixedAmountOrZero

isReal :: RawTransaction -> Bool
isReal t = rttype t == RegularTransaction

hasAmount :: RawTransaction -> Bool
hasAmount = (/= missingamt) . tamount
