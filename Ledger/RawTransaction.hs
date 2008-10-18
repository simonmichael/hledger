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

showRawTransaction :: RawTransaction -> String
showRawTransaction t = (showaccountname $ taccount t) ++ " " ++ (showamount $ tamount t) 
    where
      showaccountname = printf "%-22s" . elideAccountName 22
      showamount = printf "%12s" . showAmountOrZero

isReal :: RawTransaction -> Bool
isReal t = rttype t == RegularTransaction

hasAmount :: RawTransaction -> Bool
hasAmount = ("AUTO" /=) . symbol . commodity . tamount

sumRawTransactions :: [RawTransaction] -> MixedAmount
sumRawTransactions = normaliseMixedAmount . map tamount
