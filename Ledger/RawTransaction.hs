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

-- | Fill in the missing balance in an entry's transactions. There can be
-- at most one missing balance, otherwise we'll return Nothing.
autofillTransactions :: [RawTransaction] -> Maybe [RawTransaction]
autofillTransactions ts =
    case (length blanks) of
      0 -> Just ts
      1 -> Just $ map balance ts
      otherwise -> Nothing
    where 
      (normals, blanks) = partition isnormal ts
      isnormal t = (symbol $ commodity $ tamount t) /= "AUTO"
      balance t = if isnormal t then t else t{tamount = -(sumLedgerTransactions normals)}

sumLedgerTransactions :: [RawTransaction] -> Amount
sumLedgerTransactions = sumAmounts . map tamount
