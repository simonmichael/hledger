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

-- | Fill in the missing balance in an entry's transactions. Excluding
-- virtual transactions, there should be at most one missing balance,
-- otherwise return Nothing.
autofillTransactions :: [RawTransaction] -> Maybe [RawTransaction]
autofillTransactions ts =
    case (length missingamounts) of
      0 -> Just ts
      1 -> Just $ map balance ts
      otherwise -> Nothing
    where 
      (reals, _) = partition isReal ts
      (realamounts, missingamounts) = partition hasAmount reals
      balance t = if (isReal t) && (not $ hasAmount t) 
                  then t{tamount = -(sumLedgerTransactions realamounts)}
                  else t

isReal :: RawTransaction -> Bool
isReal t = rttype t == RegularTransaction

hasAmount :: RawTransaction -> Bool
hasAmount = ("AUTO" /=) . symbol . commodity . tamount

sumLedgerTransactions :: [RawTransaction] -> Amount
sumLedgerTransactions = sumAmounts . map tamount
