
module LedgerTransaction
where
import Utils
import Types
import AccountName
import Amount


instance Show LedgerTransaction where show = showTransaction

showTransaction :: LedgerTransaction -> String
showTransaction t = (showaccountname $ taccount t) ++ "  " ++ (showamount $ tamount t) 
    where
      showaccountname = printf "%-22s" . elideRight 22
      showamount = printf "%11s" . showAmountRoundedOrZero

elideRight width s =
    case length s > width of
      True -> take (width - 2) s ++ ".."
      False -> s

autofillTransactions :: [LedgerTransaction] -> [LedgerTransaction]
autofillTransactions ts =
    let (ns, as) = partition isNormal ts
            where isNormal t = (symbol $ currency $ tamount t) /= "AUTO" in
    case (length as) of
      0 -> ns
      1 -> ns ++ [balanceTransaction $ head as]
          where balanceTransaction t = t{tamount = -(sumTransactions ns)}
      otherwise -> error "too many blank transactions in this entry"

sumTransactions :: [LedgerTransaction] -> Amount
sumTransactions = sum . map tamount

transactionSetPrecision :: Int -> LedgerTransaction -> LedgerTransaction
transactionSetPrecision p (LedgerTransaction a amt) = LedgerTransaction a amt{precision=p}
