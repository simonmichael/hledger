module Transaction
where
import Utils
import Types
import AccountName
import LedgerEntry
import LedgerTransaction
import Amount
import Currency


flattenEntry :: LedgerEntry -> [Transaction]
flattenEntry (LedgerEntry d _ _ desc ts) = [Transaction d desc (taccount t) (tamount t) | t <- ts]

transactionSetPrecision :: Int -> Transaction -> Transaction
transactionSetPrecision p (Transaction d desc a amt) = Transaction d desc a amt{precision=p}

accountNamesFromTransactions :: [Transaction] -> [AccountName]
accountNamesFromTransactions ts = nub $ map account ts

sumTransactions :: [Transaction] -> Amount
sumTransactions = sum . map amount

matchTransactionAccount :: Regex -> Transaction -> Bool
matchTransactionAccount r t =
    case matchRegex r (account t) of
      Nothing -> False
      otherwise -> True

matchTransactionDescription :: Regex -> Transaction -> Bool
matchTransactionDescription r t =
    case matchRegex r (description t) of
      Nothing -> False
      otherwise -> True

-- for register command 

showTransactionsWithBalances :: [Transaction] -> Amount -> String
showTransactionsWithBalances [] _ = []
showTransactionsWithBalances ts b =
    unlines $ showTransactionsWithBalances' ts dummyt b
        where
          dummyt = Transaction "" "" "" (dollars 0)
          showTransactionsWithBalances' [] _ _ = []
          showTransactionsWithBalances' (t:ts) tprev b =
              (if sameentry t tprev
               then [showTransactionDescriptionAndBalance t b']
               else [showTransactionAndBalance t b'])
              ++ (showTransactionsWithBalances' ts t b')
                  where 
                    b' = b + (amount t)
                    sameentry (Transaction d1 desc1 _ _) (Transaction d2 desc2 _ _) = 
                        d1 == d2 && desc1 == desc2
                        -- we forgot the entry-txn relationships.. good enough ?

showTransactionDescriptionAndBalance :: Transaction -> Amount -> String
showTransactionDescriptionAndBalance t b =
    (showEntryDescription $ LedgerEntry (date t) False "" (description t) []) 
    ++ (showLedgerTransaction $ LedgerTransaction (account t) (amount t)) ++ (showBalance b)

showTransactionAndBalance :: Transaction -> Amount -> String
showTransactionAndBalance t b =
    (replicate 32 ' ') ++ (showLedgerTransaction $ LedgerTransaction (account t) (amount t)) ++ (showBalance b)

showBalance :: Amount -> String
showBalance b = printf " %12s" (showAmountRoundedOrZero b)

transactionsWithAccountName :: AccountName -> [Transaction] -> [Transaction]
transactionsWithAccountName a ts = [t | t <- ts, account t == a]
    
transactionsWithOrBelowAccountName :: AccountName -> [Transaction] -> [Transaction]
transactionsWithOrBelowAccountName a ts = 
    [t | t <- ts, account t == a || a `isAccountNamePrefixOf` (account t)]
