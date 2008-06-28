module Transaction
where
import Utils
import Types
import AccountName
import LedgerEntry
import LedgerTransaction
import Amount
import Currency


instance Show Transaction where 
    show (Transaction eno d desc a amt) = 
        unwords [d,desc,a,show amt]

-- we use the entry number e to remember the grouping of txns
flattenEntry :: (LedgerEntry, Int) -> [Transaction]
flattenEntry (LedgerEntry d _ _ desc _ ts _, e) = 
    [Transaction e d desc (taccount t) (tamount t) | t <- ts]

transactionSetPrecision :: Int -> Transaction -> Transaction
transactionSetPrecision p (Transaction e d desc a amt) = Transaction e d desc a amt{precision=p}

accountNamesFromTransactions :: [Transaction] -> [AccountName]
accountNamesFromTransactions ts = nub $ map account ts

sumTransactions :: [Transaction] -> Amount
sumTransactions = sum . map amount

-- for register command 

showTransactionsWithBalances :: [Transaction] -> Amount -> String
showTransactionsWithBalances [] _ = []
showTransactionsWithBalances ts b =
    unlines $ showTransactionsWithBalances' ts dummyt b
        where
          dummyt = Transaction 0 "" "" "" (dollars 0)
          showTransactionsWithBalances' [] _ _ = []
          showTransactionsWithBalances' (t:ts) tprev b =
              (if sameentry t tprev
               then [showTransactionAndBalance t b']
               else [showTransactionDescriptionAndBalance t b'])
              ++ (showTransactionsWithBalances' ts t b')
                  where 
                    b' = b + (amount t)
                    sameentry (Transaction e1 _ _ _ _) (Transaction e2 _ _ _ _) = e1 == e2

showTransactionDescriptionAndBalance :: Transaction -> Amount -> String
showTransactionDescriptionAndBalance t b =
    (showEntryDescription $ LedgerEntry (date t) False "" (description t) "" [] "") 
    ++ (showLedgerTransaction $ LedgerTransaction (account t) (amount t) "") ++ (showBalance b)

showTransactionAndBalance :: Transaction -> Amount -> String
showTransactionAndBalance t b =
    (replicate 32 ' ') ++ (showLedgerTransaction $ LedgerTransaction (account t) (amount t) "") ++ (showBalance b)

showBalance :: Amount -> String
showBalance b = printf " %12s" (showAmountRoundedOrZero b)

