module Transaction
where
import Utils
import Types
import AccountName
import LedgerEntry
import LedgerTransaction
import Amount
import Currency


entry       (e,t) = e
transaction (e,t) = t
date        (e,t) = edate e
status      (e,t) = estatus e
code        (e,t) = ecode e
description (e,t) = edescription e
account     (e,t) = taccount t
amount      (e,t) = tamount t
                                         
flattenEntry :: LedgerEntry -> [Transaction]
flattenEntry e = [(e,t) | t <- etransactions e]

entryTransactionSetPrecision :: Int -> Transaction -> Transaction
entryTransactionSetPrecision p (e, LedgerTransaction a amt) = (e, LedgerTransaction a amt{precision=p})

accountNamesFromTransactions :: [Transaction] -> [AccountName]
accountNamesFromTransactions ts = nub $ map account ts

entryTransactionsFrom :: [LedgerEntry] -> [Transaction]
entryTransactionsFrom es = concat $ map flattenEntry es

sumEntryTransactions :: [Transaction] -> Amount
sumEntryTransactions ets = 
    sumTransactions $ map transaction ets

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
          dummyt = (LedgerEntry "" False "" "" [], LedgerTransaction "" (dollars 0))
          showTransactionsWithBalances' [] _ _ = []
          showTransactionsWithBalances' (t:ts) tprev b =
              (if (entry t /= (entry tprev))
               then [showTransactionDescriptionAndBalance t b']
               else [showTransactionAndBalance t b'])
              ++ (showTransactionsWithBalances' ts t b')
                  where b' = b + (amount t)

showTransactionDescriptionAndBalance :: Transaction -> Amount -> String
showTransactionDescriptionAndBalance t b =
    (showEntryDescription $ entry t) ++ (showTransaction $ transaction t) ++ (showBalance b)

showTransactionAndBalance :: Transaction -> Amount -> String
showTransactionAndBalance t b =
    (replicate 32 ' ') ++ (showTransaction $ transaction t) ++ (showBalance b)

showBalance :: Amount -> String
showBalance b = printf " %12s" (showAmountRoundedOrZero b)

transactionsWithAccountName :: AccountName -> [Transaction] -> [Transaction]
transactionsWithAccountName a ts = [t | t <- ts, account t == a]
    
transactionsWithOrBelowAccountName :: AccountName -> [Transaction] -> [Transaction]
transactionsWithOrBelowAccountName a ts = 
    [t | t <- ts, account t == a || a `isAccountNamePrefixOf` (account t)]
