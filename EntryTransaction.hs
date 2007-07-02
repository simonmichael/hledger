
module EntryTransaction
where
import Utils
import Types
import AccountName
import Entry
import Transaction
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
                                         
flattenEntry :: Entry -> [EntryTransaction]
flattenEntry e = [(e,t) | t <- etransactions e]

accountNamesFromTransactions :: [EntryTransaction] -> [AccountName]
accountNamesFromTransactions ts = nub $ map account ts

entryTransactionsFrom :: [Entry] -> [EntryTransaction]
entryTransactionsFrom es = concat $ map flattenEntry es

sumEntryTransactions :: [EntryTransaction] -> Amount
sumEntryTransactions ets = 
    sumTransactions $ map transaction ets

matchTransactionAccount :: String -> EntryTransaction -> Bool
matchTransactionAccount s t =
    case matchRegex (mkRegex s) (account t) of
      Nothing -> False
      otherwise -> True

matchTransactionDescription :: String -> EntryTransaction -> Bool
matchTransactionDescription s t =
    case matchRegex (mkRegex s) (description t) of
      Nothing -> False
      otherwise -> True

showTransactionsWithBalances :: [EntryTransaction] -> Amount -> String
showTransactionsWithBalances [] _ = []
showTransactionsWithBalances ts b =
    unlines $ showTransactionsWithBalances' ts dummyt b
        where
          dummyt = (Entry "" False "" "" [], Transaction "" (dollars 0))
          showTransactionsWithBalances' [] _ _ = []
          showTransactionsWithBalances' (t:ts) tprev b =
              (if (entry t /= (entry tprev))
               then [showTransactionDescriptionAndBalance t b']
               else [showTransactionAndBalance t b'])
              ++ (showTransactionsWithBalances' ts t b')
                  where b' = b + (amount t)

showTransactionDescriptionAndBalance :: EntryTransaction -> Amount -> String
showTransactionDescriptionAndBalance t b =
    (showEntry $ entry t) ++ (showTransaction $ transaction t) ++ (showBalance b)

showTransactionAndBalance :: EntryTransaction -> Amount -> String
showTransactionAndBalance t b =
    (replicate 32 ' ') ++ (showTransaction $ transaction t) ++ (showBalance b)

showBalance :: Amount -> String
showBalance b = printf " %12s" (showAmountRoundedOrZero b)

transactionsMatching :: ([String],[String]) -> [EntryTransaction] -> [EntryTransaction]
transactionsMatching ([],[]) ts = transactionsMatching ([".*"],[".*"]) ts
transactionsMatching (rs,[]) ts = transactionsMatching (rs,[".*"]) ts
transactionsMatching ([],rs) ts = transactionsMatching ([".*"],rs) ts
transactionsMatching (acctregexps,descregexps) ts =
    intersect 
    (concat [filter (matchTransactionAccount r) ts | r <- acctregexps])
    (concat [filter (matchTransactionDescription r) ts | r <- descregexps])

transactionsWithAccountName :: AccountName -> [EntryTransaction] -> [EntryTransaction]
transactionsWithAccountName a ts = [t | t <- ts, account t == a]
    
transactionsWithOrBelowAccountName :: AccountName -> [EntryTransaction] -> [EntryTransaction]
transactionsWithOrBelowAccountName a ts = 
    [t | t <- ts, account t == a || a `isAccountNamePrefixOf` (account t)]
    
