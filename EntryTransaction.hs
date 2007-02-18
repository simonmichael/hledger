
module EntryTransaction
where
import Utils
import BasicTypes
import Entry
import Transaction


-- We convert Transactions into EntryTransactions, which are (entry,
-- transaction) pairs, since I couldn't easily just have transactions
-- reference their entry like in OO.  These are referred to as just
-- "transactions" hereafter.

type EntryTransaction = (Entry,Transaction)

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
          dummyt = (Entry "" False "" "" [], Transaction "" (Amount "" 0))
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
showBalance b = printf " %12s" (amountRoundedOrZero b)

