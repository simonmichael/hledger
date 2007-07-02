module RawLedger
where
import qualified Data.Map as Map

import Utils
import AccountName
import Types
import Entry
import EntryTransaction


instance Show RawLedger where
    show l = printf "RawLedger with %d entries"
             ((length $ entries l) +
              (length $ modifier_entries l) +
              (length $ periodic_entries l))

ledgerTransactions :: RawLedger -> [EntryTransaction]
ledgerTransactions l = entryTransactionsFrom $ entries l

ledgerTransactionsMatching :: ([String],[String]) -> RawLedger -> [EntryTransaction]
ledgerTransactionsMatching ([],[]) l = ledgerTransactionsMatching ([".*"],[".*"]) l
ledgerTransactionsMatching (rs,[]) l = ledgerTransactionsMatching (rs,[".*"]) l
ledgerTransactionsMatching ([],rs) l = ledgerTransactionsMatching ([".*"],rs) l
ledgerTransactionsMatching (acctregexps,descregexps) l =
    intersect 
    (concat [filter (matchTransactionAccount r) ts | r <- acctregexps])
    (concat [filter (matchTransactionDescription r) ts | r <- descregexps])
    where ts = ledgerTransactions l

ledgerAccountTransactions :: RawLedger -> AccountName -> [EntryTransaction]
ledgerAccountTransactions l a = ledgerTransactionsMatching (["^" ++ a ++ "$"], []) l
           
accountNamesFromTransactions :: [EntryTransaction] -> [AccountName]
accountNamesFromTransactions ts = nub $ map account ts

ledgerAccountNamesUsed :: RawLedger -> [AccountName]
ledgerAccountNamesUsed l = accountNamesFromTransactions $ entryTransactionsFrom $ entries l

ledgerAccountNames :: RawLedger -> [AccountName]
ledgerAccountNames = sort . expandAccountNames . ledgerAccountNamesUsed

ledgerTopAccountNames :: RawLedger -> [AccountName]
ledgerTopAccountNames l = filter (notElem ':') (ledgerAccountNames l)

ledgerAccountNamesMatching :: [String] -> RawLedger -> [AccountName]
ledgerAccountNamesMatching [] l = ledgerAccountNamesMatching [".*"] l
ledgerAccountNamesMatching acctregexps l =
    concat [filter (matchAccountName r) accountNames | r <- acctregexps]
        where accountNames = ledgerTopAccountNames l

ledgerAccountNameTree :: RawLedger -> Tree AccountName
ledgerAccountNameTree l = accountNameTreeFrom $ ledgerAccountNames l



