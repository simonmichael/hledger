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

rawLedgerTransactions :: RawLedger -> [EntryTransaction]
rawLedgerTransactions l = entryTransactionsFrom $ entries l

rawLedgerTransactionsMatching :: ([String],[String]) -> RawLedger -> [EntryTransaction]
rawLedgerTransactionsMatching ([],[]) l = rawLedgerTransactionsMatching ([".*"],[".*"]) l
rawLedgerTransactionsMatching (rs,[]) l = rawLedgerTransactionsMatching (rs,[".*"]) l
rawLedgerTransactionsMatching ([],rs) l = rawLedgerTransactionsMatching ([".*"],rs) l
rawLedgerTransactionsMatching (acctregexps,descregexps) l =
    intersect 
    (concat [filter (matchTransactionAccount r) ts | r <- acctregexps])
    (concat [filter (matchTransactionDescription r) ts | r <- descregexps])
    where ts = rawLedgerTransactions l

rawLedgerAccountTransactions :: RawLedger -> AccountName -> [EntryTransaction]
rawLedgerAccountTransactions l a = rawLedgerTransactionsMatching (["^" ++ a ++ "$"], []) l
           
accountNamesFromTransactions :: [EntryTransaction] -> [AccountName]
accountNamesFromTransactions ts = nub $ map account ts

rawLedgerAccountNamesUsed :: RawLedger -> [AccountName]
rawLedgerAccountNamesUsed l = accountNamesFromTransactions $ entryTransactionsFrom $ entries l

rawLedgerAccountNames :: RawLedger -> [AccountName]
rawLedgerAccountNames = sort . expandAccountNames . rawLedgerAccountNamesUsed

rawLedgerTopAccountNames :: RawLedger -> [AccountName]
rawLedgerTopAccountNames l = filter (notElem ':') (rawLedgerAccountNames l)

rawLedgerAccountNamesMatching :: [String] -> RawLedger -> [AccountName]
rawLedgerAccountNamesMatching [] l = rawLedgerAccountNamesMatching [".*"] l
rawLedgerAccountNamesMatching acctregexps l =
    concat [filter (matchAccountName r) accountNames | r <- acctregexps]
        where accountNames = rawLedgerTopAccountNames l

rawLedgerAccountNameTree :: RawLedger -> Tree AccountName
rawLedgerAccountNameTree l = accountNameTreeFrom $ rawLedgerAccountNames l



