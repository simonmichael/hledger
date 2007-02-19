module Ledger
where
import Utils
import AccountName
import BasicTypes
import Entry
import EntryTransaction


data Ledger = Ledger {
                      modifier_entries :: [ModifierEntry],
                      periodic_entries :: [PeriodicEntry],
                      entries :: [Entry]
                     } deriving (Eq)

instance Show Ledger where
    show l = printf "Ledger with %d normal, %d modifier, %d periodic entries"
             (length $ modifier_entries l)
             (length $ periodic_entries l)
             (length $ entries l)

ledgerTransactions :: Ledger -> [EntryTransaction]
ledgerTransactions l = entryTransactionsFrom $ entries l

ledgerTransactionsMatching :: ([String],[String]) -> Ledger -> [EntryTransaction]
ledgerTransactionsMatching ([],[]) l = ledgerTransactionsMatching ([".*"],[".*"]) l
ledgerTransactionsMatching (rs,[]) l = ledgerTransactionsMatching (rs,[".*"]) l
ledgerTransactionsMatching ([],rs) l = ledgerTransactionsMatching ([".*"],rs) l
ledgerTransactionsMatching (acctregexps,descregexps) l =
    intersect 
    (concat [filter (matchTransactionAccount r) ts | r <- acctregexps])
    (concat [filter (matchTransactionDescription r) ts | r <- descregexps])
    where ts = ledgerTransactions l

accountNamesFromTransactions :: [EntryTransaction] -> [AccountName]
accountNamesFromTransactions ts = nub $ map account ts

ledgerAccountNamesUsed :: Ledger -> [AccountName]
ledgerAccountNamesUsed l = accountNamesFromTransactions $ entryTransactionsFrom $ entries l

ledgerAccountNames :: Ledger -> [AccountName]
ledgerAccountNames = sort . expandAccountNames . ledgerAccountNamesUsed

ledgerTopAccountNames :: Ledger -> [AccountName]
ledgerTopAccountNames l = filter (notElem ':') (ledgerAccountNames l)

ledgerAccountNamesMatching :: [String] -> Ledger -> [AccountName]
ledgerAccountNamesMatching [] l = ledgerAccountNamesMatching [".*"] l
ledgerAccountNamesMatching acctregexps l =
    concat [filter (matchAccountName r) accountNames | r <- acctregexps]
        where accountNames = ledgerTopAccountNames l

ledgerAccountNameTree :: Ledger -> Tree AccountName
ledgerAccountNameTree l = accountNameTreeFrom $ ledgerAccountNames l



