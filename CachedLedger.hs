module CachedLedger
where
import qualified Data.Map as Map

import Utils
import Types
import Account
import AccountName
import EntryTransaction
import Ledger


cacheLedger :: Ledger -> CachedLedger
cacheLedger l = 
    CachedLedger 
    l
    (ledgerAccountNameTree l)
    (Map.fromList [(a, ledgerAccount l a) | a <- ledgerAccountNames l])

cLedgerTransactions :: CachedLedger -> [EntryTransaction]
cLedgerTransactions l = concatMap atransactions $ Map.elems $ accounts l

-- unoptimised
cLedgerTransactionsMatching :: ([String],[String]) -> CachedLedger -> [EntryTransaction]
cLedgerTransactionsMatching pats l = ledgerTransactionsMatching pats $ uncached_ledger l

-- XXX optimise
cLedgerTransactionsMatching1 :: ([String],[String]) -> CachedLedger -> [EntryTransaction]
cLedgerTransactionsMatching1 ([],[]) l = ledgerTransactionsMatching ([".*"],[".*"]) (uncached_ledger l)
cLedgerTransactionsMatching1 (rs,[]) l = ledgerTransactionsMatching (rs,[".*"]) (uncached_ledger l)
cLedgerTransactionsMatching1 ([],rs) l = ledgerTransactionsMatching ([".*"],rs) (uncached_ledger l)
cLedgerTransactionsMatching1 (acctregexps,descregexps) l =
    intersect 
    (concat [filter (matchTransactionAccount r) ts | r <- acctregexps])
    (concat [filter (matchTransactionDescription r) ts | r <- descregexps])
    where ts = cLedgerTransactions l

-- unoptimised
showCLedgerAccounts :: CachedLedger -> [String] -> Bool -> Int -> String
showCLedgerAccounts l acctpats showsubs maxdepth = 
    showLedgerAccounts (uncached_ledger l) acctpats showsubs maxdepth

-- XXX optimise
showCLedgerAccounts1 :: CachedLedger -> [String] -> Bool -> Int -> String
showCLedgerAccounts1 l acctpats showsubs maxdepth = 
    concatMap 
    (showAccountTree (uncached_ledger l)) 
    (branches (ledgerAccountTreeMatching (uncached_ledger l) acctpats showsubs maxdepth))

