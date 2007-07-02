module Ledger
where
import qualified Data.Map as Map

import Utils
import Types
import Account
import AccountName
import EntryTransaction
import RawLedger


cacheLedger :: RawLedger -> Ledger
cacheLedger l = 
    Ledger 
    l
    (ledgerAccountNameTree l)
    (Map.fromList [(a, ledgerAccount l a) | a <- ledgerAccountNames l])

cLedgerTransactions :: Ledger -> [EntryTransaction]
cLedgerTransactions l = concatMap atransactions $ Map.elems $ accounts l

-- unoptimised
cLedgerTransactionsMatching :: ([String],[String]) -> Ledger -> [EntryTransaction]
cLedgerTransactionsMatching pats l = ledgerTransactionsMatching pats $ rawledger l

-- XXX optimise
cLedgerTransactionsMatching1 :: ([String],[String]) -> Ledger -> [EntryTransaction]
cLedgerTransactionsMatching1 ([],[]) l = ledgerTransactionsMatching ([".*"],[".*"]) (rawledger l)
cLedgerTransactionsMatching1 (rs,[]) l = ledgerTransactionsMatching (rs,[".*"]) (rawledger l)
cLedgerTransactionsMatching1 ([],rs) l = ledgerTransactionsMatching ([".*"],rs) (rawledger l)
cLedgerTransactionsMatching1 (acctregexps,descregexps) l =
    intersect 
    (concat [filter (matchTransactionAccount r) ts | r <- acctregexps])
    (concat [filter (matchTransactionDescription r) ts | r <- descregexps])
    where ts = cLedgerTransactions l

-- unoptimised
showCLedgerAccounts :: Ledger -> [String] -> Bool -> Int -> String
showCLedgerAccounts l acctpats showsubs maxdepth = 
    showLedgerAccounts (rawledger l) acctpats showsubs maxdepth

-- XXX optimise
showCLedgerAccounts1 :: Ledger -> [String] -> Bool -> Int -> String
showCLedgerAccounts1 l acctpats showsubs maxdepth = 
    concatMap 
    (showAccountTree (rawledger l)) 
    (branches (ledgerAccountTreeMatching (rawledger l) acctpats showsubs maxdepth))

