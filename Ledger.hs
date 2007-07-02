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
    (rawLedgerAccountNameTree l)
    (Map.fromList [(a, rawLedgerAccount l a) | a <- rawLedgerAccountNames l])

ledgerTransactions :: Ledger -> [EntryTransaction]
ledgerTransactions l = concatMap atransactions $ Map.elems $ accounts l

-- unoptimised
ledgerTransactionsMatching :: ([String],[String]) -> Ledger -> [EntryTransaction]
ledgerTransactionsMatching pats l = rawLedgerTransactionsMatching pats $ rawledger l

-- XXX optimise
ledgerTransactionsMatching1 :: ([String],[String]) -> Ledger -> [EntryTransaction]
ledgerTransactionsMatching1 ([],[]) l = rawLedgerTransactionsMatching ([".*"],[".*"]) (rawledger l)
ledgerTransactionsMatching1 (rs,[]) l = rawLedgerTransactionsMatching (rs,[".*"]) (rawledger l)
ledgerTransactionsMatching1 ([],rs) l = rawLedgerTransactionsMatching ([".*"],rs) (rawledger l)
ledgerTransactionsMatching1 (acctregexps,descregexps) l =
    intersect 
    (concat [filter (matchTransactionAccount r) ts | r <- acctregexps])
    (concat [filter (matchTransactionDescription r) ts | r <- descregexps])
    where ts = ledgerTransactions l

-- unoptimised
showLedgerAccounts :: Ledger -> [String] -> Bool -> Int -> String
showLedgerAccounts l acctpats showsubs maxdepth = 
    showRawLedgerAccounts (rawledger l) acctpats showsubs maxdepth

-- XXX optimise
showLedgerAccounts1 :: Ledger -> [String] -> Bool -> Int -> String
showLedgerAccounts1 l acctpats showsubs maxdepth = 
    concatMap 
    (showAccountTree (rawledger l)) 
    (branches (rawLedgerAccountTreeMatching (rawledger l) acctpats showsubs maxdepth))

