module Ledger
where

import Debug.Trace
import Text.Printf
import Text.Regex
import Data.List

import Utils
import Account
import Entry
import EntryTransaction


data Ledger = Ledger {
                      modifier_entries :: [ModifierEntry],
                      periodic_entries :: [PeriodicEntry],
                      entries :: [Entry]
                     } deriving (Eq)

instance Show Ledger where
    show l = printf "Ledger with %d normal, %d modifier, %d periodic entries"
             (show $ length $ modifier_entries l)
             (show $ length $ periodic_entries l)
             (show $ length $ entries l)

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

ledgerAccounts :: Ledger -> Tree AccountName
ledgerAccounts l = accountFrom $ ledgerAccountNames l

showLedgerAccounts :: Ledger -> [String] -> Int -> String
showLedgerAccounts l acctpats depth = 
    showAccountsWithBalances l accounts depth
        where
          accounts = ledgerAccountsMatching l acctpats

showAccountsWithBalances :: Ledger -> [Account] -> Int -> String
showAccountsWithBalances l accts depth =
    ""

ledgerAccountsMatching :: Ledger -> [String] -> [Account]
ledgerAccountsMatching l acctpats = []
