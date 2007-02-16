-- data types & behaviours
module Models (
               module Models,
               module Ledger,
               module EntryTransaction,
               module Transaction,
               module Entry,
               module Account,
               module BasicTypes,
              )
where

import Debug.Trace
import Text.Printf
import Text.Regex
import Data.List

import Utils
import BasicTypes
import Account
import Entry
import Transaction
import EntryTransaction
import Ledger


-- any top-level stuff that mixed up the other types


-- showAccountNamesWithBalances :: [(AccountName,String)] -> Ledger -> String
-- showAccountNamesWithBalances as l =
--     unlines $ map (showAccountNameAndBalance l) as

-- showAccountNameAndBalance :: Ledger -> (AccountName, String) -> String
-- showAccountNameAndBalance l (a, adisplay) =
--     printf "%20s  %s" (showBalance $ accountBalance l a) adisplay

accountBalance :: Ledger -> AccountName -> Amount
accountBalance l a =
    sumEntryTransactions (accountTransactions l a)

accountTransactions :: Ledger -> AccountName -> [EntryTransaction]
accountTransactions l a = ledgerTransactionsMatching (["^" ++ a ++ "(:.+)?$"], []) l

accountBalanceNoSubs :: Ledger -> AccountName -> Amount
accountBalanceNoSubs l a =
    sumEntryTransactions (accountTransactionsNoSubs l a)

accountTransactionsNoSubs :: Ledger -> AccountName -> [EntryTransaction]
accountTransactionsNoSubs l a = ledgerTransactionsMatching (["^" ++ a ++ "$"], []) l

addDataToAccounts :: Ledger -> (Tree AccountName) -> (Tree AccountData)
addDataToAccounts l acct = 
    Tree (acctdata, map (addDataToAccounts l) (atsubs acct))
        where 
          acctdata = (aname, atxns, abal)
          aname = atacct acct
          atxns = accountTransactionsNoSubs l aname
          abal = accountBalance l aname

-- an AccountData tree adds some other things we want to cache for
-- convenience, like the account's balance and transactions.
type AccountData = (AccountName,[EntryTransaction],Amount)
type AccountDataTree = Tree AccountData
adtdata = fst . unTree
adtsubs = snd . unTree
nullad = Tree (("", [], 0), [])
adname (a,_,_) = a
adtxns (_,ts,_) = ts
adamt (_,_,amt) = amt

-- a (2 txns)
--   b (boring acct - 0 txns, exactly 1 sub)
--     c (5 txns)
--       d
-- to:
-- a (2 txns)
--   b:c (5 txns)
--     d

-- elideAccount adt = adt

-- elideAccount :: Tree AccountData -> Tree AccountData
-- elideAccount adt = adt
    

-- a
--   b
--     c
-- d
-- to:
-- $7  a
-- $5    b
-- $5      c
-- $0  d
showAccountWithBalances :: Ledger -> Tree AccountData -> String
showAccountWithBalances l adt = (showAccountsWithBalance l) (adtsubs adt)

showAccountsWithBalance :: Ledger -> [Tree AccountData] -> String
showAccountsWithBalance l adts =
    concatMap showAccountDataBranch adts
        where
          showAccountDataBranch :: Tree AccountData -> String
          showAccountDataBranch adt = 
              topacct ++ "\n" ++ subs
--               case boring of
--                 True  -> 
--                 False -> 
              where
                topacct = (showAmount abal) ++ "  " ++ (indentAccountName aname)
                showAmount amt = printf "%11s" (show amt)
                aname = adname $ adtdata adt
                atxns = adtxns $ adtdata adt
                abal = adamt $ adtdata adt
                subs = (showAccountsWithBalance l) $ adtsubs adt
                boring = (length atxns == 0) && ((length subs) == 1)

ledgerAccountsData :: Ledger -> Tree AccountData
ledgerAccountsData l = addDataToAccounts l (ledgerAccounts l)

showLedgerAccountsWithBalances :: Ledger -> Tree AccountData -> String
showLedgerAccountsWithBalances l adt =
    showAccountWithBalances l adt
