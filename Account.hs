module Account
where
import Utils
import BasicTypes
import AccountName
import Entry
import Transaction
import EntryTransaction
import Ledger


-- an Account caches an account's name, balance (including sub-accounts)
-- and transactions (not including sub-accounts)
type Account = (AccountName,[EntryTransaction],Amount)

aname (a,_,_) = a
atransactions (_,ts,_) = ts
abalance (_,_,b) = b

mkAccount :: Ledger -> AccountName -> Account
mkAccount l a = 
    (a, transactionsInAccountNamed l a, aggregateBalanceInAccountNamed l a)

balanceInAccountNamed :: Ledger -> AccountName -> Amount
balanceInAccountNamed l a = 
    sumEntryTransactions (transactionsInAccountNamed l a)

aggregateBalanceInAccountNamed :: Ledger -> AccountName -> Amount
aggregateBalanceInAccountNamed l a = 
    sumEntryTransactions (aggregateTransactionsInAccountNamed l a)

transactionsInAccountNamed :: Ledger -> AccountName -> [EntryTransaction]
transactionsInAccountNamed l a = 
    ledgerTransactionsMatching (["^" ++ a ++ "$"], []) l

aggregateTransactionsInAccountNamed :: Ledger -> AccountName -> [EntryTransaction]
aggregateTransactionsInAccountNamed l a = 
    ledgerTransactionsMatching (["^" ++ a ++ "(:.+)?$"], []) l

-- a tree of Accounts

atacct = fst . node

addDataToAccountNameTree :: Ledger -> Tree AccountName -> Tree Account
addDataToAccountNameTree l ant = 
    Tree (mkAccount l aname, map (addDataToAccountNameTree l) (branches ant))
        where 
          aname = antacctname ant

showAccountTreeWithBalances :: Ledger -> Int -> Tree Account -> String
showAccountTreeWithBalances l depth at = (showAccountTreesWithBalances l depth) (branches at)

showAccountTreesWithBalances :: Ledger -> Int -> [Tree Account] -> String
showAccountTreesWithBalances _ 0 _ = ""
showAccountTreesWithBalances l depth ats =
    concatMap showAccountBranch ats
        where
          showAccountBranch :: Tree Account -> String
          showAccountBranch at = 
              topacct ++ "\n" ++ subaccts
--               case boring of
--                 True  -> 
--                 False -> 
              where
                topacct = (showAmount bal) ++ "  " ++ (indentAccountName name)
                showAmount amt = printf "%20s" (show amt)
                name = aname $ atacct at
                txns = atransactions $ atacct at
                bal = abalance $ atacct at
                subaccts = (showAccountTreesWithBalances l (depth - 1)) $ branches at
                boring = (length txns == 0) && ((length subaccts) == 1)

-- we want to elide boring accounts in the account tree
--
-- a (2 txns)
--   b (boring acct - 0 txns, exactly 1 sub)
--     c (5 txns)
--       d
-- to:
-- a (2 txns)
--   b:c (5 txns)
--     d

-- elideAccountTree at = at

elideAccountTree :: Tree Account -> Tree Account
elideAccountTree = id

ledgerAccountTree :: Ledger -> Tree Account
ledgerAccountTree l = elideAccountTree $ addDataToAccountNameTree l (ledgerAccountNameTree l)

ledgerAccountsMatching :: Ledger -> [String] -> [Account]
ledgerAccountsMatching l acctpats = undefined

showLedgerAccounts :: Ledger -> Int -> String
showLedgerAccounts l depth = 
    showAccountTreeWithBalances l depth (ledgerAccountTree l)

