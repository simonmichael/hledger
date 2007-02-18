module Account
where
import Utils
import BasicTypes
import AccountName
import Entry
import Transaction
import EntryTransaction
import Ledger


-- an Account caches an account's name, balance and transactions for convenience
type Account = (AccountName,[EntryTransaction],Amount)

aname (a,_,_) = a
atransactions (_,ts,_) = ts
abalance (_,_,b) = b

mkAccount :: Ledger -> AccountName -> Account
mkAccount l a = (a, accountNameTransactionsNoSubs l a, accountNameBalance l a)

accountNameBalance :: Ledger -> AccountName -> Amount
accountNameBalance l a = sumEntryTransactions (accountNameTransactions l a)

accountNameTransactions :: Ledger -> AccountName -> [EntryTransaction]
accountNameTransactions l a = ledgerTransactionsMatching (["^" ++ a ++ "(:.+)?$"], []) l

accountNameBalanceNoSubs :: Ledger -> AccountName -> Amount
accountNameBalanceNoSubs l a = sumEntryTransactions (accountNameTransactionsNoSubs l a)

accountNameTransactionsNoSubs :: Ledger -> AccountName -> [EntryTransaction]
accountNameTransactionsNoSubs l a = ledgerTransactionsMatching (["^" ++ a ++ "$"], []) l

-- showAccountNamesWithBalances :: [(AccountName,String)] -> Ledger -> String
-- showAccountNamesWithBalances as l =
--     unlines $ map (showAccountNameAndBalance l) as

-- showAccountNameAndBalance :: Ledger -> (AccountName, String) -> String
-- showAccountNameAndBalance l (a, adisplay) =
--     printf "%20s  %s" (showBalance $ accountBalance l a) adisplay


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

