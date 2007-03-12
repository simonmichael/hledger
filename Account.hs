module Account
where
import qualified Data.Map as Map

import Utils
import BasicTypes
import AccountName
import Amount
import Entry
import Transaction
import EntryTransaction
import Ledger


-- an Account caches an account's name, balance (including sub-accounts)
-- and transactions (excluding sub-accounts)
data Account = Account {
      aname :: AccountName, 
      atransactions :: [EntryTransaction],
      abalance :: Amount
}

instance Show Account where
    show (Account a ts b) = printf "Account %s with %d transactions" a $ length ts

nullacct = Account "" [] nullamt

ledgerAccount :: Ledger -> AccountName -> Account
ledgerAccount l a = 
    Account 
    a 
    (transactionsInAccountNamed l a) 
    (aggregateBalanceInAccountNamed l a)

-- queries

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

-- build a tree of Accounts
addDataToAccountNameTree :: Ledger -> Tree AccountName -> Tree Account
addDataToAccountNameTree l ant = 
    Node 
    (ledgerAccount l $ root ant) 
    (map (addDataToAccountNameTree l) $ branches ant)

-- balance report support
--
-- some examples, ignoring the issue of eliding boring accounts
-- here is a sample account tree:
--
-- assets
--  cash
--  checking
--  saving
-- equity
-- expenses
--  food
--  shelter
-- income
--  salary
-- liabilities
--  debts
--
-- standard balance command shows all top-level accounts:
--
-- > ledger bal
-- $ assets      
-- $ equity
-- $ expenses    
-- $ income      
-- $ liabilities 
--
-- with an account pattern, show only the ones with matching names:
--
-- > ledger bal asset
-- $ assets      
--
-- with -s, show all subaccounts of matched accounts:
--
-- > ledger -s bal asset
-- $ assets      
-- $  cash       
-- $  checking   
-- $  saving

showLedgerAccounts :: Ledger -> [String] -> Bool -> Int -> String
showLedgerAccounts l acctpats showsubs maxdepth = 
    concatMap 
    (showAccountTree l) 
    (branches (ledgerAccountTreeMatching l acctpats showsubs maxdepth))

ledgerAccountTreeMatching :: Ledger -> [String] -> Bool -> Int -> Tree Account
ledgerAccountTreeMatching l [] showsubs maxdepth = 
    ledgerAccountTreeMatching l [".*"] showsubs maxdepth
ledgerAccountTreeMatching l acctpats showsubs maxdepth = 
    addDataToAccountNameTree l $ 
    filterAccountNameTree acctpats showsubs maxdepth $ 
    ledgerAccountNameTree l

-- when displaying an account tree, we elide boring accounts.
-- 1. leaf accounts and branches with 0 balance or 0 transactions are omitted
-- 2. inner accounts with 0 transactions and 1 subaccount are displayed as
--    a prefix of the sub
--
-- so, for example:
--
-- a (0 txns)
--   b (0 txns)
--     c
--       d
-- e (0 txns)
--   f
--   g
-- h (0 txns)
--   i (0 balance)
--
-- displays as:
--
-- a:b:c
--   d
-- e
--   f
--   g
showAccountTree :: Ledger -> Tree Account -> String
showAccountTree l = showAccountTree' l 0 . interestingAccountsFrom

showAccountTree' l indentlevel t
    -- if this acct is boring, don't show it
    | isBoringInnerAccount l acct = subacctsindented 0
    -- otherwise show normal indented account name with balance, 
    -- prefixing the names of any boring parents
    | otherwise = 
        bal ++ "  " ++ indent ++ prefix ++ leafname ++ "\n" ++ (subacctsindented 1)
    where
      acct = root t
      subacctsindented i = concatMap (showAccountTree' l (indentlevel+i)) $ branches t
      bal = printf "%20s" $ show $ abalance $ acct
      indent = replicate (indentlevel * 2) ' '
      prefix = concatMap (++ ":") $ map accountLeafName boringparents
      boringparents = takeWhile (isBoringInnerAccountName l) $ parentAccountNames $ aname acct
      leafname = accountLeafName $ aname acct

isBoringInnerAccount :: Ledger -> Account -> Bool
isBoringInnerAccount l a
    | name == "top" = False
    | (length txns == 0) && ((length subs) == 1) = True
    | otherwise = False
    where
      name = aname a
      txns = atransactions a
      subs = subAccountNamesFrom (ledgerAccountNames l) name

-- darnit, still need this
isBoringInnerAccountName :: Ledger -> AccountName -> Bool
isBoringInnerAccountName l name
    | name == "top" = False
    | (length txns == 0) && ((length subs) == 1) = True
    | otherwise = False
    where
      txns = transactionsInAccountNamed l name
      subs = subAccountNamesFrom (ledgerAccountNames l) name

interestingAccountsFrom :: Tree Account -> Tree Account
interestingAccountsFrom =
    treefilter hastxns . treefilter hasbalance
    where 
      hasbalance = (/= 0) . abalance
      hastxns = (> 0) . length . atransactions

