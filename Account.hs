module Account
where
import qualified Data.Map as Map

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

nullacct = ("",[],nullamt)

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

addDataToAccountNameTree :: Ledger -> Tree AccountName -> Tree Account
addDataToAccountNameTree l ant = 
    Node 
    (mkAccount l $ rootLabel ant) 
    (map (addDataToAccountNameTree l) $ subForest ant)

-- would be straightforward except we want to elide boring accounts when
-- displaying account trees:
-- a (0 txns, only 1 subacct)
--   b (another boring acct.)
--     c
--       d
-- becomes:
-- a:b:c
--   d
showAccountTree :: Ledger -> Int -> Int -> Tree Account -> String
showAccountTree _ 0 _ _ = ""
showAccountTree l maxdepth indentlevel t
    -- if this acct is boring, don't show it (unless this is as deep as we're going)
    | (boringacct && (maxdepth > 1)) = subacctsindented 0

    -- otherwise show normal indented account name with balance
    -- if this acct has one or more boring parents, prepend their names
    | otherwise = 
        bal ++ "  " ++ indent ++ parentnames ++ leafname ++ "\n" ++ (subacctsindented 1)

    where
      boringacct = isBoringAccount2 l name
      boringparents = takeWhile (isBoringAccount2 l) $ parentAccountNames name
      bal = printf "%20s" $ show $ abalance $ rootLabel t
      indent = replicate (indentlevel * 2) ' '
      parentnames = concatMap (++ ":") $ map accountLeafName boringparents
      leafname = accountLeafName name
      name = aname $ rootLabel t
      subacctsindented i = 
          case maxdepth > 1 of
            True -> concatMap (showAccountTree l (maxdepth-1) (indentlevel+i)) $ subForest t
            False -> ""

isBoringAccount :: Tree Account -> Bool
isBoringAccount at = 
    (length txns == 0) && ((length subaccts) == 1) && (not $ name == "top")
        where
          a = rootLabel at
          name = aname a
          txns = atransactions a
          subaccts = subForest at

isBoringAccount2 :: Ledger -> AccountName -> Bool
isBoringAccount2 l a
    | a == "top" = False
    | (length txns == 0) && ((length subs) == 1) = True
    | otherwise = False
    where
      txns = transactionsInAccountNamed l a
      subs = subAccountNamesFrom (ledgerAccountNames l) a

ledgerAccountTree :: Ledger -> Tree Account
ledgerAccountTree l = addDataToAccountNameTree l (ledgerAccountNameTree l)

-- ledgerAccountTreeForAccount :: Ledger -> AccountName -> Tree Account
-- ledgerAccountTreeForAccount l a = addDataToAccountNameTree l (ledgerAccountNameTree l)

ledgerAccountsMatching :: Ledger -> [String] -> [Account]
ledgerAccountsMatching l acctpats = undefined

showLedgerAccounts :: Ledger -> Int -> String
showLedgerAccounts l maxdepth = 
    concatMap (showAccountTree l maxdepth 0) (subForest (ledgerAccountTree l))
