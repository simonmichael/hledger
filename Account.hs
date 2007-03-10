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
data Account = Account {
      aname :: AccountName, 
      atransactions :: [EntryTransaction],
      abalance :: Amount
}

nullacct = Account "" [] nullamt

mkAccount :: Ledger -> AccountName -> Account
mkAccount l a = 
    Account 
    a 
    (transactionsInAccountNamed l a) 
    (aggregateBalanceInAccountNamed l a)

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
    (mkAccount l $ root ant) 
    (map (addDataToAccountNameTree l) $ branches ant)

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
--     | (boringacct && (maxdepth > 1)) = subacctsindented 0

    -- otherwise show normal indented account name with balance
    -- if this acct has one or more boring parents, prepend their names
    | otherwise = 
        bal ++ "  " ++ indent ++ parentnames ++ leafname ++ "\n" ++ (subacctsindented 1)

    where
      boringacct = isBoringAccount2 l name
      boringparents = takeWhile (isBoringAccount2 l) $ parentAccountNames name
      bal = printf "%20s" $ show $ abalance $ root t
      indent = replicate (indentlevel * 2) ' '
      parentnames = concatMap (++ ":") $ map accountLeafName boringparents
      leafname = accountLeafName name
      name = aname $ root t
      subacctsindented i = 
          case maxdepth > 1 of
            True -> concatMap (showAccountTree l (maxdepth-1) (indentlevel+i)) $ branches t
            False -> ""

isBoringAccount :: Tree Account -> Bool
isBoringAccount at = 
    (length txns == 0) && ((length subaccts) == 1) && (not $ name == "top")
        where
          a = root at
          name = aname a
          txns = atransactions a
          subaccts = branches at

isBoringAccount2 :: Ledger -> AccountName -> Bool
isBoringAccount2 l a
    | a == "top" = False
    | (length txns == 0) && ((length subs) == 1) = True
    | otherwise = False
    where
      txns = transactionsInAccountNamed l a
      subs = subAccountNamesFrom (ledgerAccountNames l) a

ledgerAccountTreeMatching :: Ledger -> Bool -> [String] -> Tree Account
ledgerAccountTreeMatching l showsubs [] = 
    ledgerAccountTreeMatching l showsubs [".*"]
ledgerAccountTreeMatching l showsubs acctpats = 
    addDataToAccountNameTree l $ 
    filterAccountNameTree acctpats $ 
    ledgerAccountNameTree l

showLedgerAccounts :: Ledger -> Bool -> [String] -> String
showLedgerAccounts l showsubs acctpats = 
    concatMap 
    (showAccountTree l 999 0) 
    (branches (ledgerAccountTreeMatching l showsubs acctpats))
