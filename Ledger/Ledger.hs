{-|

A 'Ledger' stores, for efficiency, a 'RawLedger' plus its tree of account
names, a map from account names to 'Account's, and the display precision.

-}

module Ledger.Ledger
where
import qualified Data.Map as Map
import Data.Map ((!))
import Ledger.Utils
import Ledger.Types
import Ledger.Amount
import Ledger.Account
import Ledger.AccountName
import Ledger.Transaction
import Ledger.RawLedger
import Ledger.Entry


rawLedgerTransactions :: RawLedger -> [Transaction]
rawLedgerTransactions = txns . entries
    where
      txns :: [Entry] -> [Transaction]
      txns es = concat $ map flattenEntry $ zip es (iterate (+1) 1)

rawLedgerAccountNamesUsed :: RawLedger -> [AccountName]
rawLedgerAccountNamesUsed = accountNamesFromTransactions . rawLedgerTransactions

rawLedgerAccountNames :: RawLedger -> [AccountName]
rawLedgerAccountNames = sort . expandAccountNames . rawLedgerAccountNamesUsed

rawLedgerAccountNameTree :: RawLedger -> Tree AccountName
rawLedgerAccountNameTree l = accountNameTreeFrom $ rawLedgerAccountNames l


instance Show Ledger where
    show l = printf "Ledger with %d entries, %d accounts"
             ((length $ entries $ rawledger l) +
              (length $ modifier_entries $ rawledger l) +
              (length $ periodic_entries $ rawledger l))
             (length $ accountnames l)

-- | at startup, to improve performance, we refine the parsed ledger entries:
-- 1. filter based on account/description patterns, if any
-- 2. cache per-account info
-- 3. figure out the precision(s) to use
cacheLedger :: RawLedger -> (Regex,Regex) -> Ledger
cacheLedger l pats = 
    let 
        lprecision = maximum $ map (precision . amount) $ rawLedgerTransactions l
        l' = filterLedgerEntries pats l
        l'' = filterLedgerTransactions pats l'
        ant = rawLedgerAccountNameTree l''
        ans = flatten ant
        ts = rawLedgerTransactions l''
        sortedts = sortBy (comparing account) ts
        groupedts = groupBy (\t1 t2 -> account t1 == account t2) sortedts
        tmap = Map.union 
               (Map.fromList [(account $ head g, g) | g <- groupedts])
               (Map.fromList [(a,[]) | a <- ans])
        txns = (tmap !)
        subaccts a = filter (isAccountNamePrefixOf a) ans
        subtxns a = concat [txns a | a <- [a] ++ subaccts a]
        bmap = Map.union 
               (Map.fromList [(a, (sumTransactions $ subtxns a){precision=lprecision}) | a <- ans])
               (Map.fromList [(a,nullamt) | a <- ans])
        amap = Map.fromList [(a, Account a (tmap ! a) (bmap ! a)) | a <- ans]
    in
      Ledger l'' ant amap lprecision

-- | keep only entries whose description matches one of the description
-- patterns, and which have at least one transaction matching one of the
-- account patterns. (One or both patterns may be the wildcard.)
filterLedgerEntries :: (Regex,Regex) -> RawLedger -> RawLedger
filterLedgerEntries (acctpat,descpat) (RawLedger ms ps es f) = 
    RawLedger ms ps filteredentries f
    where
      filteredentries :: [Entry]
      filteredentries = (filter matchdesc $ filter (any matchtxn . etransactions) es)
      matchtxn :: RawTransaction -> Bool
      matchtxn t = case matchRegex acctpat (taccount t) of
                     Nothing -> False
                     otherwise -> True
      matchdesc :: Entry -> Bool
      matchdesc e = case matchRegex descpat (edescription e) of
                      Nothing -> False
                      otherwise -> True

-- | in each ledger entry, filter out transactions which do not match the
-- account patterns.  (Entries are no longer balanced after this.)
filterLedgerTransactions :: (Regex,Regex) -> RawLedger -> RawLedger
filterLedgerTransactions (acctpat,descpat) (RawLedger ms ps es f) = 
    RawLedger ms ps (map filterentrytxns es) f
    where
      filterentrytxns l@(Entry _ _ _ _ _ ts _) = l{etransactions=filter matchtxn ts}
      matchtxn t = case matchRegex acctpat (taccount t) of
                     Nothing -> False
                     otherwise -> True

accountnames :: Ledger -> [AccountName]
accountnames l = flatten $ accountnametree l

ledgerAccount :: Ledger -> AccountName -> Account
ledgerAccount l a = (accounts l) ! a

-- | This sets all amount precisions to that of the highest-precision
-- amount, to help with report output. It should perhaps be done in the
-- display functions, but those are far removed from the ledger. Keep in
-- mind if doing more arithmetic with these.
ledgerTransactions :: Ledger -> [Transaction]
ledgerTransactions l = 
    setprecisions $ rawLedgerTransactions $ rawledger l
    where
      setprecisions = map (transactionSetPrecision (lprecision l))

ledgerAccountTree :: Ledger -> Int -> Tree Account
ledgerAccountTree l depth = 
    addDataToAccountNameTree l $ treeprune depth $ accountnametree l

addDataToAccountNameTree :: Ledger -> Tree AccountName -> Tree Account
addDataToAccountNameTree = treemap . ledgerAccount

-- | for the print command
printentries :: Ledger -> IO ()
printentries l = putStr $ showEntries $ setprecisions $ entries $ rawledger l
    where setprecisions = map (entrySetPrecision (lprecision l))
      
-- | for the register command
printregister :: Ledger -> IO ()
printregister l = putStr $ showTransactionsWithBalances 
                  (sortBy (comparing date) $ ledgerTransactions l)
                  nullamt{precision=lprecision l}

{-| 
This and the functions below generate ledger-compatible balance report
output. Here's how it should work:

a sample account tree:

@
assets
 cash
 checking
 saving
equity
expenses
 food
 shelter
income
 salary
liabilities
 debts
@

the standard balance command shows all top-level accounts:

@
\> ledger balance
$ assets      
$ equity
$ expenses    
$ income      
$ liabilities 
@

with an account pattern, show only the ones with matching names:

@
\> ledger balance asset
$ assets      
@

with -s, show all subaccounts of matched accounts:

@
\> ledger -s balance asset
$ assets      
$  cash       
$  checking   
$  saving
@

we elide boring accounts in two ways:

- leaf accounts and branches with 0 balance or 0 transactions are omitted

- inner accounts with 0 transactions and 1 subaccount are displayed inline

so this:

@
a (0 txns)
  b (0 txns)
    c
      d
e (0 txns)
  f
  g
h (0 txns)
  i (0 balance)
@

is displayed like:

@
a:b:c
  d
e
  f
  g
@
-}
showLedgerAccounts :: Ledger -> Int -> String
showLedgerAccounts l maxdepth = 
    concatMap 
    (showAccountTree l) 
    (branches $ ledgerAccountTree l maxdepth)
-- XXX need to add up and show balances too

showAccountTree :: Ledger -> Tree Account -> String
showAccountTree l = showAccountTree' l 0 . pruneBoringBranches

showAccountTree' :: Ledger -> Int -> Tree Account -> String
showAccountTree' l indentlevel t
    -- skip a boring inner account
    | length subs > 0 && isBoringInnerAccount l acct = subsindented 0
    -- otherwise show normal indented account name with balance, 
    -- prefixing the names of any boring parents
    | otherwise = 
        bal ++ "  " ++ indent ++ prefix ++ leafname ++ "\n" ++ (subsindented 1)
    where
      acct = root t
      subs = branches t
      subsindented i = concatMap (showAccountTree' l (indentlevel+i)) subs
      bal = printf "%20s" $ show $ abalance $ acct
      indent = replicate (indentlevel * 2) ' '
      prefix = concatMap (++ ":") $ map accountLeafName $ reverse boringparents
      boringparents = takeWhile (isBoringInnerAccountName l) $ parentAccountNames $ aname acct
      leafname = accountLeafName $ aname acct

-- | Is this account a boring inner account in this ledger ? 
-- Boring inner accounts have no transactions and one subaccount.
isBoringInnerAccount :: Ledger -> Account -> Bool
isBoringInnerAccount l a
    | name == "top" = False
    | (length txns == 0) && ((length subs) == 1) = True
    | otherwise = False
    where      
      name = aname a
      txns = atransactions a
      subs = subAccountNamesFrom (accountnames l) name

-- | Is the named account a boring inner account in this ledger ?
isBoringInnerAccountName :: Ledger -> AccountName -> Bool
isBoringInnerAccountName l = isBoringInnerAccount l . ledgerAccount l

-- | Remove boring branches (and leaves) from a tree of accounts.
-- A boring branch contains only accounts which have a 0 balance or no
-- transactions.
pruneBoringBranches :: Tree Account -> Tree Account
pruneBoringBranches =
    treefilter hastxns . treefilter hasbalance
    where 
      hasbalance = (/= 0) . abalance
      hastxns = (> 0) . length . atransactions

