{-|

A 'Ledger' stores, for efficiency, a 'RawLedger' plus its tree of account
names, a map from account names to 'Account's, and the display precision.
Typically it has also has had the uninteresting 'Entry's and
'Transaction's filtered out.

-}

module Ledger.Ledger (
cacheLedger,
filterLedger,
accountnames,
ledgerAccount,
ledgerTransactions,
ledgerAccountTree,
addDataToAccountNameTree,
printentries,
printregister,
showLedgerAccountBalances,
showAccountTree,
isBoringInnerAccount,
isBoringInnerAccountName,
pruneBoringBranches,
)
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


instance Show Ledger where
    show l = printf "Ledger with %d entries, %d accounts"
             ((length $ entries $ rawledger l) +
              (length $ modifier_entries $ rawledger l) +
              (length $ periodic_entries $ rawledger l))
             (length $ accountnames l)

-- | Convert a raw ledger to a more efficient cached type, described above.  
cacheLedger :: RawLedger -> Ledger
cacheLedger l = 
    let 
        lprecision = maximum $ map (precision . amount) $ rawLedgerTransactions l
        ant = rawLedgerAccountNameTree l
        ans = flatten ant
        ts = rawLedgerTransactions l
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
      Ledger l ant amap lprecision

-- | Remove ledger entries and transactions we are not interested in - 
-- keep only those which fall between the begin and end dates and match the
-- account and description patterns.
filterLedger :: String -> String -> Regex -> Regex -> RawLedger -> RawLedger
filterLedger begin end acctpat descpat = 
    filterEmptyLedgerEntries .
    filterLedgerTransactions acctpat .
    filterLedgerEntriesByDate begin end .
    filterLedgerEntriesByDescription descpat

-- | Keep only entries whose description matches the description pattern.
filterLedgerEntriesByDescription :: Regex -> RawLedger -> RawLedger
filterLedgerEntriesByDescription descpat (RawLedger ms ps es f) = 
    RawLedger ms ps (filter matchdesc es) f
    where
      matchdesc :: Entry -> Bool
      matchdesc e = case matchRegex descpat (edescription e) of
                      Nothing -> False
                      otherwise -> True

-- | Keep only entries which fall between begin and end dates. 
-- We include entries on the begin date and exclude entries on the end
-- date, like ledger.  An empty date string means no restriction.
filterLedgerEntriesByDate :: String -> String -> RawLedger -> RawLedger
filterLedgerEntriesByDate begin end (RawLedger ms ps es f) = 
    RawLedger ms ps (filter matchdate es) f
    where
      matchdate :: Entry -> Bool
      matchdate e = (begin == "" || entrydate >= begindate) && 
                    (end == "" || entrydate < enddate)
                    where 
                      begindate = parsedate begin :: UTCTime
                      enddate   = parsedate end
                      entrydate = parsedate $ edate e

-- | Remove entries which have no transactions.
filterEmptyLedgerEntries :: RawLedger -> RawLedger
filterEmptyLedgerEntries (RawLedger ms ps es f) =
    RawLedger ms ps (filter (not . null . etransactions) es) f

-- | In each ledger entry, filter out transactions which do not match the
-- account pattern. Entries are no longer balanced after this.
filterLedgerTransactions :: Regex -> RawLedger -> RawLedger
filterLedgerTransactions acctpat (RawLedger ms ps es f) = 
    RawLedger ms ps (map filterentrytxns es) f
    where
      filterentrytxns l@(Entry _ _ _ _ _ ts _) = l{etransactions=filter matchtxn ts}
      matchtxn t = case matchRegex acctpat (taccount t) of
                     Nothing -> False
                     otherwise -> True

-- | List a 'Ledger' 's account names.
accountnames :: Ledger -> [AccountName]
accountnames l = drop 1 $ flatten $ accountnametree l

-- | Get the named account from a ledger.
ledgerAccount :: Ledger -> AccountName -> Account
ledgerAccount l a = (accounts l) ! a

-- | List a ledger's transactions.
--
-- NB this sets the amount precisions to that of the highest-precision
-- amount, to help with report output. It should perhaps be done in the
-- display functions, but those are far removed from the ledger. Keep in
-- mind if doing more arithmetic with these.
ledgerTransactions :: Ledger -> [Transaction]
ledgerTransactions l = 
    setprecisions $ rawLedgerTransactions $ rawledger l
    where
      setprecisions = map (transactionSetPrecision (lprecision l))

-- | Get a ledger's tree of accounts to the specified depth.
ledgerAccountTree :: Ledger -> Int -> Tree Account
ledgerAccountTree l depth = 
    addDataToAccountNameTree l $ treeprune depth $ accountnametree l

-- | Convert a tree of account names into a tree of accounts, using their
-- parent ledger.
addDataToAccountNameTree :: Ledger -> Tree AccountName -> Tree Account
addDataToAccountNameTree = treemap . ledgerAccount

-- | Print a print report.
printentries :: Ledger -> IO ()
printentries l = putStr $ showEntries $ setprecisions $ entries $ rawledger l
    where setprecisions = map (entrySetPrecision (lprecision l))
      
-- | Print a register report.
printregister :: Ledger -> IO ()
printregister l = putStr $ showTransactionsWithBalances 
                  (sortBy (comparing date) $ ledgerTransactions l)
                  nullamt{precision=lprecision l}

{-| 
This and the helper functions below generate ledger-compatible balance
report output. Here's how it should work:

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

The standard balance command shows all top-level accounts:

@
\> ledger balance
$ assets      
$ equity
$ expenses    
$ income      
$ liabilities 
@

With an account pattern, show only the ones with matching names:

@
\> ledger balance asset
$ assets      
@

With -s, show all subaccounts of matched accounts:

@
\> ledger -s balance asset
$ assets      
$  cash       
$  checking   
$  saving
@

Elide boring accounts in two ways:

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

If the overall balance of accounts shown is non-zero (eg when using filter
patterns), display it:

@
--------------------
                   $
@
-}
showLedgerAccountBalances :: Ledger -> Int -> String
showLedgerAccountBalances l maxdepth = 
    concatMap (showAccountTree l) bs
    ++
    if isZeroAmount total 
    then ""
    else printf "--------------------\n%20s\n" $ showAmountRounded total
    where 
      bs = branches $ ledgerAccountTree l maxdepth
      total = sum $ map (abalance . root) bs

-- | Get the string representation of a tree of accounts.
-- The ledger from which the accounts come is also required, so that
-- we can check for boring accounts.
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

