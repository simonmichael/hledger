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
    show l = printf "Ledger with %d entries, %d accounts: %s"
             ((length $ entries $ rawledger l) +
              (length $ modifier_entries $ rawledger l) +
              (length $ periodic_entries $ rawledger l))
             (length $ accountnames l)
             (show $ accountnames l)
             ++ "\n" ++ (showtree $ accountnametree l)
             ++ "\n" ++ (showtree $ filteredaccountnametree l)

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

A sample account tree (as in the sample.ledger file):

@
 assets
  cash
  checking
  saving
 expenses
  food
  supplies
 income
  gifts
  salary
 liabilities
  debts
@

The balance command shows top-level accounts by default:

@
 \> ledger balance
 $-1  assets
  $2  expenses
 $-2  income
  $1  liabilities
@

With -s (--showsubs), also show the subaccounts:

@
 $-1  assets
 $-2    cash
  $1    saving
  $2  expenses
  $1    food
  $1    supplies
 $-2  income
 $-1    gifts
 $-1    salary
  $1  liabilities:debts
@

- @checking@ is not shown because it has a zero balance and no interesting
  subaccounts.  

- @liabilities@ is displayed only as a prefix because it has no transactions
  of its own and only one subaccount.

With an account pattern, show only the accounts with matching names:

@
 \> ledger balance o
  $1  expenses:food
 $-2  income
--------------------
 $-1  
@

- The o matched @food@ and @income@, so they are shown.

- Parents of matched accounts are also shown for context (@expenses@).

- This time the grand total is also shown, because it is not zero.

Again, -s adds the subaccounts:

@
\> ledger -s balance o
  $1  expenses:food
 $-2  income
 $-1    gifts
 $-1    salary
--------------------
 $-1  
@

- @food@ has no subaccounts. @income@ has two, so they are shown. 

- We do not add the subaccounts of parents included for context (@expenses@).

Here are some rules for account balance display, as seen above:

- grand total is omitted if it is 0

- leaf accounts and branches with 0 balance or 0 transactions are omitted

- inner accounts with 0 transactions and 1 subaccount are displayed inline

- in a filtered report, matched accounts are displayed with their parents
  inline (a consequence of the above)

- in a showsubs report, all subaccounts of matched accounts are displayed

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

