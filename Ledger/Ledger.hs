{-|

A 'Ledger' stores, for efficiency, a 'RawLedger' plus its tree of account
names, a map from account names to 'Account's, and the display precision.
Typically it has also has had the uninteresting 'Entry's filtered out.
In addition, it stores the account filter pattern and a second set of fields
providing the filtered entries & transactions.

-}

module Ledger.Ledger (
cacheLedger,
filterLedgerEntries,
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
-- pruneBoringBranches,
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
cacheLedger :: Regex -> RawLedger -> Ledger
cacheLedger acctpat l = 
    let 
        ant = rawLedgerAccountNameTree l
        anames = flatten ant
        ts = rawLedgerTransactions l
        sortedts = sortBy (comparing account) ts
        groupedts = groupBy (\t1 t2 -> account t1 == account t2) sortedts
        txnmap = Map.union 
               (Map.fromList [(account $ head g, g) | g <- groupedts])
               (Map.fromList [(a,[]) | a <- anames])
        txnsof = (txnmap !)
        subacctsof a = filter (isAccountNamePrefixOf a) anames
        subtxnsof a = concat [txnsof a | a <- [a] ++ subacctsof a]
        balmap = Map.union 
               (Map.fromList [(a, (sumTransactions $ subtxnsof a){precision=maxprecision}) | a <- anames])
               (Map.fromList [(a,nullamt) | a <- anames])
        amap = Map.fromList [(a, Account a (txnmap ! a) (balmap ! a)) | a <- anames]
        -- the same again, considering only accounts and transactions matching the account pattern
        matchacct :: AccountName -> Bool
        matchacct = containsRegex acctpat . accountLeafName
        filteredant = treefilter matchacct ant
        -- rebuild the tree after filtering to include all parents
        filteredanames = flatten $ accountNameTreeFrom $ filter matchacct anames
        filteredts = filter (matchacct . account) ts
        filteredsortedts = sortBy (comparing account) filteredts
        filteredgroupedts = groupBy (\t1 t2 -> account t1 == account t2) filteredsortedts
        filteredtxnmap = Map.union 
               (Map.fromList [(account $ head g, g) | g <- filteredgroupedts])
               (Map.fromList [(a,[]) | a <- filteredanames])
        filteredtxnsof = (filteredtxnmap !)
        filteredsubacctsof a = filter (isAccountNamePrefixOf a) filteredanames
        filteredsubtxnsof a = concat [filteredtxnsof a | a <- [a] ++ filteredsubacctsof a]
        filteredbalmap = Map.union 
               (Map.fromList [(a, (sumTransactions $ filteredsubtxnsof a){precision=maxprecision}) | a <- filteredanames])
               (Map.fromList [(a,nullamt) | a <- filteredanames])
        filteredamap = Map.fromList [(a, Account a (filteredtxnmap ! a) (filteredbalmap ! a)) | a <- filteredanames]

        maxprecision = maximum $ map (precision . amount) ts
    in
      Ledger l ant amap maxprecision acctpat filteredant filteredamap

-- | Remove ledger entries we are not interested in.
-- Keep only those which fall between the begin and end dates, match the
-- description patterns, or transact with an account matching the account
-- patterns.
filterLedgerEntries :: String -> String -> Regex -> Regex -> RawLedger -> RawLedger
filterLedgerEntries begin end acctpat descpat = 
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

-- | List a 'Ledger' 's account names.
accountnames :: Ledger -> [AccountName]
accountnames l = drop 1 $ flatten $ accountnametree l

-- | List a 'Ledger' 's account names filtered by the account match pattern.
filteredaccountnames :: Ledger -> [AccountName]
filteredaccountnames l = filter (containsRegex (acctpat l) . accountLeafName) $ accountnames l

-- | Get the named account from a ledger.
ledgerAccount :: Ledger -> AccountName -> Account
ledgerAccount l a = (accounts l) ! a

-- | Get the named filtered account from a ledger.
ledgerFilteredAccount :: Ledger -> AccountName -> Account
ledgerFilteredAccount l a = (filteredaccounts l) ! a

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
ledgerAccountTree :: Int -> Ledger -> Tree Account
ledgerAccountTree depth l = 
    addDataToAccountNameTree l depthpruned
    where
      nametree = filteredaccountnametree l --
      depthpruned = treeprune depth nametree

-- | Get a ledger's tree of accounts to the specified depth, filtered by
-- the account pattern.
ledgerFilteredAccountTree :: Int -> Regex -> Ledger -> Tree Account
ledgerFilteredAccountTree depth acctpat l = 
    addFilteredDataToAccountNameTree l $ treeprune depth $ filteredaccountnametree l

-- | Convert a tree of account names into a tree of accounts, using their
-- parent ledger.
addDataToAccountNameTree :: Ledger -> Tree AccountName -> Tree Account
addDataToAccountNameTree = treemap . ledgerAccount

-- | Convert a tree of account names into a tree of accounts, using their
-- parent ledger's filtered account data.
addFilteredDataToAccountNameTree :: Ledger -> Tree AccountName -> Tree Account
addFilteredDataToAccountNameTree l = treemap (ledgerFilteredAccount l)

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
    concatMap (showAccountTree l maxdepth) acctbranches
    ++
    if isZeroAmount total 
    then ""
    else printf "--------------------\n%20s\n" $ showAmountRounded total
    where 
      acctbranches = branches $ ledgerAccountTree maxdepth l
      filteredacctbranches = branches $ ledgerFilteredAccountTree maxdepth (acctpat l) l
      total = sum $ map (abalance . root) filteredacctbranches

-- | Get the string representation of a tree of accounts.
-- The ledger from which the accounts come is also required, so that
-- we can check for boring accounts.
showAccountTree :: Ledger -> Int -> Tree Account -> String
showAccountTree l maxdepth = showAccountTree' l maxdepth 0 ""

showAccountTree' :: Ledger -> Int -> Int -> String -> Tree Account -> String
showAccountTree' l maxdepth indentlevel prefix t
    -- merge boring inner account names with the next line
    | isBoringInnerAccount l maxdepth acct = subsindented 0 (fullname++":")
    -- ditto with unmatched parent accounts when filtering by account
    |  filtering && doesnotmatch = subsindented 0 (fullname++":")
    -- otherwise show this account's name & balance
    | otherwise = bal ++ "  " ++ indent ++ prefix ++ leafname ++ "\n" ++ (subsindented 1 "")
    where
      acct = root t
      subs = branches t
      subsindented i p = concatMap (showAccountTree' l maxdepth (indentlevel+i) p) subs
      bal = printf "%20s" $ show $ abalance $ acct
      indent = replicate (indentlevel * 2) ' '
      fullname = aname acct
      leafname = accountLeafName fullname
      filtering = filteredaccountnames l /= (accountnames l)
      doesnotmatch = not (containsRegex (acctpat l) leafname)

-- | Is this account a boring inner account in this ledger ? 
-- Boring inner accounts have no transactions, one subaccount,
-- and depth less than the maximum display depth.
-- Also, they are unmatched parent accounts when account matching is in effect.
isBoringInnerAccount :: Ledger -> Int -> Account -> Bool
isBoringInnerAccount l maxdepth a
    | name == "top" = False
    | depth < maxdepth && numtxns == 0 && numsubs == 1 = True
    | otherwise = False
    where      
      name = aname a
      depth = accountNameLevel name
      numtxns = length $ atransactions a
      -- how many (filter-matching) subaccounts has this account ?
      numsubs = length $ subAccountNamesFrom (filteredaccountnames l) name

-- | Is the named account a boring inner account in this ledger ?
isBoringInnerAccountName :: Ledger -> Int -> AccountName -> Bool
isBoringInnerAccountName l maxdepth = isBoringInnerAccount l maxdepth . ledgerAccount l

