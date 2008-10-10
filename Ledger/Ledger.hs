{-|

A 'Ledger' stores, for efficiency, a 'RawLedger' plus its tree of account
names, a map from account names to 'Account's, and the display precision.
Typically it has also has had the uninteresting 'Entry's filtered out.
In addition, it stores the account filter pattern and a second set of fields
providing the filtered entries & transactions.

-}

module Ledger.Ledger
where
import qualified Data.Map as Map
import Data.Map ((!))
import Ledger.Utils
import Ledger.Types
import Ledger.Amount
import Ledger.AccountName
import Ledger.Transaction
import Ledger.RawLedger
import Ledger.Entry


ledgertests = TestList [
              ]

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

-- | List a 'Ledger' 's account names.
accountnames :: Ledger -> [AccountName]
accountnames l = drop 1 $ flatten $ accountnametree l

-- | List a 'Ledger' 's account names filtered by the account match pattern.
filteredaccountnames :: Ledger -> [AccountName]
filteredaccountnames l = filter (containsRegex (acctpat l) . accountLeafName) $ accountnames l

-- | Get the named account from a ledger.
ledgerAccount :: Ledger -> AccountName -> Account
ledgerAccount l a = (accountmap l) ! a

-- | Get the named filtered account from a ledger.
ledgerFilteredAccount :: Ledger -> AccountName -> Account
ledgerFilteredAccount l a = (filteredaccountmap l) ! a

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

