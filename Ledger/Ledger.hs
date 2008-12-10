{-|

A 'Ledger' stores, for efficiency, a 'RawLedger' plus its tree of account
names, and a map from account names to 'Account's. It may also have had
uninteresting 'Entry's and 'Transaction's filtered out. It also stores
the complete ledger file text for the ui command.

-}

module Ledger.Ledger
where
import qualified Data.Map as Map
import Data.Map ((!))
import Ledger.Utils
import Ledger.Types
import Ledger.Amount
import Ledger.AccountName
import Ledger.Account
import Ledger.Transaction
import Ledger.RawLedger
import Ledger.Entry


instance Show Ledger where
    show l = printf "Ledger with %d entries, %d accounts\n%s"
             ((length $ entries $ rawledger l) +
              (length $ modifier_entries $ rawledger l) +
              (length $ periodic_entries $ rawledger l))
             (length $ accountnames l)
             (showtree $ accountnametree l)

-- | Convert a raw ledger to a more efficient cached type, described above.  
cacheLedger :: [String] -> RawLedger -> Ledger
cacheLedger apats l = Ledger{rawledgertext="",rawledger=l,accountnametree=ant,accountmap=acctmap}
    where
      ts = filtertxns apats $ rawLedgerTransactions l
      ant = rawLedgerAccountNameTree l
      anames = flatten ant
      txnmap = Map.union (transactionsByAccount ts) (Map.fromList [(a,[]) | a <- anames])
      subacctsof a = filter (a `isAccountNamePrefixOf`) anames
      subtxnsof a = concat [txnmap ! a | a <- [a] ++ subacctsof a]
      balmap = Map.union 
               (Map.fromList [(a,sumTransactions $ subtxnsof a) | a <- anames]) 
               (Map.fromList [(a,Mixed []) | a <- anames])
      acctmap = Map.fromList [(a, mkacct a) | a <- anames]
      mkacct a = Account a (txnmap ! a) (balmap ! a)

-- | Convert a list of transactions to a map from account name to the list
-- of all transactions in that account.
transactionsByAccount :: [Transaction] -> Map.Map AccountName [Transaction]
transactionsByAccount ts = Map.fromList [(account $ head g, g) | g <- groupedts]
    where
      sortedts = sortBy (comparing account) ts
      groupedts = groupBy (\t1 t2 -> account t1 == account t2) sortedts

filtertxns :: [String] -> [Transaction] -> [Transaction]
filtertxns apats ts = filter (matchpats apats . account) ts

-- | List a ledger's account names.
accountnames :: Ledger -> [AccountName]
accountnames l = drop 1 $ flatten $ accountnametree l

-- | Get the named account from a ledger.
ledgerAccount :: Ledger -> AccountName -> Account
ledgerAccount l a = (accountmap l) ! a

-- | List a ledger's accounts, in tree order
accounts :: Ledger -> [Account]
accounts l = drop 1 $ flatten $ ledgerAccountTree 9999 l

-- | List a ledger's top-level accounts, in tree order
topAccounts :: Ledger -> [Account]
topAccounts l = map root $ branches $ ledgerAccountTree 9999 l

-- | Accounts in ledger whose name matches the pattern, in tree order.
accountsMatching :: [String] -> Ledger -> [Account]
accountsMatching pats l = filter (matchpats pats . aname) $ accounts l

-- | List a ledger account's immediate subaccounts
subAccounts :: Ledger -> Account -> [Account]
subAccounts l Account{aname=a} = 
    map (ledgerAccount l) $ filter (a `isAccountNamePrefixOf`) $ accountnames l

-- | List a ledger's transactions.
ledgerTransactions :: Ledger -> [Transaction]
ledgerTransactions l = rawLedgerTransactions $ rawledger l

-- | Get a ledger's tree of accounts to the specified depth.
ledgerAccountTree :: Int -> Ledger -> Tree Account
ledgerAccountTree depth l = treemap (ledgerAccount l) $ treeprune depth $ accountnametree l

-- | Get a ledger's tree of accounts rooted at the specified account.
ledgerAccountTreeAt :: Ledger -> Account -> Maybe (Tree Account)
ledgerAccountTreeAt l acct = subtreeat acct $ ledgerAccountTree 9999 l

-- | The (explicit) date span containing all the ledger's transactions,
-- or DateSpan Nothing Nothing if there are no transactions.
ledgerDateSpan l
    | null ts = DateSpan Nothing Nothing
    | otherwise = DateSpan (Just $ date $ head ts) (Just $ addDays 1 $ date $ last ts)
    where
      ts = sortBy (comparing date) $ ledgerTransactions l
