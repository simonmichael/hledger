{-|

A compound data type for efficiency. A 'Ledger' caches information derived
from a 'RawLedger' so that it is easy to query. It typically has had
uninteresting 'LedgerTransaction's and 'Posting's removed. It contains

- the original 'RawLedger'

- a tree of 'AccountName's

- a map from account names to 'Account's

- the full text of the journal file, when available

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
import Ledger.LedgerTransaction


instance Show Ledger where
    show l = printf "Ledger with %d transactions, %d accounts\n%s"
             ((length $ ledger_txns $ rawledger l) +
              (length $ modifier_txns $ rawledger l) +
              (length $ periodic_txns $ rawledger l))
             (length $ accountnames l)
             (showtree $ accountnametree l)

-- | Convert a raw ledger to a more efficient cached type, described above.  
cacheLedger :: [String] -> RawLedger -> Ledger
cacheLedger apats l = Ledger{rawledgertext="",rawledger=l,accountnametree=ant,accountmap=acctmap}
    where
      (ant,txnsof,_,inclbalof) = groupTransactions $ filtertxns apats $ rawLedgerTransactions l
      acctmap = Map.fromList [(a, mkacct a) | a <- flatten ant]
          where mkacct a = Account a (txnsof a) (inclbalof a)

-- | Given a list of transactions, return an account name tree and three
-- query functions that fetch transactions, balance, and
-- subaccount-including balance by account name. 
-- This is to factor out common logic from cacheLedger and
-- summariseTransactionsInDateSpan.
groupTransactions :: [Transaction] -> (Tree AccountName,
                                     (AccountName -> [Transaction]), 
                                     (AccountName -> MixedAmount), 
                                     (AccountName -> MixedAmount))
groupTransactions ts = (ant,txnsof,exclbalof,inclbalof)
    where
      txnanames = sort $ nub $ map account ts
      ant = accountNameTreeFrom $ expandAccountNames $ txnanames
      allanames = flatten ant
      txnmap = Map.union (transactionsByAccount ts) (Map.fromList [(a,[]) | a <- allanames])
      balmap = Map.fromList $ flatten $ calculateBalances ant txnsof
      txnsof = (txnmap !) 
      exclbalof = fst . (balmap !)
      inclbalof = snd . (balmap !)
-- debug
--       txnsof a = (txnmap ! (trace ("ts "++a) a))
--       exclbalof a = fst $ (balmap ! (trace ("eb "++a) a))
--       inclbalof a = snd $ (balmap ! (trace ("ib "++a) a))

-- | Add subaccount-excluding and subaccount-including balances to a tree
-- of account names somewhat efficiently, given a function that looks up
-- transactions by account name.
calculateBalances :: Tree AccountName -> (AccountName -> [Transaction]) -> Tree (AccountName, (MixedAmount, MixedAmount))
calculateBalances ant txnsof = addbalances ant
    where 
      addbalances (Node a subs) = Node (a,(bal,bal+subsbal)) subs'
          where
            bal         = sumTransactions $ txnsof a
            subsbal     = sum $ map (snd . snd . root) subs'
            subs'       = map addbalances subs

-- | Convert a list of transactions to a map from account name to the list
-- of all transactions in that account. 
transactionsByAccount :: [Transaction] -> Map.Map AccountName [Transaction]
transactionsByAccount ts = m'
    where
      sortedts = sortBy (comparing account) ts
      groupedts = groupBy (\t1 t2 -> account t1 == account t2) sortedts
      m' = Map.fromList [(account $ head g, g) | g <- groupedts]
-- The special account name "top" can be used to look up all transactions. ?
--      m' = Map.insert "top" sortedts m

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
    map (ledgerAccount l) $ filter (`isSubAccountNameOf` a) $ accountnames l

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
