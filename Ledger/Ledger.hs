{-|

A compound data type for efficiency. A 'Ledger' caches information derived
from a 'Journal' for easier querying. Also it typically has had
uninteresting 'Transaction's and 'Posting's filtered out. It
contains:

- the original unfiltered 'Journal'

- a tree of 'AccountName's

- a map from account names to 'Account's

- the full text of the journal file, when available

This is the main object you'll deal with as a user of the Ledger
library. The most useful functions also have shorter, lower-case
aliases for easier interaction. Here's an example:

> > import Ledger
> > l <- readLedger "sample.ledger"
> > accountnames l
> ["assets","assets:bank","assets:bank:checking","assets:bank:saving",...
> > accounts l
> [Account assets with 0 txns and $-1 balance,Account assets:bank with...
> > topaccounts l
> [Account assets with 0 txns and $-1 balance,Account expenses with...
> > account l "assets"
> Account assets with 0 txns and $-1 balance
> > accountsmatching ["ch"] l
> accountsmatching ["ch"] l
> [Account assets:bank:checking with 4 txns and $0 balance]
> > subaccounts l (account l "assets")
> subaccounts l (account l "assets")
> [Account assets:bank with 0 txns and $1 balance,Account assets:cash...
> > head $ transactions l
> 2008/01/01 income assets:bank:checking $1 RegularPosting
> > accounttree 2 l
> Node {rootLabel = Account top with 0 txns and 0 balance, subForest = [...
> > accounttreeat l (account l "assets")
> Just (Node {rootLabel = Account assets with 0 txns and $-1 balance, ...
> > datespan l -- disabled
> DateSpan (Just 2008-01-01) (Just 2009-01-01)
> > rawdatespan l
> DateSpan (Just 2008-01-01) (Just 2009-01-01)
> > ledgeramounts l
> [$1,$-1,$1,$-1,$1,$-1,$1,$1,$-2,$1,$-1]
> > commodities l
> [Commodity {symbol = "$", side = L, spaced = False, comma = False, ...


-}

module Ledger.Ledger
where
import qualified Data.Map as Map
import Data.Map ((!))
import Ledger.Utils
import Ledger.Types
import Ledger.Account ()
import Ledger.AccountName
import Ledger.Journal
import Ledger.Posting


instance Show Ledger where
    show l = printf "Ledger with %d transactions, %d accounts\n%s"
             (length (jtxns $ journal l) +
              length (jmodifiertxns $ journal l) +
              length (jperiodictxns $ journal l))
             (length $ accountnames l)
             (showtree $ accountnametree l)

-- | Convert a raw ledger to a more efficient cached type, described above.  
cacheLedger :: [String] -> Journal -> Ledger
cacheLedger apats j = Ledger{journaltext="",journal=j,accountnametree=ant,accountmap=acctmap}
    where
      (ant,psof,_,inclbalof) = groupPostings $ filterPostings apats $ journalPostings j
      acctmap = Map.fromList [(a, mkacct a) | a <- flatten ant]
          where mkacct a = Account a (psof a) (inclbalof a)

-- | Given a list of transactions, return an account name tree and three
-- query functions that fetch transactions, balance, and
-- subaccount-including balance by account name. 
-- This is to factor out common logic from cacheLedger and
-- summarisePostingsInDateSpan.
groupPostings :: [Posting] -> (Tree AccountName,
                             (AccountName -> [Posting]),
                             (AccountName -> MixedAmount), 
                             (AccountName -> MixedAmount))
groupPostings ps = (ant,psof,exclbalof,inclbalof)
    where
      anames = sort $ nub $ map paccount ps
      ant = accountNameTreeFrom $ expandAccountNames anames
      allanames = flatten ant
      pmap = Map.union (postingsByAccount ps) (Map.fromList [(a,[]) | a <- allanames])
      psof = (pmap !) 
      balmap = Map.fromList $ flatten $ calculateBalances ant psof
      exclbalof = fst . (balmap !)
      inclbalof = snd . (balmap !)

-- | Add subaccount-excluding and subaccount-including balances to a tree
-- of account names somewhat efficiently, given a function that looks up
-- transactions by account name.
calculateBalances :: Tree AccountName -> (AccountName -> [Posting]) -> Tree (AccountName, (MixedAmount, MixedAmount))
calculateBalances ant psof = addbalances ant
    where 
      addbalances (Node a subs) = Node (a,(bal,bal+subsbal)) subs'
          where
            bal         = sumPostings $ psof a
            subsbal     = sum $ map (snd . snd . root) subs'
            subs'       = map addbalances subs

-- | Convert a list of postings to a map from account name to that
-- account's postings.
postingsByAccount :: [Posting] -> Map.Map AccountName [Posting]
postingsByAccount ps = m'
    where
      sortedps = sortBy (comparing paccount) ps
      groupedps = groupBy (\p1 p2 -> paccount p1 == paccount p2) sortedps
      m' = Map.fromList [(paccount $ head g, g) | g <- groupedps]

filterPostings :: [String] -> [Posting] -> [Posting]
filterPostings apats = filter (matchpats apats . paccount)

-- | List a ledger's account names.
ledgerAccountNames :: Ledger -> [AccountName]
ledgerAccountNames = drop 1 . flatten . accountnametree

-- | Get the named account from a ledger.
ledgerAccount :: Ledger -> AccountName -> Account
ledgerAccount = (!) . accountmap

-- | List a ledger's accounts, in tree order
ledgerAccounts :: Ledger -> [Account]
ledgerAccounts = drop 1 . flatten . ledgerAccountTree 9999

-- | List a ledger's top-level accounts, in tree order
ledgerTopAccounts :: Ledger -> [Account]
ledgerTopAccounts = map root . branches . ledgerAccountTree 9999

-- | Accounts in ledger whose name matches the pattern, in tree order.
ledgerAccountsMatching :: [String] -> Ledger -> [Account]
ledgerAccountsMatching pats = filter (matchpats pats . aname) . accounts

-- | List a ledger account's immediate subaccounts
ledgerSubAccounts :: Ledger -> Account -> [Account]
ledgerSubAccounts l Account{aname=a} = 
    map (ledgerAccount l) $ filter (`isSubAccountNameOf` a) $ accountnames l

-- | List a ledger's postings, in the order parsed.
ledgerPostings :: Ledger -> [Posting]
ledgerPostings = journalPostings . journal

-- | Get a ledger's tree of accounts to the specified depth.
ledgerAccountTree :: Int -> Ledger -> Tree Account
ledgerAccountTree depth l = treemap (ledgerAccount l) $ treeprune depth $ accountnametree l

-- | Get a ledger's tree of accounts rooted at the specified account.
ledgerAccountTreeAt :: Ledger -> Account -> Maybe (Tree Account)
ledgerAccountTreeAt l acct = subtreeat acct $ ledgerAccountTree 9999 l

-- | The (fully specified) date span containing all the ledger's (filtered) transactions,
-- or DateSpan Nothing Nothing if there are none.
ledgerDateSpan :: Ledger -> DateSpan
ledgerDateSpan l
    | null ps = DateSpan Nothing Nothing
    | otherwise = DateSpan (Just $ postingDate $ head ps) (Just $ addDays 1 $ postingDate $ last ps)
    where
      ps = sortBy (comparing postingDate) $ ledgerPostings l

-- | Convenience aliases.
accountnames :: Ledger -> [AccountName]
accountnames = ledgerAccountNames

account :: Ledger -> AccountName -> Account
account = ledgerAccount

accounts :: Ledger -> [Account]
accounts = ledgerAccounts

topaccounts :: Ledger -> [Account]
topaccounts = ledgerTopAccounts

accountsmatching :: [String] -> Ledger -> [Account]
accountsmatching = ledgerAccountsMatching

subaccounts :: Ledger -> Account -> [Account]
subaccounts = ledgerSubAccounts

postings :: Ledger -> [Posting]
postings = ledgerPostings

commodities :: Ledger -> [Commodity]
commodities = nub . journalCommodities . journal

accounttree :: Int -> Ledger -> Tree Account
accounttree = ledgerAccountTree

accounttreeat :: Ledger -> Account -> Maybe (Tree Account)
accounttreeat = ledgerAccountTreeAt

-- datespan :: Ledger -> DateSpan
-- datespan = ledgerDateSpan

rawdatespan :: Ledger -> DateSpan
rawdatespan = journalDateSpan . journal

ledgeramounts :: Ledger -> [MixedAmount]
ledgeramounts = journalAmounts . journal
