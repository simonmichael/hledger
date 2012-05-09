{-|

A 'Ledger' is derived from a 'Journal' by applying a filter specification
to select 'Transaction's and 'Posting's of interest. It contains the
filtered journal and knows the resulting chart of accounts, account
balances, and postings in each account.

-}

module Hledger.Data.Ledger
where
import Data.Map (Map, findWithDefault, fromList)
import Data.Tree
import Test.HUnit
import Text.Printf

import Hledger.Utils
import Hledger.Data.Types
import Hledger.Data.Account (nullacct)
import Hledger.Data.AccountName
import Hledger.Data.Journal
import Hledger.Data.Posting
import Hledger.Data.Query


instance Show Ledger where
    show l = printf "Ledger with %d transactions, %d accounts\n%s"
             (length (jtxns $ ledgerJournal l) +
              length (jmodifiertxns $ ledgerJournal l) +
              length (jperiodictxns $ ledgerJournal l))
             (length $ ledgerAccountNames l)
             (showtree $ ledgerAccountNameTree l)

nullledger :: Ledger
nullledger = Ledger{
      ledgerJournal = nulljournal,
      ledgerAccountNameTree = nullaccountnametree,
      ledgerAccountMap = fromList []
    }

-- | Filter a journal's transactions as specified, and then process them
-- to derive a ledger containing all balances, the chart of accounts,
-- canonicalised commodities etc.
journalToLedger :: FilterSpec -> Journal -> Ledger
journalToLedger fs j = nullledger{ledgerJournal=j',ledgerAccountNameTree=t,ledgerAccountMap=m}
    where j' = filterJournalPostings fs{depth=Nothing} j
          (t, m) = journalAccountInfo j'

-- | Filter a journal's transactions as specified, and then process them
-- to derive a ledger containing all balances, the chart of accounts,
-- canonicalised commodities etc.
-- Like journalToLedger but uses the new queries.
journalToLedger2 :: Query -> Journal -> Ledger
journalToLedger2 m j = nullledger{ledgerJournal=j',ledgerAccountNameTree=t,ledgerAccountMap=amap}
    where j' = filterJournalPostings2 m j
          (t, amap) = journalAccountInfo j'

-- | List a ledger's account names.
ledgerAccountNames :: Ledger -> [AccountName]
ledgerAccountNames = drop 1 . flatten . ledgerAccountNameTree

-- | Get the named account from a ledger.
ledgerAccount :: Ledger -> AccountName -> Account
ledgerAccount l a = findWithDefault nullacct a $ ledgerAccountMap l

-- | List a ledger's accounts, in tree order
ledgerAccounts :: Ledger -> [Account]
ledgerAccounts = drop 1 . flatten . ledgerAccountTree 9999

-- | List a ledger's top-level accounts, in tree order
ledgerTopAccounts :: Ledger -> [Account]
ledgerTopAccounts = map root . branches . ledgerAccountTree 9999

-- | List a ledger's bottom-level (subaccount-less) accounts, in tree order
ledgerLeafAccounts :: Ledger -> [Account]
ledgerLeafAccounts = leaves . ledgerAccountTree 9999

-- | Accounts in ledger whose name matches the pattern, in tree order.
ledgerAccountsMatching :: [String] -> Ledger -> [Account]
ledgerAccountsMatching pats = filter (matchpats pats . aname) . ledgerAccounts

-- | List a ledger account's immediate subaccounts
ledgerSubAccounts :: Ledger -> Account -> [Account]
ledgerSubAccounts l Account{aname=a} = 
    map (ledgerAccount l) $ filter (`isSubAccountNameOf` a) $ ledgerAccountNames l

-- | List a ledger's postings, in the order parsed.
ledgerPostings :: Ledger -> [Posting]
ledgerPostings = journalPostings . ledgerJournal

-- | Get a ledger's tree of accounts to the specified depth.
ledgerAccountTree :: Int -> Ledger -> Tree Account
ledgerAccountTree depth l = treemap (ledgerAccount l) $ treeprune depth $ ledgerAccountNameTree l

-- | Get a ledger's tree of accounts rooted at the specified account.
ledgerAccountTreeAt :: Ledger -> Account -> Maybe (Tree Account)
ledgerAccountTreeAt l acct = subtreeat acct $ ledgerAccountTree 9999 l

-- | The (fully specified) date span containing all the ledger's (filtered) transactions,
-- or DateSpan Nothing Nothing if there are none.
ledgerDateSpan :: Ledger -> DateSpan
ledgerDateSpan = postingsDateSpan . ledgerPostings

-- | All commodities used in this ledger, as a map keyed by symbol.
ledgerCommodities :: Ledger -> Map String Commodity
ledgerCommodities = journalCanonicalCommodities . ledgerJournal

tests_Hledger_Data_Ledger = TestList
 [
 ]

