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
             (length (jtxns $ journal l) +
              length (jmodifiertxns $ journal l) +
              length (jperiodictxns $ journal l))
             (length $ accountnames l)
             (showtree $ accountnametree l)

nullledger :: Ledger
nullledger = Ledger{
      journal = nulljournal,
      accountnametree = nullaccountnametree,
      accountmap = fromList []
    }

-- | Filter a journal's transactions as specified, and then process them
-- to derive a ledger containing all balances, the chart of accounts,
-- canonicalised commodities etc.
journalToLedger :: FilterSpec -> Journal -> Ledger
journalToLedger fs j = nullledger{journal=j',accountnametree=t,accountmap=m}
    where j' = filterJournalPostings fs{depth=Nothing} j
          (t, m) = journalAccountInfo j'

-- | Filter a journal's transactions as specified, and then process them
-- to derive a ledger containing all balances, the chart of accounts,
-- canonicalised commodities etc.
-- Like journalToLedger but uses the new matchers.
journalToLedger2 :: Matcher -> Journal -> Ledger
journalToLedger2 m j = nullledger{journal=j',accountnametree=t,accountmap=amap}
    where j' = filterJournalPostings2 m j
          (t, amap) = journalAccountInfo j'

-- | List a ledger's account names.
ledgerAccountNames :: Ledger -> [AccountName]
ledgerAccountNames = drop 1 . flatten . accountnametree

-- | Get the named account from a ledger.
ledgerAccount :: Ledger -> AccountName -> Account
ledgerAccount l a = findWithDefault nullacct a $ accountmap l

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
ledgerDateSpan = postingsDateSpan . ledgerPostings

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

commodities :: Ledger -> Map String Commodity
commodities = journalCanonicalCommodities . journal

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

tests_Hledger_Data_Ledger = TestList
 [
 ]

