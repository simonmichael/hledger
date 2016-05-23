{-|

A 'Ledger' is derived from a 'Journal' by applying a filter specification
to select 'Transaction's and 'Posting's of interest. It contains the
filtered journal and knows the resulting chart of accounts, account
balances, and postings in each account.

-}

module Hledger.Data.Ledger
where
import qualified Data.Map as M
import Safe (headDef)
import Test.HUnit
import Text.Printf

import Hledger.Data.Types
import Hledger.Data.Account
import Hledger.Data.Journal
import Hledger.Data.Posting
import Hledger.Query


instance Show Ledger where
    show l = printf "Ledger with %d transactions, %d accounts\n" --"%s"
             (length (jtxns $ ljournal l) +
              length (jmodifiertxns $ ljournal l) +
              length (jperiodictxns $ ljournal l))
             (length $ ledgerAccountNames l)
             -- (showtree $ ledgerAccountNameTree l)

nullledger :: Ledger
nullledger = Ledger {
  ljournal = nulljournal,
  laccounts = []
  }

-- | Filter a journal's transactions with the given query, then derive
-- a ledger containing the chart of accounts and balances. If the
-- query includes a depth limit, that will affect the this ledger's
-- journal but not the ledger's account tree.
ledgerFromJournal :: Query -> Journal -> Ledger
ledgerFromJournal q j = nullledger{ljournal=j'', laccounts=as}
  where
    (q',depthq)  = (filterQuery (not . queryIsDepth) q, filterQuery queryIsDepth q)
    j'  = filterJournalAmounts (filterQuery queryIsSym q) $ -- remove amount parts which the query's sym: terms would exclude
          filterJournalPostings q' j
    as  = accountsFromPostings $ journalPostings j'
    j'' = filterJournalPostings depthq j'

-- | List a ledger's account names.
ledgerAccountNames :: Ledger -> [AccountName]
ledgerAccountNames = drop 1 . map aname . laccounts

-- | Get the named account from a ledger.
ledgerAccount :: Ledger -> AccountName -> Maybe Account
ledgerAccount l a = lookupAccount a $ laccounts l

-- | Get this ledger's root account, which is a dummy "root" account
-- above all others. This should always be first in the account list,
-- if somehow not this returns a null account.
ledgerRootAccount :: Ledger -> Account
ledgerRootAccount = headDef nullacct . laccounts

-- | List a ledger's top-level accounts (the ones below the root), in tree order.
ledgerTopAccounts :: Ledger -> [Account]
ledgerTopAccounts = asubs . head . laccounts

-- | List a ledger's bottom-level (subaccount-less) accounts, in tree order.
ledgerLeafAccounts :: Ledger -> [Account]
ledgerLeafAccounts = filter (null.asubs) . laccounts

-- | Accounts in ledger whose name matches the pattern, in tree order.
ledgerAccountsMatching :: [String] -> Ledger -> [Account]
ledgerAccountsMatching pats = filter (matchpats pats . aname) . laccounts

-- | List a ledger's postings, in the order parsed.
ledgerPostings :: Ledger -> [Posting]
ledgerPostings = journalPostings . ljournal

-- | The (fully specified) date span containing all the ledger's (filtered) transactions,
-- or DateSpan Nothing Nothing if there are none.
ledgerDateSpan :: Ledger -> DateSpan
ledgerDateSpan = postingsDateSpan . ledgerPostings

-- | All commodities used in this ledger.
ledgerCommodities :: Ledger -> [CommoditySymbol]
ledgerCommodities = M.keys . jinferredcommodities . ljournal


tests_ledgerFromJournal = [
 "ledgerFromJournal" ~: do
  assertEqual "" (0) (length $ ledgerPostings $ ledgerFromJournal Any nulljournal)
  assertEqual "" (11) (length $ ledgerPostings $ ledgerFromJournal Any samplejournal)
  assertEqual "" (6) (length $ ledgerPostings $ ledgerFromJournal (Depth 2) samplejournal)
 ]

tests_Hledger_Data_Ledger = TestList $
    tests_ledgerFromJournal
