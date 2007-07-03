module RawLedger
where
import qualified Data.Map as Map

import Utils
import Types
import AccountName
import Entry
import EntryTransaction


instance Show RawLedger where
    show l = printf "RawLedger with %d entries"
             ((length $ entries l) +
              (length $ modifier_entries l) +
              (length $ periodic_entries l))

rawLedgerTransactions :: RawLedger -> [EntryTransaction]
rawLedgerTransactions l = entryTransactionsFrom $ entries l

rawLedgerAccountNamesUsed :: RawLedger -> [AccountName]
rawLedgerAccountNamesUsed l = accountNamesFromTransactions $ entryTransactionsFrom $ entries l

rawLedgerAccountNames :: RawLedger -> [AccountName]
rawLedgerAccountNames = sort . expandAccountNames . rawLedgerAccountNamesUsed

rawLedgerAccountNameTree :: RawLedger -> Tree AccountName
rawLedgerAccountNameTree l = accountNameTreeFrom $ rawLedgerAccountNames l



