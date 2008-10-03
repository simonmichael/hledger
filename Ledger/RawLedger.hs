{-|

A 'RawLedger' is a parsed ledger file. We call it raw to distinguish from
the cached 'Ledger'.

-}

module Ledger.RawLedger
where
import qualified Data.Map as Map

import Ledger.Utils
import Ledger.Types
import Ledger.AccountName
import Ledger.Entry
import Ledger.Transaction


instance Show RawLedger where
    show l = printf "RawLedger with %d entries"
             ((length $ entries l) +
              (length $ modifier_entries l) +
              (length $ periodic_entries l))

rawLedgerTransactions :: RawLedger -> [Transaction]
rawLedgerTransactions = txns . entries
    where
      txns :: [Entry] -> [Transaction]
      txns es = concat $ map flattenEntry $ zip es (iterate (+1) 1)

rawLedgerAccountNamesUsed :: RawLedger -> [AccountName]
rawLedgerAccountNamesUsed = accountNamesFromTransactions . rawLedgerTransactions

rawLedgerAccountNames :: RawLedger -> [AccountName]
rawLedgerAccountNames = sort . expandAccountNames . rawLedgerAccountNamesUsed

rawLedgerAccountNameTree :: RawLedger -> Tree AccountName
rawLedgerAccountNameTree l = accountNameTreeFrom $ rawLedgerAccountNames l
