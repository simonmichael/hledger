module Ledger.RawLedger
where
import qualified Data.Map as Map

import Ledger.Utils
import Ledger.Types
import Ledger.AccountName
import Ledger.LedgerEntry


instance Show RawLedger where
    show l = printf "RawLedger with %d entries"
             ((length $ entries l) +
              (length $ modifier_entries l) +
              (length $ periodic_entries l))
