module RawLedger
where
import qualified Data.Map as Map

import Ledger.Utils
import Ledger.Types
import AccountName
import LedgerEntry


instance Show RawLedger where
    show l = printf "RawLedger with %d entries"
             ((length $ entries l) +
              (length $ modifier_entries l) +
              (length $ periodic_entries l))
