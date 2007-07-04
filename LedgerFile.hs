module LedgerFile
where
import qualified Data.Map as Map

import Utils
import Types
import AccountName
import LedgerEntry


instance Show LedgerFile where
    show l = printf "LedgerFile with %d entries"
             ((length $ entries l) +
              (length $ modifier_entries l) +
              (length $ periodic_entries l))
