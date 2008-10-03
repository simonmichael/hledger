module Ledger.Account
where
import Ledger.Utils
import Ledger.Types
import Ledger.AccountName
import Ledger.Amount
import Ledger.LedgerEntry
import Ledger.RawTransaction
import Ledger.Transaction


instance Show Account where
    show (Account a ts b) = printf "Account %s with %d transactions" a $ length ts

nullacct = Account "" [] nullamt

