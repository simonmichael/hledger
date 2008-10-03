module Account
where
import Ledger.Utils
import Ledger.Types
import AccountName
import Amount
import LedgerEntry
import RawTransaction
import Transaction


instance Show Account where
    show (Account a ts b) = printf "Account %s with %d transactions" a $ length ts

nullacct = Account "" [] nullamt

