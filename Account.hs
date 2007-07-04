module Account
where
import Utils
import Types
import AccountName
import Amount
import LedgerEntry
import LedgerTransaction
import Transaction


instance Show Account where
    show (Account a ts b) = printf "Account %s with %d transactions" a $ length ts

nullacct = Account "" [] nullamt

