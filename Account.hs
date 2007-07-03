module Account
where
import qualified Data.Map as Map

import Utils
import Types
import AccountName
import Amount
import Entry
import Transaction
import EntryTransaction
import RawLedger


instance Show Account where
    show (Account a ts b) = printf "Account %s with %d transactions" a $ length ts

nullacct = Account "" [] nullamt

