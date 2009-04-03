{-|

A compound data type for efficiency. An 'Account' stores

- an 'AccountName',
- all `Transaction`s (postings plus ledger transaction info) in the
  account, excluding subaccounts
- and the account balance, including subaccounts.

-}

module Ledger.Account
where
import Ledger.Utils
import Ledger.Types
import Ledger.Amount


instance Show Account where
    show (Account a ts b) = printf "Account %s with %d txns and %s balance" a (length ts) (showMixedAmount b)

instance Eq Account where
    (==) (Account n1 t1 b1) (Account n2 t2 b2) = n1 == n2 && t1 == t2 && b1 == b2

nullacct = Account "" [] nullmixedamt

