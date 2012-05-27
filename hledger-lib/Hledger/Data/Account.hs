{-|

An 'Account' stores

- an 'AccountName',

- all 'Posting's in the account, excluding subaccounts

- a 'MixedAmount' representing the account balance, including subaccounts.

-}

module Hledger.Data.Account
where
import Test.HUnit
import Text.Printf

import Hledger.Data.Amount
import Hledger.Data.Types


instance Show Account where
    show (Account a ps b) = printf "Account %s with %d postings and %s balance" a (length ps) (showMixedAmountDebug b)

instance Eq Account where
    (==) (Account n1 t1 b1) (Account n2 t2 b2) = n1 == n2 && t1 == t2 && b1 == b2

nullacct = Account "" [] nullmixedamt

tests_Hledger_Data_Account = TestList [
 ]

