{-# LANGUAGE CPP #-}
{-|


An 'AccountBalance' is a data type tracking a number of postings, exclusive, and inclusive balance
for given date ranges.

-}
module Hledger.Data.AccountBalance
( mapAccountBalance
, opAccountBalance

, tests_AccountBalance
) where


import Test.Tasty (testGroup)
import Test.Tasty.HUnit ((@?=), testCase)

import Hledger.Data.Amount
import Hledger.Data.Types


instance Show AccountBalance where
  showsPrec d (AccountBalance n e i) =
    showParen (d > 10) $
        showString "AccountBalance"
      . showString "{ abnumpostings = " . shows n
      . showString ", abebalance = " . showString (wbUnpack (showMixedAmountB defaultFmt e))
      . showString ", abibalance = " . showString (wbUnpack (showMixedAmountB defaultFmt i))
      . showChar '}'

instance Semigroup AccountBalance where
  AccountBalance n e i <> AccountBalance n' e' i' = AccountBalance (n + n') (maPlus e e') (maPlus i i')

instance Monoid AccountBalance where
  mempty = AccountBalance 0 nullmixedamt nullmixedamt

-- | Apply an operation to both 'MixedAmount' in an 'AccountBalance'.
mapAccountBalance :: (MixedAmount -> MixedAmount) -> AccountBalance -> AccountBalance
mapAccountBalance f a = a{abebalance = f $ abebalance a, abibalance = f $ abibalance a}

-- | Perform an operation on the 'MixedAmount' in two 'AccountBalance'.
opAccountBalance :: (MixedAmount -> MixedAmount -> MixedAmount) -> AccountBalance -> AccountBalance -> AccountBalance
opAccountBalance f a b = a{abebalance = f (abebalance a) (abebalance b), abibalance = f (abibalance a) (abibalance b)}


-- tests

tests_AccountBalance = testGroup "AccountBalance" [

  testCase "opAccountBalance maPlus" $ do
    opAccountBalance maPlus (AccountBalance 5 (mixed [usd 1]) (mixed [usd 2])) (AccountBalance 0 (mixed [usd 3]) (mixed [usd 4]))
      @?= AccountBalance 5 (mixed [usd 4]) (mixed [usd 6]),

  testCase "opAccountBalance maMinus" $ do
    opAccountBalance maMinus (AccountBalance 5 (mixed [usd 1]) (mixed [usd 2])) (AccountBalance 0 (mixed [usd 3]) (mixed [usd 4]))
      @?= AccountBalance 5 (mixed [usd (-2)]) (mixed [usd (-2)])
  ]
