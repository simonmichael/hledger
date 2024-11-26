{-# LANGUAGE CPP #-}
{-|


An 'AccountBalance' consists of a historical balance, along with balances
for given date ranges.

-}
module Hledger.Data.AccountBalance
( emptyAccountBalances
, accountBalancesFromList

, lookupAccountBalance
, insertAccountBalances
, opAccountBalances
, mergeAccountBalances

, applyAccountBalance
, opAccountBalance
, op2AccountBalance

, tests_AccountBalance
, tests_AccountBalances
) where

import Data.Foldable1 (Foldable1(..))
import qualified Data.IntMap.Strict as IM
#if !MIN_VERSION_base(4,20,0)
import Data.List (foldl')
#endif
import Data.Time (Day(..), fromGregorian)

import Test.Tasty (testGroup)
import Test.Tasty.HUnit ((@?=), testCase)

import Hledger.Data.Amount
import Hledger.Data.Types


instance Foldable AccountBalances where
  foldr f z (AccountBalances h as) = foldr f (f h z) as
  foldl f z (AccountBalances h as) = foldl f (f z h) as
  foldl' f z (AccountBalances h as) = let fzh = f z h in fzh `seq` foldl' f fzh as

instance Foldable1 AccountBalances where
  foldrMap1 f g (AccountBalances h as) = foldr g (f h) as
  foldlMap1 f g (AccountBalances h as) = foldl g (f h) as
  foldlMap1' f g (AccountBalances h as) = let fh = f h in fh `seq` foldl' g fh as

instance Traversable AccountBalances where
  traverse f (AccountBalances h as) = liftA2 AccountBalances (f h) $ traverse f as

-- | The Semigroup instance for 'AccountBalance' will simply take the union of
-- keys in the date map section. This may not be the result you want if the
-- keys are not identical.
instance Semigroup a => Semigroup (AccountBalances a) where
  AccountBalances h1 as1 <> AccountBalances h2 as2 = AccountBalances (h1 <> h2) $ IM.unionWith (<>) as1 as2

instance Monoid a => Monoid (AccountBalances a) where
  mempty = AccountBalances mempty mempty

-- | Construct an empty 'AccountBalance' from a list of boundary days.
emptyAccountBalances :: Monoid a => [Day] -> AccountBalances a
emptyAccountBalances = accountBalancesFromList mempty . map (\d -> (d, mempty))

-- | Construct an 'AccountBalances' from a list.
accountBalancesFromList :: a -> [(Day, a)] -> AccountBalances a
accountBalancesFromList h = AccountBalances h . IM.fromList . map (\(d, a) -> (fromInteger $ toModifiedJulianDay d, a))

-- | Get the 'AccountBalance' associated to the period containing a given 'Day'.
lookupAccountBalance :: Day -> AccountBalances a -> a
lookupAccountBalance d (AccountBalances h as) =
    maybe h snd $ IM.lookupLE (fromInteger $ toModifiedJulianDay d) as

-- | Add the 'AccountBalance' to the appropriate location in 'AccountBalances'.
insertAccountBalances :: Semigroup a => Day -> a -> AccountBalances a -> AccountBalances a
insertAccountBalances day b balances = case IM.lookupLE (fromInteger $ toModifiedJulianDay day) (abdatemap balances) of
    Nothing     -> balances{abhistorical = b <> abhistorical balances}
    Just (d, a) -> balances{abdatemap = IM.insert d (b <> a) $ abdatemap balances}

-- | Performs an operation on the contents of two 'AccountBalances'.
--
-- This will drop keys if they are not present in both AccountBalances.
opAccountBalances :: (a -> b -> c) -> AccountBalances a -> AccountBalances b -> AccountBalances c
opAccountBalances f (AccountBalances h1 as1) (AccountBalances h2 as2) =
    AccountBalances (f h1 h2) $ IM.intersectionWith f as1 as2

-- | Performs an operation on the contents of two 'AccountBalances'.
mergeAccountBalances :: (a -> b -> c) -> (IM.IntMap a -> IM.IntMap c) -> (IM.IntMap b -> IM.IntMap c)
                     -> AccountBalances a -> AccountBalances b -> AccountBalances c
mergeAccountBalances f only1 only2 = \(AccountBalances h1 as1) (AccountBalances h2 as2) ->
    AccountBalances (f h1 h2) $ merge as1 as2
  where
    merge = IM.mergeWithKey (\_ x1 x2 -> Just $ f x1 x2) only1 only2


instance Semigroup AccountBalance where
  AccountBalance n e i <> AccountBalance n' e' i' = AccountBalance (n + n') (maPlus e e') (maPlus i i')

instance Monoid AccountBalance where
  mempty = AccountBalance 0 nullmixedamt nullmixedamt

-- | Apply the same operation to both 'MixedAmount' in an 'AccountBalance'.
applyAccountBalance :: (MixedAmount -> MixedAmount) -> AccountBalance -> AccountBalance
applyAccountBalance f = apply2AccountBalance f f

-- | Apply functions to both 'MixedAmount' in an 'AccountBalance'.
apply2AccountBalance :: (MixedAmount -> MixedAmount) -> (MixedAmount -> MixedAmount) -> AccountBalance -> AccountBalance
apply2AccountBalance f g a = a{abebalance = f $ abebalance a, abibalance = g $ abibalance a}

-- | Perform the same operation on the 'MixedAmount' in two 'AccountBalance'.
opAccountBalance :: (MixedAmount -> MixedAmount -> MixedAmount) -> AccountBalance -> AccountBalance -> AccountBalance
opAccountBalance f = op2AccountBalance f f

-- | Perform operations on the exclusive and inclusive amounts in two 'AccountBalance's.
op2AccountBalance :: (MixedAmount -> MixedAmount -> MixedAmount) -> (MixedAmount -> MixedAmount -> MixedAmount) -> AccountBalance -> AccountBalance -> AccountBalance
op2AccountBalance f g a b = a{abebalance = f (abebalance a) (abebalance b), abibalance = g (abibalance a) (abibalance b)}


-- tests

tests_AccountBalance = testGroup "AccountBalance" [

  testCase "opAccountBalance maPlus" $ do
    opAccountBalance maPlus (AccountBalance 5 (mixed [usd 1]) (mixed [usd 2])) (AccountBalance 0 (mixed [usd 3]) (mixed [usd 4]))
      @?= AccountBalance 5 (mixed [usd 4]) (mixed [usd 6]),

  testCase "opAccountBalance maMinus" $ do
    opAccountBalance maMinus (AccountBalance 5 (mixed [usd 1]) (mixed [usd 2])) (AccountBalance 0 (mixed [usd 3]) (mixed [usd 4]))
      @?= AccountBalance 5 (mixed [usd (-2)]) (mixed [usd (-2)])
  ]

tests_AccountBalances =
  let
    dayMap  = accountBalancesFromList (mixed [usd 1]) [(fromGregorian 2000 01 01, mixed [usd 2]), (fromGregorian 2004 02 28, mixed [usd 3])]
    dayMap2 = accountBalancesFromList (mixed [usd 2]) [(fromGregorian 2000 01 01, mixed [usd 4]), (fromGregorian 2004 02 28, mixed [usd 6])]
  in testGroup "AccountBalances" [

  testCase "accountBalancesFromList" $ do
    length dayMap @?= 3,

  testCase "Semigroup instance" $ do
    dayMap <> dayMap @?= dayMap2,

  testCase "Monoid instance" $ do
    dayMap <> mempty @?= dayMap
  ]
