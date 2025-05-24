{-# LANGUAGE CPP #-}
{-|


An 'AccountBalances' consists of a historical balance, along with balances for
given date ranges.

-}
module Hledger.Data.AccountBalances
( accountBalancesFromList

, lookupAccountBalance
, insertAccountBalances
, opAccountBalances
, mergeAccountBalances
, padAccountBalances

, tests_AccountBalances
) where

import Data.Foldable1 (Foldable1(..))
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
#if !MIN_VERSION_base(4,20,0)
import Data.List (foldl')
#endif
import Data.Time (Day(..), fromGregorian)

import Test.Tasty (testGroup)
import Test.Tasty.HUnit ((@?=), testCase)

import Hledger.Data.Amount
import Hledger.Data.Types


instance Show a => Show (AccountBalances a) where
  showsPrec d (AccountBalances h ds) =
    showParen (d > 10) $
        showString "AccountBalances"
      . showString "{ abhistorical = " . shows h
      . showString ", abdatemap = "
      . showString "fromList " . shows (map (\(day, x) -> (ModifiedJulianDay $ toInteger day, x)) $ IM.toList ds)
      . showChar '}'

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

-- | Construct an 'AccountBalances' from a list.
accountBalancesFromList :: a -> [(Day, a)] -> AccountBalances a
accountBalancesFromList h = AccountBalances h . IM.fromList . map (\(d, a) -> (fromInteger $ toModifiedJulianDay d, a))

-- | Get account balance information to the period containing a given 'Day'.
lookupAccountBalance :: Day -> AccountBalances a -> a
lookupAccountBalance d (AccountBalances h as) =
    maybe h snd $ IM.lookupLE (fromInteger $ toModifiedJulianDay d) as

-- | Add account balance information to the appropriate location in 'AccountBalances'.
insertAccountBalances :: Semigroup a => Maybe Day -> a -> AccountBalances a -> AccountBalances a
insertAccountBalances mday b balances = case mday of
    Nothing  -> balances{abhistorical = abhistorical balances <> b}
    Just day -> balances{abdatemap = IM.insertWith (<>) (fromInteger $ toModifiedJulianDay day) b $ abdatemap balances}

-- | Merges two 'AccountBalances', using the given operation to combine their balance information.
--
-- This will drop keys if they are not present in both AccountBalances.
opAccountBalances :: (a -> b -> c) -> AccountBalances a -> AccountBalances b -> AccountBalances c
opAccountBalances f (AccountBalances h1 as1) (AccountBalances h2 as2) =
    AccountBalances (f h1 h2) $ IM.intersectionWith f as1 as2

-- | Merges two 'AccountBalances', using the given operations for balance
-- information only in the first, only in the second, or in both
-- 'AccountBalances', respectively.
mergeAccountBalances :: (a -> c) -> (b -> c) -> (a -> b -> c)
                     -> AccountBalances a -> AccountBalances b -> AccountBalances c
mergeAccountBalances only1 only2 f = \(AccountBalances h1 as1) (AccountBalances h2 as2) ->
    AccountBalances (f h1 h2) $ merge as1 as2
  where
    merge = IM.mergeWithKey (\_ x y -> Just $ f x y) (fmap only1) (fmap only2)

-- | Pad out the datemap of an 'AccountBalances' so that every key from a set is present.
padAccountBalances :: Monoid a => IS.IntSet -> AccountBalances a -> AccountBalances a
padAccountBalances keys bal = bal{abdatemap = abdatemap bal <> IM.fromSet (const mempty) keys}


-- tests

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
