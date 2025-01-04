{-# LANGUAGE CPP                 #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-|


An 'Account' has a name, a list of subaccounts, an optional parent
account, and subaccounting-excluding and -including balances.

-}

module Hledger.Data.Account
( nullacct
, accountFromBalances
, accountFromPostings
, accountsFromPostings
, accountTree
, accountTreeFromBalanceAndNames
, showAccounts
, showAccountsBoringFlag
, printAccounts
, lookupAccount
, parentAccounts
, accountsLevels
, mapAccounts
, anyAccounts
, filterAccounts
, sumAccounts
, clipAccounts
, clipAccountsAndAggregate
, pruneAccounts
, flattenAccounts
, mergeAccounts
, accountSetDeclarationInfo
, sortAccountNamesByDeclaration
, sortAccountTreeByDeclaration
, sortAccountTreeByAmount
-- -- * Tests
, tests_Account
) where

import Control.Applicative ((<|>))
import qualified Data.HashSet as HS
import qualified Data.HashMap.Strict as HM
import qualified Data.IntMap as IM
import Data.List (find, sortOn)
#if !MIN_VERSION_base(4,20,0)
import Data.List (foldl')
#endif
import Data.List.NonEmpty (NonEmpty(..), groupWith)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Ord (Down(..))
import qualified Data.Text as T
import Data.These (These(..))
import Data.Time (Day(..), fromGregorian)
import Safe (headMay)
import Text.Printf (printf)

import Hledger.Data.AccountBalance
import Hledger.Data.AccountName
import Hledger.Data.Amount
import Hledger.Data.Types
import Hledger.Utils


-- deriving instance Show Account
instance Show a => Show (Account' a) where
  showsPrec d acct =
    showParen (d > 10) $
       showString "Account "
      . showString (T.unpack $ aname acct)
      . showString " (boring:"
      . showString (if aboring acct then "y" else "n")
      . showString ", abalances:"
      . shows (abalances acct)
      . showChar ')'

instance Eq (Account' a) where
  (==) a b = aname a == aname b -- quick equality test for speed
             -- and
             -- [ aname a == aname b
             -- -- , aparent a == aparent b  -- avoid infinite recursion
             -- , asubs a == asubs b
             -- , aebalance a == aebalance b
             -- , aibalance a == aibalance b
             -- ]

nullacct :: Account
nullacct = accountFromBalances "" mempty

-- | Construct an 'Account" from an account name and balances. Other fields are
-- left blank.
accountFromBalances :: AccountName -> AccountBalances a -> Account' a
accountFromBalances name bal = Account
  { aname            = name
  , adeclarationinfo = Nothing
  , asubs            = []
  , aparent          = Nothing
  , aboring          = False
  , abalances        = bal
  }

-- | Derive 1. an account tree and 2. each account's total exclusive
-- and inclusive changes from a list of postings.
-- This is the core of the balance command (and of *ledger).
-- The accounts are returned as a list in flattened tree order,
-- and also reference each other as a tree.
-- (The first account is the root of the tree.)
accountsFromPostings :: (Posting -> Day) -> [Day] -> [Posting] -> [Account]
accountsFromPostings getPostingDate days = flattenAccounts . accountFromPostings getPostingDate days

-- | Derive 1. an account tree and 2. each account's total exclusive
-- and inclusive changes from a list of postings.
-- This is the core of the balance command (and of *ledger).
-- The accounts are returned as tree.
accountFromPostings :: (Posting -> Day) -> [Day] -> [Posting] -> Account
accountFromPostings getPostingDate days ps =
    tieAccountParents . sumAccounts $ mapAccounts setBalance acctTree
  where
    -- The special name "..." is stored in the root of the tree
    acctTree     = accountTree "root" . HM.keys $ HM.delete "..." accountMap
    setBalance a = a{abalances = HM.lookupDefault emptyMap name accountMap}
      where name = if aname a == "root" then "..." else aname a
    accountMap   = processPostings ps

    processPostings :: [Posting] -> HM.HashMap AccountName (AccountBalances AccountBalance)
    processPostings = foldl' (flip processAccountName) mempty
      where
        processAccountName p = HM.alter (updateAccountBalance p) (paccount p)
        updateAccountBalance p = Just
                               . insertAccountBalances (getPostingDate p) (AccountBalance 1 (pamount p) nullmixedamt)
                               . fromMaybe emptyMap

    emptyMap = emptyAccountBalances days

-- | Convert a list of account names to a tree of Account objects,
-- with just the account names filled in.
-- A single root account with the given name is added.
accountTree :: Monoid a => AccountName -> [AccountName] -> Account' a
accountTree rootname = accountTreeFromBalanceAndNames rootname mempty

-- | Convert a list of account names to a tree of Account objects,
-- with just the account names and a constant balance filled in.
-- A single root account with the given name is added.
accountTreeFromBalanceAndNames :: AccountName -> AccountBalances a -> [AccountName] -> Account' a
accountTreeFromBalanceAndNames rootname bals as =
    (accountFromBalances rootname bals){ asubs=map (uncurry accountTree') $ M.assocs m }
  where
    T m = treeFromPaths $ map expandAccountName as :: FastTree AccountName
    accountTree' a (T m') =
      (accountFromBalances a bals){ asubs=map (uncurry accountTree') $ M.assocs m' }

-- | An efficient-to-build tree suggested by Cale Gibbard, probably
-- better than accountNameTreeFrom.
newtype FastTree a = T (M.Map a (FastTree a))
  deriving (Show, Eq, Ord)

mergeTrees :: (Ord a) => FastTree a -> FastTree a -> FastTree a
mergeTrees (T m) (T m') = T (M.unionWith mergeTrees m m')

treeFromPath :: [a] -> FastTree a
treeFromPath []     = T M.empty
treeFromPath (x:xs) = T (M.singleton x (treeFromPath xs))

treeFromPaths :: (Ord a) => [[a]] -> FastTree a
treeFromPaths = foldl' mergeTrees (T M.empty) . map treeFromPath


-- | Tie the knot so all subaccounts' parents are set correctly.
tieAccountParents :: Account' a -> Account' a
tieAccountParents = tie Nothing
  where
    tie parent a@Account{..} = a'
      where
        a' = a{aparent=parent, asubs=map (tie (Just a')) asubs}

-- | Get this account's parent accounts, from the nearest up to the root.
parentAccounts :: Account' a -> [Account' a]
parentAccounts Account{aparent=Nothing} = []
parentAccounts Account{aparent=Just a} = a:parentAccounts a

-- | List the accounts at each level of the account tree.
accountsLevels :: Account' a -> [[Account' a]]
accountsLevels = takeWhile (not . null) . iterate (concatMap asubs) . (:[])

-- | Map a (non-tree-structure-modifying) function over this and sub accounts.
mapAccounts :: (Account' a -> Account' a) -> Account' a -> Account' a
mapAccounts f a = f a{asubs = map (mapAccounts f) $ asubs a}

-- | Is the predicate true on any of this account or its subaccounts ?
anyAccounts :: (Account' a -> Bool) -> Account' a -> Bool
anyAccounts p a
    | p a = True
    | otherwise = any (anyAccounts p) $ asubs a

-- | Is the predicate true on all of this account and its subaccounts ?
allAccounts :: (Account' a -> Bool) -> Account' a -> Bool
allAccounts p a
    | not (p a) = False
    | otherwise = all (allAccounts p) $ asubs a

-- | Recalculate all the subaccount-inclusive balances in this tree.
sumAccounts :: Account -> Account
sumAccounts a = a{asubs = subs, abalances = setInclusiveBalances $ abalances a}
  where
    subs = map sumAccounts $ asubs a
    subtotals = foldMap abalances subs

    setInclusiveBalances :: AccountBalances AccountBalance -> AccountBalances AccountBalance
    setInclusiveBalances = if null subs
      then fmap setibal
      else opAccountBalances addibal subtotals

    setibal bal@(AccountBalance _ ebal _) = bal{abibalance = ebal}
    addibal (AccountBalance _ _ ibal) bal@(AccountBalance _ ebal _) = bal{abibalance = ebal <> ibal}

-- | Remove all subaccounts below a certain depth.
clipAccounts :: Int -> Account' a -> Account' a
clipAccounts 0 a = a{asubs=[]}
clipAccounts d a = a{asubs=subs}
    where
      subs = map (clipAccounts (d-1)) $ asubs a

-- | Remove subaccounts below the specified depth, aggregating their balance at the depth limit
-- (accounts at the depth limit will have any sub-balances merged into their exclusive balance).
-- If the depth is Nothing, return the original accounts
clipAccountsAndAggregate :: Monoid a => DepthSpec -> [Account' a] -> [Account' a]
clipAccountsAndAggregate (DepthSpec Nothing []) as = as
clipAccountsAndAggregate depthSpec              as = combined
    where
      clipped  = [a{aname=clipOrEllipsifyAccountName depthSpec $ aname a} | a <- as]
      combined = [a{abalances=foldMap abalances same}
                 | same@(a:|_) <- groupWith aname clipped]

{-
test cases, assuming d=1:

assets:cash 1 1
assets:checking 1 1
->
as:       [assets:cash 1 1, assets:checking 1 1]
clipped:  [assets 1 1, assets 1 1]
combined: [assets 2 2]

assets 0 2
 assets:cash 1 1
 assets:checking 1 1
->
as:       [assets 0 2, assets:cash 1 1, assets:checking 1 1]
clipped:  [assets 0 2, assets 1 1, assets 1 1]
combined: [assets 2 2]

assets 0 2
 assets:bank 1 2
  assets:bank:checking 1 1
->
as:       [assets 0 2, assets:bank 1 2, assets:bank:checking 1 1]
clipped:  [assets 0 2, assets 1 2, assets 1 1]
combined: [assets 2 2]

-}

-- | Remove all leaf accounts and subtrees matching a predicate.
pruneAccounts :: (Account' a -> Bool) -> Account' a -> Maybe (Account' a)
pruneAccounts p = headMay . prune
  where
    prune a
      | null prunedsubs = if p a then [] else [a']
      | otherwise       = [a']
      where
        prunedsubs = concatMap prune $ asubs a
        a' = a{asubs=prunedsubs}

-- | Flatten an account tree into a list, which is sometimes
-- convenient. Note since accounts link to their parents/subs, the
-- tree's structure remains intact and can still be used. It's a tree/list!
flattenAccounts :: Account' a -> [Account' a]
flattenAccounts a = squish a []
  where squish a' as = a' : Prelude.foldr squish as (asubs a')

-- | Filter an account tree (to a list).
filterAccounts :: (Account' a -> Bool) -> Account' a -> [Account' a]
filterAccounts p a
    | p a       = a : concatMap (filterAccounts p) (asubs a)
    | otherwise = concatMap (filterAccounts p) (asubs a)

-- | Merge two account trees and their subaccounts.
--
-- This assumes that the top-level 'Account's have the same name.
--
-- Unless the 'Account's and all their subaccounts have the same collection of
-- 'Day' keys, it will probably produce unhelpful output: do not do this unless
-- you really know what you're doing. Merging two accounts with unequal
-- 'Day' keys can be useful when they have the same Intervals but not
-- necessarily equal spans, as in the budget reports.
mergeAccounts :: Account' a -> Account' b -> Account' (These a b)
mergeAccounts a = tieAccountParents . merge a
  where
    merge acct1 acct2 = acct1
       { adeclarationinfo = adeclarationinfo acct1 <|> adeclarationinfo acct2
       , aparent = Nothing
       , aboring = aboring acct1 && aboring acct2
       , abalances = mergeBalances (abalances acct1) (abalances acct2)
       , asubs = mergeSubs (sortOn aname $ asubs acct1) (sortOn aname $ asubs acct2)
       }

    mergeSubs (x:xs) (y:ys) = case compare (aname x) (aname y) of
      EQ -> merge x y : mergeSubs xs ys
      LT -> fmap This x : mergeSubs xs (y:ys)
      GT -> fmap That y : mergeSubs (x:xs) ys
    mergeSubs xs [] = map (fmap This) xs
    mergeSubs [] ys = map (fmap That) ys

    mergeBalances = mergeAccountBalances These (fmap This) (fmap That)

-- | Sort each group of siblings in an account tree by inclusive amount,
-- so that the accounts with largest normal balances are listed first.
-- The provided normal balance sign determines whether normal balances
-- are negative or positive, affecting the sort order. Ie,
-- if balances are normally negative, then the most negative balances
-- sort first, and vice versa.
sortAccountTreeByAmount :: NormalSign -> Account -> Account
sortAccountTreeByAmount normalsign = mapAccounts $ \a -> a{asubs=sortSubs $ asubs a}
  where
    sortSubs = case normalsign of
        NormallyPositive -> sortOn (\a -> (Down $ amt a, aname a))
        NormallyNegative -> sortOn (\a -> (amt a, aname a))
    amt = mixedAmountStripCosts . foldMap abibalance . abalances

-- | Add extra info for this account derived from the Journal's
-- account directives, if any (comment, tags, declaration order..).
accountSetDeclarationInfo :: Journal -> Account' a -> Account' a
accountSetDeclarationInfo j a@Account{..} =
  a{ adeclarationinfo=lookup aname $ jdeclaredaccounts j }

-- | Sort account names by the order in which they were declared in
-- the journal, at each level of the account tree (ie within each
-- group of siblings). Undeclared accounts are sorted last and
-- alphabetically.
-- This is hledger's default sort for reports organised by account.
-- The account list is converted to a tree temporarily, adding any
-- missing parents; these can be kept (suitable for a tree-mode report)
-- or removed (suitable for a flat-mode report).
--
sortAccountNamesByDeclaration :: Journal -> Bool -> [AccountName] -> [AccountName]
sortAccountNamesByDeclaration j keepparents as =
    (if keepparents then id else filter (`HS.member` HS.fromList as)) $  -- maybe discard missing parents that were added
    map aname $                                         -- keep just the names
    drop 1 $                                            -- drop the root node that was added
    flattenAccounts $                                   -- convert to an account list
    sortAccountTreeByDeclaration $                      -- sort by declaration order (and name)
    mapAccounts (accountSetDeclarationInfo j) $         -- add declaration order info
    (accountTree "root" as :: Account)                  -- convert to an account tree

-- | Sort each group of siblings in an account tree by declaration order, then account name.
-- So each group will contain first the declared accounts,
-- in the same order as their account directives were parsed,
-- and then the undeclared accounts, sorted by account name.
sortAccountTreeByDeclaration :: Account' a -> Account' a
sortAccountTreeByDeclaration a
  | null $ asubs a = a
  | otherwise      = a{asubs=
      sortOn accountDeclarationOrderAndName $
      map sortAccountTreeByDeclaration $ asubs a
      }

accountDeclarationOrderAndName :: Account' a -> (Int, AccountName)
accountDeclarationOrderAndName a = (adeclarationorder', aname a)
  where
    adeclarationorder' = maybe maxBound adideclarationorder $ adeclarationinfo a

-- | Search an account list by name.
lookupAccount :: AccountName -> [Account' a] -> Maybe (Account' a)
lookupAccount a = find ((==a).aname)

-- debug helpers

printAccounts :: Show a => Account' a -> IO ()
printAccounts = putStrLn . showAccounts

showAccounts :: Show a => Account' a -> String
showAccounts = unlines . map showAccountDebug . flattenAccounts

showAccountsBoringFlag = unlines . map (show . aboring) . flattenAccounts

showAccountDebug a = printf "%-25s %s %4s"
                     (aname a)
                     (if aboring a then "b" else " " :: String)
                     (show $ abalances a)


tests_Account = testGroup "Account" [
    testGroup "accountFromPostings" [
      testCase "no postings, no days" $
        accountFromPostings undefined [] [] @?= accountTree "root" []
     ,testCase "no postings, only 2000-01-01" $
         allAccounts (all (\d -> (ModifiedJulianDay $ toInteger d) == fromGregorian 2000 01 01) . IM.keys . abdatemap . abalances)
                     (accountFromPostings undefined [fromGregorian 2000 01 01] []) @? "Not all abalances have exactly 2000-01-01"
    ]
  ]
