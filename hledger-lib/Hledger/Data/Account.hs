{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-|


An 'Account' has a name, a list of subaccounts, an optional parent
account, and subaccounting-excluding and -including balances.

-}

module Hledger.Data.Account
where
import qualified Data.HashSet as HS
import qualified Data.HashMap.Strict as HM
import Data.List (find, sortOn)
import Data.List.Extra (groupOn)
import qualified Data.Map as M
import Data.Ord (Down(..))
import Safe (headMay)
import Text.Printf

import Hledger.Data.AccountName
import Hledger.Data.Amount
import Hledger.Data.Posting ()
import Hledger.Data.Types
import Hledger.Utils


-- deriving instance Show Account
instance Show Account where
    show Account{..} = printf "Account %s (boring:%s, postings:%d, ebalance:%s, ibalance:%s)"
                       aname
                       (if aboring then "y" else "n" :: String)
                       anumpostings
                       (wbUnpack $ showMixedAmountB noColour aebalance)
                       (wbUnpack $ showMixedAmountB noColour aibalance)

instance Eq Account where
  (==) a b = aname a == aname b -- quick equality test for speed
             -- and
             -- [ aname a == aname b
             -- -- , aparent a == aparent b  -- avoid infinite recursion
             -- , asubs a == asubs b
             -- , aebalance a == aebalance b
             -- , aibalance a == aibalance b
             -- ]

nullacct = Account
  { aname            = ""
  , adeclarationinfo = Nothing
  , asubs            = []
  , aparent          = Nothing
  , aboring          = False
  , anumpostings     = 0
  , aebalance        = nullmixedamt
  , aibalance        = nullmixedamt
  }

-- | Derive 1. an account tree and 2. each account's total exclusive
-- and inclusive changes from a list of postings.
-- This is the core of the balance command (and of *ledger).
-- The accounts are returned as a list in flattened tree order,
-- and also reference each other as a tree.
-- (The first account is the root of the tree.)
accountsFromPostings :: [Posting] -> [Account]
accountsFromPostings ps =
  let
    summed = foldr (\p -> HM.insertWith addAndIncrement (paccount p) (1, pamount p)) mempty ps
      where addAndIncrement (n, a) (m, b) = (n + m, a `maPlus` b)
    acctstree      = accountTree "root" $ HM.keys summed
    acctswithebals = mapAccounts setnumpsebalance acctstree
      where setnumpsebalance a = a{anumpostings=numps, aebalance=total}
              where (numps, total) = HM.lookupDefault (0, nullmixedamt) (aname a) summed
    acctswithibals = sumAccounts acctswithebals
    acctswithparents = tieAccountParents acctswithibals
    acctsflattened = flattenAccounts acctswithparents
  in
    acctsflattened

-- | Convert a list of account names to a tree of Account objects,
-- with just the account names filled in.
-- A single root account with the given name is added.
accountTree :: AccountName -> [AccountName] -> Account
accountTree rootname as = nullacct{aname=rootname, asubs=map (uncurry accountTree') $ M.assocs m }
  where
    T m = treeFromPaths $ map expandAccountName as :: FastTree AccountName
    accountTree' a (T m) =
      nullacct{
        aname=a
       ,asubs=map (uncurry accountTree') $ M.assocs m
       }

-- | Tie the knot so all subaccounts' parents are set correctly.
tieAccountParents :: Account -> Account
tieAccountParents = tie Nothing
  where
    tie parent a@Account{..} = a'
      where
        a' = a{aparent=parent, asubs=map (tie (Just a')) asubs}

-- | Get this account's parent accounts, from the nearest up to the root.
parentAccounts :: Account -> [Account]
parentAccounts Account{aparent=Nothing} = []
parentAccounts Account{aparent=Just a} = a:parentAccounts a

-- | List the accounts at each level of the account tree.
accountsLevels :: Account -> [[Account]]
accountsLevels = takeWhile (not . null) . iterate (concatMap asubs) . (:[])

-- | Map a (non-tree-structure-modifying) function over this and sub accounts.
mapAccounts :: (Account -> Account) -> Account -> Account
mapAccounts f a = f a{asubs = map (mapAccounts f) $ asubs a}

-- | Is the predicate true on any of this account or its subaccounts ?
anyAccounts :: (Account -> Bool) -> Account -> Bool
anyAccounts p a
    | p a = True
    | otherwise = any (anyAccounts p) $ asubs a

-- | Add subaccount-inclusive balances to an account tree.
sumAccounts :: Account -> Account
sumAccounts a
  | null $ asubs a = a{aibalance=aebalance a}
  | otherwise      = a{aibalance=ibal, asubs=subs}
  where
    subs = map sumAccounts $ asubs a
    ibal = maSum $ aebalance a : map aibalance subs

-- | Remove all subaccounts below a certain depth.
clipAccounts :: Int -> Account -> Account
clipAccounts 0 a = a{asubs=[]}
clipAccounts d a = a{asubs=subs}
    where
      subs = map (clipAccounts (d-1)) $ asubs a

-- | Remove subaccounts below the specified depth, aggregating their balance at the depth limit
-- (accounts at the depth limit will have any sub-balances merged into their exclusive balance).
-- If the depth is Nothing, return the original accounts
clipAccountsAndAggregate :: Maybe Int -> [Account] -> [Account]
clipAccountsAndAggregate Nothing  as = as
clipAccountsAndAggregate (Just d) as = combined
    where
      clipped  = [a{aname=clipOrEllipsifyAccountName (Just d) $ aname a} | a <- as]
      combined = [a{aebalance=maSum $ map aebalance same}
                 | same@(a:_) <- groupOn aname clipped]
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
pruneAccounts :: (Account -> Bool) -> Account -> Maybe Account
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
flattenAccounts :: Account -> [Account]
flattenAccounts a = squish a []
  where squish a as = a : Prelude.foldr squish as (asubs a)

-- | Filter an account tree (to a list).
filterAccounts :: (Account -> Bool) -> Account -> [Account]
filterAccounts p a
    | p a       = a : concatMap (filterAccounts p) (asubs a)
    | otherwise = concatMap (filterAccounts p) (asubs a)

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
    amt = mixedAmountStripPrices . aibalance

-- | Add extra info for this account derived from the Journal's
-- account directives, if any (comment, tags, declaration order..).
accountSetDeclarationInfo :: Journal -> Account -> Account
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
    accountTree "root"                                  -- convert to an account tree
    as

-- | Sort each group of siblings in an account tree by declaration order, then account name.
-- So each group will contain first the declared accounts,
-- in the same order as their account directives were parsed,
-- and then the undeclared accounts, sorted by account name.
sortAccountTreeByDeclaration :: Account -> Account
sortAccountTreeByDeclaration a
  | null $ asubs a = a
  | otherwise      = a{asubs=
      sortOn accountDeclarationOrderAndName $
      map sortAccountTreeByDeclaration $ asubs a
      }

accountDeclarationOrderAndName :: Account -> (Int, AccountName)
accountDeclarationOrderAndName a = (adeclarationorder', aname a)
  where
    adeclarationorder' = maybe maxBound adideclarationorder $ adeclarationinfo a

-- | Search an account list by name.
lookupAccount :: AccountName -> [Account] -> Maybe Account
lookupAccount a = find ((==a).aname)

-- debug helpers

printAccounts :: Account -> IO ()
printAccounts = putStrLn . showAccounts

showAccounts = unlines . map showAccountDebug . flattenAccounts

showAccountsBoringFlag = unlines . map (show . aboring) . flattenAccounts

showAccountDebug a = printf "%-25s %4s %4s %s"
                     (aname a)
                     (wbUnpack . showMixedAmountB noColour $ aebalance a)
                     (wbUnpack . showMixedAmountB noColour $ aibalance a)
                     (if aboring a then "b" else " " :: String)
