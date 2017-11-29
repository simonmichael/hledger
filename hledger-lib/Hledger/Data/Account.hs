{-# LANGUAGE RecordWildCards, StandaloneDeriving, OverloadedStrings #-}
{-|


An 'Account' has a name, a list of subaccounts, an optional parent
account, and subaccounting-excluding and -including balances.

-}

module Hledger.Data.Account
where
import Data.List
import Data.List.Extra (groupSort, groupOn)
import Data.Maybe
import Data.Ord
import qualified Data.Map as M
import Data.Text (pack,unpack)
import Safe (headMay, lookupJustDef)
import Test.HUnit
import Text.Printf

import Hledger.Data.AccountName
import Hledger.Data.Amount
import Hledger.Data.Posting()
import Hledger.Data.Types
import Hledger.Utils


-- deriving instance Show Account
instance Show Account where
    show Account{..} = printf "Account %s (boring:%s, postings:%d, ebalance:%s, ibalance:%s)"
                       (pack $ regexReplace ":" "_" $ unpack aname)  -- hide : so pretty-show doesn't break line
                       (if aboring then "y" else "n" :: String)
                       anumpostings
                       (showMixedAmount aebalance)
                       (showMixedAmount aibalance)

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
  { aname = ""
  , aparent = Nothing
  , asubs = []
  , anumpostings = 0
  , aebalance = nullmixedamt
  , aibalance = nullmixedamt
  , aboring = False
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
    grouped = groupSort [(paccount p,pamount p) | p <- ps]
    counted = [(aname, length amts) | (aname, amts) <- grouped]
    summed =  [(aname, sumStrict amts) | (aname, amts) <- grouped]  -- always non-empty
    nametree = treeFromPaths $ map (expandAccountName . fst) summed
    acctswithnames = nameTreeToAccount "root" nametree
    acctswithnumps = mapAccounts setnumps    acctswithnames where setnumps    a = a{anumpostings=fromMaybe 0 $ lookup (aname a) counted}
    acctswithebals = mapAccounts setebalance acctswithnumps where setebalance a = a{aebalance=lookupJustDef nullmixedamt (aname a) summed}
    acctswithibals = sumAccounts acctswithebals
    acctswithparents = tieAccountParents acctswithibals
    acctsflattened = flattenAccounts acctswithparents
  in
    acctsflattened

-- | Convert an AccountName tree to an Account tree
nameTreeToAccount :: AccountName -> FastTree AccountName -> Account
nameTreeToAccount rootname (T m) =
    nullacct{ aname=rootname, asubs=map (uncurry nameTreeToAccount) $ M.assocs m }

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
    ibal = sum $ aebalance a : map aibalance subs

-- | Remove all subaccounts below a certain depth.
clipAccounts :: Int -> Account -> Account
clipAccounts 0 a = a{asubs=[]}
clipAccounts d a = a{asubs=subs}
    where
      subs = map (clipAccounts (d-1)) $ asubs a

-- | Remove subaccounts below the specified depth, aggregating their balance at the depth limit
-- (accounts at the depth limit will have any sub-balances merged into their exclusive balance).
clipAccountsAndAggregate :: Int -> [Account] -> [Account]
clipAccountsAndAggregate d as = combined
    where
      clipped  = [a{aname=clipOrEllipsifyAccountName d $ aname a} | a <- as]
      combined = [a{aebalance=sum (map aebalance same)}
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

-- | Sort each level of an account tree by inclusive amount,
-- so that the accounts with largest normal balances are listed first.  
-- The provided normal balance sign determines whether normal balances
-- are negative or positive.
sortAccountTreeByAmount :: NormalBalance -> Account -> Account
sortAccountTreeByAmount normalsign a
  | null $ asubs a = a
  | otherwise      = a{asubs=
                        sortBy (maybeflip $ comparing aibalance) $ 
                        map (sortAccountTreeByAmount normalsign) $ asubs a}
  where
    maybeflip | normalsign==NormalNegative = id
              | otherwise                  = flip

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
                     (showMixedAmount $ aebalance a)
                     (showMixedAmount $ aibalance a)
                     (if aboring a then "b" else " " :: String)


tests_Hledger_Data_Account = TestList [
 ]

