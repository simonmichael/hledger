{-# LANGUAGE RecordWildCards, StandaloneDeriving #-}
{-|


An 'Account' has a name, a list of subaccounts, an optional parent
account, and subaccounting-excluding and -including balances.

-}

module Hledger.Data.Account
where
import Data.List
import qualified Data.Map as M
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
    show Account{..} = printf "Account %s (boring:%s, ebalance:%s, ibalance:%s)"
                       aname  
                       (if aboring then "y" else "n" :: String)
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
  , aebalance = nullmixedamt
  , aibalance = nullmixedamt
  , aboring = False
  }

-- | Derive an account tree with balances from a set of postings.
-- (*ledger's core feature.) The accounts are returned in a list, but
-- retain their tree structure; the first one is the root of the tree.
accountsFromPostings :: [Posting] -> [Account]
accountsFromPostings ps =
  let
    acctamts = [(paccount p,pamount p) | p <- ps]
    grouped = groupBy (\a b -> fst a == fst b) $ sort $ acctamts
    summed = map (\as@((aname,_):_) -> (aname, sum $ map snd as)) grouped -- always non-empty
    setebalance a = a{aebalance=lookupJustDef nullmixedamt (aname a) summed}
    nametree = treeFromPaths $ map (expandAccountName . fst) summed
    acctswithnames = nameTreeToAccount "root" nametree
    acctswithebals = mapAccounts setebalance acctswithnames
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
-- -- , also noting
-- -- whether it has an interesting balance or interesting subs to help
-- -- with eliding later.
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

-- | Remove all leaf accounts and subtrees matching a predicate.
pruneAccounts :: (Account -> Bool) -> Account -> Maybe Account
pruneAccounts p = headMay . prune
  where
    prune a
      | null prunedsubs = if p a then [] else [a]
      | otherwise       = [a{asubs=prunedsubs}]
      where
        prunedsubs = concatMap prune $ asubs a

-- | Flatten an account tree into a list, which is sometimes
-- convenient. Note since accounts link to their parents/subs, the
-- account tree remains intact and can still be used. It's a tree/list!
flattenAccounts :: Account -> [Account]
flattenAccounts a = squish a []
  where squish a as = a:Prelude.foldr squish as (asubs a)

-- | Filter an account tree (to a list).
filterAccounts :: (Account -> Bool) -> Account -> [Account]
filterAccounts p a
    | p a       = a : concatMap (filterAccounts p) (asubs a)
    | otherwise = concatMap (filterAccounts p) (asubs a)

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

