
module Account
where

import Debug.Trace
import Text.Printf
import Text.Regex
import Data.List

import Utils

-- AccountNames are strings like "assets:cash:petty"; from these we build
-- the chart of accounts, which should be a simple hierarchy. We could
-- almost get by with just these, but see below.
type AccountName = String

accountNameComponents :: AccountName -> [String]
accountNameComponents = splitAtElement ':'

accountNameFromComponents :: [String] -> AccountName
accountNameFromComponents = concat . intersperse ":"

accountLeafName :: AccountName -> String
accountLeafName = rhead . accountNameComponents

accountNameLevel :: AccountName -> Int
accountNameLevel = length . accountNameComponents

-- ["a:b:c","d:e"] -> ["a","a:b","a:b:c","d","d:e"]
expandAccountNames :: [AccountName] -> [AccountName]
expandAccountNames as = nub $ concat $ map expand as
    where expand as = map accountNameFromComponents (tail $ inits $ accountNameComponents as)

-- ["a:b:c","d:e"] -> ["a","d"]
topAccountNames :: [AccountName] -> [AccountName]
topAccountNames as = [a | a <- expandAccountNames as, accountNameLevel a == 1]

parentAccountName :: AccountName -> Maybe AccountName
parentAccountName a = 
    case accountNameLevel a > 1 of
      True -> Just $ accountNameFromComponents $ rtail $ accountNameComponents a
      False -> Nothing

s `isSubAccountNameOf` p = 
    ((p ++ ":") `isPrefixOf` s) && (accountNameLevel s == (accountNameLevel p + 1))

subAccountNamesFrom :: [AccountName] -> AccountName -> [AccountName]
subAccountNamesFrom accts a = filter (`isSubAccountNameOf` a) accts

matchAccountName :: String -> AccountName -> Bool
matchAccountName s a =
    case matchRegex (mkRegex s) a of
      Nothing -> False
      otherwise -> True

-- We need structures smart enough to eg display the account tree with
-- boring accounts elided.

-- simple polymorphic tree. each node is a tuple of the node type and a
-- list of subtrees
newtype Tree a = Tree { unTree :: (a, [Tree a]) } deriving (Show,Eq)

-- an Account has a name and a list of sub-accounts - ie a tree of
-- AccountNames.
type Account = Tree AccountName
atacct = fst . unTree
atsubs = snd . unTree
nullacct = Tree ("", [])

accountFrom :: [AccountName] -> Account
accountFrom_props =
    [
     accountFrom [] == nullacct,
     accountFrom ["a"] == Tree ("", [Tree ("a",[])]),
     accountFrom ["a","b"] == Tree ("", [Tree ("a", []), Tree ("b", [])]),
     accountFrom ["a","a:b"] == Tree ("", [Tree ("a", [Tree ("a:b", [])])]),
     accountFrom ["a:b"] == Tree ("", [Tree ("a", [Tree ("a:b", [])])])
    ]
accountFrom accts = 
    Tree ("top", accountsFrom (topAccountNames accts))
        where
          accountsFrom :: [AccountName] -> [Account]
          accountsFrom [] = []
          accountsFrom as = [Tree (a, accountsFrom $ subs a) | a <- as]
          subs = (subAccountNamesFrom accts)

showAccount :: Account -> String
showAccount at = showAccounts $ atsubs at

showAccounts :: [Account] -> String
showAccounts ats =
    concatMap showAccountBranch ats
        where
          showAccountBranch at = topacct ++ "\n" ++ subs
              where
                topacct = indentAccountName $ atacct at
                subs = showAccounts $ atsubs at

indentAccountName :: AccountName -> String
indentAccountName a = replicate (((accountNameLevel a) - 1) * 2) ' ' ++ (accountLeafName a)


