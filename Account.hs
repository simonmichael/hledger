
module Account
where
import Utils
import BasicTypes

-- AccountNames are strings like "assets:cash:petty"; from these we build
-- the chart of accounts, which should be a simple hierarchy. 
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

indentAccountName :: AccountName -> String
indentAccountName a = replicate (((accountNameLevel a) - 1) * 2) ' ' ++ (accountLeafName a)


-- We could almost get by with just the above, but we need smarter
-- structures to eg display the account tree with boring accounts elided.
-- first, here is a tree of AccountNames; Account and Account tree are
-- defined later.

antacctname = fst . node
antsubs = snd . node

accountNameTreeFrom_props =
    [
     accountNameTreeFrom ["a"] == Tree ("top", [Tree ("a",[])]),
     accountNameTreeFrom ["a","b"] == Tree ("top", [Tree ("a", []), Tree ("b", [])]),
     accountNameTreeFrom ["a","a:b"] == Tree ("top", [Tree ("a", [Tree ("a:b", [])])]),
     accountNameTreeFrom ["a:b"] == Tree ("top", [Tree ("a", [Tree ("a:b", [])])])
    ]
accountNameTreeFrom :: [AccountName] -> Tree AccountName
accountNameTreeFrom accts = 
    Tree ("top", accountsFrom (topAccountNames accts))
        where
          accountsFrom :: [AccountName] -> [Tree AccountName]
          accountsFrom [] = []
          accountsFrom as = [Tree (a, accountsFrom $ subs a) | a <- as]
          subs = (subAccountNamesFrom accts)

showAccountNameTree :: Tree AccountName -> String
showAccountNameTree at = showAccountNameTrees $ antsubs at

showAccountNameTrees :: [Tree AccountName] -> String
showAccountNameTrees ats =
    concatMap showAccountNameBranch ats
        where
          showAccountNameBranch at = topacct ++ "\n" ++ subs
              where
                topacct = indentAccountName $ antacctname at
                subs = showAccountNameTrees $ antsubs at

