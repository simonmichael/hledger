module AccountName
where
import Utils
import Types

-- AccountNames are strings like "assets:cash:petty"; from these we build
-- the chart of accounts, which should be a simple hierarchy. 
type AccountName = String

accountNameComponents :: AccountName -> [String]
accountNameComponents = splitAtElement ':'

accountNameFromComponents :: [String] -> AccountName
accountNameFromComponents = concat . intersperse ":"

accountLeafName :: AccountName -> String
accountLeafName = last . accountNameComponents

accountNameLevel :: AccountName -> Int
accountNameLevel = length . accountNameComponents

-- ["a:b:c","d:e"] -> ["a","a:b","a:b:c","d","d:e"]
expandAccountNames :: [AccountName] -> [AccountName]
expandAccountNames as = nub $ concat $ map expand as
    where expand as = map accountNameFromComponents (tail $ inits $ accountNameComponents as)

-- ["a:b:c","d:e"] -> ["a","d"]
topAccountNames :: [AccountName] -> [AccountName]
topAccountNames as = [a | a <- expandAccountNames as, accountNameLevel a == 1]

parentAccountName :: AccountName -> AccountName
parentAccountName a = accountNameFromComponents $ init $ accountNameComponents a

parentAccountNames :: AccountName -> [AccountName]
parentAccountNames a = parentAccountNames' $ parentAccountName a
    where
      parentAccountNames' "" = []
      parentAccountNames' a = [a] ++ (parentAccountNames' $ parentAccountName a)

s `isSubAccountNameOf` p = 
    ((p ++ ":") `isPrefixOf` s) && (accountNameLevel s == (accountNameLevel p + 1))

subAccountNamesFrom :: [AccountName] -> AccountName -> [AccountName]
subAccountNamesFrom accts a = filter (`isSubAccountNameOf` a) accts

matchAccountName :: String -> AccountName -> Bool
matchAccountName s a =
    case matchRegex (mkRegex s) a of
      Nothing -> False
      otherwise -> True

indentAccountName ::  Int -> AccountName -> String
indentAccountName indentcorrection a = 
    replicate (indentlevel * 2) ' ' ++ (accountLeafName a)
    where indentlevel = ((accountNameLevel a) - 1) + indentcorrection


-- We could almost get by with just the above, but we need smarter
-- structures to eg display the account tree with boring accounts elided.
-- first, here is a tree of AccountNames; Account and Account tree are
-- defined later.

accountNameTreeFrom_props =
    [
     accountNameTreeFrom ["a"]       == Node "top" [Node "a" []],
     accountNameTreeFrom ["a","b"]   == Node "top" [Node "a" [], Node "b" []],
     accountNameTreeFrom ["a","a:b"] == Node "top" [Node "a" [Node "a:b" []]],
     accountNameTreeFrom ["a:b"]     == Node "top" [Node "a" [Node "a:b" []]]
    ]
accountNameTreeFrom :: [AccountName] -> Tree AccountName
accountNameTreeFrom accts = 
    Node "top" (accountsFrom (topAccountNames accts))
        where
          accountsFrom :: [AccountName] -> [Tree AccountName]
          accountsFrom [] = []
          accountsFrom as = [Node a (accountsFrom $ subs a) | a <- as]
          subs = (subAccountNamesFrom accts)

filterAccountNameTree :: [String] -> Bool -> Int -> Tree AccountName -> Tree AccountName
filterAccountNameTree pats keepsubs maxdepth =
    treefilter (\a -> matchpats a || (keepsubs && issubofmatch a)) .
    treeprune maxdepth
    where
      matchpats a = any (match a) pats
      match a pat = matchAccountName pat $ accountLeafName a
      issubofmatch a = any matchpats $ parentAccountNames a

