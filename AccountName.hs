module AccountName
where
import Utils
import Types

sepchar = ':'

accountNameComponents :: AccountName -> [String]
accountNameComponents = splitAtElement sepchar

accountNameFromComponents :: [String] -> AccountName
accountNameFromComponents = concat . intersperse [sepchar]

accountLeafName :: AccountName -> String
accountLeafName = last . accountNameComponents

accountNameLevel :: AccountName -> Int
accountNameLevel "" = 0
accountNameLevel a = (length $ filter (==sepchar) a) + 1

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

isAccountNamePrefixOf :: AccountName -> AccountName -> Bool
p `isAccountNamePrefixOf` s = ((p ++ [sepchar]) `isPrefixOf` s)

isSubAccountNameOf :: AccountName -> AccountName -> Bool
s `isSubAccountNameOf` p = 
    (p `isAccountNamePrefixOf` s) && (accountNameLevel s == (accountNameLevel p + 1))

subAccountNamesFrom :: [AccountName] -> AccountName -> [AccountName]
subAccountNamesFrom accts a = filter (`isSubAccountNameOf` a) accts

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

