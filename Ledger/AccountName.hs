{-|

'AccountName's are strings like @assets:cash:petty@.
From a set of these we derive the account hierarchy.

-}

module Ledger.AccountName
where
import Ledger.Utils
import Ledger.Types


-- change to use a different separator for nested accounts
acctsepchar = ':'

accountNameComponents :: AccountName -> [String]
accountNameComponents = splitAtElement acctsepchar

accountNameFromComponents :: [String] -> AccountName
accountNameFromComponents = concat . intersperse [acctsepchar]

accountLeafName :: AccountName -> String
accountLeafName = last . accountNameComponents

accountNameLevel :: AccountName -> Int
accountNameLevel "" = 0
accountNameLevel a = (length $ filter (==acctsepchar) a) + 1

-- | ["a:b:c","d:e"] -> ["a","a:b","a:b:c","d","d:e"]
expandAccountNames :: [AccountName] -> [AccountName]
expandAccountNames as = nub $ concat $ map expand as
    where expand as = map accountNameFromComponents (tail $ inits $ accountNameComponents as)

-- | ["a:b:c","d:e"] -> ["a","d"]
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
p `isAccountNamePrefixOf` s = ((p ++ [acctsepchar]) `isPrefixOf` s)

isSubAccountNameOf :: AccountName -> AccountName -> Bool
s `isSubAccountNameOf` p = 
    (p `isAccountNamePrefixOf` s) && (accountNameLevel s == (accountNameLevel p + 1))

subAccountNamesFrom :: [AccountName] -> AccountName -> [AccountName]
subAccountNamesFrom accts a = filter (`isSubAccountNameOf` a) accts

-- | We could almost get by with just the AccountName manipulations
-- above, but we need smarter structures to eg display the account
-- tree with boring accounts elided.  This converts a list of
-- AccountName to a tree (later we will convert that to a tree of
-- 'Account'.)
accountNameTreeFrom :: [AccountName] -> Tree AccountName
accountNameTreeFrom accts = 
    Node "top" (accountsFrom (topAccountNames accts))
        where
          accountsFrom :: [AccountName] -> [Tree AccountName]
          accountsFrom [] = []
          accountsFrom as = [Node a (accountsFrom $ subs a) | a <- as]
          subs = subAccountNamesFrom (expandAccountNames accts)

-- | Elide an account name to fit in the specified width.
-- From the ledger 2.6 news:
-- 
-- @
--   What Ledger now does is that if an account name is too long, it will
--   start abbreviating the first parts of the account name down to two
--   letters in length.  If this results in a string that is still too
--   long, the front will be elided -- not the end.  For example:
--
--     Expenses:Cash           ; OK, not too long
--     Ex:Wednesday:Cash       ; "Expenses" was abbreviated to fit
--     Ex:We:Afternoon:Cash    ; "Expenses" and "Wednesday" abbreviated
--     ; Expenses:Wednesday:Afternoon:Lunch:Snack:Candy:Chocolate:Cash
--     ..:Af:Lu:Sn:Ca:Ch:Cash  ; Abbreviated and elided!
-- @
elideAccountName :: Int -> AccountName -> AccountName
elideAccountName width s = 
    elideLeft width $ accountNameFromComponents $ elideparts width [] $ accountNameComponents s
      where
        elideparts :: Int -> [String] -> [String] -> [String]
        elideparts width done ss
          | (length $ accountNameFromComponents $ done++ss) <= width = done++ss
          | length ss > 1 = elideparts width (done++[take 2 $ head ss]) (tail ss)
          | otherwise = done++ss


