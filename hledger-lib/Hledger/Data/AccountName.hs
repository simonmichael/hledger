{-# LANGUAGE NoMonomorphismRestriction#-}
{-|

'AccountName's are strings like @assets:cash:petty@, with multiple
components separated by ':'.  From a set of these we derive the account
hierarchy.

-}

module Hledger.Data.AccountName
where
import Data.List
import Data.List.Split (splitOn)
import Data.Tree
import Test.HUnit
import Text.Printf

import Hledger.Data.Types
import Hledger.Utils



-- change to use a different separator for nested accounts
acctsepchar = ':'

accountNameComponents :: AccountName -> [String]
accountNameComponents = splitAtElement acctsepchar

accountNameFromComponents :: [String] -> AccountName
accountNameFromComponents = concat . intersperse [acctsepchar]

accountLeafName :: AccountName -> String
accountLeafName = last . accountNameComponents

-- | Truncate all account name components but the last to two characters.
accountSummarisedName :: AccountName -> String
accountSummarisedName a
  --   length cs > 1 = take 2 (head cs) ++ ":" ++ a'
  | length cs > 1 = intercalate ":" (map (take 2) $ init cs) ++ ":" ++ a'
  | otherwise     = a'
    where
      cs = accountNameComponents a
      a' = accountLeafName a

accountNameLevel :: AccountName -> Int
accountNameLevel "" = 0
accountNameLevel a = length (filter (==acctsepchar) a) + 1

accountNameDrop :: Int -> AccountName -> AccountName
accountNameDrop n = accountNameFromComponents . drop n . accountNameComponents

-- | ["a:b:c","d:e"] -> ["a","a:b","a:b:c","d","d:e"]
expandAccountNames :: [AccountName] -> [AccountName]
expandAccountNames as = nub $ concatMap expandAccountName as

-- | "a:b:c" -> ["a","a:b","a:b:c"]
expandAccountName :: AccountName -> [AccountName]
expandAccountName = map accountNameFromComponents . tail . inits . accountNameComponents

-- | ["a:b:c","d:e"] -> ["a","d"]
topAccountNames :: [AccountName] -> [AccountName]
topAccountNames as = [a | a <- expandAccountNames as, accountNameLevel a == 1]

parentAccountName :: AccountName -> AccountName
parentAccountName = accountNameFromComponents . init . accountNameComponents

parentAccountNames :: AccountName -> [AccountName]
parentAccountNames a = parentAccountNames' $ parentAccountName a
    where
      parentAccountNames' "" = []
      parentAccountNames' a = a : parentAccountNames' (parentAccountName a)

-- | Is the first account a parent or other ancestor of (and not the same as) the second ?
isAccountNamePrefixOf :: AccountName -> AccountName -> Bool
isAccountNamePrefixOf = isPrefixOf . (++ [acctsepchar])

isSubAccountNameOf :: AccountName -> AccountName -> Bool
s `isSubAccountNameOf` p =
    (p `isAccountNamePrefixOf` s) && (accountNameLevel s == (accountNameLevel p + 1))

-- | From a list of account names, select those which are direct
-- subaccounts of the given account name.
subAccountNamesFrom :: [AccountName] -> AccountName -> [AccountName]
subAccountNamesFrom accts a = filter (`isSubAccountNameOf` a) accts

-- | Convert a list of account names to a tree.
accountNameTreeFrom :: [AccountName] -> Tree AccountName
accountNameTreeFrom accts =
    Node "root" (accounttreesfrom (topAccountNames accts))
        where
          accounttreesfrom :: [AccountName] -> [Tree AccountName]
          accounttreesfrom [] = []
          accounttreesfrom as = [Node a (accounttreesfrom $ subs a) | a <- as]
          subs = subAccountNamesFrom (expandAccountNames accts)

nullaccountnametree = Node "root" []

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
elideAccountName width s
  -- XXX special case for transactions register's multi-account pseudo-names
  | " (split)" `isSuffixOf` s =
    let
      names = splitOn ", " $ take (length s - 8) s
      widthpername = (max 0 (width - 8 - 2 * (max 1 (length names) - 1))) `div` length names
    in
     fitString Nothing (Just width) True False $
     (++" (split)") $
     intercalate ", " $
     [accountNameFromComponents $ elideparts widthpername [] $ accountNameComponents s' | s' <- names]
  | otherwise =
    fitString Nothing (Just width) True False $ accountNameFromComponents $ elideparts width [] $ accountNameComponents s
      where
        elideparts :: Int -> [String] -> [String] -> [String]
        elideparts width done ss
          | strWidth (accountNameFromComponents $ done++ss) <= width = done++ss
          | length ss > 1 = elideparts width (done++[takeWidth 2 $ head ss]) (tail ss)
          | otherwise = done++ss

-- | Keep only the first n components of an account name, where n
-- is a positive integer. If n is 0, returns the empty string.
clipAccountName :: Int -> AccountName -> AccountName
clipAccountName n = accountNameFromComponents . take n . accountNameComponents

-- | Keep only the first n components of an account name, where n
-- is a positive integer. If n is 0, returns "...".
clipOrEllipsifyAccountName :: Int -> AccountName -> AccountName
clipOrEllipsifyAccountName 0 = const "..."
clipOrEllipsifyAccountName n = accountNameFromComponents . take n . accountNameComponents

-- | Convert an account name to a regular expression matching it and its subaccounts.
accountNameToAccountRegex :: String -> String
accountNameToAccountRegex "" = ""
accountNameToAccountRegex a = printf "^%s(:|$)" a

-- | Convert an account name to a regular expression matching it but not its subaccounts.
accountNameToAccountOnlyRegex :: String -> String
accountNameToAccountOnlyRegex "" = ""
accountNameToAccountOnlyRegex a = printf "^%s$" a

-- | Convert an exact account-matching regular expression to a plain account name.
accountRegexToAccountName :: String -> String
accountRegexToAccountName = regexReplace "^\\^(.*?)\\(:\\|\\$\\)$" "\\1"

-- | Does this string look like an exact account-matching regular expression ?
isAccountRegex  :: String -> Bool
isAccountRegex s = take 1 s == "^" && (take 5 $ reverse s) == ")$|:("

tests_Hledger_Data_AccountName = TestList
 [
  "accountNameTreeFrom" ~: do
    accountNameTreeFrom ["a"]       `is` Node "root" [Node "a" []]
    accountNameTreeFrom ["a","b"]   `is` Node "root" [Node "a" [], Node "b" []]
    accountNameTreeFrom ["a","a:b"] `is` Node "root" [Node "a" [Node "a:b" []]]
    accountNameTreeFrom ["a:b:c"]   `is` Node "root" [Node "a" [Node "a:b" [Node "a:b:c" []]]]

  ,"expandAccountNames" ~:
    expandAccountNames ["assets:cash","assets:checking","expenses:vacation"] `is`
     ["assets","assets:cash","assets:checking","expenses","expenses:vacation"]

  ,"isAccountNamePrefixOf" ~: do
    "assets" `isAccountNamePrefixOf` "assets" `is` False
    "assets" `isAccountNamePrefixOf` "assets:bank" `is` True
    "assets" `isAccountNamePrefixOf` "assets:bank:checking" `is` True
    "my assets" `isAccountNamePrefixOf` "assets:bank" `is` False

  ,"isSubAccountNameOf" ~: do
    "assets" `isSubAccountNameOf` "assets" `is` False
    "assets:bank" `isSubAccountNameOf` "assets" `is` True
    "assets:bank:checking" `isSubAccountNameOf` "assets" `is` False
    "assets:bank" `isSubAccountNameOf` "my assets" `is` False

 ]

