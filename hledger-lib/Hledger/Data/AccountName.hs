{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-|

'AccountName's are strings like @assets:cash:petty@, with multiple
components separated by ':'.  From a set of these we derive the account
hierarchy.

-}

module Hledger.Data.AccountName (
   accountLeafName
  ,accountNameComponents
  ,accountNameDrop
  ,accountNameFromComponents
  ,accountNameLevel
  ,accountNameToAccountOnlyRegex
  ,accountNameToAccountOnlyRegexCI
  ,accountNameToAccountRegex
  ,accountNameToAccountRegexCI
  ,accountNameTreeFrom
  ,accountSummarisedName
  ,accountNameInferType
  ,accountNameType
  ,assetAccountRegex
  ,cashAccountRegex
  ,liabilityAccountRegex
  ,equityAccountRegex
  ,conversionAccountRegex
  ,revenueAccountRegex
  ,expenseAccountRegex
  ,acctsep
  ,acctsepchar
  ,clipAccountName
  ,clipOrEllipsifyAccountName
  ,elideAccountName
  ,escapeName
  ,expandAccountName
  ,expandAccountNames
  ,isAccountNamePrefixOf
--  ,isAccountRegex
  ,isSubAccountNameOf
  ,parentAccountName
  ,parentAccountNames
  ,subAccountNamesFrom
  ,topAccountNames
  ,unbudgetedAccountName
  ,tests_AccountName
)
where

import Control.Applicative ((<|>))
import Data.Foldable (asum, toList)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.Tree (Tree(..))
import Text.DocLayout (realLength)

import Hledger.Data.Types
import Hledger.Utils

-- $setup
-- >>> :set -XOverloadedStrings

acctsepchar :: Char
acctsepchar = ':'

acctsep :: Text
acctsep = T.pack [acctsepchar]

-- accountNameComponents :: AccountName -> [String]
-- accountNameComponents = splitAtElement acctsepchar

accountNameComponents :: AccountName -> [Text]
accountNameComponents = T.splitOn acctsep

accountNameFromComponents :: [Text] -> AccountName
accountNameFromComponents = T.intercalate acctsep

accountLeafName :: AccountName -> Text
accountLeafName = last . accountNameComponents

-- | Truncate all account name components but the last to two characters.
accountSummarisedName :: AccountName -> Text
accountSummarisedName a
  --   length cs > 1 = take 2 (head cs) ++ ":" ++ a'
  | length cs > 1 = T.intercalate ":" (map (T.take 2) $ init cs) <> ":" <> a'
  | otherwise     = a'
    where
      cs = accountNameComponents a
      a' = accountLeafName a

-- | Regular expressions matching common English top-level account names,
-- used as a fallback when account types are not declared.
assetAccountRegex      = toRegexCI' "^assets?(:|$)"
cashAccountRegex       = toRegexCI' "(investment|receivable|:A/R|:fixed)"
liabilityAccountRegex  = toRegexCI' "^(debts?|liabilit(y|ies))(:|$)"
equityAccountRegex     = toRegexCI' "^equity(:|$)"
conversionAccountRegex = toRegexCI' "^equity:(trad(e|ing)|conversion)s?(:|$)"
revenueAccountRegex    = toRegexCI' "^(income|revenue)s?(:|$)"
expenseAccountRegex    = toRegexCI' "^expenses?(:|$)"

-- | Try to guess an account's type from its name,
-- matching common English top-level account names.
accountNameInferType :: AccountName -> Maybe AccountType
accountNameInferType a
  | a == "asset" || a == "assets"           = Just Asset
  | regexMatchText assetAccountRegex      a = Just $ if regexMatchText cashAccountRegex a then Asset else Cash
  | regexMatchText liabilityAccountRegex  a = Just Liability
  | regexMatchText conversionAccountRegex a = Just Conversion
  | regexMatchText equityAccountRegex     a = Just Equity
  | regexMatchText revenueAccountRegex    a = Just Revenue
  | regexMatchText expenseAccountRegex    a = Just Expense
  | otherwise                               = Nothing

-- Extract the 'AccountType' of an 'AccountName' by looking it up in the
-- provided Map, traversing the parent accounts if necessary. If none of those
-- work, try 'accountNameInferType'.
accountNameType :: M.Map AccountName AccountType -> AccountName -> Maybe AccountType
accountNameType atypes a = asum (map (`M.lookup` atypes) $ a : parentAccountNames a)
                         <|> accountNameInferType a

accountNameLevel :: AccountName -> Int
accountNameLevel "" = 0
accountNameLevel a = T.length (T.filter (==acctsepchar) a) + 1

-- | A top-level account prefixed to some accounts in budget reports.
-- Defined here so it can be ignored by accountNameDrop.
unbudgetedAccountName :: T.Text
unbudgetedAccountName = "<unbudgeted>"

-- | Remove some number of account name components from the front of the account name.
-- If the special "<unbudgeted>" top-level account is present, it is preserved and
-- dropping affects the rest of the account name.
accountNameDrop :: Int -> AccountName -> AccountName
accountNameDrop n a
  | a == unbudgetedAccountName = a
  | unbudgetedAccountAndSep `T.isPrefixOf` a =
      case accountNameDrop n $ T.drop (T.length unbudgetedAccountAndSep) a of
        "" -> unbudgetedAccountName
        a' -> unbudgetedAccountAndSep <> a'
  | otherwise = accountNameFromComponentsOrElide . drop n $ accountNameComponents a
  where
    unbudgetedAccountAndSep = unbudgetedAccountName <> acctsep
    accountNameFromComponentsOrElide [] = "..."
    accountNameFromComponentsOrElide xs = accountNameFromComponents xs

-- | Sorted unique account names implied by these account names,
-- ie these plus all their parent accounts up to the root.
-- Eg: ["a:b:c","d:e"] -> ["a","a:b","a:b:c","d","d:e"]
expandAccountNames :: [AccountName] -> [AccountName]
expandAccountNames = toList . foldMap (S.fromList . expandAccountName)

-- | "a:b:c" -> ["a","a:b","a:b:c"]
expandAccountName :: AccountName -> [AccountName]
expandAccountName = map accountNameFromComponents . NE.tail . NE.inits . accountNameComponents

-- | ["a:b:c","d:e"] -> ["a","d"]
topAccountNames :: [AccountName] -> [AccountName]
topAccountNames = filter ((1==) . accountNameLevel) . expandAccountNames

parentAccountName :: AccountName -> AccountName
parentAccountName = accountNameFromComponents . init . accountNameComponents

parentAccountNames :: AccountName -> [AccountName]
parentAccountNames a = parentAccountNames' $ parentAccountName a
    where
      parentAccountNames' "" = []
      parentAccountNames' a = a : parentAccountNames' (parentAccountName a)

-- | Is the first account a parent or other ancestor of (and not the same as) the second ?
isAccountNamePrefixOf :: AccountName -> AccountName -> Bool
isAccountNamePrefixOf = T.isPrefixOf . (<> acctsep)

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

--nullaccountnametree = Node "root" []

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
  | " (split)" `T.isSuffixOf` s =
    let
      names = T.splitOn ", " $ T.take (T.length s - 8) s
      widthpername = max 0 (width - 8 - 2 * (max 1 (length names) - 1)) `div` length names
    in
     fitText Nothing (Just width) True False $
     (<>" (split)") $
     T.intercalate ", "
     [accountNameFromComponents $ elideparts widthpername [] $ accountNameComponents s' | s' <- names]
  | otherwise =
    fitText Nothing (Just width) True False $ accountNameFromComponents $ elideparts width [] $ accountNameComponents s
      where
        elideparts :: Int -> [Text] -> [Text] -> [Text]
        elideparts width done ss
          | realLength (accountNameFromComponents $ done++ss) <= width = done++ss
          | length ss > 1 = elideparts width (done++[textTakeWidth 2 $ head ss]) (tail ss)
          | otherwise = done++ss

-- | Keep only the first n components of an account name, where n
-- is a positive integer. If n is Just 0, returns the empty string, if n is
-- Nothing, return the full name.
clipAccountName :: Maybe Int -> AccountName -> AccountName
clipAccountName Nothing  = id
clipAccountName (Just n) = accountNameFromComponents . take n . accountNameComponents

-- | Keep only the first n components of an account name, where n
-- is a positive integer. If n is Just 0, returns "...", if n is Nothing, return
-- the full name.
clipOrEllipsifyAccountName :: Maybe Int -> AccountName -> AccountName
clipOrEllipsifyAccountName (Just 0) = const "..."
clipOrEllipsifyAccountName n        = clipAccountName n

-- | Escape an AccountName for use within a regular expression.
-- >>> putStr . T.unpack $ escapeName "First?!#$*?$(*) !@^#*? %)*!@#"
-- First\?!#\$\*\?\$\(\*\) !@\^#\*\? %\)\*!@#
escapeName :: AccountName -> Text
escapeName = T.concatMap escapeChar
  where
    escapeChar c = if c `elem` escapedChars then T.snoc "\\" c else T.singleton c
    escapedChars = ['[', '?', '+', '|', '(', ')', '*', '$', '^', '\\']

-- | Convert an account name to a regular expression matching it and its subaccounts.
accountNameToAccountRegex :: AccountName -> Regexp
accountNameToAccountRegex a = toRegex' $ "^" <> escapeName a <> "(:|$)"  -- PARTIAL: Is this safe after escapeName?

-- | Convert an account name to a regular expression matching it and its subaccounts,
-- case insensitively.
accountNameToAccountRegexCI :: AccountName -> Regexp
accountNameToAccountRegexCI a = toRegexCI' $ "^" <> escapeName a <> "(:|$)"  -- PARTIAL: Is this safe after escapeName?

-- | Convert an account name to a regular expression matching it but not its subaccounts.
accountNameToAccountOnlyRegex :: AccountName -> Regexp
accountNameToAccountOnlyRegex a = toRegex' $ "^" <> escapeName a <> "$" -- PARTIAL: Is this safe after escapeName?

-- | Convert an account name to a regular expression matching it but not its subaccounts,
-- case insensitively.
accountNameToAccountOnlyRegexCI :: AccountName -> Regexp
accountNameToAccountOnlyRegexCI a = toRegexCI' $ "^" <> escapeName a <> "$" -- PARTIAL: Is this safe after escapeName?

-- -- | Does this string look like an exact account-matching regular expression ?
--isAccountRegex  :: String -> Bool
--isAccountRegex s = take 1 s == "^" && take 5 (reverse s) == ")$|:("

tests_AccountName = testGroup "AccountName" [
   testCase "accountNameTreeFrom" $ do
    accountNameTreeFrom ["a"]       @?= Node "root" [Node "a" []]
    accountNameTreeFrom ["a","b"]   @?= Node "root" [Node "a" [], Node "b" []]
    accountNameTreeFrom ["a","a:b"] @?= Node "root" [Node "a" [Node "a:b" []]]
    accountNameTreeFrom ["a:b:c"]   @?= Node "root" [Node "a" [Node "a:b" [Node "a:b:c" []]]]
  ,testCase "expandAccountNames" $ do
    expandAccountNames ["assets:cash","assets:checking","expenses:vacation"] @?=
     ["assets","assets:cash","assets:checking","expenses","expenses:vacation"]
  ,testCase "isAccountNamePrefixOf" $ do
    "assets" `isAccountNamePrefixOf` "assets" @?= False
    "assets" `isAccountNamePrefixOf` "assets:bank" @?= True
    "assets" `isAccountNamePrefixOf` "assets:bank:checking" @?= True
    "my assets" `isAccountNamePrefixOf` "assets:bank" @?= False
  ,testCase "isSubAccountNameOf" $ do
    "assets" `isSubAccountNameOf` "assets" @?= False
    "assets:bank" `isSubAccountNameOf` "assets" @?= True
    "assets:bank:checking" `isSubAccountNameOf` "assets" @?= False
    "assets:bank" `isSubAccountNameOf` "my assets" @?= False
  ,testCase "accountNameInferType" $ do
    accountNameInferType "assets"            @?= Just Asset
    accountNameInferType "assets:cash"       @?= Just Cash
    accountNameInferType "assets:A/R"        @?= Just Asset
    accountNameInferType "liabilities"       @?= Just Liability
    accountNameInferType "equity"            @?= Just Equity
    accountNameInferType "equity:conversion" @?= Just Conversion
    accountNameInferType "expenses"          @?= Just Expense
    accountNameInferType "revenues"          @?= Just Revenue
    accountNameInferType "revenue"           @?= Just Revenue
    accountNameInferType "income"            @?= Just Revenue
 ]

