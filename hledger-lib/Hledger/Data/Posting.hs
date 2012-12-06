{-|

A 'Posting' represents a change (by some 'MixedAmount') of the balance in
some 'Account'.  Each 'Transaction' contains two or more postings which
should add up to 0. Postings reference their parent transaction, so we can
look up the date or description there.

-}

module Hledger.Data.Posting (
  -- * Posting
  nullposting,
  posting,
  post,
  -- * operations
  postingCleared,
  isReal,
  isVirtual,
  isBalancedVirtual,
  isEmptyPosting,
  hasAmount,
  postingAllTags,
  transactionAllTags,
  -- * date operations
  postingDate,
  postingDate2,
  isPostingInDateSpan,
  postingsDateSpan,
  -- * account name operations
  accountNamesFromPostings,
  accountNamePostingType,
  accountNameWithoutPostingType,
  accountNameWithPostingType,
  joinAccountNames,
  concatAccountNames,
  accountNameApplyAliases,
  -- * arithmetic
  sumPostings,
  -- * rendering
  showPosting,
  showPostingForRegister,
  -- * misc.
  showComment,
  tests_Hledger_Data_Posting
)
where
import Data.List
import Data.Maybe
import Data.Ord
import Data.Time.Calendar
import Safe
import Test.HUnit
import Text.Printf

import Hledger.Utils
import Hledger.Data.Types
import Hledger.Data.Amount
import Hledger.Data.AccountName
import Hledger.Data.Dates (nulldate, spanContainsDate)


instance Show Posting where show = showPosting

nullposting, posting :: Posting
nullposting = Posting
                {pdate=Nothing
                ,pdate2=Nothing
                ,pstatus=False
                ,paccount=""
                ,pamount=nullmixedamt
                ,pcomment=""
                ,ptype=RegularPosting
                ,ptags=[]
                ,ptransaction=Nothing
                }
posting = nullposting

post :: AccountName -> Amount -> Posting
post acct amt = posting {paccount=acct, pamount=mixed amt}

showPosting :: Posting -> String
showPosting p@Posting{paccount=a,pamount=amt,ptype=t} =
    unlines $ [concatTopPadded [showaccountname a ++ " ", showamount amt, showComment (pcomment p)]]
    where
      ledger3ishlayout = False
      acctnamewidth = if ledger3ishlayout then 25 else 22
      showaccountname = printf ("%-"++(show acctnamewidth)++"s") . bracket . elideAccountName width
      (bracket,width) = case t of
                          BalancedVirtualPosting -> (\s -> "["++s++"]", acctnamewidth-2)
                          VirtualPosting -> (\s -> "("++s++")", acctnamewidth-2)
                          _ -> (id,acctnamewidth)
      showamount = padleft 12 . showMixedAmount


showComment :: String -> String
showComment s = if null s then "" else "  ;" ++ s

-- XXX refactor
showPostingForRegister :: Posting -> String
showPostingForRegister (Posting{paccount=a,pamount=amt,ptype=t}) =
    concatTopPadded [showaccountname a ++ " ", showamount amt]
    where
      ledger3ishlayout = False
      acctnamewidth = if ledger3ishlayout then 25 else 22
      showaccountname = printf ("%-"++(show acctnamewidth)++"s") . bracket . elideAccountName width
      (bracket,width) = case t of
                          BalancedVirtualPosting -> (\s -> "["++s++"]", acctnamewidth-2)
                          VirtualPosting -> (\s -> "("++s++")", acctnamewidth-2)
                          _ -> (id,acctnamewidth)
      showamount = padleft 12 . showMixedAmountWithoutPrice

isReal :: Posting -> Bool
isReal p = ptype p == RegularPosting

isVirtual :: Posting -> Bool
isVirtual p = ptype p == VirtualPosting

isBalancedVirtual :: Posting -> Bool
isBalancedVirtual p = ptype p == BalancedVirtualPosting

hasAmount :: Posting -> Bool
hasAmount = (/= missingmixedamt) . pamount

accountNamesFromPostings :: [Posting] -> [AccountName]
accountNamesFromPostings = nub . map paccount

sumPostings :: [Posting] -> MixedAmount
sumPostings = sum . map pamount

-- | Get a posting's (primary) date - it's own primary date if specified,
-- otherwise the parent transaction's primary date, or the null date if
-- there is no parent transaction.
postingDate :: Posting -> Day
postingDate p = fromMaybe txndate $ pdate p
    where 
      txndate = maybe nulldate tdate $ ptransaction p

-- | Get a posting's secondary (secondary) date, which is the first of:
-- posting's secondary date, transaction's secondary date, posting's
-- primary date, transaction's primary date, or the null date if there is
-- no parent transaction.
postingDate2 :: Posting -> Day
postingDate2 p = headDef nulldate $ catMaybes dates
  where dates = [pdate2 p
                ,maybe Nothing tdate2 $ ptransaction p
                ,pdate p
                ,maybe Nothing (Just . tdate) $ ptransaction p
                ]

-- |Is this posting cleared? If this posting was individually marked
-- as cleared, returns True. Otherwise, return the parent
-- transaction's cleared status or, if there is no parent
-- transaction, return False.
postingCleared :: Posting -> Bool
postingCleared p = if pstatus p
                    then True
                    else maybe False tstatus $ ptransaction p

-- | Tags for this posting including any inherited from its parent transaction.
postingAllTags :: Posting -> [Tag]
postingAllTags p = ptags p ++ maybe [] transactionAllTags (ptransaction p)

-- | Tags for this transaction including any inherited from above, when that is implemented.
transactionAllTags :: Transaction -> [Tag]
transactionAllTags t = ttags t

-- | Does this posting fall within the given date span ?
isPostingInDateSpan :: DateSpan -> Posting -> Bool
isPostingInDateSpan s = spanContainsDate s . postingDate

isEmptyPosting :: Posting -> Bool
isEmptyPosting = isZeroMixedAmount . pamount

-- | Get the minimal date span which contains all the postings, or the
-- null date span if there are none.
postingsDateSpan :: [Posting] -> DateSpan
postingsDateSpan [] = DateSpan Nothing Nothing
postingsDateSpan ps = DateSpan (Just $ postingDate $ head ps') (Just $ addDays 1 $ postingDate $ last ps')
    where ps' = sortBy (comparing postingDate) ps

-- AccountName stuff that depends on PostingType

accountNamePostingType :: AccountName -> PostingType
accountNamePostingType a
    | null a = RegularPosting
    | head a == '[' && last a == ']' = BalancedVirtualPosting
    | head a == '(' && last a == ')' = VirtualPosting
    | otherwise = RegularPosting

accountNameWithoutPostingType :: AccountName -> AccountName
accountNameWithoutPostingType a = case accountNamePostingType a of
                                    BalancedVirtualPosting -> init $ tail a
                                    VirtualPosting -> init $ tail a
                                    RegularPosting -> a

accountNameWithPostingType :: PostingType -> AccountName -> AccountName
accountNameWithPostingType BalancedVirtualPosting a = "["++accountNameWithoutPostingType a++"]"
accountNameWithPostingType VirtualPosting a = "("++accountNameWithoutPostingType a++")"
accountNameWithPostingType RegularPosting a = accountNameWithoutPostingType a

-- | Prefix one account name to another, preserving posting type
-- indicators like concatAccountNames.
joinAccountNames :: AccountName -> AccountName -> AccountName
joinAccountNames a b = concatAccountNames $ filter (not . null) [a,b]

-- | Join account names into one. If any of them has () or [] posting type
-- indicators, these (the first type encountered) will also be applied to
-- the resulting account name.
concatAccountNames :: [AccountName] -> AccountName
concatAccountNames as = accountNameWithPostingType t $ intercalate ":" $ map accountNameWithoutPostingType as
    where t = headDef RegularPosting $ filter (/= RegularPosting) $ map accountNamePostingType as

-- | Rewrite an account name using the first applicable alias from the given list, if any.
accountNameApplyAliases :: [(AccountName,AccountName)] -> AccountName -> AccountName
accountNameApplyAliases aliases a = withorigtype
    where
      (a',t) = (accountNameWithoutPostingType a, accountNamePostingType a)
      firstmatchingalias = headDef Nothing $ map Just $ filter (\(orig,_) -> orig == a' || orig `isAccountNamePrefixOf` a') aliases
      rewritten = maybe a' (\(orig,alias) -> alias++drop (length orig) a') firstmatchingalias
      withorigtype = accountNameWithPostingType t rewritten

tests_Hledger_Data_Posting = TestList [

  "accountNamePostingType" ~: do
    accountNamePostingType "a" `is` RegularPosting
    accountNamePostingType "(a)" `is` VirtualPosting
    accountNamePostingType "[a]" `is` BalancedVirtualPosting

 ,"accountNameWithoutPostingType" ~: do
    accountNameWithoutPostingType "(a)" `is` "a"

 ,"accountNameWithPostingType" ~: do
    accountNameWithPostingType VirtualPosting "[a]" `is` "(a)"

 ,"joinAccountNames" ~: do
    "a" `joinAccountNames` "b:c" `is` "a:b:c"
    "a" `joinAccountNames` "(b:c)" `is` "(a:b:c)"
    "[a]" `joinAccountNames` "(b:c)" `is` "[a:b:c]"
    "" `joinAccountNames` "a" `is` "a"

 ,"concatAccountNames" ~: do
    concatAccountNames [] `is` ""
    concatAccountNames ["a","(b)","[c:d]"] `is` "(a:b:c:d)"

 ]
 
