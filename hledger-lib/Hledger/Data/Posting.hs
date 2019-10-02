{-|

A 'Posting' represents a change (by some 'MixedAmount') of the balance in
some 'Account'.  Each 'Transaction' contains two or more postings which
should add up to 0. Postings reference their parent transaction, so we can
look up the date or description there.

-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

module Hledger.Data.Posting (
  -- * Posting
  nullposting,
  posting,
  post,
  vpost,
  post',
  vpost',
  nullsourcepos,
  nullassertion,
  assertion,
  balassert,
  balassertTot,
  balassertParInc,
  balassertTotInc,
  -- * operations
  originalPosting,
  postingStatus,
  isReal,
  isVirtual,
  isBalancedVirtual,
  isEmptyPosting,
  hasBalanceAssignment,
  hasAmount,
  postingAllTags,
  transactionAllTags,
  relatedPostings,
  removePrices,
  -- * date operations
  postingDate,
  postingDate2,
  isPostingInDateSpan,
  isPostingInDateSpan',
  postingsDateSpan,
  postingsDateSpan',
  -- * account name operations
  accountNamesFromPostings,
  accountNamePostingType,
  accountNameWithoutPostingType,
  accountNameWithPostingType,
  joinAccountNames,
  concatAccountNames,
  accountNameApplyAliases,
  accountNameApplyAliasesMemo,
  -- * comment/tag operations
  commentJoin,
  commentAddTag,
  commentAddTagNextLine,
  -- * arithmetic
  sumPostings,
  -- * rendering
  showPosting,
  -- * misc.
  showComment,
  postingTransformAmount,
  postingApplyValuation,
  postingToCost,
  tests_Posting
)
where
import Data.List
import qualified Data.Map as M
import Data.Maybe
import Data.MemoUgly (memo)
#if !(MIN_VERSION_base(4,11,0))
import Data.Monoid
#endif
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Calendar
import Safe

import Hledger.Utils
import Hledger.Data.Types
import Hledger.Data.Amount
import Hledger.Data.AccountName
import Hledger.Data.Dates (nulldate, spanContainsDate)
import Hledger.Data.Valuation



nullposting, posting :: Posting
nullposting = Posting
                {pdate=Nothing
                ,pdate2=Nothing
                ,pstatus=Unmarked
                ,paccount=""
                ,pamount=nullmixedamt
                ,pcomment=""
                ,ptype=RegularPosting
                ,ptags=[]
                ,pbalanceassertion=Nothing
                ,ptransaction=Nothing
                ,poriginal=Nothing
                }
posting = nullposting

-- constructors

-- | Make a posting to an account.
post :: AccountName -> Amount -> Posting
post acc amt = posting {paccount=acc, pamount=Mixed [amt]}

-- | Make a virtual (unbalanced) posting to an account.
vpost :: AccountName -> Amount -> Posting
vpost acc amt = (post acc amt){ptype=VirtualPosting}

-- | Make a posting to an account, maybe with a balance assertion.
post' :: AccountName -> Amount -> Maybe BalanceAssertion -> Posting
post' acc amt ass = posting {paccount=acc, pamount=Mixed [amt], pbalanceassertion=ass}

-- | Make a virtual (unbalanced) posting to an account, maybe with a balance assertion.
vpost' :: AccountName -> Amount -> Maybe BalanceAssertion -> Posting
vpost' acc amt ass = (post' acc amt ass){ptype=VirtualPosting, pbalanceassertion=ass}

nullsourcepos :: GenericSourcePos
nullsourcepos = JournalSourcePos "" (1,1)

nullassertion, assertion :: BalanceAssertion
nullassertion = BalanceAssertion
                  {baamount=nullamt
                  ,batotal=False
                  ,bainclusive=False
                  ,baposition=nullsourcepos
                  }
assertion = nullassertion

-- | Make a partial, exclusive balance assertion.
balassert :: Amount -> Maybe BalanceAssertion
balassert amt = Just $ nullassertion{baamount=amt}

-- | Make a total, exclusive balance assertion.
balassertTot :: Amount -> Maybe BalanceAssertion
balassertTot amt = Just $ nullassertion{baamount=amt, batotal=True}

-- | Make a partial, inclusive balance assertion.
balassertParInc :: Amount -> Maybe BalanceAssertion
balassertParInc amt = Just $ nullassertion{baamount=amt, bainclusive=True}

-- | Make a total, inclusive balance assertion.
balassertTotInc :: Amount -> Maybe BalanceAssertion
balassertTotInc amt = Just $ nullassertion{baamount=amt, batotal=True, bainclusive=True}

-- Get the original posting, if any.
originalPosting :: Posting -> Posting
originalPosting p = fromMaybe p $ poriginal p

-- XXX once rendered user output, but just for debugging now; clean up
showPosting :: Posting -> String
showPosting p@Posting{paccount=a,pamount=amt,ptype=t} =
    unlines $ [concatTopPadded [show (postingDate p) ++ " ", showaccountname a ++ " ", showamount amt, showComment (pcomment p)]]
    where
      ledger3ishlayout = False
      acctnamewidth = if ledger3ishlayout then 25 else 22
      showaccountname = fitString (Just acctnamewidth) Nothing False False . bracket . T.unpack . elideAccountName width
      (bracket,width) = case t of
                          BalancedVirtualPosting -> (\s -> "["++s++"]", acctnamewidth-2)
                          VirtualPosting -> (\s -> "("++s++")", acctnamewidth-2)
                          _ -> (id,acctnamewidth)
      showamount = padLeftWide 12 . showMixedAmount


showComment :: Text -> String
showComment t = if T.null t then "" else "  ;" ++ T.unpack t

isReal :: Posting -> Bool
isReal p = ptype p == RegularPosting

isVirtual :: Posting -> Bool
isVirtual p = ptype p == VirtualPosting

isBalancedVirtual :: Posting -> Bool
isBalancedVirtual p = ptype p == BalancedVirtualPosting

hasAmount :: Posting -> Bool
hasAmount = (/= missingmixedamt) . pamount

hasBalanceAssignment :: Posting -> Bool
hasBalanceAssignment p = not (hasAmount p) && isJust (pbalanceassertion p)

-- | Sorted unique account names referenced by these postings.
accountNamesFromPostings :: [Posting] -> [AccountName]
accountNamesFromPostings = nub . sort . map paccount

sumPostings :: [Posting] -> MixedAmount
sumPostings = sumStrict . map pamount

-- | Remove all prices of a posting
removePrices :: Posting -> Posting
removePrices p = p{ pamount = Mixed $ remove <$> amounts (pamount p) }
  where remove a = a { aprice = Nothing }

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
                ,fmap tdate (ptransaction p)
                ]

-- | Get a posting's status. This is cleared or pending if those are
-- explicitly set on the posting, otherwise the status of its parent
-- transaction, or unmarked if there is no parent transaction. (Note
-- the ambiguity, unmarked can mean "posting and transaction are both
-- unmarked" or "posting is unmarked and don't know about the transaction".
postingStatus :: Posting -> Status
postingStatus Posting{pstatus=s, ptransaction=mt}
  | s == Unmarked = case mt of Just t  -> tstatus t
                               Nothing -> Unmarked
  | otherwise = s

-- | Tags for this posting including any inherited from its parent transaction.
postingAllTags :: Posting -> [Tag]
postingAllTags p = ptags p ++ maybe [] ttags (ptransaction p)

-- | Tags for this transaction including any from its postings.
transactionAllTags :: Transaction -> [Tag]
transactionAllTags t = ttags t ++ concatMap ptags (tpostings t)

-- Get the other postings from this posting's transaction.
relatedPostings :: Posting -> [Posting]
relatedPostings p@Posting{ptransaction=Just t} = filter (/= p) $ tpostings t
relatedPostings _ = []

-- | Does this posting fall within the given date span ?
isPostingInDateSpan :: DateSpan -> Posting -> Bool
isPostingInDateSpan s = spanContainsDate s . postingDate

-- --date2-sensitive version, separate for now to avoid disturbing multiBalanceReport.
isPostingInDateSpan' :: WhichDate -> DateSpan -> Posting -> Bool
isPostingInDateSpan' PrimaryDate   s = spanContainsDate s . postingDate
isPostingInDateSpan' SecondaryDate s = spanContainsDate s . postingDate2

isEmptyPosting :: Posting -> Bool
isEmptyPosting = isZeroMixedAmount . pamount

-- | Get the minimal date span which contains all the postings, or the
-- null date span if there are none.
postingsDateSpan :: [Posting] -> DateSpan
postingsDateSpan [] = DateSpan Nothing Nothing
postingsDateSpan ps = DateSpan (Just $ postingDate $ head ps') (Just $ addDays 1 $ postingDate $ last ps')
    where ps' = sortOn postingDate ps

-- --date2-sensitive version, as above.
postingsDateSpan' :: WhichDate -> [Posting] -> DateSpan
postingsDateSpan' _  [] = DateSpan Nothing Nothing
postingsDateSpan' wd ps = DateSpan (Just $ postingdate $ head ps') (Just $ addDays 1 $ postingdate $ last ps')
    where
      ps' = sortOn postingdate ps
      postingdate = if wd == PrimaryDate then postingDate else postingDate2

-- AccountName stuff that depends on PostingType

accountNamePostingType :: AccountName -> PostingType
accountNamePostingType a
    | T.null a = RegularPosting
    | T.head a == '[' && T.last a == ']' = BalancedVirtualPosting
    | T.head a == '(' && T.last a == ')' = VirtualPosting
    | otherwise = RegularPosting

accountNameWithoutPostingType :: AccountName -> AccountName
accountNameWithoutPostingType a = case accountNamePostingType a of
                                    BalancedVirtualPosting -> T.init $ T.tail a
                                    VirtualPosting -> T.init $ T.tail a
                                    RegularPosting -> a

accountNameWithPostingType :: PostingType -> AccountName -> AccountName
accountNameWithPostingType BalancedVirtualPosting a = "["<>accountNameWithoutPostingType a<>"]"
accountNameWithPostingType VirtualPosting a = "("<>accountNameWithoutPostingType a<>")"
accountNameWithPostingType RegularPosting a = accountNameWithoutPostingType a

-- | Prefix one account name to another, preserving posting type
-- indicators like concatAccountNames.
joinAccountNames :: AccountName -> AccountName -> AccountName
joinAccountNames a b = concatAccountNames $ filter (not . T.null) [a,b]

-- | Join account names into one. If any of them has () or [] posting type
-- indicators, these (the first type encountered) will also be applied to
-- the resulting account name.
concatAccountNames :: [AccountName] -> AccountName
concatAccountNames as = accountNameWithPostingType t $ T.intercalate ":" $ map accountNameWithoutPostingType as
    where t = headDef RegularPosting $ filter (/= RegularPosting) $ map accountNamePostingType as

-- | Rewrite an account name using all matching aliases from the given list, in sequence.
-- Each alias sees the result of applying the previous aliases.
accountNameApplyAliases :: [AccountAlias] -> AccountName -> AccountName
accountNameApplyAliases aliases a = accountNameWithPostingType atype aname'
  where
    (aname,atype) = (accountNameWithoutPostingType a, accountNamePostingType a)
    aname' = foldl
             (\acct alias -> dbg6 "result" $ aliasReplace (dbg6 "alias" alias) (dbg6 "account" acct))
             aname
             aliases

-- | Memoising version of accountNameApplyAliases, maybe overkill.
accountNameApplyAliasesMemo :: [AccountAlias] -> AccountName -> AccountName
accountNameApplyAliasesMemo aliases = memo (accountNameApplyAliases aliases)
  -- XXX re-test this memoisation

-- aliasMatches :: AccountAlias -> AccountName -> Bool
-- aliasMatches (BasicAlias old _) a = old `isAccountNamePrefixOf` a
-- aliasMatches (RegexAlias re  _) a = regexMatchesCI re a

aliasReplace :: AccountAlias -> AccountName -> AccountName
aliasReplace (BasicAlias old new) a
  | old `isAccountNamePrefixOf` a || old == a = new <> T.drop (T.length old) a
  | otherwise = a
aliasReplace (RegexAlias re repl) a = T.pack $ regexReplaceCIMemo re repl $ T.unpack a -- XXX

-- | Apply a specified valuation to this posting's amount, using the
-- provided price oracle, commodity styles, reference dates, and
-- whether this is for a multiperiod report or not. See
-- amountApplyValuation.
postingApplyValuation :: PriceOracle -> M.Map CommoditySymbol AmountStyle -> Day -> Maybe Day -> Day -> Bool -> Posting -> ValuationType -> Posting
postingApplyValuation priceoracle styles periodlast mreportlast today ismultiperiod p v =
  case v of
    AtCost    Nothing            -> postingToCost styles p
    AtCost    mc                 -> postingValueAtDate priceoracle styles mc periodlast $ postingToCost styles p
    AtEnd     mc                 -> postingValueAtDate priceoracle styles mc periodlast p
    AtNow     mc                 -> postingValueAtDate priceoracle styles mc today p
    AtDefault mc | ismultiperiod -> postingValueAtDate priceoracle styles mc periodlast p
    AtDefault mc                 -> postingValueAtDate priceoracle styles mc (fromMaybe today mreportlast) p
    AtDate d  mc                 -> postingValueAtDate priceoracle styles mc d p

-- | Convert this posting's amount to cost, and apply the appropriate amount styles.
postingToCost :: M.Map CommoditySymbol AmountStyle -> Posting -> Posting
postingToCost styles p@Posting{pamount=a} = p{pamount=mixedAmountToCost styles a}

-- | Convert this posting's amount to market value in the given commodity,
-- or the default valuation commodity, at the given valuation date,
-- using the given market price oracle.
-- When market prices available on that date are not sufficient to
-- calculate the value, amounts are left unchanged.
postingValueAtDate :: PriceOracle -> M.Map CommoditySymbol AmountStyle -> Maybe CommoditySymbol -> Day -> Posting -> Posting
postingValueAtDate priceoracle styles mc d p = postingTransformAmount (mixedAmountValueAtDate priceoracle styles mc d) p

-- | Apply a transform function to this posting's amount.
postingTransformAmount :: (MixedAmount -> MixedAmount) -> Posting -> Posting
postingTransformAmount f p@Posting{pamount=a} = p{pamount=f a}

-- | Join two parts of a comment, eg a tag and another tag, or a tag
-- and a non-tag, on a single line. Interpolates a comma and space
-- unless one of the parts is empty.
commentJoin :: Text -> Text -> Text
commentJoin c1 c2
  | T.null c1 = c2
  | T.null c2 = c1
  | otherwise = c1 <> ", " <> c2

-- | Add a tag to a comment, comma-separated from any prior content.
-- A space is inserted following the colon, before the value.
commentAddTag :: Text -> Tag -> Text
commentAddTag c (t,v)
  | T.null c' = tag
  | otherwise = c' `commentJoin` tag
  where
    c'  = textchomp c
    tag = t <> ": " <> v

-- | Add a tag on its own line to a comment, preserving any prior content.
-- A space is inserted following the colon, before the value.
commentAddTagNextLine :: Text -> Tag -> Text
commentAddTagNextLine cmt (t,v) =
  cmt <> if "\n" `T.isSuffixOf` cmt then "" else "\n" <> t <> ": " <> v 


-- tests

tests_Posting = tests "Posting" [

  tests "accountNamePostingType" [
    accountNamePostingType "a" `is` RegularPosting
    ,accountNamePostingType "(a)" `is` VirtualPosting
    ,accountNamePostingType "[a]" `is` BalancedVirtualPosting
  ]

 ,tests "accountNameWithoutPostingType" [
    accountNameWithoutPostingType "(a)" `is` "a"
  ]

 ,tests "accountNameWithPostingType" [
    accountNameWithPostingType VirtualPosting "[a]" `is` "(a)"
  ]

 ,tests "joinAccountNames" [
    "a" `joinAccountNames` "b:c" `is` "a:b:c"
    ,"a" `joinAccountNames` "(b:c)" `is` "(a:b:c)"
    ,"[a]" `joinAccountNames` "(b:c)" `is` "[a:b:c]"
    ,"" `joinAccountNames` "a" `is` "a"
  ]

 ,tests "concatAccountNames" [
    concatAccountNames [] `is` ""
    ,concatAccountNames ["a","(b)","[c:d]"] `is` "(a:b:c:d)"
  ]

 ,tests "commentAddTag" [
    commentAddTag "" ("a","") `is` "a: "
   ,commentAddTag "[1/2]" ("a","") `is` "[1/2], a: "
  ]

 ,tests "commentAddTagNextLine" [
    commentAddTagNextLine "" ("a","") `is` "\na: "
   ,commentAddTagNextLine "[1/2]" ("a","") `is` "[1/2]\na: "
  ]
 ]

