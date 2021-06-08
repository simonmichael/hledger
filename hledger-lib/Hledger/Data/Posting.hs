{-# LANGUAGE NamedFieldPuns #-}
{-|

A 'Posting' represents a change (by some 'MixedAmount') of the balance in
some 'Account'.  Each 'Transaction' contains two or more postings which
should add up to 0. Postings reference their parent transaction, so we can
look up the date or description there.

-}

{-# LANGUAGE OverloadedStrings #-}

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
  postingStripPrices,
  postingApplyAliases,
  -- * date operations
  postingDate,
  postingDate2,
  isPostingInDateSpan,
  isPostingInDateSpan',
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

import Control.Monad (foldM)
import Data.Foldable (asum)
import Data.List.Extra (nubSort)
import qualified Data.Map as M
import Data.Maybe (fromMaybe, isJust)
import Data.MemoUgly (memo)
import Data.List (foldl')
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Calendar (Day)
import Safe (headDef)

import Hledger.Utils
import Hledger.Data.Types
import Hledger.Data.Amount
import Hledger.Data.AccountName
import Hledger.Data.Dates (nulldate, showDate, spanContainsDate)
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
post acc amt = posting {paccount=acc, pamount=mixedAmount amt}

-- | Make a virtual (unbalanced) posting to an account.
vpost :: AccountName -> Amount -> Posting
vpost acc amt = (post acc amt){ptype=VirtualPosting}

-- | Make a posting to an account, maybe with a balance assertion.
post' :: AccountName -> Amount -> Maybe BalanceAssertion -> Posting
post' acc amt ass = posting {paccount=acc, pamount=mixedAmount amt, pbalanceassertion=ass}

-- | Make a virtual (unbalanced) posting to an account, maybe with a balance assertion.
vpost' :: AccountName -> Amount -> Maybe BalanceAssertion -> Posting
vpost' acc amt ass = (post' acc amt ass){ptype=VirtualPosting, pbalanceassertion=ass}

nullsourcepos :: GenericSourcePos
nullsourcepos = JournalSourcePos "" (1,1)

nullassertion :: BalanceAssertion
nullassertion = BalanceAssertion
                  {baamount=nullamt
                  ,batotal=False
                  ,bainclusive=False
                  ,baposition=nullsourcepos
                  }

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
    T.unpack $ textConcatTopPadded [showDate (postingDate p) <> " ", showaccountname a <> " ", showamt, showComment $ pcomment p]
  where
    ledger3ishlayout = False
    acctnamewidth = if ledger3ishlayout then 25 else 22
    showaccountname = fitText (Just acctnamewidth) Nothing False False . bracket . elideAccountName width
    (bracket,width) = case t of
                        BalancedVirtualPosting -> (wrap "[" "]", acctnamewidth-2)
                        VirtualPosting         -> (wrap "(" ")", acctnamewidth-2)
                        _                      -> (id,acctnamewidth)
    showamt = wbToText $ showMixedAmountB noColour{displayMinWidth=Just 12} amt


showComment :: Text -> Text
showComment t = if T.null t then "" else "  ;" <> t

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
accountNamesFromPostings = nubSort . map paccount

-- | Sum all amounts from a list of postings.
sumPostings :: [Posting] -> MixedAmount
sumPostings = foldl' (\amt p -> maPlus amt $ pamount p) nullmixedamt

-- | Strip all prices from a Posting.
postingStripPrices :: Posting -> Posting
postingStripPrices = postingTransformAmount mixedAmountStripPrices

-- | Get a posting's (primary) date - it's own primary date if specified,
-- otherwise the parent transaction's primary date, or the null date if
-- there is no parent transaction.
postingDate :: Posting -> Day
postingDate p = fromMaybe nulldate $ asum dates
    where dates = [ pdate p, tdate <$> ptransaction p ]

-- | Get a posting's secondary (secondary) date, which is the first of:
-- posting's secondary date, transaction's secondary date, posting's
-- primary date, transaction's primary date, or the null date if there is
-- no parent transaction.
postingDate2 :: Posting -> Day
postingDate2 p = fromMaybe nulldate $ asum dates
  where dates = [ pdate2 p
                , tdate2 =<< ptransaction p
                , pdate p
                , tdate <$> ptransaction p
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
isPostingInDateSpan = isPostingInDateSpan' PrimaryDate

-- --date2-sensitive version, separate for now to avoid disturbing multiBalanceReport.
isPostingInDateSpan' :: WhichDate -> DateSpan -> Posting -> Bool
isPostingInDateSpan' PrimaryDate   s = spanContainsDate s . postingDate
isPostingInDateSpan' SecondaryDate s = spanContainsDate s . postingDate2

isEmptyPosting :: Posting -> Bool
isEmptyPosting = mixedAmountLooksZero . pamount

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
accountNameWithPostingType BalancedVirtualPosting = wrap "[" "]" . accountNameWithoutPostingType
accountNameWithPostingType VirtualPosting         = wrap "(" ")" . accountNameWithoutPostingType
accountNameWithPostingType RegularPosting         = accountNameWithoutPostingType

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

-- | Apply some account aliases to the posting's account name, as described by accountNameApplyAliases.
-- This can fail due to a bad replacement pattern in a regular expression alias.
postingApplyAliases :: [AccountAlias] -> Posting -> Either RegexError Posting
postingApplyAliases aliases p@Posting{paccount} =
  case accountNameApplyAliases aliases paccount of
    Right a -> Right p{paccount=a}
    Left e  -> Left err
      where
        err = "problem while applying account aliases:\n" ++ pshow aliases 
          ++ "\n to account name: "++T.unpack paccount++"\n "++e

-- | Rewrite an account name using all matching aliases from the given list, in sequence.
-- Each alias sees the result of applying the previous aliases.
-- Or, return any error arising from a bad regular expression in the aliases.
accountNameApplyAliases :: [AccountAlias] -> AccountName -> Either RegexError AccountName
accountNameApplyAliases aliases a =
  let (aname,atype) = (accountNameWithoutPostingType a, accountNamePostingType a)
  in foldM
     (\acct alias -> dbg6 "result" $ aliasReplace (dbg6 "alias" alias) (dbg6 "account" acct))
     aname
     aliases
     >>= Right . accountNameWithPostingType atype

-- | Memoising version of accountNameApplyAliases, maybe overkill.
accountNameApplyAliasesMemo :: [AccountAlias] -> AccountName -> Either RegexError AccountName
accountNameApplyAliasesMemo aliases = memo (accountNameApplyAliases aliases)
  -- XXX re-test this memoisation

-- aliasMatches :: AccountAlias -> AccountName -> Bool
-- aliasMatches (BasicAlias old _) a = old `isAccountNamePrefixOf` a
-- aliasMatches (RegexAlias re  _) a = regexMatchesCI re a

aliasReplace :: AccountAlias -> AccountName -> Either RegexError AccountName
aliasReplace (BasicAlias old new) a
  | old `isAccountNamePrefixOf` a || old == a =
      Right $ new <> T.drop (T.length old) a
  | otherwise = Right a
aliasReplace (RegexAlias re repl) a =
  fmap T.pack . regexReplace re repl $ T.unpack a -- XXX

-- | Apply a specified valuation to this posting's amount, using the
-- provided price oracle, commodity styles, and reference dates.
-- See amountApplyValuation.
postingApplyValuation :: PriceOracle -> M.Map CommoditySymbol AmountStyle -> Day -> Day -> ValuationType -> Posting -> Posting
postingApplyValuation priceoracle styles periodlast today v p =
    postingTransformAmount (mixedAmountApplyValuation priceoracle styles periodlast today (postingDate p) v) p

-- | Convert this posting's amount to cost, and apply the appropriate amount styles.
postingToCost :: M.Map CommoditySymbol AmountStyle -> Posting -> Posting
postingToCost styles = postingTransformAmount (styleMixedAmount styles . mixedAmountCost)

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
    c'  = T.stripEnd c
    tag = t <> ": " <> v

-- | Add a tag on its own line to a comment, preserving any prior content.
-- A space is inserted following the colon, before the value.
commentAddTagNextLine :: Text -> Tag -> Text
commentAddTagNextLine cmt (t,v) =
  cmt <> (if "\n" `T.isSuffixOf` cmt then "" else "\n") <> t <> ": " <> v 


-- tests

tests_Posting = tests "Posting" [

  test "accountNamePostingType" $ do
    accountNamePostingType "a" @?= RegularPosting
    accountNamePostingType "(a)" @?= VirtualPosting
    accountNamePostingType "[a]" @?= BalancedVirtualPosting

 ,test "accountNameWithoutPostingType" $ do
    accountNameWithoutPostingType "(a)" @?= "a"

 ,test "accountNameWithPostingType" $ do
    accountNameWithPostingType VirtualPosting "[a]" @?= "(a)"

 ,test "joinAccountNames" $ do
    "a" `joinAccountNames` "b:c" @?= "a:b:c"
    "a" `joinAccountNames` "(b:c)" @?= "(a:b:c)"
    "[a]" `joinAccountNames` "(b:c)" @?= "[a:b:c]"
    "" `joinAccountNames` "a" @?= "a"

 ,test "concatAccountNames" $ do
    concatAccountNames [] @?= ""
    concatAccountNames ["a","(b)","[c:d]"] @?= "(a:b:c:d)"

 ,test "commentAddTag" $ do
    commentAddTag "" ("a","") @?= "a: "
    commentAddTag "[1/2]" ("a","") @?= "[1/2], a: "

 ,test "commentAddTagNextLine" $ do
    commentAddTagNextLine "" ("a","") @?= "\na: "
    commentAddTagNextLine "[1/2]" ("a","") @?= "[1/2]\na: "

 ]

