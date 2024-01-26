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
  postingStripCosts,
  postingApplyAliases,
  postingApplyCommodityStyles,
  postingStyleAmounts,
  postingAddTags,
  -- * date operations
  postingDate,
  postingDate2,
  postingDateOrDate2,
  isPostingInDateSpan,
  isPostingInDateSpan',
  -- * account name operations
  accountNamesFromPostings,
  -- * comment/tag operations
  commentJoin,
  commentAddTag,
  commentAddTagUnspaced,
  commentAddTagNextLine,
  -- * arithmetic
  sumPostings,
  -- * rendering
  showPosting,
  showPostingLines,
  postingAsLines,
  postingsAsLines,
  postingsAsLinesBeancount,
  postingAsLinesBeancount,
  showAccountName,
  showAccountNameBeancount,
  renderCommentLines,
  showBalanceAssertion,
  -- * misc.
  postingTransformAmount,
  postingApplyValuation,
  postingToCost,
  postingAddInferredEquityPostings,
  postingPriceDirectivesFromCost,
  tests_Posting
)
where

import Data.Default (def)
import Data.Foldable (asum)
import Data.Function ((&))
import qualified Data.Map as M
import Data.Maybe (fromMaybe, isJust, mapMaybe)
import Data.List (foldl', sort, union)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TB
import Data.Time.Calendar (Day)
import Safe (maximumBound)
import Text.DocLayout (realLength)

import Text.Tabular.AsciiWide hiding (render)

import Hledger.Utils
import Hledger.Data.Types
import Hledger.Data.Amount
import Hledger.Data.AccountName
import Hledger.Data.Dates (nulldate, spanContainsDate)
import Hledger.Data.Valuation


instance HasAmounts BalanceAssertion where
  styleAmounts styles ba@BalanceAssertion{baamount} = ba{baamount=styleAmounts styles baamount}

instance HasAmounts Posting where
  styleAmounts styles p@Posting{pamount, pbalanceassertion} =
    p{ pamount=styleAmounts styles pamount
      ,pbalanceassertion=styleAmounts styles pbalanceassertion 
      }

{-# DEPRECATED postingApplyCommodityStyles "please use styleAmounts instead" #-}
-- | Find and apply the appropriate display style to the posting amounts
-- in each commodity (see journalCommodityStyles).
-- Main amount precisions may be set or not according to the styles, but cost precisions are not set.
postingApplyCommodityStyles :: M.Map CommoditySymbol AmountStyle -> Posting -> Posting
postingApplyCommodityStyles = styleAmounts

{-# DEPRECATED postingStyleAmounts "please use styleAmounts instead" #-}
-- | Like postingApplyCommodityStyles, but neither
-- main amount precisions or cost precisions are set.
postingStyleAmounts :: M.Map CommoditySymbol AmountStyle -> Posting -> Posting
postingStyleAmounts = styleAmounts

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

nullsourcepos :: (SourcePos, SourcePos)
nullsourcepos = (SourcePos "" (mkPos 1) (mkPos 1), SourcePos "" (mkPos 2) (mkPos 1))

nullassertion :: BalanceAssertion
nullassertion = BalanceAssertion
                  {baamount=nullamt
                  ,batotal=False
                  ,bainclusive=False
                  ,baposition=initialPos ""
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

-- | Render a balance assertion, as the =[=][*] symbol and expected amount.
showBalanceAssertion :: BalanceAssertion -> WideBuilder
showBalanceAssertion ba =
    singleton '=' <> eq <> ast <> singleton ' ' <> showAmountB def{displayZeroCommodity=True} (baamount ba)
  where
    eq  = if batotal ba     then singleton '=' else mempty
    ast = if bainclusive ba then singleton '*' else mempty
    singleton c = WideBuilder (TB.singleton c) 1

-- Get the original posting, if any.
originalPosting :: Posting -> Posting
originalPosting p = fromMaybe p $ poriginal p

showPosting :: Posting -> String
showPosting p = T.unpack . T.unlines $ postingsAsLines False [p]

-- | Render a posting, at the appropriate width for aligning with
-- its siblings if any. Used by the rewrite command.
showPostingLines :: Posting -> [Text]
showPostingLines p = first3 $ postingAsLines False False maxacctwidth maxamtwidth p
  where
    linesWithWidths = map (postingAsLines False False maxacctwidth maxamtwidth) . maybe [p] tpostings $ ptransaction p
    maxacctwidth = maximumBound 0 $ map second3 linesWithWidths
    maxamtwidth  = maximumBound 0 $ map third3 linesWithWidths

-- | Given a transaction and its postings, render the postings, suitable
-- for `print` output. Normally this output will be valid journal syntax which
-- hledger can reparse (though it may include no-longer-valid balance assertions).
--
-- Explicit amounts are shown, any implicit amounts are not.
--
-- Postings with multicommodity explicit amounts are handled as follows:
-- if onelineamounts is true, these amounts are shown on one line,
-- comma-separated, and the output will not be valid journal syntax.
-- Otherwise, they are shown as several similar postings, one per commodity.
-- When the posting has a balance assertion, it is attached to the last of these postings.
--
-- The output will appear to be a balanced transaction.
-- Amounts' display precisions, which may have been limited by commodity
-- directives, will be increased if necessary to ensure this.
--
-- Posting amounts will be aligned with each other, starting about 4 columns
-- beyond the widest account name (see postingAsLines for details).
postingsAsLines :: Bool -> [Posting] -> [Text]
postingsAsLines onelineamounts ps = concatMap first3 linesWithWidths
  where
    linesWithWidths = map (postingAsLines False onelineamounts maxacctwidth maxamtwidth) ps
    maxacctwidth = maximumBound 0 $ map second3 linesWithWidths
    maxamtwidth  = maximumBound 0 $ map third3 linesWithWidths

-- | Render one posting, on one or more lines, suitable for `print` output.
-- There will be an indented account name, plus one or more of status flag,
-- posting amount, balance assertion, same-line comment, next-line comments.
--
-- If the posting's amount is implicit or if elideamount is true, no amount is shown.
--
-- If the posting's amount is explicit and multi-commodity, multiple similar
-- postings are shown, one for each commodity, to help produce parseable journal syntax.
-- Or if onelineamounts is true, such amounts are shown on one line, comma-separated
-- (and the output will not be valid journal syntax).
--
-- If an amount is zero, any commodity symbol attached to it is shown
-- (and the corresponding commodity display style is used).
--
-- By default, 4 spaces (2 if there's a status flag) are shown between
-- account name and start of amount area, which is typically 12 chars wide
-- and contains a right-aligned amount (so 10-12 visible spaces between
-- account name and amount is typical).
-- When given a list of postings to be aligned with, the whitespace will be
-- increased if needed to match the posting with the longest account name.
-- This is used to align the amounts of a transaction's postings.
--
-- Also returns the account width and amount width used.
postingAsLines :: Bool -> Bool -> Int -> Int -> Posting -> ([Text], Int, Int)
postingAsLines elideamount onelineamounts acctwidth amtwidth p =
    (concatMap (++ newlinecomments) postingblocks, thisacctwidth, thisamtwidth)
  where
    -- This needs to be converted to strict Text in order to strip trailing
    -- spaces. This adds a small amount of inefficiency, and the only difference
    -- is whether there are trailing spaces in print (and related) reports. This
    -- could be removed and we could just keep everything as a Text Builder, but
    -- would require adding trailing spaces to 42 failing tests.
    postingblocks = [map T.stripEnd . T.lines . TL.toStrict $
                       render [ textCell BottomLeft statusandaccount
                              , textCell BottomLeft "  "
                              , Cell BottomLeft [pad amt]
                              , Cell BottomLeft [assertion]
                              , textCell BottomLeft samelinecomment
                              ]
                    | (amt,assertion) <- shownAmountsAssertions]
    render = renderRow def{tableBorders=False, borderSpaces=False} . Group NoLine . map Header
    pad amt = WideBuilder (TB.fromText $ T.replicate w " ") w <> amt
      where w = max 12 amtwidth - wbWidth amt  -- min. 12 for backwards compatibility

    pacctstr p' = showAccountName Nothing (ptype p') (paccount p')
    pstatusandacct p' = pstatusprefix p' <> pacctstr p'
    pstatusprefix p' = case pstatus p' of
        Unmarked -> ""
        s        -> T.pack (show s) <> " "

    -- currently prices are considered part of the amount string when right-aligning amounts
    -- Since we will usually be calling this function with the knot tied between
    -- amtwidth and thisamtwidth, make sure thisamtwidth does not depend on
    -- amtwidth at all.
    shownAmounts
      | elideamount = [mempty]
      | otherwise   = showMixedAmountLinesB displayopts $ pamount p
        where displayopts = defaultFmt{
          displayZeroCommodity=True, displayForceDecimalMark=True, displayOneLine=onelineamounts
          }
    thisamtwidth = maximumBound 0 $ map wbWidth shownAmounts

    -- when there is a balance assertion, show it only on the last posting line
    shownAmountsAssertions = zip shownAmounts shownAssertions
      where
        shownAssertions = replicate (length shownAmounts - 1) mempty ++ [assertion]
          where
            assertion = maybe mempty ((WideBuilder (TB.singleton ' ') 1 <>).showBalanceAssertion) $ pbalanceassertion p

    -- pad to the maximum account name width, plus 2 to leave room for status flags, to keep amounts aligned
    statusandaccount = lineIndent . fitText (Just $ 2 + acctwidth) Nothing False True $ pstatusandacct p
    thisacctwidth = realLength $ pacctstr p

    (samelinecomment, newlinecomments) =
      case renderCommentLines (pcomment p) of []   -> ("",[])
                                              c:cs -> (c,cs)

-- | Show an account name, clipped to the given width if any, and
-- appropriately bracketed/parenthesised for the given posting type.
showAccountName :: Maybe Int -> PostingType -> AccountName -> Text
showAccountName w = fmt
  where
    fmt RegularPosting         = maybe id T.take w
    fmt VirtualPosting         = wrap "(" ")" . maybe id (T.takeEnd . subtract 2) w
    fmt BalancedVirtualPosting = wrap "[" "]" . maybe id (T.takeEnd . subtract 2) w

-- | Like postingsAsLines but generates Beancount journal format.
postingsAsLinesBeancount :: [Posting] -> [Text]
postingsAsLinesBeancount ps = concatMap first3 linesWithWidths
  where
    linesWithWidths = map (postingAsLinesBeancount False maxacctwidth maxamtwidth) ps
    maxacctwidth = maximumBound 0 $ map second3 linesWithWidths
    maxamtwidth  = maximumBound 0 $ map third3  linesWithWidths

-- | Like postingAsLines but generates Beancount journal format.
postingAsLinesBeancount  :: Bool -> Int -> Int -> Posting -> ([Text], Int, Int)
postingAsLinesBeancount elideamount acctwidth amtwidth p =
    (concatMap (++ newlinecomments) postingblocks, thisacctwidth, thisamtwidth)
  where
    -- This needs to be converted to strict Text in order to strip trailing
    -- spaces. This adds a small amount of inefficiency, and the only difference
    -- is whether there are trailing spaces in print (and related) reports. This
    -- could be removed and we could just keep everything as a Text Builder, but
    -- would require adding trailing spaces to 42 failing tests.
    postingblocks = [map T.stripEnd . T.lines . TL.toStrict $
                       render [ textCell BottomLeft statusandaccount
                              , textCell BottomLeft "  "
                              , Cell BottomLeft [pad amt]
                              , textCell BottomLeft samelinecomment
                              ]
                    | (amt,_assertion) <- shownAmountsAssertions]
    render = renderRow def{tableBorders=False, borderSpaces=False} . Group NoLine . map Header
    pad amt = WideBuilder (TB.fromText $ T.replicate w " ") w <> amt
      where w = max 12 amtwidth - wbWidth amt  -- min. 12 for backwards compatibility

    pacct = showAccountNameBeancount Nothing $ paccount p
    pstatusandacct p' = if pstatus p' == Pending then "! " else "" <> pacct

    -- currently prices are considered part of the amount string when right-aligning amounts
    -- Since we will usually be calling this function with the knot tied between
    -- amtwidth and thisamtwidth, make sure thisamtwidth does not depend on
    -- amtwidth at all.
    shownAmounts
      | elideamount = [mempty]
      | otherwise   = showMixedAmountLinesB displayopts a'
        where
          displayopts = defaultFmt{ displayZeroCommodity=True, displayForceDecimalMark=True }
          a' = mapMixedAmount amountToBeancount $ pamount p
    thisamtwidth = maximumBound 0 $ map wbWidth shownAmounts

    -- when there is a balance assertion, show it only on the last posting line
    shownAmountsAssertions = zip shownAmounts shownAssertions
      where
        shownAssertions = replicate (length shownAmounts - 1) mempty ++ [assertion]
          where
            assertion = maybe mempty ((WideBuilder (TB.singleton ' ') 1 <>).showBalanceAssertion) $ pbalanceassertion p

    -- pad to the maximum account name width, plus 2 to leave room for status flags, to keep amounts aligned
    statusandaccount = lineIndent . fitText (Just $ 2 + acctwidth) Nothing False True $ pstatusandacct p
    thisacctwidth = realLength pacct

    (samelinecomment, newlinecomments) =
      case renderCommentLines (pcomment p) of []   -> ("",[])
                                              c:cs -> (c,cs)

type BeancountAmount = Amount

-- | Do some best effort adjustments to make an amount that renders
-- in a way that Beancount can read: forces the commodity symbol to the right,
-- converts a few currency symbols to names, capitalises all letters.
amountToBeancount :: Amount -> BeancountAmount
amountToBeancount a@Amount{acommodity=c,astyle=s,acost=mp} = a{acommodity=c', astyle=s', acost=mp'}
  -- https://beancount.github.io/docs/beancount_language_syntax.html#commodities-currencies
  where
    c' = T.toUpper $
      T.replace "$" "USD" $
      T.replace "€" "EUR" $
      T.replace "¥" "JPY" $
      T.replace "£" "GBP" $
      c
    s' = s{ascommodityside=R, ascommodityspaced=True}
    mp' = costToBeancount <$> mp
      where
        costToBeancount (TotalCost amt) = TotalCost $ amountToBeancount amt
        costToBeancount (UnitCost  amt) = UnitCost  $ amountToBeancount amt

-- | Like showAccountName for Beancount journal format.
-- Calls accountNameToBeancount first.
showAccountNameBeancount :: Maybe Int -> AccountName -> Text
showAccountNameBeancount w = maybe id T.take w . accountNameToBeancount

-- | Render a transaction or posting's comment as indented, semicolon-prefixed comment lines.
-- The first line (unless empty) will have leading space, subsequent lines will have a larger indent.
renderCommentLines :: Text -> [Text]
renderCommentLines t =
  case T.lines t of
    []      -> []
    [l]     -> [commentSpace $ comment l]        -- single-line comment
    ("":ls) -> "" : map (lineIndent . comment) ls  -- multi-line comment with empty first line
    (l:ls)  -> commentSpace (comment l) : map (lineIndent . comment) ls
  where
    comment = ("; "<>)

-- | Prepend a suitable indent for a posting (or transaction/posting comment) line.
lineIndent :: Text -> Text
lineIndent = ("    "<>)

-- | Prepend the space required before a same-line comment.
commentSpace :: Text -> Text
commentSpace = ("  "<>)


isReal :: Posting -> Bool
isReal p = ptype p == RegularPosting

isVirtual :: Posting -> Bool
isVirtual p = ptype p == VirtualPosting

isBalancedVirtual :: Posting -> Bool
isBalancedVirtual p = ptype p == BalancedVirtualPosting

hasAmount :: Posting -> Bool
hasAmount = not . isMissingMixedAmount . pamount

hasBalanceAssignment :: Posting -> Bool
hasBalanceAssignment p = not (hasAmount p) && isJust (pbalanceassertion p)

-- | Sorted unique account names referenced by these postings.
accountNamesFromPostings :: [Posting] -> [AccountName]
accountNamesFromPostings = S.toList . S.fromList . map paccount

-- | Sum all amounts from a list of postings.
sumPostings :: [Posting] -> MixedAmount
sumPostings = foldl' (\amt p -> maPlus amt $ pamount p) nullmixedamt

-- | Strip all prices from a Posting.
postingStripCosts :: Posting -> Posting
postingStripCosts = postingTransformAmount mixedAmountStripCosts

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

-- | Get a posting's primary or secondary date, as specified.
postingDateOrDate2 :: WhichDate -> Posting -> Day
postingDateOrDate2 PrimaryDate   = postingDate
postingDateOrDate2 SecondaryDate = postingDate2

-- | Get a posting's status. This is cleared or pending if those are
-- explicitly set on the posting, otherwise the status of its parent
-- transaction, or unmarked if there is no parent transaction. (Note
-- the ambiguity, unmarked can mean "posting and transaction are both
-- unmarked" or "posting is unmarked and don't know about the transaction".
postingStatus :: Posting -> Status
postingStatus Posting{pstatus=s, ptransaction=mt} = case s of
    Unmarked -> maybe Unmarked tstatus mt
    _ -> s

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

-- | Add tags to a posting, discarding any for which the posting already has a value.
postingAddTags :: Posting -> [Tag] -> Posting
postingAddTags p@Posting{ptags} tags = p{ptags=ptags `union` tags}

-- | Apply a specified valuation to this posting's amount, using the
-- provided price oracle, commodity styles, and reference dates.
-- See amountApplyValuation.
postingApplyValuation :: PriceOracle -> M.Map CommoditySymbol AmountStyle -> Day -> Day -> ValuationType -> Posting -> Posting
postingApplyValuation priceoracle styles periodlast today v p =
    postingTransformAmount (mixedAmountApplyValuation priceoracle styles periodlast today (postingDate p) v) p

-- | Maybe convert this 'Posting's amount to cost.
postingToCost :: ConversionOp -> Posting -> Maybe Posting
postingToCost NoConversionOp p = Just p
postingToCost ToCost         p
  -- If this is a conversion posting with a matched transaction price posting, ignore it
  | "_conversion-matched" `elem` map fst (ptags p) && nocosts = Nothing
  | otherwise = Just $ postingTransformAmount mixedAmountCost p
  where
    nocosts = (not . any (isJust . acost) . amountsRaw) $ pamount p

-- | Generate inferred equity postings from a 'Posting''s costs.
-- Make sure not to duplicate them when matching ones exist already.
postingAddInferredEquityPostings :: Bool -> Text -> Posting -> [Posting]
postingAddInferredEquityPostings verbosetags equityAcct p
    | "_price-matched" `elem` map fst (ptags p) = [p]
    | otherwise = taggedPosting : concatMap conversionPostings costs
  where
    costs = filter (isJust . acost) . amountsRaw $ pamount p
    taggedPosting
      | null costs = p
      | otherwise  = p{ ptags = ("_price-matched","") : ptags p }
    conversionPostings amt = case acost amt of
        Nothing -> []
        Just _  -> [ cp{ paccount = accountPrefix <> amtCommodity
                       , pamount = mixedAmount . negate $ amountStripCost amt
                       }
                   , cp{ paccount = accountPrefix <> costCommodity
                       , pamount = mixedAmount cost
                       }
                   ]
      where
        cost = amountCost amt
        amtCommodity  = commodity amt
        costCommodity = commodity cost
        cp = p{ pcomment = pcomment p & (if verbosetags then (`commentAddTag` ("generated-posting","conversion")) else id)
              , ptags    =
                   ("_conversion-matched","") : -- implementation-specific internal tag, not for users
                   ("_generated-posting","conversion") :
                   (if verbosetags then [("generated-posting", "conversion")] else [])
              , pbalanceassertion = Nothing
              , poriginal = Nothing
              }
        accountPrefix = mconcat [ equityAcct, ":", T.intercalate "-" $ sort [amtCommodity, costCommodity], ":"]
        -- Take the commodity of an amount and collapse consecutive spaces to a single space
        commodity = T.unwords . filter (not . T.null) . T.words . acommodity

-- | Make a market price equivalent to this posting's amount's unit
-- price, if any.
postingPriceDirectivesFromCost :: Posting -> [PriceDirective]
postingPriceDirectivesFromCost p@Posting{pamount} =
    mapMaybe (amountPriceDirectiveFromCost $ postingDate p) $ amountsRaw pamount

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

-- | Like commentAddTag, but omits the space after the colon.
commentAddTagUnspaced :: Text -> Tag -> Text
commentAddTagUnspaced c (t,v)
  | T.null c' = tag
  | otherwise = c' `commentJoin` tag
  where
    c'  = T.stripEnd c
    tag = t <> ":" <> v

-- | Add a tag on its own line to a comment, preserving any prior content.
-- A space is inserted following the colon, before the value.
commentAddTagNextLine :: Text -> Tag -> Text
commentAddTagNextLine cmt (t,v) =
  cmt <> (if "\n" `T.isSuffixOf` cmt then "" else "\n") <> t <> ": " <> v


-- tests

tests_Posting = testGroup "Posting" [

  testCase "accountNamePostingType" $ do
    accountNamePostingType "a" @?= RegularPosting
    accountNamePostingType "(a)" @?= VirtualPosting
    accountNamePostingType "[a]" @?= BalancedVirtualPosting

 ,testCase "accountNameWithoutPostingType" $ do
    accountNameWithoutPostingType "(a)" @?= "a"

 ,testCase "accountNameWithPostingType" $ do
    accountNameWithPostingType VirtualPosting "[a]" @?= "(a)"

 ,testCase "joinAccountNames" $ do
    "a" `joinAccountNames` "b:c" @?= "a:b:c"
    "a" `joinAccountNames` "(b:c)" @?= "(a:b:c)"
    "[a]" `joinAccountNames` "(b:c)" @?= "[a:b:c]"
    "" `joinAccountNames` "a" @?= "a"

 ,testCase "concatAccountNames" $ do
    concatAccountNames [] @?= ""
    concatAccountNames ["a","(b)","[c:d]"] @?= "(a:b:c:d)"

 ,testCase "commentAddTag" $ do
    commentAddTag "" ("a","") @?= "a: "
    commentAddTag "[1/2]" ("a","") @?= "[1/2], a: "

 ,testCase "commentAddTagNextLine" $ do
    commentAddTagNextLine "" ("a","") @?= "\na: "
    commentAddTagNextLine "[1/2]" ("a","") @?= "[1/2]\na: "

 ]

