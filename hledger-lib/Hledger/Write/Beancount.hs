{-|
Helpers for beancount output.
-}

{-# LANGUAGE OverloadedStrings    #-}

module Hledger.Write.Beancount (
  showTransactionBeancount,
  -- postingsAsLinesBeancount,
  -- postingAsLinesBeancount,
  -- showAccountNameBeancount,
  accountNameToBeancount,
  -- beancountTopLevelAccounts,

  -- * Tests
  tests_WriteBeancount
)
where

-- import Prelude hiding (Applicative(..))
import Data.Char
import Data.Default (def)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TB
import Safe (maximumBound)
import Text.DocLayout (realLength)
import Text.Printf
import Text.Tabular.AsciiWide hiding (render)

import Hledger.Utils
import Hledger.Data.Types
import Hledger.Data.AccountName
import Hledger.Data.Amount
import Hledger.Data.Currency (currencySymbolToCode)
import Hledger.Data.Dates (showDate)
import Hledger.Data.Posting (renderCommentLines, showBalanceAssertion, postingIndent)
import Hledger.Data.Transaction (payeeAndNoteFromDescription')
import Data.Function ((&))

--- ** doctest setup
-- $setup
-- >>> :set -XOverloadedStrings

-- | Like showTransaction, but generates Beancount journal format.
showTransactionBeancount :: Transaction -> Text
showTransactionBeancount t =
  -- https://beancount.github.io/docs/beancount_language_syntax.html
  -- similar to showTransactionHelper, but I haven't bothered with Builder
     firstline <> nl
  <> foldMap ((<> nl)) newlinecomments
  <> foldMap ((<> nl)) (postingsAsLinesBeancount $ tpostings t)
  <> nl
  where
    nl = "\n"
    firstline = T.concat [date, status, payee, note, tags, samelinecomment]
    date = showDate $ tdate t
    status = if tstatus t == Pending then " !" else " *"
    (payee,note) =
      case payeeAndNoteFromDescription' $ tdescription t of
        ("","") -> ("",      ""      )
        ("",n ) -> (""     , wrapq n )
        (p ,"") -> (wrapq p, wrapq "")
        (p ,n ) -> (wrapq p, wrapq n )
      where
        wrapq = wrap " \"" "\"" . escapeDoubleQuotes . escapeBackslash
    tags = T.concat $ map ((" #"<>).fst) $ ttags t
    (samelinecomment, newlinecomments) =
      case renderCommentLines (tcomment t) of []   -> ("",[])
                                              c:cs -> (c,cs)

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
          displayopts = defaultFmt{ displayZeroCommodity=True, displayForceDecimalMark=True, displayQuotes=False }
          a' = mapMixedAmount amountToBeancount $ pamount p
    thisamtwidth = maximumBound 0 $ map wbWidth shownAmounts

    -- when there is a balance assertion, show it only on the last posting line
    shownAmountsAssertions = zip shownAmounts shownAssertions
      where
        shownAssertions = replicate (length shownAmounts - 1) mempty ++ [assertion]
          where
            assertion = maybe mempty ((WideBuilder (TB.singleton ' ') 1 <>).showBalanceAssertion) $ pbalanceassertion p

    -- pad to the maximum account name width, plus 2 to leave room for status flags, to keep amounts aligned
    statusandaccount = postingIndent . fitText (Just $ 2 + acctwidth) Nothing False True $ pstatusandacct p
    thisacctwidth = realLength pacct

    (samelinecomment, newlinecomments) =
      case renderCommentLines (pcomment p) of []   -> ("",[])
                                              c:cs -> (c,cs)

-- | Like showAccountName for Beancount journal format.
-- Calls accountNameToBeancount first.
showAccountNameBeancount :: Maybe Int -> AccountName -> Text
showAccountNameBeancount w = maybe id T.take w . accountNameToBeancount

type BeancountAccountName = AccountName
type BeancountAccountNameComponent = AccountName

-- | Convert a hledger account name to a valid Beancount account name.
-- It replaces spaces with dashes and other non-supported characters with C<HEXBYTES>;
-- prepends the letter A- to any part which doesn't begin with a letter or number;
-- and capitalises each part.
-- It also checks that the first part is one of the required english
-- account names Assets, Liabilities, Equity, Income, or Expenses, and if not
-- raises an informative error.
-- Ref: https://beancount.github.io/docs/beancount_language_syntax.html#accounts
accountNameToBeancount :: AccountName -> BeancountAccountName
accountNameToBeancount a =
  dbg9 "beancount account name" $
  accountNameFromComponents bs'
  where
    bs =
      map accountNameComponentToBeancount $ accountNameComponents $
      dbg9 "hledger account name  " $
      a
    bs' =
      case bs of
        b:_ | b `notElem` beancountTopLevelAccounts -> error' e
          where
            e = T.unpack $ T.unlines [
              "bad top-level account: " <> b
              ,"in beancount account name:           " <> accountNameFromComponents bs
              ,"converted from hledger account name: " <> a
              ,"For Beancount, top-level accounts must be (or be --alias'ed to)"
              ,"one of " <> T.intercalate ", " beancountTopLevelAccounts <> "."
              -- ,"and not: " <> b
              ]
        cs -> cs

accountNameComponentToBeancount :: AccountName -> BeancountAccountNameComponent
accountNameComponentToBeancount acctpart =
  prependStartCharIfNeeded $
  case T.uncons acctpart of
    Nothing -> ""
    Just (c,cs) ->
      textCapitalise $
      T.concatMap (\d -> if isBeancountAccountChar d then (T.singleton d) else T.pack $ charToBeancount d) $ T.cons c cs
  where
    prependStartCharIfNeeded t =
      case T.uncons t of
        Just (c,_) | not $ isBeancountAccountStartChar c -> T.cons beancountAccountDummyStartChar t
        _ -> t

-- | Dummy valid starting character to prepend to Beancount account name parts if needed (A).
beancountAccountDummyStartChar :: Char
beancountAccountDummyStartChar = 'A'

charToBeancount :: Char -> String
charToBeancount c = if isSpace c then "-" else printf "C%x" c

-- XXX these probably allow too much unicode:

-- https://hackage.haskell.org/package/base-4.20.0.1/docs/Data-Char.html#v:isUpperCase would be more correct,
-- but isn't available till base 4.18/ghc 9.6. isUpper is close enough in practice.
isuppercase = isUpper

-- | Is this a valid character to start a Beancount account name part (capital letter or digit) ?
isBeancountAccountStartChar :: Char -> Bool
isBeancountAccountStartChar c = (isLetter c && isuppercase c) || isDigit c

-- | Is this a valid character to appear elsewhere in a Beancount account name part (letter, digit, or -) ?
isBeancountAccountChar :: Char -> Bool
isBeancountAccountChar c = isLetter c || isDigit c || c=='-'

beancountTopLevelAccounts = ["Assets", "Liabilities", "Equity", "Income", "Expenses"]

type BeancountAmount = Amount

-- | Do some best effort adjustments to make an amount that renders
-- in a way that Beancount can read: force the commodity symbol to the right,
-- capitalise all letters, convert a few currency symbols to codes.
amountToBeancount :: Amount -> BeancountAmount
amountToBeancount a@Amount{acommodity=c,astyle=s,acost=mp} = a{acommodity=c', astyle=s', acost=mp'}
  where
    c' = commodityToBeancount c
    s' = s{ascommodityside=R, ascommodityspaced=True}
    mp' = costToBeancount <$> mp
      where
        costToBeancount (TotalCost amt) = TotalCost $ amountToBeancount amt
        costToBeancount (UnitCost  amt) = UnitCost  $ amountToBeancount amt

type BeancountCommoditySymbol = CommoditySymbol

-- | Convert a hledger commodity name to a valid Beancount commodity name.
-- That is: 2-24 uppercase letters / digits / apostrophe / period / underscore / dash,
-- starting with a letter, and ending with a letter or digit.
-- Ref: https://beancount.github.io/docs/beancount_language_syntax.html#commodities-currencies
-- So this:
-- replaces common currency symbols with their ISO 4217 currency codes,
-- capitalises all letters,
-- replaces spaces with dashes and other invalid characters with C<HEXBYTES>,
-- prepends a C if the first character is not a letter,
-- appends a C if the last character is not a letter or digit,
-- and disables hledger's enclosing double quotes.
--
-- >>> commodityToBeancount ""
-- "C"
-- >>> commodityToBeancount "$"
-- "USD"
-- >>> commodityToBeancount "Usd"
-- "USD"
-- >>> commodityToBeancount "\"a1\""
-- "A1"
-- >>> commodityToBeancount "\"A 1!\""
-- "A-1C21"
--
commodityToBeancount :: CommoditySymbol -> BeancountCommoditySymbol
commodityToBeancount com =
  dbg9 "beancount commodity name" $
  let com' = stripquotes com
  in case currencySymbolToCode com' of
    Just code -> code
    Nothing ->
      com'
      & T.toUpper
      & T.concatMap (\d -> if isBeancountCommodityChar d then T.singleton d else T.pack $ charToBeancount d)
      & fixstart
      & fixend
  where
    fixstart bcom = case T.uncons bcom of
      Just (c,_) | isBeancountCommodityStartChar c -> bcom
      _ -> "C" <> bcom
    fixend bcom = case T.unsnoc bcom of
      Just (_,c) | isBeancountCommodityEndChar c -> bcom
      _ -> bcom <> "C"

-- | Is this a valid character in the middle of a Beancount commodity name (a capital letter, digit, or '._-) ?
isBeancountCommodityChar :: Char -> Bool
isBeancountCommodityChar c = (isLetter c && isuppercase c) || isDigit c || c `elem` ['\'', '.', '_', '-']

-- | Is this a valid character to start a Beancount commodity name (a capital letter) ?
isBeancountCommodityStartChar :: Char -> Bool
isBeancountCommodityStartChar c = isLetter c && isuppercase c

-- | Is this a valid character to end a Beancount commodity name (a capital letter or digit) ?
isBeancountCommodityEndChar :: Char -> Bool
isBeancountCommodityEndChar c = (isLetter c && isuppercase c) || isDigit c

--- ** tests

tests_WriteBeancount :: TestTree
tests_WriteBeancount = testGroup "Write.Beancount" [
  ]
