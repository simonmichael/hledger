{-|

A 'Transaction' represents a movement of some commodity(ies) between two
or more accounts. It consists of multiple account 'Posting's which balance
to zero, a date, and optional extras like description, cleared status, and
tags.

-}

{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Hledger.Data.Transaction
( -- * Transaction
  nulltransaction
, transaction
, txnTieKnot
, txnUntieKnot
  -- * operations
, showAccountName
, hasRealPostings
, realPostings
, assignmentPostings
, virtualPostings
, balancedVirtualPostings
, transactionsPostings
, transactionTransformPostings
, transactionApplyValuation
, transactionToCost
, transactionApplyAliases
, transactionMapPostings
, transactionMapPostingAmounts
  -- nonzerobalanceerror
  -- * date operations
, transactionDate2
  -- * transaction description parts
, transactionPayee
, transactionNote
  -- payeeAndNoteFromDescription
  -- * rendering
, showTransaction
, showTransactionOneLineAmounts
  -- showPostingLine
, showPostingLines
  -- * GenericSourcePos
, sourceFilePath
, sourceFirstLine
, showGenericSourcePos
, transactionFile
  -- * tests
, tests_Transaction
) where

import Data.Default (Default(..))
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TB
import Data.Time.Calendar (Day, fromGregorian)
import qualified Data.Map as M
import Safe (maximumDef)

import Hledger.Utils
import Hledger.Data.Types
import Hledger.Data.Dates
import Hledger.Data.Posting
import Hledger.Data.Amount
import Hledger.Data.Valuation
import Text.Tabular.AsciiWide

sourceFilePath :: GenericSourcePos -> FilePath
sourceFilePath = \case
    GenericSourcePos fp _ _ -> fp
    JournalSourcePos fp _ -> fp

sourceFirstLine :: GenericSourcePos -> Int
sourceFirstLine = \case
    GenericSourcePos _ line _ -> line
    JournalSourcePos _ (line, _) -> line

-- | Render source position in human-readable form.
-- Keep in sync with Hledger.UI.ErrorScreen.hledgerparseerrorpositionp (temporary). XXX
showGenericSourcePos :: GenericSourcePos -> String
showGenericSourcePos = \case
    GenericSourcePos fp line column -> show fp ++ " (line " ++ show line ++ ", column " ++ show column ++ ")"
    JournalSourcePos fp (line, line') -> show fp ++ " (lines " ++ show line ++ "-" ++ show line' ++ ")"

nulltransaction :: Transaction
nulltransaction = Transaction {
                    tindex=0,
                    tsourcepos=nullsourcepos,
                    tdate=nulldate,
                    tdate2=Nothing,
                    tstatus=Unmarked,
                    tcode="",
                    tdescription="",
                    tcomment="",
                    ttags=[],
                    tpostings=[],
                    tprecedingcomment=""
                  }

-- | Make a simple transaction with the given date and postings.
transaction :: Day -> [Posting] -> Transaction
transaction day ps = txnTieKnot $ nulltransaction{tdate=day, tpostings=ps}

transactionPayee :: Transaction -> Text
transactionPayee = fst . payeeAndNoteFromDescription . tdescription

transactionNote :: Transaction -> Text
transactionNote = snd . payeeAndNoteFromDescription . tdescription

-- | Parse a transaction's description into payee and note (aka narration) fields,
-- assuming a convention of separating these with | (like Beancount).
-- Ie, everything up to the first | is the payee, everything after it is the note.
-- When there's no |, payee == note == description.
payeeAndNoteFromDescription :: Text -> (Text,Text)
payeeAndNoteFromDescription t
  | T.null n = (t, t)
  | otherwise = (T.strip p, T.strip $ T.drop 1 n)
  where
    (p, n) = T.span (/= '|') t

{-|
Render a journal transaction as text similar to the style of Ledger's print command.

Adapted from Ledger 2.x and 3.x standard format:

@
yyyy-mm-dd[ *][ CODE] description.........          [  ; comment...............]
    account name 1.....................  ...$amount1[  ; comment...............]
    account name 2.....................  ..$-amount1[  ; comment...............]

pcodewidth    = no limit -- 10          -- mimicking ledger layout.
pdescwidth    = no limit -- 20          -- I don't remember what these mean,
pacctwidth    = 35 minimum, no maximum  -- they were important at the time.
pamtwidth     = 11
pcommentwidth = no limit -- 22
@

The output will be parseable journal syntax.
To facilitate this, postings with explicit multi-commodity amounts
are displayed as multiple similar postings, one per commodity.
(Normally does not happen with this function).
-}
showTransaction :: Transaction -> Text
showTransaction = TL.toStrict . TB.toLazyText . showTransactionHelper False

-- | Like showTransaction, but explicit multi-commodity amounts
-- are shown on one line, comma-separated. In this case the output will
-- not be parseable journal syntax.
showTransactionOneLineAmounts :: Transaction -> Text
showTransactionOneLineAmounts = TL.toStrict . TB.toLazyText . showTransactionHelper True

-- | Helper for showTransaction*.
showTransactionHelper :: Bool -> Transaction -> TB.Builder
showTransactionHelper onelineamounts t =
      TB.fromText descriptionline <> newline
    <> foldMap ((<> newline) . TB.fromText) newlinecomments
    <> foldMap ((<> newline) . TB.fromText) (postingsAsLines onelineamounts $ tpostings t)
    <> newline
  where
    descriptionline = T.stripEnd $ T.concat [date, status, code, desc, samelinecomment]
    date = showDate (tdate t) <> maybe "" (("="<>) . showDate) (tdate2 t)
    status | tstatus t == Cleared = " *"
           | tstatus t == Pending = " !"
           | otherwise            = ""
    code = if T.null (tcode t) then "" else wrap " (" ")" $ tcode t
    desc = if T.null d then "" else " " <> d where d = tdescription t
    (samelinecomment, newlinecomments) =
      case renderCommentLines (tcomment t) of []   -> ("",[])
                                              c:cs -> (c,cs)
    newline = TB.singleton '\n'

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
    maxacctwidth = maximumDef 0 $ map second3 linesWithWidths
    maxamtwidth  = maximumDef 0 $ map third3 linesWithWidths

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
                    | amt <- shownAmounts]
    render = renderRow def{tableBorders=False, borderSpaces=False} . Group NoLine . map Header
    pad amt = WideBuilder (TB.fromText $ T.replicate w " ") w <> amt
      where w = max 12 amtwidth - wbWidth amt  -- min. 12 for backwards compatibility

    assertion = maybe mempty ((WideBuilder (TB.singleton ' ') 1 <>).showBalanceAssertion) $ pbalanceassertion p
    -- pad to the maximum account name width, plus 2 to leave room for status flags, to keep amounts aligned
    statusandaccount = lineIndent . fitText (Just $ 2 + acctwidth) Nothing False True $ pstatusandacct p
    thisacctwidth = textWidth $ pacctstr p

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
      | otherwise   = showMixedAmountLinesB noColour{displayOneLine=onelineamounts} $ pamount p
    thisamtwidth = maximumDef 0 $ map wbWidth shownAmounts

    (samelinecomment, newlinecomments) =
      case renderCommentLines (pcomment p) of []   -> ("",[])
                                              c:cs -> (c,cs)

-- | Render a balance assertion, as the =[=][*] symbol and expected amount.
showBalanceAssertion :: BalanceAssertion -> WideBuilder
showBalanceAssertion BalanceAssertion{..} =
    singleton '=' <> eq <> ast <> singleton ' ' <> showAmountB def{displayZeroCommodity=True} baamount
  where
    eq  = if batotal     then singleton '=' else mempty
    ast = if bainclusive then singleton '*' else mempty
    singleton c = WideBuilder (TB.singleton c) 1

-- | Render a posting, simply. Used in balance assertion errors.
-- showPostingLine p =
--   lineIndent $
--   if pstatus p == Cleared then "* " else "" ++  -- XXX show !
--   showAccountName Nothing (ptype p) (paccount p) ++
--   "    " ++
--   showMixedAmountOneLine (pamount p) ++
--   assertion
--   where
--     -- XXX extract, handle ==
--     assertion = maybe "" ((" = " ++) . showAmountWithZeroCommodity . baamount) $ pbalanceassertion p

-- | Render a posting, at the appropriate width for aligning with
-- its siblings if any. Used by the rewrite command.
showPostingLines :: Posting -> [Text]
showPostingLines p = first3 $ postingAsLines False False maxacctwidth maxamtwidth p
  where
    linesWithWidths = map (postingAsLines False False maxacctwidth maxamtwidth) . maybe [p] tpostings $ ptransaction p
    maxacctwidth = maximumDef 0 $ map second3 linesWithWidths
    maxamtwidth  = maximumDef 0 $ map third3 linesWithWidths

-- | Prepend a suitable indent for a posting (or transaction/posting comment) line.
lineIndent :: Text -> Text
lineIndent = ("    "<>)

-- | Prepend the space required before a same-line comment.
commentSpace :: Text -> Text
commentSpace = ("  "<>)

-- | Show an account name, clipped to the given width if any, and
-- appropriately bracketed/parenthesised for the given posting type.
showAccountName :: Maybe Int -> PostingType -> AccountName -> Text
showAccountName w = fmt
  where
    fmt RegularPosting         = maybe id T.take w
    fmt VirtualPosting         = wrap "(" ")" . maybe id (T.takeEnd . subtract 2) w
    fmt BalancedVirtualPosting = wrap "[" "]" . maybe id (T.takeEnd . subtract 2) w

hasRealPostings :: Transaction -> Bool
hasRealPostings = not . null . realPostings

realPostings :: Transaction -> [Posting]
realPostings = filter isReal . tpostings

assignmentPostings :: Transaction -> [Posting]
assignmentPostings = filter hasBalanceAssignment . tpostings

virtualPostings :: Transaction -> [Posting]
virtualPostings = filter isVirtual . tpostings

balancedVirtualPostings :: Transaction -> [Posting]
balancedVirtualPostings = filter isBalancedVirtual . tpostings

transactionsPostings :: [Transaction] -> [Posting]
transactionsPostings = concatMap tpostings

-- Get a transaction's secondary date, defaulting to the primary date.
transactionDate2 :: Transaction -> Day
transactionDate2 t = fromMaybe (tdate t) $ tdate2 t

-- | Ensure a transaction's postings refer back to it, so that eg
-- relatedPostings works right.
txnTieKnot :: Transaction -> Transaction
txnTieKnot t@Transaction{tpostings=ps} = t' where
    t' = t{tpostings=map (postingSetTransaction t') ps}

-- | Ensure a transaction's postings do not refer back to it, so that eg
-- recursiveSize and GHCI's :sprint work right.
txnUntieKnot :: Transaction -> Transaction
txnUntieKnot t@Transaction{tpostings=ps} = t{tpostings=map (\p -> p{ptransaction=Nothing}) ps}

-- | Set a posting's parent transaction.
postingSetTransaction :: Transaction -> Posting -> Posting
postingSetTransaction t p = p{ptransaction=Just t}

-- | Apply a transform function to this transaction's amounts.
transactionTransformPostings :: (Posting -> Posting) -> Transaction -> Transaction
transactionTransformPostings f t@Transaction{tpostings=ps} = t{tpostings=map f ps}

-- | Apply a specified valuation to this transaction's amounts, using
-- the provided price oracle, commodity styles, and reference dates.
-- See amountApplyValuation.
transactionApplyValuation :: PriceOracle -> M.Map CommoditySymbol AmountStyle -> Day -> Day -> ValuationType -> Transaction -> Transaction
transactionApplyValuation priceoracle styles periodlast today v =
  transactionTransformPostings (postingApplyValuation priceoracle styles periodlast today v)

-- | Convert this transaction's amounts to cost, and apply the appropriate amount styles.
transactionToCost :: M.Map CommoditySymbol AmountStyle -> Transaction -> Transaction
transactionToCost styles = transactionTransformPostings (postingToCost styles)

-- | Apply some account aliases to all posting account names in the transaction, as described by accountNameApplyAliases.
-- This can fail due to a bad replacement pattern in a regular expression alias.
transactionApplyAliases :: [AccountAlias] -> Transaction -> Either RegexError Transaction
transactionApplyAliases aliases t =
  case mapM (postingApplyAliases aliases) $ tpostings t of
    Right ps -> Right $ txnTieKnot $ t{tpostings=ps}
    Left err -> Left err

-- | Apply a transformation to a transaction's postings.
transactionMapPostings :: (Posting -> Posting) -> Transaction -> Transaction
transactionMapPostings f t@Transaction{tpostings=ps} = t{tpostings=map f ps}

-- | Apply a transformation to a transaction's posting amounts.
transactionMapPostingAmounts :: (MixedAmount -> MixedAmount) -> Transaction -> Transaction
transactionMapPostingAmounts f  = transactionMapPostings (postingTransformAmount f)

-- | The file path from which this transaction was parsed.
transactionFile :: Transaction -> FilePath
transactionFile Transaction{tsourcepos} =
  case tsourcepos of
    GenericSourcePos f _ _ -> f
    JournalSourcePos f _   -> f

-- tests

tests_Transaction :: TestTree
tests_Transaction =
  testGroup "Transaction" [

      testGroup "showPostingLines" [
          testCase "null posting" $ showPostingLines nullposting @?= ["                   0"]
        , testCase "non-null posting" $
           let p =
                posting
                  { pstatus = Cleared
                  , paccount = "a"
                  , pamount = mixed [usd 1, hrs 2]
                  , pcomment = "pcomment1\npcomment2\n  tag3: val3  \n"
                  , ptype = RegularPosting
                  , ptags = [("ptag1", "val1"), ("ptag2", "val2")]
                  }
           in showPostingLines p @?=
              [ "    * a         $1.00  ; pcomment1"
              , "    ; pcomment2"
              , "    ;   tag3: val3  "
              , "    * a         2.00h  ; pcomment1"
              , "    ; pcomment2"
              , "    ;   tag3: val3  "
              ]
        ]

    , let
        -- one implicit amount
        timp = nulltransaction {tpostings = ["a" `post` usd 1, "b" `post` missingamt]}
        -- explicit amounts, balanced
        texp = nulltransaction {tpostings = ["a" `post` usd 1, "b" `post` usd (-1)]}
        -- explicit amount, only one posting
        texp1 = nulltransaction {tpostings = ["(a)" `post` usd 1]}
        -- explicit amounts, two commodities, explicit balancing price
        texp2 = nulltransaction {tpostings = ["a" `post` usd 1, "b" `post` (hrs (-1) `at` usd 1)]}
        -- explicit amounts, two commodities, implicit balancing price
        texp2b = nulltransaction {tpostings = ["a" `post` usd 1, "b" `post` hrs (-1)]}
        -- one missing amount, not the last one
        t3 = nulltransaction {tpostings = ["a" `post` usd 1, "b" `post` missingamt, "c" `post` usd (-1)]}
        -- unbalanced amounts when precision is limited (#931)
        -- t4 = nulltransaction {tpostings = ["a" `post` usd (-0.01), "b" `post` usd (0.005), "c" `post` usd (0.005)]}
      in testGroup "postingsAsLines" [
              testCase "null-transaction" $ postingsAsLines False (tpostings nulltransaction) @?= []
            , testCase "implicit-amount" $ postingsAsLines False (tpostings timp) @?=
                  [ "    a           $1.00"
                  , "    b" -- implicit amount remains implicit
                  ]
            , testCase "explicit-amounts" $ postingsAsLines False (tpostings texp) @?=
                  [ "    a           $1.00"
                  , "    b          $-1.00"
                  ]
            , testCase "one-explicit-amount" $ postingsAsLines False (tpostings texp1) @?=
                  [ "    (a)           $1.00"
                  ]
            , testCase "explicit-amounts-two-commodities" $ postingsAsLines False (tpostings texp2) @?=
                  [ "    a             $1.00"
                  , "    b    -1.00h @ $1.00"
                  ]
            , testCase "explicit-amounts-not-explicitly-balanced" $ postingsAsLines False (tpostings texp2b) @?=
                  [ "    a           $1.00"
                  , "    b          -1.00h"
                  ]
            , testCase "implicit-amount-not-last" $ postingsAsLines False (tpostings t3) @?=
                  ["    a           $1.00", "    b", "    c          $-1.00"]
            -- , testCase "ensure-visibly-balanced" $
            --    in postingsAsLines False (tpostings t4) @?=
            --       ["    a          $-0.01", "    b           $0.005", "    c           $0.005"]

            ]

    , testGroup "showTransaction" [
          testCase "null transaction" $ showTransaction nulltransaction @?= "0000-01-01\n\n"
        , testCase "non-null transaction" $ showTransaction
            nulltransaction
              { tdate = fromGregorian 2012 05 14
              , tdate2 = Just $ fromGregorian 2012 05 15
              , tstatus = Unmarked
              , tcode = "code"
              , tdescription = "desc"
              , tcomment = "tcomment1\ntcomment2\n"
              , ttags = [("ttag1", "val1")]
              , tpostings =
                  [ nullposting
                      { pstatus = Cleared
                      , paccount = "a"
                      , pamount = mixed [usd 1, hrs 2]
                      , pcomment = "\npcomment2\n"
                      , ptype = RegularPosting
                      , ptags = [("ptag1", "val1"), ("ptag2", "val2")]
                      }
                  ]
              } @?=
          T.unlines
            [ "2012-05-14=2012-05-15 (code) desc  ; tcomment1"
            , "    ; tcomment2"
            , "    * a         $1.00"
            , "    ; pcomment2"
            , "    * a         2.00h"
            , "    ; pcomment2"
            , ""
            ]
        , testCase "show a balanced transaction" $
          (let t =
                 Transaction
                   0
                   ""
                   nullsourcepos
                   (fromGregorian 2007 01 28)
                   Nothing
                   Unmarked
                   ""
                   "coopportunity"
                   ""
                   []
                   [ posting {paccount = "expenses:food:groceries", pamount = mixedAmount (usd 47.18), ptransaction = Just t}
                   , posting {paccount = "assets:checking", pamount = mixedAmount (usd (-47.18)), ptransaction = Just t}
                   ]
            in showTransaction t) @?=
          (T.unlines
             [ "2007-01-28 coopportunity"
             , "    expenses:food:groceries          $47.18"
             , "    assets:checking                 $-47.18"
             , ""
             ])
        , testCase "show an unbalanced transaction, should not elide" $
          (showTransaction
             (txnTieKnot $
              Transaction
                0
                ""
                nullsourcepos
                (fromGregorian 2007 01 28)
                Nothing
                Unmarked
                ""
                "coopportunity"
                ""
                []
                [ posting {paccount = "expenses:food:groceries", pamount = mixedAmount (usd 47.18)}
                , posting {paccount = "assets:checking", pamount = mixedAmount (usd (-47.19))}
                ])) @?=
          (T.unlines
             [ "2007-01-28 coopportunity"
             , "    expenses:food:groceries          $47.18"
             , "    assets:checking                 $-47.19"
             , ""
             ])
        , testCase "show a transaction with one posting and a missing amount" $
          (showTransaction
             (txnTieKnot $
              Transaction
                0
                ""
                nullsourcepos
                (fromGregorian 2007 01 28)
                Nothing
                Unmarked
                ""
                "coopportunity"
                ""
                []
                [posting {paccount = "expenses:food:groceries", pamount = missingmixedamt}])) @?=
          (T.unlines ["2007-01-28 coopportunity", "    expenses:food:groceries", ""])
        , testCase "show a transaction with a priced commodityless amount" $
          (showTransaction
             (txnTieKnot $
              Transaction
                0
                ""
                nullsourcepos
                (fromGregorian 2010 01 01)
                Nothing
                Unmarked
                ""
                "x"
                ""
                []
                [ posting {paccount = "a", pamount = mixedAmount $ num 1 `at` (usd 2 `withPrecision` Precision 0)}
                , posting {paccount = "b", pamount = missingmixedamt}
                ])) @?=
          (T.unlines ["2010-01-01 x", "    a          1 @ $2", "    b", ""])
        ]
    ]
