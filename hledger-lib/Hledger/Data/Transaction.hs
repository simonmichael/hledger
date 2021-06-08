{-|

A 'Transaction' represents a movement of some commodity(ies) between two
or more accounts. It consists of multiple account 'Posting's which balance
to zero, a date, and optional extras like description, cleared status, and
tags.

-}

{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types        #-}
{-# LANGUAGE RecordWildCards   #-}

{-# LANGUAGE NamedFieldPuns #-}
module Hledger.Data.Transaction (
  -- * Transaction
  nulltransaction,
  transaction,
  txnTieKnot,
  txnUntieKnot,
  -- * operations
  showAccountName,
  hasRealPostings,
  realPostings,
  assignmentPostings,
  virtualPostings,
  balancedVirtualPostings,
  transactionsPostings,
  BalancingOpts(..),
  balancingOpts,
  isTransactionBalanced,
  balanceTransaction,
  balanceTransactionHelper,
  transactionTransformPostings,
  transactionApplyValuation,
  transactionToCost,
  transactionApplyAliases,
  transactionMapPostings,
  transactionMapPostingAmounts,
  -- nonzerobalanceerror,
  -- * date operations
  transactionDate2,
  -- * transaction description parts
  transactionPayee,
  transactionNote,
  -- payeeAndNoteFromDescription,
  -- * rendering
  showTransaction,
  showTransactionOneLineAmounts,
  -- showPostingLine,
  showPostingLines,
  -- * GenericSourcePos
  sourceFilePath,
  sourceFirstLine,
  showGenericSourcePos,
  annotateErrorWithTransaction,
  transactionFile,
  -- * tests
  tests_Transaction
)
where

import Data.Default (Default(..))
import Data.Foldable (asum)
import Data.List (intercalate, partition)
import Data.List.Extra (nubSort)
import Data.Maybe (fromMaybe, isNothing, mapMaybe)
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

data BalancingOpts = BalancingOpts
  { ignore_assertions_ :: Bool  -- ^ Ignore balance assertions
  , infer_prices_      :: Bool  -- ^ Infer prices in unbalanced multicommodity amounts
  , commodity_styles_  :: Maybe (M.Map CommoditySymbol AmountStyle)  -- ^ commodity display styles
  } deriving (Show)

instance Default BalancingOpts where def = balancingOpts

balancingOpts :: BalancingOpts
balancingOpts = BalancingOpts
  { ignore_assertions_ = False
  , infer_prices_      = True
  , commodity_styles_  = Nothing
  }

-- | Check that this transaction would appear balanced to a human when displayed.
-- On success, returns the empty list, otherwise one or more error messages.
--
-- In more detail:
-- For the real postings, and separately for the balanced virtual postings:
--
-- 1. Convert amounts to cost where possible
--
-- 2. When there are two or more non-zero amounts
--    (appearing non-zero when displayed, using the given display styles if provided),
--    are they a mix of positives and negatives ?
--    This is checked separately to give a clearer error message.
--    (Best effort; could be confused by postings with multicommodity amounts.)
--
-- 3. Does the amounts' sum appear non-zero when displayed ?
--    (using the given display styles if provided)
--
transactionCheckBalanced :: BalancingOpts -> Transaction -> [String]
transactionCheckBalanced BalancingOpts{commodity_styles_} t = errs
  where
    (rps, bvps) = (realPostings t, balancedVirtualPostings t)

    -- check for mixed signs, detecting nonzeros at display precision
    canonicalise = maybe id canonicaliseMixedAmount commodity_styles_
    signsOk ps =
      case filter (not.mixedAmountLooksZero) $ map (canonicalise.mixedAmountCost.pamount) ps of
        nonzeros | length nonzeros >= 2
                   -> length (nubSort $ mapMaybe isNegativeMixedAmount nonzeros) > 1
        _          -> True
    (rsignsok, bvsignsok)       = (signsOk rps, signsOk bvps)

    -- check for zero sum, at display precision
    (rsum, bvsum)               = (sumPostings rps, sumPostings bvps)
    (rsumcost, bvsumcost)       = (mixedAmountCost rsum, mixedAmountCost bvsum)
    (rsumdisplay, bvsumdisplay) = (canonicalise rsumcost, canonicalise bvsumcost)
    (rsumok, bvsumok)           = (mixedAmountLooksZero rsumdisplay, mixedAmountLooksZero bvsumdisplay)

    -- generate error messages, showing amounts with their original precision
    errs = filter (not.null) [rmsg, bvmsg]
      where
        rmsg
          | not rsignsok  = "real postings all have the same sign"
          | not rsumok    = "real postings' sum should be 0 but is: " ++ showMixedAmount rsumcost
          | otherwise     = ""
        bvmsg
          | not bvsignsok = "balanced virtual postings all have the same sign"
          | not bvsumok   = "balanced virtual postings' sum should be 0 but is: " ++ showMixedAmount bvsumcost
          | otherwise     = ""

-- | Legacy form of transactionCheckBalanced.
isTransactionBalanced :: BalancingOpts -> Transaction -> Bool
isTransactionBalanced bopts = null . transactionCheckBalanced bopts

-- | Balance this transaction, ensuring that its postings
-- (and its balanced virtual postings) sum to 0,
-- by inferring a missing amount or conversion price(s) if needed.
-- Or if balancing is not possible, because the amounts don't sum to 0 or
-- because there's more than one missing amount, return an error message.
--
-- Transactions with balance assignments can have more than one
-- missing amount; to balance those you should use the more powerful
-- journalBalanceTransactions.
--
-- The "sum to 0" test is done using commodity display precisions,
-- if provided, so that the result agrees with the numbers users can see.
--
balanceTransaction ::
     BalancingOpts
  -> Transaction
  -> Either String Transaction
balanceTransaction bopts = fmap fst . balanceTransactionHelper bopts

-- | Helper used by balanceTransaction and balanceTransactionWithBalanceAssignmentAndCheckAssertionsB;
-- use one of those instead. It also returns a list of accounts
-- and amounts that were inferred.
balanceTransactionHelper ::
     BalancingOpts
  -> Transaction
  -> Either String (Transaction, [(AccountName, MixedAmount)])
balanceTransactionHelper bopts t = do
  (t', inferredamtsandaccts) <- inferBalancingAmount (fromMaybe M.empty $ commodity_styles_ bopts) $
    if infer_prices_ bopts then inferBalancingPrices t else t
  case transactionCheckBalanced bopts t' of
    []   -> Right (txnTieKnot t', inferredamtsandaccts)
    errs -> Left $ transactionBalanceError t' errs

-- | Generate a transaction balancing error message, given the transaction
-- and one or more suberror messages.
transactionBalanceError :: Transaction -> [String] -> String
transactionBalanceError t errs =
  annotateErrorWithTransaction t $
  intercalate "\n" $ "could not balance this transaction:" : errs

annotateErrorWithTransaction :: Transaction -> String -> String
annotateErrorWithTransaction t s =
  unlines [ showGenericSourcePos $ tsourcepos t, s
          , T.unpack . T.stripEnd $ showTransaction t
          ]

-- | Infer up to one missing amount for this transactions's real postings, and
-- likewise for its balanced virtual postings, if needed; or return an error
-- message if we can't. Returns the updated transaction and any inferred posting amounts,
-- with the corresponding accounts, in order).
--
-- We can infer a missing amount when there are multiple postings and exactly
-- one of them is amountless. If the amounts had price(s) the inferred amount
-- have the same price(s), and will be converted to the price commodity.
inferBalancingAmount ::
     M.Map CommoditySymbol AmountStyle -- ^ commodity display styles
  -> Transaction
  -> Either String (Transaction, [(AccountName, MixedAmount)])
inferBalancingAmount styles t@Transaction{tpostings=ps}
  | length amountlessrealps > 1
      = Left $ transactionBalanceError t
        ["can't have more than one real posting with no amount"
        ,"(remember to put two or more spaces between account and amount)"]
  | length amountlessbvps > 1
      = Left $ transactionBalanceError t
        ["can't have more than one balanced virtual posting with no amount"
        ,"(remember to put two or more spaces between account and amount)"]
  | otherwise
      = let psandinferredamts = map inferamount ps
            inferredacctsandamts = [(paccount p, amt) | (p, Just amt) <- psandinferredamts]
        in Right (t{tpostings=map fst psandinferredamts}, inferredacctsandamts)
  where
    (amountfulrealps, amountlessrealps) = partition hasAmount (realPostings t)
    realsum = sumPostings amountfulrealps
    (amountfulbvps, amountlessbvps) = partition hasAmount (balancedVirtualPostings t)
    bvsum = sumPostings amountfulbvps

    inferamount :: Posting -> (Posting, Maybe MixedAmount)
    inferamount p =
      let
        minferredamt = case ptype p of
          RegularPosting         | not (hasAmount p) -> Just realsum
          BalancedVirtualPosting | not (hasAmount p) -> Just bvsum
          _                                          -> Nothing
      in
        case minferredamt of
          Nothing -> (p, Nothing)
          Just a  -> (p{pamount=a', poriginal=Just $ originalPosting p}, Just a')
            where
              -- Inferred amounts are converted to cost.
              -- Also ensure the new amount has the standard style for its commodity
              -- (since the main amount styling pass happened before this balancing pass);
              a' = styleMixedAmount styles . mixedAmountCost $ maNegate a

-- | Infer prices for this transaction's posting amounts, if needed to make
-- the postings balance, and if possible. This is done once for the real
-- postings and again (separately) for the balanced virtual postings. When
-- it's not possible, the transaction is left unchanged.
--
-- The simplest example is a transaction with two postings, each in a
-- different commodity, with no prices specified. In this case we'll add a
-- price to the first posting such that it can be converted to the commodity
-- of the second posting (with -B), and such that the postings balance.
--
-- In general, we can infer a conversion price when the sum of posting amounts
-- contains exactly two different commodities and no explicit prices.  Also
-- all postings are expected to contain an explicit amount (no missing
-- amounts) in a single commodity. Otherwise no price inferring is attempted.
--
-- The transaction itself could contain more than two commodities, and/or
-- prices, if they cancel out; what matters is that the sum of posting amounts
-- contains exactly two commodities and zero prices.
--
-- There can also be more than two postings in either of the commodities.
--
-- We want to avoid excessive display of digits when the calculated price is
-- an irrational number, while hopefully also ensuring the displayed numbers
-- make sense if the user does a manual calculation. This is (mostly) achieved
-- in two ways:
--
-- - when there is only one posting in the "from" commodity, a total price
--   (@@) is used, and all available decimal digits are shown
--
-- - otherwise, a suitable averaged unit price (@) is applied to the relevant
--   postings, with display precision equal to the summed display precisions
--   of the two commodities being converted between, or 2, whichever is larger.
--
-- (We don't always calculate a good-looking display precision for unit prices
-- when the commodity display precisions are low, eg when a journal doesn't
-- use any decimal places. The minimum of 2 helps make the prices shown by the
-- print command a bit less surprising in this case. Could do better.)
--
inferBalancingPrices :: Transaction -> Transaction
inferBalancingPrices t@Transaction{tpostings=ps} = t{tpostings=ps'}
  where
    ps' = map (priceInferrerFor t BalancedVirtualPosting . priceInferrerFor t RegularPosting) ps

-- | Generate a posting update function which assigns a suitable balancing
-- price to the posting, if and as appropriate for the given transaction and
-- posting type (real or balanced virtual). If we cannot or should not infer
-- prices, just act as the identity on postings.
priceInferrerFor :: Transaction -> PostingType -> (Posting -> Posting)
priceInferrerFor t pt = maybe id inferprice inferFromAndTo
  where
    postings     = filter ((==pt).ptype) $ tpostings t
    pcommodities = map acommodity $ concatMap (amounts . pamount) postings
    sumamounts   = amounts $ sumPostings postings  -- amounts normalises to one amount per commodity & price

    -- We can infer prices if there are no prices given, exactly two commodities in the normalised
    -- sum of postings in this transaction, and these two have opposite signs. The amount we are
    -- converting from is the first commodity to appear in the ordered list of postings, and the
    -- commodity we are converting to is the other. If we cannot infer prices, return Nothing.
    inferFromAndTo = case sumamounts of
      [a,b] | noprices, oppositesigns -> asum $ map orderIfMatches pcommodities
        where
          noprices      = all (isNothing . aprice) sumamounts
          oppositesigns = signum (aquantity a) /= signum (aquantity b)
          orderIfMatches x | x == acommodity a = Just (a,b)
                           | x == acommodity b = Just (b,a)
                           | otherwise         = Nothing
      _ -> Nothing

    -- For each posting, if the posting type matches, there is only a single amount in the posting,
    -- and the commodity of the amount matches the amount we're converting from,
    -- then set its price based on the ratio between fromamount and toamount.
    inferprice (fromamount, toamount) posting
        | [a] <- amounts (pamount posting), ptype posting == pt, acommodity a == acommodity fromamount
            = posting{ pamount   = mixedAmount a{aprice=Just conversionprice}
                     , poriginal = Just $ originalPosting posting }
        | otherwise = posting
      where
        -- If only one Amount in the posting list matches fromamount we can use TotalPrice.
        -- Otherwise divide the conversion equally among the Amounts by using a unit price.
        conversionprice = case filter (== acommodity fromamount) pcommodities of
            [_] -> TotalPrice $ negate toamount
            _   -> UnitPrice  $ negate unitprice `withPrecision` unitprecision

        unitprice     = aquantity fromamount `divideAmount` toamount
        unitprecision = case (asprecision $ astyle fromamount, asprecision $ astyle toamount) of
            (Precision a, Precision b) -> Precision . max 2 $ saturatedAdd a b
            _                          -> NaturalPrecision
        saturatedAdd a b = if maxBound - a < b then maxBound else a + b

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
  tests "Transaction" [

      tests "showPostingLines" [
          test "null posting" $ showPostingLines nullposting @?= ["                   0"]
        , test "non-null posting" $
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
      in tests "postingsAsLines" [
              test "null-transaction" $ postingsAsLines False (tpostings nulltransaction) @?= []
            , test "implicit-amount" $ postingsAsLines False (tpostings timp) @?=
                  [ "    a           $1.00"
                  , "    b" -- implicit amount remains implicit
                  ]
            , test "explicit-amounts" $ postingsAsLines False (tpostings texp) @?=
                  [ "    a           $1.00"
                  , "    b          $-1.00"
                  ]
            , test "one-explicit-amount" $ postingsAsLines False (tpostings texp1) @?=
                  [ "    (a)           $1.00"
                  ]
            , test "explicit-amounts-two-commodities" $ postingsAsLines False (tpostings texp2) @?=
                  [ "    a             $1.00"
                  , "    b    -1.00h @ $1.00"
                  ]
            , test "explicit-amounts-not-explicitly-balanced" $ postingsAsLines False (tpostings texp2b) @?=
                  [ "    a           $1.00"
                  , "    b          -1.00h"
                  ]
            , test "implicit-amount-not-last" $ postingsAsLines False (tpostings t3) @?=
                  ["    a           $1.00", "    b", "    c          $-1.00"]
            -- , test "ensure-visibly-balanced" $
            --    in postingsAsLines False (tpostings t4) @?=
            --       ["    a          $-0.01", "    b           $0.005", "    c           $0.005"]

            ]

    , test "inferBalancingAmount" $ do
         (fst <$> inferBalancingAmount M.empty nulltransaction) @?= Right nulltransaction
         (fst <$> inferBalancingAmount M.empty nulltransaction{tpostings = ["a" `post` usd (-5), "b" `post` missingamt]}) @?=
           Right nulltransaction{tpostings = ["a" `post` usd (-5), "b" `post` usd 5]}
         (fst <$> inferBalancingAmount M.empty nulltransaction{tpostings = ["a" `post` usd (-5), "b" `post` (eur 3 @@ usd 4), "c" `post` missingamt]}) @?=
           Right nulltransaction{tpostings = ["a" `post` usd (-5), "b" `post` (eur 3 @@ usd 4), "c" `post` usd 1]}

    , tests "showTransaction" [
          test "null transaction" $ showTransaction nulltransaction @?= "0000-01-01\n\n"
        , test "non-null transaction" $ showTransaction
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
        , test "show a balanced transaction" $
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
        , test "show an unbalanced transaction, should not elide" $
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
        , test "show a transaction with one posting and a missing amount" $
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
        , test "show a transaction with a priced commodityless amount" $
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
    , tests "balanceTransaction" [
         test "detect unbalanced entry, sign error" $
          assertLeft
            (balanceTransaction def
               (Transaction
                  0
                  ""
                  nullsourcepos
                  (fromGregorian 2007 01 28)
                  Nothing
                  Unmarked
                  ""
                  "test"
                  ""
                  []
                  [posting {paccount = "a", pamount = mixedAmount (usd 1)}, posting {paccount = "b", pamount = mixedAmount (usd 1)}]))
        ,test "detect unbalanced entry, multiple missing amounts" $
          assertLeft $
             balanceTransaction def
               (Transaction
                  0
                  ""
                  nullsourcepos
                  (fromGregorian 2007 01 28)
                  Nothing
                  Unmarked
                  ""
                  "test"
                  ""
                  []
                  [ posting {paccount = "a", pamount = missingmixedamt}
                  , posting {paccount = "b", pamount = missingmixedamt}
                  ])
        ,test "one missing amount is inferred" $
          (pamount . last . tpostings <$>
           balanceTransaction def
             (Transaction
                0
                ""
                nullsourcepos
                (fromGregorian 2007 01 28)
                Nothing
                Unmarked
                ""
                ""
                ""
                []
                [posting {paccount = "a", pamount = mixedAmount (usd 1)}, posting {paccount = "b", pamount = missingmixedamt}])) @?=
          Right (mixedAmount $ usd (-1))
        ,test "conversion price is inferred" $
          (pamount . head . tpostings <$>
           balanceTransaction def
             (Transaction
                0
                ""
                nullsourcepos
                (fromGregorian 2007 01 28)
                Nothing
                Unmarked
                ""
                ""
                ""
                []
                [ posting {paccount = "a", pamount = mixedAmount (usd 1.35)}
                , posting {paccount = "b", pamount = mixedAmount (eur (-1))}
                ])) @?=
          Right (mixedAmount $ usd 1.35 @@ eur 1)
        ,test "balanceTransaction balances based on cost if there are unit prices" $
          assertRight $
          balanceTransaction def
            (Transaction
               0
               ""
               nullsourcepos
               (fromGregorian 2011 01 01)
               Nothing
               Unmarked
               ""
               ""
               ""
               []
               [ posting {paccount = "a", pamount = mixedAmount $ usd 1 `at` eur 2}
               , posting {paccount = "a", pamount = mixedAmount $ usd (-2) `at` eur 1}
               ])
        ,test "balanceTransaction balances based on cost if there are total prices" $
          assertRight $
          balanceTransaction def
            (Transaction
               0
               ""
               nullsourcepos
               (fromGregorian 2011 01 01)
               Nothing
               Unmarked
               ""
               ""
               ""
               []
               [ posting {paccount = "a", pamount = mixedAmount $ usd 1 @@ eur 1}
               , posting {paccount = "a", pamount = mixedAmount $ usd (-2) @@ eur (-1)}
               ])
        ]
    , tests "isTransactionBalanced" [
         test "detect balanced" $
          assertBool "" $
          isTransactionBalanced def $
          Transaction
            0
            ""
            nullsourcepos
            (fromGregorian 2009 01 01)
            Nothing
            Unmarked
            ""
            "a"
            ""
            []
            [ posting {paccount = "b", pamount = mixedAmount (usd 1.00)}
            , posting {paccount = "c", pamount = mixedAmount (usd (-1.00))}
            ]
        ,test "detect unbalanced" $
          assertBool "" $
          not $
          isTransactionBalanced def $
          Transaction
            0
            ""
            nullsourcepos
            (fromGregorian 2009 01 01)
            Nothing
            Unmarked
            ""
            "a"
            ""
            []
            [ posting {paccount = "b", pamount = mixedAmount (usd 1.00)}
            , posting {paccount = "c", pamount = mixedAmount (usd (-1.01))}
            ]
        ,test "detect unbalanced, one posting" $
          assertBool "" $
          not $
          isTransactionBalanced def $
          Transaction
            0
            ""
            nullsourcepos
            (fromGregorian 2009 01 01)
            Nothing
            Unmarked
            ""
            "a"
            ""
            []
            [posting {paccount = "b", pamount = mixedAmount (usd 1.00)}]
        ,test "one zero posting is considered balanced for now" $
          assertBool "" $
          isTransactionBalanced def $
          Transaction
            0
            ""
            nullsourcepos
            (fromGregorian 2009 01 01)
            Nothing
            Unmarked
            ""
            "a"
            ""
            []
            [posting {paccount = "b", pamount = mixedAmount (usd 0)}]
        ,test "virtual postings don't need to balance" $
          assertBool "" $
          isTransactionBalanced def $
          Transaction
            0
            ""
            nullsourcepos
            (fromGregorian 2009 01 01)
            Nothing
            Unmarked
            ""
            "a"
            ""
            []
            [ posting {paccount = "b", pamount = mixedAmount (usd 1.00)}
            , posting {paccount = "c", pamount = mixedAmount (usd (-1.00))}
            , posting {paccount = "d", pamount = mixedAmount (usd 100), ptype = VirtualPosting}
            ]
        ,test "balanced virtual postings need to balance among themselves" $
          assertBool "" $
          not $
          isTransactionBalanced def $
          Transaction
            0
            ""
            nullsourcepos
            (fromGregorian 2009 01 01)
            Nothing
            Unmarked
            ""
            "a"
            ""
            []
            [ posting {paccount = "b", pamount = mixedAmount (usd 1.00)}
            , posting {paccount = "c", pamount = mixedAmount (usd (-1.00))}
            , posting {paccount = "d", pamount = mixedAmount (usd 100), ptype = BalancedVirtualPosting}
            ]
        ,test "balanced virtual postings need to balance among themselves (2)" $
          assertBool "" $
          isTransactionBalanced def $
          Transaction
            0
            ""
            nullsourcepos
            (fromGregorian 2009 01 01)
            Nothing
            Unmarked
            ""
            "a"
            ""
            []
            [ posting {paccount = "b", pamount = mixedAmount (usd 1.00)}
            , posting {paccount = "c", pamount = mixedAmount (usd (-1.00))}
            , posting {paccount = "d", pamount = mixedAmount (usd 100), ptype = BalancedVirtualPosting}
            , posting {paccount = "3", pamount = mixedAmount (usd (-100)), ptype = BalancedVirtualPosting}
            ]
        ]
    ]
