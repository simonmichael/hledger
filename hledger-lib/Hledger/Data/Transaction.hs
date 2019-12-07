{-|

A 'Transaction' represents a movement of some commodity(ies) between two
or more accounts. It consists of multiple account 'Posting's which balance
to zero, a date, and optional extras like description, cleared status, and
tags.

-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RecordWildCards #-}

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
  isTransactionBalanced,
  balanceTransaction,
  balanceTransactionHelper,
  transactionTransformPostings,
  transactionApplyValuation,
  transactionToCost,
  -- nonzerobalanceerror,
  -- * date operations
  transactionDate2,
  -- * arithmetic
  transactionPostingBalances,
  -- * transaction description parts
  transactionPayee,
  transactionNote,
  -- payeeAndNoteFromDescription,
  -- * rendering
  showTransaction,
  showTransactionOneLineAmounts,
  showTransactionUnelided,
  showTransactionUnelidedOneLineAmounts,
  -- showPostingLine,
  showPostingLines,
  -- * GenericSourcePos
  sourceFilePath,
  sourceFirstLine,
  showGenericSourcePos,
  annotateErrorWithTransaction,
  -- * tests
  tests_Transaction
)
where
import Data.List
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Calendar
import Text.Printf
import qualified Data.Map as M

import Hledger.Utils
import Hledger.Data.Types
import Hledger.Data.Dates
import Hledger.Data.Posting
import Hledger.Data.Amount
import Hledger.Data.Valuation

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
transaction :: String -> [Posting] -> Transaction
transaction datestr ps = txnTieKnot $ nulltransaction{tdate=parsedate datestr, tpostings=ps}

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
  | otherwise = (textstrip p, textstrip $ T.drop 1 n)
  where
    (p, n) = T.span (/= '|') t

{-|
Render a journal transaction as text in the style of Ledger's print command.

Ledger 2.x's standard format looks like this:

@
yyyy/mm/dd[ *][ CODE] description.........          [  ; comment...............]
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
showTransaction :: Transaction -> String
showTransaction = showTransactionHelper False

-- | Deprecated alias for 'showTransaction'
showTransactionUnelided :: Transaction -> String
showTransactionUnelided = showTransaction  -- TODO: drop it

-- | Like showTransaction, but explicit multi-commodity amounts
-- are shown on one line, comma-separated. In this case the output will
-- not be parseable journal syntax.
showTransactionOneLineAmounts :: Transaction -> String
showTransactionOneLineAmounts = showTransactionHelper True

-- | Deprecated alias for 'showTransactionOneLineAmounts'
showTransactionUnelidedOneLineAmounts :: Transaction -> String
showTransactionUnelidedOneLineAmounts = showTransactionOneLineAmounts  -- TODO: drop it

-- | Helper for showTransaction*.
showTransactionHelper :: Bool -> Transaction -> String
showTransactionHelper onelineamounts t =
    unlines $ [descriptionline]
              ++ newlinecomments
              ++ (postingsAsLines onelineamounts (tpostings t))
              ++ [""]
    where
      descriptionline = rstrip $ concat [date, status, code, desc, samelinecomment]
      date = showDate (tdate t) ++ maybe "" (("="++) . showDate) (tdate2 t)
      status | tstatus t == Cleared = " *"
             | tstatus t == Pending = " !"
             | otherwise            = ""
      code = if T.length (tcode t) > 0 then printf " (%s)" $ T.unpack $ tcode t else ""
      desc = if null d then "" else " " ++ d where d = T.unpack $ tdescription t
      (samelinecomment, newlinecomments) =
        case renderCommentLines (tcomment t) of []   -> ("",[])
                                                c:cs -> (c,cs)

-- | Render a transaction or posting's comment as indented, semicolon-prefixed comment lines.
-- The first line (unless empty) will have leading space, subsequent lines will have a larger indent.
renderCommentLines :: Text -> [String]
renderCommentLines t =
  case lines $ T.unpack t of
    []      -> []
    [l]     -> [(commentSpace . comment) l]        -- single-line comment
    ("":ls) -> "" : map (lineIndent . comment) ls  -- multi-line comment with empty first line
    (l:ls)  -> (commentSpace . comment) l : map (lineIndent . comment) ls
  where
    comment = ("; "++)

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
--
postingsAsLines :: Bool -> [Posting] -> [String]
postingsAsLines onelineamounts ps = concatMap (postingAsLines False onelineamounts ps) ps

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
postingAsLines :: Bool -> Bool -> [Posting] -> Posting -> [String]
postingAsLines elideamount onelineamounts pstoalignwith p = concat [
    postingblock
    ++ newlinecomments
    | postingblock <- postingblocks]
  where
    postingblocks = [map rstrip $ lines $ concatTopPadded [statusandaccount, "  ", amt, assertion, samelinecomment] | amt <- shownAmounts]
    assertion = maybe "" ((' ':).showBalanceAssertion) $ pbalanceassertion p
    statusandaccount = lineIndent $ fitString (Just $ minwidth) Nothing False True $ pstatusandacct p
        where
          -- pad to the maximum account name width, plus 2 to leave room for status flags, to keep amounts aligned
          minwidth = maximum $ map ((2+) . textWidth . T.pack . pacctstr) pstoalignwith
          pstatusandacct p' = pstatusprefix p' ++ pacctstr p'
          pstatusprefix p' | null s    = ""
                           | otherwise = s ++ " "
            where s = show $ pstatus p'
          pacctstr p' = showAccountName Nothing (ptype p') (paccount p')

    -- currently prices are considered part of the amount string when right-aligning amounts
    shownAmounts
      | elideamount    = [""]
      | onelineamounts = [fitString (Just amtwidth) Nothing False False $ showMixedAmountOneLine $ pamount p]
      | null (amounts $ pamount p) = [""]
      | otherwise      = map (fitStringMulti (Just amtwidth) Nothing False False . showAmount ) . amounts $ pamount p
      where
        amtwidth = maximum $ 12 : map (strWidth . showMixedAmount . pamount) pstoalignwith  -- min. 12 for backwards compatibility

    (samelinecomment, newlinecomments) =
      case renderCommentLines (pcomment p) of []   -> ("",[])
                                              c:cs -> (c,cs)

-- | Render a balance assertion, as the =[=][*] symbol and expected amount.
showBalanceAssertion :: BalanceAssertion -> [Char]
showBalanceAssertion BalanceAssertion{..} =
  "=" ++ ['=' | batotal] ++ ['*' | bainclusive] ++ " " ++ showAmountWithZeroCommodity baamount

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
showPostingLines :: Posting -> [String]
showPostingLines p = postingAsLines False False ps p where
    ps | Just t <- ptransaction p = tpostings t
       | otherwise = [p]

-- | Prepend a suitable indent for a posting (or transaction/posting comment) line.
lineIndent :: String -> String
lineIndent = ("    "++)

-- | Prepend the space required before a same-line comment.
commentSpace :: String -> String
commentSpace = ("  "++)

-- | Show an account name, clipped to the given width if any, and
-- appropriately bracketed/parenthesised for the given posting type.
showAccountName :: Maybe Int -> PostingType -> AccountName -> String
showAccountName w = fmt
    where
      fmt RegularPosting = take w' . T.unpack
      fmt VirtualPosting = parenthesise . reverse . take (w'-2) . reverse . T.unpack
      fmt BalancedVirtualPosting = bracket . reverse . take (w'-2) . reverse . T.unpack
      w' = fromMaybe 999999 w

parenthesise :: String -> String
parenthesise s = "("++s++")"

bracket :: String -> String
bracket s = "["++s++"]"

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

-- | Get the sums of a transaction's real, virtual, and balanced virtual postings.
transactionPostingBalances :: Transaction -> (MixedAmount,MixedAmount,MixedAmount)
transactionPostingBalances t = (sumPostings $ realPostings t
                               ,sumPostings $ virtualPostings t
                               ,sumPostings $ balancedVirtualPostings t)

-- | Does this transaction appear balanced when rendered, optionally with the
-- given commodity display styles ? More precisely:
-- after converting amounts to cost using explicit transaction prices if any;
-- and summing the real postings, and summing the balanced virtual postings;
-- and applying the given display styles if any (maybe affecting decimal places);
-- do both totals appear to be zero when rendered ?
isTransactionBalanced :: Maybe (M.Map CommoditySymbol AmountStyle) -> Transaction -> Bool
isTransactionBalanced styles t =
    -- isReallyZeroMixedAmountCost rsum && isReallyZeroMixedAmountCost bvsum
    isZeroMixedAmount rsum' && isZeroMixedAmount bvsum'
    where
      (rsum, _, bvsum) = transactionPostingBalances t
      rsum'  = canonicalise $ costOfMixedAmount rsum
      bvsum' = canonicalise $ costOfMixedAmount bvsum
      canonicalise = maybe id canonicaliseMixedAmount styles

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
     Maybe (M.Map CommoditySymbol AmountStyle)  -- ^ commodity display styles
  -> Transaction
  -> Either String Transaction
balanceTransaction mstyles = fmap fst . balanceTransactionHelper mstyles

-- | Helper used by balanceTransaction and balanceTransactionWithBalanceAssignmentAndCheckAssertionsB;
-- use one of those instead. It also returns a list of accounts
-- and amounts that were inferred.
balanceTransactionHelper ::
     Maybe (M.Map CommoditySymbol AmountStyle)  -- ^ commodity display styles
  -> Transaction
  -> Either String (Transaction, [(AccountName, MixedAmount)])
balanceTransactionHelper mstyles t = do
  (t', inferredamtsandaccts) <-
    inferBalancingAmount (fromMaybe M.empty mstyles) $ inferBalancingPrices t
  if isTransactionBalanced mstyles t'
  then Right (txnTieKnot t', inferredamtsandaccts)
  else Left $ annotateErrorWithTransaction t' $ nonzerobalanceerror t'

  where
    nonzerobalanceerror :: Transaction -> String
    nonzerobalanceerror tt = printf "could not balance this transaction (%s%s%s)" rmsg sep bvmsg
        where
          (rsum, _, bvsum) = transactionPostingBalances tt
          rmsg | isReallyZeroMixedAmountCost rsum = ""
               | otherwise = "real postings are off by "
                 ++ showMixedAmount (costOfMixedAmount rsum)
          bvmsg | isReallyZeroMixedAmountCost bvsum = ""
                | otherwise = "balanced virtual postings are off by "
                  ++ showMixedAmount (costOfMixedAmount bvsum)
          sep = if not (null rmsg) && not (null bvmsg) then "; " else "" :: String

annotateErrorWithTransaction :: Transaction -> String -> String
annotateErrorWithTransaction t s = intercalate "\n" [showGenericSourcePos $ tsourcepos t, s, showTransaction t]

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
      = Left $ annotateErrorWithTransaction t "could not balance this transaction - can't have more than one real posting with no amount (remember to put 2 or more spaces before amounts)"
  | length amountlessbvps > 1
      = Left $ annotateErrorWithTransaction t "could not balance this transaction - can't have more than one balanced virtual posting with no amount (remember to put 2 or more spaces before amounts)"
  | otherwise
      = let psandinferredamts = map inferamount ps
            inferredacctsandamts = [(paccount p, amt) | (p, Just amt) <- psandinferredamts]
        in Right (t{tpostings=map fst psandinferredamts}, inferredacctsandamts)
  where
    (amountfulrealps, amountlessrealps) = partition hasAmount (realPostings t)
    realsum = sumStrict $ map pamount amountfulrealps
    (amountfulbvps, amountlessbvps) = partition hasAmount (balancedVirtualPostings t)
    bvsum = sumStrict $ map pamount amountfulbvps

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
              a' = styleMixedAmount styles $ normaliseMixedAmount $ costOfMixedAmount (-a)

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
-- posting type (real or balanced virtual).
priceInferrerFor :: Transaction -> PostingType -> (Posting -> Posting)
priceInferrerFor t pt = inferprice
  where
    postings       = filter ((==pt).ptype) $ tpostings t
    pmixedamounts  = map pamount postings
    pamounts       = concatMap amounts pmixedamounts
    pcommodities   = map acommodity pamounts
    sumamounts     = amounts $ sumStrict pmixedamounts -- sum normalises to one amount per commodity & price
    sumcommodities = map acommodity sumamounts
    sumprices      = filter (/=Nothing) $ map aprice sumamounts
    caninferprices = length sumcommodities == 2 && null sumprices

    inferprice p@Posting{pamount=Mixed [a]}
      | caninferprices && ptype p == pt && acommodity a == fromcommodity
        = p{pamount=Mixed [a{aprice=Just conversionprice}], poriginal=Just $ originalPosting p}
      where
        fromcommodity = head $ filter (`elem` sumcommodities) pcommodities -- these heads are ugly but should be safe
        conversionprice
          | fromcount==1 = TotalPrice $ abs toamount `withPrecision` maxprecision
          | otherwise    = UnitPrice $ abs unitprice `withPrecision` unitprecision
          where
            fromcount     = length $ filter ((==fromcommodity).acommodity) pamounts
            fromamount    = head $ filter ((==fromcommodity).acommodity) sumamounts
            tocommodity   = head $ filter (/=fromcommodity) sumcommodities
            toamount      = head $ filter ((==tocommodity).acommodity) sumamounts
            unitprice     = (aquantity fromamount) `divideAmount` toamount
            unitprecision = max 2 (asprecision (astyle toamount) + asprecision (astyle fromamount))
    inferprice p = p

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
-- the provided price oracle, commodity styles, reference dates, and
-- whether this is for a multiperiod report or not. See
-- amountApplyValuation.
transactionApplyValuation :: PriceOracle -> M.Map CommoditySymbol AmountStyle -> Day -> Maybe Day -> Day -> Bool -> Transaction -> ValuationType -> Transaction
transactionApplyValuation priceoracle styles periodlast mreportlast today ismultiperiod t v =
  transactionTransformPostings (\p -> postingApplyValuation priceoracle styles periodlast mreportlast today ismultiperiod p v) t

-- | Convert this transaction's amounts to cost, and apply the appropriate amount styles.
transactionToCost :: M.Map CommoditySymbol AmountStyle -> Transaction -> Transaction
transactionToCost styles t@Transaction{tpostings=ps} = t{tpostings=map (postingToCost styles) ps}

-- tests

tests_Transaction :: TestTree
tests_Transaction =
  tests "Transaction" [

      tests "postingAsLines" [
          test "null posting" $ postingAsLines False False [posting] posting @?= [""]
        , test "non-null posting" $
           let p =
                posting
                  { pstatus = Cleared
                  , paccount = "a"
                  , pamount = Mixed [usd 1, hrs 2]
                  , pcomment = "pcomment1\npcomment2\n  tag3: val3  \n"
                  , ptype = RegularPosting
                  , ptags = [("ptag1", "val1"), ("ptag2", "val2")]
                  }
           in postingAsLines False False [p] p @?=
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
          test "null transaction" $ showTransaction nulltransaction @?= "0000/01/01\n\n"
        , test "non-null transaction" $ showTransaction
            nulltransaction
              { tdate = parsedate "2012/05/14"
              , tdate2 = Just $ parsedate "2012/05/15"
              , tstatus = Unmarked
              , tcode = "code"
              , tdescription = "desc"
              , tcomment = "tcomment1\ntcomment2\n"
              , ttags = [("ttag1", "val1")]
              , tpostings =
                  [ nullposting
                      { pstatus = Cleared
                      , paccount = "a"
                      , pamount = Mixed [usd 1, hrs 2]
                      , pcomment = "\npcomment2\n"
                      , ptype = RegularPosting
                      , ptags = [("ptag1", "val1"), ("ptag2", "val2")]
                      }
                  ]
              } @?=
          unlines
            [ "2012/05/14=2012/05/15 (code) desc  ; tcomment1"
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
                   (parsedate "2007/01/28")
                   Nothing
                   Unmarked
                   ""
                   "coopportunity"
                   ""
                   []
                   [ posting {paccount = "expenses:food:groceries", pamount = Mixed [usd 47.18], ptransaction = Just t}
                   , posting {paccount = "assets:checking", pamount = Mixed [usd (-47.18)], ptransaction = Just t}
                   ]
            in showTransaction t) @?=
          (unlines
             [ "2007/01/28 coopportunity"
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
                (parsedate "2007/01/28")
                Nothing
                Unmarked
                ""
                "coopportunity"
                ""
                []
                [ posting {paccount = "expenses:food:groceries", pamount = Mixed [usd 47.18]}
                , posting {paccount = "assets:checking", pamount = Mixed [usd (-47.19)]}
                ])) @?=
          (unlines
             [ "2007/01/28 coopportunity"
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
                (parsedate "2007/01/28")
                Nothing
                Unmarked
                ""
                "coopportunity"
                ""
                []
                [posting {paccount = "expenses:food:groceries", pamount = missingmixedamt}])) @?=
          (unlines ["2007/01/28 coopportunity", "    expenses:food:groceries", ""])
        , test "show a transaction with a priced commodityless amount" $
          (showTransaction
             (txnTieKnot $
              Transaction
                0
                ""
                nullsourcepos
                (parsedate "2010/01/01")
                Nothing
                Unmarked
                ""
                "x"
                ""
                []
                [ posting {paccount = "a", pamount = Mixed [num 1 `at` (usd 2 `withPrecision` 0)]}
                , posting {paccount = "b", pamount = missingmixedamt}
                ])) @?=
          (unlines ["2010/01/01 x", "    a          1 @ $2", "    b", ""])
        ]
    , tests "balanceTransaction" [
         test "detect unbalanced entry, sign error" $
          assertLeft
            (balanceTransaction
               Nothing
               (Transaction
                  0
                  ""
                  nullsourcepos
                  (parsedate "2007/01/28")
                  Nothing
                  Unmarked
                  ""
                  "test"
                  ""
                  []
                  [posting {paccount = "a", pamount = Mixed [usd 1]}, posting {paccount = "b", pamount = Mixed [usd 1]}]))
        ,test "detect unbalanced entry, multiple missing amounts" $
          assertLeft $
             balanceTransaction
               Nothing
               (Transaction
                  0
                  ""
                  nullsourcepos
                  (parsedate "2007/01/28")
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
           balanceTransaction
             Nothing
             (Transaction
                0
                ""
                nullsourcepos
                (parsedate "2007/01/28")
                Nothing
                Unmarked
                ""
                ""
                ""
                []
                [posting {paccount = "a", pamount = Mixed [usd 1]}, posting {paccount = "b", pamount = missingmixedamt}])) @?=
          Right (Mixed [usd (-1)])
        ,test "conversion price is inferred" $
          (pamount . head . tpostings <$>
           balanceTransaction
             Nothing
             (Transaction
                0
                ""
                nullsourcepos
                (parsedate "2007/01/28")
                Nothing
                Unmarked
                ""
                ""
                ""
                []
                [ posting {paccount = "a", pamount = Mixed [usd 1.35]}
                , posting {paccount = "b", pamount = Mixed [eur (-1)]}
                ])) @?=
          Right (Mixed [usd 1.35 @@ (eur 1 `withPrecision` maxprecision)])
        ,test "balanceTransaction balances based on cost if there are unit prices" $
          assertRight $
          balanceTransaction
            Nothing
            (Transaction
               0
               ""
               nullsourcepos
               (parsedate "2011/01/01")
               Nothing
               Unmarked
               ""
               ""
               ""
               []
               [ posting {paccount = "a", pamount = Mixed [usd 1 `at` eur 2]}
               , posting {paccount = "a", pamount = Mixed [usd (-2) `at` eur 1]}
               ])
        ,test "balanceTransaction balances based on cost if there are total prices" $
          assertRight $
          balanceTransaction
            Nothing
            (Transaction
               0
               ""
               nullsourcepos
               (parsedate "2011/01/01")
               Nothing
               Unmarked
               ""
               ""
               ""
               []
               [ posting {paccount = "a", pamount = Mixed [usd 1 @@ eur 1]}
               , posting {paccount = "a", pamount = Mixed [usd (-2) @@ eur 1]}
               ])
        ]
    , tests "isTransactionBalanced" [
         test "detect balanced" $
          assertBool "" $
          isTransactionBalanced Nothing $
          Transaction
            0
            ""
            nullsourcepos
            (parsedate "2009/01/01")
            Nothing
            Unmarked
            ""
            "a"
            ""
            []
            [ posting {paccount = "b", pamount = Mixed [usd 1.00]}
            , posting {paccount = "c", pamount = Mixed [usd (-1.00)]}
            ]
        ,test "detect unbalanced" $
          assertBool "" $
          not $
          isTransactionBalanced Nothing $
          Transaction
            0
            ""
            nullsourcepos
            (parsedate "2009/01/01")
            Nothing
            Unmarked
            ""
            "a"
            ""
            []
            [ posting {paccount = "b", pamount = Mixed [usd 1.00]}
            , posting {paccount = "c", pamount = Mixed [usd (-1.01)]}
            ]
        ,test "detect unbalanced, one posting" $
          assertBool "" $
          not $
          isTransactionBalanced Nothing $
          Transaction
            0
            ""
            nullsourcepos
            (parsedate "2009/01/01")
            Nothing
            Unmarked
            ""
            "a"
            ""
            []
            [posting {paccount = "b", pamount = Mixed [usd 1.00]}]
        ,test "one zero posting is considered balanced for now" $
          assertBool "" $
          isTransactionBalanced Nothing $
          Transaction
            0
            ""
            nullsourcepos
            (parsedate "2009/01/01")
            Nothing
            Unmarked
            ""
            "a"
            ""
            []
            [posting {paccount = "b", pamount = Mixed [usd 0]}]
        ,test "virtual postings don't need to balance" $
          assertBool "" $
          isTransactionBalanced Nothing $
          Transaction
            0
            ""
            nullsourcepos
            (parsedate "2009/01/01")
            Nothing
            Unmarked
            ""
            "a"
            ""
            []
            [ posting {paccount = "b", pamount = Mixed [usd 1.00]}
            , posting {paccount = "c", pamount = Mixed [usd (-1.00)]}
            , posting {paccount = "d", pamount = Mixed [usd 100], ptype = VirtualPosting}
            ]
        ,test "balanced virtual postings need to balance among themselves" $
          assertBool "" $
          not $
          isTransactionBalanced Nothing $
          Transaction
            0
            ""
            nullsourcepos
            (parsedate "2009/01/01")
            Nothing
            Unmarked
            ""
            "a"
            ""
            []
            [ posting {paccount = "b", pamount = Mixed [usd 1.00]}
            , posting {paccount = "c", pamount = Mixed [usd (-1.00)]}
            , posting {paccount = "d", pamount = Mixed [usd 100], ptype = BalancedVirtualPosting}
            ]
        ,test "balanced virtual postings need to balance among themselves (2)" $
          assertBool "" $
          isTransactionBalanced Nothing $
          Transaction
            0
            ""
            nullsourcepos
            (parsedate "2009/01/01")
            Nothing
            Unmarked
            ""
            "a"
            ""
            []
            [ posting {paccount = "b", pamount = Mixed [usd 1.00]}
            , posting {paccount = "c", pamount = Mixed [usd (-1.00)]}
            , posting {paccount = "d", pamount = Mixed [usd 100], ptype = BalancedVirtualPosting}
            , posting {paccount = "3", pamount = Mixed [usd (-100)], ptype = BalancedVirtualPosting}
            ]
        ]
    ]
