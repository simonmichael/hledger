{-# LANGUAGE FlexibleContexts #-}
{-|

A 'Transaction' represents a movement of some commodity(ies) between two
or more accounts. It consists of multiple account 'Posting's which balance
to zero, a date, and optional extras like description, cleared status, and
tags.

-}

{-# LANGUAGE OverloadedStrings, LambdaCase #-}

module Hledger.Data.Transaction (
  -- * Transaction
  nullsourcepos,
  nulltransaction,
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
  -- nonzerobalanceerror,
  -- * date operations
  transactionDate2,
  -- * arithmetic
  transactionPostingBalances,
  balanceTransaction,
  balanceTransactionUpdate,
  -- * rendering
  showTransaction,
  showTransactionUnelided,
  showTransactionUnelidedOneLineAmounts,
  showPostingLine,
  showPostingLines,
  -- * GenericSourcePos
  sourceFilePath,
  sourceFirstLine,
  showGenericSourcePos,
  -- * misc.
  tests_Hledger_Data_Transaction
)
where
import Data.List
import Control.Monad.Except
import Control.Monad.Identity
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Calendar
import Test.HUnit
import Text.Printf
import qualified Data.Map as Map

import Hledger.Utils
import Hledger.Data.Types
import Hledger.Data.Dates
import Hledger.Data.Posting
import Hledger.Data.Amount

instance Show Transaction where show = showTransactionUnelided

instance Show ModifierTransaction where
    show t = "= " ++ T.unpack (mtvalueexpr t) ++ "\n" ++ unlines (map show (mtpostings t))

instance Show PeriodicTransaction where
    show t = "~ " ++ T.unpack (ptperiodicexpr t) ++ "\n" ++ unlines (map show (ptpostings t))

sourceFilePath :: GenericSourcePos -> FilePath
sourceFilePath = \case
    GenericSourcePos fp _ _ -> fp
    JournalSourcePos fp _ -> fp

sourceFirstLine :: GenericSourcePos -> Int
sourceFirstLine = \case
    GenericSourcePos _ line _ -> line
    JournalSourcePos _ (line, _) -> line

showGenericSourcePos :: GenericSourcePos -> String
showGenericSourcePos = \case
    GenericSourcePos fp line column -> show fp ++ " (line " ++ show line ++ ", column " ++ show column ++ ")"
    JournalSourcePos fp (line, line') -> show fp ++ " (lines " ++ show line ++ "-" ++ show line' ++ ")"

nullsourcepos :: GenericSourcePos
nullsourcepos = GenericSourcePos "" 1 1

nulltransaction :: Transaction
nulltransaction = Transaction {
                    tindex=0,
                    tsourcepos=nullsourcepos,
                    tdate=nulldate,
                    tdate2=Nothing,
                    tstatus=Nothing,
                    tcode="",
                    tdescription="",
                    tcomment="",
                    ttags=[],
                    tpostings=[],
                    tpreceding_comment_lines=""
                  }

{-|
Show a journal transaction, formatted for the print command. ledger 2.x's
standard format looks like this:

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
-}
showTransaction :: Transaction -> String
showTransaction = showTransactionHelper True False

showTransactionUnelided :: Transaction -> String
showTransactionUnelided = showTransactionHelper False False

tests_showTransactionUnelided = [
   "showTransactionUnelided" ~: do
    let t `gives` s = assertEqual "" s (showTransactionUnelided t)
    nulltransaction `gives` "0000/01/01\n\n"
    nulltransaction{
      tdate=parsedate "2012/05/14",
      tdate2=Just $ parsedate "2012/05/15",
      tstatus=Nothing,
      tcode="code",
      tdescription="desc",
      tcomment="tcomment1\ntcomment2\n",
      ttags=[("ttag1","val1")],
      tpostings=[
        nullposting{
          pstatus=Just Cleared,
          paccount="a",
          pamount=Mixed [usd 1, hrs 2],
          pcomment="\npcomment2\n",
          ptype=RegularPosting,
          ptags=[("ptag1","val1"),("ptag2","val2")]
          }
       ]
      }
      `gives` unlines [
      "2012/05/14=2012/05/15 (code) desc    ; tcomment1",
      "    ; tcomment2",
      "    * a         $1.00",
      "    ; pcomment2",
      "    * a         2.00h",
      "    ; pcomment2",
      ""
      ]
 ]

showTransactionUnelidedOneLineAmounts :: Transaction -> String
showTransactionUnelidedOneLineAmounts = showTransactionHelper False True

-- cf showPosting
showTransactionHelper :: Bool -> Bool -> Transaction -> String
showTransactionHelper elide onelineamounts t =
    unlines $ [descriptionline]
              ++ newlinecomments
              ++ (postingsAsLines elide onelineamounts t (tpostings t))
              ++ [""]
    where
      descriptionline = rstrip $ concat [date, status, code, desc, samelinecomment]
      date = showDate (tdate t) ++ maybe "" (("="++) . showDate) (tdate2 t)
      status | tstatus t == Just Cleared = " *"
             | tstatus t == Just Pending = " !"
             | otherwise            = ""
      code = if T.length (tcode t) > 0 then printf " (%s)" $ T.unpack $ tcode t else ""
      desc = if null d then "" else " " ++ d where d = T.unpack $ tdescription t
      (samelinecomment, newlinecomments) =
        case renderCommentLines (tcomment t) of []   -> ("",[])
                                                c:cs -> (c,cs)

-- Render a transaction or posting's comment as indented, semicolon-prefixed comment lines.
renderCommentLines :: Text -> [String]
renderCommentLines t  = case lines $ T.unpack t of ("":ls) -> "":map commentprefix ls
                                                   ls      -> map commentprefix ls
    where
      commentprefix = indent . ("; "++)

-- -- Render a transaction or posting's comment as semicolon-prefixed comment lines -
-- -- an inline (same-line) comment if it's a single line, otherwise multiple indented lines.
-- commentLines' :: String -> (String, [String])
-- commentLines' s
--     | null s = ("", [])
--     | length ls == 1 = (prefix $ head ls, [])
--     | otherwise = ("", (prefix $ head ls):(map prefix $ tail ls))
--     where
--       ls = lines s
--       prefix = indent . (";"++)

postingsAsLines :: Bool -> Bool -> Transaction -> [Posting] -> [String]
postingsAsLines elide onelineamounts t ps
    | elide && length ps > 1 && isTransactionBalanced Nothing t -- imprecise balanced check
       = (concatMap (postingAsLines False onelineamounts ps) $ init ps) ++ postingAsLines True onelineamounts ps (last ps)
    | otherwise = concatMap (postingAsLines False onelineamounts ps) ps

postingAsLines :: Bool -> Bool -> [Posting] -> Posting -> [String]
postingAsLines elideamount onelineamounts ps p = concat [
    postingblock
    ++ newlinecomments
    | postingblock <- postingblocks]
  where
    postingblocks = [map rstrip $ lines $ concatTopPadded [account, "  ", amount, assertion, samelinecomment] | amount <- shownAmounts]
    assertion = maybe "" ((" = " ++) . showAmountWithZeroCommodity) $ pbalanceassertion p
    account =
      indent $
        showstatus p ++ fitString (Just acctwidth) Nothing False True (showAccountName Nothing (ptype p) (paccount p))
        where
          showstatus p = if pstatus p == Just Cleared then "* " else ""
          acctwidth = maximum $ map (textWidth . paccount) ps

    -- currently prices are considered part of the amount string when right-aligning amounts
    shownAmounts
      | elideamount    = [""]
      | onelineamounts = [fitString (Just amtwidth) Nothing False False $ showMixedAmountOneLine $ pamount p]
      | otherwise      = map (fitStringMulti (Just amtwidth) Nothing False False . showAmount ) . amounts $ pamount p
      where
        amtwidth = maximum $ 12 : map (strWidth . showMixedAmount . pamount) ps  -- min. 12 for backwards compatibility

    (samelinecomment, newlinecomments) =
      case renderCommentLines (pcomment p) of []   -> ("",[])
                                              c:cs -> (c,cs)


-- used in balance assertion error
showPostingLine p =
  indent $
  if pstatus p == Just Cleared then "* " else "" ++
  showAccountName Nothing (ptype p) (paccount p) ++
  "    " ++
  showMixedAmountOneLine (pamount p)

-- | Produce posting line with all comment lines associated with it
showPostingLines :: Posting -> [String]
showPostingLines p = postingAsLines False False ps p where
    ps | Just t <- ptransaction p = tpostings t
       | otherwise = [p]

tests_postingAsLines = [
   "postingAsLines" ~: do
    let p `gives` ls = assertEqual (show p) ls (postingAsLines False False [p] p)
    posting `gives` []
    posting{
      pstatus=Just Cleared,
      paccount="a",
      pamount=Mixed [usd 1, hrs 2],
      pcomment="pcomment1\npcomment2\n  tag3: val3  \n",
      ptype=RegularPosting,
      ptags=[("ptag1","val1"),("ptag2","val2")]
      }
     `gives` [
      "    * a         $1.00    ; pcomment1",
      "    ; pcomment2",
      "    ;   tag3: val3  ",
      "    * a         2.00h    ; pcomment1",
      "    ; pcomment2",
      "    ;   tag3: val3  "
      ]
 ]

tests_inference = [
    "inferBalancingAmount" ~: do
    let p `gives` p' = assertEqual (show p) (Right p') $ inferTransaction p
        inferTransaction :: Transaction -> Either String Transaction
        inferTransaction = runIdentity . runExceptT . inferBalancingAmount (\_ _ -> return ())
    nulltransaction `gives` nulltransaction
    nulltransaction{
      tpostings=[
        "a" `post` usd (-5),
        "b" `post` missingamt
      ]}
      `gives`
      nulltransaction{
        tpostings=[
          "a" `post` usd (-5),
          "b" `post` usd 5
        ]}
    nulltransaction{
      tpostings=[
        "a" `post` usd (-5),
        "b" `post` (eur 3 @@ usd 4),
        "c" `post` missingamt
      ]}
      `gives`
      nulltransaction{
        tpostings=[
          "a" `post` usd (-5),
          "b" `post` (eur 3 @@ usd 4),
          "c" `post` usd 1
        ]}
 ]

indent :: String -> String
indent = ("    "++)

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
assignmentPostings = filter isAssignment . tpostings

virtualPostings :: Transaction -> [Posting]
virtualPostings = filter isVirtual . tpostings

balancedVirtualPostings :: Transaction -> [Posting]
balancedVirtualPostings = filter isBalancedVirtual . tpostings

transactionsPostings :: [Transaction] -> [Posting]
transactionsPostings = concat . map tpostings

-- | Get the sums of a transaction's real, virtual, and balanced virtual postings.
transactionPostingBalances :: Transaction -> (MixedAmount,MixedAmount,MixedAmount)
transactionPostingBalances t = (sumPostings $ realPostings t
                               ,sumPostings $ virtualPostings t
                               ,sumPostings $ balancedVirtualPostings t)

-- | Is this transaction balanced ? A balanced transaction's real
-- (non-virtual) postings sum to 0, and any balanced virtual postings
-- also sum to 0.
isTransactionBalanced :: Maybe (Map.Map CommoditySymbol AmountStyle) -> Transaction -> Bool
isTransactionBalanced styles t =
    -- isReallyZeroMixedAmountCost rsum && isReallyZeroMixedAmountCost bvsum
    isZeroMixedAmount rsum' && isZeroMixedAmount bvsum'
    where
      (rsum, _, bvsum) = transactionPostingBalances t
      rsum'  = canonicalise $ costOfMixedAmount rsum
      bvsum' = canonicalise $ costOfMixedAmount bvsum
      canonicalise = maybe id canonicaliseMixedAmount styles

-- | Ensure this transaction is balanced, possibly inferring a missing
-- amount or conversion price(s), or return an error message.
-- Balancing is affected by commodity display precisions, so those can
-- (optionally) be provided.
-- 
-- this fails for example, if there are several missing amounts
-- (possibly with balance assignments)
balanceTransaction :: Maybe (Map.Map CommoditySymbol AmountStyle)
                   -> Transaction -> Either String Transaction
balanceTransaction stylemap = runIdentity . runExceptT
  . balanceTransactionUpdate (\_ _ -> return ()) stylemap


-- | More general version of 'balanceTransaction' that takes an update
-- function
balanceTransactionUpdate :: MonadError String m
  => (AccountName -> MixedAmount -> m ())
     -- ^ update function
  -> Maybe (Map.Map CommoditySymbol AmountStyle)
  -> Transaction -> m Transaction
balanceTransactionUpdate update styles t =
  finalize =<< inferBalancingAmount update t
  where
    finalize t' = let t'' = inferBalancingPrices t'
                  in if isTransactionBalanced styles t''
                     then return $ txnTieKnot t''
                     else throwError $ printerr $ nonzerobalanceerror t''
    printerr s = intercalate "\n" [s, showTransactionUnelided t]
    nonzerobalanceerror :: Transaction -> String
    nonzerobalanceerror t = printf "could not balance this transaction (%s%s%s)" rmsg sep bvmsg
        where
          (rsum, _, bvsum) = transactionPostingBalances t
          rmsg | isReallyZeroMixedAmountCost rsum = ""
               | otherwise = "real postings are off by "
                 ++ showMixedAmount (costOfMixedAmount rsum)
          bvmsg | isReallyZeroMixedAmountCost bvsum = ""
                | otherwise = "balanced virtual postings are off by "
                  ++ showMixedAmount (costOfMixedAmount bvsum)
          sep = if not (null rmsg) && not (null bvmsg) then "; " else "" :: String

-- | Infer up to one missing amount for this transactions's real postings, and
-- likewise for its balanced virtual postings, if needed; or return an error
-- message if we can't.
--
-- We can infer a missing amount when there are multiple postings and exactly
-- one of them is amountless. If the amounts had price(s) the inferred amount
-- have the same price(s), and will be converted to the price commodity.
inferBalancingAmount :: MonadError String m
                     => (AccountName -> MixedAmount -> m ())
                     -- ^ update function
                     -> Transaction -> m Transaction
inferBalancingAmount update t@Transaction{tpostings=ps}
  | length amountlessrealps > 1
      = throwError $ printerr "could not balance this transaction - can't have more than one real posting with no amount (remember to put 2 or more spaces before amounts)"
  | length amountlessbvps > 1
      = throwError $ printerr "could not balance this transaction - can't have more than one balanced virtual posting with no amount (remember to put 2 or more spaces before amounts)"
  | otherwise
      = do postings <- mapM inferamount ps
           return t{tpostings=postings}
  where
    printerr s = intercalate "\n" [s, showTransactionUnelided t]
    (amountfulrealps, amountlessrealps) = partition hasAmount (realPostings t)
    realsum = sumStrict $ map pamount amountfulrealps
    (amountfulbvps, amountlessbvps) = partition hasAmount (balancedVirtualPostings t)
    bvsum = sumStrict $ map pamount amountfulbvps
    inferamount p@Posting{ptype=RegularPosting}
     | not (hasAmount p) = updateAmount p realsum
    inferamount p@Posting{ptype=BalancedVirtualPosting}
     | not (hasAmount p) = updateAmount p bvsum
    inferamount p = return p
    updateAmount p amt = update (paccount p) amt' >> return p { pamount=amt', porigin=Just $ originalPosting p }
      where amt' = normaliseMixedAmount $ costOfMixedAmount (-amt)

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
    ps' = map (priceInferrerFor t BalancedVirtualPosting) $
          map (priceInferrerFor t RegularPosting) $
          ps

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
    sumprices      = filter (/=NoPrice) $ map aprice sumamounts
    caninferprices = length sumcommodities == 2 && null sumprices

    inferprice p@Posting{pamount=Mixed [a]}
      | caninferprices && ptype p == pt && acommodity a == fromcommodity
        = p{pamount=Mixed [a{aprice=conversionprice}], porigin=Just $ originalPosting p}
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
            unitprice     = toamount `divideAmount` (aquantity fromamount)
            unitprecision = max 2 ((asprecision $ astyle $ toamount) + (asprecision $ astyle $ fromamount))
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

tests_Hledger_Data_Transaction = TestList $ concat [
  tests_postingAsLines,
  tests_showTransactionUnelided,
  tests_inference,
  [
  "showTransaction" ~: do
     assertEqual "show a balanced transaction, eliding last amount"
       (unlines
        ["2007/01/28 coopportunity"
        ,"    expenses:food:groceries        $47.18"
        ,"    assets:checking"
        ,""
        ])
       (let t = Transaction 0 nullsourcepos (parsedate "2007/01/28") Nothing Nothing "" "coopportunity" "" []
                [posting{paccount="expenses:food:groceries", pamount=Mixed [usd 47.18], ptransaction=Just t}
                ,posting{paccount="assets:checking", pamount=Mixed [usd (-47.18)], ptransaction=Just t}
                ] ""
        in showTransaction t)

  ,"showTransaction" ~: do
     assertEqual "show a balanced transaction, no eliding"
       (unlines
        ["2007/01/28 coopportunity"
        ,"    expenses:food:groceries        $47.18"
        ,"    assets:checking               $-47.18"
        ,""
        ])
       (let t = Transaction 0 nullsourcepos (parsedate "2007/01/28") Nothing Nothing "" "coopportunity" "" []
                [posting{paccount="expenses:food:groceries", pamount=Mixed [usd 47.18], ptransaction=Just t}
                ,posting{paccount="assets:checking", pamount=Mixed [usd (-47.18)], ptransaction=Just t}
                ] ""
        in showTransactionUnelided t)

     -- document some cases that arise in debug/testing:
  ,"showTransaction" ~: do
     assertEqual "show an unbalanced transaction, should not elide"
       (unlines
        ["2007/01/28 coopportunity"
        ,"    expenses:food:groceries        $47.18"
        ,"    assets:checking               $-47.19"
        ,""
        ])
       (showTransaction
        (txnTieKnot $ Transaction 0 nullsourcepos (parsedate "2007/01/28") Nothing Nothing "" "coopportunity" "" []
         [posting{paccount="expenses:food:groceries", pamount=Mixed [usd 47.18]}
         ,posting{paccount="assets:checking", pamount=Mixed [usd (-47.19)]}
         ] ""))

  ,"showTransaction" ~: do
     assertEqual "show an unbalanced transaction with one posting, should not elide"
       (unlines
        ["2007/01/28 coopportunity"
        ,"    expenses:food:groceries        $47.18"
        ,""
        ])
       (showTransaction
        (txnTieKnot $ Transaction 0 nullsourcepos (parsedate "2007/01/28") Nothing Nothing "" "coopportunity" "" []
         [posting{paccount="expenses:food:groceries", pamount=Mixed [usd 47.18]}
         ] ""))

  ,"showTransaction" ~: do
     assertEqual "show a transaction with one posting and a missing amount"
       (unlines
        ["2007/01/28 coopportunity"
        ,"    expenses:food:groceries"
        ,""
        ])
       (showTransaction
        (txnTieKnot $ Transaction 0 nullsourcepos (parsedate "2007/01/28") Nothing Nothing "" "coopportunity" "" []
         [posting{paccount="expenses:food:groceries", pamount=missingmixedamt}
         ] ""))

  ,"showTransaction" ~: do
     assertEqual "show a transaction with a priced commodityless amount"
       (unlines
        ["2010/01/01 x"
        ,"    a        1 @ $2"
        ,"    b"
        ,""
        ])
       (showTransaction
        (txnTieKnot $ Transaction 0 nullsourcepos (parsedate "2010/01/01") Nothing Nothing "" "x" "" []
         [posting{paccount="a", pamount=Mixed [num 1 `at` (usd 2 `withPrecision` 0)]}
         ,posting{paccount="b", pamount= missingmixedamt}
         ] ""))

  ,"balanceTransaction" ~: do
     assertBool "detect unbalanced entry, sign error"
                    (isLeft $ balanceTransaction Nothing
                           (Transaction 0 nullsourcepos (parsedate "2007/01/28") Nothing Nothing "" "test" "" []
                            [posting{paccount="a", pamount=Mixed [usd 1]}
                            ,posting{paccount="b", pamount=Mixed [usd 1]}
                            ] ""))

     assertBool "detect unbalanced entry, multiple missing amounts"
                    (isLeft $ balanceTransaction Nothing
                           (Transaction 0 nullsourcepos (parsedate "2007/01/28") Nothing Nothing "" "test" "" []
                            [posting{paccount="a", pamount=missingmixedamt}
                            ,posting{paccount="b", pamount=missingmixedamt}
                            ] ""))

     let e = balanceTransaction Nothing (Transaction 0 nullsourcepos (parsedate "2007/01/28") Nothing Nothing "" "" "" []
                           [posting{paccount="a", pamount=Mixed [usd 1]}
                           ,posting{paccount="b", pamount=missingmixedamt}
                           ] "")
     assertBool "balanceTransaction allows one missing amount" (isRight e)
     assertEqual "balancing amount is inferred"
                     (Mixed [usd (-1)])
                     (case e of
                        Right e' -> (pamount $ last $ tpostings e')
                        Left _ -> error' "should not happen")

     let e = balanceTransaction Nothing (Transaction 0 nullsourcepos (parsedate "2011/01/01") Nothing Nothing "" "" "" []
                           [posting{paccount="a", pamount=Mixed [usd 1.35]}
                           ,posting{paccount="b", pamount=Mixed [eur (-1)]}
                           ] "")
     assertBool "balanceTransaction can infer conversion price" (isRight e)
     assertEqual "balancing conversion price is inferred"
                     (Mixed [usd 1.35 @@ (eur 1 `withPrecision` maxprecision)])
                     (case e of
                        Right e' -> (pamount $ head $ tpostings e')
                        Left _ -> error' "should not happen")

     assertBool "balanceTransaction balances based on cost if there are unit prices" (isRight $
       balanceTransaction Nothing (Transaction 0 nullsourcepos (parsedate "2011/01/01") Nothing Nothing "" "" "" []
                           [posting{paccount="a", pamount=Mixed [usd 1 `at` eur 2]}
                           ,posting{paccount="a", pamount=Mixed [usd (-2) `at` eur 1]}
                           ] ""))

     assertBool "balanceTransaction balances based on cost if there are total prices" (isRight $
       balanceTransaction Nothing (Transaction 0 nullsourcepos (parsedate "2011/01/01") Nothing Nothing "" "" "" []
                           [posting{paccount="a", pamount=Mixed [usd 1    @@ eur 1]}
                           ,posting{paccount="a", pamount=Mixed [usd (-2) @@ eur 1]}
                           ] ""))

  ,"isTransactionBalanced" ~: do
     let t = Transaction 0 nullsourcepos (parsedate "2009/01/01") Nothing Nothing "" "a" "" []
             [posting{paccount="b", pamount=Mixed [usd 1.00], ptransaction=Just t}
             ,posting{paccount="c", pamount=Mixed [usd (-1.00)], ptransaction=Just t}
             ] ""
     assertBool "detect balanced" (isTransactionBalanced Nothing t)
     let t = Transaction 0 nullsourcepos (parsedate "2009/01/01") Nothing Nothing "" "a" "" []
             [posting{paccount="b", pamount=Mixed [usd 1.00], ptransaction=Just t}
             ,posting{paccount="c", pamount=Mixed [usd (-1.01)], ptransaction=Just t}
             ] ""
     assertBool "detect unbalanced" (not $ isTransactionBalanced Nothing t)
     let t = Transaction 0 nullsourcepos (parsedate "2009/01/01") Nothing Nothing "" "a" "" []
             [posting{paccount="b", pamount=Mixed [usd 1.00], ptransaction=Just t}
             ] ""
     assertBool "detect unbalanced, one posting" (not $ isTransactionBalanced Nothing t)
     let t = Transaction 0 nullsourcepos (parsedate "2009/01/01") Nothing Nothing "" "a" "" []
             [posting{paccount="b", pamount=Mixed [usd 0], ptransaction=Just t}
             ] ""
     assertBool "one zero posting is considered balanced for now" (isTransactionBalanced Nothing t)
     let t = Transaction 0 nullsourcepos (parsedate "2009/01/01") Nothing Nothing "" "a" "" []
             [posting{paccount="b", pamount=Mixed [usd 1.00], ptransaction=Just t}
             ,posting{paccount="c", pamount=Mixed [usd (-1.00)], ptransaction=Just t}
             ,posting{paccount="d", pamount=Mixed [usd 100], ptype=VirtualPosting, ptransaction=Just t}
             ] ""
     assertBool "virtual postings don't need to balance" (isTransactionBalanced Nothing t)
     let t = Transaction 0 nullsourcepos (parsedate "2009/01/01") Nothing Nothing "" "a" "" []
             [posting{paccount="b", pamount=Mixed [usd 1.00], ptransaction=Just t}
             ,posting{paccount="c", pamount=Mixed [usd (-1.00)], ptransaction=Just t}
             ,posting{paccount="d", pamount=Mixed [usd 100], ptype=BalancedVirtualPosting, ptransaction=Just t}
             ] ""
     assertBool "balanced virtual postings need to balance among themselves" (not $ isTransactionBalanced Nothing t)
     let t = Transaction 0 nullsourcepos (parsedate "2009/01/01") Nothing Nothing "" "a" "" []
             [posting{paccount="b", pamount=Mixed [usd 1.00], ptransaction=Just t}
             ,posting{paccount="c", pamount=Mixed [usd (-1.00)], ptransaction=Just t}
             ,posting{paccount="d", pamount=Mixed [usd 100], ptype=BalancedVirtualPosting, ptransaction=Just t}
             ,posting{paccount="3", pamount=Mixed [usd (-100)], ptype=BalancedVirtualPosting, ptransaction=Just t}
             ] ""
     assertBool "balanced virtual postings need to balance among themselves (2)" (isTransactionBalanced Nothing t)

  ]]
