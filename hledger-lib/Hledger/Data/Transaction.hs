{-|

A 'Transaction' represents a movement of some commodity(ies) between two
or more accounts. It consists of multiple account 'Posting's which balance
to zero, a date, and optional extras like description, cleared status, and
tags.

-}

module Hledger.Data.Transaction (
  -- * Transaction
  nulltransaction,
  txnTieKnot,
  -- settxn,
  -- * operations
  showAccountName,
  hasRealPostings,
  realPostings,
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
  -- * rendering
  showTransaction,
  showTransactionUnelided,
  -- * misc.
  tests_Hledger_Data_Transaction
)
where
import Data.List
import Data.Maybe
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
    show t = "= " ++ mtvalueexpr t ++ "\n" ++ unlines (map show (mtpostings t))

instance Show PeriodicTransaction where 
    show t = "~ " ++ ptperiodicexpr t ++ "\n" ++ unlines (map show (ptpostings t))

nulltransaction :: Transaction
nulltransaction = Transaction {
                    tdate=nulldate,
                    tdate2=Nothing,
                    tstatus=False, 
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
showTransaction = showTransaction' True

showTransactionUnelided :: Transaction -> String
showTransactionUnelided = showTransaction' False

tests_showTransactionUnelided = [
   "showTransactionUnelided" ~: do
    let t `gives` s = assertEqual "" s (showTransactionUnelided t)
    nulltransaction `gives` "0000/01/01\n\n"
    nulltransaction{
      tdate=parsedate "2012/05/14",
      tdate2=Just $ parsedate "2012/05/15",
      tstatus=False,
      tcode="code",
      tdescription="desc",
      tcomment="tcomment1\ntcomment2\n",
      ttags=[("ttag1","val1")],
      tpostings=[
        nullposting{
          pstatus=True,
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
      "                $1.00",
      "    * a          2.0h",
      "    ; pcomment2",
      ""
      ]
 ]

-- XXX overlaps showPosting
showTransaction' :: Bool -> Transaction -> String
showTransaction' elide t =
    unlines $ [descriptionline]
              ++ newlinecomments
              ++ (postingsAsLines elide t (tpostings t))
              ++ [""]
    where
      descriptionline = rstrip $ concat [date, status, code, desc, samelinecomment]
      date = showdate (tdate t) ++ maybe "" showedate (tdate2 t)
      showdate = printf "%-10s" . showDate
      showedate = printf "=%s" . showdate
      status = if tstatus t then " *" else ""
      code = if length (tcode t) > 0 then printf " (%s)" $ tcode t else ""
      desc = if null d then "" else " " ++ d where d = tdescription t
      (samelinecomment, newlinecomments) =
        case renderCommentLines (tcomment t) of []   -> ("",[])
                                                c:cs -> (c,cs)

-- Render a transaction or posting's comment as indented, semicolon-prefixed comment lines.
renderCommentLines :: String -> [String]
renderCommentLines s  = case lines s of ("":ls) -> "":map commentprefix ls
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

postingsAsLines :: Bool -> Transaction -> [Posting] -> [String]
postingsAsLines elide t ps
    | elide && length ps > 1 && isTransactionBalanced Nothing t -- imprecise balanced check
       = (concatMap (postingAsLines False ps) $ init ps) ++ postingAsLines True ps (last ps)
    | otherwise = concatMap (postingAsLines False ps) ps

postingAsLines :: Bool -> [Posting] -> Posting -> [String]
postingAsLines elideamount ps p =
    postinglines
    ++ newlinecomments
  where
    postinglines = map rstrip $ lines $ concatTopPadded [showacct p, "  ", amount, samelinecomment]
    amount = if elideamount then "" else showamt (pamount p)
    (samelinecomment, newlinecomments) =
      case renderCommentLines (pcomment p) of []   -> ("",[])
                                              c:cs -> (c,cs)
    showacct p =
      indent $ showstatus p ++ printf (printf "%%-%ds" w) (showAccountName Nothing (ptype p) (paccount p))
        where
          showstatus p = if pstatus p then "* " else ""
          w = maximum $ map (length . paccount) ps
    showamt =
        padleft 12 . showMixedAmount

tests_postingAsLines = [
   "postingAsLines" ~: do
    let p `gives` ls = assertEqual "" ls (postingAsLines False [p] p)
    posting `gives` ["                 0"]
    posting{
      pstatus=True,
      paccount="a",
      pamount=Mixed [usd 1, hrs 2],
      pcomment="pcomment1\npcomment2\n  tag3: val3  \n",
      ptype=RegularPosting,
      ptags=[("ptag1","val1"),("ptag2","val2")]
      }
     `gives` [
      "                $1.00",
      "    * a          2.0h    ; pcomment1",
      "    ; pcomment2",
      "    ;   tag3: val3  "
      ]
 ]

indent :: String -> String
indent = ("    "++)

-- | Show an account name, clipped to the given width if any, and
-- appropriately bracketed/parenthesised for the given posting type.
showAccountName :: Maybe Int -> PostingType -> AccountName -> String
showAccountName w = fmt
    where
      fmt RegularPosting = take w'
      fmt VirtualPosting = parenthesise . reverse . take (w'-2) . reverse
      fmt BalancedVirtualPosting = bracket . reverse . take (w'-2) . reverse
      w' = fromMaybe 999999 w
      parenthesise s = "("++s++")"
      bracket s = "["++s++"]"

hasRealPostings :: Transaction -> Bool
hasRealPostings = not . null . realPostings

realPostings :: Transaction -> [Posting]
realPostings = filter isReal . tpostings

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
isTransactionBalanced :: Maybe (Map.Map Commodity AmountStyle) -> Transaction -> Bool
isTransactionBalanced styles t =
    -- isReallyZeroMixedAmountCost rsum && isReallyZeroMixedAmountCost bvsum
    isZeroMixedAmount rsum' && isZeroMixedAmount bvsum'
    where
      (rsum, _, bvsum) = transactionPostingBalances t
      rsum'  = canonicalise $ costOfMixedAmount rsum
      bvsum' = canonicalise $ costOfMixedAmount bvsum
      canonicalise = maybe id canonicaliseMixedAmount styles

-- | Ensure this transaction is balanced, possibly inferring a missing
-- amount or conversion price, or return an error message.
--
-- Balancing is affected by commodity display precisions, so those may
-- be provided.
--
-- We can infer a missing real amount when there are multiple real
-- postings and exactly one of them is amountless (likewise for
-- balanced virtual postings). Inferred amounts are converted to cost
-- basis when possible.
--
-- We can infer a conversion price when all real amounts are specified
-- and the sum of real postings' amounts is exactly two
-- non-explicitly-priced amounts in different commodities (likewise
-- for balanced virtual postings).
balanceTransaction :: Maybe (Map.Map Commodity AmountStyle) -> Transaction -> Either String Transaction
balanceTransaction styles t@Transaction{tpostings=ps}
    | length rwithoutamounts > 1 || length bvwithoutamounts > 1
        = Left $ printerr "could not balance this transaction (can't have more than one missing amount; remember to put 2 or more spaces before amounts)"
    | not $ isTransactionBalanced styles t''' = Left $ printerr $ nonzerobalanceerror t'''
    | otherwise = Right t''''
    where
      -- maybe infer missing amounts
      (rwithamounts, rwithoutamounts)   = partition hasAmount $ realPostings t
      (bvwithamounts, bvwithoutamounts) = partition hasAmount $ balancedVirtualPostings t
      ramounts  = map pamount rwithamounts
      bvamounts = map pamount bvwithamounts
      t' = t{tpostings=map inferamount ps}
          where 
            inferamount p | not (hasAmount p) && isReal p            = p{pamount = costOfMixedAmount (- sum ramounts)}
                          | not (hasAmount p) && isBalancedVirtual p = p{pamount = costOfMixedAmount (- sum bvamounts)}
                          | otherwise                             = p

      -- maybe infer conversion prices, for real postings
      rmixedamountsinorder = map pamount $ realPostings t'
      ramountsinorder = concatMap amounts rmixedamountsinorder
      rcommoditiesinorder  = map acommodity ramountsinorder
      rsumamounts  = amounts $ sum rmixedamountsinorder
      -- assumption: the sum of mixed amounts is normalised (one simple amount per commodity)
      t'' = if length rsumamounts == 2 && all ((==NoPrice).aprice) rsumamounts && t'==t
             then t'{tpostings=map inferprice ps}
             else t'
          where
            -- assumption: a posting's mixed amount contains one simple amount
            inferprice p@Posting{pamount=Mixed [a@Amount{acommodity=c,aprice=NoPrice}], ptype=RegularPosting}
                = p{pamount=Mixed [a{aprice=conversionprice c}]}
                where
                  conversionprice c | c == unpricedcommodity
                                        -- assign a balancing price. Use @@ for more exact output when possible.
                                        -- invariant: prices should always be positive. Enforced with "abs"
                                        = if length ramountsinunpricedcommodity == 1
                                           then TotalPrice $ abs targetcommodityamount `withPrecision` maxprecision
                                           else UnitPrice $ abs (targetcommodityamount `divideAmount` (aquantity unpricedamount)) `withPrecision` maxprecision
                                    | otherwise = NoPrice
                      where
                        unpricedcommodity     = head $ filter (`elem` (map acommodity rsumamounts)) rcommoditiesinorder
                        unpricedamount        = head $ filter ((==unpricedcommodity).acommodity) rsumamounts
                        targetcommodityamount = head $ filter ((/=unpricedcommodity).acommodity) rsumamounts
                        ramountsinunpricedcommodity = filter ((==unpricedcommodity).acommodity) ramountsinorder
            inferprice p = p

      -- maybe infer prices for balanced virtual postings. Just duplicates the above for now.
      bvmixedamountsinorder = map pamount $ balancedVirtualPostings t''
      bvamountsinorder = concatMap amounts bvmixedamountsinorder
      bvcommoditiesinorder  = map acommodity bvamountsinorder
      bvsumamounts  = amounts $ sum bvmixedamountsinorder
      t''' = if length bvsumamounts == 2 && all ((==NoPrice).aprice) bvsumamounts && t'==t -- XXX could check specifically for bv amount inferring
             then t''{tpostings=map inferprice ps}
             else t''
          where
            inferprice p@Posting{pamount=Mixed [a@Amount{acommodity=c,aprice=NoPrice}], ptype=BalancedVirtualPosting}
                = p{pamount=Mixed [a{aprice=conversionprice c}]}
                where
                  conversionprice c | c == unpricedcommodity
                                        = if length bvamountsinunpricedcommodity == 1
                                           then TotalPrice $ abs targetcommodityamount `withPrecision` maxprecision
                                           else UnitPrice $ abs (targetcommodityamount `divideAmount` (aquantity unpricedamount)) `withPrecision` maxprecision
                                    | otherwise = NoPrice
                      where
                        unpricedcommodity     = head $ filter (`elem` (map acommodity bvsumamounts)) bvcommoditiesinorder
                        unpricedamount        = head $ filter ((==unpricedcommodity).acommodity) bvsumamounts
                        targetcommodityamount = head $ filter ((/=unpricedcommodity).acommodity) bvsumamounts
                        bvamountsinunpricedcommodity = filter ((==unpricedcommodity).acommodity) bvamountsinorder
            inferprice p = p

      -- tie the knot so eg relatedPostings works right
      t'''' = txnTieKnot t'''

      printerr s = intercalate "\n" [s, showTransactionUnelided t]

nonzerobalanceerror :: Transaction -> String
nonzerobalanceerror t = printf "could not balance this transaction (%s%s%s)" rmsg sep bvmsg
    where
      (rsum, _, bvsum) = transactionPostingBalances t
      rmsg | isReallyZeroMixedAmountCost rsum = ""
           | otherwise = "real postings are off by " ++ showMixedAmount (costOfMixedAmount rsum)
      bvmsg | isReallyZeroMixedAmountCost bvsum = ""
            | otherwise = "balanced virtual postings are off by " ++ showMixedAmount (costOfMixedAmount bvsum)
      sep = if not (null rmsg) && not (null bvmsg) then "; " else "" :: String

-- Get a transaction's secondary date, defaulting to the primary date.
transactionDate2 :: Transaction -> Day
transactionDate2 t = fromMaybe (tdate t) $ tdate2 t

-- | Ensure a transaction's postings refer back to it.
txnTieKnot :: Transaction -> Transaction
txnTieKnot t@Transaction{tpostings=ps} = t{tpostings=map (settxn t) ps}

-- | Set a posting's parent transaction.
settxn :: Transaction -> Posting -> Posting
settxn t p = p{ptransaction=Just t}

tests_Hledger_Data_Transaction = TestList $ concat [
  tests_postingAsLines,
  tests_showTransactionUnelided,
  [
  "showTransaction" ~: do
     assertEqual "show a balanced transaction, eliding last amount"
       (unlines
        ["2007/01/28 coopportunity"
        ,"    expenses:food:groceries        $47.18"
        ,"    assets:checking"
        ,""
        ])
       (let t = Transaction (parsedate "2007/01/28") Nothing False "" "coopportunity" "" []
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
       (let t = Transaction (parsedate "2007/01/28") Nothing False "" "coopportunity" "" []
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
        (txnTieKnot $ Transaction (parsedate "2007/01/28") Nothing False "" "coopportunity" "" []
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
        (txnTieKnot $ Transaction (parsedate "2007/01/28") Nothing False "" "coopportunity" "" []
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
        (txnTieKnot $ Transaction (parsedate "2007/01/28") Nothing False "" "coopportunity" "" []
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
        (txnTieKnot $ Transaction (parsedate "2010/01/01") Nothing False "" "x" "" []
         [posting{paccount="a", pamount=Mixed [num 1 `at` (usd 2 `withPrecision` 0)]}
         ,posting{paccount="b", pamount= missingmixedamt}
         ] ""))

  ,"balanceTransaction" ~: do
     assertBool "detect unbalanced entry, sign error"
                    (isLeft $ balanceTransaction Nothing
                           (Transaction (parsedate "2007/01/28") Nothing False "" "test" "" []
                            [posting{paccount="a", pamount=Mixed [usd 1]}
                            ,posting{paccount="b", pamount=Mixed [usd 1]}
                            ] ""))

     assertBool "detect unbalanced entry, multiple missing amounts"
                    (isLeft $ balanceTransaction Nothing
                           (Transaction (parsedate "2007/01/28") Nothing False "" "test" "" []
                            [posting{paccount="a", pamount=missingmixedamt}
                            ,posting{paccount="b", pamount=missingmixedamt}
                            ] ""))

     let e = balanceTransaction Nothing (Transaction (parsedate "2007/01/28") Nothing False "" "" "" []
                           [posting{paccount="a", pamount=Mixed [usd 1]}
                           ,posting{paccount="b", pamount=missingmixedamt}
                           ] "")
     assertBool "balanceTransaction allows one missing amount" (isRight e)
     assertEqual "balancing amount is inferred"
                     (Mixed [usd (-1)])
                     (case e of
                        Right e' -> (pamount $ last $ tpostings e')
                        Left _ -> error' "should not happen")

     let e = balanceTransaction Nothing (Transaction (parsedate "2011/01/01") Nothing False "" "" "" []
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
       balanceTransaction Nothing (Transaction (parsedate "2011/01/01") Nothing False "" "" "" []
                           [posting{paccount="a", pamount=Mixed [usd 1 `at` eur 2]}
                           ,posting{paccount="a", pamount=Mixed [usd (-2) `at` eur 1]}
                           ] ""))

     assertBool "balanceTransaction balances based on cost if there are total prices" (isRight $
       balanceTransaction Nothing (Transaction (parsedate "2011/01/01") Nothing False "" "" "" []
                           [posting{paccount="a", pamount=Mixed [usd 1    @@ eur 1]}
                           ,posting{paccount="a", pamount=Mixed [usd (-2) @@ eur 1]}
                           ] ""))

  ,"isTransactionBalanced" ~: do
     let t = Transaction (parsedate "2009/01/01") Nothing False "" "a" "" []
             [posting{paccount="b", pamount=Mixed [usd 1.00], ptransaction=Just t}
             ,posting{paccount="c", pamount=Mixed [usd (-1.00)], ptransaction=Just t}
             ] ""
     assertBool "detect balanced" (isTransactionBalanced Nothing t)
     let t = Transaction (parsedate "2009/01/01") Nothing False "" "a" "" []
             [posting{paccount="b", pamount=Mixed [usd 1.00], ptransaction=Just t}
             ,posting{paccount="c", pamount=Mixed [usd (-1.01)], ptransaction=Just t}
             ] ""
     assertBool "detect unbalanced" (not $ isTransactionBalanced Nothing t)
     let t = Transaction (parsedate "2009/01/01") Nothing False "" "a" "" []
             [posting{paccount="b", pamount=Mixed [usd 1.00], ptransaction=Just t}
             ] ""
     assertBool "detect unbalanced, one posting" (not $ isTransactionBalanced Nothing t)
     let t = Transaction (parsedate "2009/01/01") Nothing False "" "a" "" []
             [posting{paccount="b", pamount=Mixed [usd 0], ptransaction=Just t}
             ] ""
     assertBool "one zero posting is considered balanced for now" (isTransactionBalanced Nothing t)
     let t = Transaction (parsedate "2009/01/01") Nothing False "" "a" "" []
             [posting{paccount="b", pamount=Mixed [usd 1.00], ptransaction=Just t}
             ,posting{paccount="c", pamount=Mixed [usd (-1.00)], ptransaction=Just t}
             ,posting{paccount="d", pamount=Mixed [usd 100], ptype=VirtualPosting, ptransaction=Just t}
             ] ""
     assertBool "virtual postings don't need to balance" (isTransactionBalanced Nothing t)
     let t = Transaction (parsedate "2009/01/01") Nothing False "" "a" "" []
             [posting{paccount="b", pamount=Mixed [usd 1.00], ptransaction=Just t}
             ,posting{paccount="c", pamount=Mixed [usd (-1.00)], ptransaction=Just t}
             ,posting{paccount="d", pamount=Mixed [usd 100], ptype=BalancedVirtualPosting, ptransaction=Just t}
             ] ""
     assertBool "balanced virtual postings need to balance among themselves" (not $ isTransactionBalanced Nothing t)
     let t = Transaction (parsedate "2009/01/01") Nothing False "" "a" "" []
             [posting{paccount="b", pamount=Mixed [usd 1.00], ptransaction=Just t}
             ,posting{paccount="c", pamount=Mixed [usd (-1.00)], ptransaction=Just t}
             ,posting{paccount="d", pamount=Mixed [usd 100], ptype=BalancedVirtualPosting, ptransaction=Just t}
             ,posting{paccount="3", pamount=Mixed [usd (-100)], ptype=BalancedVirtualPosting, ptransaction=Just t}
             ] ""
     assertBool "balanced virtual postings need to balance among themselves (2)" (isTransactionBalanced Nothing t)

  ]]
