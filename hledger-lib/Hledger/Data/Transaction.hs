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
  transactionActualDate,
  transactionEffectiveDate,
  journalTransactionWithDate,
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
                    teffectivedate=Nothing, 
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
      teffectivedate=Just $ parsedate "2012/05/15",
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
          pcomment="pcomment1\npcomment2\n",
          ptype=RegularPosting,
          ptags=[("ptag1","val1"),("ptag2","val2")]
          }
       ]
      }
      `gives` unlines [
      "2012/05/14=2012/05/15 (code) desc  ; tcomment1",
      "    ; tcomment2",
      "    ; ttag1: val1",
      "                $1.00",
      "    * a          2.0h  ; pcomment1",
      "    ; pcomment2",
      "    ; ptag1: val1",
      "    ; ptag2: val2",
      ""
      ]
 ]

-- XXX overlaps showPosting
showTransaction' :: Bool -> Transaction -> String
showTransaction' elide t =
    unlines $ [descriptionline]
              ++ commentlines
              ++ (tagsAsLines $ ttags t)
              ++ (postingsAsLines elide t (tpostings t))
              ++ [""]
    where
      descriptionline = rstrip $ concat [date, status, code, desc, firstcomment]
      date = showdate (tdate t) ++ maybe "" showedate (teffectivedate t)
      showdate = printf "%-10s" . showDate
      showedate = printf "=%s" . showdate
      status = if tstatus t then " *" else ""
      code = if length (tcode t) > 0 then printf " (%s)" $ tcode t else ""
      desc = if null d then "" else " " ++ d where d = tdescription t
      (firstcomment, commentlines) = commentLines $ tcomment t

-- Render a transaction or posting's comment as indented & prefixed comment lines.
commentLines :: String -> (String, [String])
commentLines s
    | null s = ("", [])
    | otherwise = ("  ; " ++ first, map (indent . ("; "++)) rest)
    where (first:rest) = lines s

postingsAsLines :: Bool -> Transaction -> [Posting] -> [String]
postingsAsLines elide t ps
    | elide && length ps > 1 && isTransactionBalanced Nothing t -- imprecise balanced check
       = (concatMap (postingAsLines False ps) $ init ps) ++ postingAsLines True ps (last ps)
    | otherwise = concatMap (postingAsLines False ps) ps

postingAsLines :: Bool -> [Posting] -> Posting -> [String]
postingAsLines elideamount ps p =
    postinglines
    ++ commentlines
    ++ tagsAsLines (ptags p)
  where
    postinglines = map rstrip $ lines $ concatTopPadded [showacct p, "  ", amount, firstcomment]
    amount = if elideamount then "" else showamt (pamount p)
    (firstcomment, commentlines) = commentLines $ pcomment p
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
    nullposting `gives` ["                 0"]
    nullposting{
      pstatus=True,
      paccount="a",
      pamount=Mixed [usd 1, hrs 2],
      pcomment="pcomment1\npcomment2\n",
      ptype=RegularPosting,
      ptags=[("ptag1","val1"),("ptag2","val2")]
      }
     `gives` [
      "                $1.00",
      "    * a          2.0h  ; pcomment1",
      "    ; pcomment2",
      "    ; ptag1: val1",
      "    ; ptag2: val2"
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
        = Left $ printerr "could not balance this transaction (too many missing amounts)"
    | not $ isTransactionBalanced styles t''' = Left $ printerr $ nonzerobalanceerror t'''
    | otherwise = Right t'''
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
      t'' = if length rsumamounts == 2 && all (isNothing.aprice) rsumamounts && t'==t
             then t'{tpostings=map inferprice ps}
             else t'
          where
            -- assumption: a posting's mixed amount contains one simple amount
            inferprice p@Posting{pamount=Mixed [a@Amount{acommodity=c,aprice=Nothing}], ptype=RegularPosting}
                = p{pamount=Mixed [a{aprice=conversionprice c}]}
                where
                  conversionprice c | c == unpricedcommodity
                                        -- assign a balancing price. Use @@ for more exact output when possible.
                                        -- invariant: prices should always be positive. Enforced with "abs"
                                        = if length ramountsinunpricedcommodity == 1
                                           then Just $ TotalPrice $ Mixed [setAmountPrecision maxprecision $ abs $ targetcommodityamount]
                                           else Just $ UnitPrice $ Mixed [setAmountPrecision maxprecision $ abs $ targetcommodityamount `divideAmount` (aquantity unpricedamount)]
                                    | otherwise = Nothing
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
      t''' = if length bvsumamounts == 2 && all (isNothing.aprice) bvsumamounts && t'==t -- XXX could check specifically for bv amount inferring
             then t''{tpostings=map inferprice ps}
             else t''
          where
            inferprice p@Posting{pamount=Mixed [a@Amount{acommodity=c,aprice=Nothing}], ptype=BalancedVirtualPosting}
                = p{pamount=Mixed [a{aprice=conversionprice c}]}
                where
                  conversionprice c | c == unpricedcommodity
                                        = if length bvamountsinunpricedcommodity == 1
                                           then Just $ TotalPrice $ Mixed [setAmountPrecision maxprecision $ abs $ targetcommodityamount]
                                           else Just $ UnitPrice $ Mixed [setAmountPrecision maxprecision $ abs $ targetcommodityamount `divideAmount` (aquantity unpricedamount)]
                                    | otherwise = Nothing
                      where
                        unpricedcommodity     = head $ filter (`elem` (map acommodity bvsumamounts)) bvcommoditiesinorder
                        unpricedamount        = head $ filter ((==unpricedcommodity).acommodity) bvsumamounts
                        targetcommodityamount = head $ filter ((/=unpricedcommodity).acommodity) bvsumamounts
                        bvamountsinunpricedcommodity = filter ((==unpricedcommodity).acommodity) bvamountsinorder
            inferprice p = p

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

transactionActualDate :: Transaction -> Day
transactionActualDate = tdate

-- Get a transaction's effective date, defaulting to the actual date.
transactionEffectiveDate :: Transaction -> Day
transactionEffectiveDate t = fromMaybe (tdate t) $ teffectivedate t

-- | Once we no longer need both, set the main transaction date to either
-- the actual or effective date. A bit hacky.
journalTransactionWithDate :: WhichDate -> Transaction -> Transaction
journalTransactionWithDate ActualDate t = t
journalTransactionWithDate EffectiveDate t = txnTieKnot t{tdate=transactionEffectiveDate t}

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
                [Posting False "expenses:food:groceries" (Mixed [usd 47.18]) "" RegularPosting [] (Just t)
                ,Posting False "assets:checking" (Mixed [usd (-47.18)]) "" RegularPosting [] (Just t)
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
                [Posting False "expenses:food:groceries" (Mixed [usd 47.18]) "" RegularPosting [] (Just t)
                ,Posting False "assets:checking" (Mixed [usd (-47.18)]) "" RegularPosting [] (Just t)
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
         [Posting False "expenses:food:groceries" (Mixed [usd 47.18]) "" RegularPosting [] Nothing
         ,Posting False "assets:checking" (Mixed [usd (-47.19)]) "" RegularPosting [] Nothing
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
         [Posting False "expenses:food:groceries" (Mixed [usd 47.18]) "" RegularPosting [] Nothing
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
         [Posting False "expenses:food:groceries" missingmixedamt "" RegularPosting [] Nothing
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
         [Posting False "a" (Mixed [amt 1 `at` (setAmountPrecision 0 $ usd 2)]) "" RegularPosting [] Nothing
         ,Posting False "b" missingmixedamt "" RegularPosting [] Nothing
         ] ""))

  ,"balanceTransaction" ~: do
     assertBool "detect unbalanced entry, sign error"
                    (isLeft $ balanceTransaction Nothing
                           (Transaction (parsedate "2007/01/28") Nothing False "" "test" "" []
                            [Posting False "a" (Mixed [usd 1]) "" RegularPosting [] Nothing,
                             Posting False "b" (Mixed [usd 1]) "" RegularPosting [] Nothing
                            ] ""))

     assertBool "detect unbalanced entry, multiple missing amounts"
                    (isLeft $ balanceTransaction Nothing
                           (Transaction (parsedate "2007/01/28") Nothing False "" "test" "" []
                            [Posting False "a" missingmixedamt "" RegularPosting [] Nothing,
                             Posting False "b" missingmixedamt "" RegularPosting [] Nothing
                            ] ""))

     let e = balanceTransaction Nothing (Transaction (parsedate "2007/01/28") Nothing False "" "" "" []
                           [Posting False "a" (Mixed [usd 1]) "" RegularPosting [] Nothing,
                            Posting False "b" missingmixedamt "" RegularPosting [] Nothing
                           ] "")
     assertBool "balanceTransaction allows one missing amount" (isRight e)
     assertEqual "balancing amount is inferred"
                     (Mixed [usd (-1)])
                     (case e of
                        Right e' -> (pamount $ last $ tpostings e')
                        Left _ -> error' "should not happen")

     let e = balanceTransaction Nothing (Transaction (parsedate "2011/01/01") Nothing False "" "" "" []
                           [Posting False "a" (Mixed [usd 1.35]) "" RegularPosting [] Nothing,
                            Posting False "b" (Mixed [eur (-1)]) "" RegularPosting [] Nothing
                           ] "")
     assertBool "balanceTransaction can infer conversion price" (isRight e)
     assertEqual "balancing conversion price is inferred"
                     (Mixed [usd 1.35 @@ (setAmountPrecision maxprecision $ eur 1)])
                     (case e of
                        Right e' -> (pamount $ head $ tpostings e')
                        Left _ -> error' "should not happen")

     assertBool "balanceTransaction balances based on cost if there are unit prices" (isRight $
       balanceTransaction Nothing (Transaction (parsedate "2011/01/01") Nothing False "" "" "" []
                           [Posting False "a" (Mixed [usd 1 `at` eur 2]) "" RegularPosting [] Nothing
                           ,Posting False "a" (Mixed [usd (-2) `at` eur 1]) "" RegularPosting [] Nothing
                           ] ""))

     assertBool "balanceTransaction balances based on cost if there are total prices" (isRight $
       balanceTransaction Nothing (Transaction (parsedate "2011/01/01") Nothing False "" "" "" []
                           [Posting False "a" (Mixed [usd 1    @@ eur 1]) "" RegularPosting [] Nothing
                           ,Posting False "a" (Mixed [usd (-2) @@ eur 1]) "" RegularPosting [] Nothing
                           ] ""))

  ,"isTransactionBalanced" ~: do
     let t = Transaction (parsedate "2009/01/01") Nothing False "" "a" "" []
             [Posting False "b" (Mixed [usd 1.00]) "" RegularPosting [] (Just t)
             ,Posting False "c" (Mixed [usd (-1.00)]) "" RegularPosting [] (Just t)
             ] ""
     assertBool "detect balanced" (isTransactionBalanced Nothing t)
     let t = Transaction (parsedate "2009/01/01") Nothing False "" "a" "" []
             [Posting False "b" (Mixed [usd 1.00]) "" RegularPosting [] (Just t)
             ,Posting False "c" (Mixed [usd (-1.01)]) "" RegularPosting [] (Just t)
             ] ""
     assertBool "detect unbalanced" (not $ isTransactionBalanced Nothing t)
     let t = Transaction (parsedate "2009/01/01") Nothing False "" "a" "" []
             [Posting False "b" (Mixed [usd 1.00]) "" RegularPosting [] (Just t)
             ] ""
     assertBool "detect unbalanced, one posting" (not $ isTransactionBalanced Nothing t)
     let t = Transaction (parsedate "2009/01/01") Nothing False "" "a" "" []
             [Posting False "b" (Mixed [usd 0]) "" RegularPosting [] (Just t)
             ] ""
     assertBool "one zero posting is considered balanced for now" (isTransactionBalanced Nothing t)
     let t = Transaction (parsedate "2009/01/01") Nothing False "" "a" "" []
             [Posting False "b" (Mixed [usd 1.00]) "" RegularPosting [] (Just t)
             ,Posting False "c" (Mixed [usd (-1.00)]) "" RegularPosting [] (Just t)
             ,Posting False "d" (Mixed [usd 100]) "" VirtualPosting [] (Just t)
             ] ""
     assertBool "virtual postings don't need to balance" (isTransactionBalanced Nothing t)
     let t = Transaction (parsedate "2009/01/01") Nothing False "" "a" "" []
             [Posting False "b" (Mixed [usd 1.00]) "" RegularPosting [] (Just t)
             ,Posting False "c" (Mixed [usd (-1.00)]) "" RegularPosting [] (Just t)
             ,Posting False "d" (Mixed [usd 100]) "" BalancedVirtualPosting [] (Just t)
             ] ""
     assertBool "balanced virtual postings need to balance among themselves" (not $ isTransactionBalanced Nothing t)
     let t = Transaction (parsedate "2009/01/01") Nothing False "" "a" "" []
             [Posting False "b" (Mixed [usd 1.00]) "" RegularPosting [] (Just t)
             ,Posting False "c" (Mixed [usd (-1.00)]) "" RegularPosting [] (Just t)
             ,Posting False "d" (Mixed [usd 100]) "" BalancedVirtualPosting [] (Just t)
             ,Posting False "e" (Mixed [usd (-100)]) "" BalancedVirtualPosting [] (Just t)
             ] ""
     assertBool "balanced virtual postings need to balance among themselves (2)" (isTransactionBalanced Nothing t)

  ]]
