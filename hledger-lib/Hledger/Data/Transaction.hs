{-|

A 'Transaction' consists of two or more related 'Posting's which balance
to zero, representing a movement of some commodity(ies) between accounts,
plus a date and optional metadata like description and cleared status.

-}

module Hledger.Data.Transaction
where
import qualified Data.Map as Map

import Hledger.Data.Utils
import Hledger.Data.Types
import Hledger.Data.Dates
import Hledger.Data.Posting
import Hledger.Data.Amount
import Hledger.Data.Commodity

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
                    tmetadata=[],
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
showTransaction = showTransaction' True False

showTransactionUnelided :: Transaction -> String
showTransactionUnelided = showTransaction' False False

showTransactionForPrint :: Bool -> Transaction -> String
showTransactionForPrint effective = showTransaction' False effective

showTransaction' :: Bool -> Bool -> Transaction -> String
showTransaction' elide effective t =
    unlines $ [description] ++ showpostings (tpostings t) ++ [""]
    where
      description = concat [date, status, code, desc, comment]
      date | effective = showdate $ fromMaybe (tdate t) $ teffectivedate t
           | otherwise = showdate (tdate t) ++ maybe "" showedate (teffectivedate t)
      status = if tstatus t then " *" else ""
      code = if length (tcode t) > 0 then printf " (%s)" $ tcode t else ""
      desc = if null d then "" else " " ++ d where d = tdescription t
      comment = if null c then "" else "  ; " ++ c where c = tcomment t
      showdate = printf "%-10s" . showDate
      showedate = printf "=%s" . showdate
      showpostings ps
          | elide && length ps > 1 && isTransactionBalanced Nothing t -- imprecise balanced check
              = map showposting (init ps) ++ [showpostingnoamt (last ps)]
          | otherwise = map showposting ps
          where
            showpostingnoamt p = rstrip $ showacct p ++ "              " ++ showcomment (pcomment p)
            showposting p = concatTopPadded [showacct p
                                            ,"  "
                                            ,showamt (pamount p)
                                            ,showcomment (pcomment p)
                                            ]
            showacct p = "    " ++ showstatus p ++ printf (printf "%%-%ds" w) (showAccountName Nothing (ptype p) (paccount p))
                where w = maximum $ map (length . paccount) ps
                      showstatus p = if pstatus p then "* " else ""
            showamt =
                padleft 12 . showMixedAmountOrZero
            showcomment s = if null s then "" else "  ; "++s

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

realPostings :: Transaction -> [Posting]
realPostings = filter isReal . tpostings

virtualPostings :: Transaction -> [Posting]
virtualPostings = filter isVirtual . tpostings

balancedVirtualPostings :: Transaction -> [Posting]
balancedVirtualPostings = filter isBalancedVirtual . tpostings

-- | Get the sums of a transaction's real, virtual, and balanced virtual postings.
transactionPostingBalances :: Transaction -> (MixedAmount,MixedAmount,MixedAmount)
transactionPostingBalances t = (sumPostings $ realPostings t
                               ,sumPostings $ virtualPostings t
                               ,sumPostings $ balancedVirtualPostings t)

-- | Is this transaction balanced ? A balanced transaction's real
-- (non-virtual) postings sum to 0, and any balanced virtual postings
-- also sum to 0.
isTransactionBalanced :: Maybe (Map.Map String Commodity) -> Transaction -> Bool
isTransactionBalanced canonicalcommoditymap t =
    -- isReallyZeroMixedAmountCost rsum && isReallyZeroMixedAmountCost bvsum
    isZeroMixedAmount rsum' && isZeroMixedAmount bvsum'
    where
      (rsum, _, bvsum) = transactionPostingBalances t
      rsum'  = canonicaliseMixedAmount canonicalcommoditymap $ costOfMixedAmount rsum
      bvsum' = canonicaliseMixedAmount canonicalcommoditymap $ costOfMixedAmount bvsum

-- | Ensure this transaction is balanced, possibly inferring a missing
-- amount or a conversion price first, or return an error message.
--
-- Balancing is affected by the provided commodities' display precisions.
--
-- We can infer an amount when there are multiple real postings and
-- exactly one of them is amountless; likewise for balanced virtual
-- postings. Inferred amounts are converted to cost basis when possible.
--
-- We can infer a price when all amounts were specified and the sum of
-- real postings' amounts is exactly two non-explicitly-priced amounts in
-- different commodities; likewise for balanced virtual postings.
balanceTransaction :: Maybe (Map.Map String Commodity) -> Transaction -> Either String Transaction
balanceTransaction canonicalcommoditymap t@Transaction{tpostings=ps}
    | length rwithoutamounts > 1 || length bvwithoutamounts > 1
        = Left $ printerr "could not balance this transaction (too many missing amounts)"
    | not $ isTransactionBalanced canonicalcommoditymap t''' = Left $ printerr $ nonzerobalanceerror t'''
    | otherwise = Right t'''
    where
      -- maybe infer missing amounts
      (rwithamounts, rwithoutamounts)   = partition hasAmount $ realPostings t
      (bvwithamounts, bvwithoutamounts) = partition hasAmount $ balancedVirtualPostings t
      ramounts  = map pamount rwithamounts
      bvamounts = map pamount bvwithamounts
      t' = t{tpostings=map inferamount ps}
          where 
            inferamount p | not (hasAmount p) && isReal p            = p{pamount = (- sum ramounts)}
                          | not (hasAmount p) && isBalancedVirtual p = p{pamount = (- sum bvamounts)}
                          | otherwise                             = p

      -- maybe infer conversion prices, for real postings
      rmixedamountsinorder = map pamount $ realPostings t'
      ramountsinorder = concatMap amounts rmixedamountsinorder
      rcommoditiesinorder  = map commodity ramountsinorder
      rsumamounts  = amounts $ sum rmixedamountsinorder
      -- assumption: the sum of mixed amounts is normalised (one simple amount per commodity)
      t'' = if length rsumamounts == 2 && all (isNothing.price) rsumamounts && t'==t
             then t'{tpostings=map inferprice ps}
             else t'
          where
            -- assumption: a posting's mixed amount contains one simple amount
            inferprice p@Posting{pamount=Mixed [a@Amount{commodity=c,price=Nothing}], ptype=RegularPosting}
                = p{pamount=Mixed [a{price=conversionprice c}]}
                where
                  conversionprice c | c == unpricedcommodity
                                        -- assign a balancing price. Use @@ for more exact output when possible.
                                        = if length ramountsinunpricedcommodity == 1
                                           then Just $ TotalPrice $ Mixed [setAmountPrecision maxprecision $ negate $ targetcommodityamount]
                                           else Just $ UnitPrice $ Mixed [setAmountPrecision maxprecision $ negate $ targetcommodityamount `divideAmount` (quantity unpricedamount)]
                                    | otherwise = Nothing
                      where
                        unpricedcommodity     = head $ filter (`elem` (map commodity rsumamounts)) rcommoditiesinorder
                        unpricedamount        = head $ filter ((==unpricedcommodity).commodity) rsumamounts
                        targetcommodityamount = head $ filter ((/=unpricedcommodity).commodity) rsumamounts
                        ramountsinunpricedcommodity = filter ((==unpricedcommodity).commodity) ramountsinorder
            inferprice p = p

      -- maybe infer prices for balanced virtual postings. Just duplicates the above for now.
      bvmixedamountsinorder = map pamount $ balancedVirtualPostings t''
      bvamountsinorder = concatMap amounts bvmixedamountsinorder
      bvcommoditiesinorder  = map commodity bvamountsinorder
      bvsumamounts  = amounts $ sum bvmixedamountsinorder
      t''' = if length bvsumamounts == 2 && all (isNothing.price) bvsumamounts && t'==t -- XXX could check specifically for bv amount inferring
             then t''{tpostings=map inferprice ps}
             else t''
          where
            inferprice p@Posting{pamount=Mixed [a@Amount{commodity=c,price=Nothing}], ptype=BalancedVirtualPosting}
                = p{pamount=Mixed [a{price=conversionprice c}]}
                where
                  conversionprice c | c == unpricedcommodity
                                        = if length bvamountsinunpricedcommodity == 1
                                           then Just $ TotalPrice $ Mixed [setAmountPrecision maxprecision $ negate $ targetcommodityamount]
                                           else Just $ UnitPrice $ Mixed [setAmountPrecision maxprecision $ negate $ targetcommodityamount `divideAmount` (quantity unpricedamount)]
                                    | otherwise = Nothing
                      where
                        unpricedcommodity     = head $ filter (`elem` (map commodity bvsumamounts)) bvcommoditiesinorder
                        unpricedamount        = head $ filter ((==unpricedcommodity).commodity) bvsumamounts
                        targetcommodityamount = head $ filter ((/=unpricedcommodity).commodity) bvsumamounts
                        bvamountsinunpricedcommodity = filter ((==unpricedcommodity).commodity) bvamountsinorder
            inferprice p = p

      printerr s = intercalate "\n" [s, showTransactionUnelided t]

nonzerobalanceerror :: Transaction -> String
nonzerobalanceerror t = printf "could not balance this transaction (%s%s%s)" rmsg sep bvmsg
    where
      (rsum, _, bvsum) = transactionPostingBalances t
      rmsg | isReallyZeroMixedAmountCost rsum = ""
           | otherwise = "real postings are off by " ++ show (costOfMixedAmount rsum)
      bvmsg | isReallyZeroMixedAmountCost bvsum = ""
            | otherwise = "balanced virtual postings are off by " ++ show (costOfMixedAmount bvsum)
      sep = if not (null rmsg) && not (null bvmsg) then "; " else ""

-- | Convert the primary date to either the actual or effective date.
journalTransactionWithDate :: WhichDate -> Transaction -> Transaction
journalTransactionWithDate ActualDate t = t
journalTransactionWithDate EffectiveDate t = txnTieKnot t{tdate=fromMaybe (tdate t) (teffectivedate t)}

-- | Ensure a transaction's postings refer back to it.
txnTieKnot :: Transaction -> Transaction
txnTieKnot t@Transaction{tpostings=ps} = t{tpostings=map (settxn t) ps}

-- | Set a posting's parent transaction.
settxn :: Transaction -> Posting -> Posting
settxn t p = p{ptransaction=Just t}

tests_Hledger_Data_Transaction = TestList [
  "showTransaction" ~: do
     assertEqual "show a balanced transaction, eliding last amount"
       (unlines
        ["2007/01/28 coopportunity"
        ,"    expenses:food:groceries        $47.18"
        ,"    assets:checking"
        ,""
        ])
       (let t = Transaction (parsedate "2007/01/28") Nothing False "" "coopportunity" "" []
                [Posting False "expenses:food:groceries" (Mixed [dollars 47.18]) "" RegularPosting [] (Just t)
                ,Posting False "assets:checking" (Mixed [dollars (-47.18)]) "" RegularPosting [] (Just t)
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
                [Posting False "expenses:food:groceries" (Mixed [dollars 47.18]) "" RegularPosting [] (Just t)
                ,Posting False "assets:checking" (Mixed [dollars (-47.18)]) "" RegularPosting [] (Just t)
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
         [Posting False "expenses:food:groceries" (Mixed [dollars 47.18]) "" RegularPosting [] Nothing
         ,Posting False "assets:checking" (Mixed [dollars (-47.19)]) "" RegularPosting [] Nothing
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
         [Posting False "expenses:food:groceries" (Mixed [dollars 47.18]) "" RegularPosting [] Nothing
         ] ""))

  ,"showTransaction" ~: do
     assertEqual "show a transaction with one posting and a missing amount"
       (unlines
        ["2007/01/28 coopportunity"
        ,"    expenses:food:groceries              "
        ,""
        ])
       (showTransaction
        (txnTieKnot $ Transaction (parsedate "2007/01/28") Nothing False "" "coopportunity" "" []
         [Posting False "expenses:food:groceries" missingamt "" RegularPosting [] Nothing
         ] ""))

  ,"showTransaction" ~: do
     assertEqual "show a transaction with a priced commodityless amount"
       (unlines
        ["2010/01/01 x"
        ,"    a        1 @ $2"
        ,"    b              "
        ,""
        ])
       (showTransaction
        (txnTieKnot $ Transaction (parsedate "2010/01/01") Nothing False "" "x" "" []
         [Posting False "a" (Mixed [Amount unknown 1 (Just $ UnitPrice $ Mixed [Amount dollar{precision=0} 2 Nothing])]) "" RegularPosting [] Nothing
         ,Posting False "b" missingamt "" RegularPosting [] Nothing
         ] ""))

  ,"balanceTransaction" ~: do
     assertBool "detect unbalanced entry, sign error"
                    (isLeft $ balanceTransaction Nothing
                           (Transaction (parsedate "2007/01/28") Nothing False "" "test" "" []
                            [Posting False "a" (Mixed [dollars 1]) "" RegularPosting [] Nothing,
                             Posting False "b" (Mixed [dollars 1]) "" RegularPosting [] Nothing
                            ] ""))
     assertBool "detect unbalanced entry, multiple missing amounts"
                    (isLeft $ balanceTransaction Nothing
                           (Transaction (parsedate "2007/01/28") Nothing False "" "test" "" []
                            [Posting False "a" missingamt "" RegularPosting [] Nothing,
                             Posting False "b" missingamt "" RegularPosting [] Nothing
                            ] ""))
     let e = balanceTransaction Nothing (Transaction (parsedate "2007/01/28") Nothing False "" "" "" []
                           [Posting False "a" (Mixed [dollars 1]) "" RegularPosting [] Nothing,
                            Posting False "b" missingamt "" RegularPosting [] Nothing
                           ] "")
     assertBool "balanceTransaction allows one missing amount" (isRight e)
     assertEqual "balancing amount is inferred"
                     (Mixed [dollars (-1)])
                     (case e of
                        Right e' -> (pamount $ last $ tpostings e')
                        Left _ -> error' "should not happen")
     let e = balanceTransaction Nothing (Transaction (parsedate "2011/01/01") Nothing False "" "" "" []
                           [Posting False "a" (Mixed [dollars 1.35]) "" RegularPosting [] Nothing,
                            Posting False "b" (Mixed [euros   (-1)]) "" RegularPosting [] Nothing
                           ] "")
     assertBool "balanceTransaction can infer conversion price" (isRight e)
     assertEqual "balancing conversion price is inferred"
                     (Mixed [Amount{commodity=dollar{precision=2},
                                    quantity=1.35,
                                    price=(Just $ TotalPrice $ Mixed [Amount{commodity=euro{precision=maxprecision},
                                                                             quantity=1,
                                                                             price=Nothing}])}])
                     (case e of
                        Right e' -> (pamount $ head $ tpostings e')
                        Left _ -> error' "should not happen")

  ,"isTransactionBalanced" ~: do
     let t = Transaction (parsedate "2009/01/01") Nothing False "" "a" "" []
             [Posting False "b" (Mixed [dollars 1.00]) "" RegularPosting [] (Just t)
             ,Posting False "c" (Mixed [dollars (-1.00)]) "" RegularPosting [] (Just t)
             ] ""
     assertBool "detect balanced" (isTransactionBalanced Nothing t)
     let t = Transaction (parsedate "2009/01/01") Nothing False "" "a" "" []
             [Posting False "b" (Mixed [dollars 1.00]) "" RegularPosting [] (Just t)
             ,Posting False "c" (Mixed [dollars (-1.01)]) "" RegularPosting [] (Just t)
             ] ""
     assertBool "detect unbalanced" (not $ isTransactionBalanced Nothing t)
     let t = Transaction (parsedate "2009/01/01") Nothing False "" "a" "" []
             [Posting False "b" (Mixed [dollars 1.00]) "" RegularPosting [] (Just t)
             ] ""
     assertBool "detect unbalanced, one posting" (not $ isTransactionBalanced Nothing t)
     let t = Transaction (parsedate "2009/01/01") Nothing False "" "a" "" []
             [Posting False "b" (Mixed [dollars 0]) "" RegularPosting [] (Just t)
             ] ""
     assertBool "one zero posting is considered balanced for now" (isTransactionBalanced Nothing t)
     let t = Transaction (parsedate "2009/01/01") Nothing False "" "a" "" []
             [Posting False "b" (Mixed [dollars 1.00]) "" RegularPosting [] (Just t)
             ,Posting False "c" (Mixed [dollars (-1.00)]) "" RegularPosting [] (Just t)
             ,Posting False "d" (Mixed [dollars 100]) "" VirtualPosting [] (Just t)
             ] ""
     assertBool "virtual postings don't need to balance" (isTransactionBalanced Nothing t)
     let t = Transaction (parsedate "2009/01/01") Nothing False "" "a" "" []
             [Posting False "b" (Mixed [dollars 1.00]) "" RegularPosting [] (Just t)
             ,Posting False "c" (Mixed [dollars (-1.00)]) "" RegularPosting [] (Just t)
             ,Posting False "d" (Mixed [dollars 100]) "" BalancedVirtualPosting [] (Just t)
             ] ""
     assertBool "balanced virtual postings need to balance among themselves" (not $ isTransactionBalanced Nothing t)
     let t = Transaction (parsedate "2009/01/01") Nothing False "" "a" "" []
             [Posting False "b" (Mixed [dollars 1.00]) "" RegularPosting [] (Just t)
             ,Posting False "c" (Mixed [dollars (-1.00)]) "" RegularPosting [] (Just t)
             ,Posting False "d" (Mixed [dollars 100]) "" BalancedVirtualPosting [] (Just t)
             ,Posting False "e" (Mixed [dollars (-100)]) "" BalancedVirtualPosting [] (Just t)
             ] ""
     assertBool "balanced virtual postings need to balance among themselves (2)" (isTransactionBalanced Nothing t)

  ]
