{-|

A 'Transaction' consists of two or more related 'Posting's which balance
to zero, representing a movement of some commodity(ies) between accounts,
plus a date and optional metadata like description and cleared status.

-}

module Hledger.Data.Transaction
where
import Hledger.Data.Utils
import Hledger.Data.Types
import Hledger.Data.Dates
import Hledger.Data.Posting
import Hledger.Data.Amount
import Hledger.Data.Commodity (dollars, dollar, unknown)

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
      desc = ' ' : tdescription t
      comment = if null com then "" else "  ; " ++ com where com = tcomment t
      showdate = printf "%-10s" . showDate
      showedate = printf "=%s" . showdate
      showpostings ps
          | elide && length ps > 1 && isTransactionBalanced t
              = map showposting (init ps) ++ [showpostingnoamt (last ps)]
          | otherwise = map showposting ps
          where
            showposting p = showacct p ++ "  " ++ showamount (pamount p) ++ showcomment (pcomment p)
            showpostingnoamt p = rstrip $ showacct p ++ "              " ++ showcomment (pcomment p)
            showacct p = "    " ++ showstatus p ++ printf (printf "%%-%ds" w) (showAccountName Nothing (ptype p) (paccount p))
            w = maximum $ map (length . paccount) ps
            showamount = printf "%12s" . showMixedAmountOrZero
            showcomment s = if null s then "" else "  ; "++s
            showstatus p = if pstatus p then "* " else ""

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
isTransactionBalanced :: Transaction -> Bool
isTransactionBalanced t = isReallyZeroMixedAmountCost rsum && isReallyZeroMixedAmountCost bvsum
    where (rsum, _, bvsum) = transactionPostingBalances t

-- | Ensure that this entry is balanced, possibly auto-filling a missing
-- amount first. We can auto-fill if there is just one non-virtual
-- transaction without an amount. The auto-filled balance will be
-- converted to cost basis if possible. If the entry can not be balanced,
-- return an error message instead.
balanceTransaction :: Transaction -> Either String Transaction
balanceTransaction t@Transaction{tpostings=ps}
    | length rwithoutamounts > 1 || length bvwithoutamounts > 1
        = Left $ printerr "could not balance this transaction (too many missing amounts)"
    | not $ isTransactionBalanced t' = Left $ printerr $ nonzerobalanceerror t'
    | otherwise = Right t'
    where
      rps = filter isReal ps
      bvps = filter isBalancedVirtual ps
      (rwithamounts, rwithoutamounts) = partition hasAmount rps
      (bvwithamounts, bvwithoutamounts) = partition hasAmount bvps
      t' = t{tpostings=map balance ps}
          where 
            balance p | not (hasAmount p) && isReal p
                          = p{pamount = costOfMixedAmount (-(sum $ map pamount rwithamounts))}
                      | not (hasAmount p) && isBalancedVirtual p
                          = p{pamount = costOfMixedAmount (-(sum $ map pamount bvwithamounts))}
                      | otherwise = p
      printerr s = intercalate "\n" [s, showTransactionUnelided t]

nonzerobalanceerror :: Transaction -> String
nonzerobalanceerror t = printf "could not balance this transaction (%s%s%s)" rmsg sep bvmsg
    where
      (rsum, _, bvsum) = transactionPostingBalances t
      rmsg | isReallyZeroMixedAmountCost rsum = ""
           | otherwise = "real postings are off by " ++ show rsum
      bvmsg | isReallyZeroMixedAmountCost bvsum = ""
            | otherwise = "balanced virtual postings are off by " ++ show bvsum
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

tests_Transaction = TestList [
  "showTransaction" ~: do
     assertEqual "show a balanced transaction, eliding last amount"
       (unlines
        ["2007/01/28 coopportunity"
        ,"    expenses:food:groceries        $47.18"
        ,"    assets:checking"
        ,""
        ])
       (let t = Transaction (parsedate "2007/01/28") Nothing False "" "coopportunity" ""
                [Posting False "expenses:food:groceries" (Mixed [dollars 47.18]) "" RegularPosting (Just t)
                ,Posting False "assets:checking" (Mixed [dollars (-47.18)]) "" RegularPosting (Just t)
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
       (let t = Transaction (parsedate "2007/01/28") Nothing False "" "coopportunity" ""
                [Posting False "expenses:food:groceries" (Mixed [dollars 47.18]) "" RegularPosting (Just t)
                ,Posting False "assets:checking" (Mixed [dollars (-47.18)]) "" RegularPosting (Just t)
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
        (txnTieKnot $ Transaction (parsedate "2007/01/28") Nothing False "" "coopportunity" ""
         [Posting False "expenses:food:groceries" (Mixed [dollars 47.18]) "" RegularPosting Nothing
         ,Posting False "assets:checking" (Mixed [dollars (-47.19)]) "" RegularPosting Nothing
         ] ""))

  ,"showTransaction" ~: do
     assertEqual "show an unbalanced transaction with one posting, should not elide"
       (unlines
        ["2007/01/28 coopportunity"
        ,"    expenses:food:groceries        $47.18"
        ,""
        ])
       (showTransaction
        (txnTieKnot $ Transaction (parsedate "2007/01/28") Nothing False "" "coopportunity" ""
         [Posting False "expenses:food:groceries" (Mixed [dollars 47.18]) "" RegularPosting Nothing
         ] ""))

  ,"showTransaction" ~: do
     assertEqual "show a transaction with one posting and a missing amount"
       (unlines
        ["2007/01/28 coopportunity"
        ,"    expenses:food:groceries              "
        ,""
        ])
       (showTransaction
        (txnTieKnot $ Transaction (parsedate "2007/01/28") Nothing False "" "coopportunity" ""
         [Posting False "expenses:food:groceries" missingamt "" RegularPosting Nothing
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
        (txnTieKnot $ Transaction (parsedate "2010/01/01") Nothing False "" "x" ""
         [Posting False "a" (Mixed [Amount unknown 1 (Just $ Mixed [Amount dollar{precision=0} 2 Nothing])]) "" RegularPosting Nothing
         ,Posting False "b" missingamt "" RegularPosting Nothing
         ] ""))

  ]
