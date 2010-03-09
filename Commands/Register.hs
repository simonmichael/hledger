{-# LANGUAGE CPP #-}
{-| 

A ledger-compatible @register@ command.

-}

module Commands.Register (
  register
 ,showRegisterReport
 ,showPostingWithBalance
 ,tests_Register
) where

import Safe (headMay, lastMay)
import Ledger
import Options
#if __GLASGOW_HASKELL__ <= 610
import Prelude hiding ( putStr )
import System.IO.UTF8
#endif


-- | Print a register report.
register :: [Opt] -> [String] -> Ledger -> IO ()
register opts args l = do
  t <- getCurrentLocalTime
  putStr $ showRegisterReport opts (optsToFilterSpec opts args t) l

-- | Generate the register report, which is a list of postings with transaction
-- info and a running balance.
showRegisterReport :: [Opt] -> FilterSpec -> Ledger -> String
showRegisterReport opts filterspec l = showPostingsWithBalance ps nullposting startbal
    where
      ps | interval == NoInterval = displayableps
         | otherwise             = summarisePostings interval depth empty filterspan displayableps
      startbal = sumPostings precedingps
      (precedingps,displayableps,_) =
          postingsMatchingDisplayExpr (displayExprFromOpts opts) $ journalPostings $ filterJournalPostings filterspec $ journal l
      (interval, depth, empty) = (intervalFromOpts opts, depthFromOpts opts, Empty `elem` opts)
      filterspan = datespan filterspec

-- | Convert a list of postings into summary postings, one per interval.
summarisePostings :: Interval -> Maybe Int -> Bool -> DateSpan -> [Posting] -> [Posting]
summarisePostings interval depth empty filterspan ps = concatMap summarisespan $ splitSpan interval reportspan
    where
      summarisespan s = summarisePostingsInDateSpan s depth empty (postingsinspan s)
      postingsinspan s = filter (isPostingInDateSpan s) ps
      dataspan = postingsDateSpan ps
      reportspan | empty = filterspan `orDatesFrom` dataspan
                 | otherwise = dataspan

-- | Combine two datespans, filling any unspecified dates in the first
-- with dates from the second.
orDatesFrom (DateSpan a1 b1) (DateSpan a2 b2) = DateSpan a b
    where a = if isJust a1 then a1 else a2
          b = if isJust b1 then b1 else b2

-- | Date-sort and split a list of postings into three spans - postings matched
-- by the given display expression, and the preceding and following postings.
postingsMatchingDisplayExpr :: Maybe String -> [Posting] -> ([Posting],[Posting],[Posting])
postingsMatchingDisplayExpr d ps = (before, matched, after)
    where 
      sorted = sortBy (comparing postingDate) ps
      (before, rest) = break (displayExprMatches d) sorted
      (matched, after) = span (displayExprMatches d) rest

-- | Does this display expression allow this posting to be displayed ?
-- Raises an error if the display expression can't be parsed.
displayExprMatches :: Maybe String -> Posting -> Bool
displayExprMatches Nothing  _ = True
displayExprMatches (Just d) p = (fromparse $ parsewith datedisplayexpr d) p
                        
-- XXX confusing, refactor
-- | Given a date span (representing a reporting interval) and a list of
-- postings within it: aggregate the postings so there is only one per
-- account, and adjust their date/description so that they will render
-- as a summary for this interval.
-- 
-- As usual with date spans the end date is exclusive, but for display
-- purposes we show the previous day as end date, like ledger.
-- 
-- When a depth argument is present, postings to accounts of greater
-- depth are aggregated where possible.
-- 
-- The showempty flag forces the display of a zero-posting span
-- and also zero-posting accounts within the span.
summarisePostingsInDateSpan :: DateSpan -> Maybe Int -> Bool -> [Posting] -> [Posting]
summarisePostingsInDateSpan (DateSpan b e) depth showempty ps
    | null ps && (isNothing b || isNothing e) = []
    | null ps && showempty = [summaryp]
    | otherwise = summaryps'
    where
      summaryp = summaryPosting b' ("- "++ showDate (addDays (-1) e'))
      b' = fromMaybe (maybe nulldate postingDate $ headMay ps) b
      e' = fromMaybe (maybe (addDays 1 nulldate) postingDate $ lastMay ps) e
      summaryPosting date desc = nullposting{ptransaction=Just nulltransaction{tdate=date,tdescription=desc}}

      summaryps' = (if showempty then id else filter (not . isZeroMixedAmount . pamount)) summaryps
      summaryps = [summaryp{paccount=a,pamount=balancetoshowfor a} | a <- clippedanames]
      anames = sort $ nub $ map paccount ps
      -- aggregate balances by account, like cacheLedger, then do depth-clipping
      (_,_,exclbalof,inclbalof) = groupPostings ps
      clippedanames = nub $ map (clipAccountName d) anames
      isclipped a = accountNameLevel a >= d
      d = fromMaybe 99999 $ depth
      balancetoshowfor a =
          (if isclipped a then inclbalof else exclbalof) (if null a then "top" else a)

{- |
Show postings one per line, plus transaction info for the first posting of
each transaction, and a running balance. Eg:

@
date (10)  description (20)     account (22)            amount (11)  balance (12)
DDDDDDDDDD dddddddddddddddddddd aaaaaaaaaaaaaaaaaaaaaa  AAAAAAAAAAA AAAAAAAAAAAA
                                aaaaaaaaaaaaaaaaaaaaaa  AAAAAAAAAAA AAAAAAAAAAAA
@
-}
showPostingsWithBalance :: [Posting] -> Posting -> MixedAmount -> String
showPostingsWithBalance [] _ _ = ""
showPostingsWithBalance (p:ps) pprev bal = this ++ showPostingsWithBalance ps p bal'
    where
      this = showPostingWithBalance isfirst p bal'
      isfirst = ptransaction p /= ptransaction pprev
      bal' = bal + pamount p

-- | Show one posting and running balance, with or without transaction info.
showPostingWithBalance :: Bool -> Posting -> MixedAmount -> String
showPostingWithBalance withtxninfo p b = concatBottomPadded [txninfo ++ pstr ++ " ", bal] ++ "\n"
    where
      ledger3ishlayout = False
      datedescwidth = if ledger3ishlayout then 34 else 32
      txninfo = if withtxninfo then printf "%s %s " date desc else replicate datedescwidth ' '
      date = showDate da
      datewidth = 10
      descwidth = datedescwidth - datewidth - 2
      desc = printf ("%-"++(show descwidth)++"s") $ elideRight descwidth de :: String
      pstr = showPostingForRegister p
      bal = padleft 12 (showMixedAmountOrZeroWithoutPrice b)
      (da,de) = case ptransaction p of Just (Transaction{tdate=da',tdescription=de'}) -> (da',de')
                                       Nothing -> (nulldate,"")

tests_Register :: Test
tests_Register = TestList [

         "summarisePostings" ~: do
           summarisePostings Quarterly Nothing False (DateSpan Nothing Nothing) [] ~?= []

        ]
