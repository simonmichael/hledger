{-# LANGUAGE CPP #-}
{-| 

A ledger-compatible @register@ command.

-}

module Commands.Register
where
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
showRegisterReport opts filterspec l
    | interval == NoInterval = showpostings displayedps nullposting startbal
    | otherwise = showpostings summaryps nullposting startbal
    where
      startbal = sumPostings precedingps
      (displayedps, _) = span displayExprMatches restofps
      (precedingps, restofps) = break displayExprMatches sortedps
      sortedps = sortBy (comparing postingDate) ps
      ps = journalPostings $ filterJournalPostings filterspec $ journal l
      summaryps = concatMap summarisespan spans
      summarisespan s = summarisePostingsInDateSpan s depth empty (postingsinspan s)
      postingsinspan s = filter (isPostingInDateSpan s) displayedps
      spans = splitSpan interval (postingsDateSpan displayedps)
      interval = intervalFromOpts opts
      empty = Empty `elem` opts
      depth = depthFromOpts opts
      dispexpr = displayExprFromOpts opts
      displayExprMatches p = case dispexpr of
                               Nothing -> True
                               Just e  -> (fromparse $ parsewith datedisplayexpr e) p
                        
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
showpostings :: [Posting] -> Posting -> MixedAmount -> String
showpostings [] _ _ = ""
showpostings (p:ps) pprev bal = this ++ showpostings ps p bal'
    where
      this = showposting isfirst p bal'
      isfirst = ptransaction p /= ptransaction pprev
      bal' = bal + pamount p

-- | Show one posting and running balance, with or without transaction info.
showposting :: Bool -> Posting -> MixedAmount -> String
showposting withtxninfo p b = concatBottomPadded [txninfo ++ pstr ++ " ", bal] ++ "\n"
    where
      ledger3ishlayout = False
      datedescwidth = if ledger3ishlayout then 34 else 32
      txninfo = if withtxninfo then printf "%s %s " date desc else replicate datedescwidth ' '
      date = showDate da
      datewidth = 10
      descwidth = datedescwidth - datewidth - 2
      desc = printf ("%-"++(show descwidth)++"s") $ elideRight descwidth de :: String
      pstr = showPostingWithoutPrice p
      bal = padleft 12 (showMixedAmountOrZeroWithoutPrice b)
      (da,de) = case ptransaction p of Just (Transaction{tdate=da',tdescription=de'}) -> (da',de')
                                       Nothing -> (nulldate,"")

