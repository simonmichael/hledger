{-| 

A ledger-compatible @register@ command.

-}

module Commands.Register
where
import Prelude hiding (putStr)
import Ledger
import Options
import System.IO.UTF8


-- | Print a register report.
register :: [Opt] -> [String] -> Ledger -> IO ()
register opts args = putStr . showRegisterReport opts args

{- |
Generate the register report. Each ledger entry is displayed as two or
more lines like this:

@
date (10)  description (20)     account (22)            amount (11)  balance (12)
DDDDDDDDDD dddddddddddddddddddd aaaaaaaaaaaaaaaaaaaaaa  AAAAAAAAAAA AAAAAAAAAAAA
                                aaaaaaaaaaaaaaaaaaaaaa  AAAAAAAAAAA AAAAAAAAAAAA
                                ...                     ...         ...
@
-}
showRegisterReport :: [Opt] -> [String] -> Ledger -> String
showRegisterReport opts args l
    | interval == NoInterval = showps displayedps nullposting startbal
    | otherwise = showps summaryps nullposting startbal
    where
      interval = intervalFromOpts opts
      ps = sortBy (comparing postingDate) $ filterempties $ filterPostings apats $ filterdepth $ ledgerPostings l
      filterdepth | interval == NoInterval = filter (\p -> accountNameLevel (paccount p) <= depth)
                  | otherwise = id
      filterempties
          | Empty `elem` opts = id
          | otherwise = filter (not . isZeroMixedAmount . pamount)
      (precedingps, ps') = break (matchdisplayopt dopt) ps
      (displayedps, _) = span (matchdisplayopt dopt) ps'
      startbal = sumPostings precedingps
      (apats,_) = parsePatternArgs args
      matchdisplayopt Nothing _ = True
      matchdisplayopt (Just e) p = (fromparse $ parsewith datedisplayexpr e) p
      dopt = displayFromOpts opts
      empty = Empty `elem` opts
      depth = depthFromOpts opts
      summaryps = concatMap summarisespan spans
      summarisespan s = summarisePostingsInDateSpan s depth empty (postingsinspan s)
      postingsinspan s = filter (isPostingInDateSpan s) displayedps
      spans = splitSpan interval (ledgerDateSpan l)
                        
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
summarisePostingsInDateSpan :: DateSpan -> Int -> Bool -> [Posting] -> [Posting]
summarisePostingsInDateSpan (DateSpan b e) depth showempty ps
    | null ps && showempty = [p]
    | null ps = []
    | otherwise = summaryps'
    where
      postingwithinfo date desc = nullposting{ptransaction=Just nulltransaction{tdate=date,tdescription=desc}}
      p = postingwithinfo b' ("- "++ showDate (addDays (-1) e'))
      b' = fromMaybe (postingDate $ head ps) b
      e' = fromMaybe (postingDate $ last ps) e
      summaryps'
          | showempty = summaryps
          | otherwise = filter (not . isZeroMixedAmount . pamount) summaryps
      anames = sort $ nub $ map paccount ps
      -- aggregate balances by account, like cacheLedger, then do depth-clipping
      (_,_,exclbalof,inclbalof) = groupPostings ps
      clippedanames = clipAccountNames depth anames
      isclipped a = accountNameLevel a >= depth
      balancetoshowfor a =
          (if isclipped a then inclbalof else exclbalof) (if null a then "top" else a)
      summaryps = [p{paccount=a,pamount=balancetoshowfor a} | a <- clippedanames]

clipAccountNames :: Int -> [AccountName] -> [AccountName]
clipAccountNames d as = nub $ map (clip d) as 
    where clip d = accountNameFromComponents . take d . accountNameComponents

-- | Show postings one per line, along with transaction info for the first
-- posting of each transaction, and a running balance.
showps :: [Posting] -> Posting -> MixedAmount -> String
showps [] _ _ = ""
showps (p:ps) pprev bal = this ++ showps ps p bal'
    where
      this = showp isfirst p bal'
      isfirst = ptransaction p /= ptransaction pprev
      bal' = bal + pamount p

-- | Show one posting and running balance, with or without transaction info.
showp :: Bool -> Posting -> MixedAmount -> String
showp withtxninfo p b = concatBottomPadded [txninfo ++ pstr ++ " ", bal] ++ "\n"
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

