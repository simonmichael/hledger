{-| 

A ledger-compatible @register@ command.

-}

module Commands.Register
where
import Data.Function (on)
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
    | interval == NoInterval = showtxns displayedts nulltxn startbal
    | otherwise = showtxns summaryts nulltxn startbal
    where
      interval = intervalFromOpts opts
      ts = sortBy (comparing tdate) $ filterempties $ filtertxns apats $ filterdepth $ ledgerTransactions l
      filterdepth | interval == NoInterval = filter (\t -> accountNameLevel (taccount t) <= depth)
                  | otherwise = id
      filterempties
          | Empty `elem` opts = id
          | otherwise = filter (not . isZeroMixedAmount . tamount)
      (precedingts, ts') = break (matchdisplayopt dopt) ts
      (displayedts, _) = span (matchdisplayopt dopt) ts'
      startbal = sumTransactions precedingts
      (apats,_) = parsePatternArgs args
      matchdisplayopt Nothing _ = True
      matchdisplayopt (Just e) t = (fromparse $ parsewith datedisplayexpr e) t
      dopt = displayFromOpts opts
      empty = Empty `elem` opts
      depth = depthFromOpts opts
      summaryts = concatMap summarisespan (zip spans [1..])
      summarisespan (s,n) = summariseTransactionsInDateSpan s n depth empty (transactionsinspan s)
      transactionsinspan s = filter (isTransactionInDateSpan s) displayedts
      spans = splitSpan interval (ledgerDateSpan l)
                        
-- | Convert a date span (representing a reporting interval) and a list of
-- transactions within it to a new list of transactions aggregated by
-- account, which showtxns will render as a summary for this interval.
-- 
-- As usual with date spans the end date is exclusive, but for display
-- purposes we show the previous day as end date, like ledger.
-- 
-- A unique tnum value is provided so that the new transactions will be
-- grouped as one entry.
-- 
-- When a depth argument is present, transactions to accounts of greater
-- depth are aggregated where possible.
-- 
-- The showempty flag forces the display of a zero-transaction span
-- and also zero-transaction accounts within the span.
summariseTransactionsInDateSpan :: DateSpan -> Int -> Int -> Bool -> [Transaction] -> [Transaction]
summariseTransactionsInDateSpan (DateSpan b e) tnum depth showempty ts
    | null ts && showempty = [txn]
    | null ts = []
    | otherwise = summaryts'
    where
      txn = nulltxn{tnum=tnum, tdate=b', tdescription="- "++ showDate (addDays (-1) e')}
      b' = fromMaybe (tdate $ head ts) b
      e' = fromMaybe (tdate $ last ts) e
      summaryts'
          | showempty = summaryts
          | otherwise = filter (not . isZeroMixedAmount . tamount) summaryts
      txnanames = sort $ nub $ map taccount ts
      -- aggregate balances by account, like cacheLedger, then do depth-clipping
      (_,_,exclbalof,inclbalof) = groupTransactions ts
      clippedanames = clipAccountNames depth txnanames
      isclipped a = accountNameLevel a >= depth
      balancetoshowfor a =
          (if isclipped a then inclbalof else exclbalof) (if null a then "top" else a)
      summaryts = [txn{taccount=a,tamount=balancetoshowfor a} | a <- clippedanames]

clipAccountNames :: Int -> [AccountName] -> [AccountName]
clipAccountNames d as = nub $ map (clip d) as 
    where clip d = accountNameFromComponents . take d . accountNameComponents

-- | Show transactions one per line, with each date/description appearing
-- only once, and a running balance.
showtxns [] _ _ = ""
showtxns (t:ts) tprev bal = this ++ showtxns ts t bal'
    where
      this = showtxn (t `issame` tprev) t bal'
      issame = (==) `on` tnum
      bal' = bal + tamount t

-- | Show one transaction line and balance with or without the entry details.
showtxn :: Bool -> Transaction -> MixedAmount -> String
showtxn omitdesc t b = concatBottomPadded [entrydesc ++ p ++ " ", bal] ++ "\n"
    where
      ledger3ishlayout = False
      datedescwidth = if ledger3ishlayout then 34 else 32
      entrydesc = if omitdesc then replicate datedescwidth ' ' else printf "%s %s " date desc
      date = showDate da
      datewidth = 10
      descwidth = datedescwidth - datewidth - 2
      desc = printf ("%-"++(show descwidth)++"s") $ elideRight descwidth de :: String
      p = showPostingWithoutPrice $ Posting s a amt "" tt
      bal = padleft 12 (showMixedAmountOrZeroWithoutPrice b)
      Transaction{tstatus=s,tdate=da,tdescription=de,taccount=a,tamount=amt,ttype=tt} = t

