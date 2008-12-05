{-| 

A ledger-compatible @register@ command.

-}

module RegisterCommand
where
import qualified Data.Map as Map
import Data.Map ((!))
import Ledger
import Options


-- | Print a register report.
register :: [Opt] -> [String] -> Ledger -> IO ()
register opts args l = putStr $ showRegisterReport opts args l

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
      ts = filter (not . isZeroMixedAmount . amount) $ filter matchapats $ ledgerTransactions l
      (precedingts, ts') = break (matchdisplayopt dopt) ts
      (displayedts, _) = span (matchdisplayopt dopt) ts'
      startbal = sumTransactions precedingts
      matchapats t = matchpats apats $ account t
      apats = fst $ parseAccountDescriptionArgs opts args
      matchdisplayopt Nothing t = True
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
-- A unique entryno value is provided so that the new transactions will be
-- grouped as one entry.
-- 
-- When a depth argument is present, transactions to accounts of greater
-- depth are aggregated where possible.
-- 
-- The showempty flag forces the display of a zero-transaction span
-- and also zero-transaction accounts within the span.
summariseTransactionsInDateSpan :: DateSpan -> Int -> Maybe Int -> Bool -> [Transaction] -> [Transaction]
summariseTransactionsInDateSpan (DateSpan b e) entryno depth showempty ts
    | null ts && showempty = [txn]
    | null ts = []
    | otherwise = summaryts'
    where
      txn = nulltxn{entryno=entryno, date=b', description="- "++(showDate $ addDays (-1) e')}
      b' = fromMaybe (date $ head ts) b
      e' = fromMaybe (date $ last ts) e
      summaryts'
          | showempty = summaryts
          | otherwise = filter (not . isZeroMixedAmount . amount) summaryts
      txnanames = sort $ nub $ map account ts
      -- aggregate balances by account, like cacheLedger, then do depth-clipping
      (_,_,exclbalof,inclbalof) = groupTransactions ts
      clippedanames = clipAccountNames depth txnanames
      isclipped a = accountNameLevel a >= fromMaybe 9999 depth
      balancetoshowfor a =
          (if isclipped a then inclbalof else exclbalof) (if null a then "top" else a)
      summaryts = [txn{account=a,amount=balancetoshowfor a} | a <- clippedanames]

clipAccountNames :: Maybe Int -> [AccountName] -> [AccountName]
clipAccountNames Nothing as = as
clipAccountNames (Just d) as = nub $ map (clip d) as 
    where clip d = accountNameFromComponents . take d . accountNameComponents

-- | Does the given transaction fall within the given date span ?
isTransactionInDateSpan :: DateSpan -> Transaction -> Bool
isTransactionInDateSpan (DateSpan Nothing Nothing)   _ = True
isTransactionInDateSpan (DateSpan Nothing (Just e))  (Transaction{date=d}) = d<e
isTransactionInDateSpan (DateSpan (Just b) Nothing)  (Transaction{date=d}) = d>=b
isTransactionInDateSpan (DateSpan (Just b) (Just e)) (Transaction{date=d}) = d>=b && d<e

-- | Show transactions one per line, with each date/description appearing
-- only once, and a running balance.
showtxns [] _ _ = ""
showtxns (t@Transaction{amount=a}:ts) tprev bal = this ++ showtxns ts t bal'
    where
      this = showtxn (t `issame` tprev) t bal'
      issame t1 t2 = entryno t1 == entryno t2
      bal' = bal + amount t

-- | Show one transaction line and balance with or without the entry details.
showtxn :: Bool -> Transaction -> MixedAmount -> String
showtxn omitdesc t b = concatBottomPadded [entrydesc ++ txn ++ " ", bal] ++ "\n"
    where
      entrydesc = if omitdesc then replicate 32 ' ' else printf "%s %s " date desc
      date = showDate $ da
      desc = printf "%-20s" $ elideRight 20 de :: String
      txn = showRawTransaction $ RawTransaction a amt "" tt
      bal = padleft 12 (showMixedAmountOrZero b)
      Transaction{date=da,description=de,account=a,amount=amt,ttype=tt} = t

