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
    | interval == NoInterval = showtxns ts nulltxn nullmixedamt
    | otherwise = showtxns summaryts nulltxn nullmixedamt
    where
      interval = intervalFromOpts opts
      ts = filter (not . isZeroMixedAmount . amount) $ filter (matchdisplayopt dopt) $ filter matchapats $ ledgerTransactions l
      matchapats t = matchpats apats $ account t
      apats = fst $ parseAccountDescriptionArgs opts args
      matchdisplayopt Nothing t = True
      matchdisplayopt (Just e) t = (fromparse $ parsewith datedisplayexpr e) t
      dopt = displayFromOpts opts
      empty = Empty `elem` opts
      summaryts = concat $ map (\(n,s) -> summarise n s (filter (isTransactionInDateSpan s) ts)) $ zip [1..] spans
      spans = splitSpan interval (ledgerDateSpan l)
      -- generate a grouped set of summary transactions for this date span
      summarise :: Int -> DateSpan -> [Transaction] -> [Transaction]
      summarise _ _ [] = []
      summarise n (DateSpan b e) ts = summarytxns (b',e') n empty ts
          where
            b' = fromMaybe (date $ head ts) b
            e' = fromMaybe (date $ last ts) e

-- | Does the given transaction fall within the given date span ?
isTransactionInDateSpan :: DateSpan -> Transaction -> Bool
isTransactionInDateSpan (DateSpan Nothing Nothing)   _ = True
isTransactionInDateSpan (DateSpan Nothing (Just e))  (Transaction{date=d}) = d<e
isTransactionInDateSpan (DateSpan (Just b) Nothing)  (Transaction{date=d}) = d>=b
isTransactionInDateSpan (DateSpan (Just b) (Just e)) (Transaction{date=d}) = d>=b && d<e

-- | Convert a date span and a list of transactions within that date span
-- to a new list of transactions aggregated by account, which when
-- rendered by showtxns will display a summary for the date span.  Both
-- ends of the date span must be specified so we pass a tuple of dates.
-- As usual with date spans the second date is exclusive, but when
-- rendering we will show the previous (inclusive) date.  
-- A unique entryno value is provided so that these dummy transactions
-- will be rendered as one entry. Also the showempty flag is provided to
-- control display of zero-balance accounts.
summarytxns :: (Day,Day) -> Int -> Bool -> [Transaction] -> [Transaction]
summarytxns (b,e) entryno showempty ts = summaryts'
    where
      summaryts'
          | showempty = summaryts
          | otherwise = filter (not . isZeroMixedAmount . amount) summaryts
      summaryts = [templtxn{account=a,amount=balmap ! a} | a <- anames]
      templtxn = nulltxn{entryno=entryno,date=b,description="- "++(showDate eprev)}
      eprev = addDays (-1) e
      anames = sort $ nub $ map account ts
      -- from cacheLedger:
      sortedts = sortBy (comparing account) ts
      groupedts = groupBy (\t1 t2 -> account t1 == account t2) sortedts
      txnmap = Map.union 
               (Map.fromList [(account $ head g, g) | g <- groupedts])
               (Map.fromList [(a,[]) | a <- anames])
      txnsof = (txnmap !)
      subacctsof a = filter (a `isAccountNamePrefixOf`) anames
      subtxnsof a = concat [txnsof a | a <- [a] ++ subacctsof a]
      balmap = Map.union 
               (Map.fromList [(a,(sumTransactions $ subtxnsof a)) | a <- anames])
               (Map.fromList [(a,Mixed []) | a <- anames])
      --

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

