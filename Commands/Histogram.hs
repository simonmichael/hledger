{-| 

Print a histogram report.

-}

module Commands.Histogram
where
import Prelude hiding (putStr)
import Ledger
import Options
import System.IO.UTF8


barchar = '*'

-- | Print a histogram of some statistic per reporting interval, such as
-- number of transactions per day.
histogram :: [Opt] -> [String] -> Ledger -> IO ()
histogram opts args = putStr . showHistogram opts args

showHistogram :: [Opt] -> [String] -> Ledger -> String
showHistogram opts args l = concatMap (printDayWith countBar) daytxns
    where
      i = intervalFromOpts opts
      interval | i == NoInterval = Daily
               | otherwise = i
      fullspan = rawLedgerDateSpan $ rawledger l
      days = filter (DateSpan Nothing Nothing /=) $ splitSpan interval fullspan
      daytxns = [(s, filter (isTransactionInDateSpan s) ts) | s <- days]
      -- same as Register
      ts = sortBy (comparing tdate) $ filterempties $ filter matchapats $ filterdepth $ ledgerTransactions l
      filterempties
          | Empty `elem` opts = id
          | otherwise = filter (not . isZeroMixedAmount . tamount)
      matchapats = matchpats apats . taccount
      (apats,_) = parsePatternArgs args
      filterdepth | interval == NoInterval = filter (\t -> accountNameLevel (taccount t) <= depth)
                  | otherwise = id
      depth = depthFromOpts opts

printDayWith f (DateSpan b _, ts) = printf "%s %s\n" (show $ fromJust b) (f ts)

countBar ts = replicate (length ts) barchar

total = show . sumTransactions

-- totalBar ts = replicate (sumTransactions ts) barchar
