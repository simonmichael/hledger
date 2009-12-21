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
-- number of postings per day.
histogram :: [Opt] -> [String] -> Ledger -> IO ()
histogram opts args l = do
  t <- getCurrentLocalTime
  putStr $ showHistogram opts (optsToFilterSpec opts args t) l

showHistogram :: [Opt] -> FilterSpec -> Ledger -> String
showHistogram opts filterspec l = concatMap (printDayWith countBar) dayps
    where
      i = intervalFromOpts opts
      interval | i == NoInterval = Daily
               | otherwise = i
      fullspan = journalDateSpan $ journal l
      days = filter (DateSpan Nothing Nothing /=) $ splitSpan interval fullspan
      dayps = [(s, filter (isPostingInDateSpan s) ps) | s <- days]
      -- same as Register
      -- should count transactions, not postings ?
      ps = sortBy (comparing postingDate) $ filterempties $ filter matchapats $ filterdepth $ ledgerPostings l
      filterempties
          | Empty `elem` opts = id
          | otherwise = filter (not . isZeroMixedAmount . pamount)
      matchapats = matchpats apats . paccount
      apats = acctpats filterspec
      filterdepth | interval == NoInterval = filter (\p -> accountNameLevel (paccount p) <= depth)
                  | otherwise = id
      depth = fromMaybe 99999 $ depthFromOpts opts

printDayWith f (DateSpan b _, ts) = printf "%s %s\n" (show $ fromJust b) (f ts)

countBar ps = replicate (length ps) barchar
