{-# LANGUAGE CPP #-}
{-| 

Print a histogram report.

-}

module Hledger.Cli.Commands.Histogram
where
import Hledger.Data
import Hledger.Cli.Options
#if __GLASGOW_HASKELL__ <= 610
import Prelude hiding ( putStr )
import System.IO.UTF8
#endif


barchar = '*'

-- | Print a histogram of some statistic per reporting interval, such as
-- number of postings per day.
histogram :: [Opt] -> [String] -> Journal -> IO ()
histogram opts args j = do
  t <- getCurrentLocalTime
  putStr $ showHistogram opts (optsToFilterSpec opts args t) j

showHistogram :: [Opt] -> FilterSpec -> Journal -> String
showHistogram opts filterspec j = concatMap (printDayWith countBar) dayps
    where
      i = intervalFromOpts opts
      interval | i == NoInterval = Daily
               | otherwise = i
      fullspan = journalDateSpan j
      days = filter (DateSpan Nothing Nothing /=) $ splitSpan interval fullspan
      dayps = [(s, filter (isPostingInDateSpan s) ps) | s <- days]
      -- same as Register
      -- should count transactions, not postings ?
      ps = sortBy (comparing postingDate) $ filterempties $ filter matchapats $ filterdepth $ journalPostings j
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
