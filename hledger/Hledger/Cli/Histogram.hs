{-| 

Print a histogram report.

-}

module Hledger.Cli.Histogram
where
import Data.List
import Data.Maybe
import Data.Ord
import Text.Printf

import Hledger.Cli.Options
import Hledger.Data
import Hledger.Reports
import Prelude hiding (putStr)
import Hledger.Utils.UTF8IOCompat (putStr)


barchar = '*'

-- | Print a histogram of some statistic per reporting interval, such as
-- number of postings per day.
histogram :: CliOpts -> Journal -> IO ()
histogram CliOpts{reportopts_=reportopts_} j = do
  d <- getCurrentDay
  putStr $ showHistogram reportopts_ (optsToFilterSpec reportopts_ d) j

showHistogram :: ReportOpts -> FilterSpec -> Journal -> String
showHistogram opts filterspec j = concatMap (printDayWith countBar) spanps
    where
      i = intervalFromOpts opts
      interval | i == NoInterval = Days 1
               | otherwise = i
      span = datespan filterspec `orDatesFrom` journalDateSpan j
      spans = filter (DateSpan Nothing Nothing /=) $ splitSpan interval span
      spanps = [(s, filter (isPostingInDateSpan s) ps) | s <- spans]
      -- same as Register
      -- should count transactions, not postings ?
      ps = sortBy (comparing postingDate) $ filterempties $ filter matchapats $ filterdepth $ journalPostings j
      filterempties
          | empty_ opts = id
          | otherwise = filter (not . isZeroMixedAmount . pamount)
      matchapats = matchpats apats . paccount
      apats = acctpats filterspec
      filterdepth | interval == NoInterval = filter (\p -> accountNameLevel (paccount p) <= depth)
                  | otherwise = id
      depth = fromMaybe 99999 $ depth_ opts

printDayWith f (DateSpan b _, ts) = printf "%s %s\n" (show $ fromJust b) (f ts)

countBar ps = replicate (length ps) barchar
