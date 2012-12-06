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
import Hledger.Query
import Prelude hiding (putStr)
import Hledger.Utils.UTF8IOCompat (putStr)


barchar = '*'

-- | Print a histogram of some statistic per reporting interval, such as
-- number of postings per day.
histogram :: CliOpts -> Journal -> IO ()
histogram CliOpts{reportopts_=ropts} j = do
  d <- getCurrentDay
  putStr $ showHistogram ropts (queryFromOpts d ropts) j

showHistogram :: ReportOpts -> Query -> Journal -> String
showHistogram opts q j = concatMap (printDayWith countBar) spanps
    where
      i = intervalFromOpts opts
      interval | i == NoInterval = Days 1
               | otherwise = i
      span = queryDateSpan (date2_ opts) q `orDatesFrom` journalDateSpan j
      spans = filter (DateSpan Nothing Nothing /=) $ splitSpan interval span
      spanps = [(s, filter (isPostingInDateSpan s) ps) | s <- spans]
      -- same as Register
      -- should count transactions, not postings ?
      -- ps = sortBy (comparing postingDate) $ filterempties $ filter matchapats $ filterdepth $ journalPostings j
      ps = sortBy (comparing postingDate) $ filterempties $ filter (q `matchesPosting`) $ journalPostings j
      filterempties
          | queryEmpty q = id
          | otherwise = filter (not . isZeroMixedAmount . pamount)

printDayWith f (DateSpan b _, ts) = printf "%s %s\n" (show $ fromJust b) (f ts)

countBar ps = replicate (length ps) barchar
