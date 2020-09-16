{-# LANGUAGE TemplateHaskell #-}

{-|

Print a bar chart of posting activity per day, or other report interval.

-}

module Hledger.Cli.Commands.Activity
where

import Data.List
import Data.Maybe
import Text.Printf

import Hledger
import Hledger.Cli.CliOptions
import Prelude hiding (putStr)
import Hledger.Utils.UTF8IOCompat (putStr)

activitymode = hledgerCommandMode
  $(embedFileRelative "Hledger/Cli/Commands/Activity.txt")
  []
  [generalflagsgroup1]
  hiddenflags
  ([], Just $ argsFlag "[QUERY]")

barchar :: Char
barchar = '*'

-- | Print a bar chart of number of postings per report interval.
activity :: CliOpts -> Journal -> IO ()
activity CliOpts{reportspec_=rspec} j = putStr $ showHistogram rspec j

showHistogram :: ReportSpec -> Journal -> String
showHistogram ReportSpec{rsQuery=q,rsOpts=ReportOpts{interval_=i,date2_=date2}} j =
    concatMap (printDayWith countBar) spanps
  where
    interval | i == NoInterval = Days 1
             | otherwise = i
    span' = queryDateSpan date2 q `spanDefaultsFrom` journalDateSpan date2 j
    spans = filter (DateSpan Nothing Nothing /=) $ splitSpan interval span'
    spanps = [(s, filter (isPostingInDateSpan s) ps) | s <- spans]
    -- same as Register
    -- should count transactions, not postings ?
    -- ps = sortBy (comparing postingDate) $ filterempties $ filter matchapats $ filterdepth $ journalPostings j
    ps = sortOn postingDate $ filter (q `matchesPosting`) $ journalPostings j

printDayWith f (DateSpan b _, ps) = printf "%s %s\n" (show $ fromJust b) (f ps)

countBar ps = replicate (length ps) barchar
