{-# LANGUAGE TemplateHaskell #-}

{-|

Print a bar chart of posting activity per day, or other report interval.

-}

module Hledger.Cli.Commands.Activity
where

import Data.List (sortOn)
import Text.Printf (printf)
import Lens.Micro ((^.), set)

import Hledger
import Hledger.Cli.CliOptions

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
showHistogram rspec@ReportSpec{_rsQuery=q} j =
    concatMap (printDayWith countBar) spanps
  where
    spans = filter (DateSpan Nothing Nothing /=) . snd . reportSpan j $ case rspec ^. interval of
      NoInterval -> set interval (Days 1) rspec
      _ -> rspec
    spanps = [(s, filter (isPostingInDateSpan s) ps) | s <- spans]
    -- same as Register
    -- should count transactions, not postings ?
    -- ps = sortBy (comparing postingDate) $ filterempties $ filter matchapats $ filterdepth $ journalPostings j
    ps = sortOn postingDate $ filter (q `matchesPosting`) $ journalPostings j

printDayWith f (DateSpan (Just b) _, ps) = printf "%s %s\n" (show b) (f ps)
printDayWith _ _ = error "Expected start date for DateSpan"  -- PARTIAL:

countBar ps = replicate (length ps) barchar
