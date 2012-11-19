{-|

Print some statistics for the journal.

-}

module Hledger.Cli.Stats
where
import Data.List
import Data.Maybe
import Data.Ord
import Data.Time.Calendar
import Text.Printf
import qualified Data.Map as Map

import Hledger
import Hledger.Cli.Options
import Prelude hiding (putStr)
import Hledger.Utils.UTF8IOCompat (putStr)


-- like Register.summarisePostings
-- | Print various statistics for the journal.
stats :: CliOpts -> Journal -> IO ()
stats CliOpts{reportopts_=reportopts_} j = do
  d <- getCurrentDay
  let q = queryFromOpts d reportopts_
      l = ledgerFromJournal q j
      reportspan = (ledgerDateSpan l) `orDatesFrom` (queryDateSpan False q)
      intervalspans = splitSpan (intervalFromOpts reportopts_) reportspan
      showstats = showLedgerStats l d
      s = intercalate "\n" $ map showstats intervalspans
  putStr s

showLedgerStats :: Ledger -> Day -> DateSpan -> String
showLedgerStats l today span =
    unlines $ map (\(label,value) -> concatBottomPadded [printf fmt1 label, value]) stats
    where
      fmt1 = "%-" ++ show w1 ++ "s: "
      -- fmt2 = "%-" ++ show w2 ++ "s"
      w1 = maximum $ map (length . fst) stats
      -- w2 = maximum $ map (length . show . snd) stats
      stats = [
         ("Main journal file" :: String, path) -- ++ " (from " ++ source ++ ")")
        ,("Included journal files", unlines $ reverse $ -- cf journalAddFile
                                    drop 1 $ journalFilePaths j)
        ,("Transactions span", printf "%s to %s (%d days)" (start span) (end span) days)
        ,("Last transaction", maybe "none" show lastdate ++ showelapsed lastelapsed)
        ,("Transactions", printf "%d (%0.1f per day)" tnum txnrate)
        ,("Transactions last 30 days", printf "%d (%0.1f per day)" tnum30 txnrate30)
        ,("Transactions last 7 days", printf "%d (%0.1f per day)" tnum7 txnrate7)
        ,("Payees/descriptions", show $ length $ nub $ map tdescription ts)
        ,("Accounts", printf "%d (depth %d)" acctnum acctdepth)
        ,("Commodities", printf "%s (%s)" (show $ length cs) (intercalate ", " cs))
      -- Transactions this month     : %(monthtxns)s (last month in the same period: %(lastmonthtxns)s)
      -- Uncleared transactions      : %(uncleared)s
      -- Days since reconciliation   : %(reconcileelapsed)s
      -- Days since last transaction : %(recentelapsed)s
       ]
           where
             j = ljournal l
             path = journalFilePath j
             ts = sortBy (comparing tdate) $ filter (spanContainsDate span . tdate) $ jtxns j
             as = nub $ map paccount $ concatMap tpostings ts
             cs = Map.keys $ canonicalStyles $ concatMap amounts $ map pamount $ concatMap tpostings ts
             lastdate | null ts = Nothing
                      | otherwise = Just $ tdate $ last ts
             lastelapsed = maybe Nothing (Just . diffDays today) lastdate
             showelapsed Nothing = ""
             showelapsed (Just days) = printf " (%d %s)" days' direction
                                       where days' = abs days
                                             direction | days >= 0 = "days ago" :: String
                                                       | otherwise = "days from now"
             tnum = length ts
             start (DateSpan (Just d) _) = show d
             start _ = ""
             end (DateSpan _ (Just d)) = show d
             end _ = ""
             days = fromMaybe 0 $ daysInSpan span
             txnrate | days==0 = 0
                     | otherwise = fromIntegral tnum / fromIntegral days :: Double
             tnum30 = length $ filter withinlast30 ts
             withinlast30 t = d >= addDays (-30) today && (d<=today) where d = tdate t
             txnrate30 = fromIntegral tnum30 / 30 :: Double
             tnum7 = length $ filter withinlast7 ts
             withinlast7 t = d >= addDays (-7) today && (d<=today) where d = tdate t
             txnrate7 = fromIntegral tnum7 / 7 :: Double
             acctnum = length as
             acctdepth | null as = 0
                       | otherwise = maximum $ map accountNameLevel as

