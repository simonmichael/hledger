{-|

Print some statistics for the journal.

-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NamedFieldPuns #-}

module Hledger.Cli.Commands.Stats (
  statsmode
 ,stats
)
where

import Data.Default (def)
import Data.List (nub, sortOn)
import Data.List.Extra (nubSort)
import Data.Maybe (fromMaybe)
import Data.HashSet (size, fromList)
import qualified Data.Text as T
import qualified Data.Text.Lazy.Builder as TB
import Data.Time.Calendar (Day, addDays, diffDays)
import System.Console.CmdArgs.Explicit hiding (Group)
import Text.Printf (printf)
import qualified Data.Map as Map

import Hledger
import Hledger.Cli.CliOptions
import Hledger.Cli.Utils (writeOutputLazyText)
import Text.Tabular.AsciiWide
import Data.Time.Clock.POSIX (getPOSIXTime)


statsmode = hledgerCommandMode
  $(embedFileRelative "Hledger/Cli/Commands/Stats.txt")
  [flagReq  ["output-file","o"]   (\s opts -> Right $ setopt "output-file" s opts) "FILE" "write output to FILE."
  ]
  [generalflagsgroup1]
  hiddenflags
  ([], Just $ argsFlag "[QUERY]")

-- like Register.summarisePostings
-- | Print various statistics for the journal.
stats :: CliOpts -> Journal -> IO ()
stats opts@CliOpts{reportspec_=rspec, progstarttime_} j = do
  let today = _rsDay rspec
      q = _rsQuery rspec
      l = ledgerFromJournal q j
      intervalspans = snd $ reportSpanBothDates j rspec
      showstats = showLedgerStats l today
      (ls, txncounts) = unzip $ map showstats intervalspans
      numtxns = sum txncounts
      b = unlinesB ls
  writeOutputLazyText opts $ TB.toLazyText b
  t <- getPOSIXTime
  let dt = t - progstarttime_
  printf "Run time (throughput)    : %.2fs (%.0f txns/s)\n" 
    (realToFrac dt :: Float) (fromIntegral numtxns / realToFrac dt :: Float)

showLedgerStats :: Ledger -> Day -> DateSpan -> (TB.Builder, Int)
showLedgerStats l today span =
    (unlinesB $ map (renderRowB def{tableBorders=False, borderSpaces=False} . showRow) stats
    ,tnum)
  where
    showRow (label, value) = Group NoLine $ map (Header . textCell TopLeft)
      [fitText (Just w1) (Just w1) False True label `T.append` ": ", T.pack value]
    w1 = maximum $ map (T.length . fst) stats
    (stats, tnum) = ([
       ("Main file", path) -- ++ " (from " ++ source ++ ")")
      ,("Included files", unlines $ drop 1 $ journalFilePaths j)
      ,("Transactions span", printf "%s to %s (%d days)" (start span) (end span) days)
      ,("Last transaction", maybe "none" show lastdate ++ showelapsed lastelapsed)
      ,("Transactions", printf "%d (%0.1f per day)" tnum txnrate)
      ,("Transactions last 30 days", printf "%d (%0.1f per day)" tnum30 txnrate30)
      ,("Transactions last 7 days", printf "%d (%0.1f per day)" tnum7 txnrate7)
      ,("Payees/descriptions", show $ size $ fromList $ map (tdescription) ts)
      ,("Accounts", printf "%d (depth %d)" acctnum acctdepth)
      ,("Commodities", printf "%s (%s)" (show $ length cs) (T.intercalate ", " cs))
      ,("Market prices", printf "%s (%s)" (show $ length mktprices) (T.intercalate ", " mktpricecommodities))
    -- Transactions this month     : %(monthtxns)s (last month in the same period: %(lastmonthtxns)s)
    -- Unmarked transactions      : %(unmarked)s
    -- Days since reconciliation   : %(reconcileelapsed)s
    -- Days since last transaction : %(recentelapsed)s
     ] 
     ,tnum)
       where
         j = ljournal l
         path = journalFilePath j
         ts = sortOn tdate $ filter (spanContainsDate span . tdate) $ jtxns j
         as = nub $ map paccount $ concatMap tpostings ts
         cs = either error' Map.keys $ commodityStylesFromAmounts $ concatMap (amountsRaw . pamount) $ concatMap tpostings ts  -- PARTIAL:
         lastdate | null ts = Nothing
                  | otherwise = Just $ tdate $ last ts
         lastelapsed = fmap (diffDays today) lastdate
         showelapsed Nothing = ""
         showelapsed (Just days) = printf " (%d %s)" days' direction
                                   where days' = abs days
                                         direction | days >= 0 = "days ago" :: String
                                                   | otherwise = "days from now"
         tnum = length ts  -- Integer would be better
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
         mktprices = jpricedirectives j
         mktpricecommodities = nubSort $ map pdcommodity mktprices
