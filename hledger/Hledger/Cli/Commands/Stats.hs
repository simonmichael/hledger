{-|

Print some statistics for the journal.

-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Hledger.Cli.Commands.Stats (
  statsmode
 ,stats
)
where

import Data.Default (def)
import System.FilePath (takeFileName)
import Data.List (intercalate, nub, sortOn)
import Data.List.Extra (nubSort)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.HashSet (size, fromList)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TB
import Data.Time.Calendar (Day, addDays, diffDays)
import Data.Time.Clock.POSIX (getPOSIXTime)
import GHC.Stats
import System.Console.CmdArgs.Explicit hiding (Group)
import System.Mem (performMajorGC)
import Text.Printf (printf)
import Text.Tabular.AsciiWide

import Hledger
import Hledger.Cli.CliOptions
import Hledger.Cli.Utils (writeOutputLazyText)


statsmode = hledgerCommandMode
  $(embedFileRelative "Hledger/Cli/Commands/Stats.txt")
  [ flagNone ["verbose","v"]    (setboolopt "verbose") "show more detailed output"
  ,flagReq  ["output-file","o"] (\s opts -> Right $ setopt "output-file" s opts) "FILE" "write output to FILE."
  ]
  cligeneralflagsgroups1
  hiddenflags
  ([], Just $ argsFlag "[QUERY]")

-- like Register.summarisePostings
-- | Print various statistics for the journal.
stats :: CliOpts -> Journal -> IO ()
stats opts@CliOpts{rawopts_=rawopts, reportspec_=rspec, progstarttime_} j = do
  let today = _rsDay rspec
      verbose = boolopt "verbose" rawopts
      q = _rsQuery rspec
      l = ledgerFromJournal q j
      intervalspans = snd $ reportSpanBothDates j rspec
      ismultiperiod = length intervalspans > 1
      (ls, txncounts) = unzip . map (showLedgerStats verbose l today) $ maybePeriodDataToDateSpans intervalspans
      numtxns = sum txncounts
      txt = (if ismultiperiod then id else TL.init) $ TB.toLazyText $ unlinesB ls
  writeOutputLazyText opts txt
  t <- getPOSIXTime
  let dt = t - progstarttime_
  rtsStatsEnabled <- getRTSStatsEnabled
  if rtsStatsEnabled
  then do
    -- do one last GC for most accurate memory stats; probably little effect, hopefully little wasted time
    performMajorGC
    RTSStats{..} <- getRTSStats
    printf
      (intercalate ", "
        ["Runtime stats       : %.2f s elapsed"  -- keep synced
        ,"%.0f txns/s"                           --
        -- ,"%0.0f MB avg live"
        ,"%0.0f MB live"
        ,"%0.0f MB alloc"
        -- ,"(%0.0f MiB"
        -- ,"%0.0f MiB)"
        ] ++ "\n")
      (realToFrac dt :: Float)
      (fromIntegral numtxns / realToFrac dt :: Float)
      -- (toMegabytes $ fromIntegral cumulative_live_bytes / fromIntegral major_gcs)
      (toMegabytes max_live_bytes)
      (toMegabytes max_mem_in_use_bytes)
  else
    printf
      (intercalate ", "
        ["Runtime stats       : %.2f s elapsed"  -- keep
        ,"%.0f txns/s"
        ] ++ "\n(add +RTS -T -RTS for more)\n")
      (realToFrac dt :: Float)
      (fromIntegral numtxns / realToFrac dt :: Float)

toMegabytes n = realToFrac n / 1000000 ::Float  -- SI preferred definition, 10^6
-- toMebibytes n = realToFrac n / 1048576 ::Float  -- traditional computing definition, 2^20

showLedgerStats :: Bool -> Ledger -> Day -> DateSpan -> (TB.Builder, Int)
showLedgerStats verbose l today spn =
    (unlinesB $ map (renderRowB def{tableBorders=False, borderSpaces=False} . showRow) stts
    ,tnum)
  where
    showRow (label, val) = Group NoLine $ map (Header . textCell TopLeft)
      [fitText (Just w) (Just w) False True label `T.append` ": ", T.pack val]
    w = 20  -- keep synced with labels above
    -- w = maximum $ map (T.length . fst) stts
    (stts, tnum) = ([
       ("Main file", path') -- ++ " (from " ++ source ++ ")")
      ,("Included files", if verbose then unlines includedpaths else show (length includedpaths))
      ,("Txns span", printf "%s to %s (%d days)" (showstart spn) (showend spn) days)
      ,("Last txn", maybe "none" show lastdate ++ showelapsed lastelapsed)
      ,("Txns", printf "%d (%0.1f per day)" tnum txnrate)
      ,("Txns last 30 days", printf "%d (%0.1f per day)" tnum30 txnrate30)
      ,("Txns last 7 days", printf "%d (%0.1f per day)" tnum7 txnrate7)
      ,("Payees/descriptions", show $ size $ fromList $ map (tdescription) ts)
      ,("Accounts", printf "%d (depth %d)" acctnum acctdepth)
      ,("Commodities",   printf "%s%s" (show $ length cs)        (if verbose then " (" <> T.intercalate ", " cs <> ")" else ""))
      ,("Market prices", printf "%s%s" (show $ length mktprices) (if verbose then " (" <> T.intercalate ", " mktpricecommodities <> ")" else ""))
    -- Txns this month     : %(monthtxns)s (last month in the same period: %(lastmonthtxns)s)
    -- Unmarked txns      : %(unmarked)s
    -- Days since reconciliation   : %(reconcileelapsed)s
    -- Days since last txn : %(recentelapsed)s
     ] 
     ,tnum1)
       where
         j = ljournal l
         path' = if verbose then path else ".../" <> takeFileName path where path = journalFilePath j
         includedpaths = drop 1 $ journalFilePaths j
         ts = sortOn tdate $ filter (spanContainsDate spn . tdate) $ jtxns j
         as = nub $ map paccount $ concatMap tpostings ts
         cs = either error' Map.keys $ commodityStylesFromAmounts $ concatMap (amountsRaw . pamount) $ concatMap tpostings ts  -- PARTIAL:
         lastdate | null ts = Nothing
                  | otherwise = Just $ tdate $ last ts
         lastelapsed = fmap (diffDays today) lastdate
         showelapsed Nothing = ""
         showelapsed (Just dys) = printf " (%d %s)" dys' direction
                                   where dys' = abs dys
                                         direction | dys >= 0 = "days ago" :: String
                                                   | otherwise = "days from now"
         tnum1 = length ts  -- Integer would be better
         showstart (DateSpan (Just efd) _) = show $ fromEFDay efd
         showstart _ = ""
         showend (DateSpan _ (Just efd)) = show $ fromEFDay efd
         showend _ = ""
         days = fromMaybe 0 $ daysInSpan spn
         txnrate | days==0 = 0
                 | otherwise = fromIntegral tnum1 / fromIntegral days :: Double
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
