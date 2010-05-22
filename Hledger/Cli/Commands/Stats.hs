{-# LANGUAGE CPP #-}
{-|

Print some statistics for the ledger.

-}

module Hledger.Cli.Commands.Stats
where
import Hledger.Data
import Hledger.Cli.Options
#if __GLASGOW_HASKELL__ <= 610
import Prelude hiding ( putStr )
import System.IO.UTF8
#endif


-- | Print various statistics for the ledger.
stats :: [Opt] -> [String] -> Ledger -> IO ()
stats opts args l = do
  today <- getCurrentDay
  putStr $ showStats opts args (filterLedger nullfilterspec l) today

showStats :: [Opt] -> [String] -> Ledger -> Day -> String
showStats _ _ l today =
    heading ++ unlines (map (uncurry (printf fmt)) stats)
    where
      heading = underline $ printf "Ledger statistics as of %s" (show today)
      fmt = "%-" ++ show w1 ++ "s: %-" ++ show w2 ++ "s"
      w1 = maximum $ map (length . fst) stats
      w2 = maximum $ map (length . show . snd) stats
      stats = [
         ("File", filepath $ journal l)
        ,("Period", printf "%s to %s (%d days)" (start span) (end span) days)
        ,("Last transaction", maybe "none" show lastdate ++ showelapsed lastelapsed)
        ,("Transactions", printf "%d (%0.1f per day)" tnum txnrate)
        ,("Transactions last 30 days", printf "%d (%0.1f per day)" tnum30 txnrate30)
        ,("Transactions last 7 days", printf "%d (%0.1f per day)" tnum7 txnrate7)
--        ,("Payees/descriptions", show $ length $ nub $ map tdescription ts)
        ,("Accounts", show $ length $ accounts l)
        ,("Account tree depth", show $ maximum $ map (accountNameLevel.aname) $ accounts l)
        ,("Commodities", printf "%s (%s)" (show $ length $ cs) (intercalate ", " $ sort $ map symbol cs)) 
      -- Transactions this month     : %(monthtxns)s (last month in the same period: %(lastmonthtxns)s)
      -- Uncleared transactions      : %(uncleared)s
      -- Days since reconciliation   : %(reconcileelapsed)s
      -- Days since last transaction : %(recentelapsed)s
       ]
           where
             ts = sortBy (comparing tdate) $ jtxns $ journal l
             lastdate | null ts = Nothing
                      | otherwise = Just $ tdate $ last ts
             lastelapsed = maybe Nothing (Just . diffDays today) lastdate
             showelapsed Nothing = ""
             showelapsed (Just days) = printf " (%d %s)" days' direction
                                       where days' = abs days
                                             direction | days >= 0 = "days ago"
                                                       | otherwise = "days from now"
             tnum = length ts
             span = rawdatespan l
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
             cs = commodities l

