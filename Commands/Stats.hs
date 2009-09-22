{-| 

Print some statistics for the ledger.

-}

module Commands.Stats
where
import Prelude hiding (putStr)
import Ledger
import Options
import System.IO.UTF8


-- | Print various statistics for the ledger.
stats :: [Opt] -> [String] -> Ledger -> IO ()
stats opts args l = do
  today <- getCurrentDay
  putStr $ showStats opts args l today

showStats :: [Opt] -> [String] -> Ledger -> Day -> String
showStats _ _ l today = 
    heading ++ unlines (map (\(a,b) -> printf fmt a b) stats)
    where
      heading = underline $ printf "Ledger statistics as of %s" (show today)
      fmt = "%-" ++ show w1 ++ "s: %-" ++ show w2 ++ "s"
      w1 = maximum $ map (length . fst) stats
      w2 = maximum $ map (length . show . snd) stats
      stats = [
         ("File", filepath $ rawledger l)
        ,("Period", printf "%s to %s (%d days)" (start span) (end span) days)
        ,("Transactions", printf "%d (%0.1f per day)" tnum txnrate)
        ,("Transactions last 30 days", printf "%d (%0.1f per day)" tnum30 txnrate30)
        ,("Transactions last 7 days", printf "%d (%0.1f per day)" tnum7 txnrate7)
        ,("Last transaction", maybe "none" show lastdate ++
                              maybe "" (printf " (%d days ago)") lastelapsed)
--        ,("Payees/descriptions", show $ length $ nub $ map ltdescription ts)
        ,("Accounts", show $ length $ accounts l)
        ,("Commodities", show $ length $ commodities l)
      -- Transactions this month     : %(monthtxns)s (last month in the same period: %(lastmonthtxns)s)
      -- Uncleared transactions      : %(uncleared)s
      -- Days since reconciliation   : %(reconcileelapsed)s
      -- Days since last transaction : %(recentelapsed)s
       ]
           where 
             ts = sortBy (comparing ltdate) $ ledger_txns $ rawledger l
             lastdate | null ts = Nothing
                      | otherwise = Just $ ltdate $ last ts
             lastelapsed = maybe Nothing (Just . diffDays today) lastdate
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
             withinlast30 t = d >= addDays (-30) today && (d<=today) where d = ltdate t
             txnrate30 = fromIntegral tnum30 / 30 :: Double
             tnum7 = length $ filter withinlast7 ts
             withinlast7 t = d >= addDays (-7) today && (d<=today) where d = ltdate t
             txnrate7 = fromIntegral tnum7 / 7 :: Double

