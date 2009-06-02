{-| 

Print some statistics for the ledger.

-}

module Commands.Stats
where
import Prelude hiding (putStr)
import qualified Data.Map as Map
import Data.Map ((!))
import Ledger
import Options
import System.IO.UTF8
import Utils (filterAndCacheLedgerWithOpts)


-- | Print various statistics for the ledger.
stats :: [Opt] -> [String] -> Ledger -> IO ()
stats opts args l = do
  today <- getCurrentDay
  putStr $ showStats opts args l today

showStats :: [Opt] -> [String] -> Ledger -> Day -> String
showStats opts args l today = 
    heading ++ (unlines $ map (\(a,b) -> printf fmt a b) stats)
    where
      heading = underline $ printf "Ledger statistics as of %s" (show today)
      fmt = "%-" ++ (show w1) ++ "s: %-" ++ (show w2) ++ "s"
      w1 = maximum $ map (length . fst) stats
      w2 = maximum $ map (length . show . snd) stats
      stats = [
         ("File", filepath $ rawledger l)
        ,("Period", printf "%s to %s (%d days)" (start span) (end span) days)
        ,("Transactions", printf "%d (%0.1f per day)" txns txnrate)
        ,("Transactions last 30 days", printf "%d (%0.1f per day)" txns30 txnrate30)
        ,("Payees/descriptions", show $ length $ nub $ map ltdescription rawledgertransactions)
        ,("Accounts", show $ length $ accounts l)
        ,("Commodities", show $ length $ commodities l)
      -- Transactions this month     : %(monthtxns)s (last month in the same period: %(lastmonthtxns)s)
      -- Uncleared transactions      : %(uncleared)s
      -- Days since reconciliation   : %(reconcileelapsed)s
      -- Days since last transaction : %(recentelapsed)s
       ]
           where 
             rawledgertransactions = ledger_txns $ rawledger l
             txns = length rawledgertransactions
             span = rawdatespan l
             start (DateSpan (Just d) _) = show d
             start _ = ""
             end (DateSpan _ (Just d)) = show d
             end _ = ""
             days = fromMaybe 0 $ daysInSpan span
             txnrate | days==0 = 0
                     | otherwise = fromIntegral txns / fromIntegral days :: Float
             txns30 = length $ filter withinlast30 rawledgertransactions
             withinlast30 t = (d>=(addDays (-30) today) && (d<=today)) where d = ltdate t
             txnrate30 = fromIntegral txns30 / 30 :: Float

