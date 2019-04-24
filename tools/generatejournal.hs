#!/usr/bin/env runhaskell
{-
generatejournal.hs NUMTXNS NUMACCTS ACCTDEPTH [--chinese|--mixed]

This generates synthetic journal data for benchmarking & profiling. It
prints a dummy journal on stdout, with NUMTXNS transactions, one per
day, using NUMACCTS account names with depths up to ACCTDEPTH. It will
also contain NUMACCTS P records, one per day. By default it uses only
ascii characters, with --chinese it uses wide chinese characters, or
with --mixed it uses both.
-}

module Main
where
import Data.Char
import Data.List
import Data.Time.Calendar
import Data.Time.LocalTime
import Numeric
import System.Environment
import Text.Printf
-- import Hledger.Utils.Debug

main = do
  rawargs <- getArgs
  let (opts,args) = partition (isPrefixOf "-") rawargs
  let [numtxns, numaccts, acctdepth] = map read args :: [Int]
  -- today <- getCurrentDay
  -- let (year,_,_) = toGregorian today
  let d = fromGregorian 2000 1 1
  let dates = iterate (addDays 1) d
  let accts = pair $ cycle $ take numaccts $ uniqueAccountNames opts acctdepth
  mapM_ (\(n,d,(a,b)) -> putStr $ showtxn n d a b) $ take numtxns $ zip3 [1..] dates accts
  let rates = [0.70, 0.71 .. 1.3]
  mapM_ (\(d,rate) -> putStr $ showmarketprice d rate) $ take numtxns $ zip dates (cycle $ rates ++ init (tail (reverse rates)))
  return ()

showtxn :: Int -> Day -> String -> String -> String
showtxn txnno date acct1 acct2 =
    printf "%s transaction %d\n  %-40s  %2d A\n  %-40s  %2d A\n\n" d txnno acct1 amt acct2 (-amt)
    where
      d = show date
      amt = txnno

showmarketprice :: Day -> Double -> String
showmarketprice date rate = printf "P %s A  %.2f B\n" (show date) rate

uniqueAccountNames :: [String] -> Int -> [String]
uniqueAccountNames opts depth =
  mkacctnames uniquenames
  where
    mkacctnames names = mkacctnamestodepth some ++ mkacctnames rest
      where
        (some, rest) = splitAt depth names
        -- mkacctnamestodepth ["a", "b", "c"] = ["a","a:b","a:b:c"]
        mkacctnamestodepth :: [String] -> [String]
        mkacctnamestodepth [] = []
        mkacctnamestodepth (a:as) = a : map ((a++":")++) (mkacctnamestodepth as)
    uniquenames
      | "--mixed" `elem` opts   = concat $ zipWith (\a b -> [a,b]) uniqueNamesHex uniqueNamesWide
      | "--chinese" `elem` opts = uniqueNamesWide
      | otherwise               = uniqueNamesHex

uniqueNamesHex = map hex [1..] where hex = flip showHex ""

uniqueNamesWide = concat [sequences n wideChars | n <- [1..]]

-- Get the sequences of specified size starting at each element of a list,
-- cycling it if needed to fill the last sequence. If the list's elements
-- are unique, then the sequences will be too.
sequences :: Show a => Int -> [a] -> [[a]]
sequences n l = go l
  where
    go [] = []
    go l' = s : go (tail l')
      where
        s' = take n l'
        s | length s' == n = s'
          | otherwise      = take n (l' ++ cycle l)

wideChars = map chr [0x3400..0x4db0]


pair :: [a] -> [(a,a)]
pair [] = []
pair [a] = [(a,a)]
pair (a:b:rest) = (a,b):pair rest

-- getCurrentDay :: IO Day
-- getCurrentDay = do
--     t <- getZonedTime
--     return $ localDay (zonedTimeToLocalTime t)

