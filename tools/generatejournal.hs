#!/usr/bin/env runhaskell
{-
generatejournal.hs NUMTXNS NUMACCTS ACCTDEPTH [--chinese|--mixed]

Outputs a dummy journal file with the specified number of
transactions, number of accounts, and account tree depth. By default
it uses only ascii characters, with --chinese it uses wide chinese
characters, or with --mixed it uses both.  These files are used for
testing, benchmarking, profiling, etc.
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
  today <- getCurrentDay
  let (year,_,_) = toGregorian today
  let d = fromGregorian (year-1) 1 1
  let dates = iterate (addDays 1) d
  let accts = pair $ cycle $ take numaccts $ uniqueAccountNames opts acctdepth
  mapM_ (\(n,d,(a,b)) -> putStr $ showtxn n d a b) $ take numtxns $ zip3 [1..] dates accts
  return ()

showtxn :: Int -> Day -> String -> String -> String
showtxn txnno date acct1 acct2 =
    printf "%s transaction %d\n  %-40s  %2d\n  %-40s  %2d\n\n" d txnno acct1 amt acct2 (-amt)
    where
      d = show date
      amt = 1::Int

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

getCurrentDay :: IO Day
getCurrentDay = do
    t <- getZonedTime
    return $ localDay (zonedTimeToLocalTime t)

