#!/usr/bin/env runhaskell
{-
generateledger.hs NUMTXNS NUMACCTS ACCTDEPTH

Outputs a dummy ledger file with the specified number of transactions,
number of accounts, and account tree depth. Useful for
testing/profiling/benchmarking.

-}

module Main
where
import System.Environment
import Control.Monad
import Data.Time.LocalTime
import Data.Time.Calendar
import Text.Printf
import Numeric

main = do
  args <- getArgs
  let [numtxns, numaccts, acctdepth] = map read args :: [Int]
  today <- getCurrentDay
  let (year,_,_) = toGregorian today
  let d = fromGregorian (year-1) 1 1
  let dates = iterate (addDays 1) d
  let accts = pair $ cycle $ take numaccts $ uniqueacctnames acctdepth
  mapM_ (\(n,d,(a,b)) -> putStr $ showtxn n d a b) $ take numtxns $ zip3 [1..] dates accts
  return ()

showtxn :: Int -> Day -> String -> String -> String
showtxn txnno date acct1 acct2 =
    printf "%s transaction %d\n  %-40s  %2d\n  %-40s  %2d\n\n" d txnno acct1 amt acct2 (-amt)
    where
      d = show date
      amt = 1::Int

uniqueacctnames :: Int -> [String]
uniqueacctnames depth = uniqueacctnames' depth uniquenames
    where uniquenames = map hex [1..] where hex = flip showHex ""

uniqueacctnames' depth uniquenames = group some ++ uniqueacctnames' depth rest
    where (some, rest) = splitAt depth uniquenames

-- group ["a", "b", "c"] = ["a","a:b","a:b:c"]
group :: [String] -> [String]
group [] = []
group (a:as) = a : map ((a++":")++) (group as)

pair :: [a] -> [(a,a)]
pair [] = []
pair [a] = [(a,a)]
pair (a:b:rest) = (a,b):pair rest

getCurrentDay :: IO Day
getCurrentDay = do
    t <- getZonedTime
    return $ localDay (zonedTimeToLocalTime t)

