#!/usr/bin/env runhaskell
{-
Display the current darcs repository's last tag and the number of days since.
Similar to:
$ darcs changes --to-tag . --from-tag .|head -n 1 |cut -d' ' -f-7 |xargs -I {} date -d "{}" +%s |xargs -I {} expr \( $(date +%s) - {} \) / 60 / 60 / 24
-}
import Data.Time
import System.Locale
import System.Process

main = do
  s <- readProcess "darcs" ["changes","--from-tag",".","--to-tag","."] ""
  let datestr = unwords $ take 6 $ words $ head $ lines s
      date = readTime defaultTimeLocale "%a %b %e %H:%M:%S %Z %Y" datestr :: Day
  today <- getCurrentDay
  putStrLn $ show (diffDays today date) ++ " days since the last tag:\n"
  putStr s

getCurrentDay = do
    t <- getZonedTime
    return $ localDay (zonedTimeToLocalTime t)
