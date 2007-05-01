-- overview.hs - update an OVERVIEW file
--
-- OVERVIEW should begin with a list of module names (updated manually);
-- below that we generate a list of filename, function, type entries.
-- Useful for getting the big picture and refactoring.

import System
import System.Process
import IO
import Data.List
import Text.Printf

main = do
  old <- readFile "OVERVIEW"
  let preamble = takeWhile ((> 0) . length) $ lines old
  putStr $ unlines preamble
  let modules = concat $ map ((++ ".hs ") . dropWhile (== ' ')) preamble
  let grep = "grep -H '^\\w[^=]*::' " ++ modules
  (inp, out, err, pid) <- runInteractiveCommand grep
  waitForProcess pid
  grepoutput <- hGetContents out
  let groups = groupBy (\a b -> samefile a b) $ lines grepoutput
  sequence $ map printgroup groups
  putStr "\n"

printgroup ls = do putStr "\n"; sequence $ map printline ls

printline l = 
    putStrLn $ printf "%-22s %-40s %s" (file++":") code typedecl
    where
      (file, rest) = break (== ':') l
      (code, typedecl) = break (== ':') $ tail rest

samefile a b = ((takeWhile (/= ':') a) ++ ":") `isPrefixOf` b
