#!/usr/bin/env runhaskell
{-
overview.hs - print an overview of functions from a given list of modules
Simon Michael 2007

Usage: ./overview.hs somefile

where somefile, typically your main module, contains the word "overview"
followed by a blank-line-delimited list of module names, like so:

firstmodule
anothermodule
 submodule

Useful for getting the big picture and refactoring.  
-}

import System
import System.Process
import IO
import Data.List
import Text.Printf

main = do
  args <- getArgs
  file <- readFile $ head args
  let mods = takeWhile ((> 0) . length) $ 
             dropWhile ((== 0) . length) $ 
             dropWhile ((> 0) . length) $ 
             dropWhile (notElem "overview" . words) $ 
             lines file
  putStr $ unlines mods
  let files = concat $ map ((++ ".hs ") . dropWhile (== ' ')) mods
  let grep = "grep -H '^\\w[^=]*::' " ++ files
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
