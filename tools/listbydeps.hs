#!/usr/bin/env runhaskell

import System
import System.Directory
import IO
import Data.List
import Data.Char
import Data.Maybe
import Data.Ord

{-
    Read a Haskell *.hs file and get a list of all the modules
    that it imports.
-}
findDeps base pkg = do
        let hi = base ++ map dotToSlash pkg ++ ".hs"
        ex <- doesFileExist hi
        if not ex then return [] else do
            src <- readFile hi
            let imps = filter isImport (lines src)
            return $ mapMaybe getTargetModule imps

    where dotToSlash '.' = '/'
          dotToSlash x   = x

          isImport (' ':t)  = isImport t
          isImport ('\t':t) = isImport t
          isImport t        = "import" `isPrefixOf` t

          getTargetModule s = let pre = takeWhile (/= '(') s in
                              find (isUpper . head) (words pre)

{-
    Find the transitive, reflexive closure of the relation defined
    by the findDeps function.  This returns a list of ALL modules
    that this one uses or depends on, directly or indirectly.
-}
allDeps base mod = allDeps' [mod] [mod] where
    allDeps' (m:ms) full = do d <- findDeps base m
                              let d' = filter (not . flip elem full) d
                              t <- allDeps' (d' ++ ms) (d' ++ full)
                              return (m : t)
    allDeps' [] _        = return []

{-
    Usage: OrderByComplexity  

        = directory where source code is found.  This MUST
                end in '/'
     = file that lists the modules you're interested in,
                one per line.  This is often taken from a .cabal
-}
main = do [ base, pkgFile ] <- getArgs
          pkgStr            <- readFile pkgFile
          let pkgs           = lines pkgStr
          mods              <- mapM (allDeps base) pkgs
          let deps           = zip pkgs mods
          let deps'          = sortBy (comparing fst) deps
          mapM_ (print . fst) (sortBy (comparing $ length . snd) deps')

