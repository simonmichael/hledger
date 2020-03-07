{-
Run doctests in Hledger source files under the current directory
(./Hledger.hs, ./Hledger/**, ./Text/**) using the doctest runner.

Arguments are case-insensitive file path substrings, to limit the files searched.
--verbose shows files being searched for doctests and progress while running.
--slow reloads ghci between each test (https://github.com/sol/doctest#a-note-on-performance).

Eg, in hledger source dir:

$ make ghci-doctest, :main [--verbose] [--slow] [CIFILEPATHSUBSTRINGS]

or:

$ stack test hledger-lib:test:doctests [--test-arguments '[--verbose] [--slow] [CIFILEPATHSUBSTRINGS]']

-}
-- This file can't be called doctest.hs ("File name does not match module name")


{-# LANGUAGE PackageImports #-}

import Control.Monad
import Data.Char
import Data.List
import System.Environment
import "Glob" System.FilePath.Glob
import Test.DocTest

main :: IO ()
main = do
  args <- getArgs
  let
    verbose = "--verbose" `elem` args
    slow    = "--slow" `elem` args
    pats    = filter (not . ("-" `isPrefixOf`)) args

  -- find source files
  sourcefiles <- (filter (not . isInfixOf "/.") . concat) <$> sequence [
     glob "Hledger.hs"
    ,glob "Hledger/**/*.hs"
    ,glob "Text/**/*.hs"
    ]

  -- filter by patterns (case insensitive infix substring match)
  let
    fs | null pats = sourcefiles
       | otherwise = [f | f <- sourcefiles, let f' = map toLower f, any (`isInfixOf` f') pats']
          where pats' = map (map toLower) pats
    fslen = length fs

  if (null fs)
  then do
    putStrLn $ "No file paths found matching: " ++ unwords pats

  else do
    putStrLn $
      "Loading and searching for doctests in "
      ++ show fslen
      ++ if fslen > 1 then " files, plus any files they import:" else " file, plus any files it imports:"
    when verbose $ putStrLn $ unwords fs

    doctest $
      (if verbose then ("--verbose" :) else id) $  -- doctest >= 0.15.0
      (if slow then id else ("--fast" :)) $        -- doctest >= 0.11.4
      fs
