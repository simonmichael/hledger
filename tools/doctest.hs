#!/usr/bin/env runhaskell
{- |
Extract (shell) tests from haddock comments in Haskell code, run them and
verify expected output, like Python's doctest system.

Here, a doctest is a haddock literal block whose first line begins with a
$ (leading whitespace ignored). The rest of the line is a shell command
and the remaining lines are the expected output.

Usage example: $ doctest.hs doctest.hs

Doctest examples:

@
$ ls doctest.hs
This doctest will fail.
@

@
$ ls doctest.hs
doctest.hs
@

After writing this I found the doctest on hackage; that one runs haskell
expressions in comments, converting them to hunit tests. We might add this
to that, and/or add this to hledger's built-in test runner.

-}

module Main where
import Data.List (isPrefixOf)
import System (getArgs)
import System.Exit (exitFailure, exitWith, ExitCode(ExitSuccess)) -- base 3 compatible
import System.IO (hGetContents, hPutStr, hPutStrLn, stderr)
import System.Process (runInteractiveCommand, waitForProcess)
import Text.Printf (printf)

main = do
  f <- head `fmap` getArgs
  s <- readFile f
  let tests = doctests s
  putStrLn $ printf "Running %d doctests from %s" (length tests) f
  ok <-  mapM runShellDocTest $ doctests s
  if any not ok then exitFailure else exitWith ExitSuccess

runShellDocTest :: String -> IO Bool
runShellDocTest s = do
  let (cmd, expected) = splitDocTest s
  putStr $ printf "Testing: %s .. " cmd
  (_, out, _, h) <- runInteractiveCommand cmd
  exit <- waitForProcess h
  output <- hGetContents out
  if exit == ExitSuccess
    then
      if output == expected
        then do
          putStrLn "ok"
          return True
        else do
          hPutStr stderr $ printf "FAILED\nExpected:\n%sGot:\n%s" expected output
          return False
    else do
      hPutStrLn stderr $ printf "ERROR: %s" (show exit)
      return False

splitDocTest s = (strip $ drop 1 $ strip $ head ls, unlines $ tail ls)
    where ls = lines s

-- extract doctests from haskell source code
doctests :: String -> [String]
doctests s = filter isDocTest $ haddockLiterals s
    where
      isDocTest s = (("$" `isPrefixOf`) . dropws) $ head $ lines s

-- extract haddock literal blocks from haskell source code
haddockLiterals :: String -> [String]
haddockLiterals "" = []
haddockLiterals s | null lit = []
                  | otherwise = [lit] ++ haddockLiterals rest
    where
      ls = drop 1 $ dropWhile (not . isLiteralBoundary) $ lines s
      lit = unlines $ takeWhile (not . isLiteralBoundary) ls
      rest = unlines $ drop 1 $ dropWhile (not . isLiteralBoundary) ls
      isLiteralBoundary = (== "@") . strip

strip = dropws . reverse . dropws . reverse
dropws = dropWhile (`elem` " \t")
