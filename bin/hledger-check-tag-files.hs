#!/usr/bin/env stack
-- stack script --compile --resolver nightly-2020-03-26 --package hledger-lib --package hledger --package directory --package text
-- Add this to see more progress: --verbosity info
-- Change --resolver to an already-installed one if you like
{-
hledger-check-tag-files script (stack version)
Read the default journal and give an error if any tag values
containing '/' do not exist as file paths.
Usage:

$ hledger-check-tag-files.hs    # compiles if needed

or:

$ hledger check-tag-files       # compiles if there's no compiled version
-}

import Control.Monad
import qualified Data.Text as T
import Hledger.Cli
import System.Directory
import System.Exit

main = withJournalDo defcliopts $ \j -> do
  let filetags = [ (t,v)
                 | (t',v') <- concatMap transactionAllTags $ jtxns j
                 , let t = T.unpack t'
                 , let v = T.unpack v'
                 , '/' `elem` v
                 ]
  forM_ filetags $ \(t,f) -> do
    exists <- doesFileExist f
    when (not exists) $ do
      putStrLn $ "file not found in tag: " ++ t ++ ": " ++ f
      exitFailure
