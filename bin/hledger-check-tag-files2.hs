#!/usr/bin/env cabal
{- cabal:
build-depends: base, directory, hledger-lib, hledger, text
-}
{-
hledger-check-tag-files script (cabal version, requires cabal-install 3.0.0.0+)
Read the default journal and give an error if any tag values
containing '/' do not exist as file paths.
Usage:

$ hledger-check-tag-files.hs    # compiles every time (?)

or:

$ hledger check-tag-files       # compiles every time (?)
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
