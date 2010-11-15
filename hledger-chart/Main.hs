{-# LANGUAGE CPP #-}
{-|
hledger-web - a hledger add-on providing rudimentary pie chart generation.
Copyright (c) 2007-2010 Simon Michael <simon@joyful.com>
Released under GPL version 3 or later.
-}

module Main where

#if __GLASGOW_HASKELL__ <= 610
import Prelude hiding (putStr, putStrLn)
import System.IO.UTF8 (putStr, putStrLn)
#endif

import Hledger.Chart
import Hledger.Cli.Commands
import Hledger.Cli.Options
import Hledger.Cli.Tests
import Hledger.Cli.Utils (withJournalDo)
import Hledger.Cli.Version (versionmsg, binaryfilename)
import Hledger.Data

main :: IO ()
main = do
  (opts, cmd, args) <- parseArguments
  run cmd opts args
    where
      run cmd opts args
       | Help `elem` opts             = putStr help1
       | HelpOptions `elem` opts      = putStr help2
       | HelpAll `elem` opts          = putStr $ help1 ++ "\n" ++ help2
       | Version `elem` opts          = putStrLn versionmsg
       | BinaryFilename `elem` opts   = putStrLn binaryfilename
       | null cmd                     = maybe (putStr help1) (withJournalDo opts args cmd) defaultcmd
       | cmd `isPrefixOf` "chart"     = withJournalDo opts args cmd chart
       | otherwise                    = putStr help1

      defaultcmd = Just chart
