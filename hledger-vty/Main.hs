{-# LANGUAGE CPP #-}
{-|
hledger-vty - a hledger add-on providing a curses-style interface.
Copyright (c) 2007-2010 Simon Michael <simon@joyful.com>
Released under GPL version 3 or later.
-}

module Main where

#if __GLASGOW_HASKELL__ <= 610
import Prelude hiding (putStr, putStrLn)
import System.IO.UTF8 (putStr, putStrLn)
#endif

import Hledger.Cli.Options
import Hledger.Cli.Utils (withJournalDo)
import Hledger.Cli.Version (versionmsg, binaryfilename)
import Hledger.Data
import Hledger.Vty

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
       | cmd `isPrefixOf` "vty"       = withJournalDo opts args cmd vty
       | otherwise                    = putStr help1

      defaultcmd = Just vty
