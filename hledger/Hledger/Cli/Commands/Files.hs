{-|

The @files@ command lists included files.

-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Hledger.Cli.Commands.Files (
  filesmode
 ,files
) where

import Data.List
import Safe

import Hledger
import Prelude hiding (putStrLn)
import Hledger.Utils.UTF8IOCompat (putStrLn)
import Hledger.Cli.CliOptions


-- | Command line options for this command.
filesmode = hledgerCommandMode
  $(embedFileRelative "Hledger/Cli/Commands/Files.txt")
  []
  [generalflagsgroup2]
  []
  ([], Just $ argsFlag "[REGEX]")

-- | The files command.
files :: CliOpts -> Journal -> IO ()
files CliOpts{rawopts_=rawopts} j = do
  let args = listofstringopt "args" rawopts
      regex = headMay args
      files = maybe id (filter . regexMatches) regex
              $ map fst
              $ jfiles j
  mapM_ putStrLn files
