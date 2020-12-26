{-|

The @files@ command lists included files.

-}

{-# LANGUAGE TemplateHaskell #-}

module Hledger.Cli.Commands.Files (
  filesmode
 ,files
) where

import qualified Data.Text as T
import Safe (headMay)

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
  regex <- mapM (either fail pure . toRegex . T.pack) $ headMay args
  let files = maybe id (filter . regexMatch) regex
              $ map fst
              $ jfiles j
  mapM_ putStrLn files
