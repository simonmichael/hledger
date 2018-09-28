{-|

The @files@ command lists included files.

-}

{-# LANGUAGE OverloadedStrings #-}

module Hledger.Cli.Commands.Files (
  filesmode
 ,files
) where

import Data.List
-- import Data.Text (Text)
import Safe
import System.Console.CmdArgs.Explicit as C

import Hledger
import Prelude hiding (putStrLn)
import Hledger.Utils.UTF8IOCompat (putStrLn)
import Hledger.Cli.CliOptions


-- | Command line options for this command.
filesmode = (defCommandMode $ ["files"] ) {
  modeHelp = "show names of included files" 
 ,modeHelpSuffix = [
     "This command lists names of all files included in the parsed journal(s)." 
    ,"With REGEX argument will list only files matching regular expression (case sensitive)."
   ]
 ,modeGroupFlags = C.Group {
     groupUnnamed = []
    ,groupHidden = []
    ,groupNamed = [generalflagsgroup2]
    }
 ,modeArgs=  ([], Just $ argsFlag "[REGEX]")
 }

-- | The files command.
files :: CliOpts -> Journal -> IO ()
files CliOpts{rawopts_=rawopts} j = do
  let args = listofstringopt "args" rawopts
      regex = headMay args
      files = (maybe id (filter . regexMatches) regex) 
              $ map fst 
              $ jfiles j
  mapM_ putStrLn files
