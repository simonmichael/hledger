{-|

The help command.

|-}
--TODO rename manuals
--TODO substring matching

{-# LANGUAGE PackageImports #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Hledger.Cli.Commands.Help (

   helpmode
  ,help'

  ) where

import Prelude ()
import "base-compat-batteries" Prelude.Compat
import Data.Maybe
import System.Console.CmdArgs.Explicit
import System.Environment
import System.IO

import Hledger.Utils (embedFileRelative)
import Hledger.Data.RawOptions
import Hledger.Data.Types
import Hledger.Cli.CliOptions
import Hledger.Cli.DocFiles
import Safe (headMay)
--import Hledger.Utils.Debug

helpmode = hledgerCommandMode
  $(embedFileRelative "Hledger/Cli/Commands/Help.txt")
  [flagNone ["i"]  (setboolopt "info")  "show the manual with info"
  ,flagNone ["m"]   (setboolopt "man")   "show the manual with man"
  ,flagNone ["p"] (setboolopt "pager") "show the manual with $PAGER or less"
  ,flagNone ["help","h"]  (setboolopt "help")  "show this help"
  ]
  []
  []
  ([], Just $ argsFlag "[TOPIC]")

-- | Display the hledger manual in various formats.
-- You can select a docs viewer with one of the `--info`, `--man`, `--pager` flags.
-- Otherwise it will use the first available of: info, man, $PAGER, less, stdout
-- (and always stdout if output is non-interactive).
help' :: CliOpts -> Journal -> IO ()
help' opts _ = do
  exes <- likelyExecutablesInPath
  pagerprog <- fromMaybe "less" <$> lookupEnv "PAGER"
  interactive <- hIsTerminalDevice stdout
  let
    args = take 1 $ listofstringopt "args" $ rawopts_ opts
    mtopic = headMay args
    [info, man, pager, cat] =
      [runInfoForTopic, runManForTopic, runPagerForTopic, printHelpForTopic]
    viewer
      | boolopt "info"  $ rawopts_ opts = info
      | boolopt "man"   $ rawopts_ opts = man
      | boolopt "pager" $ rawopts_ opts = pager
      | not interactive                 = cat
      | "info"    `elem` exes           = info
      | "man"     `elem` exes           = man
      | pagerprog `elem` exes           = pager
      | "less"    `elem` exes           = pager
      | otherwise                       = cat

  viewer "hledger" mtopic
