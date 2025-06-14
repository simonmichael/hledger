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

import Data.Maybe
import Safe (headMay)
import System.Console.CmdArgs.Explicit
import System.Environment
import System.IO

import Hledger.Cli.CliOptions
import Hledger.Cli.DocFiles
import Hledger.Data.RawOptions
import Hledger.Data.Types
import Hledger.Utils (embedFileRelative)
--import Hledger.Utils.Debug

helpmode = hledgerCommandMode
  $(embedFileRelative "Hledger/Cli/Commands/Help.txt")
  -- The help-* names avoid a clash with the --info and --man flags handled in Cli.hs.
  [flagNone ["i"] (setboolopt "help-i")  "show the manual with info"
  ,flagNone ["m"] (setboolopt "help-m")   "show the manual with man"
  ,flagNone ["p"] (setboolopt "help-p") "show the manual with $PAGER or less\n(less is always used if TOPIC is specified)"
  ]
  [(helpflagstitle, helpflags)]
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
      | boolopt "help-i" $ rawopts_ opts = info
      | boolopt "help-m" $ rawopts_ opts = man
      | boolopt "help-p" $ rawopts_ opts = pager
      | not interactive                  = cat
      | "info"    `elem` exes            = info
      | "man"     `elem` exes            = man
      | pagerprog `elem` exes            = pager
      | "less"    `elem` exes            = pager
      | otherwise                        = cat

  viewer "hledger" mtopic
