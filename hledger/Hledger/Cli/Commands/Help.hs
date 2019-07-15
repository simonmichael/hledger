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
import Data.Char
import Data.List
import Data.Maybe
import Safe
import System.Console.CmdArgs.Explicit
import System.Environment
import System.IO

import Hledger.Utils (embedFileRelative)
import Hledger.Data.RawOptions
import Hledger.Data.Types
import Hledger.Cli.CliOptions
import Hledger.Cli.DocFiles
--import Hledger.Utils.Debug

helpmode = hledgerCommandMode
  $(embedFileRelative "Hledger/Cli/Commands/Help.txt")
  [flagNone ["info"]  (setboolopt "info")  "show the manual with info"
  ,flagNone ["man"]   (setboolopt "man")   "show the manual with man"
  ,flagNone ["pager"] (setboolopt "pager") "show the manual with $PAGER or less"
  ,flagNone ["cat"]   (setboolopt "cat")   "show the manual on stdout"
  ,flagNone ["help","h"]  (setboolopt "help")  "show this help"
  ]
  []
  []
  ([], Just $ argsFlag "[MANUAL]")

-- | List or display one of the hledger manuals in various formats.
-- You can select a docs viewer with one of the `--info`, `--man`, `--pager`, `--cat` flags.
-- Otherwise it will use the first available of: info, man, $PAGER, less, stdout
-- (and always stdout if output is non-interactive).
help' :: CliOpts -> Journal -> IO ()
help' opts _ = do
  exes <- likelyExecutablesInPath
  pagerprog <- fromMaybe "less" <$> lookupEnv "PAGER"
  interactive <- hIsTerminalDevice stdout
  let
    args = take 1 $ listofstringopt "args" $ rawopts_ opts
    topic = case args of
              [pat] -> headMay [t | t <- docTopics, map toLower pat `isInfixOf` t]
              _   -> Nothing
    [info, man, pager, cat] =
      [runInfoForTopic, runManForTopic, runPagerForTopic pagerprog, printHelpForTopic]
    viewer
      | boolopt "info"  $ rawopts_ opts = info
      | boolopt "man"   $ rawopts_ opts = man
      | boolopt "pager" $ rawopts_ opts = pager
      | boolopt "cat"   $ rawopts_ opts = cat
      | not interactive                 = cat
      | "info"    `elem` exes           = info
      | "man"     `elem` exes           = man
      | pagerprog `elem` exes           = pager
      | otherwise                       = cat
  case topic of
    Nothing -> putStrLn $ unlines [
       "Please choose a manual by typing \"hledger help MANUAL\" (any substring is ok)."
      ,"A viewer (info, man, a pager, or stdout) will be auto-selected,"
      ,"or type \"hledger help -h\" to see options. Manuals available:"
      ]
      ++ "\n " ++ unwords docTopics
    Just t  -> viewer t
