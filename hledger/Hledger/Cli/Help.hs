{-|

The help command.

|-}
--TODO rename manuals
--TODO substring matching

module Hledger.Cli.Help (

   helpmode
  ,help'

  ) where

import Prelude ()
import Prelude.Compat
import Data.List
import Data.Maybe
import System.Console.CmdArgs.Explicit
import System.Environment
import System.IO

import Hledger.Data.RawOptions
import Hledger.Cli.CliOptions
import Hledger.Cli.DocFiles
--import Hledger.Utils.Debug

helpmode = (defCommandMode $ ["help"] ++ aliases) {
  modeHelp = "show any of the hledger manuals, as plain text. With no argument, list the manuals." `withAliases` aliases
 ,modeGroupFlags = Group {
     groupUnnamed = [
      flagNone ["info"]  (setboolopt "info")  "show the manual with info"
     ,flagNone ["man"]   (setboolopt "man")   "show the manual with man"
     ,flagNone ["pager"] (setboolopt "pager") "show the manual with $PAGER or less"
     ,flagNone ["cat"]   (setboolopt "cat")   "show the manual on stdout"
     ,flagNone ["help","h"]  (setboolopt "help")  "show this help"
     ]
    ,groupHidden = []
    ,groupNamed = []
    }
 ,modeArgs = ([], Just $ argsFlag "[MANUAL]")
}
  where aliases = []

-- | List or display one of the hledger manuals in various formats. 
-- You can select a docs viewer with one of the `--info`, `--man`, `--pager`, `--cat` flags.
-- Otherwise it will use the first available of: info, man, $PAGER, less, stdout
-- (and always stdout if output is non-interactive). 
help' :: CliOpts -> IO ()
help' opts = do
  exes <- likelyExecutablesInPath
  pagerprog <- fromMaybe "less" <$> lookupEnv "PAGER"
  interactive <- hIsTerminalDevice stdout
  let
    args = take 1 $ listofstringopt "args" $ rawopts_ opts
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
  case args of
    [t] -> viewer t
    _   -> putStrLn $ "Please choose a manual:\nhledger help " ++ intercalate "|" docTopics

