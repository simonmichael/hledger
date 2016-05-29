{-|

The info command.

|-}

module Hledger.Cli.Info (

   infomode
  ,info'

  ) where

import Prelude ()
import Prelude.Compat
import Data.List
import System.Console.CmdArgs.Explicit

import Hledger.Data.RawOptions
import Hledger.Cli.CliOptions
import Hledger.Cli.DocFiles

infomode = (defCommandMode $ ["info"] ++ aliases) {
  modeHelp = "show any of the hledger manuals with info" `withAliases` aliases
 ,modeGroupFlags = Group {
     groupUnnamed = []
    ,groupHidden = []
    ,groupNamed = []
    }
 }
  where aliases = []

-- | Try to use info to view the selected manual.
info' :: CliOpts -> IO ()
info' opts = do
  let args = listofstringopt "args" $ rawopts_ opts
  case args of
    []    -> putStrLn $
             "Choose a topic, eg: hledger info cli\n" ++
             intercalate ", " docTopics
    topic:_ -> runInfoForTopic topic
