{-|

The man command.

|-}

module Hledger.Cli.Man (

   manmode
  ,man

  ) where

import Prelude ()
import Prelude.Compat
import Data.List
import System.Console.CmdArgs.Explicit

import Hledger.Data.RawOptions
import Hledger.Cli.CliOptions
import Hledger.Cli.DocFiles

manmode = (defCommandMode $ ["man"] ++ aliases) {
  modeHelp = "show manual with man" `withAliases` aliases
 ,modeGroupFlags = Group {
     groupUnnamed = []
    ,groupHidden = []
    ,groupNamed = []
    }
 }
  where aliases = []

-- | Try to use man to view the selected manual.
man :: CliOpts -> IO ()
man opts = do
  let args = listofstringopt "args" $ rawopts_ opts
  case args of
    []    -> putStrLn $
             "Choose a topic, eg: hledger man cli\n" ++
             intercalate ", " docTopics
    topic:_ -> runManForTopic topic
