{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
{-|

Embedded documentation files in various formats, and helpers for viewing them.

|-}

module Hledger.Cli.DocFiles (

   Topic
  ,docFiles
  ,docTopics
  ,lookupDocNroff
  ,lookupDocTxt
  ,lookupDocInfo
  ,printHelpForTopic
  ,runManForTopic
  ,runInfoForTopic

  ) where

import Prelude ()
import Prelude.Compat
import Data.FileEmbed
import Data.String
import System.IO
import System.IO.Temp
import System.Process

import Hledger.Utils (first3, second3, third3)

type Topic = String

docFiles :: IsString a => [(Topic, (a, a, a))]
docFiles = [
   ("cli",
    ($(makeRelativeToProject "doc/hledger.1" >>= embedStringFile)
    ,$(makeRelativeToProject "doc/hledger.1.txt" >>= embedStringFile)
    ,$(makeRelativeToProject "doc/hledger.1.info" >>= embedStringFile)
    ))
  ,("ui",
    ($(makeRelativeToProject "doc/other/hledger-ui.1" >>= embedStringFile)
    ,$(makeRelativeToProject "doc/other/hledger-ui.1.txt" >>= embedStringFile)
    ,$(makeRelativeToProject "doc/other/hledger-ui.1.info" >>= embedStringFile)
    ))
  ,("web",
    ($(makeRelativeToProject "doc/other/hledger-web.1" >>= embedStringFile)
    ,$(makeRelativeToProject "doc/other/hledger-web.1.txt" >>= embedStringFile)
    ,$(makeRelativeToProject "doc/other/hledger-web.1.info" >>= embedStringFile)
    ))
  ,("api",
    ($(makeRelativeToProject "doc/other/hledger-api.1" >>= embedStringFile)
    ,$(makeRelativeToProject "doc/other/hledger-api.1.txt" >>= embedStringFile)
    ,$(makeRelativeToProject "doc/other/hledger-api.1.info" >>= embedStringFile)
    ))
  ,("journal",
    ($(makeRelativeToProject "doc/other/hledger_journal.5" >>= embedStringFile)
    ,$(makeRelativeToProject "doc/other/hledger_journal.5.txt" >>= embedStringFile)
    ,$(makeRelativeToProject "doc/other/hledger_journal.5.info" >>= embedStringFile)
    ))
  ,("csv",
    ($(makeRelativeToProject "doc/other/hledger_csv.5" >>= embedStringFile)
    ,$(makeRelativeToProject "doc/other/hledger_csv.5.txt" >>= embedStringFile)
    ,$(makeRelativeToProject "doc/other/hledger_csv.5.info" >>= embedStringFile)
    ))
  ,("timeclock",
    ($(makeRelativeToProject "doc/other/hledger_timeclock.5" >>= embedStringFile)
    ,$(makeRelativeToProject "doc/other/hledger_timeclock.5.txt" >>= embedStringFile)
    ,$(makeRelativeToProject "doc/other/hledger_timeclock.5.info" >>= embedStringFile)
    ))
  ,("timedot",
    ($(makeRelativeToProject "doc/other/hledger_timedot.5" >>= embedStringFile)
    ,$(makeRelativeToProject "doc/other/hledger_timedot.5.txt" >>= embedStringFile)
    ,$(makeRelativeToProject "doc/other/hledger_timedot.5.info" >>= embedStringFile)
    ))
  ]

docTopics :: [Topic]
docTopics = map fst docFiles

lookupDocTxt :: IsString a => Topic -> a
lookupDocTxt name =
  maybe (fromString $ "No text manual found for topic: "++name) second3 $ lookup name docFiles

lookupDocNroff :: IsString a => Topic -> a
lookupDocNroff name =
  maybe (fromString $ "No man page found for topic: "++name) first3 $ lookup name docFiles

lookupDocInfo :: IsString a => Topic -> a
lookupDocInfo name =
  maybe (fromString $ "No info manual found for topic: "++name) third3 $ lookup name docFiles

printHelpForTopic :: Topic -> IO ()
printHelpForTopic t =
  putStrLn $ lookupDocTxt t

runManForTopic :: Topic -> IO ()
runManForTopic t =
  withSystemTempFile ("hledger-"++t++".nroff") $ \f h -> do
    hPutStrLn h $ lookupDocNroff t
    hClose h
     -- the temp file path will presumably have a slash in it, so man should read it
    callCommand $ "man " ++ f

runInfoForTopic :: Topic -> IO ()
runInfoForTopic t =
  withSystemTempFile ("hledger-"++t++".info") $ \f h -> do
    hPutStrLn h $ lookupDocInfo t
    hClose h
    callCommand $ "info " ++ f

