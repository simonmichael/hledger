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
  ,runPagerForTopic

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
   ("hledger",
    ($(makeRelativeToProject "hledger.1" >>= embedStringFile)
    ,$(makeRelativeToProject "hledger.txt" >>= embedStringFile)
    ,$(makeRelativeToProject "hledger.info" >>= embedStringFile)
    ))
  ,("hledger-ui",
    ($(makeRelativeToProject ".otherdocs/hledger-ui.1" >>= embedStringFile)
    ,$(makeRelativeToProject ".otherdocs/hledger-ui.txt" >>= embedStringFile)
    ,$(makeRelativeToProject ".otherdocs/hledger-ui.info" >>= embedStringFile)
    ))
  ,("hledger-web",
    ($(makeRelativeToProject ".otherdocs/hledger-web.1" >>= embedStringFile)
    ,$(makeRelativeToProject ".otherdocs/hledger-web.txt" >>= embedStringFile)
    ,$(makeRelativeToProject ".otherdocs/hledger-web.info" >>= embedStringFile)
    ))
  ,("hledger-api",
    ($(makeRelativeToProject ".otherdocs/hledger-api.1" >>= embedStringFile)
    ,$(makeRelativeToProject ".otherdocs/hledger-api.txt" >>= embedStringFile)
    ,$(makeRelativeToProject ".otherdocs/hledger-api.info" >>= embedStringFile)
    ))
  ,("journal",
    ($(makeRelativeToProject ".otherdocs/hledger_journal.5" >>= embedStringFile)
    ,$(makeRelativeToProject ".otherdocs/hledger_journal.txt" >>= embedStringFile)
    ,$(makeRelativeToProject ".otherdocs/hledger_journal.info" >>= embedStringFile)
    ))
  ,("csv",
    ($(makeRelativeToProject ".otherdocs/hledger_csv.5" >>= embedStringFile)
    ,$(makeRelativeToProject ".otherdocs/hledger_csv.txt" >>= embedStringFile)
    ,$(makeRelativeToProject ".otherdocs/hledger_csv.info" >>= embedStringFile)
    ))
  ,("timeclock",
    ($(makeRelativeToProject ".otherdocs/hledger_timeclock.5" >>= embedStringFile)
    ,$(makeRelativeToProject ".otherdocs/hledger_timeclock.txt" >>= embedStringFile)
    ,$(makeRelativeToProject ".otherdocs/hledger_timeclock.info" >>= embedStringFile)
    ))
  ,("timedot",
    ($(makeRelativeToProject ".otherdocs/hledger_timedot.5" >>= embedStringFile)
    ,$(makeRelativeToProject ".otherdocs/hledger_timedot.txt" >>= embedStringFile)
    ,$(makeRelativeToProject ".otherdocs/hledger_timedot.info" >>= embedStringFile)
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

runPagerForTopic :: FilePath -> Topic -> IO ()
runPagerForTopic exe t = do
  (Just inp, _, _, ph) <- createProcess (proc exe []){
    std_in=CreatePipe
    }
  hPutStrLn inp (lookupDocTxt t)
  _ <- waitForProcess ph
  return ()

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

