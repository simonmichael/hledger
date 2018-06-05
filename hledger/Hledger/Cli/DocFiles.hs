{-# LANGUAGE TemplateHaskell, OverloadedStrings, PackageImports #-}
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
import "base-compat-batteries" Prelude.Compat
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
    ($(makeRelativeToProject "embeddedfiles/hledger.1" >>= embedStringFile)
    ,$(makeRelativeToProject "embeddedfiles/hledger.txt" >>= embedStringFile)
    ,$(makeRelativeToProject "embeddedfiles/hledger.info" >>= embedStringFile)
    ))
  ,("hledger-ui",
    ($(makeRelativeToProject "embeddedfiles/hledger-ui.1" >>= embedStringFile)
    ,$(makeRelativeToProject "embeddedfiles/hledger-ui.txt" >>= embedStringFile)
    ,$(makeRelativeToProject "embeddedfiles/hledger-ui.info" >>= embedStringFile)
    ))
  ,("hledger-web",
    ($(makeRelativeToProject "embeddedfiles/hledger-web.1" >>= embedStringFile)
    ,$(makeRelativeToProject "embeddedfiles/hledger-web.txt" >>= embedStringFile)
    ,$(makeRelativeToProject "embeddedfiles/hledger-web.info" >>= embedStringFile)
    ))
  ,("hledger-api",
    ($(makeRelativeToProject "embeddedfiles/hledger-api.1" >>= embedStringFile)
    ,$(makeRelativeToProject "embeddedfiles/hledger-api.txt" >>= embedStringFile)
    ,$(makeRelativeToProject "embeddedfiles/hledger-api.info" >>= embedStringFile)
    ))
  ,("journal",
    ($(makeRelativeToProject "embeddedfiles/hledger_journal.5" >>= embedStringFile)
    ,$(makeRelativeToProject "embeddedfiles/hledger_journal.txt" >>= embedStringFile)
    ,$(makeRelativeToProject "embeddedfiles/hledger_journal.info" >>= embedStringFile)
    ))
  ,("csv",
    ($(makeRelativeToProject "embeddedfiles/hledger_csv.5" >>= embedStringFile)
    ,$(makeRelativeToProject "embeddedfiles/hledger_csv.txt" >>= embedStringFile)
    ,$(makeRelativeToProject "embeddedfiles/hledger_csv.info" >>= embedStringFile)
    ))
  ,("timeclock",
    ($(makeRelativeToProject "embeddedfiles/hledger_timeclock.5" >>= embedStringFile)
    ,$(makeRelativeToProject "embeddedfiles/hledger_timeclock.txt" >>= embedStringFile)
    ,$(makeRelativeToProject "embeddedfiles/hledger_timeclock.info" >>= embedStringFile)
    ))
  ,("timedot",
    ($(makeRelativeToProject "embeddedfiles/hledger_timedot.5" >>= embedStringFile)
    ,$(makeRelativeToProject "embeddedfiles/hledger_timedot.txt" >>= embedStringFile)
    ,$(makeRelativeToProject "embeddedfiles/hledger_timedot.info" >>= embedStringFile)
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

