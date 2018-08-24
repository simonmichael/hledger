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
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC
import Data.FileEmbed
import Data.String
import System.IO
import System.IO.Temp
import System.Process

import Hledger.Utils (first3, second3, third3)

type Topic = String

docFiles :: [(Topic, (ByteString, ByteString, ByteString))]
docFiles = [
   ("hledger",
    ($(makeRelativeToProject "embeddedfiles/hledger.1" >>= embedFile)
    ,$(makeRelativeToProject "embeddedfiles/hledger.txt" >>= embedFile)
    ,$(makeRelativeToProject "embeddedfiles/hledger.info" >>= embedFile)
    ))
  ,("hledger-ui",
    ($(makeRelativeToProject "embeddedfiles/hledger-ui.1" >>= embedFile)
    ,$(makeRelativeToProject "embeddedfiles/hledger-ui.txt" >>= embedFile)
    ,$(makeRelativeToProject "embeddedfiles/hledger-ui.info" >>= embedFile)
    ))
  ,("hledger-web",
    ($(makeRelativeToProject "embeddedfiles/hledger-web.1" >>= embedFile)
    ,$(makeRelativeToProject "embeddedfiles/hledger-web.txt" >>= embedFile)
    ,$(makeRelativeToProject "embeddedfiles/hledger-web.info" >>= embedFile)
    ))
  ,("hledger-api",
    ($(makeRelativeToProject "embeddedfiles/hledger-api.1" >>= embedFile)
    ,$(makeRelativeToProject "embeddedfiles/hledger-api.txt" >>= embedFile)
    ,$(makeRelativeToProject "embeddedfiles/hledger-api.info" >>= embedFile)
    ))
  ,("journal",
    ($(makeRelativeToProject "embeddedfiles/hledger_journal.5" >>= embedFile)
    ,$(makeRelativeToProject "embeddedfiles/hledger_journal.txt" >>= embedFile)
    ,$(makeRelativeToProject "embeddedfiles/hledger_journal.info" >>= embedFile)
    ))
  ,("csv",
    ($(makeRelativeToProject "embeddedfiles/hledger_csv.5" >>= embedFile)
    ,$(makeRelativeToProject "embeddedfiles/hledger_csv.txt" >>= embedFile)
    ,$(makeRelativeToProject "embeddedfiles/hledger_csv.info" >>= embedFile)
    ))
  ,("timeclock",
    ($(makeRelativeToProject "embeddedfiles/hledger_timeclock.5" >>= embedFile)
    ,$(makeRelativeToProject "embeddedfiles/hledger_timeclock.txt" >>= embedFile)
    ,$(makeRelativeToProject "embeddedfiles/hledger_timeclock.info" >>= embedFile)
    ))
  ,("timedot",
    ($(makeRelativeToProject "embeddedfiles/hledger_timedot.5" >>= embedFile)
    ,$(makeRelativeToProject "embeddedfiles/hledger_timedot.txt" >>= embedFile)
    ,$(makeRelativeToProject "embeddedfiles/hledger_timedot.info" >>= embedFile)
    ))
  ]

docTopics :: [Topic]
docTopics = map fst docFiles

lookupDocTxt :: Topic -> ByteString
lookupDocTxt name =
  maybe (fromString $ "No text manual found for topic: "++name) second3 $ lookup name docFiles

lookupDocNroff :: Topic -> ByteString
lookupDocNroff name =
  maybe (fromString $ "No man page found for topic: "++name) first3 $ lookup name docFiles

lookupDocInfo :: Topic -> ByteString
lookupDocInfo name =
  maybe (fromString $ "No info manual found for topic: "++name) third3 $ lookup name docFiles

printHelpForTopic :: Topic -> IO ()
printHelpForTopic t =
  BC.putStr (lookupDocTxt t)

runPagerForTopic :: FilePath -> Topic -> IO ()
runPagerForTopic exe t = do
  (Just inp, _, _, ph) <- createProcess (proc exe []){
    std_in=CreatePipe
    }
  BC.hPutStrLn inp (lookupDocTxt t)
  _ <- waitForProcess ph
  return ()

runManForTopic :: Topic -> IO ()
runManForTopic t =
  withSystemTempFile ("hledger-"++t++".nroff") $ \f h -> do
    BC.hPutStrLn h $ lookupDocNroff t
    hClose h
     -- the temp file path will presumably have a slash in it, so man should read it
    callCommand $ "man " ++ f

runInfoForTopic :: Topic -> IO ()
runInfoForTopic t =
  withSystemTempFile ("hledger-"++t++".info") $ \f h -> do
    BC.hPutStrLn h $ lookupDocInfo t
    hClose h
    callCommand $ "info " ++ f

