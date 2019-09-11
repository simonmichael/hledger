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
import Data.String
import System.IO
import System.IO.Temp
import System.Process

import Hledger.Utils (first3, second3, third3, embedFileRelative)

type Topic = String

-- | These are all the main hledger manuals, in man, txt, and info formats.
-- Only files under the current package directory can be embedded,
-- so most of these are symlinked here from the other package directories.
docFiles :: [(Topic, (ByteString, ByteString, ByteString))]
docFiles = [
   ("hledger",
    ($(embedFileRelative "embeddedfiles/hledger.1")
    ,$(embedFileRelative "embeddedfiles/hledger.txt")
    ,$(embedFileRelative "embeddedfiles/hledger.info")
    ))
  ,("hledger-ui",
    ($(embedFileRelative "embeddedfiles/hledger-ui.1")
    ,$(embedFileRelative "embeddedfiles/hledger-ui.txt")
    ,$(embedFileRelative "embeddedfiles/hledger-ui.info")
    ))
  ,("hledger-web",
    ($(embedFileRelative "embeddedfiles/hledger-web.1")
    ,$(embedFileRelative "embeddedfiles/hledger-web.txt")
    ,$(embedFileRelative "embeddedfiles/hledger-web.info")
    ))
  ,("journal",
    ($(embedFileRelative "embeddedfiles/hledger_journal.5")
    ,$(embedFileRelative "embeddedfiles/hledger_journal.txt")
    ,$(embedFileRelative "embeddedfiles/hledger_journal.info")
    ))
  ,("csv",
    ($(embedFileRelative "embeddedfiles/hledger_csv.5")
    ,$(embedFileRelative "embeddedfiles/hledger_csv.txt")
    ,$(embedFileRelative "embeddedfiles/hledger_csv.info")
    ))
  ,("timeclock",
    ($(embedFileRelative "embeddedfiles/hledger_timeclock.5")
    ,$(embedFileRelative "embeddedfiles/hledger_timeclock.txt")
    ,$(embedFileRelative "embeddedfiles/hledger_timeclock.info")
    ))
  ,("timedot",
    ($(embedFileRelative "embeddedfiles/hledger_timedot.5")
    ,$(embedFileRelative "embeddedfiles/hledger_timedot.txt")
    ,$(embedFileRelative "embeddedfiles/hledger_timedot.info")
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

