{-# LANGUAGE TemplateHaskell #-}
{-|

Embedded documentation files in various formats, and helpers for viewing them.

|-}

module Hledger.UI.DocFiles (
   printHelpForTopic
  ,runManForTopic
  ,runInfoForTopic
  ,runPagerForTopic
  ,runTldrForPage
  ) where

import Data.ByteString (ByteString)

import Hledger.Utils (embedFileRelative)
import Hledger.Utils.DocFiles

-- | All hledger-ui pages from the tldr-pages project.
tldrs :: [(TldrPage, ByteString)]
tldrs = [
   ("hledger-ui", $(embedFileRelative "embeddedfiles/hledger-ui.md"))
  ]

-- | The main hledger-ui manuals as source for man, info and as plain text.
man :: ByteString
man = $(embedFileRelative "embeddedfiles/hledger-ui.1")
txt :: ByteString
txt = $(embedFileRelative "embeddedfiles/hledger-ui.txt")
info :: ByteString
info = $(embedFileRelative "embeddedfiles/hledger-ui.info")

printHelpForTopic :: Maybe Topic -> IO ()
printHelpForTopic = printHelpForTopic' txt

runManForTopic :: Maybe Topic -> IO ()
runManForTopic = runManForTopic' "hledger-ui" man

runInfoForTopic :: Maybe Topic -> IO ()
runInfoForTopic = runInfoForTopic' "hledger-ui" info

runPagerForTopic :: Maybe Topic -> IO ()
runPagerForTopic = runPagerForTopic' "hledger-ui" txt

runTldrForPage :: TldrPage -> IO ()
runTldrForPage = runTldrForPage' tldrs
