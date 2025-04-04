{-# LANGUAGE TemplateHaskell #-}
{-|

Embedded documentation files in various formats, and helpers for viewing them.

|-}

module Hledger.Web.DocFiles (
   printHelpForTopic
  ,runManForTopic
  ,runInfoForTopic
  ,runPagerForTopic
  ,runTldrForPage
  ) where

import Data.ByteString (ByteString)

import Hledger.Utils (embedFileRelative)
import Hledger.Utils.DocFiles

-- | All hledger-web pages from the tldr-pages project.
tldrs :: [(TldrPage, ByteString)]
tldrs = [
   ("hledger-web", $(embedFileRelative "embeddedfiles/hledger-web.md"))
  ]

-- | The main hledger-web manuals as source for man, info and as plain text.
man :: ByteString
man = $(embedFileRelative "embeddedfiles/hledger-web.1")
txt :: ByteString
txt = $(embedFileRelative "embeddedfiles/hledger-web.txt")
info :: ByteString
info = $(embedFileRelative "embeddedfiles/hledger-web.info")

printHelpForTopic :: Maybe Topic -> IO ()
printHelpForTopic = printHelpForTopic' txt

runManForTopic :: Maybe Topic -> IO ()
runManForTopic = runManForTopic' "hledger-web" man

runInfoForTopic :: Maybe Topic -> IO ()
runInfoForTopic = runInfoForTopic' "hledger-web" info

runPagerForTopic :: Maybe Topic -> IO ()
runPagerForTopic = runPagerForTopic' "hledger-web" txt

runTldrForPage :: TldrPage -> IO ()
runTldrForPage = runTldrForPage' tldrs
