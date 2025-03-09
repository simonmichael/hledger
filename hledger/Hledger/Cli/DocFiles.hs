{-# LANGUAGE TemplateHaskell #-}
{-|

Embedded documentation files in various formats, and helpers for viewing them.

|-}

module Hledger.Cli.DocFiles (
   printHelpForTopic
  ,runManForTopic
  ,runInfoForTopic
  ,runPagerForTopic
  ,runTldrForPage
  ) where

import Data.ByteString (ByteString)

import Hledger.Utils (embedFileRelative)
import Hledger.Utils.DocFiles

-- | All hledger (but not hledger-ui/web) pages from the tldr-pages project.
-- All are symlinked into the hledger package directory to allow embeddeding.
tldrs :: [(TldrPage, ByteString)]
tldrs = [
   ("hledger-accounts",        $(embedFileRelative "embeddedfiles/hledger-accounts.md"))
  ,("hledger-add",             $(embedFileRelative "embeddedfiles/hledger-add.md"))
  ,("hledger-aregister",       $(embedFileRelative "embeddedfiles/hledger-aregister.md"))
  ,("hledger-balance",         $(embedFileRelative "embeddedfiles/hledger-balance.md"))
  ,("hledger-balancesheet",    $(embedFileRelative "embeddedfiles/hledger-balancesheet.md"))
  ,("hledger-import",          $(embedFileRelative "embeddedfiles/hledger-import.md"))
  ,("hledger-incomestatement", $(embedFileRelative "embeddedfiles/hledger-incomestatement.md"))
  ,("hledger-print",           $(embedFileRelative "embeddedfiles/hledger-print.md"))
  ,("hledger",                 $(embedFileRelative "embeddedfiles/hledger.md"))
  ]

-- | The main hledger manuals as source for man, info and as plain text.
man :: ByteString
man = $(embedFileRelative "embeddedfiles/hledger.1")
txt :: ByteString
txt = $(embedFileRelative "embeddedfiles/hledger.txt")
info :: ByteString
info = $(embedFileRelative "embeddedfiles/hledger.info")

printHelpForTopic :: Maybe Topic -> IO ()
printHelpForTopic = printHelpForTopic' txt

runManForTopic :: Maybe Topic -> IO ()
runManForTopic = runManForTopic' "hledger" man

runInfoForTopic :: Maybe Topic -> IO ()
runInfoForTopic = runInfoForTopic' "hledger" info

runPagerForTopic :: Maybe Topic -> IO ()
runPagerForTopic = runPagerForTopic' "hledger" txt

runTldrForPage :: TldrPage -> IO ()
runTldrForPage = runTldrForPage' tldrs
