{-# LANGUAGE TemplateHaskell, OverloadedStrings, PackageImports, ScopedTypeVariables #-}
{-|

Embedded documentation files in various formats, and helpers for viewing them.

|-}

module Hledger.Cli.DocFiles (

   Topic
  ,printHelpForTopic
  ,runManForTopic
  ,runInfoForTopic
  ,runPagerForTopic
  ,runTldrForPage

  ) where

import Control.Exception
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC
import Data.Maybe (fromMaybe)
import System.Environment (setEnv)
import System.IO
import System.IO.Temp
import System.Process

import Hledger.Utils (embedFileRelative, error')
import Text.Printf (printf)
import System.Environment (lookupEnv)
import Hledger.Utils.Debug

-- The name of any hledger executable.
type Tool = String

-- Any heading in the hledger user manual (and perhaps later the hledger-ui/hledger-web manuals).
type Topic = String

-- Any name of a hledger tldr page (hledger, hledger-ui, hledger-print etc.)
type TldrPage = String

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

-- | Print plain text help for this tool.
-- Takes an optional topic argument for convenience but it is currently ignored.
printHelpForTopic' :: ByteString -> Maybe Topic -> IO ()
printHelpForTopic' b _mtopic = BC.putStr b

-- | Display an info manual for this topic, opened at the given topic if provided,
-- using the "info" executable in $PATH.
-- Topic can be an exact heading or a heading prefix; info will favour an exact match.
runInfoForTopic' :: Tool -> ByteString -> Maybe Topic -> IO ()
runInfoForTopic' tool b mtopic =
  withSystemTempFile ("hledger-"++tool++".info") $ \f h -> do
    BC.hPutStrLn h b
    hClose h
    callCommand $ dbg1 "info command" $
      "info -f " ++ f ++ maybe "" (printf " -n '%s'") mtopic

-- less with any vertical whitespace squashed, case-insensitive searching, the $ regex metacharacter accessible as \$.
less = "less -s -i --use-backslash"

-- | Display plain text help for this tool, scrolled to the given topic if any, using the users $PAGER or "less".
-- When a topic is provided we always use less, ignoring $PAGER.
--
-- This is less robust than the newer Hledger.Utils.IO.runPager,
-- but that one doesn't yet support scrolling to a topic.
runPagerForTopic' :: Tool -> ByteString -> Maybe Topic -> IO ()
runPagerForTopic' tool b mtopic = do
  withSystemTempFile ("hledger-"++tool++".txt") $ \f h -> do
    BC.hPutStrLn h b
    hClose h
    envpager <- fromMaybe less <$> lookupEnv "PAGER"
    let
      exactmatch = True
      (pager, searcharg) =
        case mtopic of
          Nothing -> (envpager, "")
          Just t  -> (less, "-p'^(   )?" ++ t ++ if exactmatch then "\\$'" else "")
    callCommand $ dbg1 "pager command" $ unwords [pager, searcharg, f]

-- | Display a man page for this tool, scrolled to the given topic if provided, using "man".
-- When a topic is provided we force man to use "less", ignoring $MANPAGER and $PAGER.
runManForTopic' :: Tool -> ByteString -> Maybe Topic -> IO ()
runManForTopic' tool b mtopic =
  -- This temp file path should have a slash in it, man requires at least one.
  withSystemTempFile ("hledger-"++tool++".1") $ \f h -> do
    BC.hPutStrLn h b
    hClose h
    let
      exactmatch = True
      pagerarg =
        case mtopic of
          Nothing -> ""
          Just t  -> "-P \"" ++ less ++ " -p'^(   )?" ++ t ++ (if exactmatch then "\\\\$" else "") ++ "'\""
    callCommand $ dbg1 "man command" $ unwords ["man", pagerarg, f]

-- | Display one of the tldr pages, using "tldr".
runTldrForPage' :: [(TldrPage, ByteString)] -> TldrPage -> IO ()
runTldrForPage' tldrs name =
  case lookup name tldrs of
    Nothing -> error' $ "sorry, there's no " <> name <> " tldr page yet"
    Just b -> (do
      withSystemTempFile (name++".md") $ \f h -> do
        BC.hPutStrLn h b
        hClose h
        -- tldr clients tend to auto-update their data, try to discourage that here
        -- tealdeer - doesn't auto-update by default
        -- tlrc - ?
        -- tldr-node-client - undocumented env var suggested in output
        setEnv "TLDR_AUTO_UPDATE_DISABLED" "1"
        callCommand $ dbg1 "tldr command" $ "tldr --render " <> f
      ) `catch` (\(_e::IOException) -> do
        hPutStrLn stderr $ "Warning: could not run tldr --render, using fallback viewer instead.\n"
        BC.putStrLn b
      )

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
