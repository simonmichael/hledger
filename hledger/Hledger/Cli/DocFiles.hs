{-# LANGUAGE TemplateHaskell, OverloadedStrings, PackageImports #-}
{-|

Embedded documentation files in various formats, and helpers for viewing them.

|-}

module Hledger.Cli.DocFiles (

   Topic
  -- ,toolDocs
  -- ,toolDocNames
  -- ,toolDocMan
  -- ,toolDocTxt
  -- ,toolDocInfo
  ,printHelpForTopic
  ,runManForTopic
  ,runInfoForTopic
  ,runPagerForTopic

  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC
import Data.Maybe (fromMaybe)
import Data.String
import System.IO
import System.IO.Temp
import System.Process

import Hledger.Utils (first3, second3, third3, embedFileRelative)
import Text.Printf (printf)
import System.Environment (lookupEnv)
import Hledger.Utils.Debug

-- The name of any hledger executable.
type Tool = String

-- Any heading in the hledger user manual (and perhaps later the hledger-ui/hledger-web manuals).
type Topic = String

-- | The main hledger manuals as source for man, info and as plain text.
-- Only files under the current package directory can be embedded,
-- so some of these are symlinked from the other package directories.
toolDocs :: [(Tool, (ByteString, ByteString, ByteString))]
toolDocs = [
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
  ]

-- toolNames :: [Tool]
-- toolNames = map fst toolDocs

-- | Get the manual as plain text for this tool, or a not found message.
toolDocTxt :: Tool -> ByteString
toolDocTxt name =
  maybe (fromString $ "No text manual found for tool: "++name) second3 $ lookup name toolDocs

-- | Get the manual as man source (nroff) for this tool, or a not found message.
toolDocMan :: Tool -> ByteString
toolDocMan name =
  maybe (fromString $ "No man page found for tool: "++name) first3 $ lookup name toolDocs

-- | Get the manual as info source (texinfo) for this tool, or a not found message.
toolDocInfo :: Tool -> ByteString
toolDocInfo name =
  maybe (fromString $ "No info manual found for tool: "++name) third3 $ lookup name toolDocs

-- | Print plain text help for this tool.
-- Takes an optional topic argument for convenience but it is currently ignored.
printHelpForTopic :: Tool -> Maybe Topic -> IO ()
printHelpForTopic tool _mtopic =
  BC.putStr (toolDocTxt tool)

-- | Display an info manual for this topic, opened at the given topic if provided,
-- using the "info" executable in $PATH.
-- Topic can be an exact heading or a heading prefix; info will favour an exact match.
runInfoForTopic :: Tool -> Maybe Topic -> IO ()
runInfoForTopic tool mtopic =
  withSystemTempFile ("hledger-"++tool++".info") $ \f h -> do
    BC.hPutStrLn h $ toolDocInfo tool
    hClose h
    callCommand $ dbg1 "info command" $
      "info -f " ++ f ++ maybe "" (printf " -n '%s'") mtopic

-- less with any vertical whitespace squashed, case-insensitive searching, the $ regex metacharacter accessible as \$.
less = "less -s -i --use-backslash"

-- | Display plain text help for this tool, scrolled to the given topic if any, using the users $PAGER or "less".
-- When a topic is provided we always use less, ignoring $PAGER.
runPagerForTopic :: Tool -> Maybe Topic -> IO ()
runPagerForTopic tool mtopic = do
  withSystemTempFile ("hledger-"++tool++".txt") $ \f h -> do
    BC.hPutStrLn h $ toolDocTxt tool
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
runManForTopic :: Tool -> Maybe Topic -> IO ()
runManForTopic tool mtopic =
  -- This temp file path should have a slash in it, man requires at least one.
  withSystemTempFile ("hledger-"++tool++".1") $ \f h -> do
    BC.hPutStrLn h $ toolDocMan tool
    hClose h
    let
      exactmatch = True
      pagerarg =
        case mtopic of
          Nothing -> ""
          Just t  -> "-P \"" ++ less ++ " -p'^(   )?" ++ t ++ (if exactmatch then "\\\\$" else "") ++ "'\""
    callCommand $ dbg1 "man command" $ unwords ["man", pagerarg, f]
