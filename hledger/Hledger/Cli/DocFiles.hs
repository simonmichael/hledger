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

import Prelude ()
import "base-compat-batteries" Prelude.Compat
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC
import Data.String
import System.IO
import System.IO.Temp
import System.Process

import Hledger.Utils (first3, second3, third3, embedFileRelative)
import Text.Printf (printf)
import Data.Maybe (fromMaybe)
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

-- | Display plain text help for this tool, scrolled to the given topic
-- if provided, using the given pager executable.
-- Note when a topic is provided we ignore the provided pager and
-- use the "less" executable in $PATH.
runPagerForTopic :: Tool -> Maybe Topic -> IO ()
runPagerForTopic tool mtopic = do
  -- avoids a temp file but different from the others and not sure how to make it scroll
  -- pager <- fromMaybe "less" <$> lookupEnv "PAGER"
  -- (Just inp, _, _, ph) <- createProcess (proc pager []){
  --   std_in=CreatePipe
  --   }
  -- BC.hPutStrLn inp (toolDocTxt tool)
  -- _ <- waitForProcess ph
  -- return ()
  
  withSystemTempFile ("hledger-"++tool++".txt") $ \f h -> do
    BC.hPutStrLn h $ toolDocTxt tool
    hClose h
    let defpager = "less -is"
    envpager <- fromMaybe defpager <$> lookupEnv "PAGER"
    -- force the use of less if a topic is provided, since we know how to scroll it
    let pager = if mtopic==Nothing then envpager else defpager
    callCommand $ dbg1 "pager command" $ 
      pager ++ maybe "" (printf " +'/^(   )?%s'") mtopic ++ " " ++ f

-- | Display a man page for this tool, scrolled to the given topic if provided, 
-- using the "man" executable in $PATH. Note when a topic is provided we force 
-- man to use the "less" executable in $PATH, ignoring $MANPAGER and $PAGER.
runManForTopic :: Tool -> Maybe Topic -> IO ()
runManForTopic tool mtopic =
  withSystemTempFile ("hledger-"++tool++".nroff") $ \f h -> do
    BC.hPutStrLn h $ toolDocMan tool
    hClose h
    -- the temp file path will presumably have a slash in it, so man should read it
    callCommand $ dbg1 "man command" $ 
      "man " ++ f ++ maybe "" (printf " -P \"less -is +'/^(   )?%s'\"") mtopic

-- | Display an info manual for this topic, opened at the given topic if provided,
-- using the "info" executable in $PATH.
runInfoForTopic :: Tool -> Maybe Topic -> IO ()
runInfoForTopic tool mtopic =
  withSystemTempFile ("hledger-"++tool++".info") $ \f h -> do
    BC.hPutStrLn h $ toolDocInfo tool
    hClose h
    callCommand $ dbg1 "info command" $
      "info -f " ++ f ++ maybe "" (printf " -n '%s'") mtopic
