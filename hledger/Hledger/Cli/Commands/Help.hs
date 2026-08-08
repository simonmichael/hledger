{-|

The help command.

|-}
--TODO rename manuals
--TODO substring matching

{-# LANGUAGE PackageImports #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Hledger.Cli.Commands.Help (

   helpmode
  ,manual

  ) where

import Control.Monad (void)
import Data.Char (toLower)
import Data.List (isInfixOf)
import Data.Maybe
import System.Console.CmdArgs.Explicit
import System.Environment
import System.IO

import Hledger.Cli.CliOptions
import Hledger.Cli.DocFiles
import Hledger.Cli.Utils (openBrowserOn)
import Hledger.Cli.Version (webManualUrl)
import Hledger.Data.RawOptions
import Hledger.Utils (embedFileRelative)
--import Hledger.Utils.Debug

helpmode = hledgerCommandMode
  $(embedFileRelative "Hledger/Cli/Commands/Help.txt")
  -- The help-* names avoid a clash with the --info and --man flags handled in Cli.hs.
  [flagNone ["i"] (setboolopt "help-i")  "show the manual with info"
  ,flagNone ["m"] (setboolopt "help-m")   "show the manual with man"
  ,flagNone ["p"] (setboolopt "help-p") "show the manual with $PAGER or less\n(less is always used if TOPIC is specified)"
  ,flagNone ["w"] (setboolopt "help-w") "show the manual on the web"
  ,flagNone ["builtin"] (setboolopt "builtin") "with the commands topic, show only built-in commands"
  ]
  [(helpflagstitle, helpflags)]
  hiddenflags  -- accept --conf/--no-conf etc., eg so "help commands" can show config aliases
  ([], Just $ argsFlag "[TOPIC]")

-- | Display the hledger manual in various formats, optionally positioned at the given topic.
-- You can select a docs viewer with one of the `--info`, `--man`, `--pager` flags.
-- Otherwise it will use the first available of: info, man, $PAGER, less, stdout
-- (and always stdout if output is non-interactive, the terminal is dumb, or we are
-- in an Emacs buffer that can't render fullscreen viewers, eg shell/comint or eshell).
manual :: CliOpts -> Maybe Topic -> IO ()
manual opts mtopic = case mtopic of
  Nothing -> showManualAt Nothing
  Just t  -> case resolveManualTopic "hledger" t of
    TopicFound h      -> showManualAt (Just h)
    -- If the topic matches no heading or several, show a note on stderr and
    -- stop - rather than raising an error, or showing the manual from the top.
    TopicNotFound     -> hPutStrLn stderr $
      "\"" <> t <> "\" does not match any section heading in the hledger manual.\n"
      <> "Run `hledger help manual` to browse the manual, or `hledger help` for the quick reference."
    TopicAmbiguous hs -> hPutStrLn stderr $
      "\"" <> t <> "\" matches several manual sections:\n"
      <> unlines (map ("  " <>) hs)
      <> "Please use a more specific topic."
  where
    -- Show the manual, positioned at the given heading if any, in the best viewer.
    showManualAt mtopic' = do
      exes <- likelyExecutablesInPath
      pagerprog <- fromMaybe "less" <$> lookupEnv "PAGER"
      interactive <- hIsTerminalDevice stdout
      -- A dumb terminal (eg TERM=dumb, as in emacs shells) can't run info, and
      -- degrades man/less, so fall back to plain text there like the non-interactive case.
      dumbterminal <- ((== "dumb") . map toLower . fromMaybe "") <$> lookupEnv "TERM"
      -- Inside Emacs, only its terminal-emulator modes (term/ansi-term/vterm, whose
      -- INSIDE_EMACS mode contains "term") can render fullscreen viewers like info/man/less;
      -- other modes (shell/comint, eshell) are line-oriented and would garble them, so fall
      -- back to plain text there like the dumb/non-interactive case.
      emacsnonterm <- (\m -> not (null m) && not ("term" `isInfixOf` m)) . fromMaybe "" <$> lookupEnv "INSIDE_EMACS"
      let
        [info, man, pager, cat] =
          [runInfoForTopic, runManForTopic, runPagerForTopic, printHelpForTopic]
        web tool mt = void $ openBrowserOn $ webManualUrl tool mt
        viewer
          | boolopt "help-w" $ rawopts_ opts = web
          | boolopt "help-i" $ rawopts_ opts = info
          | boolopt "help-m" $ rawopts_ opts = man
          | boolopt "help-p" $ rawopts_ opts = pager
          | not interactive || dumbterminal || emacsnonterm = cat
          | "info"    `elem` exes            = info
          | "man"     `elem` exes            = man
          | pagerprog `elem` exes            = pager
          | "less"    `elem` exes            = pager
          | otherwise                        = cat
      viewer "hledger" mtopic'
