{- | Editor integration. -}

-- {-# LANGUAGE OverloadedStrings #-}

module Hledger.UI.Editor
where

import Control.Applicative ((<|>))
import Data.List
import Safe
import System.Environment
import System.Exit
import System.FilePath
import System.Process

import Hledger

-- | A shell command line for invoking an editor, containing one placeholder ("FILE")
-- which will be replaced with a quoted file path. This exists because some desirable
-- editor commands do not fit the simple "$EDITOR FILE" pattern.
type EditorCommandTemplate = String

-- | Editors we know how to create more specific command lines for.
data EditorType = Emacs | Other

-- | A position we can move to in a text editor: a line number
-- and optionally character number. 1 (or 0) means the first; a negative number
-- counts back from the end (so -1 means the last line, -2 the second last etc.)
type TextPosition = (Int, Maybe Int)

endPos :: Maybe TextPosition
endPos = Just (1,Nothing)

-- | Construct a shell command template for starting the user's preferred text editor,
-- optionally at a given position.
-- XXX The position parameter is currently ignored and assumed to be end-of-file.
--
-- The basic editor command will be the value of environment variable $HLEDGER_UI_EDITOR,
-- or $EDITOR, or "emacs -nw". If a position is specified, and the command looks like one of
-- the editors we know (currently only emacs and emacsclient), it is modified so as to jump
-- to that position.
--
-- Some examples:
-- $EDITOR=vi            -> "vi FILE"
-- $EDITOR=emacs         -> "emacs FILE -f end-of-buffer"
-- $EDITOR not set       -> "emacs -nw FILE -f end-of-buffer"
--
editorCommandTemplate :: Maybe TextPosition -> IO EditorCommandTemplate
editorCommandTemplate mpos = do
  hledger_ui_editor_env <- lookupEnv "HLEDGER_UI_EDITOR"
  editor_env            <- lookupEnv "EDITOR"
  let Just exe = hledger_ui_editor_env <|> editor_env <|> Just "emacs -nw"
  return $
   case (identifyEditor exe, mpos) of
    (Emacs,_) -> exe ++ " FILE -f end-of-buffer"
    _         -> exe ++ " FILE"

-- Identify the editor type, if we know it, from the value of $HLEDGER_EDITOR_UI or $EDITOR.
identifyEditor :: String -> EditorType
identifyEditor cmd
  | "emacs" `isPrefixOf` exe = Emacs
  | otherwise = Other
  where
    exe = lowercase $ takeFileName $ headDef "" $ words' cmd

fillEditorCommandTemplate :: FilePath -> EditorCommandTemplate -> String
fillEditorCommandTemplate f t = regexReplace "FILE" (singleQuoteIfNeeded f) t

-- | Try running $EDITOR, or a default edit command, on the main journal file,
-- blocking until it exits, and returning the exit code; or raise an error.
runEditor :: Maybe TextPosition -> Journal -> IO ExitCode
runEditor mpos j = do
  fillEditorCommandTemplate (journalFilePath j) <$> editorCommandTemplate mpos
  >>= runCommand
  >>= waitForProcess
