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

-- | Editors we know how to create more specific command lines for.
data EditorType = Emacs | EmacsClient | Vi | Other

-- | A position we can move to in a text editor: a line and optional column number.
-- 1 (or 0) means the first and -1 means the last (and -2 means the second last, etc.
-- though this may not be well supported.)
type TextPosition = (Int, Maybe Int)

endPos :: Maybe TextPosition
endPos = Just (-1,Nothing)

-- | Try running the user's preferred text editor, or a default edit command,
-- on the main journal file, blocking until it exits, and returning the exit code;
-- or raise an error.
runEditor :: Maybe TextPosition -> FilePath -> IO ExitCode
runEditor mpos f = editorOpenPositionCommand mpos f >>= runCommand >>= waitForProcess

-- Get the basic shell command to start the user's preferred text editor.
-- This is the value of environment variable $HLEDGER_UI_EDITOR, or $EDITOR, or
-- a default (emacsclient -a '' -nw, start/connect to an emacs daemon in terminal mode).
editorCommand :: IO String
editorCommand = do
  hledger_ui_editor_env <- lookupEnv "HLEDGER_UI_EDITOR"
  editor_env            <- lookupEnv "EDITOR"
  let Just cmd =
        hledger_ui_editor_env
        <|> editor_env
        <|> Just "emacsclient -a '' -nw"
  return cmd

-- | Get a shell command to start the user's preferred text editor, or a default,
-- and optionally jump to a given position in the file. This will be the basic
-- editor command, with the appropriate options added, if we know how.
-- Currently we know how to do this for emacs and vi.
-- Some examples:
-- $EDITOR=notepad         -> "notepad FILE"
-- $EDITOR=vi              -> "vi +LINE FILE"
-- $EDITOR=vi, line -1     -> "vi + FILE"
-- $EDITOR=emacs           -> "emacs +LINE:COL FILE"
-- $EDITOR=emacs, line -1  -> "emacs FILE -f end-of-buffer"
-- $EDITOR not set         -> "emacs -nw FILE -f end-of-buffer"
--
editorOpenPositionCommand :: Maybe TextPosition -> FilePath -> IO String
editorOpenPositionCommand mpos f = do
  cmd <- editorCommand
  let f' = singleQuoteIfNeeded f
  return $
   case (identifyEditor cmd, mpos) of
    (EmacsClient, Just (l,mc)) | l >= 0 -> cmd ++ " " ++ emacsposopt l mc ++ " " ++ f'
    (EmacsClient, Just (l,mc)) | l < 0  -> cmd ++ " " ++ emacsposopt 999999999 mc ++ " " ++ f'
    (Emacs, Just (l,mc))       | l >= 0 -> cmd ++ " " ++ emacsposopt l mc ++ " " ++ f'
    (Emacs, Just (l,_))        | l < 0  -> cmd ++ " " ++ f' ++ " -f end-of-buffer"
    (Vi, Just (l,_))                    -> cmd ++ " " ++ viposopt l ++ " " ++ f'
    _                                   -> cmd ++ " " ++ f'
    where
      emacsposopt l mc = "+" ++ show l ++ maybe "" ((":"++).show) mc
      viposopt l       = "+" ++ if l >= 0 then show l else ""

-- Identify which text editor is used in the basic editor command, if possible.
identifyEditor :: String -> EditorType
identifyEditor cmd
  | "emacsclient" `isPrefixOf` exe = EmacsClient
  | "emacs" `isPrefixOf` exe       = Emacs
  | exe `elem` ["vi","vim","ex","view","gvim","gview","evim","eview","rvim","rview","rgvim","rgview"]
                                   = Vi
  | otherwise                      = Other
  where
    exe = lowercase $ takeFileName $ headDef "" $ words' cmd
