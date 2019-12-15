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

-- | A position we can move to in a text editor: a line and optional column number.
-- 1 (or 0) means the first and -1 means the last (and -2 means the second last, etc.
-- though this may not be well supported.)
type TextPosition = (Int, Maybe Int)

endPos :: Maybe TextPosition
endPos = Just (-1,Nothing)

-- | Run the hledger-iadd executable (an alternative to the built-in add command),
-- or raise an error.
runIadd :: FilePath -> IO ExitCode
runIadd f = runCommand ("hledger-iadd -f " ++ f) >>= waitForProcess

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

-- | Editors which we know how to open at a specific file position,
-- and Other for the rest.
data EditorType = Emacs | EmacsClient | Vi | Other

-- Identify which text editor is being used in the basic editor command, if possible.
identifyEditor :: String -> EditorType
identifyEditor cmd
  | "emacsclient" `isPrefixOf` exe = EmacsClient
  | "emacs" `isPrefixOf` exe       = Emacs
  | exe `elem` ["vi","nvim","vim","ex","view","gvim","gview","evim","eview","rvim","rview","rgvim","rgview"]
                                   = Vi
  | otherwise                      = Other
  where
    exe = lowercase $ takeFileName $ headDef "" $ words' cmd

-- | Get a shell command to start the user's preferred text editor, or a default,
-- and optionally jump to a given position in the file. This will be the basic
-- editor command, with the appropriate options added, if we know how.
-- Currently we know how to do this for emacs and vi.
--
-- @
-- Some tests:
-- When EDITOR is:  The command should be:
-- ---------------  -----------------------------------
-- notepad          notepad FILE
-- vi               vi +LINE FILE
-- emacs            emacs +LINE:COL FILE
-- (unset)          emacs -nw FILE -f end-of-buffer
-- @
--
-- How to open editors at the last line of a file:
-- @
-- emacs:  emacs FILE -f end-of-buffer
-- vi:     vi + FILE
-- @
--
editorOpenPositionCommand :: Maybe TextPosition -> FilePath -> IO String
editorOpenPositionCommand mpos f = do
  cmd <- editorCommand
  return $ cmd ++ " " ++ 
   case (identifyEditor cmd, mpos) of
    (EmacsClient , Just (l,mc)) | l >= 0 -> emacsposopt l mc ++ " " ++ f'
    (EmacsClient , Just (l,mc)) | l <  0 -> emacsposopt 999999999 mc ++ " " ++ f'
    (Emacs       , Just (l,mc)) | l >= 0 -> emacsposopt l mc ++ " " ++ f'
    (Emacs       , Just (l,_))  | l <  0 -> f' ++ " -f end-of-buffer"
    (Vi          , Just (l,_))           -> viposopt l ++ " " ++ f'
    _                                    -> f'
    where
      f' = singleQuoteIfNeeded f
      emacsposopt l mc = "+" ++ show l ++ maybe "" ((":"++).show) mc
      viposopt l       = "+" ++ if l >= 0 then show l else ""

