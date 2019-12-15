{- | Editor integration. -}

-- {-# LANGUAGE OverloadedStrings #-}

module Hledger.UI.Editor (
   -- TextPosition
   endPosition
  ,runEditor
  ,runIadd
  )
where

import Control.Applicative ((<|>))
import Safe
import System.Environment
import System.Exit
import System.FilePath
import System.Process

import Hledger

-- | A position we can move to in a text editor: a line and optional column number.
-- Line number 1 or 0 means the first line. A negative line number means the last line.
type TextPosition = (Int, Maybe Int)

-- | The text position meaning "last line, first column".
endPosition :: Maybe TextPosition
endPosition = Just (-1,Nothing)

-- | Run the hledger-iadd executable on the given file, blocking until it exits,
-- and return the exit code; or raise an error.
-- hledger-iadd is an alternative to the built-in add command.
runIadd :: FilePath -> IO ExitCode
runIadd f = runCommand ("hledger-iadd -f " ++ f) >>= waitForProcess

-- | Run the user's preferred text editor (or try a default editor),
-- on the given file, blocking until it exits, and return the exit
-- code; or raise an error. If a text position is provided, the editor
-- will be focussed at that position in the file, if we know how.
runEditor :: Maybe TextPosition -> FilePath -> IO ExitCode
runEditor mpos f = editFileAtPositionCommand mpos f >>= runCommand >>= waitForProcess

-- | Get a shell command line to open the user's preferred text editor
-- (or a default editor) on the given file, and to focus it at the
-- given text position if one is provided and if we know how.
-- We know how to focus on position for: emacs, vi, nano.
-- We know how to focus on last line for: vi.
--
-- Some tests: With line and column numbers specified,
-- @
-- if EDITOR is:  the command should be:
-- -------------  -----------------------------------
-- notepad        notepad FILE
-- vi             vi +LINE FILE
--                vi + FILE                                    # negative LINE
-- emacs          emacs +LINE:COL FILE
--                emacs FILE                                   # negative LINE
-- (unset)        emacsclient -a '' -nw +LINE:COL FILE
--                emacsclient -a '' -nw FILE                   # negative LINE
-- @
--
-- How to open editors at the last line of a file:
-- @
-- emacs:  emacs FILE -f end-of-buffer
-- vi:     vi + FILE
-- @
--
editFileAtPositionCommand :: Maybe TextPosition -> FilePath -> IO String
editFileAtPositionCommand mpos f = do
  let f' = singleQuoteIfNeeded f
  editcmd <- getEditCommand
  let editor = lowercase $ takeFileName $ headDef "" $ words' editcmd
  let positionarg =
        case mpos of
          Just (l, mc)
            | editor `elem` [
                "ex",
                "vi","vim","view","nvim","evim","eview",
                "gvim","gview","rvim","rview","rgvim","rgview"
                ] -> plusAndMaybeLine l mc
          Just (l, mc)
            | editor `elem` ["emacs", "emacsclient"] -> plusLineAndMaybeColonColumnOrEnd l mc
          Just (l, mc)
            | editor `elem` ["nano"] -> plusLineAndMaybeCommaColumn l mc
          _ -> ""
        where
          plusAndMaybeLine            l _  = "+" ++ if l >= 0 then show l else ""
          plusLineAndMaybeCommaColumn l mc = "+" ++ show l ++ maybe "" ((","++).show) mc
          plusLineAndMaybeColonColumnOrEnd l mc
            | l >= 0    = "+" ++ show l ++ maybe "" ((":"++).show) mc
            | otherwise = ""
            -- otherwise = "-f end-of-buffer"
            -- XXX Problems with this:
            -- it must appear after the filename, whereas +LINE:COL must appear before
            -- it works only with emacs, not emacsclient
  return $ unwords [editcmd, positionarg, f']

-- | Get the user's preferred edit command. This is the value of the
-- $HLEDGER_UI_EDITOR environment variable, or of $EDITOR, or a
-- default ("emacsclient -a '' -nw", which starts/connects to an emacs
-- daemon in terminal mode).
getEditCommand :: IO String
getEditCommand = do
  hledger_ui_editor_env <- lookupEnv "HLEDGER_UI_EDITOR"
  editor_env            <- lookupEnv "EDITOR"
  let Just cmd = hledger_ui_editor_env <|> editor_env <|> Just "emacsclient -a '' -nw"
  return cmd

