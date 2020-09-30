{- | Editor integration. -}

module Hledger.UI.Editor (
   -- TextPosition
   endPosition
  ,runEditor
  ,runIadd
  )
where

import Control.Applicative ((<|>))
import Data.List (intercalate)
import Data.Maybe (catMaybes)
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
-- We know how to focus on position for: emacs, vi, nano, VS code.
-- We know how to focus on last line for: vi.
--
-- Some tests:
-- @
-- EDITOR program is:  LINE/COL specified ?  Command should be:               
-- ------------------  --------------------  ----------------------------------- 
-- emacs, emacsclient  LINE COL              emacs +LINE:COL FILE
--                     LINE                  emacs +LINE     FILE
--                                           emacs           FILE
--
-- nano                LINE COL              nano +LINE,COL FILE
--                     LINE                  nano +LINE     FILE
--                                           nano           FILE
--
-- code                LINE COL              code --goto FILE:LINE:COL
--                     LINE                  code --goto FILE:LINE
--                                           code        FILE
--
-- vi, & variants      LINE [COL]            vi +LINE FILE
--                     LINE (negative)       vi +     FILE
--                                           vi       FILE
--
-- (other PROG)        [LINE [COL]]          PROG FILE
--
-- (not set)           LINE COL              emacsclient -a '' -nw +LINE:COL FILE
--                     LINE                  emacsclient -a '' -nw +LINE     FILE
--                                           emacsclient -a '' -nw           FILE
-- @
--
-- Notes on opening editors at the last line of a file:
-- @
-- emacs:       emacs FILE -f end-of-buffer  # (-f must appear after FILE, +LINE:COL must appear before)
-- emacsclient: can't
-- vi:          vi + FILE
-- @
--
editFileAtPositionCommand :: Maybe TextPosition -> FilePath -> IO String
editFileAtPositionCommand mpos f = do
  cmd <- getEditCommand
  let
    editor = lowercase $ takeBaseName $ headDef "" $ words' cmd
    f' = singleQuoteIfNeeded f
    ml = show.fst <$> mpos
    mc = maybe Nothing (fmap show.snd) mpos
    args = case editor of
             e | e `elem` ["emacs", "emacsclient"] -> ['+' : join ":" [ml,mc], f']
             e | e `elem` ["nano"]                 -> ['+' : join "," [ml,mc], f']
             e | e `elem` ["code"]                 -> ["--goto " ++ join ":" [Just f',ml,mc]]
             e | e `elem` ["vi","vim","view","nvim","evim","eview","gvim","gview","rvim","rview",
                           "rgvim","rgview","ex"]  -> [maybe "" plusMaybeLine ml, f']
             _ -> [f']
           where
             join sep = intercalate sep . catMaybes
             plusMaybeLine l = "+" ++ if take 1 l == "-" then "" else l

  return $ unwords $ cmd:args

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

