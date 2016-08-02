-- The error screen, showing a current error condition (such as a parse error after reloading the journal)

{-# LANGUAGE OverloadedStrings, FlexibleContexts, RecordWildCards #-}

module Hledger.UI.ErrorScreen
 (errorScreen
 ,uiCheckBalanceAssertions
 ,uiReloadJournal
 ,uiReloadJournalIfChanged
 )
where

import Brick
-- import Brick.Widgets.Border (borderAttr)
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Data.Monoid
import Data.Time.Calendar (Day)
import Graphics.Vty (Event(..),Key(..))
import Text.Megaparsec

import Hledger.Cli hiding (progname,prognameandversion,green)
import Hledger.UI.UIOptions
import Hledger.UI.UITypes
import Hledger.UI.UIState
import Hledger.UI.UIUtils
import Hledger.UI.Editor

errorScreen :: Screen
errorScreen = ErrorScreen{
   sInit    = esInit
  ,sDraw    = esDraw
  ,sHandle  = esHandle
  ,esError  = ""
  }

esInit :: Day -> Bool -> UIState -> UIState
esInit _ _ ui@UIState{aScreen=ErrorScreen{}} = ui
esInit _ _ _ = error "init function called with wrong screen type, should not happen"

esDraw :: UIState -> [Widget Name]
esDraw UIState{ --aopts=UIOpts{cliopts_=copts@CliOpts{}}
               aScreen=ErrorScreen{..}
              ,aMode=mode} =
  case mode of
    Help       -> [helpDialog, maincontent]
    -- Minibuffer e -> [minibuffer e, maincontent]
    _          -> [maincontent]
  where
    maincontent = Widget Greedy Greedy $ do
      render $ defaultLayout toplabel bottomlabel $ withAttr "error" $ str $ esError
      where
        toplabel =
              withAttr ("border" <> "bold") (str "Oops. Please fix this problem then press g to reload")
              -- <+> (if ignore_assertions_ copts then withAttr (borderAttr <> "query") (str " ignoring") else str " not ignoring")

        bottomlabel = case mode of
                        -- Minibuffer ed -> minibuffer ed
                        _             -> quickhelp
          where
            quickhelp = borderKeysStr [
               ("h", "help")
              ,("ESC", "cancel/top")
              ,("E", "editor")
              ,("g", "reload")
              ,("q", "quit")
              ]

esDraw _ = error "draw function called with wrong screen type, should not happen"

esHandle :: UIState -> Event -> EventM Name (Next UIState)
esHandle ui@UIState{
   aScreen=ErrorScreen{..}
  ,aopts=UIOpts{cliopts_=copts}
  ,ajournal=j
  ,aMode=mode
  } ev =
  case mode of
    Help ->
      case ev of
        EvKey (KChar 'q') [] -> halt ui
        _                    -> helpHandle ui ev

    _ -> do
      d <- liftIO getCurrentDay
      case ev of
        EvKey (KChar 'q') [] -> halt ui
        EvKey KEsc        [] -> continue $ uiCheckBalanceAssertions d $ resetScreens d ui
        EvKey (KChar c)   [] | c `elem` ['h','?'] -> continue $ setMode Help ui
        EvKey (KChar 'E') [] -> suspendAndResume $ void (runEditor pos f) >> uiReloadJournalIfChanged copts d j (popScreen ui)
          where
            (pos,f) = case parsewithString hledgerparseerrorpositionp esError of
                        Right (f,l,c) -> (Just (l, Just c),f)
                        Left  _       -> (endPos, journalFilePath j)
        EvKey (KChar 'g') [] -> liftIO (uiReloadJournalIfChanged copts d j (popScreen ui)) >>= continue . uiCheckBalanceAssertions d
--           (ej, _) <- liftIO $ journalReloadIfChanged copts d j
--           case ej of
--             Left err -> continue ui{aScreen=s{esError=err}} -- show latest parse error
--             Right j' -> continue $ regenerateScreens j' d $ popScreen ui  -- return to previous screen, and reload it
        EvKey (KChar 'I') [] -> continue $ uiCheckBalanceAssertions d (popScreen $ toggleIgnoreBalanceAssertions ui)
        _ -> continue ui

esHandle _ _ = error "event handler called with wrong screen type, should not happen"

-- | Parse the file name, line and column number from a hledger parse error message, if possible.
-- Temporary, we should keep the original parse error location. XXX
hledgerparseerrorpositionp :: ParsecT Dec String t (String, Int, Int)
hledgerparseerrorpositionp = do
  anyChar `manyTill` char '"'
  f <- anyChar `manyTill` (oneOf ['"','\n'])
  string " (line "
  l <- read <$> some digitChar
  string ", column "
  c <- read <$> some digitChar
  return (f, l, c)

-- Unconditionally reload the journal, regenerating the current screen
-- and all previous screens in the history.
-- If reloading fails, enter the error screen, or if we're already
-- on the error screen, update the error displayed.
-- The provided CliOpts are used for reloading, and then saved
-- in the UIState if reloading is successful (otherwise the
-- ui state keeps its old cli opts.)
-- Defined here so it can reference the error screen.
uiReloadJournal :: CliOpts -> Day -> UIState -> IO UIState
uiReloadJournal copts d ui = do
  ej <- journalReload copts
  return $ case ej of
    Right j  -> regenerateScreens j d ui{aopts=(aopts ui){cliopts_=copts}}
    Left err ->
      case ui of
        UIState{aScreen=s@ErrorScreen{}} -> ui{aScreen=s{esError=err}}
        _                                -> screenEnter d errorScreen{esError=err} ui

-- Like uiReloadJournal, but does not bother re-parsing the journal if
-- the file(s) have not changed since last loaded. Always regenerates
-- the current and previous screens though, since opts or date may have changed.
uiReloadJournalIfChanged :: CliOpts -> Day -> Journal -> UIState -> IO UIState
uiReloadJournalIfChanged copts d j ui = do
  (ej, _changed) <- journalReloadIfChanged copts d j
  return $ case ej of
    Right j' -> regenerateScreens j' d ui{aopts=(aopts ui){cliopts_=copts}}
    Left err ->
      case ui of
        UIState{aScreen=s@ErrorScreen{}} -> ui{aScreen=s{esError=err}}
        _                                -> screenEnter d errorScreen{esError=err} ui

-- Re-check any balance assertions in the current journal, and if any
-- fail, enter (or update) the error screen. Or if balance assertions
-- are disabled, do nothing.
uiCheckBalanceAssertions :: Day -> UIState -> UIState
uiCheckBalanceAssertions d ui@UIState{aopts=UIOpts{cliopts_=copts}, ajournal=j}
  | ignore_assertions_ copts = ui
  | otherwise =
    case journalCheckBalanceAssertions j of
      Right _  -> ui
      Left err ->
        case ui of
          UIState{aScreen=s@ErrorScreen{}} -> ui{aScreen=s{esError=err}}
          _                                -> screenEnter d errorScreen{esError=err} ui
