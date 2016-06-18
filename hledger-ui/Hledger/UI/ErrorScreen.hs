-- The error screen, showing a current error condition (such as a parse error after reloading the journal)

{-# LANGUAGE OverloadedStrings, FlexibleContexts, RecordWildCards #-}

module Hledger.UI.ErrorScreen
 (errorScreen
 ,uiReloadJournalIfChanged
 )
where

import Control.Monad.IO.Class (liftIO)
import Data.Monoid
import Data.Time.Calendar (Day)
import Graphics.Vty
import Brick

import Hledger.Cli hiding (progname,prognameandversion,green)
import Hledger.UI.UIOptions
import Hledger.UI.UITypes
import Hledger.UI.UIState
import Hledger.UI.UIUtils

errorScreen :: ErrorScreen
errorScreen = ErrorScreen{
   esInit    = esInit_
  ,esDraw    = esDraw_
  ,esHandle  = esHandle_
  ,esError  = ""
  }

esInit_ :: Day -> Bool -> UIState -> UIState
esInit_ _ _ ui@UIState{aScreen=ErrScreen{}} = ui
esInit_ _ _ _ = error "init function called with wrong screen type, should not happen"

esDraw_ :: UIState -> [Widget]
esDraw_ UIState{ -- aopts=_uopts@UIOpts{cliopts_=_copts@CliOpts{reportopts_=_ropts@ReportOpts{query_=querystr}}},
                             aScreen=ErrScreen (ErrorScreen{..})
                             ,aMode=mode} =
  case mode of
    Help       -> [helpDialog, maincontent]
    -- Minibuffer e -> [minibuffer e, maincontent]
    _          -> [maincontent]
  where
    toplabel = withAttr ("border" <> "bold") (str "Oops. Please fix this problem then press g to reload")
    maincontent = Widget Greedy Greedy $ do
      render $ defaultLayout toplabel bottomlabel $ withAttr "error" $ str $ esError
      where
        bottomlabel = case mode of
                        -- Minibuffer ed -> minibuffer ed
                        _             -> quickhelp
        quickhelp = borderKeysStr [
           ("h", "help")
          ,("ESC", "cancel/top")
          ,("g", "reload")
          ,("q", "quit")
          ]

esDraw_ _ = error "draw function called with wrong screen type, should not happen"

esHandle_ :: UIState -> Event -> EventM (Next UIState)
esHandle_ ui@UIState{
   aScreen=ErrScreen(s@ErrorScreen{})
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
        EvKey KEsc        [] -> continue $ resetScreens d ui
        EvKey (KChar c)   [] | c `elem` ['h','?'] -> continue $ setMode Help ui
        EvKey (KChar 'g') [] -> do
          (ej, _) <- liftIO $ journalReloadIfChanged copts d j
          case ej of
            Left err -> continue ui{aScreen=ErrScreen(s{esError=err})} -- show latest parse error
            Right j' -> continue $ regenerateScreens j' d $ popScreen ui  -- return to previous screen, and reload it
        _ -> continue ui

esHandle_ _ _ = error "event handler called with wrong screen type, should not happen"

-- If journal file(s) have changed, reload the journal and regenerate all screens.
-- This is here so it can reference the error screen.
uiReloadJournalIfChanged :: CliOpts -> Day -> Journal -> UIState -> IO UIState
uiReloadJournalIfChanged copts d j ui = do
  (ej, _) <- journalReloadIfChanged copts d j
  return $ case ej of
    Right j' -> regenerateScreens j' d ui
    Left err -> screenEnter d (ErrScreen errorScreen{esError=err}) ui

