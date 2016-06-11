-- The error screen, showing a current error condition (such as a parse error after reloading the journal)

{-# LANGUAGE OverloadedStrings, FlexibleContexts, RecordWildCards #-}

module Hledger.UI.ErrorScreen
 (errorScreen
 ,uiReloadJournalIfChanged
 )
where

-- import Lens.Micro.Platform ((^.))
import Control.Monad.IO.Class (liftIO)
import Data.Monoid
-- import Data.Maybe
import Data.Time.Calendar (Day)
import Graphics.Vty
import Brick
-- import Brick.Widgets.List
-- import Brick.Widgets.Border
-- import Brick.Widgets.Border.Style
-- import Brick.Widgets.Center
-- import Text.Printf

-- import Hledger
import Hledger.Cli hiding (progname,prognameandversion,green)
import Hledger.UI.UIOptions
-- import Hledger.UI.Theme
import Hledger.UI.UITypes
import Hledger.UI.UIState
import Hledger.UI.UIUtils

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

esDraw :: UIState -> [Widget]
esDraw UIState{ -- aopts=_uopts@UIOpts{cliopts_=_copts@CliOpts{reportopts_=_ropts@ReportOpts{query_=querystr}}},
                             aScreen=ErrorScreen{..}
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

esDraw _ = error "draw function called with wrong screen type, should not happen"

esHandle :: UIState -> Event -> EventM (Next UIState)
esHandle ui@UIState{
   aScreen=s@ErrorScreen{}
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
        EvKey k [] | k `elem` [KChar 'h', KChar '?'] -> continue $ setMode Help ui
        EvKey (KChar 'g') [] -> do
          (ej, _) <- liftIO $ journalReloadIfChanged copts d j
          case ej of
            Left err -> continue ui{aScreen=s{esError=err}} -- show latest parse error
            Right j' -> continue $ regenerateScreens j' d $ popScreen ui  -- return to previous screen, and reload it
        -- EvKey (KLeft) []     -> continue $ popScreen ui
        -- EvKey (KRight) []    -> error (show curItem) where curItem = listSelectedElement is
        -- fall through to the list's event handler (handles [pg]up/down)
        _                       -> do continue ui
                                     -- is' <- handleEvent ev is
                                     -- continue ui{aScreen=s{rsState=is'}}
                                     -- continue =<< handleEventLensed ui someLens e
esHandle _ _ = error "event handler called with wrong screen type, should not happen"

-- If journal file(s) have changed, reload the journal and regenerate all screens.
-- This is here so it can reference the error screen.
uiReloadJournalIfChanged :: CliOpts -> Day -> Journal -> UIState -> IO UIState
uiReloadJournalIfChanged copts d j ui = do
  (ej, _) <- journalReloadIfChanged copts d j
  return $ case ej of
    Right j' -> regenerateScreens j' d ui
    Left err -> screenEnter d errorScreen{esError=err} ui

