-- The error screen, showing a current error condition (such as a parse error after reloading the journal)

{-# LANGUAGE OverloadedStrings, FlexibleContexts, RecordWildCards #-}

module Hledger.UI.ErrorScreen
 (errorScreen
 ,stReloadJournalIfChanged
 )
where

-- import Lens.Micro.Platform ((^.))
import Control.Monad.IO.Class (liftIO)
import Data.Monoid
-- import Data.Maybe
import Data.Time.Calendar (Day)
import Graphics.Vty as Vty
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
import Hledger.UI.UIUtils

errorScreen :: Screen
errorScreen = ErrorScreen{
   sInit    = esInit
  ,sDraw    = esDraw
  ,sHandle  = esHandle
  ,esError  = ""
  }

esInit :: Day -> Bool -> AppState -> AppState
esInit _ _ st@AppState{aScreen=ErrorScreen{}} = st
esInit _ _ _ = error "init function called with wrong screen type, should not happen"

esDraw :: AppState -> [Widget]
esDraw AppState{ -- aopts=_uopts@UIOpts{cliopts_=_copts@CliOpts{reportopts_=_ropts@ReportOpts{query_=querystr}}},
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

esHandle :: AppState -> Vty.Event -> EventM (Next AppState)
esHandle st@AppState{
   aScreen=s@ErrorScreen{}
  ,aopts=UIOpts{cliopts_=copts}
  ,ajournal=j
  ,aMode=mode
  } ev =
  case mode of
    Help ->
      case ev of
        EvKey (KChar 'q') [] -> halt st
        _                    -> helpHandle st ev

    _ -> do
      d <- liftIO getCurrentDay
      case ev of
        EvKey (KChar 'q') [] -> halt st
        EvKey KEsc        [] -> continue $ resetScreens d st
        EvKey k [] | k `elem` [KChar 'h', KChar '?'] -> continue $ setMode Help st
        EvKey (KChar 'g') [] -> do
          (ej, _) <- liftIO $ journalReloadIfChanged copts d j
          case ej of
            Left err -> continue st{aScreen=s{esError=err}} -- show latest parse error
            Right j' -> continue $ regenerateScreens j' d $ popScreen st  -- return to previous screen, and reload it
        -- EvKey (KLeft) []     -> continue $ popScreen st
        -- EvKey (KRight) []    -> error (show curItem) where curItem = listSelectedElement is
        -- fall through to the list's event handler (handles [pg]up/down)
        _                       -> do continue st
                                     -- is' <- handleEvent ev is
                                     -- continue st{aScreen=s{rsState=is'}}
                                     -- continue =<< handleEventLensed st someLens e
esHandle _ _ = error "event handler called with wrong screen type, should not happen"

-- If journal file(s) have changed, reload the journal and regenerate all screens.
-- This is here so it can reference the error screen.
stReloadJournalIfChanged :: CliOpts -> Day -> Journal -> AppState -> IO AppState
stReloadJournalIfChanged copts d j st = do
  (ej, _) <- journalReloadIfChanged copts d j
  return $ case ej of
    Right j' -> regenerateScreens j' d st
    Left err -> screenEnter d errorScreen{esError=err} st

