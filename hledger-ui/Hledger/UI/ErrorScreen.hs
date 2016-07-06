-- The error screen, showing a current error condition (such as a parse error after reloading the journal)

{-# LANGUAGE OverloadedStrings, FlexibleContexts, RecordWildCards #-}

module Hledger.UI.ErrorScreen
 (errorScreen
 ,uiReloadJournalIfChanged
 )
where

import Brick
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Data.Monoid
import Data.Time.Calendar (Day)
import Graphics.Vty (Event(..),Key(..))
import Text.Parsec

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
          ,("E", "editor")
          ,("g", "reload")
          ,("q", "quit")
          ]

esDraw _ = error "draw function called with wrong screen type, should not happen"

esHandle :: UIState -> Event -> EventM (Next UIState)
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
        EvKey KEsc        [] -> continue $ resetScreens d ui
        EvKey (KChar c)   [] | c `elem` ['h','?'] -> continue $ setMode Help ui
        EvKey (KChar 'E') [] -> suspendAndResume $ void (runEditor pos f) >> uiReloadJournalIfChanged copts d j (popScreen ui)
          where
            (pos,f) = case parsewith hledgerparseerrorpositionp esError of
                        Right (f,l,c) -> (Just (l, Just c),f)
                        Left  _       -> (endPos, journalFilePath j)
        EvKey (KChar 'g') [] -> liftIO (uiReloadJournalIfChanged copts d j (popScreen ui)) >>= continue
--           (ej, _) <- liftIO $ journalReloadIfChanged copts d j
--           case ej of
--             Left err -> continue ui{aScreen=s{esError=err}} -- show latest parse error
--             Right j' -> continue $ regenerateScreens j' d $ popScreen ui  -- return to previous screen, and reload it
        _ -> continue ui

esHandle _ _ = error "event handler called with wrong screen type, should not happen"

-- | Parse the file name, line and column number from a hledger parse error message, if possible.
-- Temporary, we should keep the original parse error location. XXX
hledgerparseerrorpositionp = do
  anyChar `manyTill` char '"'
  f <- anyChar `manyTill` (oneOf ['"','\n'])
  string " (line "
  l <- read <$> many1 digit
  string ", column "
  c <- read <$> many1 digit
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

