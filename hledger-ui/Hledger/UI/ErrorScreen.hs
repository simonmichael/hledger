-- The error screen, showing a current error condition (such as a parse error after reloading the journal)

{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Hledger.UI.ErrorScreen
 (errorScreen
 ,uiCheckBalanceAssertions
 ,uiReloadJournal
 ,uiReloadJournalIfChanged
 )
where

import Brick
-- import Brick.Widgets.Border ("border")
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Data.Time.Calendar (Day)
import Data.Void (Void)
import Graphics.Vty (Event(..),Key(..),Modifier(..))
import Text.Megaparsec
import Text.Megaparsec.Char

import Hledger.Cli hiding (progname,prognameandversion)
import Hledger.UI.UIOptions
import Hledger.UI.UITypes
import Hledger.UI.UIState
import Hledger.UI.UIUtils
import Hledger.UI.Editor
import Data.Foldable (asum)

errorScreen :: Screen
errorScreen = ErrorScreen{
   sInit    = esInit
  ,sDraw    = esDraw
  ,sHandle  = esHandle
  ,esError  = ""
  }

esInit :: Day -> Bool -> UIState -> UIState
esInit _ _ ui@UIState{aScreen=ErrorScreen{}} = ui
esInit _ _ _ = error "init function called with wrong screen type, should not happen"  -- PARTIAL:

esDraw :: UIState -> [Widget Name]
esDraw UIState{aopts=UIOpts{cliopts_=copts@CliOpts{}}
              ,aScreen=ErrorScreen{..}
              ,aMode=mode
              } =
  case mode of
    Help       -> [helpDialog copts, maincontent]
    -- Minibuffer e -> [minibuffer e, maincontent]
    _          -> [maincontent]
  where
    maincontent = Widget Greedy Greedy $ do
      render $ defaultLayout toplabel bottomlabel $ withAttr "error" $ str $ esError
      where
        toplabel =
              withAttr ("border" <> "bold") (str "Oops. Please fix this problem then press g to reload")
              -- <+> (if ignore_assertions_ copts then withAttr ("border" <> "query") (str " ignoring") else str " not ignoring")

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

esDraw _ = error "draw function called with wrong screen type, should not happen"  -- PARTIAL:

esHandle :: UIState -> BrickEvent Name AppEvent -> EventM Name (Next UIState)
esHandle ui@UIState{aScreen=ErrorScreen{..}
                   ,aopts=UIOpts{cliopts_=copts}
                   ,ajournal=j
                   ,aMode=mode
                   }
         ev =
  case mode of
    Help ->
      case ev of
        VtyEvent (EvKey (KChar 'q') []) -> halt ui
        VtyEvent (EvKey (KChar 'l') [MCtrl]) -> redraw ui
        VtyEvent (EvKey (KChar 'z') [MCtrl]) -> suspend ui
        _                    -> helpHandle ui ev

    _ -> do
      d <- liftIO getCurrentDay
      case ev of
        VtyEvent (EvKey (KChar 'q') []) -> halt ui
        VtyEvent (EvKey KEsc        []) -> continue $ uiCheckBalanceAssertions d $ resetScreens d ui
        VtyEvent (EvKey (KChar c)   []) | c `elem` ['h','?'] -> continue $ setMode Help ui
        VtyEvent (EvKey (KChar 'E') []) -> suspendAndResume $ void (runEditor pos f) >> uiReloadJournalIfChanged copts d j (popScreen ui)
          where
            (pos,f) = case parsewithString hledgerparseerrorpositionp esError of
                        Right (f,l,c) -> (Just (l, Just c),f)
                        Left  _       -> (endPosition, journalFilePath j)
        e | e `elem` [VtyEvent (EvKey (KChar 'g') []), AppEvent FileChange] ->
          liftIO (uiReloadJournal copts d (popScreen ui)) >>= continue . uiCheckBalanceAssertions d
--           (ej, _) <- liftIO $ journalReloadIfChanged copts d j
--           case ej of
--             Left err -> continue ui{aScreen=s{esError=err}} -- show latest parse error
--             Right j' -> continue $ regenerateScreens j' d $ popScreen ui  -- return to previous screen, and reload it
        VtyEvent (EvKey (KChar 'I') []) -> continue $ uiCheckBalanceAssertions d (popScreen $ toggleIgnoreBalanceAssertions ui)
        VtyEvent (EvKey (KChar 'l') [MCtrl]) -> redraw ui
        VtyEvent (EvKey (KChar 'z') [MCtrl]) -> suspend ui
        _ -> continue ui

esHandle _ _ = error "event handler called with wrong screen type, should not happen"  -- PARTIAL:

-- | Parse the file name, line and column number from a hledger parse error message, if possible.
-- Temporary, we should keep the original parse error location. XXX
-- Keep in sync with 'Hledger.Data.Transaction.showGenericSourcePos'
hledgerparseerrorpositionp :: ParsecT Void String t (String, Int, Int)
hledgerparseerrorpositionp = do
  anySingle `manyTill` char '"'
  f <- anySingle `manyTill` (oneOf ['"','\n'])
  choice [
      do
          string " (line "
          l <- read <$> some digitChar
          string ", column "
          c <- read <$> some digitChar
          return (f, l, c),
      do
          string " (lines "
          l <- read <$> some digitChar
          char '-'
          some digitChar
          char ')'
          return (f, l, 1)
      ]


-- | Unconditionally reload the journal, regenerating the current screen
-- and all previous screens in the history as of the provided today-date.
-- If reloading fails, enter the error screen, or if we're already
-- on the error screen, update the error displayed.
-- Defined here so it can reference the error screen.
--
-- The provided CliOpts are used for reloading, and then saved in the
-- UIState if reloading is successful (otherwise the UIState keeps its old
-- CliOpts.) (XXX needed for.. ?)
--
-- Forecasted transactions are always generated, as at hledger-ui startup.
-- If a forecast period is specified in the provided opts, or was specified
-- at startup, it is preserved.
--
uiReloadJournal :: CliOpts -> Day -> UIState -> IO UIState
uiReloadJournal copts d ui = do
  ej <-
    let copts' = enableForecastPreservingPeriod ui copts
    in journalReload copts'
  return $ case ej of
    Right j  -> regenerateScreens j d ui
    Left err ->
      case ui of
        UIState{aScreen=s@ErrorScreen{}} -> ui{aScreen=s{esError=err}}
        _                                -> screenEnter d errorScreen{esError=err} ui

-- | Like uiReloadJournal, but does not re-parse the journal if the file(s)
-- have not changed since last loaded. Always regenerates the screens though,
-- since the provided options or today-date may have changed.
uiReloadJournalIfChanged :: CliOpts -> Day -> Journal -> UIState -> IO UIState
uiReloadJournalIfChanged copts d j ui = do
  (ej, _changed) <-
    let copts' = enableForecastPreservingPeriod ui copts
    in journalReloadIfChanged copts' d j
  return $ case ej of
    Right j' -> regenerateScreens j' d ui
    Left err ->
      case ui of
        UIState{aScreen=s@ErrorScreen{}} -> ui{aScreen=s{esError=err}}
        _                                -> screenEnter d errorScreen{esError=err} ui

-- | Ensure this CliOpts enables forecasted transactions.
-- If a forecast period was specified in the old CliOpts,
-- or in the provided UIState's startup options,
-- it is preserved.
enableForecastPreservingPeriod :: UIState -> CliOpts -> CliOpts
enableForecastPreservingPeriod ui copts@CliOpts{reportspec_=rspec@ReportSpec{rsOpts=ropts}} =
  copts{reportspec_=rspec{rsOpts=ropts{forecast_=mforecast}}}
  where
    mforecast = asum [mprovidedforecastperiod, mstartupforecastperiod, mdefaultforecastperiod]
      where
        mprovidedforecastperiod = forecast_ ropts
        mstartupforecastperiod  = forecast_ $ rsOpts $ reportspec_ $ cliopts_ $ astartupopts ui
        mdefaultforecastperiod  = Just nulldatespan

-- Re-check any balance assertions in the current journal, and if any
-- fail, enter (or update) the error screen. Or if balance assertions
-- are disabled, do nothing.
uiCheckBalanceAssertions :: Day -> UIState -> UIState
uiCheckBalanceAssertions d ui@UIState{aopts=UIOpts{cliopts_=copts}, ajournal=j}
  | ignore_assertions_ . balancingopts_ $ inputopts_ copts = ui
  | otherwise =
    case journalCheckBalanceAssertions j of
      Nothing  -> ui
      Just err ->
        case ui of
          UIState{aScreen=s@ErrorScreen{}} -> ui{aScreen=s{esError=err}}
          _                                -> screenEnter d errorScreen{esError=err} ui
