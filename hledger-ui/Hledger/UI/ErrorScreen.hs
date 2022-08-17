-- The error screen, showing a current error condition (such as a parse error after reloading the journal)

{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}

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
import Control.Monad.Except (liftIO)
import Data.Time.Calendar (Day)
import Data.Void (Void)
import Graphics.Vty (Event(..),Key(..),Modifier(..))
import Lens.Micro ((^.))
import Text.Megaparsec
import Text.Megaparsec.Char

import Hledger.Cli hiding (progname,prognameandversion)
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
esInit _ _ _ = error "init function called with wrong screen type, should not happen"  -- PARTIAL:

esDraw :: UIState -> [Widget Name]
esDraw UIState{aopts=UIOpts{uoCliOpts=copts}
              ,aScreen=ErrorScreen{..}
              ,aMode=mode
              } =
  case mode of
    Help       -> [helpDialog copts, maincontent]
    -- Minibuffer e -> [minibuffer e, maincontent]
    _          -> [maincontent]
  where
    maincontent = Widget Greedy Greedy $ do
      render $ defaultLayout toplabel bottomlabel $ withAttr (attrName "error") $ str $ esError
      where
        toplabel =
              withAttr (attrName "border" <> attrName "bold") (str "Oops. Please fix this problem then press g to reload")
              -- <+> (if ignore_assertions_ copts then withAttr ("border" <> "query") (str " ignoring") else str " not ignoring")

        bottomlabel = quickhelp
                        -- case mode of
                        -- Minibuffer ed -> minibuffer ed
                        -- _             -> quickhelp
          where
            quickhelp = borderKeysStr [
               ("h", "help")
              ,("ESC", "cancel/top")
              ,("E", "editor")
              ,("g", "reload")
              ,("q", "quit")
              ]

esDraw _ = error "draw function called with wrong screen type, should not happen"  -- PARTIAL:

esHandle :: BrickEvent Name AppEvent -> EventM Name UIState ()
esHandle ev = do
  ui0 <- get
  case ui0 of
    ui@UIState{aScreen=ErrorScreen{..}
              ,aopts=UIOpts{uoCliOpts=copts}
              ,ajournal=j
              ,aMode=mode
              } ->
      case mode of
        Help ->
          case ev of
            VtyEvent (EvKey (KChar 'q') []) -> halt
            VtyEvent (EvKey (KChar 'l') [MCtrl]) -> redraw
            VtyEvent (EvKey (KChar 'z') [MCtrl]) -> suspend ui
            _                    -> helpHandle ev

        _ -> do
          let d = copts^.rsDay
          case ev of
            VtyEvent (EvKey (KChar 'q') []) -> halt
            VtyEvent (EvKey KEsc        []) -> put $ uiCheckBalanceAssertions d $ resetScreens d ui
            VtyEvent (EvKey (KChar c)   []) | c `elem` ['h','?'] -> put $ setMode Help ui
            VtyEvent (EvKey (KChar 'E') []) -> suspendAndResume $ void (runEditor pos f) >> uiReloadJournalIfChanged copts d j (popScreen ui)
              where
                (pos,f) = case parsewithString hledgerparseerrorpositionp esError of
                            Right (f,l,c) -> (Just (l, Just c),f)
                            Left  _       -> (endPosition, journalFilePath j)
            e | e `elem` [VtyEvent (EvKey (KChar 'g') []), AppEvent FileChange] ->
              liftIO (uiReloadJournal copts d (popScreen ui)) >>= put . uiCheckBalanceAssertions d
              -- (ej, _) <- liftIO $ journalReloadIfChanged copts d j
              -- case ej of
              --   Left err -> continue ui{aScreen=s{esError=err}} -- show latest parse error
              --   Right j' -> continue $ regenerateScreens j' d $ popScreen ui  -- return to previous screen, and reload it
            VtyEvent (EvKey (KChar 'I') []) -> put $ uiCheckBalanceAssertions d (popScreen $ toggleIgnoreBalanceAssertions ui)
            VtyEvent (EvKey (KChar 'l') [MCtrl]) -> redraw
            VtyEvent (EvKey (KChar 'z') [MCtrl]) -> suspend ui
            _ -> return ()

    _ -> errorWrongScreenType

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
    in runExceptT $ journalReload copts'
  return $ case ej of
    Right j  -> regenerateScreens j d ui
    Left err ->
      case ui of
        UIState{aScreen=s@ErrorScreen{}} -> ui{aScreen=s{esError=err}}
        _                                -> screenEnter d errorScreen{esError=err} ui
      -- XXX GHC 9.2 warning:
      -- hledger-ui/Hledger/UI/ErrorScreen.hs:164:59: warning: [-Wincomplete-record-updates]
      --     Pattern match(es) are non-exhaustive
      --     In a record-update construct:
      --         Patterns of type ‘Screen’ not matched:
      --             AccountsScreen _ _ _ _ _
      --             RegisterScreen _ _ _ _ _ _
      --             TransactionScreen _ _ _ _ _ _

-- | Like uiReloadJournal, but does not re-parse the journal if the file(s)
-- have not changed since last loaded. Always regenerates the screens though,
-- since the provided options or today-date may have changed.
uiReloadJournalIfChanged :: CliOpts -> Day -> Journal -> UIState -> IO UIState
uiReloadJournalIfChanged copts d j ui = do
  let copts' = enableForecastPreservingPeriod ui copts
  ej <- runExceptT $ journalReloadIfChanged copts' d j
  return $ case ej of
    Right (j', _) -> regenerateScreens j' d ui
    Left err -> case ui of
        UIState{aScreen=s@ErrorScreen{}} -> ui{aScreen=s{esError=err}}
        _                                -> screenEnter d errorScreen{esError=err} ui

-- Re-check any balance assertions in the current journal, and if any
-- fail, enter (or update) the error screen. Or if balance assertions
-- are disabled, do nothing.
uiCheckBalanceAssertions :: Day -> UIState -> UIState
uiCheckBalanceAssertions d ui@UIState{ajournal=j}
  | ui^.ignore_assertions = ui
  | otherwise =
    case journalCheckBalanceAssertions j of
      Nothing  -> ui
      Just err ->
        case ui of
          UIState{aScreen=s@ErrorScreen{}} -> ui{aScreen=s{esError=err}}
          _                                -> screenEnter d errorScreen{esError=err} ui
