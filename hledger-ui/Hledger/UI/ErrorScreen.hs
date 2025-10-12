-- The error screen, showing a current error condition (such as a parse error after reloading the journal)

{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}

module Hledger.UI.ErrorScreen
 (esNew
 ,esUpdate
 ,esDraw
 ,esHandle
 ,uiCheckBalanceAssertions
 ,uiReload
 ,uiReloadIfFileChanged
 ,uiToggleBalanceAssertions
 )
where

import Brick
-- import Brick.Widgets.Border ("border")
import Control.Monad.IO.Class (liftIO)
import Data.Time.Calendar (Day)
import Data.Void (Void)
import Graphics.Vty (Event(..),Key(..),Modifier(..))
import Lens.Micro ((^.))
import Safe (headMay)
import Text.Megaparsec
import Text.Megaparsec.Char

import Hledger.Cli hiding (mode, progname,prognameandversion)
import Hledger.UI.UIOptions
import Hledger.UI.UITypes
import Hledger.UI.UIState
import Hledger.UI.UIUtils
import Hledger.UI.UIScreens
import Hledger.UI.Editor

esDraw :: UIState -> [Widget Name]
esDraw UIState{aScreen=ES ESS{..}
              ,aMode=mode
              } =
  case mode of
    Help       -> [helpDialog, maincontent]
    _          -> [maincontent]
  where
    maincontent = Widget Greedy Greedy $ do
      render $ defaultLayout toplabel bottomlabel $ withAttr (attrName "error") $ str $ _essError
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

esDraw _ = error' "draw function called with wrong screen type, should not happen"  -- PARTIAL:

esHandle :: BrickEvent Name AppEvent -> EventM Name UIState ()
esHandle ev = do
  ui0 <- get'
  case ui0 of
    ui@UIState{aScreen=ES ESS{..}
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
          d <- liftIO getCurrentDay
          case ev of
            VtyEvent (EvKey (KChar 'q') []) -> halt
            VtyEvent (EvKey KEsc        []) -> put' $ uiCheckBalanceAssertions d $ resetScreens d ui
            VtyEvent (EvKey (KChar c)   []) | c `elem` ['h','?'] -> put' $ setMode Help ui

            -- g or file change: reload the journal and rebuild app state.
            e | e `elem` [VtyEvent (EvKey (KChar 'g') []), AppEvent FileChange] -> esReload copts d ui

            -- E: run editor, reload the journal.
            VtyEvent (EvKey (KChar 'E') []) -> do
              suspendAndResume' $ do
                let
                  (pos,f) = case parsewithString hledgerparseerrorpositionp _essError of
                              Right (f',l,c) -> (Just (l, Just c),f')
                              Left  _       -> (endPosition, journalFilePath j)
                runEditor pos f
              esReloadIfFileChanged copts d j ui

            VtyEvent (EvKey (KChar 'I') []) -> uiToggleBalanceAssertions d (popScreen ui)
            VtyEvent (EvKey (KChar 'l') [MCtrl]) -> redraw
            VtyEvent (EvKey (KChar 'z') [MCtrl]) -> suspend ui
            _ -> return ()

    _ -> errorWrongScreenType "esHandle"

    where
      -- Reload and fully regenerate the error screen.
      -- XXX On an error screen below the transaction screen, this is tricky because of a current limitation of regenerateScreens.
      -- For now we try to work around by re-entering the transaction screen.
      -- This can show flicker in the UI and it's hard to handle all situations robustly.
      esReload copts d ui = uiReload copts d ui >>= maybeReloadErrorScreen copts d
      esReloadIfFileChanged copts d j ui = liftIO (uiReloadIfFileChanged copts d j ui) >>= maybeReloadErrorScreen copts d
      maybeReloadErrorScreen copts d ui =
        case headMay $ aPrevScreens ui of
          Just (TS _) -> do
            -- check balance assertions, exit to register screen, enter transaction screen, reload once more
            put' $ popScreen $ popScreen $ uiCheckBalanceAssertions d ui
            sendVtyEvents [EvKey KEnter [], EvKey (KChar 'g') []]  -- XXX Might be disrupted if other events are queued ?
          _ -> uiReload copts d (popScreen ui) >>= put' . uiCheckBalanceAssertions d

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


-- Defined here so it can reference the error screen:

-- | Modify some input options for hledger-ui (enable --forecast).
uiAdjustOpts :: UIOpts -> CliOpts -> CliOpts
uiAdjustOpts uopts = enableForecast uopts

-- | Reload the journal from its input files, then update the ui app state accordingly.
-- This means regenerate the entire screen stack from top level down to the current screen, using the provided today-date.
-- As a convenience (usually), if journal reloading fails, this enters the error screen, or if already there, updates its message.
--
-- The provided cli options can influence reloading; then if reloading succeeds they are saved in the ui state,
-- otherwise the UIState keeps its old options. (XXX needed for.. ?)
--
-- Like at hledger-ui startup, --forecast is always enabled.
-- A forecast period specified in the provided opts, or at startup, is preserved.
--
uiReload :: CliOpts -> Day -> UIState -> EventM Name UIState UIState
uiReload copts d ui = liftIO $ do
  ej <-
    let copts1 = uiAdjustOpts (astartupopts ui) copts
    in runExceptT $ journalTransform copts1 <$> journalReload copts1
  -- dbg1IO "uiReload before reload" (map tdescription $ jtxns $ ajournal ui)
  return $ case ej of
    Right j  ->
      -- dbg1 "uiReload after reload" (map tdescription $ jtxns j) $
      regenerateScreens j d ui
    Left err ->
      case ui of
        UIState{aScreen=ES _} -> ui{aScreen=esNew err}
        _                      -> pushScreen (esNew err) ui
      -- XXX GHC 9.2 warning:
      -- hledger-ui/Hledger/UI/ErrorScreen.hs:164:59: warning: [-Wincomplete-record-updates]
      --     Pattern match(es) are non-exhaustive
      --     In a record-update construct:
      --         Patterns of type ‘Screen’ not matched:
      --             AccountsScreen _ _ _ _ _
      --             RegisterScreen _ _ _ _ _ _
      --             TransactionScreen _ _ _ _ _ _

-- | Like uiReload, except it skips re-reading the journal if its file(s) have not changed
-- since it was last loaded. The up app state is always updated, since the options or today-date may have changed.
-- Also, this one runs in IO, suitable for suspendAndResume.
uiReloadIfFileChanged :: CliOpts -> Day -> Journal -> UIState -> IO UIState
uiReloadIfFileChanged copts d j ui = do
  ej <-
    let copts1 = uiAdjustOpts (astartupopts ui) copts
    in runExceptT $ journalReloadIfChanged copts1 d j
  return $ case ej of
    Right (j', _) -> regenerateScreens j' d ui
    Left err -> case aScreen ui of
        ES _ -> ui{aScreen=esNew err}
        _    -> pushScreen (esNew err) ui

-- Re-check any balance assertions in the current journal,
-- and if any fail, enter (or update) the error screen.
-- Or if balance assertions are disabled or pivot is active, do nothing.
-- (When pivot is active, assertions have already been checked on the pre-pivot journal,
-- and the current post-pivot journal's account names don't match the original assertions.)
uiCheckBalanceAssertions :: Day -> UIState -> UIState
uiCheckBalanceAssertions _d ui@UIState{ajournal=j, aopts=UIOpts{uoCliOpts=CliOpts{inputopts_=InputOpts{pivot_=pval}}}}
  | ui^.ignore_assertions = ui        -- user disabled checks
  | not (null pval) = ui              -- post-pivot journal, assertions already checked pre-pivot
  | otherwise =
    case journalCheckBalanceAssertions j of
      Right () -> ui
      Left err ->
        case ui of
          UIState{aScreen=ES sst} -> ui{aScreen=ES sst{_essError=err}}
          _                        -> pushScreen (esNew err) ui

-- | Toggle ignoring balance assertions (when user presses I), and if no longer ignoring, recheck them.
-- Normally the recheck is done quickly on the in-memory journal.
-- But if --pivot is active, a full journal reload is done instead
-- (because we can't check balance assertions after pivoting has occurred).
-- In that case, this operation could be slower and could reveal other data changes (not just balance assertion failures).
uiToggleBalanceAssertions :: Day -> UIState -> EventM Name UIState ()
uiToggleBalanceAssertions d ui@UIState{aopts=UIOpts{uoCliOpts=copts@CliOpts{inputopts_=InputOpts{pivot_=pivotval}}}} =
  let ui' = toggleIgnoreBalanceAssertions ui
  in case (ui'^.ignore_assertions, null pivotval) of
    (True, _)      -> put' ui'                                -- ignoring enabled, no check needed
    (False, True)  -> put' $ uiCheckBalanceAssertions d ui'   -- unpivoted journal, can check in memory
    (False, False) -> uiReload copts d ui' >>= put'           -- pivoted journal, must reload to check it
