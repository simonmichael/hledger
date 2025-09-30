-- The transaction screen, showing a single transaction's general journal entry.

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}
{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}

module Hledger.UI.TransactionScreen
(tsNew
,tsUpdate
,tsDraw
,tsHandle
) where

import Brick
import Brick.Widgets.Edit (editorText, renderEditor)
import Brick.Widgets.List (listMoveTo)
import Control.Monad.IO.Class (liftIO)
import Data.List
import Data.Maybe
import Data.Text qualified as T
import Graphics.Vty (Event(..),Key(..),Modifier(..), Button (BLeft))
import System.Exit (ExitCode (..))

import Hledger
import Hledger.Cli hiding (mode, prices, progname,prognameandversion)
import Hledger.UI.UIOptions
import Hledger.UI.UITypes
import Hledger.UI.UIState
import Hledger.UI.UIUtils
import Hledger.UI.UIScreens
import Hledger.UI.Editor
import Hledger.UI.ErrorScreen (uiCheckBalanceAssertions, uiReload, uiReloadIfFileChanged)
import Hledger.UI.RegisterScreen (rsHandle)

tsDraw :: UIState -> [Widget Name]
tsDraw UIState{aopts=UIOpts{uoCliOpts=copts@CliOpts{reportspec_=rspec@ReportSpec{_rsReportOpts=ropts}}}
              ,ajournal=j
              ,aScreen=TS TSS{_tssTransaction=(i,t')
                              ,_tssTransactions=nts
                              ,_tssAccount=acct
                              }
              ,aMode=mode
              } =
  case mode of
    Help       -> [helpDialog, maincontent]
    _          -> [maincontent]
  where
    maincontent = Widget Greedy Greedy $ render $ defaultLayout toplabel bottomlabel txneditor
      where
        -- as with print, show amounts with all of their decimal places
        t = transactionMapPostingAmounts mixedAmountSetFullPrecision t'

        -- XXX would like to shrink the editor to the size of the entry,
        -- so handler can more easily detect clicks below it
        txneditor =
          renderEditor (vBox . map txt) False $
          editorText TransactionEditor Nothing $
          showTxn ropts rspec j t

        toplabel =
          str "Transaction "
          -- <+> withAttr ("border" <> "bold") (str $ "#" ++ show (tindex t))
          -- <+> str (" ("++show i++" of "++show (length nts)++" in "++acct++")")
          <+> (str $ "#" ++ show (tindex t))
          <+> str " ("
          <+> withAttr (attrName "border" <> attrName "bold") (str $ show i)
          <+> str (" of "++show (length nts))
          <+> togglefilters
          <+> borderQueryStr (unwords . map (quoteIfNeeded . T.unpack) $ querystring_ ropts)
          <+> str (" in "++T.unpack (replaceHiddenAccountsNameWith "All" acct)++")")
          <+> (if ignore_assertions_ . balancingopts_ $ inputopts_ copts then withAttr (attrName "border" <> attrName "query") (str " ignoring balance assertions") else str "")
          where
            togglefilters =
              case concat [
                   uiShowStatus copts $ statuses_ ropts
                  ,if real_ ropts then ["real"] else []
                  ,if empty_ ropts then [] else ["nonzero"]
                  ] of
                [] -> str ""
                fs -> withAttr (attrName "border" <> attrName "query") (str $ " " ++ intercalate ", " fs)

        bottomlabel = quickhelp
                        -- case mode of
                        -- Minibuffer ed -> minibuffer ed
                        -- _             -> quickhelp
          where
            quickhelp = borderKeysStr [
               ("LEFT", "back")
              ,("UP/DOWN", "prev/next txn")
              --,("ESC", "cancel/top")
              -- ,("a", "add")
              ,("E", "edit")
              ,("g", "reload")
              ,("?", "help")
              -- ,("q", "quit")
              ]

tsDraw _ = errorWrongScreenType "tsDraw"  -- PARTIAL:

-- Render a transaction suitably for the transaction screen.
showTxn :: ReportOpts -> ReportSpec -> Journal -> Transaction -> T.Text
showTxn ropts rspec j t =
      showTransactionOneLineAmounts
    $ maybe id (transactionApplyValuation prices styles periodlast (_rsDay rspec)) (value_ ropts)
    $ maybe id transactionToCost (conversionop_ ropts) t
    -- (if real_ ropts then filterTransactionPostings (Real True) else id) -- filter postings by --real
  where
    prices = journalPriceOracle (infer_prices_ ropts) j
    styles = journalCommodityStyles j
    periodlast =
      fromMaybe (error' "TransactionScreen: expected a non-empty journal") $  -- PARTIAL: shouldn't happen
      reportPeriodOrJournalLastDay rspec j

tsHandle :: BrickEvent Name AppEvent -> EventM Name UIState ()
tsHandle ev = do
  ui0 <- get'
  case ui0 of
    ui@UIState{aScreen=TS TSS{_tssTransaction=(i,t), _tssTransactions=nts}
              ,aopts=UIOpts{uoCliOpts=copts}
              ,ajournal=j
              ,aMode=mode
              } ->
      case mode of
        Help ->
          case ev of
            -- VtyEvent (EvKey (KChar 'q') []) -> halt
            VtyEvent (EvKey (KChar 'l') [MCtrl]) -> redraw
            VtyEvent (EvKey (KChar 'z') [MCtrl]) -> suspend ui
            _ -> helpHandle ev

        _ -> do
          d <- liftIO getCurrentDay
          let
            (iprev,tprev) = maybe (i,t) ((i-1),) $ lookup (i-1) nts
            (inext,tnext) = maybe (i,t) ((i+1),) $ lookup (i+1) nts
          case ev of
            VtyEvent (EvKey (KChar 'q') []) -> halt
            VtyEvent (EvKey KEsc        []) -> put' $ resetScreens d ui
            VtyEvent (EvKey (KChar c)   []) | c == '?' -> put' $ setMode Help ui

            -- g or file change: reload the journal and rebuild app state.
            e | e `elem` [VtyEvent (EvKey (KChar 'g') []), AppEvent FileChange] ->
              tsReload copts d ui

              -- for debugging; leaving these here because they were hard to find
              -- \u -> dbguiEv (pshow u) >> put' u  -- doesn't log
              -- \UIState{aScreen=TS tss} -> error' $ pshow $ _tssTransaction tss

            -- E: run editor, reload the journal.
            VtyEvent (EvKey (KChar 'E') []) -> do
              suspendAndResume' $ do
                let (pos,f) = case tsourcepos t of (SourcePos f' l1 c1,_) -> (Just (unPos l1, Just $ unPos c1),f')
                exitcode <- runEditor pos f
                case exitcode of
                  ExitSuccess   -> return ()
                  ExitFailure c -> error' $ "running the text editor failed with exit code " ++ show c
              tsReloadIfFileChanged copts d j ui

            AppEvent (DateChange old _) | isStandardPeriod p && p `periodContainsDate` old ->
              put' $ regenerateScreens j d $ setReportPeriod (DayPeriod d) ui
              where
                p = reportPeriod ui

            VtyEvent (EvKey (KChar 'I') []) -> put' $ uiCheckBalanceAssertions d (toggleIgnoreBalanceAssertions ui)

            -- for toggles that may change the current/prev/next transactions,
            -- we must regenerate the transaction list, like the g handler above ? with regenerateTransactions ? TODO WIP
            -- EvKey (KChar 'E') [] -> put' $ regenerateScreens j d $ stToggleEmpty ui
            -- EvKey (KChar 'C') [] -> put' $ regenerateScreens j d $ stToggleCleared ui
            -- EvKey (KChar 'R') [] -> put' $ regenerateScreens j d $ stToggleReal ui
            VtyEvent (EvKey (KChar 'B') []) -> put' . regenerateScreens j d $ toggleConversionOp ui
            VtyEvent (EvKey (KChar 'V') []) -> put' . regenerateScreens j d $ toggleValue ui

            VtyEvent e | e `elem` moveUpEvents   -> put' $ tsSelect iprev tprev ui
            VtyEvent e | e `elem` moveDownEvents -> put' $ tsSelect inext tnext ui

            -- exit screen on LEFT
            VtyEvent e | e `elem` moveLeftEvents -> put' . popScreen $ tsSelect i t ui  -- Probably not necessary to tsSelect here, but it's safe.
            -- or on a click in the app's left margin.
            VtyEvent (EvMouseUp x _y (Just BLeft)) | x==0 -> put' . popScreen $ tsSelect i t ui

            VtyEvent (EvKey (KChar 'l') [MCtrl]) -> redraw
            VtyEvent (EvKey (KChar 'z') [MCtrl]) -> suspend ui
            _ -> return ()

    _ -> errorWrongScreenType "tsHandle"

    where
      -- Reload and fully regenerate the transaction screen.
      -- XXX On transaction screen or below, this is tricky because of a current limitation of regenerateScreens.
      -- For now we try to work around by re-entering the screen(s).
      -- This can show flicker in the UI and it's hard to handle all situations robustly.
      tsReload copts d ui = uiReload copts d ui >>= reEnterTransactionScreen copts d
      tsReloadIfFileChanged copts d j ui = liftIO (uiReloadIfFileChanged copts d j ui) >>= reEnterTransactionScreen copts d
      
      reEnterTransactionScreen _copts d ui = do
        -- 1. If uiReload (or checking balance assertions) moved us to the error screen, save that, and return to the transaction screen.
        let
          (merrscr, uiTxn) = case aScreen $ uiCheckBalanceAssertions d ui of
            s@(ES _) -> (Just s,  popScreen ui)
            _        -> (Nothing, ui)
        -- 2. Exit to register screen
        let uiReg = popScreen uiTxn
        put' uiReg
        -- 3. Re-enter the transaction screen
        rsHandle (VtyEvent (EvKey KEnter [])) -- PARTIAL assumes we are on the register screen.
        -- 4. Return to the error screen (below the transaction screen) if there was one.
        -- Next events will be handled by esHandle. Error repair will return to the transaction screen.
        maybe (return ()) (put' . flip pushScreen uiTxn) merrscr
          -- doesn't uiTxn have old state from before step 3 ? seems to work

        -- XXX some problem:
        -- 4. Reload once more, possibly re-entering the error screen, by sending a g event.
        -- sendVtyEvents [EvKey (KChar 'g') []]  --  XXX Might be disrupted if other events are queued

        -- XXX doesn't update on non-error change:
        -- 4. Reload once more, possibly re-entering the error screen.
        -- uiTxnOrErr <- uiReload copts d uiTxn
          -- uiReloadIfChanged ?
          -- uiCheckBalanceAssertions ? seems unneeded
        -- put' uiTxnOrErr

        -- XXX not working right:
        -- -- 1. If uiReload (or checking balance assertions) moved us to the error screen, exit to the transaction screen.
        -- let
        --   uiTxn = case aScreen $ uiCheckBalanceAssertions d ui of
        --     ES _ -> popScreen ui
        --     _    -> ui
        -- -- 2. Exit to register screen
        -- put' $ popScreen uiTxn
        -- -- 3. Re-enter the transaction screen, and reload once more.
        -- sendVtyEvents [EvKey KEnter [], EvKey (KChar 'g') []]  -- XXX Might be disrupted if other events are queued


-- | Select a new transaction and update the previous register screen
tsSelect :: Integer -> Transaction -> UIState -> UIState
tsSelect i t ui@UIState{aScreen=TS sst} = case aPrevScreens ui of
    x:xs -> ui'{aPrevScreens=rsSelect i x : xs}
    []   -> ui'
  where ui' = ui{aScreen=TS sst{_tssTransaction=(i,t)}}
tsSelect _ _ ui = ui

-- | Select the nth item on the register screen.
rsSelect :: Integer -> Screen -> Screen
rsSelect i (RS sst@RSS{..}) = RS sst{_rssList=listMoveTo (fromInteger $ i-1) _rssList}
rsSelect _ scr = scr
