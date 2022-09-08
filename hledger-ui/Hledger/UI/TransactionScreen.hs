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

import Control.Monad
import Control.Monad.Except (liftIO)
import Data.List
import Data.Maybe
import qualified Data.Text as T
import Graphics.Vty (Event(..),Key(..),Modifier(..), Button (BLeft))
import Lens.Micro ((^.))
import Brick
import Brick.Widgets.List (listMoveTo)

import Hledger
import Hledger.Cli hiding (mode, prices, progname,prognameandversion)
import Hledger.UI.UIOptions
import Hledger.UI.UITypes
import Hledger.UI.UIState
import Hledger.UI.UIUtils
import Hledger.UI.UIScreens
import Hledger.UI.Editor
import Brick.Widgets.Edit (editorText, renderEditor)
import Hledger.UI.ErrorScreen (uiReloadJournalIfChanged, uiCheckBalanceAssertions)

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
    Help       -> [helpDialog copts, maincontent]
    -- Minibuffer e -> [minibuffer e, maincontent]
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
               ("?", "help")
              ,("LEFT", "back")
              ,("UP/DOWN", "prev/next")
              --,("ESC", "cancel/top")
              -- ,("a", "add")
              ,("E", "editor")
              ,("g", "reload")
              ,("q", "quit")
              ]

tsDraw _ = errorWrongScreenType "draw function"  -- PARTIAL:

-- Render a transaction suitably for the transaction screen.
showTxn :: ReportOpts -> ReportSpec -> Journal -> Transaction -> T.Text
showTxn ropts rspec j t =
      showTransactionOneLineAmounts
    $ maybe id (transactionApplyValuation prices styles periodlast (_rsDay rspec)) (value_ ropts)
    $ maybe id (transactionToCost styles) (conversionop_ ropts) t
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
          let
            d = copts^.rsDay
            (iprev,tprev) = maybe (i,t) ((i-1),) $ lookup (i-1) nts
            (inext,tnext) = maybe (i,t) ((i+1),) $ lookup (i+1) nts
          case ev of
            VtyEvent (EvKey (KChar 'q') []) -> halt
            VtyEvent (EvKey KEsc        []) -> put' $ resetScreens d ui
            VtyEvent (EvKey (KChar c)   []) | c == '?' -> put' $ setMode Help ui
            VtyEvent (EvKey (KChar 'E') []) -> suspendAndResume $ void (runEditor pos f) >> uiReloadJournalIfChanged copts d j ui
              where
                (pos,f) = case tsourcepos t of
                            (SourcePos f' l1 c1,_) -> (Just (unPos l1, Just $ unPos c1),f')
            AppEvent (DateChange old _) | isStandardPeriod p && p `periodContainsDate` old ->
              put' $ regenerateScreens j d $ setReportPeriod (DayPeriod d) ui
              where
                p = reportPeriod ui
            e | e `elem` [VtyEvent (EvKey (KChar 'g') []), AppEvent FileChange] -> do
              -- plog (if e == AppEvent FileChange then "file change" else "manual reload") "" `seq` return ()
              ej <- liftIO . runExceptT $ journalReload copts
              case ej of
                Left err -> put' $ pushScreen (esNew err) ui
                Right j' -> put' $ regenerateScreens j' d ui
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

    _ -> errorWrongScreenType "event handler"

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
