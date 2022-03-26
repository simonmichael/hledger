-- The transaction screen, showing a single transaction's general journal entry.

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}

module Hledger.UI.TransactionScreen
( transactionScreen
) where

import Control.Monad
import Control.Monad.Except (liftIO)
import Data.List
import Data.Maybe
import qualified Data.Text as T
import Data.Time.Calendar (Day)
import qualified Data.Vector as V
import Graphics.Vty (Event(..),Key(..),Modifier(..), Button (BLeft))
import Lens.Micro ((^.))
import Brick
import Brick.Widgets.List (listElementsL, listMoveTo, listSelectedElement)

import Hledger
import Hledger.Cli hiding (progname,prognameandversion)
import Hledger.UI.UIOptions
-- import Hledger.UI.Theme
import Hledger.UI.UITypes
import Hledger.UI.UIState
import Hledger.UI.UIUtils
import Hledger.UI.Editor
import Hledger.UI.ErrorScreen
import Brick.Widgets.Edit (editorText, renderEditor)

transactionScreen :: Screen
transactionScreen = TransactionScreen{
   sInit   = tsInit
  ,sDraw   = tsDraw
  ,sHandle = tsHandle
  ,tsTransaction  = (1,nulltransaction)
  ,tsTransactions = [(1,nulltransaction)]
  ,tsAccount      = ""
  }

tsInit :: Day -> Bool -> UIState -> UIState
tsInit _d _reset ui@UIState{aopts=UIOpts{}
                           ,ajournal=_j
                           ,aScreen=s@TransactionScreen{tsTransaction=(_,t),tsTransactions=nts}
                           ,aPrevScreens=prevscreens
                           } =
    ui{aScreen=s{tsTransaction=(i',t'),tsTransactions=nts'}}
  where
    i' = maybe 0 (toInteger . (+1)) . elemIndex t' $ map snd nts'
    -- If the previous screen was RegisterScreen, use the listed and selected items as
    -- the transactions. Otherwise, use the provided transaction and list.
    (t',nts') = case prevscreens of
        RegisterScreen{rsList=xs}:_ -> (seltxn, zip [1..] $ map rsItemTransaction nonblanks)
          where
            seltxn = maybe nulltransaction (rsItemTransaction . snd) $ listSelectedElement xs
            nonblanks = V.toList . V.takeWhile (not . T.null . rsItemDate) $ xs ^. listElementsL
        _                           -> (t, nts)
tsInit _ _ _ = error "init function called with wrong screen type, should not happen"  -- PARTIAL:

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

tsDraw :: UIState -> [Widget Name]
tsDraw UIState{aopts=UIOpts{uoCliOpts=copts@CliOpts{reportspec_=rspec@ReportSpec{_rsReportOpts=ropts}}}
              ,ajournal=j
              ,aScreen=TransactionScreen{tsTransaction=(i,t')
                                        ,tsTransactions=nts
                                        ,tsAccount=acct
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
          <+> withAttr ("border" <> "bold") (str $ show i)
          <+> str (" of "++show (length nts))
          <+> togglefilters
          <+> borderQueryStr (unwords . map (quoteIfNeeded . T.unpack) $ querystring_ ropts)
          <+> str (" in "++T.unpack (replaceHiddenAccountsNameWith "All" acct)++")")
          <+> (if ignore_assertions_ . balancingopts_ $ inputopts_ copts then withAttr ("border" <> "query") (str " ignoring balance assertions") else str "")
          where
            togglefilters =
              case concat [
                   uiShowStatus copts $ statuses_ ropts
                  ,if real_ ropts then ["real"] else []
                  ,if empty_ ropts then [] else ["nonzero"]
                  ] of
                [] -> str ""
                fs -> withAttr ("border" <> "query") (str $ " " ++ intercalate ", " fs)

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

tsDraw _ = error "draw function called with wrong screen type, should not happen"  -- PARTIAL:

tsHandle :: UIState -> BrickEvent Name AppEvent -> EventM Name (Next UIState)
tsHandle ui@UIState{aScreen=TransactionScreen{tsTransaction=(i,t), tsTransactions=nts}
                   ,aopts=UIOpts{uoCliOpts=copts@CliOpts{reportspec_=rspec@ReportSpec{_rsReportOpts=ropts}}}
                   ,ajournal=j
                   ,aMode=mode
                   }
         ev =
  case mode of
    Help ->
      case ev of
        -- VtyEvent (EvKey (KChar 'q') []) -> halt ui
        VtyEvent (EvKey (KChar 'l') [MCtrl]) -> redraw ui
        VtyEvent (EvKey (KChar 'z') [MCtrl]) -> suspend ui
        _                    -> helpHandle ui ev

    _ -> do
      let
        d = copts^.rsDay
        (iprev,tprev) = maybe (i,t) ((i-1),) $ lookup (i-1) nts
        (inext,tnext) = maybe (i,t) ((i+1),) $ lookup (i+1) nts
      case ev of
        VtyEvent (EvKey (KChar 'q') []) -> halt ui
        VtyEvent (EvKey KEsc        []) -> continue $ resetScreens d ui
        VtyEvent (EvKey (KChar c)   []) | c == '?' -> continue $ setMode Help ui
        VtyEvent (EvKey (KChar 'E') []) -> suspendAndResume $ void (runEditor pos f) >> uiReloadJournalIfChanged copts d j ui
          where
            (pos,f) = case tsourcepos t of
                        (SourcePos f l1 c1,_) -> (Just (unPos l1, Just $ unPos c1),f)
        AppEvent (DateChange old _) | isStandardPeriod p && p `periodContainsDate` old ->
          continue $ regenerateScreens j d $ setReportPeriod (DayPeriod d) ui
          where
            p = reportPeriod ui
        e | e `elem` [VtyEvent (EvKey (KChar 'g') []), AppEvent FileChange] -> do
          -- plog (if e == AppEvent FileChange then "file change" else "manual reload") "" `seq` return ()
          ej <- liftIO . runExceptT $ journalReload copts
          case ej of
            Left err -> continue $ screenEnter d errorScreen{esError=err} ui
            Right j' -> continue $ regenerateScreens j' d ui
        VtyEvent (EvKey (KChar 'I') []) -> continue $ uiCheckBalanceAssertions d (toggleIgnoreBalanceAssertions ui)

        -- for toggles that may change the current/prev/next transactions,
        -- we must regenerate the transaction list, like the g handler above ? with regenerateTransactions ? TODO WIP
        -- EvKey (KChar 'E') [] -> continue $ regenerateScreens j d $ stToggleEmpty ui
        -- EvKey (KChar 'C') [] -> continue $ regenerateScreens j d $ stToggleCleared ui
        -- EvKey (KChar 'R') [] -> continue $ regenerateScreens j d $ stToggleReal ui
        VtyEvent (EvKey (KChar 'B') []) -> continue . regenerateScreens j d $ toggleConversionOp ui
        VtyEvent (EvKey (KChar 'V') []) -> continue . regenerateScreens j d $ toggleValue ui

        VtyEvent e | e `elem` moveUpEvents   -> continue $ tsSelect iprev tprev ui
        VtyEvent e | e `elem` moveDownEvents -> continue $ tsSelect inext tnext ui

        -- exit screen on LEFT
        VtyEvent e | e `elem` moveLeftEvents -> continue . popScreen $ tsSelect i t ui  -- Probably not necessary to tsSelect here, but it's safe.
        -- or on a click in the app's left margin.
        VtyEvent (EvMouseUp x _y (Just BLeft)) | x==0 -> continue . popScreen $ tsSelect i t ui
        -- or on clicking the blank area below the transaction.
        MouseUp _ (Just BLeft) Location{loc=(_,y)} | y+1 > numentrylines -> continue . popScreen $ tsSelect i t ui
          where numentrylines = length (T.lines $ showTxn ropts rspec j t) - 1

        VtyEvent (EvKey (KChar 'l') [MCtrl]) -> redraw ui
        VtyEvent (EvKey (KChar 'z') [MCtrl]) -> suspend ui
        _ -> continue ui

tsHandle _ _ = error "event handler called with wrong screen type, should not happen"  -- PARTIAL:

-- | Select a new transaction and update the previous register screen
tsSelect i t ui@UIState{aScreen=s@TransactionScreen{}} = case aPrevScreens ui of
    x:xs -> ui'{aPrevScreens=rsSelect i x : xs}
    []   -> ui'
  where ui' = ui{aScreen=s{tsTransaction=(i,t)}}
tsSelect _ _ ui = ui

-- | Select the nth item on the register screen.
rsSelect i scr@RegisterScreen{..} = scr{rsList=listMoveTo (fromInteger $ i-1) rsList}
rsSelect _ scr = scr
