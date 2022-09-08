-- The balance sheet screen, like the accounts screen but restricted to balance sheet accounts.

{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Hledger.UI.BalancesheetScreen
 (bsNew
 ,bsUpdate
 ,bsDraw
 ,bsHandle
 ,bsSetSelectedAccount
 )
where

import Brick hiding (bsDraw)
import Brick.Widgets.List
import Brick.Widgets.Edit
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Data.List hiding (reverse)
import Data.Maybe
import qualified Data.Text as T
import Data.Time.Calendar (Day)
import qualified Data.Vector as V
import Data.Vector ((!?))
import Graphics.Vty (Event(..),Key(..),Modifier(..), Button (BLeft, BScrollDown, BScrollUp))
import Lens.Micro.Platform
import System.Console.ANSI
import System.FilePath (takeFileName)
import Text.DocLayout (realLength)

import Hledger
import Hledger.Cli hiding (mode, progname, prognameandversion)
import Hledger.UI.UIOptions
import Hledger.UI.UITypes
import Hledger.UI.UIState
import Hledger.UI.UIUtils
import Hledger.UI.UIScreens
import Hledger.UI.Editor
import Hledger.UI.ErrorScreen (uiReloadJournal, uiCheckBalanceAssertions, uiReloadJournalIfChanged)
import Hledger.UI.RegisterScreen (rsCenterSelection)


bsDraw :: UIState -> [Widget Name]
bsDraw UIState{aopts=_uopts@UIOpts{uoCliOpts=copts@CliOpts{reportspec_=rspec}}
              ,ajournal=j
              ,aScreen=BS sst
              ,aMode=mode
              } = dlogUiTrace "bsDraw 1" $
    case mode of
      Help       -> [helpDialog copts, maincontent]
      -- Minibuffer e -> [minibuffer e, maincontent]
      _          -> [maincontent]
  where
    maincontent = Widget Greedy Greedy $ do
      c <- getContext
      let
        availwidth =
          -- ltrace "availwidth" $
          c^.availWidthL
          - 2 -- XXX due to margin ? shouldn't be necessary (cf UIUtils)
        displayitems = sst ^. bssList . listElementsL

        acctwidths = V.map (\AccountsScreenItem{..} -> asItemIndentLevel + realLength asItemDisplayAccountName) displayitems
        balwidths  = V.map (maybe 0 (wbWidth . showMixedAmountB oneLine) . asItemMixedAmount) displayitems
        preferredacctwidth = V.maximum acctwidths
        totalacctwidthseen = V.sum acctwidths
        preferredbalwidth  = V.maximum balwidths
        totalbalwidthseen  = V.sum balwidths

        totalwidthseen = totalacctwidthseen + totalbalwidthseen
        shortfall = preferredacctwidth + preferredbalwidth + 2 - availwidth
        acctwidthproportion = fromIntegral totalacctwidthseen / fromIntegral totalwidthseen
        adjustedacctwidth = min preferredacctwidth . max 15 . round $ acctwidthproportion * fromIntegral (availwidth - 2)  -- leave 2 whitespace for padding
        adjustedbalwidth  = availwidth - 2 - adjustedacctwidth

        -- XXX how to minimise the balance column's jumping around as you change the depth limit ?

        colwidths | shortfall <= 0 = (preferredacctwidth, preferredbalwidth)
                  | otherwise      = (adjustedacctwidth, adjustedbalwidth)

      render $ defaultLayout toplabel bottomlabel $ renderList (bsDrawItem colwidths) True (sst ^. bssList)

      where
        ropts = (_rsReportOpts rspec){balanceaccum_=Historical}
        ishistorical = balanceaccum_ ropts == Historical

        toplabel =
              withAttr (attrName "border" <> attrName "filename") files
          <+> toggles
          <+> str (" balance sheet")
          <+> borderPeriodStr (if ishistorical then "at end of" else "in") (period_ ropts)
          <+> borderQueryStr (T.unpack . T.unwords . map textQuoteIfNeeded $ querystring_ ropts)
          <+> borderDepthStr mdepth
          <+> str (" ("++curidx++"/"++totidx++")")
          <+> (if ignore_assertions_ . balancingopts_ $ inputopts_ copts
               then withAttr (attrName "border" <> attrName "query") (str " ignoring balance assertions")
               else str "")
          where
            files = case journalFilePaths j of
                           [] -> str ""
                           f:_ -> str $ takeFileName f
                           -- [f,_:[]] -> (withAttr ("border" <> "bold") $ str $ takeFileName f) <+> str " (& 1 included file)"
                           -- f:fs  -> (withAttr ("border" <> "bold") $ str $ takeFileName f) <+> str (" (& " ++ show (length fs) ++ " included files)")
            toggles = withAttr (attrName "border" <> attrName "query") $ str $ unwords $ concat [
               [""]
              ,if empty_ ropts then [] else ["nonzero"]
              ,uiShowStatus copts $ statuses_ ropts
              ,if real_ ropts then ["real"] else []
              ]
            mdepth = depth_ ropts
            curidx = case sst ^. bssList . listSelectedL of
                       Nothing -> "-"
                       Just i -> show (i + 1)
            totidx = show $ V.length nonblanks
              where
                nonblanks = V.takeWhile (not . T.null . asItemAccountName) $ sst ^. bssList . listElementsL

        bottomlabel = case mode of
                        Minibuffer label ed -> minibuffer label ed
                        _                   -> quickhelp
          where
            quickhelp = borderKeysStr' [
               ("?", str "help")
--              ,("RIGHT", str "register")
              ,("t", renderToggle (tree_ ropts) "list" "tree")
              -- ,("t", str "tree")
              -- ,("l", str "list")
              ,("-+", str "depth")
              ,("", renderToggle (not ishistorical) "end-bals" "changes")
              ,("F", renderToggle1 (isJust . forecast_ $ inputopts_ copts) "forecast")
              --,("/", "filter")
              --,("DEL", "unfilter")
              --,("ESC", "cancel/top")
              ,("a", str "add")
--               ,("g", "reload")
              ,("q", str "quit")
              ]

bsDraw _ =  dlogUiTrace "bsDraw 2" $ errorWrongScreenType "draw function"  -- PARTIAL:

bsDrawItem :: (Int,Int) -> Bool -> AccountsScreenItem -> Widget Name
bsDrawItem (acctwidth, balwidth) selected AccountsScreenItem{..} =
  Widget Greedy Fixed $ do
    -- c <- getContext
      -- let showitem = intercalate "\n" . balanceReportItemAsText defreportopts fmt
    render $
      txt (fitText (Just acctwidth) (Just acctwidth) True True $ T.replicate (asItemIndentLevel) " " <> asItemDisplayAccountName) <+>
      txt balspace <+>
      splitAmounts balBuilder
      where
        balBuilder = maybe mempty showamt asItemMixedAmount
        showamt = showMixedAmountB oneLine{displayMinWidth=Just balwidth, displayMaxWidth=Just balwidth}
        balspace = T.replicate (2 + balwidth - wbWidth balBuilder) " "
        splitAmounts = foldr1 (<+>) . intersperse (str ", ") . map renderamt . T.splitOn ", " . wbToText
        renderamt :: T.Text -> Widget Name
        renderamt a | T.any (=='-') a = withAttr (sel $ attrName "list" <> attrName "balance" <> attrName "negative") $ txt a
                    | otherwise       = withAttr (sel $ attrName "list" <> attrName "balance" <> attrName "positive") $ txt a
        sel | selected  = (<> attrName "selected")
            | otherwise = id

bsHandle :: BrickEvent Name AppEvent -> EventM Name UIState ()
bsHandle ev = do
  ui0 <- get'
  dlogUiTraceM "bsHandle 1"
  case ui0 of
    ui1@UIState{
       aopts=UIOpts{uoCliOpts=copts}
      ,ajournal=j
      ,aMode=mode
      ,aScreen=BS sst
      } -> do

      let
        -- save the currently selected account, in case we leave this screen and lose the selection
        selacct = case listSelectedElement $ _bssList sst of
                    Just (_, AccountsScreenItem{..}) -> asItemAccountName
                    Nothing -> sst ^. bssSelectedAccount
        ui = ui1{aScreen=BS sst{_bssSelectedAccount=selacct}}
        nonblanks = V.takeWhile (not . T.null . asItemAccountName) $ listElements $ _bssList sst
        lastnonblankidx = max 0 (length nonblanks - 1)
        journalspan = journalDateSpan False j
        d = copts^.rsDay

      case mode of
        Minibuffer _ ed ->
          case ev of
            VtyEvent (EvKey KEsc   []) -> put' $ closeMinibuffer ui
            VtyEvent (EvKey KEnter []) -> put' $ regenerateScreens j d $
                case setFilter s $ closeMinibuffer ui of
                  Left bad -> showMinibuffer "Cannot compile regular expression" (Just bad) ui
                  Right ui' -> ui'
              where s = chomp $ unlines $ map strip $ getEditContents ed
            VtyEvent (EvKey (KChar 'l') [MCtrl]) -> redraw
            VtyEvent (EvKey (KChar 'z') [MCtrl]) -> suspend ui
            VtyEvent e -> do
              ed' <- nestEventM' ed $ handleEditorEvent (VtyEvent e)
              put' ui{aMode=Minibuffer "filter" ed'}
            AppEvent _  -> return ()
            MouseDown{} -> return ()
            MouseUp{}   -> return ()

        Help ->
          case ev of
            -- VtyEvent (EvKey (KChar 'q') []) -> halt
            VtyEvent (EvKey (KChar 'l') [MCtrl]) -> redraw
            VtyEvent (EvKey (KChar 'z') [MCtrl]) -> suspend ui
            _ -> helpHandle ev

        Normal ->
          case ev of
            VtyEvent (EvKey (KChar 'q') []) -> halt
            -- EvKey (KChar 'l') [MCtrl] -> do
            VtyEvent (EvKey KEsc        []) -> put' $ resetScreens d ui
            VtyEvent (EvKey (KChar c)   []) | c == '?' -> put' $ setMode Help ui
            -- XXX AppEvents currently handled only in Normal mode
            -- XXX be sure we don't leave unconsumed events piling up
            AppEvent (DateChange old _) | isStandardPeriod p && p `periodContainsDate` old ->
              put' $ regenerateScreens j d $ setReportPeriod (DayPeriod d) ui
              where
                p = reportPeriod ui
            e | e `elem` [VtyEvent (EvKey (KChar 'g') []), AppEvent FileChange] ->
              liftIO (uiReloadJournal copts d ui) >>= put'
            VtyEvent (EvKey (KChar 'I') []) -> put' $ uiCheckBalanceAssertions d (toggleIgnoreBalanceAssertions ui)
            VtyEvent (EvKey (KChar 'a') []) -> suspendAndResume $ clearScreen >> setCursorPosition 0 0 >> add copts j >> uiReloadJournalIfChanged copts d j ui
            VtyEvent (EvKey (KChar 'A') []) -> suspendAndResume $ void (runIadd (journalFilePath j)) >> uiReloadJournalIfChanged copts d j ui
            VtyEvent (EvKey (KChar 'E') []) -> suspendAndResume $ void (runEditor endPosition (journalFilePath j)) >> uiReloadJournalIfChanged copts d j ui
            VtyEvent (EvKey (KChar 'B') []) -> put' $ regenerateScreens j d $ toggleConversionOp ui
            VtyEvent (EvKey (KChar 'V') []) -> put' $ regenerateScreens j d $ toggleValue ui
            VtyEvent (EvKey (KChar '0') []) -> put' $ regenerateScreens j d $ setDepth (Just 0) ui
            VtyEvent (EvKey (KChar '1') []) -> put' $ regenerateScreens j d $ setDepth (Just 1) ui
            VtyEvent (EvKey (KChar '2') []) -> put' $ regenerateScreens j d $ setDepth (Just 2) ui
            VtyEvent (EvKey (KChar '3') []) -> put' $ regenerateScreens j d $ setDepth (Just 3) ui
            VtyEvent (EvKey (KChar '4') []) -> put' $ regenerateScreens j d $ setDepth (Just 4) ui
            VtyEvent (EvKey (KChar '5') []) -> put' $ regenerateScreens j d $ setDepth (Just 5) ui
            VtyEvent (EvKey (KChar '6') []) -> put' $ regenerateScreens j d $ setDepth (Just 6) ui
            VtyEvent (EvKey (KChar '7') []) -> put' $ regenerateScreens j d $ setDepth (Just 7) ui
            VtyEvent (EvKey (KChar '8') []) -> put' $ regenerateScreens j d $ setDepth (Just 8) ui
            VtyEvent (EvKey (KChar '9') []) -> put' $ regenerateScreens j d $ setDepth (Just 9) ui
            VtyEvent (EvKey (KChar '-') []) -> put' $ regenerateScreens j d $ decDepth ui
            VtyEvent (EvKey (KChar '_') []) -> put' $ regenerateScreens j d $ decDepth ui
            VtyEvent (EvKey (KChar c)   []) | c `elem` ['+','='] -> put' $ regenerateScreens j d $ incDepth ui
            VtyEvent (EvKey (KChar 'T') []) -> put' $ regenerateScreens j d $ setReportPeriod (DayPeriod d) ui

            -- display mode/query toggles
            -- VtyEvent (EvKey (KChar 'H') []) -> modify' (regenerateScreens j d . toggleHistorical) >> bsCenterAndContinue
            VtyEvent (EvKey (KChar 't') []) -> modify' (regenerateScreens j d . toggleTree) >> bsCenterAndContinue
            VtyEvent (EvKey (KChar c) []) | c `elem` ['z','Z'] -> modify' (regenerateScreens j d . toggleEmpty) >> bsCenterAndContinue
            VtyEvent (EvKey (KChar 'R') []) -> modify' (regenerateScreens j d . toggleReal) >> bsCenterAndContinue
            VtyEvent (EvKey (KChar 'U') []) -> modify' (regenerateScreens j d . toggleUnmarked) >> bsCenterAndContinue
            VtyEvent (EvKey (KChar 'P') []) -> modify' (regenerateScreens j d . togglePending) >> bsCenterAndContinue
            VtyEvent (EvKey (KChar 'C') []) -> modify' (regenerateScreens j d . toggleCleared) >> bsCenterAndContinue
            VtyEvent (EvKey (KChar 'F') []) -> modify' (regenerateScreens j d . toggleForecast d)

            VtyEvent (EvKey (KDown)     [MShift]) -> put' $ regenerateScreens j d $ shrinkReportPeriod d ui
            VtyEvent (EvKey (KUp)       [MShift]) -> put' $ regenerateScreens j d $ growReportPeriod d ui
            VtyEvent (EvKey (KRight)    [MShift]) -> put' $ regenerateScreens j d $ nextReportPeriod journalspan ui
            VtyEvent (EvKey (KLeft)     [MShift]) -> put' $ regenerateScreens j d $ previousReportPeriod journalspan ui
            VtyEvent (EvKey (KChar '/') []) -> put' $ regenerateScreens j d $ showMinibuffer "filter" Nothing ui
            VtyEvent (EvKey k           []) | k `elem` [KBS, KDel] -> (put' $ regenerateScreens j d $ resetFilter ui)
            VtyEvent e | e `elem` moveLeftEvents -> put' $ popScreen ui
            VtyEvent (EvKey (KChar 'l') [MCtrl]) -> scrollSelectionToMiddle (_bssList sst) >> redraw
            VtyEvent (EvKey (KChar 'z') [MCtrl]) -> suspend ui

            -- exit screen on LEFT
            VtyEvent e | e `elem` moveLeftEvents -> put' $ popScreen ui
            -- or on a click in the app's left margin. This is a VtyEvent since not in a clickable widget.
            VtyEvent (EvMouseUp x _y (Just BLeft)) | x==0 -> put' $ popScreen ui
            -- or on clicking a blank list item.
            MouseUp _ (Just BLeft) Location{loc=(_,y)} | clickedacct == "" -> put' $ popScreen ui
              where clickedacct = maybe "" asItemAccountName $ listElements (_bssList sst) !? y

            -- enter register screen for selected account (if there is one),
            -- centering its selected transaction if possible
            -- XXX should propagate ropts{balanceaccum_=Historical}
            VtyEvent e | e `elem` moveRightEvents
                      , not $ isBlankElement $ listSelectedElement (_bssList sst) -> bsEnterRegisterScreen d selacct ui

            -- MouseDown is sometimes duplicated, https://github.com/jtdaugherty/brick/issues/347
            -- just use it to move the selection
            MouseDown _n BLeft _mods Location{loc=(_x,y)} | not $ (=="") clickedacct -> do
              put' ui{aScreen=BS sst}  -- XXX does this do anything ?
              where clickedacct = maybe "" asItemAccountName $ listElements (_bssList sst) !? y
            -- and on MouseUp, enter the subscreen
            MouseUp _n (Just BLeft) Location{loc=(_x,y)} | not $ (=="") clickedacct -> do
              bsEnterRegisterScreen d clickedacct ui
              where clickedacct = maybe "" asItemAccountName $ listElements (_bssList sst) !? y

            -- when selection is at the last item, DOWN scrolls instead of moving, until maximally scrolled
            VtyEvent e | e `elem` moveDownEvents, isBlankElement mnextelement -> do
              vScrollBy (viewportScroll $ (_bssList sst)^.listNameL) 1
              where mnextelement = listSelectedElement $ listMoveDown (_bssList sst)

            -- mouse scroll wheel scrolls the viewport up or down to its maximum extent,
            -- pushing the selection when necessary.
            MouseDown name btn _mods _loc | btn `elem` [BScrollUp, BScrollDown] -> do
              let scrollamt = if btn==BScrollUp then -1 else 1
              list' <- nestEventM' (_bssList sst) $ listScrollPushingSelection name (bsListSize (_bssList sst)) scrollamt
              put' ui{aScreen=BS sst{_bssList=list'}}

            -- if page down or end leads to a blank padding item, stop at last non-blank
            VtyEvent e@(EvKey k           []) | k `elem` [KPageDown, KEnd] -> do
              l <- nestEventM' (_bssList sst) $ handleListEvent e
              if isBlankElement $ listSelectedElement l
              then do
                let l' = listMoveTo lastnonblankidx l
                scrollSelectionToMiddle l'
                put' ui{aScreen=BS sst{_bssList=l'}}
              else
                put' ui{aScreen=BS sst{_bssList=l}}

            -- fall through to the list's event handler (handles up/down)
            VtyEvent e -> do
              list' <- nestEventM' (_bssList sst) $ handleListEvent (normaliseMovementKeys e)
              put' ui{aScreen=BS $ sst & bssList .~ list' & bssSelectedAccount .~ selacct }

            MouseDown{} -> return ()
            MouseUp{}   -> return ()
            AppEvent _  -> return ()

    _ -> dlogUiTraceM "bsHandle 2" >> errorWrongScreenType "event handler"

bsEnterRegisterScreen :: Day -> AccountName -> UIState -> EventM Name UIState ()
bsEnterRegisterScreen d acct ui@UIState{ajournal=j, aopts=uopts} = do
  dlogUiTraceM "bsEnterRegisterScreen"
  let
    regscr = rsNew uopts d j acct isdepthclipped
      where
        isdepthclipped = case getDepth ui of
                          Just de -> accountNameLevel acct >= de
                          Nothing -> False
    ui1 = pushScreen regscr ui
  rsCenterSelection ui1 >>= put'

-- | Set the selected account on an accounts screen. No effect on other screens.
bsSetSelectedAccount :: AccountName -> Screen -> Screen
bsSetSelectedAccount a (BS bss@BSS{}) = BS bss{_bssSelectedAccount=a}
bsSetSelectedAccount _ s = s

isBlankElement mel = ((asItemAccountName . snd) <$> mel) == Just ""

-- | Scroll the accounts screen's selection to the center. No effect if on another screen.
bsCenterAndContinue :: EventM Name UIState ()
bsCenterAndContinue = do
  ui <- get'
  case aScreen ui of
    BS sst -> scrollSelectionToMiddle $ _bssList sst
    _ -> return ()

bsListSize = V.length . V.takeWhile ((/="").asItemAccountName) . listElements


