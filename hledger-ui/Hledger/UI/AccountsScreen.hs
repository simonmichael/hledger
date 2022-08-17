-- The accounts screen, showing accounts and balances like the CLI balance command.

{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Hledger.UI.AccountsScreen
 (accountsScreen
 ,asInit
 ,asSetSelectedAccount
 )
where

import Brick
import Brick.Widgets.List
import Brick.Widgets.Edit
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Data.List hiding (reverse)
import Data.Maybe
import qualified Data.Text as T
import Data.Time.Calendar (Day)
import qualified Data.Vector as V
import Graphics.Vty (Event(..),Key(..),Modifier(..), Button (BLeft, BScrollDown, BScrollUp))
import Lens.Micro.Platform
import Safe
import System.Console.ANSI
import System.FilePath (takeFileName)
import Text.DocLayout (realLength)

import Hledger
import Hledger.Cli hiding (progname,prognameandversion)
import Hledger.UI.UIOptions
import Hledger.UI.UITypes
import Hledger.UI.UIState
import Hledger.UI.UIUtils
import Hledger.UI.Editor
import Hledger.UI.RegisterScreen
import Hledger.UI.ErrorScreen
import Data.Vector ((!?))


accountsScreen :: Screen
accountsScreen = AccountsScreen{
   sInit   = asInit
  ,sDraw   = asDraw
  ,sHandle = asHandle
  ,_asList            = list AccountsList V.empty 1
  ,_asSelectedAccount = ""
  }

asInit :: Day -> Bool -> UIState -> UIState
asInit d reset ui@UIState{
  aopts=UIOpts{uoCliOpts=copts@CliOpts{reportspec_=rspec@ReportSpec{_rsReportOpts=ropts}}},
  ajournal=j,
  aScreen=s@AccountsScreen{}
  } =
  ui{aScreen=s & asList .~ newitems'}
   where
    newitems = list AccountsList (V.fromList $ displayitems ++ blankitems) 1

    -- decide which account is selected:
    -- if reset is true, the first account;
    -- otherwise, the previously selected account if possible;
    -- otherwise, the first account with the same prefix (eg first leaf account when entering flat mode);
    -- otherwise, the alphabetically preceding account.
    newitems' = listMoveTo selidx newitems
      where
        selidx = case (reset, listSelectedElement $ _asList s) of
                   (True, _)               -> 0
                   (_, Nothing)            -> 0
                   (_, Just (_,AccountsScreenItem{asItemAccountName=a})) ->
                     headDef 0 $ catMaybes [
                       elemIndex a as
                      ,findIndex (a `isAccountNamePrefixOf`) as
                      ,Just $ max 0 (length (filter (< a) as) - 1)
                      ]
                      where
                        as = map asItemAccountName displayitems

    rspec' =
      -- Further restrict the query based on the current period and future/forecast mode.
      (reportSpecSetFutureAndForecast d (forecast_ $ inputopts_ copts) rspec)
      -- always show declared accounts even if unused
        {_rsReportOpts=ropts{declared_=True}}

    -- run the report
    (items,_total) = balanceReport rspec' j

    -- pre-render the list items
    displayitem (fullacct, shortacct, indent, bal) =
      AccountsScreenItem{asItemIndentLevel        = indent
                        ,asItemAccountName        = fullacct
                        ,asItemDisplayAccountName = replaceHiddenAccountsNameWith "All" $ if tree_ ropts then shortacct else fullacct
                        ,asItemMixedAmount        = Just bal
                        }
    displayitems = map displayitem items
    -- blanks added for scrolling control, cf RegisterScreen.
    -- XXX Ugly. Changing to 0 helps when debugging.
    blankitems = replicate 100
      AccountsScreenItem{asItemIndentLevel        = 0
                        ,asItemAccountName        = ""
                        ,asItemDisplayAccountName = ""
                        ,asItemMixedAmount        = Nothing
                        }


asInit _ _ _ = error "init function called with wrong screen type, should not happen"  -- PARTIAL:

asDraw :: UIState -> [Widget Name]
asDraw UIState{aopts=_uopts@UIOpts{uoCliOpts=copts@CliOpts{reportspec_=rspec}}
              ,ajournal=j
              ,aScreen=s@AccountsScreen{}
              ,aMode=mode
              } =
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
        displayitems = s ^. asList . listElementsL

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

      render $ defaultLayout toplabel bottomlabel $ renderList (asDrawItem colwidths) True (_asList s)

      where
        ropts = _rsReportOpts rspec
        ishistorical = balanceaccum_ ropts == Historical

        toplabel =
              withAttr (attrName "border" <> attrName "filename") files
          <+> toggles
          <+> str (" account " ++ if ishistorical then "balances" else "changes")
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
            curidx = case _asList s ^. listSelectedL of
                       Nothing -> "-"
                       Just i -> show (i + 1)
            totidx = show $ V.length nonblanks
              where
                nonblanks = V.takeWhile (not . T.null . asItemAccountName) $ s ^. asList . listElementsL

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
              ,("H", renderToggle (not ishistorical) "end-bals" "changes")
              ,("F", renderToggle1 (isJust . forecast_ $ inputopts_ copts) "forecast")
              --,("/", "filter")
              --,("DEL", "unfilter")
              --,("ESC", "cancel/top")
              ,("a", str "add")
--               ,("g", "reload")
              ,("q", str "quit")
              ]

asDraw _ = error "draw function called with wrong screen type, should not happen"  -- PARTIAL:

asDrawItem :: (Int,Int) -> Bool -> AccountsScreenItem -> Widget Name
asDrawItem (acctwidth, balwidth) selected AccountsScreenItem{..} =
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

asHandle :: BrickEvent Name AppEvent -> EventM Name UIState ()
asHandle ev = do
  ui0 <- get
  case ui0 of
    ui1@UIState{
      aScreen=scr@AccountsScreen{..}
      ,aopts=UIOpts{uoCliOpts=copts}
      ,ajournal=j
      ,aMode=mode
      } -> do

      let
        -- save the currently selected account, in case we leave this screen and lose the selection
        selacct = case listSelectedElement _asList of
                    Just (_, AccountsScreenItem{..}) -> asItemAccountName
                    Nothing -> scr ^. asSelectedAccount
        ui = ui1{aScreen=scr & asSelectedAccount .~ selacct}
        nonblanks = V.takeWhile (not . T.null . asItemAccountName) $ _asList^.listElementsL
        lastnonblankidx = max 0 (length nonblanks - 1)
        journalspan = journalDateSpan False j
        d = copts^.rsDay

      case mode of
        Minibuffer _ ed ->
          case ev of
            VtyEvent (EvKey KEsc   []) -> put $ closeMinibuffer ui
            VtyEvent (EvKey KEnter []) -> put $ regenerateScreens j d $
                case setFilter s $ closeMinibuffer ui of
                  Left bad -> showMinibuffer "Cannot compile regular expression" (Just bad) ui
                  Right ui' -> ui'
              where s = chomp $ unlines $ map strip $ getEditContents ed
            VtyEvent (EvKey (KChar 'l') [MCtrl]) -> redraw
            VtyEvent (EvKey (KChar 'z') [MCtrl]) -> suspend ui
            VtyEvent ev -> do
              ed' <- nestEventM' ed $ handleEditorEvent (VtyEvent ev)
              put ui{aMode=Minibuffer "filter" ed'}
            AppEvent _  -> return ()
            MouseDown{} -> return ()
            MouseUp{}   -> return ()

        Help ->
          case ev of
            -- VtyEvent (EvKey (KChar 'q') []) -> halt
            VtyEvent (EvKey (KChar 'l') [MCtrl]) -> redraw
            VtyEvent (EvKey (KChar 'z') [MCtrl]) -> suspend ui
            _                    -> helpHandle ev

        Normal ->
          case ev of
            VtyEvent (EvKey (KChar 'q') []) -> halt
            -- EvKey (KChar 'l') [MCtrl] -> do
            VtyEvent (EvKey KEsc        []) -> put $ resetScreens d ui
            VtyEvent (EvKey (KChar c)   []) | c == '?' -> put $ setMode Help ui
            -- XXX AppEvents currently handled only in Normal mode
            -- XXX be sure we don't leave unconsumed events piling up
            AppEvent (DateChange old _) | isStandardPeriod p && p `periodContainsDate` old ->
              put $ regenerateScreens j d $ setReportPeriod (DayPeriod d) ui
              where
                p = reportPeriod ui
            e | e `elem` [VtyEvent (EvKey (KChar 'g') []), AppEvent FileChange] ->
              liftIO (uiReloadJournal copts d ui) >>= put
            VtyEvent (EvKey (KChar 'I') []) -> put $ uiCheckBalanceAssertions d (toggleIgnoreBalanceAssertions ui)
            VtyEvent (EvKey (KChar 'a') []) -> suspendAndResume $ clearScreen >> setCursorPosition 0 0 >> add copts j >> uiReloadJournalIfChanged copts d j ui
            VtyEvent (EvKey (KChar 'A') []) -> suspendAndResume $ void (runIadd (journalFilePath j)) >> uiReloadJournalIfChanged copts d j ui
            VtyEvent (EvKey (KChar 'E') []) -> suspendAndResume $ void (runEditor endPosition (journalFilePath j)) >> uiReloadJournalIfChanged copts d j ui
            VtyEvent (EvKey (KChar 'B') []) -> put $ regenerateScreens j d $ toggleConversionOp ui
            VtyEvent (EvKey (KChar 'V') []) -> put $ regenerateScreens j d $ toggleValue ui
            VtyEvent (EvKey (KChar '0') []) -> put $ regenerateScreens j d $ setDepth (Just 0) ui
            VtyEvent (EvKey (KChar '1') []) -> put $ regenerateScreens j d $ setDepth (Just 1) ui
            VtyEvent (EvKey (KChar '2') []) -> put $ regenerateScreens j d $ setDepth (Just 2) ui
            VtyEvent (EvKey (KChar '3') []) -> put $ regenerateScreens j d $ setDepth (Just 3) ui
            VtyEvent (EvKey (KChar '4') []) -> put $ regenerateScreens j d $ setDepth (Just 4) ui
            VtyEvent (EvKey (KChar '5') []) -> put $ regenerateScreens j d $ setDepth (Just 5) ui
            VtyEvent (EvKey (KChar '6') []) -> put $ regenerateScreens j d $ setDepth (Just 6) ui
            VtyEvent (EvKey (KChar '7') []) -> put $ regenerateScreens j d $ setDepth (Just 7) ui
            VtyEvent (EvKey (KChar '8') []) -> put $ regenerateScreens j d $ setDepth (Just 8) ui
            VtyEvent (EvKey (KChar '9') []) -> put $ regenerateScreens j d $ setDepth (Just 9) ui
            VtyEvent (EvKey (KChar '-') []) -> put $ regenerateScreens j d $ decDepth ui
            VtyEvent (EvKey (KChar '_') []) -> put $ regenerateScreens j d $ decDepth ui
            VtyEvent (EvKey (KChar c)   []) | c `elem` ['+','='] -> put $ regenerateScreens j d $ incDepth ui
            VtyEvent (EvKey (KChar 'T') []) -> put $ regenerateScreens j d $ setReportPeriod (DayPeriod d) ui

            -- display mode/query toggles
            VtyEvent (EvKey (KChar 'H') []) -> modify (regenerateScreens j d . toggleHistorical) >> asCenterAndContinue
            VtyEvent (EvKey (KChar 't') []) -> modify (regenerateScreens j d . toggleTree) >> asCenterAndContinue
            VtyEvent (EvKey (KChar c) []) | c `elem` ['z','Z'] -> modify (regenerateScreens j d . toggleEmpty) >> asCenterAndContinue
            VtyEvent (EvKey (KChar 'R') []) -> modify (regenerateScreens j d . toggleReal) >> asCenterAndContinue
            VtyEvent (EvKey (KChar 'U') []) -> modify (regenerateScreens j d . toggleUnmarked) >> asCenterAndContinue
            VtyEvent (EvKey (KChar 'P') []) -> modify (regenerateScreens j d . togglePending) >> asCenterAndContinue
            VtyEvent (EvKey (KChar 'C') []) -> modify (regenerateScreens j d . toggleCleared) >> asCenterAndContinue
            VtyEvent (EvKey (KChar 'F') []) -> modify (regenerateScreens j d . toggleForecast d)

            VtyEvent (EvKey (KDown)     [MShift]) -> put $ regenerateScreens j d $ shrinkReportPeriod d ui
            VtyEvent (EvKey (KUp)       [MShift]) -> put $ regenerateScreens j d $ growReportPeriod d ui
            VtyEvent (EvKey (KRight)    [MShift]) -> put $ regenerateScreens j d $ nextReportPeriod journalspan ui
            VtyEvent (EvKey (KLeft)     [MShift]) -> put $ regenerateScreens j d $ previousReportPeriod journalspan ui
            VtyEvent (EvKey (KChar '/') []) -> put $ regenerateScreens j d $ showMinibuffer "filter" Nothing ui
            VtyEvent (EvKey k           []) | k `elem` [KBS, KDel] -> (put $ regenerateScreens j d $ resetFilter ui)
            VtyEvent e | e `elem` moveLeftEvents -> put $ popScreen ui
            VtyEvent (EvKey (KChar 'l') [MCtrl]) -> scrollSelectionToMiddle _asList >> redraw
            VtyEvent (EvKey (KChar 'z') [MCtrl]) -> suspend ui

            -- enter register screen for selected account (if there is one),
            -- centering its selected transaction if possible
            VtyEvent e | e `elem` moveRightEvents
                      , not $ isBlankElement $ listSelectedElement _asList -> asEnterRegister d selacct ui

            -- MouseDown is sometimes duplicated, https://github.com/jtdaugherty/brick/issues/347
            -- just use it to move the selection
            MouseDown _n BLeft _mods Location{loc=(_x,y)} | not $ (=="") clickedacct -> do
              put ui{aScreen=scr}  -- XXX does this do anything ?
              where clickedacct = maybe "" asItemAccountName $ listElements _asList !? y
            -- and on MouseUp, enter the subscreen
            MouseUp _n (Just BLeft) Location{loc=(_x,y)} | not $ (=="") clickedacct -> do
              asEnterRegister d clickedacct ui
              where clickedacct = maybe "" asItemAccountName $ listElements _asList !? y

            -- when selection is at the last item, DOWN scrolls instead of moving, until maximally scrolled
            VtyEvent e | e `elem` moveDownEvents, isBlankElement mnextelement -> do
              vScrollBy (viewportScroll $ _asList^.listNameL) 1
              where mnextelement = listSelectedElement $ listMoveDown _asList

            -- mouse scroll wheel scrolls the viewport up or down to its maximum extent,
            -- pushing the selection when necessary.
            MouseDown name btn _mods _loc | btn `elem` [BScrollUp, BScrollDown] -> do
              let scrollamt = if btn==BScrollUp then -1 else 1
              list' <- nestEventM' _asList $ listScrollPushingSelection name (asListSize _asList) scrollamt
              put ui{aScreen=scr{_asList=list'}}

            -- if page down or end leads to a blank padding item, stop at last non-blank
            VtyEvent e@(EvKey k           []) | k `elem` [KPageDown, KEnd] -> do
              list <- nestEventM' _asList $ handleListEvent e
              if isBlankElement $ listSelectedElement list
              then do
                let list' = listMoveTo lastnonblankidx list
                scrollSelectionToMiddle list'
                put ui{aScreen=scr{_asList=list'}}
              else
                put ui{aScreen=scr{_asList=list}}

            -- fall through to the list's event handler (handles up/down)
            VtyEvent ev -> do
              list' <- nestEventM' _asList $ handleListEvent (normaliseMovementKeys ev)
              put $ ui{aScreen=scr & asList .~ list' & asSelectedAccount .~ selacct }

            MouseDown{} -> put ui
            MouseUp{}   -> put ui
            AppEvent _  -> put ui

    _ -> errorWrongScreenType

asEnterRegister d selacct ui = do
  rsCenterAndContinue $
  -- flip rsHandle (VtyEvent (EvKey (KChar 'l') [MCtrl])) $
    screenEnter d regscr ui
      where
        regscr = rsSetAccount selacct isdepthclipped registerScreen
        isdepthclipped = case getDepth ui of
                            Just d  -> accountNameLevel selacct >= d
                            Nothing -> False

asSetSelectedAccount a s@AccountsScreen{} = s & asSelectedAccount .~ a
asSetSelectedAccount _ s = s

isBlankElement mel = ((asItemAccountName . snd) <$> mel) == Just ""

asCenterAndContinue :: EventM Name UIState ()
asCenterAndContinue = do
  ui <- get
  scrollSelectionToMiddle (_asList $ aScreen ui)

asListSize = V.length . V.takeWhile ((/="").asItemAccountName) . listElements
