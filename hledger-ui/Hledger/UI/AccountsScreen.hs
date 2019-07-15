-- The accounts screen, showing accounts and balances like the CLI balance command.

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE CPP #-}

module Hledger.UI.AccountsScreen
 (accountsScreen
 ,asInit
 ,asSetSelectedAccount
 )
where

import Brick
import Brick.Widgets.List
  (handleListEvent, list, listElementsL, listMoveDown, listMoveTo, listNameL, listSelectedElement, listSelectedL, renderList)
import Brick.Widgets.Edit
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Data.List
import Data.Maybe
#if !(MIN_VERSION_base(4,11,0))
import Data.Monoid ((<>))
#endif
import qualified Data.Text as T
import Data.Time.Calendar (Day, addDays)
import qualified Data.Vector as V
import Graphics.Vty (Event(..),Key(..),Modifier(..))
import Lens.Micro.Platform
import Safe
import System.Console.ANSI
import System.FilePath (takeFileName)

import Hledger
import Hledger.Cli hiding (progname,prognameandversion)
import Hledger.UI.UIOptions
import Hledger.UI.UITypes
import Hledger.UI.UIState
import Hledger.UI.UIUtils
import Hledger.UI.Editor
import Hledger.UI.RegisterScreen
import Hledger.UI.ErrorScreen


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
  aopts=uopts@UIOpts{cliopts_=copts@CliOpts{reportopts_=ropts}},
  ajournal=j,
  aScreen=s@AccountsScreen{}
  } =
  ui{aopts=uopts', aScreen=s & asList .~ newitems'}
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
                       findIndex (a ==) as
                      ,findIndex (a `isAccountNamePrefixOf`) as
                      ,Just $ max 0 (length (filter (< a) as) - 1)
                      ]
                      where
                        as = map asItemAccountName displayitems

    uopts' = uopts{cliopts_=copts{reportopts_=ropts'}}
    ropts' = ropts{accountlistmode_=if tree_ ropts then ALTree else ALFlat}

    -- Add a date:-tomorrow to the query to exclude future txns, by default.
    -- XXX this necessitates special handling in multiBalanceReport, at least
    pfq | presentorfuture_ uopts == PFFuture = Any
        | otherwise                          = Date $ DateSpan Nothing (Just $ addDays 1 d)
    q = And [queryFromOpts d ropts, pfq]


    -- run the report
    (items,_total) = report ropts' q j
      where
                 -- XXX in historical mode, --forecast throws off the starting balances
        report | balancetype_ ropts == HistoricalBalance = balanceReportFromMultiBalanceReport
               | otherwise                               = balanceReport
                    -- still using the old balanceReport for change reports as it
                    -- does not include every account from before the report period


    -- pre-render the list items
    displayitem (fullacct, shortacct, indent, bal) =
      AccountsScreenItem{asItemIndentLevel        = indent
                        ,asItemAccountName        = fullacct
                        ,asItemDisplayAccountName = replaceHiddenAccountsNameWith "All" $ if tree_ ropts' then shortacct else fullacct
                        ,asItemRenderedAmounts    = map showAmountWithoutPrice amts -- like showMixedAmountOneLineWithoutPrice
                        }
      where
        Mixed amts = normaliseMixedAmountSquashPricesForDisplay $ stripPrices bal
        stripPrices (Mixed as) = Mixed $ map stripprice as where stripprice a = a{aprice=Nothing}
    displayitems = map displayitem items
    -- blanks added for scrolling control, cf RegisterScreen
    blankitems = replicate 100
      AccountsScreenItem{asItemIndentLevel        = 0
                        ,asItemAccountName        = ""
                        ,asItemDisplayAccountName = ""
                        ,asItemRenderedAmounts    = []
                        }


asInit _ _ _ = error "init function called with wrong screen type, should not happen"

asDraw :: UIState -> [Widget Name]
asDraw UIState{aopts=uopts@UIOpts{cliopts_=copts@CliOpts{reportopts_=ropts}}
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
        maxacctwidthseen =
          -- ltrace "maxacctwidthseen" $
          V.maximum $
          V.map (\AccountsScreenItem{..} -> asItemIndentLevel + Hledger.Cli.textWidth asItemDisplayAccountName) $
          -- V.filter (\(indent,_,_,_) -> (indent-1) <= fromMaybe 99999 mdepth) $
          displayitems
        maxbalwidthseen =
          -- ltrace "maxbalwidthseen" $
          V.maximum $ V.map (\AccountsScreenItem{..} -> sum (map strWidth asItemRenderedAmounts) + 2 * (length asItemRenderedAmounts - 1)) displayitems
        maxbalwidth =
          -- ltrace "maxbalwidth" $
          max 0 (availwidth - 2 - 4) -- leave 2 whitespace plus least 4 for accts
        balwidth =
          -- ltrace "balwidth" $
          min maxbalwidth maxbalwidthseen
        maxacctwidth =
          -- ltrace "maxacctwidth" $
          availwidth - 2 - balwidth
        acctwidth =
          -- ltrace "acctwidth" $
          min maxacctwidth maxacctwidthseen

        -- XXX how to minimise the balance column's jumping around
        -- as you change the depth limit ?

        colwidths = (acctwidth, balwidth)

      render $ defaultLayout toplabel bottomlabel $ renderList (asDrawItem colwidths) True (_asList s)

      where
        ishistorical = balancetype_ ropts == HistoricalBalance

        toplabel =
              withAttr ("border" <> "filename") files
          <+> toggles
          <+> str (" account " ++ if ishistorical then "balances" else "changes")
          <+> borderPeriodStr (if ishistorical then "at end of" else "in") (period_ ropts)
          <+> borderQueryStr querystr
          <+> borderDepthStr mdepth
          <+> str (" ("++curidx++"/"++totidx++")")
          <+> (if ignore_assertions_ $ inputopts_ copts
               then withAttr ("border" <> "query") (str " ignoring balance assertions")
               else str "")
          where
            files = case journalFilePaths j of
                           [] -> str ""
                           f:_ -> str $ takeFileName f
                           -- [f,_:[]] -> (withAttr ("border" <> "bold") $ str $ takeFileName f) <+> str " (& 1 included file)"
                           -- f:fs  -> (withAttr ("border" <> "bold") $ str $ takeFileName f) <+> str (" (& " ++ show (length fs) ++ " included files)")
            toggles = withAttr ("border" <> "query") $ str $ unwords $ concat [
               [""]
              ,if empty_ ropts then [] else ["nonzero"]
              ,uiShowStatus copts $ statuses_ ropts
              ,if real_ ropts then ["real"] else []
              ]
            querystr = query_ ropts
            mdepth = depth_ ropts
            curidx = case _asList s ^. listSelectedL of
                       Nothing -> "-"
                       Just i -> show (i + 1)
            totidx = show $ V.length nonblanks
              where
                nonblanks = V.takeWhile (not . T.null . asItemAccountName) $ s ^. asList . listElementsL

        bottomlabel = case mode of
                        Minibuffer ed -> minibuffer ed
                        _             -> quickhelp
          where
            quickhelp = borderKeysStr' [
               ("?", str "help")
--              ,("RIGHT", str "register")
              ,("-+", str "depth")
              ,("T", renderToggle (tree_ ropts) "flat" "tree")
              ,("H", renderToggle (not ishistorical) "end-bals" "changes")
              ,("F", renderToggle (presentorfuture_ uopts == PFFuture) "present" "future")
              --,("/", "filter")
              --,("DEL", "unfilter")
              --,("ESC", "cancel/top")
              ,("a", str "add")
--               ,("g", "reload")
              ,("q", str "quit")
              ]

asDraw _ = error "draw function called with wrong screen type, should not happen"

asDrawItem :: (Int,Int) -> Bool -> AccountsScreenItem -> Widget Name
asDrawItem (acctwidth, balwidth) selected AccountsScreenItem{..} =
  Widget Greedy Fixed $ do
    -- c <- getContext
      -- let showitem = intercalate "\n" . balanceReportItemAsText defreportopts fmt
    render $
      addamts asItemRenderedAmounts $
      str (T.unpack $ fitText (Just acctwidth) (Just acctwidth) True True $ T.replicate (asItemIndentLevel) " " <> asItemDisplayAccountName) <+>
      str "  " <+>
      str (balspace asItemRenderedAmounts)
      where
        balspace as = replicate n ' '
          where n = max 0 (balwidth - (sum (map strWidth as) + 2 * (length as - 1)))
        addamts :: [String] -> Widget Name -> Widget Name
        addamts [] w = w
        addamts [a] w = (<+> renderamt a) w
        -- foldl' :: (b -> a -> b) -> b -> t a -> b
        -- foldl' (Widget -> String -> Widget) -> Widget -> [String] -> Widget
        addamts (a:as) w = foldl' addamt (addamts [a] w) as
        addamt :: Widget Name -> String -> Widget Name
        addamt w a = ((<+> renderamt a) . (<+> str ", ")) w
        renderamt :: String -> Widget Name
        renderamt a | '-' `elem` a = withAttr (sel $ "list" <> "balance" <> "negative") $ str a
                    | otherwise    = withAttr (sel $ "list" <> "balance" <> "positive") $ str a
        sel | selected  = (<> "selected")
            | otherwise = id

asHandle :: UIState -> BrickEvent Name AppEvent -> EventM Name (Next UIState)
asHandle ui0@UIState{
   aScreen=scr@AccountsScreen{..}
  ,aopts=UIOpts{cliopts_=copts}
  ,ajournal=j
  ,aMode=mode
  } ev = do
  d <- liftIO getCurrentDay
  let
    nonblanks = V.takeWhile (not . T.null . asItemAccountName) $ _asList^.listElementsL
    lastnonblankidx = max 0 (length nonblanks - 1)

  -- save the currently selected account, in case we leave this screen and lose the selection
  let
    selacct = case listSelectedElement _asList of
                Just (_, AccountsScreenItem{..}) -> asItemAccountName
                Nothing -> scr ^. asSelectedAccount
    ui = ui0{aScreen=scr & asSelectedAccount .~ selacct}

  case mode of
    Minibuffer ed ->
      case ev of
        VtyEvent (EvKey KEsc   []) -> continue $ closeMinibuffer ui
        VtyEvent (EvKey KEnter []) -> continue $ regenerateScreens j d $ setFilter s $ closeMinibuffer ui
                            where s = chomp $ unlines $ map strip $ getEditContents ed
        VtyEvent (EvKey (KChar 'l') [MCtrl]) -> redraw ui
        VtyEvent (EvKey (KChar 'z') [MCtrl]) -> suspend ui
        VtyEvent ev        -> do ed' <- handleEditorEvent ev ed
                                 continue $ ui{aMode=Minibuffer ed'}
        AppEvent _        -> continue ui
        MouseDown _ _ _ _ -> continue ui
        MouseUp _ _ _     -> continue ui

    Help ->
      case ev of
        VtyEvent (EvKey (KChar 'q') []) -> halt ui
        VtyEvent (EvKey (KChar 'l') [MCtrl]) -> redraw ui
        VtyEvent (EvKey (KChar 'z') [MCtrl]) -> suspend ui
        _                    -> helpHandle ui ev

    Normal ->
      case ev of
        VtyEvent (EvKey (KChar 'q') []) -> halt ui
        -- EvKey (KChar 'l') [MCtrl] -> do
        VtyEvent (EvKey KEsc        []) -> continue $ resetScreens d ui
        VtyEvent (EvKey (KChar c)   []) | c `elem` ['?'] -> continue $ setMode Help ui
        -- XXX AppEvents currently handled only in Normal mode
        -- XXX be sure we don't leave unconsumed events piling up
        AppEvent (DateChange old _) | isStandardPeriod p && p `periodContainsDate` old ->
          continue $ regenerateScreens j d $ setReportPeriod (DayPeriod d) ui
          where
            p = reportPeriod ui
        e | e `elem` [VtyEvent (EvKey (KChar 'g') []), AppEvent FileChange] ->
          liftIO (uiReloadJournal copts d ui) >>= continue
        VtyEvent (EvKey (KChar 'I') []) -> continue $ uiCheckBalanceAssertions d (toggleIgnoreBalanceAssertions ui)
        VtyEvent (EvKey (KChar 'a') []) -> suspendAndResume $ clearScreen >> setCursorPosition 0 0 >> add copts j >> uiReloadJournalIfChanged copts d j ui
        VtyEvent (EvKey (KChar 'A') []) -> suspendAndResume $ void (runIadd (journalFilePath j)) >> uiReloadJournalIfChanged copts d j ui
        VtyEvent (EvKey (KChar 'E') []) -> suspendAndResume $ void (runEditor endPos (journalFilePath j)) >> uiReloadJournalIfChanged copts d j ui
        VtyEvent (EvKey (KChar '0') []) -> continue $ regenerateScreens j d $ setDepth (Just 0) ui
        VtyEvent (EvKey (KChar '1') []) -> continue $ regenerateScreens j d $ setDepth (Just 1) ui
        VtyEvent (EvKey (KChar '2') []) -> continue $ regenerateScreens j d $ setDepth (Just 2) ui
        VtyEvent (EvKey (KChar '3') []) -> continue $ regenerateScreens j d $ setDepth (Just 3) ui
        VtyEvent (EvKey (KChar '4') []) -> continue $ regenerateScreens j d $ setDepth (Just 4) ui
        VtyEvent (EvKey (KChar '5') []) -> continue $ regenerateScreens j d $ setDepth (Just 5) ui
        VtyEvent (EvKey (KChar '6') []) -> continue $ regenerateScreens j d $ setDepth (Just 6) ui
        VtyEvent (EvKey (KChar '7') []) -> continue $ regenerateScreens j d $ setDepth (Just 7) ui
        VtyEvent (EvKey (KChar '8') []) -> continue $ regenerateScreens j d $ setDepth (Just 8) ui
        VtyEvent (EvKey (KChar '9') []) -> continue $ regenerateScreens j d $ setDepth (Just 9) ui
        VtyEvent (EvKey (KChar '-') []) -> continue $ regenerateScreens j d $ decDepth ui
        VtyEvent (EvKey (KChar '_') []) -> continue $ regenerateScreens j d $ decDepth ui
        VtyEvent (EvKey (KChar c)   []) | c `elem` ['+','='] -> continue $ regenerateScreens j d $ incDepth ui
        VtyEvent (EvKey (KChar 't') [])    -> continue $ regenerateScreens j d $ setReportPeriod (DayPeriod d) ui

        -- display mode/query toggles
        VtyEvent (EvKey (KChar 'H') []) -> asCenterAndContinue $ regenerateScreens j d $ toggleHistorical ui
        VtyEvent (EvKey (KChar 'T') []) -> asCenterAndContinue $ regenerateScreens j d $ toggleTree ui
        VtyEvent (EvKey (KChar 'Z') []) -> asCenterAndContinue $ regenerateScreens j d $ toggleEmpty ui
        VtyEvent (EvKey (KChar 'R') []) -> asCenterAndContinue $ regenerateScreens j d $ toggleReal ui
        VtyEvent (EvKey (KChar 'U') []) -> asCenterAndContinue $ regenerateScreens j d $ toggleUnmarked ui
        VtyEvent (EvKey (KChar 'P') []) -> asCenterAndContinue $ regenerateScreens j d $ togglePending ui
        VtyEvent (EvKey (KChar 'C') []) -> asCenterAndContinue $ regenerateScreens j d $ toggleCleared ui
        VtyEvent (EvKey (KChar 'F') []) -> asCenterAndContinue $ regenerateScreens j d $ toggleFuture ui

        VtyEvent (EvKey (KDown)     [MShift]) -> continue $ regenerateScreens j d $ shrinkReportPeriod d ui
        VtyEvent (EvKey (KUp)       [MShift]) -> continue $ regenerateScreens j d $ growReportPeriod d ui
        VtyEvent (EvKey (KRight)    [MShift]) -> continue $ regenerateScreens j d $ nextReportPeriod journalspan ui
        VtyEvent (EvKey (KLeft)     [MShift]) -> continue $ regenerateScreens j d $ previousReportPeriod journalspan ui
        VtyEvent (EvKey (KChar '/') []) -> continue $ regenerateScreens j d $ showMinibuffer ui
        VtyEvent (EvKey k           []) | k `elem` [KBS, KDel] -> (continue $ regenerateScreens j d $ resetFilter ui)
        VtyEvent e | e `elem` moveLeftEvents -> continue $ popScreen ui
        VtyEvent (EvKey (KChar 'l') [MCtrl]) -> scrollSelectionToMiddle _asList >> redraw ui
        VtyEvent (EvKey (KChar 'z') [MCtrl]) -> suspend ui

        -- enter register screen for selected account (if there is one),
        -- centering its selected transaction if possible
        VtyEvent e | e `elem` moveRightEvents
                   , not $ isBlankElement $ listSelectedElement _asList->
          -- TODO center selection after entering register screen; neither of these works till second time entering; easy strictifications didn't help
          rsCenterAndContinue $
          -- flip rsHandle (VtyEvent (EvKey (KChar 'l') [MCtrl])) $
            screenEnter d regscr ui
          where
            regscr = rsSetAccount selacct isdepthclipped registerScreen
            isdepthclipped = case getDepth ui of
                                Just d  -> accountNameLevel selacct >= d
                                Nothing -> False

        -- prevent moving down over blank padding items;
        -- instead scroll down by one, until maximally scrolled - shows the end has been reached
        VtyEvent (EvKey (KDown)     []) | isBlankElement mnextelement -> do
          vScrollBy (viewportScroll $ _asList^.listNameL) 1
          continue ui
          where
            mnextelement = listSelectedElement $ listMoveDown _asList

        -- if page down or end leads to a blank padding item, stop at last non-blank
        VtyEvent e@(EvKey k           []) | k `elem` [KPageDown, KEnd] -> do
          list <- handleListEvent e _asList
          if isBlankElement $ listSelectedElement list
          then do
            let list' = listMoveTo lastnonblankidx list
            scrollSelectionToMiddle list'
            continue ui{aScreen=scr{_asList=list'}}
          else
            continue ui{aScreen=scr{_asList=list}}

        -- fall through to the list's event handler (handles up/down)
        VtyEvent ev -> do
          newitems <- handleListEvent (normaliseMovementKeys ev) _asList
          continue $ ui{aScreen=scr & asList .~ newitems
                                    & asSelectedAccount .~ selacct
                                    }

        AppEvent _        -> continue ui
        MouseDown _ _ _ _ -> continue ui
        MouseUp _ _ _     -> continue ui

  where
    journalspan = journalDateSpan False j

asHandle _ _ = error "event handler called with wrong screen type, should not happen"

asSetSelectedAccount a s@AccountsScreen{} = s & asSelectedAccount .~ a
asSetSelectedAccount _ s = s

isBlankElement mel = ((asItemAccountName . snd) <$> mel) == Just ""

asCenterAndContinue ui = do
  scrollSelectionToMiddle $ _asList $ aScreen ui
  continue ui
