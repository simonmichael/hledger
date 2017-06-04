-- The accounts screen, showing accounts and balances like the CLI balance command.

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
import Brick.Widgets.Border (borderAttr)
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Data.List
import Data.Maybe
import Data.Monoid
import qualified Data.Text as T
import Data.Time.Calendar (Day)
import qualified Data.Vector as V
import Graphics.Vty (Event(..),Key(..),Modifier(..))
import Lens.Micro.Platform
import System.Console.ANSI
import System.FilePath (takeFileName)

import Hledger
import Hledger.Cli hiding (progname,prognameandversion,green)
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
    newitems = list AccountsList (V.fromList displayitems) 1

    -- keep the selection near the last selected account
    -- (may need to move to the next leaf account when entering flat mode)
    newitems' = listMoveTo selidx newitems
      where
        selidx = case (reset, listSelectedElement $ _asList s) of
                   (True, _)               -> 0
                   (_, Nothing)            -> 0
                   (_, Just (_,AccountsScreenItem{asItemAccountName=a})) -> fromMaybe (fromMaybe 0 mprefixmatch) mexactmatch
                     where
                       mexactmatch  = findIndex ((a ==)                      . asItemAccountName) displayitems
                       mprefixmatch = findIndex ((a `isAccountNamePrefixOf`) . asItemAccountName) displayitems
    uopts' = uopts{cliopts_=copts{reportopts_=ropts'}}
    ropts' = ropts{accountlistmode_=if flat_ ropts then ALFlat else ALTree}

    q = queryFromOpts d ropts

    -- maybe convert balances to market value
    convert | value_ ropts' = balanceReportValue j valuedate
            | otherwise    = id
      where
        valuedate = fromMaybe d $ queryEndDate False q

    -- run the report
    (items,_total) = convert $ report ropts' q j
      where
        -- still using the old balanceReport for change reports as it
        -- does not include every account from before the report period
        report | balancetype_ ropts == HistoricalBalance = singleBalanceReport
               | otherwise                               = balanceReport


    -- pre-render the list items
    displayitem (fullacct, shortacct, indent, bal) =
      AccountsScreenItem{asItemIndentLevel        = indent
                        ,asItemAccountName        = fullacct
                        ,asItemDisplayAccountName = replaceHiddenAccountsNameWith "All" $ if flat_ ropts' then fullacct else shortacct
                        ,asItemRenderedAmounts    = map showAmountWithoutPrice amts -- like showMixedAmountOneLineWithoutPrice
                        }
      where
        Mixed amts = normaliseMixedAmountSquashPricesForDisplay $ stripPrices bal
        stripPrices (Mixed as) = Mixed $ map stripprice as where stripprice a = a{aprice=NoPrice}
    displayitems = map displayitem items


asInit _ _ _ = error "init function called with wrong screen type, should not happen"

asDraw :: UIState -> [Widget Name]
asDraw UIState{aopts=UIOpts{cliopts_=copts@CliOpts{reportopts_=ropts}}
                           ,ajournal=j
                           ,aScreen=s@AccountsScreen{}
                           ,aMode=mode
                           } =
  case mode of
    Help       -> [helpDialog, maincontent]
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
              files
          -- <+> withAttr (borderAttr <> "query") (str (if flat_ ropts then " flat" else ""))
          <+> nonzero
          <+> str (if ishistorical then " accounts" else " account changes")
          -- <+> str (if ishistorical then " balances" else " changes")
          <+> borderPeriodStr (if ishistorical then "at end of" else "in") (period_ ropts)
          <+> borderQueryStr querystr
          <+> togglefilters
          <+> borderDepthStr mdepth
          <+> str " ("
          <+> cur
          <+> str "/"
          <+> total
          <+> str ")"
          <+> (if ignore_assertions_ copts
               then withAttr (borderAttr <> "query") (str " ignoring balance assertions")
               else str "")
          where
            files = case journalFilePaths j of
                           [] -> str ""
                           f:_ -> withAttr ("border" <> "bold") $ str $ takeFileName f
                           -- [f,_:[]] -> (withAttr ("border" <> "bold") $ str $ takeFileName f) <+> str " (& 1 included file)"
                           -- f:fs  -> (withAttr ("border" <> "bold") $ str $ takeFileName f) <+> str (" (& " ++ show (length fs) ++ " included files)")
            querystr = query_ ropts
            mdepth = depth_ ropts
            togglefilters =
              case concat [
                   uiShowClearedStatus $ clearedstatus_ ropts
                  ,if real_ ropts then ["real"] else []
                  ] of
                [] -> str ""
                fs -> str " from " <+> withAttr (borderAttr <> "query") (str $ intercalate ", " fs) <+> str " txns"
            nonzero | empty_ ropts = str ""
                    | otherwise    = withAttr (borderAttr <> "query") (str " nonzero")
            cur = str (case _asList s ^. listSelectedL of
                        Nothing -> "-"
                        Just i -> show (i + 1))
            total = str $ show $ V.length $ s ^. asList . listElementsL

        bottomlabel = case mode of
                        Minibuffer ed -> minibuffer ed
                        _             -> quickhelp
          where
            selectedstr = withAttr (borderAttr <> "query") . str
            quickhelp = borderKeysStr' [
               ("?", str "help")
              ,("right", str "register")
              ,("H"
               ,if ishistorical
                then selectedstr "historical" <+> str "/period"
                else str "historical/" <+> selectedstr "period")
              ,("F"
               ,if flat_ ropts
                then str "tree/" <+> selectedstr "flat"
                else selectedstr "tree" <+> str "/flat")
              ,("-+", str "depth")
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
  -- c <- getContext
  -- let h = c^.availHeightL
  --     moveSel n l = listMoveBy n l

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
        VtyEvent ev        -> do ed' <- handleEditorEvent ev ed
                                 continue $ ui{aMode=Minibuffer ed'}
        AppEvent _        -> continue ui
        MouseDown _ _ _ _ -> continue ui
        MouseUp _ _ _     -> continue ui

    Help ->
      case ev of
        VtyEvent (EvKey (KChar 'q') []) -> halt ui
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
        VtyEvent (EvKey (KChar 'H') []) -> continue $ regenerateScreens j d $ toggleHistorical ui
        VtyEvent (EvKey (KChar 'F') []) -> continue $ regenerateScreens j d $ toggleFlat ui
        VtyEvent (EvKey (KChar 'Z') []) -> scrollTop >> (continue $ regenerateScreens j d $ toggleEmpty ui)
        VtyEvent (EvKey (KChar 'C') []) -> scrollTop >> (continue $ regenerateScreens j d $ toggleCleared ui)
        VtyEvent (EvKey (KChar 'U') []) -> scrollTop >> (continue $ regenerateScreens j d $ toggleUncleared ui)
        VtyEvent (EvKey (KChar 'P') []) -> scrollTop >> (continue $ regenerateScreens j d $ togglePending ui)
        VtyEvent (EvKey (KChar 'X') []) -> scrollTop >> (continue $ regenerateScreens j d $ toggleNotPending ui)
        VtyEvent (EvKey (KChar 'R') []) -> scrollTop >> (continue $ regenerateScreens j d $ toggleReal ui)
        VtyEvent (EvKey (KDown)     [MShift]) -> continue $ regenerateScreens j d $ shrinkReportPeriod d ui
        VtyEvent (EvKey (KUp)       [MShift]) -> continue $ regenerateScreens j d $ growReportPeriod d ui
        VtyEvent (EvKey (KRight)    [MShift]) -> continue $ regenerateScreens j d $ nextReportPeriod journalspan ui
        VtyEvent (EvKey (KLeft)     [MShift]) -> continue $ regenerateScreens j d $ previousReportPeriod journalspan ui
        VtyEvent (EvKey (KChar '/') []) -> continue $ regenerateScreens j d $ showMinibuffer ui
        VtyEvent (EvKey k           []) | k `elem` [KBS, KDel] -> (continue $ regenerateScreens j d $ resetFilter ui)
        VtyEvent (EvKey k           []) | k `elem` [KLeft, KChar 'h']  -> continue $ popScreen ui
        VtyEvent (EvKey k           []) | k `elem` [KRight, KChar 'l'] -> scrollTopRegister >> continue (screenEnter d scr ui)
          where
            scr = rsSetAccount selacct isdepthclipped registerScreen
            isdepthclipped = case getDepth ui of
                                Just d  -> accountNameLevel selacct >= d
                                Nothing -> False

        -- fall through to the list's event handler (handles up/down)
        VtyEvent ev ->
              do
                let ev' = case ev of
                            EvKey (KChar 'k') [] -> EvKey (KUp) []
                            EvKey (KChar 'j') [] -> EvKey (KDown) []
                            _                    -> ev
                newitems <- handleListEvent ev' _asList
                continue $ ui{aScreen=scr & asList .~ newitems
                                          & asSelectedAccount .~ selacct
                                          }
                -- continue =<< handleEventLensed ui someLens ev
        AppEvent _        -> continue ui
        MouseDown _ _ _ _ -> continue ui
        MouseUp _ _ _     -> continue ui

  where
    -- Encourage a more stable scroll position when toggling list items.
    -- We scroll to the top, and the viewport will automatically
    -- scroll down just far enough to reveal the selection, which
    -- usually leaves it at bottom of screen).
    -- XXX better: scroll so selection is in middle of screen ?
    scrollTop         = vScrollToBeginning $ viewportScroll AccountsViewport
    scrollTopRegister = vScrollToBeginning $ viewportScroll RegisterViewport
    journalspan = journalDateSpan False j

asHandle _ _ = error "event handler called with wrong screen type, should not happen"

asSetSelectedAccount a s@AccountsScreen{} = s & asSelectedAccount .~ a
asSetSelectedAccount _ s = s

