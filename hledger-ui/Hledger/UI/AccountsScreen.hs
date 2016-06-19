-- The accounts screen, showing accounts and balances like the CLI balance command.

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Hledger.UI.AccountsScreen
 (accountsScreen
 ,asInit_
 ,asSetSelectedAccount
 )
where

import Brick
import Brick.Widgets.List
import Brick.Widgets.Edit
import Brick.Widgets.Border (borderAttr)
import Control.Monad.IO.Class (liftIO)
import Data.List
import Data.Maybe
import Data.Monoid
import qualified Data.Text as T
import Data.Time.Calendar (Day)
import qualified Data.Vector as V
import Graphics.Vty
import Lens.Micro.Platform
import System.Console.ANSI
import System.FilePath (takeFileName)

import Hledger
import Hledger.Cli hiding (progname,prognameandversion,green)
import Hledger.UI.UIOptions
import Hledger.UI.UITypes
import Hledger.UI.UIState
import Hledger.UI.UIUtils
import Hledger.UI.RegisterScreen
import Hledger.UI.ErrorScreen

accountsScreen :: AccountsScreen
accountsScreen = AccountsScreen{
   asInit   = asInit_
  ,asDraw   = asDraw_
  ,asHandle = asHandle_
  ,_asList            = list "accounts" V.empty 1
  ,_asSelectedAccount = ""
  }

asInit_ :: Day -> Bool -> UIState -> UIState
asInit_ d reset ui@UIState{
  aopts=uopts@UIOpts{cliopts_=copts@CliOpts{reportopts_=ropts}},
  ajournal=j,
  aScreen=AcctsScreen(s@AccountsScreen{})
  } =
  ui{aopts=uopts', aScreen=AcctsScreen (s & asList .~ newitems')}
   where
    newitems = list (Name "accounts") (V.fromList displayitems) 1

    -- keep the selection near the last selected account
    -- (may need to move to the next leaf account when entering flat mode)
    newitems' = listMoveTo selidx newitems
      where
        selidx = case (reset, listSelectedElement $ s ^. asList) of
                   (True, _)               -> 0
                   (_, Nothing)            -> 0
                   (_, Just (_,AccountsScreenItem{asItemAccountName=a})) -> fromMaybe (fromMaybe 0 mprefixmatch) mexactmatch
                     where
                       mexactmatch  = findIndex ((a ==)                      . asItemAccountName) displayitems
                       mprefixmatch = findIndex ((a `isAccountNamePrefixOf`) . asItemAccountName) displayitems
    uopts' = uopts{cliopts_=copts{reportopts_=ropts'}}
    ropts' = ropts {
      -- XXX balanceReport doesn't respect this yet
      balancetype_=HistoricalBalance
      }

    q = queryFromOpts d ropts

    -- maybe convert balances to market value
    convert | value_ ropts' = balanceReportValue j valuedate
            | otherwise    = id
      where
        valuedate = fromMaybe d $ queryEndDate False q

    -- run the report
    (items,_total) = convert $ balanceReport ropts' q j

    -- pre-render the list items
    displayitem ((fullacct, shortacct, indent), bal) =
      AccountsScreenItem{asItemIndentLevel        = indent
                        ,asItemAccountName        = fullacct
                        ,asItemDisplayAccountName = replaceHiddenAccountsNameWith "All" $ if flat_ ropts' then fullacct else shortacct
                        ,asItemRenderedAmounts    = map showAmountWithoutPrice amts -- like showMixedAmountOneLineWithoutPrice
                        }
      where
        Mixed amts = normaliseMixedAmountSquashPricesForDisplay $ stripPrices bal
        stripPrices (Mixed as) = Mixed $ map stripprice as where stripprice a = a{aprice=NoPrice}
    displayitems = map displayitem items


asInit_ _ _ _ = error "init function called with wrong screen type, should not happen"

asDraw_ :: UIState -> [Widget]
asDraw_ UIState{aopts=UIOpts{cliopts_=CliOpts{reportopts_=ropts}}
                            ,ajournal=j
                            ,aScreen=AcctsScreen(s@AccountsScreen{})
                            ,aMode=mode
                            } =
  case mode of
    Help       -> [helpDialog, maincontent]
    -- Minibuffer e -> [minibuffer e, maincontent]
    _          -> [maincontent]
  where
    toplabel = files
            <+> nonzero
            <+> str " accounts"
            <+> withAttr (borderAttr <> "query") (str (if flat_ ropts then " (flat)" else ""))
            <+> borderQueryStr querystr
            <+> togglefilters
            <+> borderDepthStr mdepth
            <+> str " ("
            <+> cur
            <+> str "/"
            <+> total
            <+> str ")"
    files = case journalFilePaths j of
                   [] -> str ""
                   f:_ -> withAttr ("border" <> "bold") $ str $ takeFileName f
                   -- [f,_:[]] -> (withAttr ("border" <> "bold") $ str $ takeFileName f) <+> str " (& 1 included file)"
                   -- f:fs  -> (withAttr ("border" <> "bold") $ str $ takeFileName f) <+> str (" (& " ++ show (length fs) ++ " included files)")
    querystr = query_ ropts
    mdepth = depth_ ropts
    togglefilters =
      case concat [
           if cleared_ ropts then ["cleared"] else []
          ,if uncleared_ ropts then ["uncleared"] else []
          ,if pending_ ropts then ["pending"] else []
          ,if real_ ropts then ["real"] else []
          ] of
        [] -> str ""
        fs -> str " with " <+> withAttr (borderAttr <> "query") (str $ intercalate ", " fs) <+> str " txns"
    nonzero | empty_ ropts = str ""
            | otherwise    = withAttr (borderAttr <> "query") (str " nonzero")
    cur = str (case s ^. asList ^. listSelectedL of  -- XXX second ^. required here but not below..
                Nothing -> "-"
                Just i -> show (i + 1))
    total = str $ show $ V.length $ s ^. asList . listElementsL
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
          V.map (\AccountsScreenItem{..} -> asItemIndentLevel*2 + textWidth asItemDisplayAccountName) $
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

      render $ defaultLayout toplabel bottomlabel $ renderList (s ^. asList) (asDrawItem colwidths)

      where
        bottomlabel = case mode of
                        Minibuffer ed -> minibuffer ed
                        _             -> quickhelp
        quickhelp = borderKeysStr [
           ("h", "help")
          ,("right", "register")
          ,("F", "flat?")
          ,("-+0123456789", "depth")
          --,("/", "filter")
          --,("DEL", "unfilter")
          --,("ESC", "cancel/top")
          ,("a", "add")
          ,("g", "reload")
          ,("q", "quit")
          ]

asDraw_ _ = error "draw function called with wrong screen type, should not happen"

asDrawItem :: (Int,Int) -> Bool -> AccountsScreenItem -> Widget
asDrawItem (acctwidth, balwidth) selected AccountsScreenItem{..} =
  Widget Greedy Fixed $ do
    -- c <- getContext
      -- let showitem = intercalate "\n" . balanceReportItemAsText defreportopts fmt
    render $
      addamts asItemRenderedAmounts $
      str (T.unpack $ fitText (Just acctwidth) (Just acctwidth) True True $ T.replicate (2*asItemIndentLevel) " " <> asItemDisplayAccountName) <+>
      str "  " <+>
      str (balspace asItemRenderedAmounts)
      where
        balspace as = replicate n ' '
          where n = max 0 (balwidth - (sum (map strWidth as) + 2 * (length as - 1)))
        addamts :: [String] -> Widget -> Widget
        addamts [] w = w
        addamts [a] w = (<+> renderamt a) w
        -- foldl' :: (b -> a -> b) -> b -> t a -> b
        -- foldl' (Widget -> String -> Widget) -> Widget -> [String] -> Widget
        addamts (a:as) w = foldl' addamt (addamts [a] w) as
        addamt :: Widget -> String -> Widget
        addamt w a = ((<+> renderamt a) . (<+> str ", ")) w
        renderamt :: String -> Widget
        renderamt a | '-' `elem` a = withAttr (sel $ "list" <> "balance" <> "negative") $ str a
                    | otherwise    = withAttr (sel $ "list" <> "balance" <> "positive") $ str a
        sel | selected  = (<> "selected")
            | otherwise = id

asHandle_ :: UIState -> Event -> EventM (Next UIState)
asHandle_ ui0@UIState{
   aScreen=AcctsScreen(scr@AccountsScreen{..})
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
    selacct = case listSelectedElement $ scr ^. asList of
                Just (_, AccountsScreenItem{..}) -> asItemAccountName
                Nothing -> scr ^. asSelectedAccount
    ui = ui0{aScreen=AcctsScreen (scr & asSelectedAccount .~ selacct)}

  case mode of
    Minibuffer ed ->
      case ev of
        EvKey KEsc   [] -> continue $ closeMinibuffer ui
        EvKey KEnter [] -> continue $ regenerateScreens j d $ setFilter s $ closeMinibuffer ui
                            where s = chomp $ unlines $ getEditContents ed
        ev              -> do ed' <- handleEvent ev ed
                              continue $ ui{aMode=Minibuffer ed'}

    Help ->
      case ev of
        EvKey (KChar 'q') [] -> halt ui
        _                    -> helpHandle ui ev

    Normal ->
      case ev of
        EvKey (KChar 'q') [] -> halt ui
        -- EvKey (KChar 'l') [MCtrl] -> do
        EvKey KEsc        [] -> continue $ resetScreens d ui
        EvKey (KChar c)   [] | c `elem` ['h','?'] -> continue $ setMode Help ui
        EvKey (KChar 'g') [] -> liftIO (uiReloadJournalIfChanged copts d j ui) >>= continue
        EvKey (KChar 'a') [] -> suspendAndResume $ clearScreen >> setCursorPosition 0 0 >> add copts j >> uiReloadJournalIfChanged copts d j ui
        EvKey (KChar '0') [] -> continue $ regenerateScreens j d $ setDepth (Just 0) ui
        EvKey (KChar '1') [] -> continue $ regenerateScreens j d $ setDepth (Just 1) ui
        EvKey (KChar '2') [] -> continue $ regenerateScreens j d $ setDepth (Just 2) ui
        EvKey (KChar '3') [] -> continue $ regenerateScreens j d $ setDepth (Just 3) ui
        EvKey (KChar '4') [] -> continue $ regenerateScreens j d $ setDepth (Just 4) ui
        EvKey (KChar '5') [] -> continue $ regenerateScreens j d $ setDepth (Just 5) ui
        EvKey (KChar '6') [] -> continue $ regenerateScreens j d $ setDepth (Just 6) ui
        EvKey (KChar '7') [] -> continue $ regenerateScreens j d $ setDepth (Just 7) ui
        EvKey (KChar '8') [] -> continue $ regenerateScreens j d $ setDepth (Just 8) ui
        EvKey (KChar '9') [] -> continue $ regenerateScreens j d $ setDepth (Just 9) ui
        EvKey (KChar '-') [] -> continue $ regenerateScreens j d $ decDepth ui
        EvKey (KChar '_') [] -> continue $ regenerateScreens j d $ decDepth ui
        EvKey (KChar c)   [] | c `elem` ['+','='] -> continue $ regenerateScreens j d $ incDepth ui
        EvKey (KChar 'F') [] -> continue $ regenerateScreens j d $ toggleFlat ui
        EvKey (KChar 'Z') [] -> scrollTop >> (continue $ regenerateScreens j d $ toggleEmpty ui)
        EvKey (KChar 'C') [] -> scrollTop >> (continue $ regenerateScreens j d $ toggleCleared ui)
        EvKey (KChar 'U') [] -> scrollTop >> (continue $ regenerateScreens j d $ toggleUncleared ui)
        EvKey (KChar 'R') [] -> scrollTop >> (continue $ regenerateScreens j d $ toggleReal ui)
        EvKey (KChar '/') [] -> continue $ regenerateScreens j d $ showMinibuffer ui
        EvKey k           [] | k `elem` [KBS, KDel] -> (continue $ regenerateScreens j d $ resetFilter ui)
        EvKey (KLeft) []     -> continue $ popScreen ui
        EvKey k           [] | k `elem` [KRight, KEnter] -> scrollTopRegister >> continue (screenEnter d scr ui)
          where
            scr = rsSetAccount selacct (RegScreen registerScreen)

        -- fall through to the list's event handler (handles up/down)
        ev                       -> do
                                     newitems <- handleEvent ev (scr ^. asList)
                                     continue $ ui{aScreen=AcctsScreen (scr & asList .~ newitems
                                                                            & asSelectedAccount .~ selacct
                                                                       )}
                                 -- continue =<< handleEventLensed ui someLens ev

  where
    -- Encourage a more stable scroll position when toggling list items.
    -- We scroll to the top, and the viewport will automatically
    -- scroll down just far enough to reveal the selection, which
    -- usually leaves it at bottom of screen).
    -- XXX better: scroll so selection is in middle of screen ?
    scrollTop         = vScrollToBeginning $ viewportScroll "accounts"
    scrollTopRegister = vScrollToBeginning $ viewportScroll "register"

asHandle_ _ _ = error "event handler called with wrong screen type, should not happen"

asSetSelectedAccount :: AccountName -> Screen -> Screen
asSetSelectedAccount a (AcctsScreen s@AccountsScreen{}) = AcctsScreen (s & asSelectedAccount .~ a)
asSetSelectedAccount _ s = s

