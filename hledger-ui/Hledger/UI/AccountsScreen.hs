-- The accounts screen, showing accounts and balances like the CLI balance command.

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Hledger.UI.AccountsScreen
 (accountsScreen
 ,asInit
 ,asSetSelectedAccount
 )
where

-- import Control.Monad
import Control.Monad.IO.Class (liftIO)
-- import Data.Default
import Data.List
import Data.Maybe
import Data.Monoid
-- import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Calendar (Day)
import System.FilePath (takeFileName)
import qualified Data.Vector as V
import Graphics.Vty as Vty
import Brick
-- import Brick.Widgets.Center
import Brick.Widgets.List
import Brick.Widgets.Edit
import Brick.Widgets.Border (borderAttr)
-- import Brick.Widgets.Center
import Lens.Micro.Platform
import System.Console.ANSI

import Hledger
import Hledger.Cli hiding (progname,prognameandversion,green)
-- import Hledger.Cli.CliOptions (defaultBalanceLineFormat)
import Hledger.UI.UIOptions
-- import Hledger.UI.Theme
import Hledger.UI.UITypes
import Hledger.UI.UIUtils
import Hledger.UI.RegisterScreen
import Hledger.UI.ErrorScreen

accountsScreen :: Screen
accountsScreen = AccountsScreen{
   sInit   = asInit
  ,sDraw   = asDraw
  ,sHandle = asHandle
  ,_asList            = list "accounts" V.empty 1
  ,_asSelectedAccount = ""
  }

asInit :: Day -> Bool -> AppState -> AppState
asInit d reset st@AppState{
  aopts=uopts@UIOpts{cliopts_=copts@CliOpts{reportopts_=ropts}},
  ajournal=j,
  aScreen=s@AccountsScreen{}
  } =
  st{aopts=uopts', aScreen=s & asList .~ newitems'}
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
                        ,asItemDisplayAccountName = if flat_ ropts' then fullacct else shortacct
                        ,asItemRenderedAmounts    = map showAmountWithoutPrice amts -- like showMixedAmountOneLineWithoutPrice
                        }
      where
        Mixed amts = normaliseMixedAmountSquashPricesForDisplay $ stripPrices bal
        stripPrices (Mixed as) = Mixed $ map stripprice as where stripprice a = a{aprice=NoPrice}
    displayitems = map displayitem items


asInit _ _ _ = error "init function called with wrong screen type, should not happen"

asDraw :: AppState -> [Widget]
asDraw AppState{aopts=UIOpts{cliopts_=CliOpts{reportopts_=ropts}}
                           ,ajournal=j
                           ,aScreen=s@AccountsScreen{}
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
          ,("-+=1234567890", "depth")
          --,("/", "filter")
          --,("DEL", "unfilter")
          --,("ESC", "cancel/top")
          ,("a", "add")
          ,("g", "reload")
          ,("q", "quit")
          ]

asDraw _ = error "draw function called with wrong screen type, should not happen"

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

asHandle :: AppState -> Vty.Event -> EventM (Next AppState)
asHandle st'@AppState{
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
    selacct = case listSelectedElement $ scr ^. asList of
                Just (_, AccountsScreenItem{..}) -> asItemAccountName
                Nothing -> scr ^. asSelectedAccount
    st = st'{aScreen=scr & asSelectedAccount .~ selacct}

  case mode of
    Minibuffer ed ->
      case ev of
        Vty.EvKey Vty.KEsc   [] -> continue $ stCloseMinibuffer st'
        Vty.EvKey Vty.KEnter [] -> continue $ regenerateScreens j d $ stFilter s $ stCloseMinibuffer st'
                                    where s = chomp $ unlines $ getEditContents ed
        ev                      -> do ed' <- handleEvent ev ed
                                      continue $ st'{aMode=Minibuffer ed'}

    Help ->
      case ev of
        Vty.EvKey (Vty.KChar 'q') [] -> halt st
        _                            -> helpHandle st ev

    Normal ->
      case ev of
        Vty.EvKey (Vty.KChar 'q') [] -> halt st
        -- Vty.EvKey (Vty.KChar 'l') [Vty.MCtrl] -> do
        Vty.EvKey Vty.KEsc   [] -> continue $ resetScreens d st
        Vty.EvKey k [] | k `elem` [Vty.KChar 'h', Vty.KChar '?'] -> continue $ setMode Help st
        Vty.EvKey (Vty.KChar 'g') [] -> liftIO (stReloadJournalIfChanged copts d j st) >>= continue
        Vty.EvKey (Vty.KChar 'a') [] -> suspendAndResume $ clearScreen >> setCursorPosition 0 0 >> add copts j >> stReloadJournalIfChanged copts d j st
        Vty.EvKey (Vty.KChar '-') [] -> continue $ regenerateScreens j d $ decDepth st
        Vty.EvKey (Vty.KChar '+') [] -> continue $ regenerateScreens j d $ incDepth st
        Vty.EvKey (Vty.KChar '=') [] -> continue $ regenerateScreens j d $ incDepth st
        Vty.EvKey (Vty.KChar '1') [] -> continue $ regenerateScreens j d $ setDepth 1 st
        Vty.EvKey (Vty.KChar '2') [] -> continue $ regenerateScreens j d $ setDepth 2 st
        Vty.EvKey (Vty.KChar '3') [] -> continue $ regenerateScreens j d $ setDepth 3 st
        Vty.EvKey (Vty.KChar '4') [] -> continue $ regenerateScreens j d $ setDepth 4 st
        Vty.EvKey (Vty.KChar '5') [] -> continue $ regenerateScreens j d $ setDepth 5 st
        Vty.EvKey (Vty.KChar '6') [] -> continue $ regenerateScreens j d $ setDepth 6 st
        Vty.EvKey (Vty.KChar '7') [] -> continue $ regenerateScreens j d $ setDepth 7 st
        Vty.EvKey (Vty.KChar '8') [] -> continue $ regenerateScreens j d $ setDepth 8 st
        Vty.EvKey (Vty.KChar '9') [] -> continue $ regenerateScreens j d $ setDepth 9 st
        Vty.EvKey (Vty.KChar '0') [] -> continue $ regenerateScreens j d $ setDepth 0 st
        Vty.EvKey (Vty.KChar 'F') [] -> continue $ regenerateScreens j d $ stToggleFlat st
        Vty.EvKey (Vty.KChar 'E') [] -> scrollTop >> (continue $ regenerateScreens j d $ stToggleEmpty st)
        Vty.EvKey (Vty.KChar 'C') [] -> scrollTop >> (continue $ regenerateScreens j d $ stToggleCleared st)
        Vty.EvKey (Vty.KChar 'U') [] -> scrollTop >> (continue $ regenerateScreens j d $ stToggleUncleared st)
        Vty.EvKey (Vty.KChar 'R') [] -> scrollTop >> (continue $ regenerateScreens j d $ stToggleReal st)
        Vty.EvKey k [] | k `elem` [Vty.KChar '/'] -> continue $ regenerateScreens j d $ stShowMinibuffer st
        Vty.EvKey k [] | k `elem` [Vty.KBS, Vty.KDel] -> (continue $ regenerateScreens j d $ stResetFilter st)
        Vty.EvKey (Vty.KLeft) []     -> continue $ popScreen st
        Vty.EvKey (k) [] | k `elem` [Vty.KRight, Vty.KEnter] -> scrollTopRegister >> continue (screenEnter d scr st)
          where
            scr = rsSetAccount selacct registerScreen

        -- fall through to the list's event handler (handles up/down)
        ev                       -> do
                                     newitems <- handleEvent ev (scr ^. asList)
                                     continue $ st'{aScreen=scr & asList .~ newitems
                                                                & asSelectedAccount .~ selacct
                                                                }
                                 -- continue =<< handleEventLensed st' someLens ev

  where
    -- Encourage a more stable scroll position when toggling list items.
    -- We scroll to the top, and the viewport will automatically
    -- scroll down just far enough to reveal the selection, which
    -- usually leaves it at bottom of screen).
    -- XXX better: scroll so selection is in middle of screen ?
    scrollTop         = vScrollToBeginning $ viewportScroll "accounts"
    scrollTopRegister = vScrollToBeginning $ viewportScroll "register"

asHandle _ _ = error "event handler called with wrong screen type, should not happen"

asSetSelectedAccount a s@AccountsScreen{} = s & asSelectedAccount .~ a
asSetSelectedAccount _ s = s

