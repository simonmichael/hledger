-- The accounts screen, showing accounts and balances like the CLI balance command.

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Hledger.UI.AccountsScreen
 (screen
 ,initAccountsScreen
 ,asSetSelectedAccount
 )
where

import Lens.Micro ((^.))
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
import Brick.Widgets.List
import Brick.Widgets.Border (borderAttr)
-- import Brick.Widgets.Center

import Hledger
import Hledger.Cli hiding (progname,prognameandversion,green)
-- import Hledger.Cli.CliOptions (defaultBalanceLineFormat)
import Hledger.UI.UIOptions
-- import Hledger.UI.Theme
import Hledger.UI.UITypes
import Hledger.UI.UIUtils
import qualified Hledger.UI.RegisterScreen as RS (screen, rsSetCurrentAccount)
import qualified Hledger.UI.ErrorScreen as ES (screen)

screen = AccountsScreen{
   asState   = (list "accounts" V.empty 1, "")
  ,sInitFn   = initAccountsScreen
  ,sDrawFn   = drawAccountsScreen
  ,sHandleFn = handleAccountsScreen
  }

asSetSelectedAccount a scr@AccountsScreen{asState=(l,_)} = scr{asState=(l,a)}
asSetSelectedAccount _ scr = scr

initAccountsScreen :: Day -> AppState -> AppState
initAccountsScreen d st@AppState{
  aopts=uopts@UIOpts{cliopts_=copts@CliOpts{reportopts_=ropts}},
  ajournal=j,
  aScreen=s@AccountsScreen{asState=(_,selacct)}
  } =
  st{aopts=uopts', aScreen=s{asState=(l',selacct)}}
   where
    l = list (Name "accounts") (V.fromList displayitems) 1

    -- keep the selection near the last known selected account if possible
    l' | T.null selacct = l
       | otherwise = maybe l (flip listMoveTo l) midx
      where
        midx = findIndex (\((a,_,_),_) -> a==selacctclipped) items
        selacctclipped = case depth_ ropts of
                          Nothing -> selacct
                          Just d  -> clipAccountName d selacct

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
      (indent
      ,fullacct
      ,if flat_ ropts' then fullacct else shortacct
      ,map showAmountWithoutPrice amts -- like showMixedAmountOneLineWithoutPrice
      )
      where
        Mixed amts = normaliseMixedAmountSquashPricesForDisplay $ stripPrices bal
        stripPrices (Mixed as) = Mixed $ map stripprice as where stripprice a = a{aprice=NoPrice}
    displayitems = map displayitem items


initAccountsScreen _ _ = error "init function called with wrong screen type, should not happen"

drawAccountsScreen :: AppState -> [Widget]
drawAccountsScreen AppState{aopts=UIOpts{cliopts_=CliOpts{reportopts_=ropts}}
                           ,ajournal=j
                           ,aScreen=AccountsScreen{asState=(l,_)}} =
  [ui]
    where
      toplabel = files
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
            ,if real_ ropts then ["real"] else []
            ] of
          [] -> str ""
          fs -> str " with " <+> withAttr (borderAttr <> "query") (str $ intercalate ", " fs) <+> str " txns"
      cur = str (case l^.listSelectedL of
                  Nothing -> "-"
                  Just i -> show (i + 1))
      total = str $ show $ V.length $ l^.listElementsL

      bottomlabel = borderKeysStr [
         -- ("up/down/pgup/pgdown/home/end", "move")
         ("-=1234567890", "depth")
        ,("F", "flat?")
        ,("E", "empty?")
        ,("C", "cleared?")
        ,("R", "real?")
        ,("right/enter", "register")
        ,("g", "reload")
        ,("q", "quit")
        ]

      ui = Widget Greedy Greedy $ do
        c <- getContext
        let
          availwidth =
            -- ltrace "availwidth" $
            c^.availWidthL
            - 2 -- XXX due to margin ? shouldn't be necessary (cf UIUtils)
          displayitems = listElements l
          maxacctwidthseen =
            -- ltrace "maxacctwidthseen" $
            V.maximum $
            V.map (\(indent,_,displayacct,_) -> indent*2 + textWidth displayacct) $
            -- V.filter (\(indent,_,_,_) -> (indent-1) <= fromMaybe 99999 mdepth) $
            displayitems
          maxbalwidthseen =
            -- ltrace "maxbalwidthseen" $
            V.maximum $ V.map (\(_,_,_,amts) -> sum (map strWidth amts) + 2 * (length amts-1)) displayitems
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

        render $ defaultLayout toplabel bottomlabel $ renderList l (drawAccountsItem colwidths)

drawAccountsScreen _ = error "draw function called with wrong screen type, should not happen"

drawAccountsItem :: (Int,Int) -> Bool -> (Int, AccountName, AccountName, [String]) -> Widget
drawAccountsItem (acctwidth, balwidth) selected (indent, _fullacct, displayacct, balamts) =
  Widget Greedy Fixed $ do
    -- c <- getContext
      -- let showitem = intercalate "\n" . balanceReportItemAsText defreportopts fmt
    render $
      addamts balamts $
      str (T.unpack $ fitText (Just acctwidth) (Just acctwidth) True True $ T.replicate (2*indent) " " <> displayacct) <+>
      str "  " <+>
      str (balspace balamts)
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

handleAccountsScreen :: AppState -> Vty.Event -> EventM (Next AppState)
handleAccountsScreen st@AppState{
   aScreen=scr@AccountsScreen{asState=(l,selacct)}
  ,aopts=UIOpts{cliopts_=copts}
  ,ajournal=j
  } e = do
    d <- liftIO getCurrentDay
    -- c <- getContext
    -- let h = c^.availHeightL
    --     moveSel n l = listMoveBy n l

    -- before we go anywhere, remember the currently selected account.
    -- (This is preserved across screen changes, unlike List's selection state)
    let
      selacct' = case listSelectedElement l of
                  Just (_, (_, fullacct, _, _)) -> fullacct
                  Nothing -> selacct
      st' = st{aScreen=scr{asState=(l,selacct')}}

    case e of
        Vty.EvKey Vty.KEsc []        -> halt st'
        Vty.EvKey (Vty.KChar 'q') [] -> halt st'
        -- Vty.EvKey (Vty.KChar 'l') [Vty.MCtrl] -> do

        Vty.EvKey (Vty.KChar 'g') [] -> do
          (ej, _) <- liftIO $ journalReloadIfChanged copts d j
          case ej of
            Right j' -> continue $ reload j' d st'
            Left err -> continue $ screenEnter d ES.screen{esState=err} st'

        Vty.EvKey (Vty.KChar '-') [] -> continue $ reload j d $ decDepth st'
        Vty.EvKey (Vty.KChar '+') [] -> continue $ reload j d $ incDepth st'
        Vty.EvKey (Vty.KChar '=') [] -> continue $ reload j d $ incDepth st'
        Vty.EvKey (Vty.KChar '1') [] -> continue $ reload j d $ setDepth 1 st'
        Vty.EvKey (Vty.KChar '2') [] -> continue $ reload j d $ setDepth 2 st'
        Vty.EvKey (Vty.KChar '3') [] -> continue $ reload j d $ setDepth 3 st'
        Vty.EvKey (Vty.KChar '4') [] -> continue $ reload j d $ setDepth 4 st'
        Vty.EvKey (Vty.KChar '5') [] -> continue $ reload j d $ setDepth 5 st'
        Vty.EvKey (Vty.KChar '6') [] -> continue $ reload j d $ setDepth 6 st'
        Vty.EvKey (Vty.KChar '7') [] -> continue $ reload j d $ setDepth 7 st'
        Vty.EvKey (Vty.KChar '8') [] -> continue $ reload j d $ setDepth 8 st'
        Vty.EvKey (Vty.KChar '9') [] -> continue $ reload j d $ setDepth 9 st'
        Vty.EvKey (Vty.KChar '0') [] -> continue $ reload j d $ setDepth 0 st'
        Vty.EvKey (Vty.KChar 'F') [] -> continue $ reload j d $ stToggleFlat st'
        Vty.EvKey (Vty.KChar 'E') [] -> continue $ reload j d $ stToggleEmpty st'
        Vty.EvKey (Vty.KChar 'C') [] -> continue $ reload j d $ stToggleCleared st'
        Vty.EvKey (Vty.KChar 'R') [] -> continue $ reload j d $ stToggleReal st'
        Vty.EvKey (Vty.KLeft) []     -> continue $ popScreen st'
        Vty.EvKey (k) [] | k `elem` [Vty.KRight, Vty.KEnter] -> do
          let
            scr = RS.rsSetCurrentAccount selacct' RS.screen
            st'' = screenEnter d scr st'
          vScrollToBeginning $ viewportScroll "register"
          continue st''

        -- fall through to the list's event handler (handles up/down)
        ev                       -> do
                                     l' <- handleEvent ev l
                                     continue $ st'{aScreen=scr{asState=(l',selacct')}}
                                 -- continue =<< handleEventLensed st' someLens ev
handleAccountsScreen _ _ = error "event handler called with wrong screen type, should not happen"

-- | Get the maximum account depth in the current journal.
maxDepth :: AppState -> Int
maxDepth AppState{ajournal=j} = maximum $ map accountNameLevel $ journalAccountNames j

-- | Decrement the current depth limit towards 0. If there was no depth limit,
-- set it to one less than the maximum account depth.
decDepth :: AppState -> AppState
decDepth st@AppState{aopts=uopts@UIOpts{cliopts_=copts@CliOpts{reportopts_=ropts@ReportOpts{..}}}}
  = st{aopts=uopts{cliopts_=copts{reportopts_=ropts{depth_=dec depth_}}}}
  where
    dec (Just d) = Just $ max 0 (d-1)
    dec Nothing  = Just $ maxDepth st - 1

-- | Increment the current depth limit. If this makes it equal to the
-- the maximum account depth, remove the depth limit.
incDepth :: AppState -> AppState
incDepth st@AppState{aopts=uopts@UIOpts{cliopts_=copts@CliOpts{reportopts_=ropts@ReportOpts{..}}}}
  = st{aopts=uopts{cliopts_=copts{reportopts_=ropts{depth_=inc depth_}}}}
  where
    inc (Just d) | d < (maxDepth st - 1) = Just $ d+1
    inc _ = Nothing

-- | Set the current depth limit to the specified depth, which should
-- be a positive number.  If it is zero, or equal to or greater than the
-- current maximum account depth, the depth limit will be removed.
-- (Slight inconsistency here: zero is currently a valid display depth
-- which can be reached using the - key.  But we need a key to remove
-- the depth limit, and 0 is it.)
setDepth :: Int -> AppState -> AppState
setDepth depth st@AppState{aopts=uopts@UIOpts{cliopts_=copts@CliOpts{reportopts_=ropts}}}
  = st{aopts=uopts{cliopts_=copts{reportopts_=ropts{depth_=mdepth'}}}}
  where
    mdepth' | depth < 0            = depth_ ropts
            | depth == 0           = Nothing
            | depth >= maxDepth st = Nothing
            | otherwise            = Just depth

