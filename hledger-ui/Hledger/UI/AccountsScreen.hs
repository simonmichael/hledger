-- The accounts screen, showing accounts and balances like the CLI balance command.

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Hledger.UI.AccountsScreen
 (screen
 ,initAccountsScreen
 )
where

import Control.Lens ((^.))
-- import Control.Monad
import Control.Monad.IO.Class (liftIO)
-- import Data.Default
import Data.List
import Data.Maybe
import Data.Monoid
import Data.Time.Calendar (Day)
import System.FilePath (takeFileName)
import qualified Data.Vector as V
import Graphics.Vty as Vty
import Brick
import Brick.Widgets.List
-- import Brick.Widgets.Border
-- import Brick.Widgets.Center

import Hledger
import Hledger.Cli hiding (progname,prognameandversion,green)
-- import Hledger.Cli.CliOptions (defaultBalanceLineFormat)
import Hledger.UI.UIOptions
-- import Hledger.UI.Theme
import Hledger.UI.UITypes
import Hledger.UI.UIUtils
import qualified Hledger.UI.RegisterScreen as RS (screen)

screen = AccountsScreen{
   asState  = list "accounts" V.empty 1
  ,sInitFn    = initAccountsScreen Nothing
  ,sDrawFn    = drawAccountsScreen
  ,sHandleFn = handleAccountsScreen
  }

initAccountsScreen :: Maybe AccountName -> Day -> AppState -> AppState
initAccountsScreen mselacct d st@AppState{
  aopts=uopts@UIOpts{cliopts_=copts@CliOpts{reportopts_=ropts}},
  ajournal=j,
  aScreen=s@AccountsScreen{}
  } =
  st{aopts=uopts', aScreen=s{asState=l'}}
   where
    l = list (Name "accounts") (V.fromList displayitems) 1

    -- hacky: when we're adjusting depth, mselacct is the account that was selected previously,
    -- in which case try and keep the selection near where it was
    l' = case mselacct of
             Nothing -> l
             Just a  -> -- vScrollToBeginning $ viewportScroll "accounts"
                           maybe l (flip listMoveTo l) mi
               where
                 mi = findIndex (\((acct,_,_),_) -> acct==a') items
                 a' = maybe a (flip clipAccountName a) $ depth_ ropts

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


initAccountsScreen _ _ _ = error "init function called with wrong screen type, should not happen"

drawAccountsScreen :: AppState -> [Widget]
drawAccountsScreen _st@AppState{aopts=uopts, ajournal=j, aScreen=AccountsScreen{asState=l}} =
  [ui]
    where
      toplabel = files
              <+> str " accounts"
              <+> borderQueryStr querystr
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
      querystr = query_ $ reportopts_ $ cliopts_ uopts
      mdepth = depth_ $ reportopts_ $ cliopts_ uopts
      cur = str (case l^.listSelectedL of
                  Nothing -> "-"
                  Just i -> show (i + 1))
      total = str $ show $ V.length $ l^.listElementsL

      bottomlabel = borderKeysStr [
         -- ("up/down/pgup/pgdown/home/end", "move")
         ("-+=1234567890", "adjust depth limit")
        ,("f", "flat/tree mode")
        ,("right/enter", "show transactions")
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
            V.map (\(indent,_,displayacct,_) -> indent*2 + strWidth displayacct) $
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

drawAccountsItem :: (Int,Int) -> Bool -> (Int, String, String, [String]) -> Widget
drawAccountsItem (acctwidth, balwidth) selected (indent, _fullacct, displayacct, balamts) =
  Widget Greedy Fixed $ do
    -- c <- getContext
      -- let showitem = intercalate "\n" . balanceReportItemAsText defreportopts fmt
    render $
      addamts balamts $
      str (fitString (Just acctwidth) (Just acctwidth) True True $ replicate (2*indent) ' ' ++ displayacct) <+>
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
  aScreen=scr@AccountsScreen{asState=l}
  ,aopts=UIOpts{cliopts_=copts@CliOpts{reportopts_=ropts}}
  ,ajournal=j
  } e = do
    d <- liftIO getCurrentDay
    -- c <- getContext
    -- let h = c^.availHeightL
    --     moveSel n l = listMoveBy n l
    let
      acct = case listSelectedElement l of
              Just (_, (_, fullacct, _, _)) -> fullacct
              Nothing -> ""
      reload = continue . initAccountsScreen (Just acct) d

    case e of
        Vty.EvKey Vty.KEsc []        -> halt st
        Vty.EvKey (Vty.KChar 'q') [] -> halt st
        -- Vty.EvKey (Vty.KChar 'l') [Vty.MCtrl] -> do
        Vty.EvKey (Vty.KChar 'g') [] -> do
          (ej, changed) <- liftIO $ journalReloadIfChanged copts j
          case (changed, ej) of
            (True, Right j') -> reload st{ajournal=j'}
            -- (True, Left err) -> continue st{amsg=err} -- XXX report parse error
            _                -> continue st
        Vty.EvKey (Vty.KChar '-') [] -> reload $ decDepth st
        Vty.EvKey (Vty.KChar '+') [] -> reload $ incDepth st
        Vty.EvKey (Vty.KChar '=') [] -> reload $ incDepth st
        Vty.EvKey (Vty.KChar '1') [] -> reload $ setDepth 1 st
        Vty.EvKey (Vty.KChar '2') [] -> reload $ setDepth 2 st
        Vty.EvKey (Vty.KChar '3') [] -> reload $ setDepth 3 st
        Vty.EvKey (Vty.KChar '4') [] -> reload $ setDepth 4 st
        Vty.EvKey (Vty.KChar '5') [] -> reload $ setDepth 5 st
        Vty.EvKey (Vty.KChar '6') [] -> reload $ setDepth 6 st
        Vty.EvKey (Vty.KChar '7') [] -> reload $ setDepth 7 st
        Vty.EvKey (Vty.KChar '8') [] -> reload $ setDepth 8 st
        Vty.EvKey (Vty.KChar '9') [] -> reload $ setDepth 9 st
        Vty.EvKey (Vty.KChar '0') [] -> reload $ setDepth 0 st
        Vty.EvKey (Vty.KChar 'f') [] -> reload $ st'
          where
            st' = st{
              aopts=(aopts st){
                 cliopts_=copts{
                    reportopts_=toggleFlatMode ropts
                    }
                 }
              }
        Vty.EvKey (Vty.KLeft) []     -> continue $ popScreen st
        Vty.EvKey (k) [] | k `elem` [Vty.KRight, Vty.KEnter] -> do
          let st' = screenEnter d RS.screen{rsAcct=acct} st
          vScrollToBeginning $ viewportScroll "register"
          continue st'

        -- Vty.EvKey (Vty.KPageDown) [] -> continue $ st{aScreen=scr{asState=moveSel h l}}
        -- Vty.EvKey (Vty.KPageUp) []   -> continue $ st{aScreen=scr{asState=moveSel (-h) l}}

        -- fall through to the list's event handler (handles up/down)
        ev                       -> do
                                     l' <- handleEvent ev l
                                     continue $ st{aScreen=scr{asState=l'}}
                                 -- continue =<< handleEventLensed st someLens ev
handleAccountsScreen _ _ = error "event handler called with wrong screen type, should not happen"

-- | Toggle between flat and tree mode. If in the third "default" mode, go to flat mode.
toggleFlatMode :: ReportOpts -> ReportOpts
toggleFlatMode ropts@ReportOpts{accountlistmode_=ALFlat} = ropts{accountlistmode_=ALTree}
toggleFlatMode ropts = ropts{accountlistmode_=ALFlat}

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

