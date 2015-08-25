-- The accounts screen, showing accounts and balances like the CLI balance command.

{-# LANGUAGE OverloadedStrings #-}

module Hledger.UI.AccountsScreen
 (screen)
where

import Control.Lens ((^.))
-- import Control.Monad
import Control.Monad.IO.Class
-- import Data.Default
import Data.List
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
-- import Hledger.Cli.Options (defaultBalanceLineFormat)
import Hledger.UI.Options
-- import Hledger.UI.Theme
import Hledger.UI.UITypes
import Hledger.UI.UIUtils
import qualified Hledger.UI.RegisterScreen2 as RS2 (screen)

screen = AccountsScreen{
   asState  = list "accounts" V.empty 1
  ,sInitFn    = initAccountsScreen
  ,sDrawFn    = drawAccountsScreen
  ,sHandleFn = handleAccountsScreen
  }

initAccountsScreen :: Day -> [String] -> AppState -> AppState
initAccountsScreen d args st@AppState{aopts=opts, ajournal=j, aScreen=s@AccountsScreen{}} =
  st{aScreen=s{asState=is'}}
   where
    is' = list (Name "accounts") (V.fromList items) 1
    (items,_total) = balanceReport ropts q j
      where
        q = queryFromOpts d ropts
             -- query_="cur:\\$"} -- XXX limit to one commodity to ensure one-line items
             --{query_=unwords' $ locArgs l}
        ropts = (reportopts_ cliopts)
                {
                  query_=unwords' args,
                  balancetype_=HistoricalBalance -- XXX balanceReport doesn't respect this yet
                }
        cliopts = cliopts_ opts
initAccountsScreen _ _ _ = error "init function called with wrong screen type, should not happen"

drawAccountsScreen :: AppState -> [Widget]
drawAccountsScreen st@AppState{aopts=uopts, ajournal=j, aScreen=AccountsScreen{asState=is}} = [ui]
    where
      label = str "Accounts in "
              <+> withAttr ("border" <> "bold") files
              <+> borderQuery querystr
              <+> str " ("
              <+> cur
              <+> str " of "
              <+> total
              <+> str ")"
      files = str $ case journalFilePaths j of
                     [] -> ""
                     [f] -> takeFileName f
                     [f,_] -> takeFileName f ++ " (& 1 included file)"
                     f:fs -> takeFileName f ++ " (& " ++ show (length fs) ++ " included files)"
      querystr = query_ $ reportopts_ $ cliopts_ uopts
      cur = str (case is^.listSelectedL of
                  Nothing -> "-"
                  Just i -> show (i + 1))
      total = str $ show $ length $ is^.listElementsL

      items = listElements is
      flat = flat_ $ reportopts_ $ cliopts_ $ aopts st
      acctcolwidth = maximum $
                      V.map
                       (\((full,short,indent),_) ->
                         if flat then length full else length short + indent*2)
                       items 
      fmt = OneLine [ -- use a one-line format, List elements must have equal height
               FormatField True (Just 2) Nothing DepthSpacerField
             , FormatField True (Just acctcolwidth) Nothing AccountField
             , FormatLiteral "  "
             , FormatField False (Just 40) Nothing TotalField
             ]

      ui = defaultLayout label $ renderList is (drawAccountsItem fmt)

drawAccountsScreen _ = error "draw function called with wrong screen type, should not happen"

drawAccountsItem :: StringFormat -> Bool -> BalanceReportItem -> Widget
drawAccountsItem fmt _sel item =
  Widget Greedy Fixed $ do
    -- c <- getContext
    let
      showitem = intercalate "\n" . balanceReportItemAsText defreportopts fmt
    render $ str $ showitem item

handleAccountsScreen :: AppState -> Vty.Event -> EventM (Next AppState)
handleAccountsScreen st@AppState{aargs=args, aScreen=scr@AccountsScreen{asState=is}} e = do
    d <- liftIO getCurrentDay
    -- c <- getContext
    -- let h = c^.availHeightL
    --     moveSel n l = listMoveBy n l
    case e of
        Vty.EvKey Vty.KEsc []        -> halt st
        Vty.EvKey (Vty.KChar 'q') [] -> halt st
        Vty.EvKey (Vty.KLeft) []     -> continue $ popScreen st
        Vty.EvKey (Vty.KRight) []    -> do
          let st' = screenEnter d args RS2.screen{rs2Acct=acct} st
          vScrollToBeginning $ viewportScroll "register"
          continue st'
          where
            acct = case listSelectedElement is of
                    Just (_, ((a, _, _), _)) -> a
                    Nothing -> ""

        -- Vty.EvKey (Vty.KPageDown) [] -> continue $ st{aScreen=scr{asState=moveSel h is}}
        -- Vty.EvKey (Vty.KPageUp) []   -> continue $ st{aScreen=scr{asState=moveSel (-h) is}}

        -- fall through to the list's event handler (handles up/down)
        ev                       -> do
                                     is' <- handleEvent ev is
                                     continue $ st{aScreen=scr{asState=is'}}
                                 -- continue =<< handleEventLensed st someLens ev
handleAccountsScreen _ _ = error "event handler called with wrong screen type, should not happen"
