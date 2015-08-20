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
-- import Data.Monoid              -- 
import Data.Time.Calendar (Day)
import qualified Data.Vector as V
import qualified Graphics.Vty as Vty
import Brick
import Brick.Widgets.List
import Brick.Widgets.Border
import Brick.Widgets.Center

import Hledger
import Hledger.Cli hiding (progname,prognameandversion,green)
-- import Hledger.Cli.Options (defaultBalanceLineFormat)
import Hledger.UI.Options
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
                {no_elide_=True}
                {query_=unwords' args}
        cliopts = cliopts_ opts
initAccountsScreen _ _ _ = error "init function called with wrong screen type, should not happen"

drawAccountsScreen :: AppState -> [Widget]
drawAccountsScreen st@AppState{aScreen=AccountsScreen{asState=is}} = [ui]
    where
      label = str "Account " <+> cur <+> str " of " <+> total
      cur = str (case is^.listSelectedL of
                  Nothing -> "-"
                  Just i -> show (i + 1))
      total = str $ show $ length $ is^.listElementsL
      box = borderWithLabel label $
            -- hLimit 25 $
            -- vLimit 15 $
            renderList is (drawAccountsItem fmt)
      ui = box
      _ui = vCenter $ vBox [ hCenter box
                            , str " "
                            , hCenter $ str "Press Esc to exit."
                            ]
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

drawAccountsScreen _ = error "draw function called with wrong screen type, should not happen"

drawAccountsItem :: StringFormat -> Bool -> BalanceReportItem -> Widget
drawAccountsItem fmt sel item =
    let selStr i = if sel
                   then withAttr customAttr (str $ showitem i)
                   else str $ showitem i
        showitem = intercalate "\n" . balanceReportItemAsText defreportopts fmt
    in
     selStr item

handleAccountsScreen :: AppState -> Vty.Event -> EventM (Next AppState)
handleAccountsScreen st@AppState{aScreen=scr@AccountsScreen{asState=is}} e = do
    d <- liftIO getCurrentDay
    -- c <- getContext
    -- let h = c^.availHeightL
    --     moveSel n l = listMoveBy n l
    case e of
        Vty.EvKey Vty.KEsc []        -> halt st
        Vty.EvKey (Vty.KChar 'q') [] -> halt st
        Vty.EvKey (Vty.KLeft) []     -> continue $ popScreen st
        Vty.EvKey (Vty.KRight) []    -> continue st'
          where
            st' = screenEnter d args RS2.screen st
            args = case listSelectedElement is of
                    Just (_, ((acct, _, _), _)) -> ["acct:"++accountNameToAccountRegex acct]
                    Nothing -> []

        -- Vty.EvKey (Vty.KPageDown) [] -> continue $ st{aScreen=scr{asState=moveSel h is}}
        -- Vty.EvKey (Vty.KPageUp) []   -> continue $ st{aScreen=scr{asState=moveSel (-h) is}}

        -- fall through to the list's event handler (handles up/down)
        ev                       -> do
                                     is' <- handleEvent ev is
                                     continue $ st{aScreen=scr{asState=is'}}
                                 -- continue =<< handleEventLensed st someLens ev
handleAccountsScreen _ _ = error "event handler called with wrong screen type, should not happen"
