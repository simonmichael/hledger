-- The register screen, showing account postings, like the CLI register command.

{-# LANGUAGE OverloadedStrings #-}

module Hledger.UI.RegisterScreen
 (screen)
where

import Control.Lens ((^.))
import Data.List
import Data.Time.Calendar (Day)
import qualified Data.Vector as V
import qualified Graphics.Vty as Vty
import Brick
import Brick.Widgets.List
import Brick.Widgets.Border
import Brick.Widgets.Center

import Hledger
import Hledger.Cli hiding (progname,prognameandversion,green)
import Hledger.UI.Options
import Hledger.UI.UITypes
import Hledger.UI.UIUtils

screen = RegisterScreen{
   rsState  = list "register" V.empty 1
  ,sInitFn    = initRegisterScreen
  ,sDrawFn    = drawRegisterScreen
  ,sHandleFn = handleRegisterScreen
  }

initRegisterScreen :: Day -> [String] -> AppState -> AppState
initRegisterScreen d args st@AppState{aopts=opts, ajournal=j, aScreen=s@RegisterScreen{}} =
  st{aScreen=s{rsState=is'}}
  where
    is' =
      listMoveTo (length items) $
      list (Name "register") (V.fromList items) 1
    (_label,items) = postingsReport ropts q j
      where
        q = queryFromOpts d ropts
             -- query_="cur:\\$"} -- XXX limit to one commodity to ensure one-line items
             --{query_=unwords' $ locArgs l}
        ropts = (reportopts_ cliopts)
                {query_=unwords' args}
        cliopts = cliopts_ opts
initRegisterScreen _ _ _ = error "init function called with wrong screen type, should not happen"

drawRegisterScreen :: AppState -> [Widget]
drawRegisterScreen AppState{aopts=_opts, aScreen=RegisterScreen{rsState=is}} = [ui]
  where
    label = str "Posting "
            <+> cur
            <+> str " of "
            <+> total
            <+> str " in this account and subaccounts" -- " <+> str query <+> "and subaccounts"
    cur = str $ case is^.(listSelectedL) of
                 Nothing -> "-"
                 Just i -> show (i + 1)
    total = str $ show $ length $ is^.(listElementsL)
    -- query = query_ $ reportopts_ $ cliopts_ opts
    box = borderWithLabel label $
          -- hLimit 25 $
          -- vLimit 15 $
          renderList is drawRegisterItem
    ui = box
    _ui = vCenter $ vBox [ hCenter box
                          , str " "
                          , hCenter $ str "Press Esc to exit."
                          ]
drawRegisterScreen _ = error "draw function called with wrong screen type, should not happen"

drawRegisterItem :: Bool -> PostingsReportItem -> Widget
drawRegisterItem sel item =
  let selStr i = if sel
                 then withAttr customAttr (str $ showitem i)
                 else str $ showitem i
      showitem (_,_,_,p,b) =
        intercalate ", " $ map strip $ lines $ 
        postingsReportItemAsText defcliopts $
        mkpostingsReportItem True True PrimaryDate Nothing p b
      -- fmt = BottomAligned [
      --     FormatField False (Just 20) Nothing TotalField
      --   , FormatLiteral "  "
      --   , FormatField True (Just 2) Nothing DepthSpacerField
      --   , FormatField True Nothing Nothing AccountField
      --   ]
  in
   selStr item

handleRegisterScreen :: AppState -> Vty.Event -> EventM (Next AppState)
handleRegisterScreen st@AppState{aScreen=s@RegisterScreen{rsState=is}} e =
  case e of
    Vty.EvKey Vty.KEsc []        -> halt st
    Vty.EvKey (Vty.KChar 'q') [] -> halt st
    Vty.EvKey (Vty.KLeft) []     -> continue $ popScreen st
    -- Vty.EvKey (Vty.KRight) []    -> error (show curItem) where curItem = listSelectedElement is
    -- fall through to the list's event handler (handles [pg]up/down)
    ev                       -> do
                                 is' <- handleEvent ev is
                                 continue st{aScreen=s{rsState=is'}}
                                 -- continue =<< handleEventLensed st someLens ev
handleRegisterScreen _ _ = error "event handler called with wrong screen type, should not happen"
