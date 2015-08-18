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
import qualified Brick.Types as T
import qualified Brick.Main as M
-- import qualified Brick.AttrMap as A
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.List as L
-- import Brick.Util (fg, on)
import Brick.Widgets.Core

import Hledger
import Hledger.Cli hiding (progname,prognameandversion,green)
import Hledger.UI.Options
import Hledger.UI.UITypes
import Hledger.UI.UIUtils

screen = RegisterScreen{
   rsState  = L.list "register" V.empty
  ,sInitFn    = initRegisterScreen
  ,sDrawFn    = drawRegisterScreen
  ,sHandleFn = handleRegisterScreen
  }

initRegisterScreen :: Day -> [String] -> AppState -> AppState
initRegisterScreen d args st@AppState{aopts=opts, ajournal=j, aScreen=s@RegisterScreen{}} =
  st{aScreen=s{rsState=is'}}
  where
    is' =
      L.listMoveTo (length items) $
      L.list (T.Name "register") (V.fromList items)
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
    label = "Posting " <+> cur <+> " of " <+> total <+> " in this account and subaccounts" -- " <+> str query <+> "and subaccounts"
    cur = case is^.(L.listSelectedL) of
            Nothing -> "-"
            Just i -> str (show (i + 1))
    total = str $ show $ length $ is^.(L.listElementsL)
    -- query = query_ $ reportopts_ $ cliopts_ opts
    box = B.borderWithLabel label $
          -- hLimit 25 $
          -- vLimit 15 $
          L.renderList is drawRegisterItem 1
    ui = box
    _ui = C.vCenter $ vBox [ C.hCenter box
                          , " "
                          , C.hCenter "Press Esc to exit."
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

handleRegisterScreen :: AppState -> Vty.Event -> M.EventM (M.Next AppState)
handleRegisterScreen st@AppState{aScreen=s@RegisterScreen{rsState=is}} e =
  case e of
    Vty.EvKey Vty.KEsc []        -> M.halt st
    Vty.EvKey (Vty.KChar 'q') [] -> M.halt st
    Vty.EvKey (Vty.KLeft) []     -> M.continue $ popScreen st
    -- Vty.EvKey (Vty.KRight) []    -> error (show curItem) where curItem = L.listSelectedElement is
    -- fall through to the list's event handler (handles up/down)
    ev                       -> M.continue st{aScreen=s{rsState=T.handleEvent ev is}}
handleRegisterScreen _ _ = error "event handler called with wrong screen type, should not happen"
