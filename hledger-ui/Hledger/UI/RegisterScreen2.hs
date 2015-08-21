-- The register screen, showing account postings, like the CLI register command.

{-# LANGUAGE OverloadedStrings #-}

module Hledger.UI.RegisterScreen2
 (screen)
where

import Control.Lens ((^.))
-- import Control.Monad.IO.Class (liftIO)
import Data.List
import Data.List.Split (splitOn)
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

screen = RegisterScreen2{
   rs2State  = list "register" V.empty 1
  ,rs2Size   = (0,0)
  ,sInitFn    = initRegisterScreen2
  ,sDrawFn    = drawRegisterScreen2
  ,sHandleFn = handleRegisterScreen2
  }

initRegisterScreen2 :: Day -> [String] -> AppState -> AppState
initRegisterScreen2 d args st@AppState{aopts=opts, ajournal=j, aScreen=s@RegisterScreen2{rs2Size=size}} =
  st{aScreen=s{rs2State=is'}}
  where
    is' =
      -- listMoveTo (length items) $
      list (Name "register") (V.fromList items') 1

    -- XXX temporary hack: include saved viewport size in list elements
    -- for element draw function
    items' = zip (repeat size) items
    (_label,items) = accountTransactionsReport ropts j thisacctq q
      where
        -- XXX temp
        curacct = drop 5 $ head args -- should be "acct:..." 
        thisacctq = Acct $ curacct -- XXX why is this excluding subs: accountNameToAccountRegex curacct

        q = queryFromOpts d ropts
             -- query_="cur:\\$"} -- XXX limit to one commodity to ensure one-line items
             --{query_=unwords' $ locArgs l}
        ropts = (reportopts_ cliopts)
                {query_=unwords' args}
        cliopts = cliopts_ opts
initRegisterScreen2 _ _ _ = error "init function called with wrong screen type, should not happen"

drawRegisterScreen2 :: AppState -> [Widget]
drawRegisterScreen2 AppState{aopts=_opts, aScreen=RegisterScreen2{rs2State=is}} = [ui]
  where
    label = str "Transaction "
            <+> cur
            <+> str " of "
            <+> total
            <+> str " to/from this account" -- " <+> str query <+> "and subaccounts"
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
drawRegisterScreen2 _ = error "draw function called with wrong screen type, should not happen"

drawRegisterItem :: Bool -> ((Int,Int), AccountTransactionsReportItem) -> Widget
drawRegisterItem sel ((w,_h),item) =

  -- (w,_) <- getViewportSize "register" -- getCurrentViewportSize
  -- st@AppState{aopts=opts} <- getAppState
  -- let opts' = opts{width_=Just $ show w}

  let selStr i = if sel
                 then withAttr customAttr (str $ showitem i)
                 else str $ showitem i
      showitem (_origt,t,split,acctsstr,postedamt,totalamt) =
        -- make a fake posting to render
        let p = nullposting{
                  pdate=Just $ tdate t
                 ,paccount=if split then intercalate ", " acctnames ++" (split)" else acctsstr
                    -- XXX elideAccountName doesn't elide combined split names well
                 ,pamount=postedamt
                 ,ptransaction=Just t
                 }
            acctnames = nub $ sort $ splitOn ", " acctsstr -- XXX
        in
         intercalate ", " $ map strip $ lines $ 
         postingsReportItemAsText defcliopts{width_=Just (show w)} $
         mkpostingsReportItem True True PrimaryDate Nothing p totalamt
      -- fmt = BottomAligned [
      --     FormatField False (Just 20) Nothing TotalField
      --   , FormatLiteral "  "
      --   , FormatField True (Just 2) Nothing DepthSpacerField
      --   , FormatField True Nothing Nothing AccountField
      --   ]
  in
   selStr item

handleRegisterScreen2 :: AppState -> Vty.Event -> EventM (Next AppState)
handleRegisterScreen2 st@AppState{aopts=_opts,aScreen=s@RegisterScreen2{rs2State=is}} e = do
  case e of
    Vty.EvKey Vty.KEsc []        -> halt st
    Vty.EvKey (Vty.KChar 'q') [] -> halt st
    Vty.EvKey (Vty.KLeft) []     -> continue $ popScreen st
    -- Vty.EvKey (Vty.KRight) []    -> error (show curItem) where curItem = listSelectedElement is
    -- fall through to the list's event handler (handles [pg]up/down)
    ev                       -> do
                                 is' <- handleEvent ev is
                                 continue st{aScreen=s{rs2State=is'}}
                                 -- continue =<< handleEventLensed st someLens ev
handleRegisterScreen2 _ _ = error "event handler called with wrong screen type, should not happen"
