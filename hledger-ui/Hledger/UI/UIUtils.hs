{-# LANGUAGE OverloadedStrings #-}

module Hledger.UI.UIUtils (
  pushScreen
 ,popScreen
 ,screenEnter
 ,getViewportSize
 ,margin
 ,withBorderAttr
 ,topBottomBorderWithLabel
 ,defaultLayout
 ) where

import Control.Lens ((^.))
-- import Control.Monad
-- import Control.Monad.IO.Class
-- import Data.Default
-- import Data.Monoid              -- 
import Data.Time.Calendar (Day)
import Brick
-- import Brick.Widgets.List
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import Graphics.Vty as Vty

import Hledger.UI.UITypes
import Hledger.Utils (applyN)

pushScreen :: Screen -> AppState -> AppState
pushScreen scr st = st{aPrevScreens=(aScreen st:aPrevScreens st)
                      ,aScreen=scr
                      }

popScreen :: AppState -> AppState
popScreen st@AppState{aPrevScreens=s:ss} = st{aScreen=s, aPrevScreens=ss}
popScreen st = st

-- clearScreens :: AppState -> AppState
-- clearScreens st = st{aPrevScreens=[]}

-- | Enter a new screen, saving the old screen & state in the
-- navigation history and initialising the new screen's state.
-- Extra args can be passed to the new screen's init function,
-- these can be eg query arguments.
screenEnter :: Day -> [String] -> Screen -> AppState -> AppState
screenEnter d args scr st = (sInitFn scr) d args $
                            pushScreen scr
                            st

-- | In the EventM monad, get the named current viewport's width and height,
-- or (0,0) if the named viewport is not found.
getViewportSize :: Name -> EventM (Int,Int)
getViewportSize name = do
  mvp <- lookupViewport name
  let (w,h) = case mvp of
        Just vp -> vp ^. vpSize
        Nothing -> (0,0)
  -- liftIO $ putStrLn $ show (w,h)
  return (w,h)

defaultLayout label =
  topBottomBorderWithLabel label .
  margin 1 0 Nothing
  -- margin 1 0 (Just white)

topBottomBorderWithLabel label = \wrapped ->
  Widget Greedy Greedy $ do
    c <- getContext
    let (_w,h) = (c^.availWidthL, c^.availHeightL)
        h' = h - 2
        wrapped' = vLimit (h') wrapped
        debugmsg =
          ""
          -- "  debug: "++show (_w,h')
    render $
      hBorderWithLabel (label <+> str debugmsg)
      <=>
      wrapped'
      <=>
      hBorder

-- | Wrap a widget in a margin with the given horizontal and vertical
-- thickness, using the current background colour or the specified
-- colour. XXX May disrupt border style of inner widgets.
margin :: Int -> Int -> Maybe Color -> Widget -> Widget
margin h v mcolour = \w ->
  Widget Greedy Greedy $ do
    c <- getContext
    let w' = vLimit (c^.availHeightL - v*2) $ hLimit (c^.availWidthL - h*2) w
        attr = maybe currentAttr (\c -> c `on` c) mcolour
    render $
      withBorderAttr attr $
      withBorderStyle (borderStyleFromChar ' ') $
      applyN v (hBorder <=>) $
      applyN h (vBorder <+>) $
      applyN v (<=> hBorder) $
      applyN h (<+> vBorder) $
      w'

   -- withBorderAttr attr .
   -- withBorderStyle (borderStyleFromChar ' ') .
   -- applyN n border

withBorderAttr attr = updateAttrMap (applyAttrMappings [(borderAttr, attr)])

-- _ui = vCenter $ vBox [ hCenter box
--                       , str " "
--                       , hCenter $ str "Press Esc to exit."
--                       ]
