{-# LANGUAGE OverloadedStrings #-}

module Hledger.UI.UIUtils (
  pushScreen
 ,popScreen
 ,screenEnter
 ,attrMap
 ,customAttrMap
 ,customAttr
 ,getViewportSize
 ) where

import Control.Lens ((^.))
-- import Control.Monad
import Control.Monad.IO.Class
-- import Data.Default
import Data.Monoid              -- 
import Data.Time.Calendar (Day)
import qualified Graphics.Vty as V
import Brick
import Brick.Widgets.List

import Hledger.UI.UITypes

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

customAttrMap :: AttrMap
customAttrMap = attrMap V.defAttr
    [ (listAttr,            V.white `on` V.blue)
    , (listSelectedAttr,    V.black `on` V.white)
    -- , (customAttr,            fg V.cyan)
    ]

customAttr :: AttrName

-- | In the EventM monad, get the named current viewport's width and height,
-- or (0,0) if the named viewport is not found.
getViewportSize :: Name -> EventM (Int,Int)
getViewportSize name = do
  mvp <- lookupViewport name
  let (w,h) = case mvp of
        Just vp -> vp ^. vpSize
        Nothing -> (0,0)
  return (w,h)

customAttr = listSelectedAttr <> "custom"
