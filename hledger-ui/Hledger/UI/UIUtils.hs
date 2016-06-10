{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Hledger.UI.UIUtils
--   (
--   pushScreen
--  ,popScreen
--  ,resetScreens
--  ,screenEnter
--  ,regenerateScreens
--  ,getViewportSize
--  -- ,margin
--  ,withBorderAttr
--  ,topBottomBorderWithLabel
--  ,topBottomBorderWithLabels
--  ,defaultLayout
--  ,borderQueryStr
--  ,borderDepthStr
--  ,borderKeysStr
--  ,minibuffer
--  --
--  ,stToggleCleared
--  ,stTogglePending
--  ,stToggleUncleared
--  ,stToggleEmpty
--  ,stToggleFlat
--  ,stToggleReal
--  ,stFilter
--  ,stResetFilter
--  ,stShowMinibuffer
--  ,stCloseMinibuffer
--  )
  where

import Lens.Micro.Platform ((^.))
-- import Control.Monad
-- import Control.Monad.IO.Class
-- import Data.Default
import Data.List
import Data.Monoid
import Data.Text.Zipper (gotoEOL)
import Data.Time.Calendar (Day)
import Brick
import Brick.Widgets.Dialog
-- import Brick.Widgets.List
import Brick.Widgets.Edit
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import Graphics.Vty as Vty

import Hledger
import Hledger.Cli.CliOptions
import Hledger.UI.UITypes
import Hledger.UI.UIOptions

-- | Toggle between showing only cleared items or all items.
stToggleCleared :: AppState -> AppState
stToggleCleared st@AppState{aopts=uopts@UIOpts{cliopts_=copts@CliOpts{reportopts_=ropts}}} =
  st{aopts=uopts{cliopts_=copts{reportopts_=toggleCleared ropts}}}
  where
    toggleCleared ropts = ropts{cleared_=not $ cleared_ ropts, uncleared_=False, pending_=False}

-- | Toggle between showing only pending items or all items.
stTogglePending :: AppState -> AppState
stTogglePending st@AppState{aopts=uopts@UIOpts{cliopts_=copts@CliOpts{reportopts_=ropts}}} =
  st{aopts=uopts{cliopts_=copts{reportopts_=togglePending ropts}}}
  where
    togglePending ropts = ropts{pending_=not $ pending_ ropts, uncleared_=False, cleared_=False}

-- | Toggle between showing only uncleared items or all items.
stToggleUncleared :: AppState -> AppState
stToggleUncleared st@AppState{aopts=uopts@UIOpts{cliopts_=copts@CliOpts{reportopts_=ropts}}} =
  st{aopts=uopts{cliopts_=copts{reportopts_=toggleUncleared ropts}}}
  where
    toggleUncleared ropts = ropts{uncleared_=not $ uncleared_ ropts, cleared_=False, pending_=False}

-- | Toggle between showing all and showing only nonempty (more precisely, nonzero) items.
stToggleEmpty :: AppState -> AppState
stToggleEmpty st@AppState{aopts=uopts@UIOpts{cliopts_=copts@CliOpts{reportopts_=ropts}}} =
  st{aopts=uopts{cliopts_=copts{reportopts_=toggleEmpty ropts}}}
  where
    toggleEmpty ropts = ropts{empty_=not $ empty_ ropts}

-- | Toggle between flat and tree mode. If in the third "default" mode, go to flat mode.
stToggleFlat :: AppState -> AppState
stToggleFlat st@AppState{aopts=uopts@UIOpts{cliopts_=copts@CliOpts{reportopts_=ropts}}} =
  st{aopts=uopts{cliopts_=copts{reportopts_=toggleFlatMode ropts}}}
  where
    toggleFlatMode ropts@ReportOpts{accountlistmode_=ALFlat} = ropts{accountlistmode_=ALTree}
    toggleFlatMode ropts = ropts{accountlistmode_=ALFlat}

-- | Toggle between showing all and showing only real (non-virtual) items.
stToggleReal :: AppState -> AppState
stToggleReal st@AppState{aopts=uopts@UIOpts{cliopts_=copts@CliOpts{reportopts_=ropts}}} =
  st{aopts=uopts{cliopts_=copts{reportopts_=toggleReal ropts}}}
  where
    toggleReal ropts = ropts{real_=not $ real_ ropts}

-- | Apply a new filter query.
stFilter :: String -> AppState -> AppState
stFilter s st@AppState{aopts=uopts@UIOpts{cliopts_=copts@CliOpts{reportopts_=ropts}}} =
  st{aopts=uopts{cliopts_=copts{reportopts_=ropts{query_=s}}}}

-- | Clear all filter queries/flags.
stResetFilter :: AppState -> AppState
stResetFilter st@AppState{aopts=uopts@UIOpts{cliopts_=copts@CliOpts{reportopts_=ropts}}} =
  st{aopts=uopts{cliopts_=copts{reportopts_=ropts{
     empty_=True
    ,cleared_=False
    ,pending_=False
    ,uncleared_=False
    ,real_=False
    ,query_=""
    }}}}

resetDepth :: AppState -> AppState
resetDepth st@AppState{aopts=uopts@UIOpts{cliopts_=copts@CliOpts{reportopts_=ropts}}} =
  st{aopts=uopts{cliopts_=copts{reportopts_=ropts{depth_=Nothing}}}}

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

-- | Set the current depth limit to the specified depth, or remove the depth limit.
-- Also remove the depth limit if the specified depth is greater than the current
-- maximum account depth. If the specified depth is negative, reset the depth limit
-- to whatever was specified at startup.
setDepth :: Maybe Int -> AppState -> AppState
setDepth mdepth st@AppState{aopts=uopts@UIOpts{cliopts_=copts@CliOpts{reportopts_=ropts}}}
  = st{aopts=uopts{cliopts_=copts{reportopts_=ropts{depth_=mdepth'}}}}
  where
    mdepth' = case mdepth of
                Nothing                   -> Nothing
                Just d | d < 0            -> depth_ ropts
                       | d >= maxDepth st -> Nothing
                       | otherwise        -> mdepth

-- | Open the minibuffer, setting its content to the current query with the cursor at the end.
stShowMinibuffer st = setMode (Minibuffer e) st
  where
    e = applyEdit gotoEOL $ editor "minibuffer" (str . unlines) (Just 1) oldq
    oldq = query_ $ reportopts_ $ cliopts_ $ aopts st

-- | Close the minibuffer, discarding any edit in progress.
stCloseMinibuffer = setMode Normal

setMode :: Mode -> AppState -> AppState
setMode m st = st{aMode=m}

-- | Regenerate the content for the current and previous screens, from a new journal and current date.
regenerateScreens :: Journal -> Day -> AppState -> AppState
regenerateScreens j d st@AppState{aScreen=s,aPrevScreens=ss} =
  -- XXX clumsy due to entanglement of AppState and Screen.
  -- sInit operates only on an appstate's current screen, so
  -- remove all the screens from the appstate and then add them back
  -- one at a time, regenerating as we go.
  let
    first:rest = reverse $ s:ss :: [Screen]
    st0 = st{ajournal=j, aScreen=first, aPrevScreens=[]} :: AppState
    st1 = (sInit first) d False st0 :: AppState
    st2 = foldl' (\st s -> (sInit s) d False $ pushScreen s st) st1 rest :: AppState
  in
    st2

pushScreen :: Screen -> AppState -> AppState
pushScreen scr st = st{aPrevScreens=(aScreen st:aPrevScreens st)
                      ,aScreen=scr
                      }

popScreen :: AppState -> AppState
popScreen st@AppState{aPrevScreens=s:ss} = st{aScreen=s, aPrevScreens=ss}
popScreen st = st

resetScreens :: Day -> AppState -> AppState
resetScreens d st@AppState{aScreen=s,aPrevScreens=ss} =
  (sInit topscreen) d True $ resetDepth $ stResetFilter $ stCloseMinibuffer st{aScreen=topscreen, aPrevScreens=[]}
  where
    topscreen = case ss of _:_ -> last ss
                           []  -> s

-- clearScreens :: AppState -> AppState
-- clearScreens st = st{aPrevScreens=[]}

-- | Enter a new screen, saving the old screen & state in the
-- navigation history and initialising the new screen's state.
screenEnter :: Day -> Screen -> AppState -> AppState
screenEnter d scr st = (sInit scr) d True $
                       pushScreen scr
                       st

-- | Draw the help dialog, called when help mode is active.
helpDialog =
  Widget Fixed Fixed $ do
    c <- getContext
    render $
      renderDialog (dialog "help" (Just "Help (h/ESC to close)") Nothing (c^.availWidthL - 2)) $ -- (Just (0,[("ok",())]))
      padTopBottom 1 $ padLeftRight 1 $
      hBox [
         (padLeftRight 1 $
           vBox [
             str "MISC"
            ,renderKey ("h", "toggle help")
            ,renderKey ("a", "add transaction")
            ,renderKey ("g", "reload data")
            ,renderKey ("q", "quit")
            ,str " "
            ,str "NAVIGATION"
            ,renderKey ("UP/DOWN/PGUP/PGDN/HOME/END", "")
            ,str "  move selection"
            ,renderKey ("RIGHT/ENTER", "drill down")
            ,renderKey ("LEFT", "previous screen")
            ,renderKey ("ESC", "cancel / reset to top")
            ]
        )
        ,(padLeftRight 1 $
           vBox [
             str "FILTERING"
            ,renderKey ("C", "toggle cleared filter")
            ,renderKey ("U", "toggle uncleared filter")
            ,renderKey ("R", "toggle real filter")
            ,renderKey ("E", "toggle nonzero filter")
            ,renderKey ("/", "set a filter query")
            ,renderKey ("DEL/BS", "remove filters")
            ,str " "
            ,str "accounts screen:"
            ,renderKey ("F", "toggle flat mode")
            ,renderKey ("-+0123456789", "")
            ,str "  adjust/set depth limit"
            ]
        )
      ]
  where
    renderKey (key,desc) = withAttr (borderAttr <> "keys") (str key) <+> str " " <+> str desc

-- | Event handler used when help mode is active.
helpHandle st ev =
  case ev of
    EvKey k [] | k `elem` [KEsc, KChar 'h'] -> continue $ setMode Normal st
    _ -> continue st

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

defaultLayout toplabel bottomlabel =
  topBottomBorderWithLabels (str " "<+>toplabel<+>str " ") (str " "<+>bottomlabel<+>str " ") .
  margin 1 0 Nothing
  -- topBottomBorderWithLabel2 label .
  -- padLeftRight 1 -- XXX should reduce inner widget's width by 2, but doesn't
                    -- "the layout adjusts... if you use the core combinators"

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

topBottomBorderWithLabels toplabel bottomlabel = \wrapped ->
  Widget Greedy Greedy $ do
    c <- getContext
    let (_w,h) = (c^.availWidthL, c^.availHeightL)
        h' = h - 2
        wrapped' = vLimit (h') wrapped
        debugmsg =
          ""
          -- "  debug: "++show (_w,h')
    render $
      hBorderWithLabel (toplabel <+> str debugmsg)
      <=>
      wrapped'
      <=>
      hBorderWithLabel bottomlabel

-- XXX should be equivalent to the above, but isn't (page down goes offscreen)
_topBottomBorderWithLabel2 label = \wrapped ->
 let debugmsg = ""
 in hBorderWithLabel (label <+> str debugmsg)
    <=>
    wrapped
    <=>
    hBorder

-- XXX superseded by pad, in theory
-- | Wrap a widget in a margin with the given horizontal and vertical
-- thickness, using the current background colour or the specified
-- colour.
-- XXX May disrupt border style of inner widgets.
-- XXX Should reduce the available size visible to inner widget, but doesn't seem to (cf rsDraw2).
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

borderQueryStr :: String -> Widget
borderQueryStr ""  = str ""
borderQueryStr qry = str " matching " <+> withAttr (borderAttr <> "query") (str qry)

borderDepthStr :: Maybe Int -> Widget
borderDepthStr Nothing  = str ""
borderDepthStr (Just d) = str " to " <+> withAttr (borderAttr <> "depth") (str $ "depth "++show d)

borderKeysStr :: [(String,String)] -> Widget
borderKeysStr keydescs =
  hBox $
  intersperse sep $
  [withAttr (borderAttr <> "keys") (str keys) <+> str ":" <+> str desc | (keys, desc) <- keydescs]
  where
    -- sep = str " | "
    sep = str " "

minibuffer :: Editor -> Widget
minibuffer ed =
  forceAttr (borderAttr <> "minibuffer") $
  hBox $
  [txt "filter: ", renderEditor ed]

