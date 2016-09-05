{- | Rendering & misc. helpers. -}

{-# LANGUAGE OverloadedStrings #-}

module Hledger.UI.UIUtils
where

import Brick
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
-- import Brick.Widgets.Center
import Brick.Widgets.Dialog
import Brick.Widgets.Edit
import Data.List
import Data.Monoid
import Graphics.Vty (Event(..),Key(..),Color,Attr,currentAttr)
import Lens.Micro.Platform
import System.Process

import Hledger
import Hledger.UI.UITypes
import Hledger.UI.UIState


runInfo = runCommand "hledger-ui --info" >>= waitForProcess
runMan  = runCommand "hledger-ui --man" >>= waitForProcess
runHelp = runCommand "hledger-ui --help | less" >>= waitForProcess

-- ui

uiShowClearedStatus mc =
 case mc of
   Just Cleared   -> ["cleared"]
   Just Pending   -> ["pending"]
   Just Uncleared -> ["uncleared"]
   Nothing        -> []

-- | Draw the help dialog, called when help mode is active.
helpDialog :: Widget Name
helpDialog =
  Widget Fixed Fixed $ do
    c <- getContext
    render $
      renderDialog (dialog (Just "Help (?/LEFT/ESC to close)") Nothing (c^.availWidthL)) $ -- (Just (0,[("ok",())]))
      padTopBottom 1 $ padLeftRight 1 $
        vBox [
           hBox [
              padLeftRight 1 $
                vBox [
                   str "NAVIGATION"
                  ,renderKey ("UP/DOWN/k/j/PGUP/PGDN/HOME/END", "")
                  ,str "  move selection"
                  ,renderKey ("RIGHT/l", "more detail")
                  ,renderKey ("LEFT/h", "previous screen")
                  ,renderKey ("ESC", "cancel / reset")
                  ,str " "
                  ,str "MISC"
                  ,renderKey ("?", "toggle help")
                  ,renderKey ("a", "add transaction")
                  ,renderKey ("E", "open editor")
                  ,renderKey ("g", "reload data")
                  ,renderKey ("I", "toggle balance assertions")
                  ,renderKey ("q", "quit")
                  ,str " "
                  ,str "MANUAL"
                  ,str "from help dialog:"
                  ,renderKey ("t", "text")
                  ,renderKey ("m", "man page")
                  ,renderKey ("i", "info")
                ]
             ,padLeftRight 1 $
                vBox [
                   str "FILTERING"
                  ,renderKey ("SHIFT-DOWN/UP", "shrink/grow report period")
                  ,renderKey ("SHIFT-RIGHT/LEFT", "next/previous report period")
                  ,renderKey ("t", "set report period to today")
                  ,str " "
                  ,renderKey ("/", "set a filter query")
                  ,renderKey ("C", "toggle cleared/all")
                  ,renderKey ("U", "toggle uncleared/all")
                  ,renderKey ("R", "toggle real/all")
                  ,renderKey ("Z", "toggle nonzero/all")
                  ,renderKey ("DEL/BS", "remove filters")
                  ,str " "
                  ,str "accounts screen:"
                  ,renderKey ("-+0123456789", "set depth limit")
                  ,renderKey ("H", "toggle period balance (shows change) or\nhistorical balance (includes older postings)")
                  ,renderKey ("F", "toggle tree (amounts include subaccounts) or\nflat mode (amounts exclude subaccounts\nexcept when account is depth-clipped)")
                  ,str " "
                  ,str "register screen:"
                  ,renderKey ("H", "toggle period or historical total")
                  ,renderKey ("F", "toggle subaccount transaction inclusion\n(and tree/flat mode)")
                ]
             ]
--           ,vBox [
--              str " "
--             ,hCenter $ padLeftRight 1 $
--               hCenter (str "MANUAL")
--               <=>
--               hCenter (hBox [
--                  renderKey ("t", "text")
--                 ,str " "
--                 ,renderKey ("m", "man page")
--                 ,str " "
--                 ,renderKey ("i", "info")
--                 ])
--             ]
          ]
  where
    renderKey (key,desc) = withAttr (borderAttr <> "keys") (str key) <+> str " " <+> str desc

-- | Event handler used when help mode is active.
helpHandle :: UIState -> Event -> EventM Name (Next UIState)
helpHandle ui ev =
  case ev of
    EvKey k [] | k `elem` [KEsc, KLeft, KChar 'h', KChar '?'] -> continue $ setMode Normal ui
    EvKey (KChar 't') [] -> suspendAndResume $ runHelp >> return ui'
    EvKey (KChar 'm') [] -> suspendAndResume $ runMan  >> return ui'
    EvKey (KChar 'i') [] -> suspendAndResume $ runInfo >> return ui'
    _ -> continue ui
  where
    ui' = setMode Normal ui

-- | Draw the minibuffer.
minibuffer :: Editor Name -> Widget Name
minibuffer ed =
  forceAttr (borderAttr <> "minibuffer") $
  hBox $
  [txt "filter: ", renderEditor True ed]

-- | Wrap a widget in the default hledger-ui screen layout.
defaultLayout :: Widget Name -> Widget Name -> Widget Name -> Widget Name
defaultLayout toplabel bottomlabel =
  topBottomBorderWithLabels (str " "<+>toplabel<+>str " ") (str " "<+>bottomlabel<+>str " ") .
  margin 1 0 Nothing
  -- topBottomBorderWithLabel2 label .
  -- padLeftRight 1 -- XXX should reduce inner widget's width by 2, but doesn't
                    -- "the layout adjusts... if you use the core combinators"

borderQueryStr :: String -> Widget Name
borderQueryStr ""  = str ""
borderQueryStr qry = str " matching " <+> withAttr (borderAttr <> "query") (str qry)

borderDepthStr :: Maybe Int -> Widget Name
borderDepthStr Nothing  = str ""
borderDepthStr (Just d) = str " to " <+> withAttr (borderAttr <> "query") (str $ "depth "++show d)

borderPeriodStr :: String -> Period -> Widget Name
borderPeriodStr _           PeriodAll = str ""
borderPeriodStr preposition p         = str (" "++preposition++" ") <+> withAttr (borderAttr <> "query") (str $ showPeriod p)

borderKeysStr :: [(String,String)] -> Widget Name
borderKeysStr = borderKeysStr' . map (\(a,b) -> (a, str b))

borderKeysStr' :: [(String,Widget Name)] -> Widget Name
borderKeysStr' keydescs =
  hBox $
  intersperse sep $
  [withAttr (borderAttr <> "keys") (str keys) <+> str ":" <+> desc | (keys, desc) <- keydescs]
  where
    -- sep = str " | "
    sep = str " "

-- temporary shenanigans:

-- | Convert the special account name "*" (from balance report with depth limit 0) to something clearer.
replaceHiddenAccountsNameWith :: AccountName -> AccountName -> AccountName
replaceHiddenAccountsNameWith anew a | a == hiddenAccountsName = anew
                                     | a == "*"                = anew
                                     | otherwise               = a

hiddenAccountsName = "..." -- for now

-- generic

topBottomBorderWithLabel :: Widget Name -> Widget Name -> Widget Name
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

topBottomBorderWithLabels :: Widget Name -> Widget Name -> Widget Name -> Widget Name
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
_topBottomBorderWithLabel2 :: Widget Name -> Widget Name -> Widget Name
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
margin :: Int -> Int -> Maybe Color -> Widget Name -> Widget Name
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

withBorderAttr :: Attr -> Widget Name -> Widget Name
withBorderAttr attr = updateAttrMap (applyAttrMappings [(borderAttr, attr)])

