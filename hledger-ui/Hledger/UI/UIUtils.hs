{- | Rendering & misc. helpers. -}

{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Hledger.UI.UIUtils (
   borderDepthStr
  ,borderKeysStr
  ,borderKeysStr'
  ,borderPeriodStr
  ,borderQueryStr
  ,defaultLayout
  ,helpDialog
  ,helpHandle
  ,minibuffer
  ,moveDownEvents
  ,moveLeftEvents
  ,moveRightEvents
  ,moveUpEvents
  ,normaliseMovementKeys
  ,renderToggle
  ,renderToggle1
  ,replaceHiddenAccountsNameWith
  ,scrollSelectionToMiddle
  ,get'
  ,put'
  ,modify'
  ,suspend
  ,redraw
  ,reportSpecAddQuery
  ,reportSpecSetFutureAndForecast
  ,listScrollPushingSelection
  ,dlogUiTrace
  ,dlogUiTraceM
  ,uiDebugLevel
  ,uiNumBlankItems
  )
where

import Brick
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import Brick.Widgets.Dialog
import Brick.Widgets.Edit
import Brick.Widgets.List (List, listSelectedL, listNameL, listItemHeightL, listSelected, listMoveDown, listMoveUp, GenericList)
import Control.Monad.IO.Class
import Data.Bifunctor (second)
import Data.List
import qualified Data.Text as T
import Data.Time (addDays)
import Graphics.Vty
  (Event(..),Key(..),Modifier(..),Vty(..),Color,Attr,currentAttr,refresh, displayBounds
  -- ,Output(displayBounds,mkDisplayContext),DisplayContext(..)
  )
import Lens.Micro.Platform

import Hledger
import Hledger.Cli (CliOpts)
import Hledger.Cli.DocFiles
import Hledger.UI.UITypes


-- | On posix platforms, send the system STOP signal to suspend the
-- current program. On windows, does nothing.
#ifdef mingw32_HOST_OS
suspendSignal :: IO ()
suspendSignal = return ()
#else
import System.Posix.Signals
import Data.Vector (Vector)
suspendSignal :: IO ()
suspendSignal = raiseSignal sigSTOP
#endif

-- Debug logging for UI state changes.

get' = do
  x <- get
  dlogUiTraceM $ "getting state: " ++ (head $ lines $ pshow $ aScreen x)
  return x

put' x = do
  dlogUiTraceM $ "putting state: " ++ (head $ lines $ pshow $ aScreen x)
  put x

modify' f = do
  x <- get
  let x' = f x
  dlogUiTraceM $ "modifying state: " ++ (head $ lines $ pshow $ aScreen x')
  modify f

-- | On posix platforms, suspend the program using the STOP signal,
-- like control-z in bash, returning to the original shell prompt,
-- and when resumed, continue where we left off.
-- On windows, does nothing.
suspend :: Ord a => s -> EventM a s ()
suspend st = suspendAndResume $ suspendSignal >> return st

-- | Tell vty to redraw the whole screen.
redraw :: EventM a s ()
redraw = getVtyHandle >>= liftIO . refresh

-- | Wrap a widget in the default hledger-ui screen layout.
defaultLayout :: Widget Name -> Widget Name -> Widget Name -> Widget Name
defaultLayout toplabel bottomlabel =
  topBottomBorderWithLabels (str " "<+>toplabel<+>str " ") (str " "<+>bottomlabel<+>str " ") .
  margin 1 0 Nothing
  -- topBottomBorderWithLabel label .
  -- padLeftRight 1 -- XXX should reduce inner widget's width by 2, but doesn't
                    -- "the layout adjusts... if you use the core combinators"

-- | Draw the help dialog, called when help mode is active.
helpDialog :: CliOpts -> Widget Name
helpDialog _copts =
  Widget Fixed Fixed $ do
    c <- getContext
    render $
      withDefAttr (attrName "help") $
      renderDialog (dialog (Just "Help (LEFT/ESC/?/q to close help)") Nothing (c^.availWidthL)) $ -- (Just (0,[("ok",())]))
      padTop (Pad 0) $ padLeft (Pad 1) $ padRight (Pad 1) $
        vBox [
           hBox [
              padRight (Pad 1) $
                vBox [
                   withAttr (attrName "help" <> attrName "heading") $ str "Navigation"
                  ,renderKey ("UP/DOWN/PUP/PDN/HOME/END/k/j/C-p/C-n", "")
                  ,str "     move selection up/down"
                  ,renderKey ("RIGHT/l/C-f", "show txns, or txn detail")
                  ,renderKey ("LEFT/h/C-b ", "go back")
                  ,renderKey ("ESC ", "cancel, or reset app state")

                  ,str " "
                  ,withAttr (attrName "help" <> attrName "heading") $ str "Accounts screen"
                  ,renderKey ("1234567890-+ ", "set/adjust depth limit")
                  ,renderKey ("t ", "toggle accounts tree/list mode")
                  ,renderKey ("H ", "toggle historical balance/change")
                  ,str " "
                  ,withAttr (attrName "help" <> attrName "heading") $ str "Register screen"
                  ,renderKey ("t ", "toggle subaccount txns\n(and accounts tree/list mode)")
                  ,renderKey ("H ", "toggle historical/period total")
                  ,str " "
                  ,withAttr (attrName "help" <> attrName "heading") $ str "Help"
                  ,renderKey ("?    ", "toggle this help")
                  ,renderKey ("p/m/i", "while help is open:\nshow manual in pager/man/info")
                  ,str " "
                ]
             ,padLeft (Pad 1) $ padRight (Pad 0) $
                vBox [
                   withAttr (attrName "help" <> attrName "heading") $ str "Filtering"
                  ,renderKey ("/   ", "set a filter query")
                  ,renderKey ("F   ", "show future & periodic txns")
                  ,renderKey ("R   ", "show real/all postings")
                  ,renderKey ("z   ", "show nonzero/all amounts")
                  ,renderKey ("U/P/C ", "show unmarked/pending/cleared")
                  ,renderKey ("S-DOWN /S-UP  ", "shrink/grow period")
                  ,renderKey ("S-RIGHT/S-LEFT", "next/previous period")
                  ,renderKey ("T             ", "set period to today")
                  ,renderKey ("DEL ", "reset filters")
                  ,str " "
                  ,withAttr (attrName "help" <> attrName "heading") $ str "Other"
                  ,renderKey ("a   ", "add transaction (hledger add)")
                  ,renderKey ("A   ", "add transaction (hledger-iadd)")
                  ,renderKey ("B   ", "show amounts/costs")
                  ,renderKey ("E   ", "open editor")
                  ,renderKey ("I   ", "toggle balance assertions")
                  ,renderKey ("V   ", "show amounts/market values")
                  ,renderKey ("g   ", "reload data")
                  ,renderKey ("C-l ", "redraw & recenter")
                  ,renderKey ("C-z ", "suspend")
                  ,renderKey ("q   ", "quit")
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
    renderKey (key,desc) = withAttr (attrName "help" <> attrName "key") (str key) <+> str " " <+> str desc

-- | Event handler used when help mode is active.
-- May invoke $PAGER, less, man or info, which is likely to fail on MS Windows, TODO.
helpHandle :: BrickEvent Name AppEvent -> EventM Name UIState ()
helpHandle ev = do
  ui <- get
  let ui' = ui{aMode=Normal}
  case ev of
    VtyEvent e | e `elem` closeHelpEvents -> put' ui'
    VtyEvent (EvKey (KChar 'p') []) -> suspendAndResume (runPagerForTopic "hledger-ui" Nothing >> return ui')
    VtyEvent (EvKey (KChar 'm') []) -> suspendAndResume (runManForTopic   "hledger-ui" Nothing >> return ui')
    VtyEvent (EvKey (KChar 'i') []) -> suspendAndResume (runInfoForTopic  "hledger-ui" Nothing >> return ui')
    _ -> return ()
  where
    closeHelpEvents = moveLeftEvents ++ [EvKey KEsc [], EvKey (KChar '?') [], EvKey (KChar 'q') []]

-- | Draw the minibuffer with the given label.
minibuffer :: T.Text -> Editor String Name -> Widget Name
minibuffer string ed =
  forceAttr (attrName "border" <> attrName "minibuffer") $
  hBox [txt $ string <> ": ", renderEditor (str . unlines) True ed]

borderQueryStr :: String -> Widget Name
borderQueryStr ""  = str ""
borderQueryStr qry = str " matching " <+> withAttr (attrName "border" <> attrName "query") (str qry)

borderDepthStr :: Maybe Int -> Widget Name
borderDepthStr Nothing  = str ""
borderDepthStr (Just d) = str " to depth " <+> withAttr (attrName "border" <> attrName "query") (str $ show d)

borderPeriodStr :: String -> Period -> Widget Name
borderPeriodStr _           PeriodAll = str ""
borderPeriodStr preposition p         = str (" "++preposition++" ") <+> withAttr (attrName "border" <> attrName "query") (str . T.unpack $ showPeriod p)

borderKeysStr :: [(String,String)] -> Widget Name
borderKeysStr = borderKeysStr' . map (second str)

borderKeysStr' :: [(String,Widget Name)] -> Widget Name
borderKeysStr' keydescs =
  hBox $
  intersperse sep $
  [withAttr (attrName "border" <> attrName "key") (str keys) <+> str ":" <+> desc | (keys, desc) <- keydescs]
  where
    -- sep = str " | "
    sep = str " "

-- | Show both states of a toggle ("aaa/bbb"), highlighting the active one.
renderToggle :: Bool -> String -> String -> Widget Name
renderToggle isright l r =
  let bold = withAttr (attrName "border" <> attrName "selected") in
  if isright
  then str (l++"/") <+> bold (str r)
  else bold (str l) <+> str ("/"++r)

-- | Show a toggle's label, highlighted (bold) when the toggle is active.
renderToggle1 :: Bool -> String -> Widget Name
renderToggle1 isactive l =
  let bold = withAttr (attrName "border" <> attrName "selected") in
  if isactive
  then bold (str l)
  else str l

-- temporary shenanigans:

-- | Convert the special account name "*" (from balance report with depth limit 0) to something clearer.
replaceHiddenAccountsNameWith :: AccountName -> AccountName -> AccountName
replaceHiddenAccountsNameWith anew a | a == hiddenAccountsName = anew
                                     | a == "*"                = anew
                                     | otherwise               = a

hiddenAccountsName = "..." -- for now

-- generic

--topBottomBorderWithLabel :: Widget Name -> Widget Name -> Widget Name
--topBottomBorderWithLabel label = \wrapped ->
--  Widget Greedy Greedy $ do
--    c <- getContext
--    let (_w,h) = (c^.availWidthL, c^.availHeightL)
--        h' = h - 2
--        wrapped' = vLimit (h') wrapped
--        debugmsg =
--          ""
--          -- "  debug: "++show (_w,h')
--    render $
--      hBorderWithLabel (label <+> str debugmsg)
--      <=>
--      wrapped'
--      <=>
--      hBorder

topBottomBorderWithLabels :: Widget Name -> Widget Name -> Widget Name -> Widget Name
topBottomBorderWithLabels toplabel bottomlabel body =
  Widget Greedy Greedy $ do
    c <- getContext
    let (_w,h) = (c^.availWidthL, c^.availHeightL)
        h' = h - 2
        body' = vLimit (h') body
        debugmsg =
          ""
          -- "  debug: "++show (_w,h')
    render $
      hBorderWithLabel (withAttr (attrName "border") $ toplabel <+> str debugmsg)
      <=>
      body'
      <=>
      hBorderWithLabel (withAttr (attrName "border") bottomlabel)

---- XXX should be equivalent to the above, but isn't (page down goes offscreen)
--_topBottomBorderWithLabel :: Widget Name -> Widget Name -> Widget Name
--_topBottomBorderWithLabel label = \wrapped ->
-- let debugmsg = ""
-- in hBorderWithLabel (label <+> str debugmsg)
--    <=>
--    wrapped
--    <=>
--    hBorder

-- XXX superseded by pad, in theory
-- | Wrap a widget in a margin with the given horizontal and vertical
-- thickness, using the current background colour or the specified
-- colour.
-- XXX May disrupt border style of inner widgets.
-- XXX Should reduce the available size visible to inner widget, but doesn't seem to (cf rsDraw).
margin :: Int -> Int -> Maybe Color -> Widget Name -> Widget Name
margin h v mcolour w = Widget Greedy Greedy $ do
    ctx <- getContext
    let w' = vLimit (ctx^.availHeightL - v*2) $ hLimit (ctx^.availWidthL - h*2) w
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
withBorderAttr attr = updateAttrMap (applyAttrMappings [(attrName "border", attr)])

---- | Like brick's continue, but first run some action to modify brick's state.
---- This action does not affect the app state, but might eg adjust a widget's scroll position.
--continueWith :: EventM n () -> ui -> EventM n (Next ui)
--continueWith brickaction ui = brickaction >> continue ui

---- | Scroll a list's viewport so that the selected item is at the top
---- of the display area.
--scrollToTop :: List Name e -> EventM Name ()
--scrollToTop list = do
--  let vpname = list^.listNameL
--  setTop (viewportScroll vpname) 0

-- | Scroll a list's viewport so that the selected item is centered in the
-- middle of the display area.
scrollSelectionToMiddle :: List Name item -> EventM Name UIState ()
scrollSelectionToMiddle list = do
  case list^.listSelectedL of
    Nothing -> return ()
    Just selectedrow -> do
      Vty{outputIface} <- getVtyHandle
      pageheight <- dbg4 "pageheight" . snd <$> liftIO (displayBounds outputIface)
      let
        itemheight   = dbg4 "itemheight" $ list^.listItemHeightL
        itemsperpage = dbg4 "itemsperpage" $ pageheight `div` itemheight
        toprow       = dbg4 "toprow" $ max 0 (selectedrow - (itemsperpage `div` 2)) -- assuming ViewportScroll's row offset is measured in list items not screen rows
      setTop (viewportScroll $ list^.listNameL) toprow

--                 arrow keys       vi keys               emacs keys
moveUpEvents    = [EvKey KUp []   , EvKey (KChar 'k') [], EvKey (KChar 'p') [MCtrl]]
moveDownEvents  = [EvKey KDown [] , EvKey (KChar 'j') [], EvKey (KChar 'n') [MCtrl]]
moveLeftEvents  = [EvKey KLeft [] , EvKey (KChar 'h') [], EvKey (KChar 'b') [MCtrl]]
moveRightEvents = [EvKey KRight [], EvKey (KChar 'l') [], EvKey (KChar 'f') [MCtrl]]

normaliseMovementKeys ev
  | ev `elem` moveUpEvents    = EvKey KUp []
  | ev `elem` moveDownEvents  = EvKey KDown []
  | ev `elem` moveLeftEvents  = EvKey KLeft []
  | ev `elem` moveRightEvents = EvKey KRight []
  | otherwise = ev

-- | Restrict the ReportSpec's query by adding the given additional query.
reportSpecAddQuery :: Query -> ReportSpec -> ReportSpec
reportSpecAddQuery q rspec =
    rspec{_rsQuery=simplifyQuery $ And [_rsQuery rspec, q]}

-- | Update the ReportSpec's query to exclude future transactions (later than its "today" date)
-- and forecast transactions (generated by --forecast), if the given forecast DateSpan is Nothing,
-- and include them otherwise.
reportSpecSetFutureAndForecast :: Maybe DateSpan -> ReportSpec -> ReportSpec
reportSpecSetFutureAndForecast fcast rspec =
    rspec{_rsQuery=simplifyQuery $ And [_rsQuery rspec, periodq, excludeforecastq fcast]}
  where
    periodq = Date . periodAsDateSpan . period_ $ _rsReportOpts rspec
    -- Except in forecast mode, exclude future/forecast transactions.
    excludeforecastq (Just _) = Any
    excludeforecastq Nothing  =  -- not:date:tomorrow- not:tag:generated-transaction
      And [
         Not (Date $ DateSpan (Just $ addDays 1 $ _rsDay rspec) Nothing)
        ,Not generatedTransactionTag
      ]

-- Vertically scroll the named list's viewport with the given number of non-empty items
-- by the given positive or negative number of items (usually 1 or -1).
-- The selection will be moved when necessary to keep it visible and allow the scroll.
listScrollPushingSelection :: Name -> Int -> Int -> EventM Name (List Name item) (GenericList Name Vector item)
listScrollPushingSelection name listheight scrollamt = do
  list <- get
  viewportScroll name `vScrollBy` scrollamt
  mvp <- lookupViewport name
  case mvp of
    Just VP{_vpTop, _vpSize=(_,vpheight)} -> do
      let mselidx = listSelected list
      case mselidx of
        Just selidx -> return $ pushsel list
          where
            pushsel 
              | scrollamt > 0, selidx <= _vpTop                && selidx < (listheight-1) = listMoveDown
              | scrollamt < 0, selidx >= _vpTop + vpheight - 1 && selidx > 0              = listMoveUp
              | otherwise = id
        _ -> return list
    _ -> return list

-- | Log a string to ./debug.log before returning the second argument,
-- if the global debug level is at or above a standard hledger-ui debug level.
-- Uses unsafePerformIO.
dlogUiTrace :: String -> a -> a
dlogUiTrace = dlogTraceAt uiDebugLevel

-- | Like dlogUiTrace, but within the hledger-ui brick event handler monad.
dlogUiTraceM :: String -> EventM Name UIState ()
dlogUiTraceM s = dlogUiTrace s $ return ()

-- | Log hledger-ui events at this debug level.
uiDebugLevel :: Int
uiDebugLevel = 2

-- | How many blank items to add to lists to fill the full window height.
uiNumBlankItems :: Int
uiNumBlankItems
  -- | debugLevel >= uiDebugLevel = 0    -- suppress to improve debug output.
  -- | otherwise 
  = 100  -- 100 ought to be enough for anyone
