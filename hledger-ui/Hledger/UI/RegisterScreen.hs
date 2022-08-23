-- The account register screen, showing transactions in an account, like hledger-web's register.

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}

module Hledger.UI.RegisterScreen
 (registerScreen
 ,rsHandle
 ,rsSetAccount
 ,rsCenterSelection
 )
where

import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Data.List
import Data.Maybe
import qualified Data.Text as T
import Data.Time.Calendar
import qualified Data.Vector as V
import Graphics.Vty (Event(..),Key(..),Modifier(..), Button (BLeft, BScrollDown, BScrollUp))
import Brick
import Brick.Widgets.List hiding (reverse)
import Brick.Widgets.Edit
import Lens.Micro.Platform
import Safe
import System.Console.ANSI


import Hledger
import Hledger.Cli hiding (progname,prognameandversion)
import Hledger.UI.UIOptions
-- import Hledger.UI.Theme
import Hledger.UI.UITypes
import Hledger.UI.UIState
import Hledger.UI.UIUtils
import Hledger.UI.Editor
import Hledger.UI.TransactionScreen
import Hledger.UI.ErrorScreen
import Data.Vector ((!?))

registerScreen :: Screen
registerScreen = RegisterScreen{
   sInit   = rsInit
  ,sDraw   = rsDraw
  ,sHandle = rsHandle
  ,rsList    = list RegisterList V.empty 1
  ,rsAccount = ""
  ,rsForceInclusive = False
  }

rsSetAccount :: AccountName -> Bool -> Screen -> Screen
rsSetAccount a forceinclusive scr@RegisterScreen{} =
  scr{rsAccount=replaceHiddenAccountsNameWith "*" a, rsForceInclusive=forceinclusive}
rsSetAccount _ _ scr = scr

rsInit :: Day -> Bool -> UIState -> UIState
rsInit d reset ui@UIState{aopts=_uopts@UIOpts{uoCliOpts=copts@CliOpts{reportspec_=rspec@ReportSpec{_rsReportOpts=ropts}}}, ajournal=j, aScreen=s@RegisterScreen{..}} =
  dlogUiTrace "rsInit 1" $
  ui{aScreen=s{rsList=newitems'}}
  where
    -- gather arguments and queries
    -- XXX temp
    inclusive = tree_ ropts || rsForceInclusive
    thisacctq = Acct $ (if inclusive then accountNameToAccountRegex else accountNameToAccountOnlyRegex) rsAccount

    -- adjust the report options and regenerate the ReportSpec, carefully as usual to avoid screwups (#1523)
    ropts' = ropts {
        -- ignore any depth limit, as in postingsReport; allows register's total to match accounts screen
        depth_=Nothing
        -- do not strip prices so we can toggle costs within the ui
      , show_costs_=True
      -- XXX aregister also has this, needed ?
        -- always show historical balance
      -- , balanceaccum_= Historical
      }
    wd = whichDate ropts'
    rspec' = reportSpecSetFutureAndForecast d (forecast_ $ inputopts_ copts) .
      either (error "rsInit: adjusting the query for register, should not have failed") id $ -- PARTIAL:
      updateReportSpec ropts' rspec{_rsDay=d}
    items = accountTransactionsReport rspec' j thisacctq
    items' = (if empty_ ropts then id else filter (not . mixedAmountLooksZero . fifth6)) $  -- without --empty, exclude no-change txns
             reverse  -- most recent last
             items

    -- generate pre-rendered list items. This helps calculate column widths.
    displayitems = map displayitem items'
      where
        displayitem (t, _, _issplit, otheracctsstr, change, bal) =
          RegisterScreenItem{rsItemDate          = showDate $ transactionRegisterDate wd (_rsQuery rspec') thisacctq t
                            ,rsItemStatus        = tstatus t
                            ,rsItemDescription   = tdescription t
                            ,rsItemOtherAccounts = otheracctsstr
                                                     -- _   -> "<split>"  -- should do this if accounts field width < 30
                            ,rsItemChangeAmount  = showamt change
                            ,rsItemBalanceAmount = showamt bal
                            ,rsItemTransaction   = t
                            }
            where showamt = showMixedAmountB oneLine{displayMaxWidth=Just 32}
    -- blank items are added to allow more control of scroll position; we won't allow movement over these.
    -- XXX Ugly. Changing to 0 helps when debugging.
    blankitems = replicate uiNumBlankItems
          RegisterScreenItem{rsItemDate          = ""
                            ,rsItemStatus        = Unmarked
                            ,rsItemDescription   = ""
                            ,rsItemOtherAccounts = ""
                            ,rsItemChangeAmount  = mempty
                            ,rsItemBalanceAmount = mempty
                            ,rsItemTransaction   = nulltransaction
                            }
    -- build the List
    newitems = list RegisterList (V.fromList $ displayitems ++ blankitems) 1

    -- decide which transaction is selected:
    -- if reset is true, the last (latest) transaction;
    -- otherwise, the previously selected transaction if possible;
    -- otherwise, the transaction nearest in date to it;
    -- or if there's several with the same date, the nearest in journal order;
    -- otherwise, the last (latest) transaction.
    newitems' = listMoveTo newselidx newitems
      where
        newselidx =
          case (reset, listSelectedElement rsList) of
            (True, _)    -> endidx
            (_, Nothing) -> endidx
            (_, Just (_, RegisterScreenItem{rsItemTransaction=Transaction{tindex=prevselidx, tdate=prevseld}})) ->
              headDef endidx $ catMaybes [
                 findIndex ((==prevselidx) . tindex . rsItemTransaction) displayitems
                ,findIndex ((==nearestidbydatethenid) . Just . tindex . rsItemTransaction) displayitems
                ]
              where
                nearestidbydatethenid = third3 <$> (headMay $ sort
                  [(abs $ diffDays (tdate t) prevseld, abs (tindex t - prevselidx), tindex t) | t <- ts])
                ts = map rsItemTransaction displayitems
        endidx = max 0 $ length displayitems - 1

rsInit _ _ _ = dlogUiTrace "rsInit 2" $ errorWrongScreenType "init function"  -- PARTIAL:

rsDraw :: UIState -> [Widget Name]
rsDraw UIState{aopts=_uopts@UIOpts{uoCliOpts=copts@CliOpts{reportspec_=rspec}}
              ,aScreen=RegisterScreen{..}
              ,aMode=mode
              } = dlogUiTrace "rsDraw 1" $
  case mode of
    Help       -> [helpDialog copts, maincontent]
    -- Minibuffer e -> [minibuffer e, maincontent]
    _          -> [maincontent]
  where
    displayitems = V.toList $ rsList ^. listElementsL
    maincontent = Widget Greedy Greedy $ do
      -- calculate column widths, based on current available width
      c <- getContext
      let
        totalwidth = c^.availWidthL
                     - 2 -- XXX due to margin ? shouldn't be necessary (cf UIUtils)
        -- the date column is fixed width
        datewidth = 10
        -- multi-commodity amounts rendered on one line can be
        -- arbitrarily wide.  Give the two amounts as much space as
        -- they need, while reserving a minimum of space for other
        -- columns and whitespace.  If they don't get all they need,
        -- allocate it to them proportionally to their maximum widths.
        whitespacewidth = 10 -- inter-column whitespace, fixed width
        minnonamtcolswidth = datewidth + 1 + 2 + 2 -- date column plus at least 1 for status and 2 for desc and accts
        maxamtswidth = max 0 (totalwidth - minnonamtcolswidth - whitespacewidth)
        maxchangewidthseen = maximum' $ map (wbWidth . rsItemChangeAmount) displayitems
        maxbalwidthseen = maximum' $ map (wbWidth . rsItemBalanceAmount) displayitems
        changewidthproportion = fromIntegral maxchangewidthseen / fromIntegral (maxchangewidthseen + maxbalwidthseen)
        maxchangewidth = round $ changewidthproportion * fromIntegral maxamtswidth
        maxbalwidth = maxamtswidth - maxchangewidth
        changewidth = min maxchangewidth maxchangewidthseen
        balwidth = min maxbalwidth maxbalwidthseen
        -- assign the remaining space to the description and accounts columns
        -- maxdescacctswidth = totalwidth - (whitespacewidth - 4) - changewidth - balwidth
        maxdescacctswidth =
          -- trace (show (totalwidth, datewidth, changewidth, balwidth, whitespacewidth)) $
          max 0 (totalwidth - datewidth - 1 - changewidth - balwidth - whitespacewidth)
        -- allocating proportionally.
        -- descwidth' = maximum' $ map (strWidth . second6) displayitems
        -- acctswidth' = maximum' $ map (strWidth . third6) displayitems
        -- descwidthproportion = (descwidth' + acctswidth') / descwidth'
        -- maxdescwidth = min (maxdescacctswidth - 7) (maxdescacctswidth / descwidthproportion)
        -- maxacctswidth = maxdescacctswidth - maxdescwidth
        -- descwidth = min maxdescwidth descwidth'
        -- acctswidth = min maxacctswidth acctswidth'
        -- allocating equally.
        descwidth = maxdescacctswidth `div` 2
        acctswidth = maxdescacctswidth - descwidth
        colwidths = (datewidth,descwidth,acctswidth,changewidth,balwidth)

      render $ defaultLayout toplabel bottomlabel $ renderList (rsDrawItem colwidths) True rsList

      where
        ropts = _rsReportOpts rspec
        ishistorical = balanceaccum_ ropts == Historical
        -- inclusive = tree_ ropts || rsForceInclusive

        toplabel =
              withAttr (attrName "border" <> attrName "bold") (str $ T.unpack $ replaceHiddenAccountsNameWith "All" rsAccount)
--           <+> withAttr ("border" <> "query") (str $ if inclusive then "" else " exclusive")
          <+> togglefilters
          <+> str " transactions"
          -- <+> str (if ishistorical then " historical total" else " period total")
          <+> borderQueryStr (T.unpack . T.unwords . map textQuoteIfNeeded $ querystring_ ropts)
          -- <+> str " and subs"
          <+> borderPeriodStr "in" (period_ ropts)
          <+> str " ("
          <+> cur
          <+> str "/"
          <+> total
          <+> str ")"
          <+> (if ignore_assertions_ . balancingopts_ $ inputopts_ copts then withAttr (attrName "border" <> attrName "query") (str " ignoring balance assertions") else str "")
          where
            togglefilters =
              case concat [
                   uiShowStatus copts $ statuses_ ropts
                  ,if real_ ropts then ["real"] else []
                  ,if empty_ ropts then [] else ["nonzero"]
                  ] of
                [] -> str ""
                fs -> withAttr (attrName "border" <> attrName "query") (str $ " " ++ intercalate ", " fs)
            cur = str $ case rsList ^. listSelectedL of
                         Nothing -> "-"
                         Just i -> show (i + 1)
            total = str $ show $ length nonblanks
            nonblanks = V.takeWhile (not . T.null . rsItemDate) $ rsList^.listElementsL

            -- query = query_ $ reportopts_ $ cliopts_ opts

        bottomlabel = case mode of
                        Minibuffer label ed -> minibuffer label ed
                        _                   -> quickhelp
          where
            quickhelp = borderKeysStr' [
               ("?", str "help")
              ,("LEFT", str "back")
--              ,("RIGHT", str "transaction")

              -- tree/list mode - rsForceInclusive may override, but use tree_ to ensure a visible toggle effect
              ,("t", renderToggle (tree_ ropts) "list(-subs)" "tree(+subs)")
              -- ,("t", str "tree(+subs)")
              -- ,("l", str "list(-subs)")

              ,("H", renderToggle (not ishistorical) "historical" "period")
              ,("F", renderToggle1 (isJust . forecast_ . inputopts_ $ copts) "forecast")
--               ,("a", "add")
--               ,("g", "reload")
--               ,("q", "quit")
              ]

rsDraw _ = dlogUiTrace "rsDraw 2" $ errorWrongScreenType "draw function"  -- PARTIAL:

rsDrawItem :: (Int,Int,Int,Int,Int) -> Bool -> RegisterScreenItem -> Widget Name
rsDrawItem (datewidth,descwidth,acctswidth,changewidth,balwidth) selected RegisterScreenItem{..} =
  Widget Greedy Fixed $ do
    render $
      txt (fitText (Just datewidth) (Just datewidth) True True rsItemDate) <+>
      txt " " <+>
      txt (fitText (Just 1) (Just 1) True True (T.pack $ show rsItemStatus)) <+>
      txt " " <+>
      txt (fitText (Just descwidth) (Just descwidth) True True rsItemDescription) <+>
      txt "  " <+>
      txt (fitText (Just acctswidth) (Just acctswidth) True True rsItemOtherAccounts) <+>
      txt "   " <+>
      withAttr changeattr (txt $ fitText (Just changewidth) (Just changewidth) True False changeAmt) <+>
      txt "   " <+>
      withAttr balattr (txt $ fitText (Just balwidth) (Just balwidth) True False balanceAmt)
  where
    changeAmt  = wbToText rsItemChangeAmount
    balanceAmt = wbToText rsItemBalanceAmount
    changeattr | T.any (=='-') changeAmt  = sel $ attrName "list" <> attrName "amount" <> attrName "decrease"
               | otherwise                = sel $ attrName "list" <> attrName "amount" <> attrName "increase"
    balattr    | T.any (=='-') balanceAmt = sel $ attrName "list" <> attrName "balance" <> attrName "negative"
               | otherwise                = sel $ attrName "list" <> attrName "balance" <> attrName "positive"
    sel | selected  = (<> attrName "selected")
        | otherwise = id

rsHandle :: BrickEvent Name AppEvent -> EventM Name UIState ()
rsHandle ev = do
  ui0 <- get'
  dlogUiTraceM "rsHandle 1"
  case ui0 of
    ui@UIState{
      aScreen=s@RegisterScreen{..}
      ,aopts=UIOpts{uoCliOpts=copts}
      ,ajournal=j
      ,aMode=mode
      } -> do
      let
        d = copts^.rsDay
        journalspan = journalDateSpan False j
        nonblanks = V.takeWhile (not . T.null . rsItemDate) $ rsList^.listElementsL
        lastnonblankidx = max 0 (length nonblanks - 1)

      case mode of
        Minibuffer _ ed ->
          case ev of
            VtyEvent (EvKey KEsc   []) -> modify' closeMinibuffer
            VtyEvent (EvKey KEnter []) -> put' $ regenerateScreens j d $
                case setFilter s $ closeMinibuffer ui of
                  Left bad -> showMinibuffer "Cannot compile regular expression" (Just bad) ui
                  Right ui' -> ui'
              where s = chomp . unlines . map strip $ getEditContents ed
            -- VtyEvent (EvKey (KChar '/') []) -> put' $ regenerateScreens j d $ showMinibuffer ui
            VtyEvent (EvKey (KChar 'l') [MCtrl]) -> redraw
            VtyEvent (EvKey (KChar 'z') [MCtrl]) -> suspend ui
            VtyEvent ev -> do
              ed' <- nestEventM' ed $ handleEditorEvent (VtyEvent ev)
              put' ui{aMode=Minibuffer "filter" ed'}
            AppEvent _  -> return ()
            MouseDown{} -> return ()
            MouseUp{}   -> return ()

        Help ->
          case ev of
            -- VtyEvent (EvKey (KChar 'q') []) -> halt
            VtyEvent (EvKey (KChar 'l') [MCtrl]) -> redraw
            VtyEvent (EvKey (KChar 'z') [MCtrl]) -> suspend ui
            _ -> helpHandle ev

        Normal ->
          case ev of
            VtyEvent (EvKey (KChar 'q') []) -> halt
            VtyEvent (EvKey KEsc        []) -> put' $ resetScreens d ui
            VtyEvent (EvKey (KChar c)   []) | c == '?' -> put' $ setMode Help ui
            AppEvent (DateChange old _) | isStandardPeriod p && p `periodContainsDate` old ->
              put' $ regenerateScreens j d $ setReportPeriod (DayPeriod d) ui
              where
                p = reportPeriod ui
            e | e `elem` [VtyEvent (EvKey (KChar 'g') []), AppEvent FileChange] ->
              liftIO (uiReloadJournal copts d ui) >>= put'
            VtyEvent (EvKey (KChar 'I') []) -> put' $ uiCheckBalanceAssertions d (toggleIgnoreBalanceAssertions ui)
            VtyEvent (EvKey (KChar 'a') []) -> suspendAndResume $ clearScreen >> setCursorPosition 0 0 >> add copts j >> uiReloadJournalIfChanged copts d j ui
            VtyEvent (EvKey (KChar 'A') []) -> suspendAndResume $ void (runIadd (journalFilePath j)) >> uiReloadJournalIfChanged copts d j ui
            VtyEvent (EvKey (KChar 'T') []) -> put' $ regenerateScreens j d $ setReportPeriod (DayPeriod d) ui
            VtyEvent (EvKey (KChar 'E') []) -> suspendAndResume $ void (runEditor pos f) >> uiReloadJournalIfChanged copts d j ui
              where
                (pos,f) = case listSelectedElement rsList of
                            Nothing -> (endPosition, journalFilePath j)
                            Just (_, RegisterScreenItem{
                              rsItemTransaction=Transaction{tsourcepos=(SourcePos f l c,_)}}) -> (Just (unPos l, Just $ unPos c),f)

            -- display mode/query toggles
            VtyEvent (EvKey (KChar 'B') []) -> rsCenterSelection (regenerateScreens j d $ toggleConversionOp ui) >>= put'
            VtyEvent (EvKey (KChar 'V') []) -> rsCenterSelection (regenerateScreens j d $ toggleValue ui) >>= put'
            VtyEvent (EvKey (KChar 'H') []) -> rsCenterSelection (regenerateScreens j d $ toggleHistorical ui) >>= put'
            VtyEvent (EvKey (KChar 't') []) -> rsCenterSelection (regenerateScreens j d $ toggleTree ui) >>= put'
            VtyEvent (EvKey (KChar c) []) | c `elem` ['z','Z'] -> rsCenterSelection (regenerateScreens j d $ toggleEmpty ui) >>= put'
            VtyEvent (EvKey (KChar 'R') []) -> rsCenterSelection (regenerateScreens j d $ toggleReal ui) >>= put'
            VtyEvent (EvKey (KChar 'U') []) -> rsCenterSelection (regenerateScreens j d $ toggleUnmarked ui) >>= put'
            VtyEvent (EvKey (KChar 'P') []) -> rsCenterSelection (regenerateScreens j d $ togglePending ui) >>= put'
            VtyEvent (EvKey (KChar 'C') []) -> rsCenterSelection (regenerateScreens j d $ toggleCleared ui) >>= put'
            VtyEvent (EvKey (KChar 'F') []) -> rsCenterSelection (regenerateScreens j d $ toggleForecast d ui) >>= put'

            VtyEvent (EvKey (KChar '/') []) -> put' $ regenerateScreens j d $ showMinibuffer "filter" Nothing ui
            VtyEvent (EvKey (KDown)     [MShift]) -> put' $ regenerateScreens j d $ shrinkReportPeriod d ui
            VtyEvent (EvKey (KUp)       [MShift]) -> put' $ regenerateScreens j d $ growReportPeriod d ui
            VtyEvent (EvKey (KRight)    [MShift]) -> put' $ regenerateScreens j d $ nextReportPeriod journalspan ui
            VtyEvent (EvKey (KLeft)     [MShift]) -> put' $ regenerateScreens j d $ previousReportPeriod journalspan ui
            VtyEvent (EvKey k           []) | k `elem` [KBS, KDel] -> (put' $ regenerateScreens j d $ resetFilter ui)
            VtyEvent (EvKey (KChar 'l') [MCtrl]) -> scrollSelectionToMiddle rsList >> redraw
            VtyEvent (EvKey (KChar 'z') [MCtrl]) -> suspend ui

            -- exit screen on LEFT
            VtyEvent e | e `elem` moveLeftEvents  -> put' $ popScreen ui
            -- or on a click in the app's left margin. This is a VtyEvent since not in a clickable widget.
            VtyEvent (EvMouseUp x _y (Just BLeft)) | x==0 -> put' $ popScreen ui
            -- or on clicking a blank list item.
            MouseUp _ (Just BLeft) Location{loc=(_,y)} | clickeddate == "" -> put' $ popScreen ui
              where clickeddate = maybe "" rsItemDate $ listElements rsList !? y

            -- enter transaction screen on RIGHT
            VtyEvent e | e `elem` moveRightEvents ->
              case listSelectedElement rsList of
                Just _  -> put' $ screenEnter d transactionScreen{tsAccount=rsAccount} ui
                Nothing -> put' ui
            -- or on transaction click
            -- MouseDown is sometimes duplicated, https://github.com/jtdaugherty/brick/issues/347
            -- just use it to move the selection
            MouseDown _n BLeft _mods Location{loc=(_x,y)} | not $ (=="") clickeddate -> do
              put' $ ui{aScreen=s{rsList=listMoveTo y rsList}}
              where clickeddate = maybe "" rsItemDate $ listElements rsList !? y
            -- and on MouseUp, enter the subscreen
            MouseUp _n (Just BLeft) Location{loc=(_x,y)} | not $ (=="") clickeddate -> do
              put' $ screenEnter d transactionScreen{tsAccount=rsAccount} ui
              where clickeddate = maybe "" rsItemDate $ listElements rsList !? y

            -- when selection is at the last item, DOWN scrolls instead of moving, until maximally scrolled
            VtyEvent e | e `elem` moveDownEvents, isBlankElement mnextelement -> do
              vScrollBy (viewportScroll $ rsList ^. listNameL) 1
              where mnextelement = listSelectedElement $ listMoveDown rsList

            -- mouse scroll wheel scrolls the viewport up or down to its maximum extent,
            -- pushing the selection when necessary.
            MouseDown name btn _mods _loc | btn `elem` [BScrollUp, BScrollDown] -> do
              let scrollamt = if btn==BScrollUp then -1 else 1
              list' <- nestEventM' rsList $ listScrollPushingSelection name (rsListSize rsList) scrollamt
              put' ui{aScreen=s{rsList=list'}}

            -- if page down or end leads to a blank padding item, stop at last non-blank
            VtyEvent e@(EvKey k           []) | k `elem` [KPageDown, KEnd] -> do
              list <- nestEventM' rsList $ handleListEvent e
              if isBlankElement $ listSelectedElement list
              then do
                let list' = listMoveTo lastnonblankidx list
                scrollSelectionToMiddle list'
                put' ui{aScreen=s{rsList=list'}}
              else
                put' ui{aScreen=s{rsList=list}}

            -- fall through to the list's event handler (handles other [pg]up/down events)
            VtyEvent ev -> do
              let ev' = normaliseMovementKeys ev
              newitems <- nestEventM' rsList $ handleListEvent ev'
              put' ui{aScreen=s{rsList=newitems}}

            MouseDown{}       -> return ()
            MouseUp{}         -> return ()
            AppEvent _        -> return ()

    _ -> dlogUiTrace "rsHandle 2" $ errorWrongScreenType "event handler"

isBlankElement mel = ((rsItemDate . snd) <$> mel) == Just ""

rsCenterSelection :: UIState -> EventM Name UIState UIState
rsCenterSelection ui = do
  scrollSelectionToMiddle $ rsList $ aScreen ui
  return ui  -- ui is unchanged, but this makes the function more chainable

rsListSize = V.length . V.takeWhile ((/="").rsItemDate) . listElements
