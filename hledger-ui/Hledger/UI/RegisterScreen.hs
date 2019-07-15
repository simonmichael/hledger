-- The account register screen, showing transactions in an account, like hledger-web's register.

{-# LANGUAGE OverloadedStrings, FlexibleContexts, RecordWildCards #-}
{-# LANGUAGE CPP #-}

module Hledger.UI.RegisterScreen
 (registerScreen
 ,rsHandle
 ,rsSetAccount
 ,rsCenterAndContinue
 )
where

import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Data.List
import Data.List.Split (splitOn)
#if !(MIN_VERSION_base(4,11,0))
import Data.Monoid ((<>))
#endif
import Data.Maybe
import qualified Data.Text as T
import Data.Time.Calendar
import qualified Data.Vector as V
import Graphics.Vty (Event(..),Key(..),Modifier(..))
import Brick
import Brick.Widgets.List
  (handleListEvent, list, listElementsL, listMoveDown, listMoveTo, listNameL, listSelectedElement, listSelectedL, renderList)
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
rsInit d reset ui@UIState{aopts=uopts@UIOpts{cliopts_=CliOpts{reportopts_=ropts}}, ajournal=j, aScreen=s@RegisterScreen{..}} =
  ui{aScreen=s{rsList=newitems'}}
  where
    -- gather arguments and queries
    -- XXX temp
    inclusive = tree_ ropts || rsForceInclusive
    thisacctq = Acct $ (if inclusive then accountNameToAccountRegex else accountNameToAccountOnlyRegex) rsAccount
    ropts' = ropts{
               depth_=Nothing
              }
    pfq | presentorfuture_ uopts == PFFuture = Any
        | otherwise                          = Date $ DateSpan Nothing (Just $ addDays 1 d)
    q = And [queryFromOpts d ropts', pfq]
--    reportq = filterQuery (not . queryIsDepth) q

    (_label,items) = accountTransactionsReport ropts' j q thisacctq
    items' = (if empty_ ropts' then id else filter (not . isZeroMixedAmount . fifth6)) $  -- without --empty, exclude no-change txns
             reverse  -- most recent last
             items

    -- generate pre-rendered list items. This helps calculate column widths.
    displayitems = map displayitem items'
      where
        displayitem (t, _, _issplit, otheracctsstr, change, bal) =
          RegisterScreenItem{rsItemDate          = showDate $ transactionRegisterDate q thisacctq t
                            ,rsItemStatus        = tstatus t
                            ,rsItemDescription   = T.unpack $ tdescription t
                            ,rsItemOtherAccounts = case splitOn ", " otheracctsstr of
                                                     [s] -> s
                                                     ss  -> intercalate ", " ss
                                                     -- _   -> "<split>"  -- should do this if accounts field width < 30
                            ,rsItemChangeAmount  = showMixedAmountOneLineWithoutPrice change
                            ,rsItemBalanceAmount = showMixedAmountOneLineWithoutPrice bal
                            ,rsItemTransaction   = t
                            }
    -- blank items are added to allow more control of scroll position; we won't allow movement over these
    blankitems = replicate 100  -- 100 ought to be enough for anyone
          RegisterScreenItem{rsItemDate          = ""
                            ,rsItemStatus        = Unmarked
                            ,rsItemDescription   = ""
                            ,rsItemOtherAccounts = ""
                            ,rsItemChangeAmount  = ""
                            ,rsItemBalanceAmount = ""
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
        endidx = length displayitems - 1

rsInit _ _ _ = error "init function called with wrong screen type, should not happen"

rsDraw :: UIState -> [Widget Name]
rsDraw UIState{aopts=uopts@UIOpts{cliopts_=copts@CliOpts{reportopts_=ropts}}
              ,aScreen=RegisterScreen{..}
              ,aMode=mode
              } =
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
        maxchangewidthseen = maximum' $ map (strWidth . rsItemChangeAmount) displayitems
        maxbalwidthseen = maximum' $ map (strWidth . rsItemBalanceAmount) displayitems
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
        ishistorical = balancetype_ ropts == HistoricalBalance
        -- inclusive = tree_ ropts || rsForceInclusive

        toplabel =
              withAttr ("border" <> "bold") (str $ T.unpack $ replaceHiddenAccountsNameWith "All" rsAccount)
--           <+> withAttr ("border" <> "query") (str $ if inclusive then "" else " exclusive")
          <+> togglefilters
          <+> str " transactions"
          -- <+> str (if ishistorical then " historical total" else " period total")
          <+> borderQueryStr (query_ ropts)
          -- <+> str " and subs"
          <+> borderPeriodStr "in" (period_ ropts)
          <+> str " ("
          <+> cur
          <+> str "/"
          <+> total
          <+> str ")"
          <+> (if ignore_assertions_ $ inputopts_ copts then withAttr ("border" <> "query") (str " ignoring balance assertions") else str "")
          where
            togglefilters =
              case concat [
                   uiShowStatus copts $ statuses_ ropts
                  ,if real_ ropts then ["real"] else []
                  ,if empty_ ropts then [] else ["nonzero"]
                  ] of
                [] -> str ""
                fs -> withAttr ("border" <> "query") (str $ " " ++ intercalate ", " fs)
            cur = str $ case rsList ^. listSelectedL of
                         Nothing -> "-"
                         Just i -> show (i + 1)
            total = str $ show $ length nonblanks
            nonblanks = V.takeWhile (not . null . rsItemDate) $ rsList^.listElementsL

            -- query = query_ $ reportopts_ $ cliopts_ opts

        bottomlabel = case mode of
                        Minibuffer ed -> minibuffer ed
                        _             -> quickhelp
          where
            quickhelp = borderKeysStr' [
               ("?", str "help")
              ,("LEFT", str "back")
--              ,("RIGHT", str "transaction")
              ,("T", renderToggle (tree_ ropts) "flat(-subs)" "tree(+subs)") -- rsForceInclusive may override, but use tree_ to ensure a visible toggle effect
              ,("H", renderToggle (not ishistorical) "historical" "period")
              ,("F", renderToggle (presentorfuture_ uopts == PFFuture) "present" "future")
--               ,("a", "add")
--               ,("g", "reload")
--               ,("q", "quit")
              ]

rsDraw _ = error "draw function called with wrong screen type, should not happen"

rsDrawItem :: (Int,Int,Int,Int,Int) -> Bool -> RegisterScreenItem -> Widget Name
rsDrawItem (datewidth,descwidth,acctswidth,changewidth,balwidth) selected RegisterScreenItem{..} =
  Widget Greedy Fixed $ do
    render $
      str (fitString (Just datewidth) (Just datewidth) True True rsItemDate) <+>
      str " " <+>
      str (fitString (Just 1) (Just 1) True True (show rsItemStatus)) <+>
      str " " <+>
      str (fitString (Just descwidth) (Just descwidth) True True rsItemDescription) <+>
      str "  " <+>
      str (fitString (Just acctswidth) (Just acctswidth) True True rsItemOtherAccounts) <+>
      str "   " <+>
      withAttr changeattr (str (fitString (Just changewidth) (Just changewidth) True False rsItemChangeAmount)) <+>
      str "   " <+>
      withAttr balattr (str (fitString (Just balwidth) (Just balwidth) True False rsItemBalanceAmount))
  where
    changeattr | '-' `elem` rsItemChangeAmount = sel $ "list" <> "amount" <> "decrease"
               | otherwise                     = sel $ "list" <> "amount" <> "increase"
    balattr    | '-' `elem` rsItemBalanceAmount = sel $ "list" <> "balance" <> "negative"
               | otherwise                      = sel $ "list" <> "balance" <> "positive"
    sel | selected  = (<> "selected")
        | otherwise = id

rsHandle :: UIState -> BrickEvent Name AppEvent -> EventM Name (Next UIState)
rsHandle ui@UIState{
   aScreen=s@RegisterScreen{..}
  ,aopts=UIOpts{cliopts_=copts}
  ,ajournal=j
  ,aMode=mode
  } ev = do
  d <- liftIO getCurrentDay
  let
    journalspan = journalDateSpan False j
    nonblanks = V.takeWhile (not . null . rsItemDate) $ rsList^.listElementsL
    lastnonblankidx = max 0 (length nonblanks - 1)

  case mode of
    Minibuffer ed ->
      case ev of
        VtyEvent (EvKey KEsc   []) -> continue $ closeMinibuffer ui
        VtyEvent (EvKey KEnter []) -> continue $ regenerateScreens j d $ setFilter s $ closeMinibuffer ui
                            where s = chomp $ unlines $ map strip $ getEditContents ed
        VtyEvent (EvKey (KChar 'l') [MCtrl]) -> redraw ui
        VtyEvent (EvKey (KChar 'z') [MCtrl]) -> suspend ui
        VtyEvent ev              -> do ed' <- handleEditorEvent ev ed
                                       continue $ ui{aMode=Minibuffer ed'}
        AppEvent _        -> continue ui
        MouseDown _ _ _ _ -> continue ui
        MouseUp _ _ _     -> continue ui

    Help ->
      case ev of
        VtyEvent (EvKey (KChar 'q') []) -> halt ui
        VtyEvent (EvKey (KChar 'l') [MCtrl]) -> redraw ui
        VtyEvent (EvKey (KChar 'z') [MCtrl]) -> suspend ui
        _                    -> helpHandle ui ev

    Normal ->
      case ev of
        VtyEvent (EvKey (KChar 'q') []) -> halt ui
        VtyEvent (EvKey KEsc        []) -> continue $ resetScreens d ui
        VtyEvent (EvKey (KChar c)   []) | c `elem` ['?'] -> continue $ setMode Help ui
        AppEvent (DateChange old _) | isStandardPeriod p && p `periodContainsDate` old ->
          continue $ regenerateScreens j d $ setReportPeriod (DayPeriod d) ui
          where
            p = reportPeriod ui
        e | e `elem` [VtyEvent (EvKey (KChar 'g') []), AppEvent FileChange] ->
          liftIO (uiReloadJournal copts d ui) >>= continue
        VtyEvent (EvKey (KChar 'I') []) -> continue $ uiCheckBalanceAssertions d (toggleIgnoreBalanceAssertions ui)
        VtyEvent (EvKey (KChar 'a') []) -> suspendAndResume $ clearScreen >> setCursorPosition 0 0 >> add copts j >> uiReloadJournalIfChanged copts d j ui
        VtyEvent (EvKey (KChar 'A') []) -> suspendAndResume $ void (runIadd (journalFilePath j)) >> uiReloadJournalIfChanged copts d j ui
        VtyEvent (EvKey (KChar 't') [])    -> continue $ regenerateScreens j d $ setReportPeriod (DayPeriod d) ui
        VtyEvent (EvKey (KChar 'E') []) -> suspendAndResume $ void (runEditor pos f) >> uiReloadJournalIfChanged copts d j ui
          where
            (pos,f) = case listSelectedElement rsList of
                        Nothing -> (endPos, journalFilePath j)
                        Just (_, RegisterScreenItem{
                          rsItemTransaction=Transaction{tsourcepos=GenericSourcePos f l c}}) -> (Just (l, Just c),f)
                        Just (_, RegisterScreenItem{
                          rsItemTransaction=Transaction{tsourcepos=JournalSourcePos f (l,_)}}) -> (Just (l, Nothing),f)

        -- display mode/query toggles
        VtyEvent (EvKey (KChar 'H') []) -> rsCenterAndContinue $ regenerateScreens j d $ toggleHistorical ui
        VtyEvent (EvKey (KChar 'T') []) -> rsCenterAndContinue $ regenerateScreens j d $ toggleTree ui
        VtyEvent (EvKey (KChar 'Z') []) -> rsCenterAndContinue $ regenerateScreens j d $ toggleEmpty ui
        VtyEvent (EvKey (KChar 'R') []) -> rsCenterAndContinue $ regenerateScreens j d $ toggleReal ui
        VtyEvent (EvKey (KChar 'U') []) -> rsCenterAndContinue $ regenerateScreens j d $ toggleUnmarked ui
        VtyEvent (EvKey (KChar 'P') []) -> rsCenterAndContinue $ regenerateScreens j d $ togglePending ui
        VtyEvent (EvKey (KChar 'C') []) -> rsCenterAndContinue $ regenerateScreens j d $ toggleCleared ui
        VtyEvent (EvKey (KChar 'F') []) -> rsCenterAndContinue $ regenerateScreens j d $ toggleFuture ui

        VtyEvent (EvKey (KChar '/') []) -> continue $ regenerateScreens j d $ showMinibuffer ui
        VtyEvent (EvKey (KDown)     [MShift]) -> continue $ regenerateScreens j d $ shrinkReportPeriod d ui
        VtyEvent (EvKey (KUp)       [MShift]) -> continue $ regenerateScreens j d $ growReportPeriod d ui
        VtyEvent (EvKey (KRight)    [MShift]) -> continue $ regenerateScreens j d $ nextReportPeriod journalspan ui
        VtyEvent (EvKey (KLeft)     [MShift]) -> continue $ regenerateScreens j d $ previousReportPeriod journalspan ui
        VtyEvent (EvKey k           []) | k `elem` [KBS, KDel] -> (continue $ regenerateScreens j d $ resetFilter ui)
        VtyEvent e | e `elem` moveLeftEvents  -> continue $ popScreen ui
        VtyEvent (EvKey (KChar 'l') [MCtrl]) -> scrollSelectionToMiddle rsList >> redraw ui
        VtyEvent (EvKey (KChar 'z') [MCtrl]) -> suspend ui

        -- enter transaction screen for selected transaction
        VtyEvent e | e `elem` moveRightEvents -> do
          case listSelectedElement rsList of
            Just (_, RegisterScreenItem{rsItemTransaction=t}) ->
              let
                ts = map rsItemTransaction $ V.toList $ nonblanks
                numberedts = zip [1..] ts
                i = fromIntegral $ maybe 0 (+1) $ elemIndex t ts -- XXX
              in
                continue $ screenEnter d transactionScreen{tsTransaction=(i,t)
                                                          ,tsTransactions=numberedts
                                                          ,tsAccount=rsAccount} ui
            Nothing -> continue ui

        -- prevent moving down over blank padding items;
        -- instead scroll down by one, until maximally scrolled - shows the end has been reached
        VtyEvent e | e `elem` moveDownEvents, isBlankElement mnextelement -> do
          vScrollBy (viewportScroll $ rsList^.listNameL) 1
          continue ui
          where
            mnextelement = listSelectedElement $ listMoveDown rsList

        -- if page down or end leads to a blank padding item, stop at last non-blank
        VtyEvent e@(EvKey k           []) | k `elem` [KPageDown, KEnd] -> do
          list <- handleListEvent e rsList
          if isBlankElement $ listSelectedElement list
          then do
            let list' = listMoveTo lastnonblankidx list
            scrollSelectionToMiddle list'
            continue ui{aScreen=s{rsList=list'}}
          else
            continue ui{aScreen=s{rsList=list}}

        -- fall through to the list's event handler (handles other [pg]up/down events)
        VtyEvent ev -> do
          let ev' = normaliseMovementKeys ev
          newitems <- handleListEvent ev' rsList
          continue ui{aScreen=s{rsList=newitems}}

        AppEvent _        -> continue ui
        MouseDown _ _ _ _ -> continue ui
        MouseUp _ _ _     -> continue ui

rsHandle _ _ = error "event handler called with wrong screen type, should not happen"

isBlankElement mel = ((rsItemDate . snd) <$> mel) == Just ""

rsCenterAndContinue ui = do
  scrollSelectionToMiddle $ rsList $ aScreen ui
  continue ui
