-- The account register screen, showing transactions in an account, like hledger-web's register.

{-# LANGUAGE OverloadedStrings, FlexibleContexts, RecordWildCards #-}

module Hledger.UI.RegisterScreen
 (registerScreen
 ,rsSetAccount
 )
where

import Lens.Micro.Platform ((^.))
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Data.List
import Data.List.Split (splitOn)
import Data.Monoid
import Data.Maybe
import qualified Data.Text as T
import Data.Time.Calendar (Day)
import qualified Data.Vector as V
import Graphics.Vty (Event(..),Key(..))
import Brick
import Brick.Widgets.List
import Brick.Widgets.Edit
import Brick.Widgets.Border (borderAttr)
import System.Console.ANSI


import Hledger
import Hledger.Cli hiding (progname,prognameandversion,green)
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
  ,rsList    = list "register" V.empty 1
  ,rsAccount = ""
  }

rsSetAccount a scr@RegisterScreen{} = scr{rsAccount=replaceHiddenAccountsNameWith "*" a}
rsSetAccount _ scr = scr

rsInit :: Day -> Bool -> UIState -> UIState
rsInit d reset ui@UIState{aopts=UIOpts{cliopts_=CliOpts{reportopts_=ropts}}, ajournal=j, aScreen=s@RegisterScreen{..}} =
  ui{aScreen=s{rsList=newitems'}}
  where
    -- gather arguments and queries
    ropts' = ropts{
               depth_=Nothing
              ,balancetype_=HistoricalBalance
              }
    -- XXX temp
    thisacctq = Acct $ (if flat_ ropts then accountNameToAccountOnlyRegex else accountNameToAccountRegex) rsAccount
    q = filterQuery (not . queryIsDepth) $ queryFromOpts d ropts'

    (_label,items) = accountTransactionsReport ropts' j q thisacctq
    items' = (if empty_ ropts' then id else filter (not . isZeroMixedAmount . fifth6)) $  -- without --empty, exclude no-change txns
             reverse  -- most recent last
             items

    -- generate pre-rendered list items. This helps calculate column widths.
    displayitems = map displayitem items'
      where
        displayitem (t, _, _issplit, otheracctsstr, change, bal) =
          RegisterScreenItem{rsItemDate          = showDate $ tdate t
                            ,rsItemDescription   = T.unpack $ tdescription t
                            ,rsItemOtherAccounts = case splitOn ", " otheracctsstr of
                                                     [s] -> s
                                                     ss  -> intercalate ", " ss
                                                     -- _   -> "<split>"  -- should do this if accounts field width < 30
                            ,rsItemChangeAmount  = showMixedAmountOneLineWithoutPrice change
                            ,rsItemBalanceAmount = showMixedAmountOneLineWithoutPrice bal
                            ,rsItemTransaction   = t
                            }

    -- build the List
    newitems = list (Name "register") (V.fromList displayitems) 1

    -- keep the selection on the previously selected transaction if possible,
    -- (eg after toggling nonzero mode), otherwise select the last element.
    newitems' = listMoveTo newselidx newitems
      where
        newselidx = case (reset, listSelectedElement rsList) of
                      (True, _)    -> endidx
                      (_, Nothing) -> endidx
                      (_, Just (_,RegisterScreenItem{rsItemTransaction=Transaction{tindex=ti}}))
                                   -> fromMaybe endidx $ findIndex ((==ti) . tindex . rsItemTransaction) displayitems
        endidx = length displayitems

rsInit _ _ _ = error "init function called with wrong screen type, should not happen"

rsDraw :: UIState -> [Widget]
rsDraw UIState{aopts=UIOpts{cliopts_=CliOpts{reportopts_=ropts}}
                            ,aScreen=RegisterScreen{..}
                            ,aMode=mode
                            } =
  case mode of
    Help       -> [helpDialog, maincontent]
    -- Minibuffer e -> [minibuffer e, maincontent]
    _          -> [maincontent]
  where
    toplabel =
          withAttr ("border" <> "bold") (str $ T.unpack $ replaceHiddenAccountsNameWith "All" rsAccount)
      <+> withAttr (borderAttr <> "query") (str $ if flat_ ropts then " (exclusive)" else "")
      <+> togglefilters
      <+> str " transactions"
      <+> borderQueryStr (query_ ropts)
      -- <+> str " and subs"
      <+> str " ("
      <+> cur
      <+> str "/"
      <+> total
      <+> str ")"
    togglefilters =
      case concat [
           if cleared_ ropts then ["cleared"] else []
          ,if uncleared_ ropts then ["uncleared"] else []
          ,if pending_ ropts then ["pending"] else []
          ,if real_ ropts then ["real"] else []
          ,if empty_ ropts then [] else ["nonzero"]
          ] of
        [] -> str ""
        fs -> withAttr (borderAttr <> "query") (str $ " " ++ intercalate ", " fs)
    cur = str $ case rsList ^. listSelectedL of
                 Nothing -> "-"
                 Just i -> show (i + 1)
    total = str $ show $ length displayitems
    displayitems = V.toList $ rsList ^. listElementsL

    -- query = query_ $ reportopts_ $ cliopts_ opts

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
        minnonamtcolswidth = datewidth + 2 + 2 -- date column plus at least 2 for desc and accts
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
          max 0 (totalwidth - datewidth - changewidth - balwidth - whitespacewidth)
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

      render $ defaultLayout toplabel bottomlabel $ renderList rsList (rsDrawItem colwidths)

      where
        bottomlabel = case mode of
                        Minibuffer ed -> minibuffer ed
                        _             -> quickhelp
        quickhelp = borderKeysStr [
           ("?", "help")
          ,("left", "back")
          ,("right", "transaction")
          ,("/", "filter")
          ,("DEL", "unfilter")
          --,("ESC", "reset")
          ,("a", "add")
          ,("g", "reload")
          ,("q", "quit")
          ]

rsDraw _ = error "draw function called with wrong screen type, should not happen"

rsDrawItem :: (Int,Int,Int,Int,Int) -> Bool -> RegisterScreenItem -> Widget
rsDrawItem (datewidth,descwidth,acctswidth,changewidth,balwidth) selected RegisterScreenItem{..} =
  Widget Greedy Fixed $ do
    render $
      str (fitString (Just datewidth) (Just datewidth) True True rsItemDate) <+>
      str "  " <+>
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

rsHandle :: UIState -> Event -> EventM (Next UIState)
rsHandle ui@UIState{
   aScreen=s@RegisterScreen{..}
  ,aopts=UIOpts{cliopts_=copts}
  ,ajournal=j
  ,aMode=mode
  } ev = do
  d <- liftIO getCurrentDay

  case mode of
    Minibuffer ed ->
      case ev of
        EvKey KEsc   [] -> continue $ closeMinibuffer ui
        EvKey KEnter [] -> continue $ regenerateScreens j d $ setFilter s $ closeMinibuffer ui
                            where s = chomp $ unlines $ getEditContents ed
        ev              -> do ed' <- handleEvent ev ed
                              continue $ ui{aMode=Minibuffer ed'}

    Help ->
      case ev of
        EvKey (KChar 'q') [] -> halt ui
        _                    -> helpHandle ui ev

    Normal ->
      case ev of
        EvKey (KChar 'q') [] -> halt ui
        EvKey KEsc        [] -> continue $ resetScreens d ui
        EvKey (KChar c)   [] | c `elem` ['?'] -> continue $ setMode Help ui
        EvKey (KChar 'g') [] -> liftIO (uiReloadJournalIfChanged copts d j ui) >>= continue
        EvKey (KChar 'a') [] -> suspendAndResume $ clearScreen >> setCursorPosition 0 0 >> add copts j >> uiReloadJournalIfChanged copts d j ui
        EvKey (KChar 'E') [] -> suspendAndResume $ void (runEditor pos f) >> uiReloadJournalIfChanged copts d j ui
          where
            (pos,f) = case listSelectedElement rsList of
                        Nothing -> (endPos, journalFilePath j)
                        Just (_, RegisterScreenItem{rsItemTransaction=Transaction{tsourcepos=GenericSourcePos f l c}}) -> (Just (l, Just c),f)
        EvKey (KChar 'F') [] -> scrollTop >> (continue $ regenerateScreens j d $ toggleFlat ui)
        EvKey (KChar 'Z') [] -> scrollTop >> (continue $ regenerateScreens j d $ toggleEmpty ui)
        EvKey (KChar 'C') [] -> scrollTop >> (continue $ regenerateScreens j d $ toggleCleared ui)
        EvKey (KChar 'U') [] -> scrollTop >> (continue $ regenerateScreens j d $ toggleUncleared ui)
        EvKey (KChar 'R') [] -> scrollTop >> (continue $ regenerateScreens j d $ toggleReal ui)
        EvKey (KChar '/') [] -> (continue $ regenerateScreens j d $ showMinibuffer ui)
        EvKey k           [] | k `elem` [KBS, KDel] -> (continue $ regenerateScreens j d $ resetFilter ui)
        EvKey k           [] | k `elem` [KLeft, KChar 'h'] -> continue $ popScreen ui
        EvKey k           [] | k `elem` [KRight, KChar 'l', KEnter] -> do
          case listSelectedElement rsList of
            Just (_, RegisterScreenItem{rsItemTransaction=t}) ->
              let
                ts = map rsItemTransaction $ V.toList $ listElements rsList
                numberedts = zip [1..] ts
                i = fromIntegral $ maybe 0 (+1) $ elemIndex t ts -- XXX
              in
                continue $ screenEnter d transactionScreen{tsTransaction=(i,t)
                                                          ,tsTransactions=numberedts
                                                          ,tsAccount=rsAccount} ui
            Nothing -> continue ui
        -- fall through to the list's event handler (handles [pg]up/down)
        ev -> do
                let ev' = case ev of
                            EvKey (KChar 'k') [] -> EvKey (KUp) []
                            EvKey (KChar 'j') [] -> EvKey (KDown) []
                            _                    -> ev
                newitems <- handleEvent ev' rsList
                continue ui{aScreen=s{rsList=newitems}}
                -- continue =<< handleEventLensed ui someLens ev
      where
        -- Encourage a more stable scroll position when toggling list items (cf AccountsScreen.hs)
        scrollTop = vScrollToBeginning $ viewportScroll "register"

rsHandle _ _ = error "event handler called with wrong screen type, should not happen"
