-- The account register screen, showing transactions in an account, like hledger-web's register.

{-# LANGUAGE OverloadedStrings, FlexibleContexts, RecordWildCards #-}

module Hledger.UI.RegisterScreen
 (registerScreen
 ,rsSetAccount
 )
where

import Lens.Micro ((^.))
import Control.Monad.IO.Class (liftIO)
import Data.List
import Data.List.Split (splitOn)
import Data.Monoid
import Data.Maybe
-- import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Calendar (Day)
import qualified Data.Vector as V
import Graphics.Vty as Vty
import Brick
import Brick.Widgets.List
import Brick.Widgets.Edit
import Brick.Widgets.Border (borderAttr)
-- import Brick.Widgets.Center
-- import Text.Printf

import Hledger
import Hledger.Cli hiding (progname,prognameandversion,green)
import Hledger.UI.UIOptions
-- import Hledger.UI.Theme
import Hledger.UI.UITypes
import Hledger.UI.UIUtils
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

rsSetAccount a scr@RegisterScreen{} = scr{rsAccount=a}
rsSetAccount _ scr = scr

rsInit :: Day -> Bool -> AppState -> AppState
rsInit d reset st@AppState{aopts=opts, ajournal=j, aScreen=s@RegisterScreen{..}} =
  st{aScreen=s{rsList=newitems'}}
  where
    -- gather arguments and queries
    ropts = (reportopts_ $ cliopts_ opts)
            {
              depth_=Nothing,
              balancetype_=HistoricalBalance
            }
    -- XXX temp
    thisacctq = Acct $ accountNameToAccountRegex rsAccount -- includes subs
    q = filterQuery (not . queryIsDepth) $ queryFromOpts d ropts

    (_label,items) = accountTransactionsReport ropts j q thisacctq
    items' = (if empty_ ropts then id else filter (not . isZeroMixedAmount . fifth6)) $  -- without --empty, exclude no-change txns
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
                      (True, _)    -> 0
                      (_, Nothing) -> endidx
                      (_, Just (_,RegisterScreenItem{rsItemTransaction=Transaction{tindex=ti}}))
                                   -> fromMaybe endidx $ findIndex ((==ti) . tindex . rsItemTransaction) displayitems
        endidx = length displayitems

rsInit _ _ _ = error "init function called with wrong screen type, should not happen"

rsDraw :: AppState -> [Widget]
rsDraw AppState{aopts=UIOpts{cliopts_=CliOpts{reportopts_=ropts}}
                           ,aScreen=RegisterScreen{..}
                           ,aMinibuffer=mbuf}
  = [ui]
  where
    toplabel = withAttr ("border" <> "bold") (str $ T.unpack rsAccount)
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

    ui = Widget Greedy Greedy $ do

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

        bottomlabel = borderKeysStr [
           -- ("up/down/pgup/pgdown/home/end", "move")
           ("left", "back")
          ,("E", "nonzero?")
          ,("C", "cleared?")
          ,("U", "uncleared?")
          ,("R", "real?")
          ,("/", "filter")
          ,("DEL", "unfilter")
          ,("right/enter", "transaction")
          ,("ESC", "cancel/top")
          ,("g", "reload")
          ,("q", "quit")
          ]

        bottomarea = case mbuf of
                      Nothing  -> bottomlabel
                      Just ed  -> minibuffer ed

      render $ defaultLayout toplabel bottomarea $ renderList rsList (rsDrawItem colwidths)

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

rsHandle :: AppState -> Vty.Event -> EventM (Next AppState)
rsHandle st@AppState{
   aScreen=s@RegisterScreen{..}
  ,aopts=UIOpts{cliopts_=copts}
  ,ajournal=j
  ,aMinibuffer=mbuf
  } ev = do
  d <- liftIO getCurrentDay
  case mbuf of
    Nothing ->

      case ev of
        Vty.EvKey (Vty.KChar 'q') [] -> halt st
        Vty.EvKey Vty.KEsc   [] -> continue $ resetScreens d st
        Vty.EvKey (Vty.KChar 'g') [] -> liftIO (stReloadJournalIfChanged copts d j st) >>= continue
        Vty.EvKey (Vty.KChar 'E') [] -> scrollTop >> (continue $ regenerateScreens j d $ stToggleEmpty st)
        Vty.EvKey (Vty.KChar 'C') [] -> scrollTop >> (continue $ regenerateScreens j d $ stToggleCleared st)
        Vty.EvKey (Vty.KChar 'U') [] -> scrollTop >> (continue $ regenerateScreens j d $ stToggleUncleared st)
        Vty.EvKey (Vty.KChar 'R') [] -> scrollTop >> (continue $ regenerateScreens j d $ stToggleReal st)
        Vty.EvKey k [] | k `elem` [Vty.KChar '/'] -> (continue $ regenerateScreens j d $ stShowMinibuffer st)
        Vty.EvKey k [] | k `elem` [Vty.KBS, Vty.KDel] -> (continue $ regenerateScreens j d $ stResetFilter st)
        Vty.EvKey (Vty.KLeft)     [] -> continue $ popScreen st

        Vty.EvKey (k) [] | k `elem` [Vty.KRight, Vty.KEnter] -> do
          case listSelectedElement rsList of
            Just (_, RegisterScreenItem{rsItemTransaction=t}) ->
              let
                ts = map rsItemTransaction $ V.toList $ listElements rsList
                numberedts = zip [1..] ts
                i = fromIntegral $ maybe 0 (+1) $ elemIndex t ts -- XXX
              in
                continue $ screenEnter d transactionScreen{tsTransaction=(i,t)
                                                          ,tsTransactions=numberedts
                                                          ,tsAccount=rsAccount} st
            Nothing -> continue st

        -- fall through to the list's event handler (handles [pg]up/down)
        ev                       -> do
                                     newitems <- handleEvent ev rsList
                                     continue st{aScreen=s{rsList=newitems}}
                                     -- continue =<< handleEventLensed st someLens ev

    Just ed ->
        case ev of
            Vty.EvKey Vty.KEsc   [] -> continue $ stHideMinibuffer st
            Vty.EvKey Vty.KEnter [] -> continue $ regenerateScreens j d $ stFilter s $ stHideMinibuffer st
                                        where s = chomp $ unlines $ getEditContents ed
            ev                      -> do ed' <- handleEvent ev ed
                                          continue $ st{aMinibuffer=Just ed'}

  where
    -- Encourage a more stable scroll position when toggling list items (cf AccountsScreen.hs)
    scrollTop = vScrollToBeginning $ viewportScroll "register"

rsHandle _ _ = error "event handler called with wrong screen type, should not happen"

