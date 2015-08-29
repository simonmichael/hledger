-- The account register screen, showing transactions in an account, like hledger-web's register.

{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

module Hledger.UI.RegisterScreen
 (screen)
where

import Control.Lens ((^.))
-- import Control.Monad.IO.Class (liftIO)
import Data.List
import Data.List.Split (splitOn)
import Data.Monoid
-- import Data.Maybe
import Data.Time.Calendar (Day)
import qualified Data.Vector as V
import Graphics.Vty as Vty
import Brick
import Brick.Widgets.List
-- import Brick.Widgets.Border
-- import Brick.Widgets.Border.Style
-- import Brick.Widgets.Center
-- import Text.Printf

import Hledger
import Hledger.Cli hiding (progname,prognameandversion,green)
import Hledger.UI.UIOptions
-- import Hledger.UI.Theme
import Hledger.UI.UITypes
import Hledger.UI.UIUtils

screen = RegisterScreen{
   rsState  = list "register" V.empty 1
  ,rsAcct   = ""
  ,sInitFn    = initRegisterScreen
  ,sDrawFn    = drawRegisterScreen
  ,sHandleFn = handleRegisterScreen
  }

initRegisterScreen :: Day -> AppState -> AppState
initRegisterScreen d st@AppState{aopts=opts, ajournal=j, aScreen=s@RegisterScreen{rsAcct=acct}} =
  st{aScreen=s{rsState=l}}
  where
    -- gather arguments and queries
    ropts = (reportopts_ $ cliopts_ opts)
            {
              depth_=Nothing,
              balancetype_=HistoricalBalance
            }
    -- XXX temp
    thisacctq = Acct $ accountNameToAccountRegex acct -- includes subs
    q = filterQuery (not . queryIsDepth) $ queryFromOpts d ropts
         -- query_="cur:\\$"} -- XXX limit to one commodity to ensure one-line items
         --{query_=unwords' $ locArgs l}

    -- run a transactions report, most recent last
    q' =
      -- ltrace "q"
      q
    thisacctq' =
      -- ltrace "thisacctq"
      thisacctq
    (_label,items') = accountTransactionsReport ropts j q' thisacctq'
    items = reverse items'

    -- pre-render all items; these will be the List elements. This helps calculate column widths.
    displayitem (_, t, _issplit, otheracctsstr, change, bal) =
      (showDate $ tdate t
      ,tdescription t
      ,case splitOn ", " otheracctsstr of
        [s] -> s
        ss  -> intercalate ", " ss
        -- _   -> "<split>"  -- should do this if accounts field width < 30
      ,showMixedAmountOneLineWithoutPrice change
      ,showMixedAmountOneLineWithoutPrice bal
      )
    displayitems = map displayitem items

    -- build the List, moving the selection to the end
    l = listMoveTo (length items) $
        list (Name "register") (V.fromList displayitems) 1

        -- (listName someList)

initRegisterScreen _ _ = error "init function called with wrong screen type, should not happen"

drawRegisterScreen :: AppState -> [Widget]
drawRegisterScreen AppState{ -- aopts=_uopts@UIOpts{cliopts_=_copts@CliOpts{reportopts_=_ropts@ReportOpts{query_=querystr}}},
                             aScreen=RegisterScreen{rsState=l,rsAcct=acct}} = [ui]
  where
    toplabel = withAttr ("border" <> "bold") (str acct)
            <+> str " transactions"
            -- <+> borderQueryStr querystr -- no, account transactions report shows all transactions in the acct ?
            -- <+> str " and subs"
            <+> str " ("
            <+> cur
            <+> str " of "
            <+> total
            <+> str ")"
    cur = str $ case l^.listSelectedL of
                 Nothing -> "-"
                 Just i -> show (i + 1)
    total = str $ show $ length displayitems
    displayitems = V.toList $ l^.listElementsL

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
        maxchangewidthseen = maximum' $ map (length . fourth5) displayitems
        maxbalwidthseen = maximum' $ map (length . fifth5) displayitems
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
        -- descwidth' = maximum' $ map (length . second5) displayitems
        -- acctswidth' = maximum' $ map (length . third5) displayitems
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
           -- "up/down/pgup/pgdown/home/end: move"
           "left: return to accounts"
          ]

      render $ defaultLayout toplabel bottomlabel $ renderList l (drawRegisterItem colwidths)

drawRegisterScreen _ = error "draw function called with wrong screen type, should not happen"

drawRegisterItem :: (Int,Int,Int,Int,Int) -> Bool -> (String,String,String,String,String) -> Widget
drawRegisterItem (datewidth,descwidth,acctswidth,changewidth,balwidth) selected (date,desc,accts,change,bal) =
  Widget Greedy Fixed $ do
    render $
      str (padright datewidth $ elideRight datewidth date) <+>
      str "  " <+>
      str (padright descwidth $ elideRight descwidth desc) <+>
      str "  " <+>
      str (padright acctswidth $ elideLeft acctswidth $ accts) <+>
      str "   " <+>
      withAttr changeattr (str (padleft changewidth $ elideLeft changewidth change)) <+>
      str "   " <+>
      withAttr balattr (str (padleft balwidth $ elideLeft balwidth bal))
  where
    changeattr | '-' `elem` change = sel $ "list" <> "amount" <> "decrease"
               | otherwise         = sel $ "list" <> "amount" <> "increase"
    balattr    | '-' `elem` bal    = sel $ "list" <> "balance" <> "negative"
               | otherwise         = sel $ "list" <> "balance" <> "positive"
    sel | selected  = (<> "selected")
        | otherwise = id

handleRegisterScreen :: AppState -> Vty.Event -> EventM (Next AppState)
handleRegisterScreen st@AppState{aopts=_opts,aScreen=s@RegisterScreen{rsState=is}} e = do
  case e of
    Vty.EvKey Vty.KEsc []        -> halt st
    Vty.EvKey (Vty.KChar 'q') [] -> halt st
    Vty.EvKey (Vty.KLeft) []     -> continue $ popScreen st
    -- Vty.EvKey (Vty.KRight) []    -> error (show curItem) where curItem = listSelectedElement is
    -- fall through to the list's event handler (handles [pg]up/down)
    ev                       -> do
                                 is' <- handleEvent ev is
                                 continue st{aScreen=s{rsState=is'}}
                                 -- continue =<< handleEventLensed st someLens ev
handleRegisterScreen _ _ = error "event handler called with wrong screen type, should not happen"
