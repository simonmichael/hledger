-- The register screen, showing account postings, like the CLI register command.

{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

module Hledger.UI.RegisterScreen2
 (screen)
where

import Control.Lens ((^.))
-- import Control.Monad.IO.Class (liftIO)
-- import Data.List
import Data.List.Split (splitOn)
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
import Hledger.UI.Options
-- import Hledger.UI.Theme
import Hledger.UI.UITypes
import Hledger.UI.UIUtils

screen = RegisterScreen2{
   rs2State  = list "register" V.empty 1
  ,sInitFn    = initRegisterScreen2
  ,sDrawFn    = drawRegisterScreen2
  ,sHandleFn = handleRegisterScreen2
  }

initRegisterScreen2 :: Day -> [String] -> AppState -> AppState
initRegisterScreen2 d args st@AppState{aopts=opts, ajournal=j, aScreen=s@RegisterScreen2{}} =
  st{aScreen=s{rs2State=l}}
  where
    -- gather arguments and queries
    ropts = (reportopts_ $ cliopts_ opts)
            {
              query_=unwords' args,
              balancetype_=HistoricalBalance
            }
    -- XXX temp
    curacct = drop 5 $ head args -- should be "acct:..." 
    thisacctq = Acct $ curacct -- XXX why is this excluding subs: accountNameToAccountRegex curacct
    q = queryFromOpts d ropts
         -- query_="cur:\\$"} -- XXX limit to one commodity to ensure one-line items
         --{query_=unwords' $ locArgs l}

    -- run a transactions report, most recent last
    (_label,items') = accountTransactionsReport ropts j thisacctq q
    items = reverse items'

    -- pre-render all items; these will be the List elements. This helps calculate column widths.
    displayitem (_, t, _issplit, otheracctsstr, change, bal) =
      (showDate $ tdate t
      ,tdescription t
      ,case splitOn ", " otheracctsstr of
        [s] -> s
        _   -> "<split>"
      ,showMixedAmountOneLineWithoutPrice change
      ,showMixedAmountOneLineWithoutPrice bal
      )
    displayitems = map displayitem items

    -- build the List, moving the selection to the end
    l = listMoveTo (length items) $
        list (Name "register") (V.fromList displayitems) 1

        -- (listName someList)

initRegisterScreen2 _ _ _ = error "init function called with wrong screen type, should not happen"

drawRegisterScreen2 :: AppState -> [Widget]
drawRegisterScreen2 AppState{aopts=_opts, aScreen=RegisterScreen2{rs2State=l}} = [ui]
  where
    label = str "Transaction "
            <+> cur
            <+> str " of "
            <+> total
            <+> str " to/from this account" -- " <+> str query <+> "and subaccounts"
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
        totalwidth = c^.availWidthL - 2 -- XXX trimmed.. for the margin ?

        -- the date column is fixed width
        datewidth = 10

        -- multi-commodity amounts rendered on one line can be
        -- arbitrarily wide.  Give the two amounts as much space as
        -- they need, while reserving a minimum of space for other
        -- columns and whitespace.  If they don't get all they need,
        -- allocate it to them proportionally to their maximum widths.
        maxamtswidth = max 0 (totalwidth - 21)
        changewidth' = maximum' $ map (length . fourth5) displayitems
        balwidth' = maximum' $ map (length . fifth5) displayitems
        changewidthproportion = fromIntegral (changewidth' + balwidth') / fromIntegral changewidth'
        maxchangewidth = round $ fromIntegral maxamtswidth / changewidthproportion
        maxbalwidth = maxamtswidth - maxchangewidth
        changewidth = min maxchangewidth changewidth' 
        balwidth = min maxbalwidth balwidth'

        -- assign the remaining space to the description and accounts columns
        maxdescacctswidth = totalwidth - 17 - changewidth - balwidth
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

      render $ defaultLayout label $ renderList l (drawRegisterItem colwidths)

drawRegisterScreen2 _ = error "draw function called with wrong screen type, should not happen"

drawRegisterItem :: (Int,Int,Int,Int,Int) -> Bool -> (String,String,String,String,String) -> Widget
drawRegisterItem (datewidth,descwidth,acctswidth,changewidth,balwidth) _sel (date,desc,accts,change,bal) =
  Widget Greedy Fixed $ do
    render $
      str (padright datewidth $ elideRight datewidth date) <+>
      str " " <+>
      str (padright descwidth $ elideRight descwidth desc) <+>
      str "  " <+>
      str (padright acctswidth $ elideLeft acctswidth $ accts) <+>
      str "  " <+>
      str (padleft changewidth $ elideLeft changewidth change) <+>
      str "  " <+>
      str (padleft balwidth $ elideLeft balwidth bal)

handleRegisterScreen2 :: AppState -> Vty.Event -> EventM (Next AppState)
handleRegisterScreen2 st@AppState{aopts=_opts,aScreen=s@RegisterScreen2{rs2State=is}} e = do
  case e of
    Vty.EvKey Vty.KEsc []        -> halt st
    Vty.EvKey (Vty.KChar 'q') [] -> halt st
    Vty.EvKey (Vty.KLeft) []     -> continue $ popScreen st
    -- Vty.EvKey (Vty.KRight) []    -> error (show curItem) where curItem = listSelectedElement is
    -- fall through to the list's event handler (handles [pg]up/down)
    ev                       -> do
                                 is' <- handleEvent ev is
                                 continue st{aScreen=s{rs2State=is'}}
                                 -- continue =<< handleEventLensed st someLens ev
handleRegisterScreen2 _ _ = error "event handler called with wrong screen type, should not happen"
