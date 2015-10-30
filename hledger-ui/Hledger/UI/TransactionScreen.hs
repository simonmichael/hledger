-- The transaction screen, showing the general journal entry representing a single transaction.

{-# LANGUAGE OverloadedStrings #-} -- , FlexibleContexts

module Hledger.UI.TransactionScreen
 (screen
 -- ,tsSetCurrentAccount
 )
where

-- import Control.Lens ((^.))
import Control.Monad.IO.Class (liftIO)
-- import Data.List
-- import Data.List.Split (splitOn)
import Data.Monoid
-- import Data.Maybe
import Data.Time.Calendar (Day)
-- import qualified Data.Vector as V
import Graphics.Vty as Vty
import Brick
-- import Brick.Widgets.List
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
import qualified Hledger.UI.ErrorScreen as ES (screen)

screen = TransactionScreen{
   tsState   = nulltransaction
  ,sInitFn   = initTransactionScreen
  ,sDrawFn   = drawTransactionScreen
  ,sHandleFn = handleTransactionScreen
  }

-- tsSetCurrentAccount a scr@TransactionScreen{tsState=(l,_)} = scr{tsState=(l,a)}
-- tsSetCurrentAccount _ scr = scr

initTransactionScreen :: Day -> AppState -> AppState
initTransactionScreen _d st@AppState{aopts=_opts, ajournal=_j, aScreen=_s@TransactionScreen{tsState=_t}} =
  st
  -- where
  --   -- gather arguments and queries
  --   ropts = (reportopts_ $ cliopts_ opts)
  --           {
  --             depth_=Nothing,
  --             balancetype_=HistoricalBalance
  --           }
  --   -- XXX temp
  --   thisacctq = Acct $ accountNameToAccountRegex acct -- includes subs
  --   q = filterQuery (not . queryIsDepth) $ queryFromOpts d ropts

  --   -- run a transactions report, most recent last
  --   q' =
  --     -- ltrace "q"
  --     q
  --   thisacctq' =
  --     -- ltrace "thisacctq"
  --     thisacctq
  --   (_label,items') = accountTransactionsReport ropts j q' thisacctq'
  --   items = reverse items'

  --   -- pre-render all items; these will be the List elements. This helps calculate column widths.
  --   displayitem (_, t, _issplit, otheracctsstr, change, bal) =
  --     (showDate $ tdate t
  --     ,tdescription t
  --     ,case splitOn ", " otheracctsstr of
  --       [s] -> s
  --       ss  -> intercalate ", " ss
  --       -- _   -> "<split>"  -- should do this if accounts field width < 30
  --     ,showMixedAmountOneLineWithoutPrice change
  --     ,showMixedAmountOneLineWithoutPrice bal
  --     )
  --   displayitems = map displayitem items

  --   -- build the List, moving the selection to the end
  --   l = listMoveTo (length items) $
  --       list (Name "register") (V.fromList displayitems) 1

  --       -- (listName someList)

initTransactionScreen _ _ = error "init function called with wrong screen type, should not happen"

drawTransactionScreen :: AppState -> [Widget]
drawTransactionScreen AppState{ -- aopts=_uopts@UIOpts{cliopts_=_copts@CliOpts{reportopts_=_ropts@ReportOpts{query_=querystr}}},
                             aScreen=TransactionScreen{tsState=t}} = [ui]
  where
    toplabel =
      str "Transaction "
      <+> withAttr ("border" <> "bold") (str $ show (tdate t) ++ " " ++ tdescription t)
      -- <+> str " of "
      -- <+> str " ("
      -- <+> cur
      -- <+> str "/"
      -- <+> total
      -- <+> str ")"
    -- cur = str $ case l^.listSelectedL of
    --              Nothing -> "-"
    --              Just i -> show (i + 1)
    -- total = str $ show $ length displayitems
    -- displayitems = V.toList $ l^.listElementsL

    -- query = query_ $ reportopts_ $ cliopts_ opts

    ui = Widget Greedy Greedy $ do

      -- calculate column widths, based on current available width
      -- c <- getContext
      let
        -- totalwidth = c^.availWidthL
        --              - 2 -- XXX due to margin ? shouldn't be necessary (cf UIUtils)

        -- -- the date column is fixed width
        -- datewidth = 10

        -- -- multi-commodity amounts rendered on one line can be
        -- -- arbitrarily wide.  Give the two amounts as much space as
        -- -- they need, while reserving a minimum of space for other
        -- -- columns and whitespace.  If they don't get all they need,
        -- -- allocate it to them proportionally to their maximum widths.
        -- whitespacewidth = 10 -- inter-column whitespace, fixed width
        -- minnonamtcolswidth = datewidth + 2 + 2 -- date column plus at least 2 for desc and accts
        -- maxamtswidth = max 0 (totalwidth - minnonamtcolswidth - whitespacewidth)
        -- maxchangewidthseen = maximum' $ map (strWidth . fourth5) displayitems
        -- maxbalwidthseen = maximum' $ map (strWidth . fifth5) displayitems
        -- changewidthproportion = fromIntegral maxchangewidthseen / fromIntegral (maxchangewidthseen + maxbalwidthseen)
        -- maxchangewidth = round $ changewidthproportion * fromIntegral maxamtswidth
        -- maxbalwidth = maxamtswidth - maxchangewidth
        -- changewidth = min maxchangewidth maxchangewidthseen 
        -- balwidth = min maxbalwidth maxbalwidthseen

        -- -- assign the remaining space to the description and accounts columns
        -- -- maxdescacctswidth = totalwidth - (whitespacewidth - 4) - changewidth - balwidth
        -- maxdescacctswidth =
        --   -- trace (show (totalwidth, datewidth, changewidth, balwidth, whitespacewidth)) $
        --   max 0 (totalwidth - datewidth - changewidth - balwidth - whitespacewidth)
        -- -- allocating proportionally.
        -- -- descwidth' = maximum' $ map (strWidth . second5) displayitems
        -- -- acctswidth' = maximum' $ map (strWidth . third5) displayitems
        -- -- descwidthproportion = (descwidth' + acctswidth') / descwidth'
        -- -- maxdescwidth = min (maxdescacctswidth - 7) (maxdescacctswidth / descwidthproportion)
        -- -- maxacctswidth = maxdescacctswidth - maxdescwidth
        -- -- descwidth = min maxdescwidth descwidth' 
        -- -- acctswidth = min maxacctswidth acctswidth'
        -- -- allocating equally.
        -- descwidth = maxdescacctswidth `div` 2
        -- acctswidth = maxdescacctswidth - descwidth
        -- colwidths = (datewidth,descwidth,acctswidth,changewidth,balwidth)

        bottomlabel = borderKeysStr [
           -- ("up/down/pgup/pgdown/home/end", "move")
           ("left", "return to register")
          ,("g", "reload")
          ,("q", "quit")
          ]

      render $ defaultLayout toplabel bottomlabel $ str $ showTransactionUnelided t

drawTransactionScreen _ = error "draw function called with wrong screen type, should not happen"

handleTransactionScreen :: AppState -> Vty.Event -> EventM (Next AppState)
handleTransactionScreen st@AppState{
   aScreen=_s@TransactionScreen{tsState=_t}
  ,aopts=UIOpts{cliopts_=_copts}
  ,ajournal=j
  } e = do
  case e of
    Vty.EvKey Vty.KEsc []        -> halt st
    Vty.EvKey (Vty.KChar 'q') [] -> halt st

    Vty.EvKey (Vty.KChar 'g') [] -> do
      d <- liftIO getCurrentDay
      ej <- liftIO $ journalReload j  -- (ej, changed) <- liftIO $ journalReloadIfChanged copts j
      case ej of
        Right j' -> continue $ reload j' d st
        Left err -> continue $ screenEnter d ES.screen{esState=err} st

    Vty.EvKey (Vty.KLeft) []     -> continue $ popScreen st

    _ev -> continue st

handleTransactionScreen _ _ = error "event handler called with wrong screen type, should not happen"
