-- The transaction screen, showing a single transaction's general journal entry.

{-# LANGUAGE OverloadedStrings, TupleSections #-} -- , FlexibleContexts

module Hledger.UI.TransactionScreen
 (screen
 )
where

-- import Control.Lens ((^.))
import Control.Monad.IO.Class (liftIO)
-- import Data.List
-- import Data.List.Split (splitOn)
-- import Data.Ord
import Data.Monoid
-- import Data.Maybe
import Data.Time.Calendar (Day)
-- import qualified Data.Vector as V
import Graphics.Vty as Vty
-- import Safe (headDef, lastDef)
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
   tsState   = ((1,nulltransaction),[(1,nulltransaction)],"")
  ,sInitFn   = initTransactionScreen
  ,sDrawFn   = drawTransactionScreen
  ,sHandleFn = handleTransactionScreen
  }

initTransactionScreen :: Day -> AppState -> AppState
initTransactionScreen _d st@AppState{aopts=_opts, ajournal=_j, aScreen=_s@TransactionScreen{tsState=_}} = st
initTransactionScreen _ _ = error "init function called with wrong screen type, should not happen"

drawTransactionScreen :: AppState -> [Widget]
drawTransactionScreen AppState{ -- aopts=_uopts@UIOpts{cliopts_=_copts@CliOpts{reportopts_=_ropts@ReportOpts{query_=querystr}}},
                               aScreen=TransactionScreen{tsState=((i,t),nts,acct)}} = [ui]
  where
    -- datedesc = show (tdate t) ++ " " ++ tdescription t
    toplabel =
      str "Transaction "
      -- <+> withAttr ("border" <> "bold") (str $ 
      <+> withAttr ("border" <> "bold") (str $ show i)
      <+> str (" of "++show (length nts)++" in "++acct)
    bottomlabel = borderKeysStr [
       ("left", "return to register")
      ,("up/down", "prev/next transaction")
      ,("g", "reload")
      ,("q", "quit")
      ]
    ui = Widget Greedy Greedy $ do
      render $ defaultLayout toplabel bottomlabel $ str $ showTransactionUnelidedOneLineAmounts t

drawTransactionScreen _ = error "draw function called with wrong screen type, should not happen"

handleTransactionScreen :: AppState -> Vty.Event -> EventM (Next AppState)
handleTransactionScreen st@AppState{
   aScreen=s@TransactionScreen{tsState=((i,t),nts,acct)}
  ,aopts=UIOpts{cliopts_=_copts}
  ,ajournal=j
  } e = do
  d <- liftIO getCurrentDay
  let
    (iprev,tprev) = maybe (i,t) ((i-1),) $ lookup (i-1) nts
    (inext,tnext) = maybe (i,t) ((i+1),) $ lookup (i+1) nts
  case e of
    Vty.EvKey Vty.KEsc []        -> halt st
    Vty.EvKey (Vty.KChar 'q') [] -> halt st

    Vty.EvKey (Vty.KChar 'g') [] -> do
      d <- liftIO getCurrentDay
      ej <- liftIO $ journalReload j  -- (ej, changed) <- liftIO $ journalReloadIfChanged copts j
      case ej of
        Right j' -> continue $ reload j' d st
        Left err -> continue $ screenEnter d ES.screen{esState=err} st

    Vty.EvKey (Vty.KUp) []       -> continue $ reload j d st{aScreen=s{tsState=((iprev,tprev),nts,acct)}}
    Vty.EvKey (Vty.KDown) []     -> continue $ reload j d st{aScreen=s{tsState=((inext,tnext),nts,acct)}}

    Vty.EvKey (Vty.KLeft) []     -> continue $ popScreen st

    _ev -> continue st

handleTransactionScreen _ _ = error "event handler called with wrong screen type, should not happen"
