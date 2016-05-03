-- The transaction screen, showing a single transaction's general journal entry.

{-# LANGUAGE OverloadedStrings, TupleSections #-} -- , FlexibleContexts

module Hledger.UI.TransactionScreen
 (screen
 )
where

-- import Lens.Micro ((^.))
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
import Brick.Widgets.List (listMoveTo)
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
      -- <+> withAttr ("border" <> "bold") (str $ "#" ++ show (tindex t))
      -- <+> str (" ("++show i++" of "++show (length nts)++" in "++acct++")")
      <+> (str $ "#" ++ show (tindex t))
      <+> str " ("
      <+> withAttr ("border" <> "bold") (str $ show i)
      <+> str (" of "++show (length nts)++" in "++acct++")")
    bottomlabel = borderKeysStr [
       ("left", "back")
      ,("up/down", "prev/next")
      ,("g", "reload")
      ,("q", "quit")
      ]
    ui = Widget Greedy Greedy $ do
      render $ defaultLayout toplabel bottomlabel $ str $ showTransactionUnelidedOneLineAmounts t

drawTransactionScreen _ = error "draw function called with wrong screen type, should not happen"

handleTransactionScreen :: AppState -> Vty.Event -> EventM (Next AppState)
handleTransactionScreen st@AppState{
   aScreen=s@TransactionScreen{tsState=((i,t),nts,acct)}
  ,aopts=UIOpts{cliopts_=copts@CliOpts{reportopts_=ropts}}
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
      (ej, _) <- liftIO $ journalReloadIfChanged copts d j
      case ej of
        Right j' -> do
          -- got to redo the register screen's transactions report, to get the latest transactions list for this screen
          -- XXX duplicates initRegisterScreen
          let
            ropts' = ropts {depth_=Nothing
                           ,balancetype_=HistoricalBalance
                           }
            q = filterQuery (not . queryIsDepth) $ queryFromOpts d ropts'
            thisacctq = Acct $ accountNameToAccountRegex acct -- includes subs
            items = reverse $ snd $ accountTransactionsReport ropts j' q thisacctq
            ts = map first6 items
            numberedts = zip [1..] ts
            -- select the best current transaction from the new list
            -- stay at the same index if possible, or if we are now past the end, select the last, otherwise select the first
            (i',t') = case lookup i numberedts
                      of Just t'' -> (i,t'')
                         Nothing | null numberedts -> (0,nulltransaction)
                                 | i > fst (last numberedts) -> last numberedts
                                 | otherwise -> head numberedts
            st' = st{aScreen=s{tsState=((i',t'),numberedts,acct)}}
          continue $ reload j' d st'

        Left err -> continue $ screenEnter d ES.screen{esState=err} st

    -- Vty.EvKey (Vty.KChar 'C') [] -> continue $ reload j d $ stToggleCleared st

    Vty.EvKey (Vty.KUp) []       -> continue $ reload j d st{aScreen=s{tsState=((iprev,tprev),nts,acct)}}
    Vty.EvKey (Vty.KDown) []     -> continue $ reload j d st{aScreen=s{tsState=((inext,tnext),nts,acct)}}

    Vty.EvKey (Vty.KLeft) []     -> continue st''
      where
        st'@AppState{aScreen=scr} = popScreen st
        st'' = st'{aScreen=rsSetSelectedTransaction (fromIntegral i) scr}

    _ev -> continue st

handleTransactionScreen _ _ = error "event handler called with wrong screen type, should not happen"

rsSetSelectedTransaction i scr@RegisterScreen{rsState=(l,a)} = scr{rsState=(l',a)}
  where l' = listMoveTo (i-1) l
rsSetSelectedTransaction _ scr = scr

