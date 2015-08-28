-- The accounts screen, showing accounts and balances like the CLI balance command.

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Hledger.UI.AccountsScreen
 (screen)
where

import Control.Lens ((^.))
-- import Control.Monad
import Control.Monad.IO.Class
-- import Data.Default
import Data.List
import Data.Maybe
import Data.Monoid
import Data.Time.Calendar (Day)
import System.FilePath (takeFileName)
import qualified Data.Vector as V
import Graphics.Vty as Vty
import Brick
import Brick.Widgets.List
-- import Brick.Widgets.Border
-- import Brick.Widgets.Center

import Hledger
import Hledger.Cli hiding (progname,prognameandversion,green)
-- import Hledger.Cli.Options (defaultBalanceLineFormat)
import Hledger.UI.Options
-- import Hledger.UI.Theme
import Hledger.UI.UITypes
import Hledger.UI.UIUtils
import qualified Hledger.UI.RegisterScreen as RS (screen)

screen = AccountsScreen{
   asState  = list "accounts" V.empty 1
  ,sInitFn    = initAccountsScreen Nothing
  ,sDrawFn    = drawAccountsScreen
  ,sHandleFn = handleAccountsScreen
  }

initAccountsScreen :: Maybe AccountName -> Day -> AppState -> AppState
initAccountsScreen mselacct d st@AppState{
  aopts=uopts@UIOpts{cliopts_=copts@CliOpts{reportopts_=ropts}},
  ajournal=j,
  aScreen=s@AccountsScreen{}
  } =
  st{aopts=opts', aScreen=s{asState=l'}}
   where
    l = list (Name "accounts") (V.fromList items) 1

    -- hacky: when we're adjusting depth, mselacct is the account that was selected previously,
    -- in which case try and keep the selection near where it was
    l' = case mselacct of
             Nothing -> l
             Just a  -> -- vScrollToBeginning $ viewportScroll "accounts"
                           maybe l (flip listMoveTo l) mi
               where
                 mi = findIndex (\((acct,_,_),_) -> acct==a') items
                 a' = maybe a (flip clipAccountName a) $ depth_ ropts

    -- XXX messing around with depth, which is different from other queries
    -- In hledger,
    -- - reportopts{depth_} indicates --depth options
    -- - reportopts{query_} is the query arguments as a string
    -- - the report query is based on both of these.
    -- For hledger-ui, currently, we move depth: arguments out of reportopts{query_}
    -- and into reportopts{depth_}, so that depth and other kinds of filter query
    -- can be displayed independently.
    opts' = uopts{cliopts_=copts{reportopts_=ropts'}}
    q = queryFromOpts d ropts
    ropts' = ropts
            {
              -- ensure depth_ also reflects depth: args
              depth_=depthfromoptsandargs,
              -- remove depth: args from query_
              query_=unwords $ -- as in ReportOptions, with same limitations
                     [v | (k,v) <- rawopts_ copts, k=="args", not $ "depth" `isPrefixOf` v],
              -- XXX balanceReport doesn't respect this yet
              balancetype_=HistoricalBalance
            }
      where
        depthfromoptsandargs = case queryDepth q of 99999 -> Nothing
                                                    d     -> Just d
    -- maybe convert balances to market value
    convert | value_ ropts' = balanceReportValue j valuedate
            | otherwise    = id
      where
        valuedate = fromMaybe d $ queryEndDate False q

    -- run the report
    (items,_total) = convert $ balanceReport ropts' q j

initAccountsScreen _ _ _ = error "init function called with wrong screen type, should not happen"

drawAccountsScreen :: AppState -> [Widget]
drawAccountsScreen st@AppState{aopts=uopts, ajournal=j, aScreen=AccountsScreen{asState=is}} =
  [ui]
    where
      toplabel = files
              <+> str " accounts"
              <+> borderQueryStr querystr
              <+> borderDepthStr depth
              <+> str " ("
              <+> cur
              <+> str " of "
              <+> total
              <+> str ")"
      files = case journalFilePaths j of
                     [] -> str ""
                     [f] -> withAttr ("border" <> "bold") $ str $ takeFileName f
                     [f,_] -> (withAttr ("border" <> "bold") $ str $ takeFileName f) <+> str " (& 1 included file)"
                     f:fs -> (withAttr ("border" <> "bold") $ str $ takeFileName f) <+> str (" (& " ++ show (length fs) ++ " included files)")
      querystr = query_ $ reportopts_ $ cliopts_ uopts
      depth = depth_ $ reportopts_ $ cliopts_ uopts
      -- ropts = reportopts_ $ cliopts_ uopts
      -- q = queryFromOpts d ropts
      -- depth = queryDepth q
      cur = str (case is^.listSelectedL of
                  Nothing -> "-"
                  Just i -> show (i + 1))
      total = str $ show $ length $ is^.listElementsL

      items = listElements is
      flat = flat_ $ reportopts_ $ cliopts_ $ aopts st
      acctcolwidth = maximum $
                      V.map
                       (\((full,short,indent),_) ->
                         if flat then length full else length short + indent*2)
                       items 
      fmt = OneLine [ -- use a one-line format, List elements must have equal height
               FormatField True (Just 2) Nothing DepthSpacerField
             , FormatField True (Just acctcolwidth) Nothing AccountField
             , FormatLiteral "  "
             , FormatField False (Just 40) Nothing TotalField
             ]

      bottomlabel = borderKeysStr [
         -- "up/down/pgup/pgdown/home/end: move"
         "1-0: adjust depth limit"
        ,"right: show transactions"
        ,"q: quit"
        ]

      ui = defaultLayout toplabel bottomlabel $ renderList is (drawAccountsItem fmt)

drawAccountsScreen _ = error "draw function called with wrong screen type, should not happen"

drawAccountsItem :: StringFormat -> Bool -> BalanceReportItem -> Widget
drawAccountsItem fmt _sel item =
  Widget Greedy Fixed $ do
    -- c <- getContext
    let
      showitem = intercalate "\n" . balanceReportItemAsText defreportopts fmt
    render $ str $ showitem item

handleAccountsScreen :: AppState -> Vty.Event -> EventM (Next AppState)
handleAccountsScreen st@AppState{aScreen=scr@AccountsScreen{asState=is}} e = do
    d <- liftIO getCurrentDay
    -- c <- getContext
    -- let h = c^.availHeightL
    --     moveSel n l = listMoveBy n l
    let
      acct = case listSelectedElement is of
              Just (_, ((a, _, _), _)) -> a
              Nothing -> ""
    case e of
        Vty.EvKey Vty.KEsc []        -> halt st
        Vty.EvKey (Vty.KChar 'q') [] -> halt st
        Vty.EvKey (Vty.KChar '0') [] -> continue $ initAccountsScreen (Just acct) d $ setDepth 0 st
        Vty.EvKey (Vty.KChar '1') [] -> continue $ initAccountsScreen (Just acct) d $ setDepth 1 st
        Vty.EvKey (Vty.KChar '2') [] -> continue $ initAccountsScreen (Just acct) d $ setDepth 2 st
        Vty.EvKey (Vty.KChar '3') [] -> continue $ initAccountsScreen (Just acct) d $ setDepth 3 st
        Vty.EvKey (Vty.KChar '4') [] -> continue $ initAccountsScreen (Just acct) d $ setDepth 4 st
        Vty.EvKey (Vty.KChar '5') [] -> continue $ initAccountsScreen (Just acct) d $ setDepth 5 st
        Vty.EvKey (Vty.KChar '6') [] -> continue $ initAccountsScreen (Just acct) d $ setDepth 6 st
        Vty.EvKey (Vty.KChar '7') [] -> continue $ initAccountsScreen (Just acct) d $ setDepth 7 st
        Vty.EvKey (Vty.KChar '8') [] -> continue $ initAccountsScreen (Just acct) d $ setDepth 8 st
        Vty.EvKey (Vty.KChar '9') [] -> continue $ initAccountsScreen (Just acct) d $ setDepth 9 st
        Vty.EvKey (Vty.KLeft) []     -> continue $ popScreen st
        Vty.EvKey (Vty.KRight) []    -> do
          let st' = screenEnter d RS.screen{rsAcct=acct} st
          vScrollToBeginning $ viewportScroll "register"
          continue st'

        -- Vty.EvKey (Vty.KPageDown) [] -> continue $ st{aScreen=scr{asState=moveSel h is}}
        -- Vty.EvKey (Vty.KPageUp) []   -> continue $ st{aScreen=scr{asState=moveSel (-h) is}}

        -- fall through to the list's event handler (handles up/down)
        ev                       -> do
                                     is' <- handleEvent ev is
                                     continue $ st{aScreen=scr{asState=is'}}
                                 -- continue =<< handleEventLensed st someLens ev
handleAccountsScreen _ _ = error "event handler called with wrong screen type, should not happen"

setDepth :: Int -> AppState -> AppState
setDepth depth st@AppState{aopts=uopts@UIOpts{cliopts_=copts@CliOpts{..}}} =
  st{aopts=uopts{cliopts_=copts{reportopts_=reportopts_{depth_=md}}}}
  where
    md | depth==0  = Nothing
       | otherwise = Just depth
