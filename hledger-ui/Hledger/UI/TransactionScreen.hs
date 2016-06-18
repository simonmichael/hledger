-- The transaction screen, showing a single transaction's general journal entry.

{-# LANGUAGE OverloadedStrings, TupleSections, RecordWildCards #-} -- , FlexibleContexts

module Hledger.UI.TransactionScreen
 (transactionScreen
 ,rsSelect
 )
where

import Control.Monad.IO.Class (liftIO)
import Data.List
import Data.Monoid
import qualified Data.Text as T
import Data.Time.Calendar (Day)
import Graphics.Vty
import Brick
import Brick.Widgets.List (listMoveTo)
import Brick.Widgets.Border (borderAttr)

import Hledger
import Hledger.Cli hiding (progname,prognameandversion,green)
import Hledger.UI.UIOptions
-- import Hledger.UI.Theme
import Hledger.UI.UITypes
import Hledger.UI.UIState
import Hledger.UI.UIUtils
import Hledger.UI.ErrorScreen

transactionScreen :: TransactionScreen
transactionScreen = TransactionScreen{
   tsInit   = tsInit_
  ,tsDraw   = tsDraw_
  ,tsHandle = tsHandle_
  ,tsTransaction  = (1,nulltransaction)
  ,tsTransactions = [(1,nulltransaction)]
  ,tsAccount      = ""
  }

tsInit_ :: Day -> Bool -> UIState -> UIState
tsInit_ _d _reset ui@UIState{aopts=UIOpts{cliopts_=CliOpts{reportopts_=_ropts}}
                                           ,ajournal=_j
                                           ,aScreen=TransScreen(TransactionScreen{..})} = ui
tsInit_ _ _ _ = error "init function called with wrong screen type, should not happen"

tsDraw_ :: UIState -> [Widget]
tsDraw_ UIState{aopts=UIOpts{cliopts_=CliOpts{reportopts_=ropts}}
                              ,aScreen=TransScreen(TransactionScreen{
                                   tsTransaction=(i,t)
                                  ,tsTransactions=nts
                                  ,tsAccount=acct})
                              ,aMode=mode} =
  case mode of
    Help       -> [helpDialog, maincontent]
    -- Minibuffer e -> [minibuffer e, maincontent]
    _          -> [maincontent]
  where
    -- datedesc = show (tdate t) ++ " " ++ tdescription t
    toplabel =
      str "Transaction "
      -- <+> withAttr ("border" <> "bold") (str $ "#" ++ show (tindex t))
      -- <+> str (" ("++show i++" of "++show (length nts)++" in "++acct++")")
      <+> (str $ "#" ++ show (tindex t))
      <+> str " ("
      <+> withAttr ("border" <> "bold") (str $ show i)
      <+> str (" of "++show (length nts))
      <+> togglefilters
      <+> borderQueryStr (query_ ropts)
      <+> str (" in "++T.unpack (replaceHiddenAccountsNameWith "All" acct)++")")
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
    maincontent = Widget Greedy Greedy $ do
      render $ defaultLayout toplabel bottomlabel $ str $
        showTransactionUnelidedOneLineAmounts $
        -- (if real_ ropts then filterTransactionPostings (Real True) else id) -- filter postings by --real
        t
      where
        bottomlabel = case mode of
                        -- Minibuffer ed -> minibuffer ed
                        _             -> quickhelp
        quickhelp = borderKeysStr [
           ("h", "help")
          ,("left", "back")
          ,("up/down", "prev/next")
          --,("ESC", "cancel/top")
          -- ,("a", "add")
          ,("g", "reload")
          ,("q", "quit")
          ]

tsDraw_ _ = error "draw function called with wrong screen type, should not happen"

tsHandle_ :: UIState -> Event -> EventM (Next UIState)
tsHandle_ ui@UIState{aScreen=TransScreen(s@TransactionScreen{tsTransaction=(i,t)
                                                            ,tsTransactions=nts
                                                            ,tsAccount=acct})
                    ,aopts=UIOpts{cliopts_=copts@CliOpts{reportopts_=ropts}}
                    ,ajournal=j
                    ,aMode=mode
                    }
         ev =
  case mode of
    Help ->
      case ev of
        EvKey (KChar 'q') [] -> halt ui
        _                    -> helpHandle ui ev

    _ -> do
      d <- liftIO getCurrentDay
      let
        (iprev,tprev) = maybe (i,t) ((i-1),) $ lookup (i-1) nts
        (inext,tnext) = maybe (i,t) ((i+1),) $ lookup (i+1) nts
      case ev of
        EvKey (KChar 'q') [] -> halt ui
        EvKey KEsc        [] -> continue $ resetScreens d ui
        EvKey (KChar c)   [] | c `elem` ['h','?'] -> continue $ setMode Help ui
        EvKey (KChar 'g') [] -> do
          d <- liftIO getCurrentDay
          (ej, _) <- liftIO $ journalReloadIfChanged copts d j
          case ej of
            Left err -> continue $ screenEnter d (ErrScreen errorScreen{esError=err}) ui
            Right j' -> do
              -- got to redo the register screen's transactions report, to get the latest transactions list for this screen
              -- XXX duplicates rsInit
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
                ui' = ui{aScreen=TransScreen(s{tsTransaction=(i',t')
                                              ,tsTransactions=numberedts
                                              ,tsAccount=acct})}
              continue $ regenerateScreens j' d ui'
        -- if allowing toggling here, we should refresh the txn list from the parent register screen
        -- EvKey (KChar 'E') [] -> continue $ regenerateScreens j d $ stToggleEmpty ui
        -- EvKey (KChar 'C') [] -> continue $ regenerateScreens j d $ stToggleCleared ui
        -- EvKey (KChar 'R') [] -> continue $ regenerateScreens j d $ stToggleReal ui
        EvKey KUp   [] -> continue $ regenerateScreens j d ui{aScreen=TransScreen(s{tsTransaction=(iprev,tprev)})}
        EvKey KDown [] -> continue $ regenerateScreens j d ui{aScreen=TransScreen(s{tsTransaction=(inext,tnext)})}
        EvKey KLeft [] -> continue ui''
          where
            ui'@UIState{aScreen=scr} = popScreen ui
            ui'' = ui'{aScreen=rsSelect (fromIntegral i) scr}
        _ -> continue ui

tsHandle_ _ _ = error "event handler called with wrong screen type, should not happen"

-- | Select the nth item on the register screen.
rsSelect :: Int -> Screen -> Screen
rsSelect i (RegScreen (scr@RegisterScreen{..})) = RegScreen (scr{rsList=l'})
  where l' = listMoveTo (i-1) rsList
rsSelect _ scr = scr
