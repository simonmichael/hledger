-- The transaction screen, showing a single transaction's general journal entry.

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}

module Hledger.UI.TransactionScreen
 (transactionScreen
 ,rsSelect
 )
where

import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Data.List
import Data.Maybe
import qualified Data.Text as T
import Data.Time.Calendar (Day)
import Graphics.Vty (Event(..),Key(..),Modifier(..))
import Brick
import Brick.Widgets.List (listMoveTo)

import Hledger
import Hledger.Cli hiding (progname,prognameandversion)
import Hledger.UI.UIOptions
-- import Hledger.UI.Theme
import Hledger.UI.UITypes
import Hledger.UI.UIState
import Hledger.UI.UIUtils
import Hledger.UI.Editor
import Hledger.UI.ErrorScreen

transactionScreen :: Screen
transactionScreen = TransactionScreen{
   sInit   = tsInit
  ,sDraw   = tsDraw
  ,sHandle = tsHandle
  ,tsTransaction  = (1,nulltransaction)
  ,tsTransactions = [(1,nulltransaction)]
  ,tsAccount      = ""
  }

tsInit :: Day -> Bool -> UIState -> UIState
tsInit _d _reset ui@UIState{aopts=UIOpts{cliopts_=CliOpts{reportspec_=_rspec}}
                           ,ajournal=_j
                           ,aScreen=TransactionScreen{}
                           } =
  -- plog ("initialising TransactionScreen, value_ is "
  --       -- ++ (pshow (Just (AtDefault Nothing)::Maybe ValuationType))
  --       ++(pshow (value_ _ropts))  -- XXX calling value_ here causes plog to fail with: debug.log: openFile: resource busy (file is locked)
  --       ++ "?"
  --       ++" and first commodity is")
  --       (acommodity$head$amounts$pamount$head$tpostings$snd$tsTransaction)
  --      `seq`
  ui
tsInit _ _ _ = error "init function called with wrong screen type, should not happen"  -- PARTIAL:

tsDraw :: UIState -> [Widget Name]
tsDraw UIState{aopts=UIOpts{cliopts_=copts@CliOpts{reportspec_=rspec@ReportSpec{rsOpts=ropts}}}
              ,ajournal=j
              ,aScreen=TransactionScreen{tsTransaction=(i,t')
                                        ,tsTransactions=nts
                                        ,tsAccount=acct
                                        }
              ,aMode=mode
              } =
  case mode of
    Help       -> [helpDialog copts, maincontent]
    -- Minibuffer e -> [minibuffer e, maincontent]
    _          -> [maincontent]
  where
    -- as with print, show amounts with all of their decimal places
    t = transactionMapPostingAmounts mixedAmountSetFullPrecision t'
    maincontent = Widget Greedy Greedy $ do
      let
        prices = journalPriceOracle (infer_value_ ropts) j
        styles = journalCommodityStyles j
        periodlast =
          fromMaybe (error' "TransactionScreen: expected a non-empty journal") $  -- PARTIAL: shouldn't happen
          reportPeriodOrJournalLastDay rspec j

      render . defaultLayout toplabel bottomlabel . str
        . T.unpack . showTransactionOneLineAmounts
        . maybe id (transactionApplyValuation prices styles periodlast (rsToday rspec)) (value_ ropts)
        $ case cost_ ropts of
               Cost   -> transactionToCost styles t
               NoCost -> t
        -- (if real_ ropts then filterTransactionPostings (Real True) else id) -- filter postings by --real
      where
        toplabel =
          str "Transaction "
          -- <+> withAttr ("border" <> "bold") (str $ "#" ++ show (tindex t))
          -- <+> str (" ("++show i++" of "++show (length nts)++" in "++acct++")")
          <+> (str $ "#" ++ show (tindex t))
          <+> str " ("
          <+> withAttr ("border" <> "bold") (str $ show i)
          <+> str (" of "++show (length nts))
          <+> togglefilters
          <+> borderQueryStr (unwords . map (quoteIfNeeded . T.unpack) $ querystring_ ropts)
          <+> str (" in "++T.unpack (replaceHiddenAccountsNameWith "All" acct)++")")
          <+> (if ignore_assertions_ . balancingopts_ $ inputopts_ copts then withAttr ("border" <> "query") (str " ignoring balance assertions") else str "")
          where
            togglefilters =
              case concat [
                   uiShowStatus copts $ statuses_ ropts
                  ,if real_ ropts then ["real"] else []
                  ,if empty_ ropts then [] else ["nonzero"]
                  ] of
                [] -> str ""
                fs -> withAttr ("border" <> "query") (str $ " " ++ intercalate ", " fs)

        bottomlabel = case mode of
                        -- Minibuffer ed -> minibuffer ed
                        _             -> quickhelp
          where
            quickhelp = borderKeysStr [
               ("?", "help")
              ,("LEFT", "back")
              ,("UP/DOWN", "prev/next")
              --,("ESC", "cancel/top")
              -- ,("a", "add")
              ,("E", "editor")
              ,("g", "reload")
              ,("q", "quit")
              ]

tsDraw _ = error "draw function called with wrong screen type, should not happen"  -- PARTIAL:

tsHandle :: UIState -> BrickEvent Name AppEvent -> EventM Name (Next UIState)
tsHandle ui@UIState{aScreen=s@TransactionScreen{tsTransaction=(i,t)
                                               ,tsTransactions=nts
                                               ,tsAccount=acct
                                               }
                   ,aopts=UIOpts{cliopts_=copts@CliOpts{reportspec_=rspec}}
                   ,ajournal=j
                   ,aMode=mode
                   }
         ev =
  case mode of
    Help ->
      case ev of
        -- VtyEvent (EvKey (KChar 'q') []) -> halt ui
        VtyEvent (EvKey (KChar 'l') [MCtrl]) -> redraw ui
        VtyEvent (EvKey (KChar 'z') [MCtrl]) -> suspend ui
        _                    -> helpHandle ui ev

    _ -> do
      d <- liftIO getCurrentDay
      let
        (iprev,tprev) = maybe (i,t) ((i-1),) $ lookup (i-1) nts
        (inext,tnext) = maybe (i,t) ((i+1),) $ lookup (i+1) nts
      case ev of
        VtyEvent (EvKey (KChar 'q') []) -> halt ui
        VtyEvent (EvKey KEsc        []) -> continue $ resetScreens d ui
        VtyEvent (EvKey (KChar c)   []) | c `elem` ['?'] -> continue $ setMode Help ui
        VtyEvent (EvKey (KChar 'E') []) -> suspendAndResume $ void (runEditor pos f) >> uiReloadJournalIfChanged copts d j ui
          where
            (pos,f) = case tsourcepos t of
                        GenericSourcePos f l c    -> (Just (l, Just c),f)
                        JournalSourcePos f (l1,_) -> (Just (l1, Nothing),f)
        AppEvent (DateChange old _) | isStandardPeriod p && p `periodContainsDate` old ->
          continue $ regenerateScreens j d $ setReportPeriod (DayPeriod d) ui
          where
            p = reportPeriod ui
        e | e `elem` [VtyEvent (EvKey (KChar 'g') []), AppEvent FileChange] -> do
          -- plog (if e == AppEvent FileChange then "file change" else "manual reload") "" `seq` return ()
          d <- liftIO getCurrentDay
          ej <- liftIO $ journalReload copts
          case ej of
            Left err -> continue $ screenEnter d errorScreen{esError=err} ui
            Right j' -> do
              continue $
                regenerateScreens j' d $
                regenerateTransactions rspec j' s acct i $   -- added (inline) 201512 (why ?)
                clearCostValue $
                ui
        VtyEvent (EvKey (KChar 'I') []) -> continue $ uiCheckBalanceAssertions d (toggleIgnoreBalanceAssertions ui)

        -- for toggles that may change the current/prev/next transactions,
        -- we must regenerate the transaction list, like the g handler above ? with regenerateTransactions ? TODO WIP
        -- EvKey (KChar 'E') [] -> continue $ regenerateScreens j d $ stToggleEmpty ui
        -- EvKey (KChar 'C') [] -> continue $ regenerateScreens j d $ stToggleCleared ui
        -- EvKey (KChar 'R') [] -> continue $ regenerateScreens j d $ stToggleReal ui
        VtyEvent (EvKey (KChar 'B') []) ->
          continue $
          regenerateScreens j d $
          -- regenerateTransactions ropts d j s acct i $
          toggleCost ui
        VtyEvent (EvKey (KChar 'V') []) ->
          continue $
          regenerateScreens j d $
          -- regenerateTransactions ropts d j s acct i $
          toggleValue ui

        VtyEvent e | e `elem` moveUpEvents   -> continue $ regenerateScreens j d ui{aScreen=s{tsTransaction=(iprev,tprev)}}
        VtyEvent e | e `elem` moveDownEvents -> continue $ regenerateScreens j d ui{aScreen=s{tsTransaction=(inext,tnext)}}
        VtyEvent e | e `elem` moveLeftEvents -> continue ui''
          where
            ui'@UIState{aScreen=scr} = popScreen ui
            ui'' = ui'{aScreen=rsSelect (fromIntegral i) scr}
        VtyEvent (EvKey (KChar 'l') [MCtrl]) -> redraw ui
        VtyEvent (EvKey (KChar 'z') [MCtrl]) -> suspend ui
        _ -> continue ui

tsHandle _ _ = error "event handler called with wrong screen type, should not happen"  -- PARTIAL:

-- Got to redo the register screen's transactions report, to get the latest transactions list for this screen.
-- XXX Duplicates rsInit. Why do we have to do this as well as regenerateScreens ?
regenerateTransactions :: ReportSpec -> Journal -> Screen -> AccountName -> Integer -> UIState -> UIState
regenerateTransactions rspec j s acct i ui =
  let
    q = filterQuery (not . queryIsDepth) $ rsQuery rspec
    thisacctq = Acct $ accountNameToAccountRegex acct -- includes subs
    items = reverse $ accountTransactionsReport rspec j q thisacctq
    ts = map first6 items
    numberedts = zip [1..] ts
    -- select the best current transaction from the new list
    -- stay at the same index if possible, or if we are now past the end, select the last, otherwise select the first
    (i',t') = case lookup i numberedts
              of Just t'' -> (i,t'')
                 Nothing | null numberedts -> (0,nulltransaction)
                         | i > fst (last numberedts) -> last numberedts
                         | otherwise -> head numberedts
  in
    ui{aScreen=s{tsTransaction=(i',t')
                ,tsTransactions=numberedts
                ,tsAccount=acct
                }}

-- | Select the nth item on the register screen.
rsSelect i scr@RegisterScreen{..} = scr{rsList=l'}
  where l' = listMoveTo (i-1) rsList
rsSelect _ scr = scr
