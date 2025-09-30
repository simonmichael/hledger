{-|
hledger-ui - a hledger add-on providing an efficient TUI.

SPDX-License-Identifier: GPL-3.0-or-later
Copyright (c) 2007-2025 (each year in this range) Simon Michael <simon@joyful.com> and contributors.

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
See the GNU General Public License for more details.
You should have received a copy of the GNU General Public License along with this program.
If not, see <https://www.gnu.org/licenses/>.

-}

{-# LANGUAGE CPP                   #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE MultiWayIf #-}

module Hledger.UI.Main where

import Control.Applicative ((<|>))
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (withAsync)
#if MIN_VERSION_base(4,20,0)
import Control.Exception.Backtrace (setBacktraceMechanismState, BacktraceMechanism(..))
#endif
import Control.Monad (forM_, void, when)
import Data.Bifunctor (first)
import Data.Function ((&))
import Data.List (find)
import Data.List.Extra (nubSort)
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Graphics.Vty (Mode (Mouse), Vty (outputIface), Output (setMode))
import Graphics.Vty.CrossPlatform (mkVty)
import Lens.Micro ((^.))
import System.Directory (canonicalizePath)
import System.Environment (withProgName)
import System.FilePath (takeDirectory)
import System.FSNotify (Event(Modified), watchDir, withManager, EventIsDirectory (IsFile))
import Brick hiding (bsDraw)
import Brick.BChan qualified as BC

import Hledger
import Hledger.Cli hiding (progname,prognameandversion)
import Hledger.UI.Theme
import Hledger.UI.UIOptions
import Hledger.UI.UITypes
import Hledger.UI.UIState (uiState, getDepth)
import Hledger.UI.UIUtils (dbguiEv, showScreenStack, showScreenSelection)
import Hledger.UI.MenuScreen
import Hledger.UI.AccountsScreen
import Hledger.UI.CashScreen
import Hledger.UI.BalancesheetScreen
import Hledger.UI.IncomestatementScreen
import Hledger.UI.RegisterScreen
import Hledger.UI.TransactionScreen
import Hledger.UI.ErrorScreen
import Hledger.UI.UIScreens


----------------------------------------------------------------------

newChan :: IO (BC.BChan a)
newChan = BC.newBChan 10

writeChan :: BC.BChan a -> a -> IO ()
writeChan = BC.writeBChan


hledgerUiMain :: IO ()
hledgerUiMain = handleExit $ withGhcDebug' $ withProgName "hledger-ui.log" $ do  -- force Hledger.Utils.Debug.* to log to hledger-ui.log
  when (ghcDebugMode == GDPauseAtStart) $ ghcDebugPause'

#if MIN_VERSION_base(4,20,0)
  -- Control ghc 9.10+'s stack traces.
  -- CostCentreBacktrace   - collect cost-centre stack backtraces (only available when built with profiling)
  -- HasCallStackBacktrace - collect HasCallStack backtraces
  -- ExecutionBacktrace    - collect backtraces from native execution stack unwinding
  -- IPEBacktrace          - collect backtraces from Info Table Provenance Entries
#ifdef DEBUG
  setBacktraceMechanismState HasCallStackBacktrace True
#else
  setBacktraceMechanismState HasCallStackBacktrace False
#endif
#endif

  dbg1MsgIO "\n\n\n\n==== hledger-ui start"
  dbg1IO "args" progArgs
  dbg1IO "debugLevel" debugLevel

  opts1@UIOpts{uoCliOpts=copts@CliOpts{inputopts_=iopts,rawopts_=rawopts}} <- getHledgerUIOpts
  -- when (debug_ $ cliopts_ opts) $ printf "%s\n" prognameandversion >> printf "opts: %s\n" (show opts)

  usecolor <- useColorOnStdout
  -- When ANSI colour/styling is available and enabled, encourage user's $PAGER to use it (for command line help).
  when usecolor setupPager
  -- And when it's not, disable colour in the TUI ?
  -- Theme.hs's themes currently hard code various colours and styles provided by vty,
  -- which probably are disabled automatically when terminal doesn't support them.
  -- But we'll at least force hledger-ui's theme to a monochrome one.
  let opts = if usecolor then opts1 else opts1{uoTheme=Just "terminal"}

  -- always generate forecasted periodic transactions; their visibility will be toggled by the UI.
  let copts' = copts{inputopts_=iopts{forecast_=forecast_ iopts <|> Just nulldatespan}}

  case True of
    _ | boolopt "help"    rawopts -> runPager $ showModeUsage uimode ++ "\n"
    _ | boolopt "tldr"    rawopts -> runTldrForPage "hledger-ui"
    _ | boolopt "info"    rawopts -> runInfoForTopic "hledger-ui" Nothing
    _ | boolopt "man"     rawopts -> runManForTopic  "hledger-ui" Nothing
    _ | boolopt "version" rawopts -> putStrLn prognameandversion
    -- _ | boolopt "binary-filename" rawopts -> putStrLn (binaryfilename progname)
    _                                         -> withJournal copts' (runBrickUi opts)

  when (ghcDebugMode == GDPauseAtEnd) $ ghcDebugPause'

runBrickUi :: UIOpts -> Journal -> IO ()
runBrickUi uopts0@UIOpts{uoCliOpts=copts@CliOpts{inputopts_=_iopts,reportspec_=rspec@ReportSpec{_rsReportOpts=ropts}}} j =
  do
  let
    today = copts^.rsDay

    -- hledger-ui's query handling is currently in flux, mixing old and new approaches.
    -- Related: #1340, #1383, #1387. Some notes and terminology:

    -- The *startup query* is the Query generated at program startup, from
    -- command line options, arguments, and the current date. hledger CLI
    -- uses this.

    -- hledger-ui/hledger-web allow the query to be changed at will, creating
    -- a new *runtime query* each time.

    -- The startup query or part of it can be used as a *constraint query*,
    -- limiting all runtime queries. hledger-web does this with the startup
    -- report period, never showing transactions outside those dates.
    -- hledger-ui does not do this.

    -- A query is a combination of multiple subqueries/terms, which are
    -- generated from command line options and arguments, ui/web app runtime
    -- state, and/or the current date.

    -- Some subqueries are generated by parsing freeform user input, which
    -- can fail. We don't want hledger users to see such failures except:

    -- 1. at program startup, in which case the program exits
    -- 2. after entering a new freeform query in hledger-ui/web, in which case
    --    the change is rejected and the program keeps running

    -- So we should parse those kinds of subquery only at those times. Any
    -- subqueries which do not require parsing can be kept separate. And
    -- these can be combined to make the full query when needed, eg when
    -- hledger-ui screens are generating their data. (TODO)

    -- Some parts of the query are also kept separate for UI reasons.
    -- hledger-ui provides special UI for controlling depth (number keys), 
    -- the report period (shift arrow keys), realness/status filters (RUPC keys) etc.
    -- There is also a freeform text area for extra query terms (/ key).
    -- It's cleaner and less conflicting to keep the former out of the latter.

    uopts = uopts0{
      uoCliOpts=copts{
         reportspec_=rspec{
            _rsQuery=filteredQuery $ _rsQuery rspec,  -- query with depth/date parts removed
            _rsReportOpts=ropts{
               depth_    = queryDepth $ _rsQuery rspec,  -- query's depth part
               period_   = periodfromoptsandargs,       -- query's date part
               no_elide_ = True,  -- avoid squashing boring account names, for a more regular tree (unlike hledger)
               empty_    = not $ empty_ ropts,  -- show zero items by default, hide them with -E (unlike hledger)
               declared_ = True  -- always show declared accounts even if unused
               }
            }
         }
      }
      where
        datespanfromargs = queryDateSpan (date2_ ropts) $ _rsQuery rspec
        periodfromoptsandargs =
          dateSpanAsPeriod $ spansIntersect [periodAsDateSpan $ period_ ropts, datespanfromargs]
        filteredQuery q = simplifyQuery $ And [queryFromFlags ropts, filtered q]
          where filtered = filterQuery (\x -> not $ queryIsDepth x || queryIsDate x)

    -- Choose the initial screen to display.
    -- We also set up a stack of previous screens, as if you had navigated down to it from the top.
    -- Note the previous screens list is ordered nearest-first, with the top-most (menu) screen last.
    -- Keep all of this synced with msNew.
    rawopts = rawopts_ $ uoCliOpts $ uopts
    (prevscrs, currscr) =
      dbg1With (showScreenStack "initial" showScreenSelection . uncurry2 (uiState defuiopts nulljournal)) $
      if
        -- An accounts screen is specified. Its previous screen will be the menu screen with it selected.
        | boolopt "cash" rawopts -> ([msSetSelectedScreen csItemIndex menuscr], csacctsscr)
        | boolopt "bs"   rawopts -> ([msSetSelectedScreen bsItemIndex menuscr], bsacctsscr)
        | boolopt "is"   rawopts -> ([msSetSelectedScreen isItemIndex menuscr], isacctsscr)
        | boolopt "all"  rawopts -> ([msSetSelectedScreen asItemIndex menuscr], allacctsscr)

        -- A register screen is specified with --register=ACCT. The initial screen stack will be:
        --
        --   menu screen, with ACCTSSCR selected
        --    ACCTSSCR (the accounts screen containing ACCT), with ACCT selected
        --     register screen for ACCT
        --
        | Just apat <- uoRegister uopts ->
          let
            -- the account being requested
            acct = fromMaybe (error' $ "--register "++apat++" did not match any account")  -- PARTIAL:
              . firstMatch $ journalAccountNamesDeclaredOrImplied j
              where
                firstMatch = case toRegexCI $ T.pack apat of
                    Right re -> find (regexMatchText re)
                    Left  _  -> const Nothing

            -- the register screen for acct
            regscr = 
              rsSetAccount acct False $
              rsNew uopts today j acct forceinclusive
                where
                  forceinclusive = case getDepth ui of
                                    Just de -> accountNameLevel acct >= de
                                    Nothing -> False

            -- The accounts screen containing acct.
            -- Keep these selidx values synced with the menu items in msNew.
            (acctsscr, selidx) =
              case journalAccountType j acct of
                Just t | isBalanceSheetAccountType t    -> (bsacctsscr, 1)
                Just t | isIncomeStatementAccountType t -> (isacctsscr, 2)
                _                                       -> (allacctsscr,0)
              & first (asSetSelectedAccount acct)

            -- the menu screen
            menuscr' = msSetSelectedScreen selidx menuscr
          in ([acctsscr, menuscr'], regscr)

        -- Otherwise, start on the menu screen.
        | otherwise -> ([], menuscr)

        where
          menuscr     = msNew
          allacctsscr = asNew uopts today j Nothing
          csacctsscr  = csNew uopts today j Nothing
          bsacctsscr  = bsNew uopts today j Nothing
          isacctsscr  = isNew uopts today j Nothing

    ui = uiState uopts j prevscrs currscr
    app = brickApp (uoTheme uopts)

  -- print (length (show ui)) >> exitSuccess  -- show any debug output to this point & quit

  let 
    -- helper: make a Vty terminal controller with mouse support enabled
    makevty = do
      v <- mkVty mempty
      setMode (outputIface v) Mouse True
      return v

  if not (uoWatch uopts)
  then do
    vty <- makevty
    void $ customMain vty makevty Nothing app ui

  else do
    -- a channel for sending misc. events to the app
    eventChan <- newChan

    -- start a background thread reporting changes in the current date
    -- use async for proper child termination in GHCI
    let
      watchDate old = do
        threadDelay 1000000 -- 1 s
        new <- getCurrentDay
        when (new /= old) $ do
          let dc = DateChange old new
          -- dbg1IO "datechange" dc -- XXX don't uncomment until dbg*IO fixed to use traceIO, GHC may block/end thread
          -- traceIO $ show dc
          writeChan eventChan dc
        watchDate new

    withAsync
      -- run this small task asynchronously:
      (getCurrentDay >>= watchDate)
      -- until this main task terminates:
      $ \_async ->
      -- start one or more background threads reporting changes in the directories of our files
      -- XXX many quick successive saves causes the problems listed in BUGS
      -- with Debounce increased to 1s it easily gets stuck on an error or blank screen
      -- until you press g, but it becomes responsive again quickly.
      -- withManagerConf defaultConfig{confDebounce=Debounce 1} $ \mgr -> do
      -- with Debounce at the default 1ms it clears transient errors itself
      -- but gets tied up for ages
      withManager $ \mgr -> do
        files <- mapM (canonicalizePath . fst) $ jfiles j
        let directories = nubSort $ map takeDirectory files
        dbg1IO "files" files
        dbg1IO "directories to watch" directories

        forM_ directories $ \d -> watchDir
          mgr
          d
          -- predicate: ignore changes not involving our files
          (\case
            Modified f _ IsFile -> f `elem` files
            -- Added    f _ -> f `elem` files
            -- Removed  f _ -> f `elem` files
            -- we don't handle adding/removing journal files right now
            -- and there might be some of those events from tmp files
            -- clogging things up so let's ignore them
            _ -> False
            )
          -- action: send event to app
          (\fev -> do
            -- return $ dbglog "fsnotify" $ showFSNEvent fev -- not working
            dbg1IO "fsnotify" $ show fev
            writeChan eventChan FileChange
            )

        -- and start the app. Must be inside the withManager block. (XXX makevty too ?)
        vty <- makevty
        void $ customMain vty makevty (Just eventChan) app ui

brickApp :: Maybe String -> App UIState AppEvent Name
brickApp mtheme = App {
    appStartEvent   = return ()
  , appAttrMap      = const $ fromMaybe defaultTheme $ getTheme =<< mtheme
  , appChooseCursor = showFirstCursor
  , appHandleEvent  = uiHandle
  , appDraw         = uiDraw
  }

uiHandle :: BrickEvent Name AppEvent -> EventM Name UIState ()
uiHandle ev = do
  dbguiEv $ "\n==== " ++ show ev
  ui <- get
  case aScreen ui of
    MS _ -> msHandle ev
    AS _ -> asHandle ev
    CS _ -> csHandle ev
    BS _ -> bsHandle ev
    IS _ -> isHandle ev
    RS _ -> rsHandle ev
    TS _ -> tsHandle ev
    ES _ -> esHandle ev

uiDraw :: UIState -> [Widget Name]
uiDraw ui =
  case aScreen ui of
    MS _ -> msDraw ui
    AS _ -> asDraw ui
    CS _ -> csDraw ui
    BS _ -> bsDraw ui
    IS _ -> isDraw ui
    RS _ -> rsDraw ui
    TS _ -> tsDraw ui
    ES _ -> esDraw ui
