{-# LANGUAGE CPP #-}
{- |

hledger-ui - a hledger add-on providing a curses-style interface.
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
import qualified Data.Text as T
import Data.Time
import qualified Data.Vector as V
import qualified Data.Set as S
import Graphics.Vty (mkVty, Mode (Mouse), outputIface, Vty (outputIface))
import Graphics.Vty.Platform.Unix (mkVty)
import Graphics.Vty.Platform.Unix.Output (UnixOutput (termUnix))  -- XXX for the custom output device feature. We should probably drop this.
import System.Directory (canonicalizePath)
import System.Environment (withProgName)
import System.FilePath
import System.FSNotify
import System.FSNotify.Devel
import qualified System.FSNotify as FSNotify
import Text.Printf

import Hledger
import Hledger.Cli hiding (progname,prognameandversion)
import Hledger.UI.Theme
import Hledger.UI.UIOptions
import Hledger.UI.UITypes
import Hledger.UI.UITypes
import Hledger.UI.UIUtils

#if MIN_VERSION_fsnotify(0,4,0)
import Control.Concurrent (threadDelay)
#endif


----------------------------------------------------------------------
-- Command
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
  -- And when it's not, disable colour in the TUI ?
  -- Theme.hs's themes currently hard code various colours and styles provided by vty,
  -- which probably are disabled automatically when terminal doesn't support them.
  -- But we'll at least force hledger-ui's theme to a monochrome one.
  let opts = if usecolor then opts1 else opts1{uoTheme=Just "terminal"}

  -- always generate forecasted periodic transactions; their visibility will be toggled by the UI.
  let copts' = copts{inputopts_=iopts{forecast_=forecast_ iopts <|> Just nulldatespan}}

  -- always load the journal with full lot detail retained; the UI collapses it for display
  -- (toggled by the L key), so the uncollapsed journal stays available without re-reading files.
  let loadcopts = copts'{rawopts_ = setboolopt "lots" (rawopts_ copts')}

  case True of
    _ | boolopt "help"    rawopts -> runPager $ showModeUsage uimode ++ "\n"
    _ | boolopt "tldr"    rawopts -> runTldrForPage "hledger-ui"
    _ | boolopt "info"    rawopts -> runInfoForTopic "hledger-ui" Nothing
    _ | boolopt "man"     rawopts -> runManForTopic  "hledger-ui" Nothing
    _ | boolopt "version" rawopts -> putStrLn prognameandversion
    -- _ | boolopt "binary-filename" rawopts -> putStrLn (binaryfilename progname)
    _                                         -> withJournal loadcopts $ \j ->
        -- Refresh the startup ReportSpec against the loaded journal so any
        -- cur: terms are expanded for the journal's commodity aliases.
        let opts' = case reportSpecExpandCurQueries j (reportspec_ copts') of
                      Right rs -> opts{uoCliOpts = (uoCliOpts opts){reportspec_ = rs}}
                      Left _   -> opts
        in runBrickUi opts' j

  when (ghcDebugMode == GDPauseAtEnd) $ ghcDebugPause'

-- | Build hledger-ui's startup state: normalise the options, choose the initial
-- screen, and set up the stack of previous screens as if the user had navigated
-- down to it from the menu. Uses the startup report date (@copts^.rsDay@), so it is
-- deterministic and reusable outside the brick app (eg from tests). Keep synced with msNew.
uiInitialState :: UIOpts -> Journal -> UIState
uiInitialState uopts0@UIOpts{uoCliOpts=copts@CliOpts{reportspec_=rspec@ReportSpec{_rsReportOpts=ropts}}} j =
  uiState uopts j prevscrs currscr
  where
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

    -- The journal collapsed for display per the current lots toggle; the initial screens
    -- are built from it, while uiState keeps the uncollapsed journal for the L toggle.
    jdisplay = uiDisplayJournal uopts j

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
              . firstMatch $ journalAccountNamesDeclaredOrImplied jdisplay
              where
                firstMatch = case toRegexCI $ T.pack apat of
                    Right re -> find (regexMatchText re)
                    Left  _  -> const Nothing

            -- the register screen for acct
            regscr = 
              rsSetAccount acct False $
              rsNew uopts today jdisplay acct forceinclusive
                where
                  -- Take the depth from uopts, not `getDepth ui`: ui depends on regscr
                  -- (this binding), so referencing ui here ties a knot that StrictData's
                  -- strict UIState fields turn into a <<loop>> at startup (#1825).
                  forceinclusive = case dsFlatDepth (depth_ regropts) of
                                    Just de -> accountNameLevel acct >= de
                                    Nothing -> False
                    where regropts = _rsReportOpts $ reportspec_ $ uoCliOpts uopts

            -- The accounts screen containing acct.
            -- Keep these selidx values synced with the menu items in msNew.
            (acctsscr, selidx) =
          -- 2. the file(s) to watch
          -- 3. the event handler
          -- 4. the action to run after stopping the watcher
#if MIN_VERSION_fsnotify(0,4,0)
          withManagerConf defaultConfig{confDebounce=NoDebounce} $ \mgr -> do
            stop <- watchDir
              mgr
            -- the menu screen
              (const True)
              (\ev -> do
                  -- debug logging
                  -- hledger: ui: fsnotify event: ...
                  when (debug_ $ cliopts_ uopts0) $ do
                    here <- getCurrentDirectory
                    hPutStrLn stderr $ "hledger: ui: fsnotify event: " ++ show ev
                  -- try to avoid excessive reloading by ignoring events not involving our files
                  let eventFile = case ev of
                        FSNotify.Action event _ _ -> eventPath event
                        FSNotify.Removed _ fp _ -> fp
                        FSNotify.Unknown _ fp _ -> fp
                  ourfiles <- readIORef jref >>= return . map (takeFileName . fst) . jfiles
                  when (takeFileName eventFile `elem` ourfiles) $ do
                    writeIORef changeref True
                    writeBChan bchan (ReloadJournalOnChange $ Just ev)
              )
            return stop
#else
          withManagerConf defaultConfig{confDebounce=NoDebounce} $ \mgr -> do
            stop <- watchDir
              mgr
              (takeDirectory $ fst $ head $ jfiles j)
              (const True)
              (\ev -> do
                  -- debug logging
                  -- hledger: ui: fsnotify event: ...
                  when (debug_ $ cliopts_ uopts0) $ do
                    here <- getCurrentDirectory
        where
          menuscr     = msNew
          allacctsscr = asNew AllAccounts             uopts today jdisplay Nothing
          csacctsscr  = asNew CashAccounts            uopts today jdisplay Nothing
          bsacctsscr  = asNew BalancesheetAccounts    uopts today jdisplay Nothing
                    writeBChan bchan (ReloadJournalOnChange $ Just ev)
              )
            return stop
#endif

        -- And start a thread to wait for data changes and generate app events.
        -- To avoid excessive reloading, we ignore events not involving our files.
  let
    ui  = uiInitialState uopts0 j
    app = brickApp (uoTheme uopts0)
        -- XXX does not catch all changes, eg with git
        -- XXX should catch "journal file(s) changed" and show in UI
        -- XXX catch exceptions, don't crash
#if !MIN_VERSION_fsnotify(0,4,0)
        -- XXX maybe use withAsync here
        _ <- forkIO $
          bracket
      v <- mkVty mempty
      setMode (outputIface v) Mouse True
      return v

  if not (uoWatch uopts0)
  then do
    vty <- makevty
    void $ customMain vty makevty Nothing app ui

  else do
    -- a channel for sending misc. events to the app
    eventChan <- newChan

    -- start a background thread reporting changes in the current date
    -- use async for proper child termination in GHCI
              -- threadDelay 1000000
              -- return ()
            )
#endif

        -- Run the brick UI (with the terminal's output channel as a custom output device).
        -- This blocks until the user quits.
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
            Added f _ IsFile -> f `elem` files -- for editors which write the whole file from scratch on saves
            Modified f _ IsFile -> f `elem` files -- for editors which modify existing files in place
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
    MS sst -> msHandle sst ev
    AS sst -> asHandle sst ev
    RS sst -> rsHandle sst ev
    TS sst -> tsHandle sst ev
    ES sst -> esHandle sst ev

uiDraw :: UIState -> [Widget Name]
uiDraw ui =
  case aScreen ui of
    MS sst -> msDraw sst ui
    AS sst -> asDraw sst ui
    RS sst -> rsDraw sst ui
    TS sst -> tsDraw sst ui
    ES sst -> esDraw sst ui
