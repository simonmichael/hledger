{-|
hledger-ui - a hledger add-on providing a curses-style interface.
Copyright (c) 2007-2015 Simon Michael <simon@joyful.com>
Released under GPL version 3 or later.
-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Hledger.UI.Main where

-- import Control.Applicative
-- import Lens.Micro.Platform ((^.))
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async
import Control.Monad
-- import Control.Monad.IO.Class (liftIO)
#if !MIN_VERSION_vty(5,15,0)
import Data.Default (def)
#endif
-- import Data.Monoid              --
import Data.List
import Data.Maybe
-- import Data.Text (Text)
import qualified Data.Text as T
-- import Data.Time.Calendar
import Graphics.Vty (mkVty)
import Safe
import System.Exit
import System.Directory
import System.FilePath
import System.FSNotify
import Brick

#if MIN_VERSION_brick(0,16,0)
import qualified Brick.BChan as BC
#else
import Control.Concurrent.Chan (newChan, writeChan)
#endif

import Hledger
import Hledger.Cli hiding (progname,prognameandversion)
import Hledger.UI.UIOptions
import Hledger.UI.UITypes
import Hledger.UI.UIState (toggleHistorical)
import Hledger.UI.Theme
import Hledger.UI.AccountsScreen
import Hledger.UI.RegisterScreen

----------------------------------------------------------------------

#if MIN_VERSION_brick(0,16,0)
newChan :: IO (BC.BChan a)
newChan = BC.newBChan 10

writeChan :: BC.BChan a -> a -> IO ()
writeChan = BC.writeBChan
#endif


main :: IO ()
main = do
  opts <- getHledgerUIOpts
  let copts = cliopts_ opts
      copts' = copts
        { inputopts_ = (inputopts_ copts) { auto_ = True }
        , reportopts_ = (reportopts_ copts) { forecast_ = True }
        }

  -- when (debug_ $ cliopts_ opts) $ printf "%s\n" prognameandversion >> printf "opts: %s\n" (show opts)
  run $ opts { cliopts_ = copts' }
    where
      run opts
        | "help"            `inRawOpts` (rawopts_ $ cliopts_ opts) = putStr (showModeUsage uimode) >> exitSuccess
        | "version"         `inRawOpts` (rawopts_ $ cliopts_ opts) = putStrLn prognameandversion >> exitSuccess
        | "binary-filename" `inRawOpts` (rawopts_ $ cliopts_ opts) = putStrLn (binaryfilename progname)
        | otherwise                                                = withJournalDo (cliopts_ opts) (runBrickUi opts)

runBrickUi :: UIOpts -> Journal -> IO ()
runBrickUi uopts@UIOpts{cliopts_=copts@CliOpts{inputopts_=_iopts,reportopts_=ropts}} j = do
  d <- getCurrentDay

  let

    -- depth: is a bit different from other queries. In hledger cli,
    -- - reportopts{depth_} indicates --depth options
    -- - reportopts{query_} is the query arguments as a string
    -- - the report query is based on both of these.
    -- For hledger-ui, for now, move depth: arguments out of reportopts{query_}
    -- and into reportopts{depth_}, so that depth and other kinds of filter query
    -- can be displayed independently.
    uopts' = uopts{
      cliopts_=copts{
         reportopts_= ropts{
            -- incorporate any depth: query args into depth_,
            -- any date: query args into period_
            depth_ =depthfromoptsandargs,
            period_=periodfromoptsandargs,
            query_ =unwords -- as in ReportOptions, with same limitations
                    [quoteIfNeeded v | (k,v) <- rawopts_ copts, k=="args", not $ any (`isPrefixOf` v) ["depth","date"]],
            -- always disable boring account name eliding, unlike the CLI, for a more regular tree
            no_elide_=True,
            -- flip the default for items with zero amounts, show them by default
            empty_=not $ empty_ ropts,
            -- show historical balances by default, unlike the CLI
            balancetype_=HistoricalBalance
            }
         }
      }
      where
        q = queryFromOpts d ropts
        depthfromoptsandargs = case queryDepth q of 99999 -> Nothing
                                                    d     -> Just d
        datespanfromargs = queryDateSpan (date2_ ropts) $ fst $ parseQuery d (T.pack $ query_ ropts)
        periodfromoptsandargs =
          dateSpanAsPeriod $ spansIntersect [periodAsDateSpan $ period_ ropts, datespanfromargs]

    -- XXX move this stuff into Options, UIOpts
    theme = maybe defaultTheme (fromMaybe defaultTheme . getTheme) $
            maybestringopt "theme" $ rawopts_ copts
    mregister = maybestringopt "register" $ rawopts_ copts

    (scr, prevscrs) = case mregister of
      Nothing   -> (accountsScreen, [])
      -- with --register, start on the register screen, and also put
      -- the accounts screen on the prev screens stack so you can exit
      -- to that as usual.
      Just apat -> (rsSetAccount acct False registerScreen, [ascr'])
        where
          acct = headDef
                 (error' $ "--register "++apat++" did not match any account")
                 $ filter (regexMatches apat . T.unpack) $ journalAccountNames j
          -- Initialising the accounts screen is awkward, requiring
          -- another temporary UIState value..
          ascr' = aScreen $
                  asInit d True
                    UIState{
                      aopts=uopts'
                    ,ajournal=j
                    ,aScreen=asSetSelectedAccount acct accountsScreen
                    ,aPrevScreens=[]
                    ,aMode=Normal
                    }

    ui =
      (sInit scr) d True $
        (if change_ uopts' then toggleHistorical else id) -- XXX
          UIState{
            aopts=uopts'
          ,ajournal=j
          ,aScreen=scr
          ,aPrevScreens=prevscrs
          ,aMode=Normal
          }

    brickapp :: App UIState AppEvent Name
    brickapp = App {
        appStartEvent   = return
      , appAttrMap      = const theme
      , appChooseCursor = showFirstCursor
      , appHandleEvent  = \ui ev -> sHandle (aScreen ui) ui ev
      , appDraw         = \ui    -> sDraw   (aScreen ui) ui
      }

  -- print (length (show ui)) >> exitSuccess  -- show any debug output to this point & quit

  if not (watch_ uopts')
  then
    void $ defaultMain brickapp ui

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
      (getCurrentDay >>= watchDate)
      $ \_ ->

      -- start one or more background threads reporting changes in the directories of our files
      -- XXX many quick successive saves causes the problems listed in BUGS
      -- with Debounce increased to 1s it easily gets stuck on an error or blank screen
      -- until you press g, but it becomes responsive again quickly.
      -- withManagerConf defaultConfig{confDebounce=Debounce 1} $ \mgr -> do
      -- with Debounce at the default 1ms it clears transient errors itself
      -- but gets tied up for ages
      withManager $ \mgr -> do
        dbg1IO "fsnotify using polling ?" $ isPollingManager mgr
        files <- mapM (canonicalizePath . fst) $ jfiles j
        let directories = nub $ sort $ map takeDirectory files
        dbg1IO "files" files
        dbg1IO "directories to watch" directories

        forM_ directories $ \d -> watchDir
          mgr
          d
          -- predicate: ignore changes not involving our files
          (\fev -> case fev of
#if MIN_VERSION_fsnotify(0,3,0)
            Modified f _ False
#else
            Modified f _
#endif
                               -> f `elem` files
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

        -- and start the app. Must be inside the withManager block
#if MIN_VERSION_vty(5,15,0)
        let mkvty = mkVty mempty
#else
        let mkvty = mkVty def
#endif
#if MIN_VERSION_brick(0,47,0)
        vty0 <- mkvty
        void $ customMain vty0 mkvty (Just eventChan) brickapp ui
#else
        void $ customMain mkvty (Just eventChan) brickapp ui
#endif
