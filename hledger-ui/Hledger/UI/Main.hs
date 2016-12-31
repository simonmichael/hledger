{-|
hledger-ui - a hledger add-on providing a curses-style interface.
Copyright (c) 2007-2015 Simon Michael <simon@joyful.com>
Released under GPL version 3 or later.
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Hledger.UI.Main where

-- import Control.Applicative
-- import Lens.Micro.Platform ((^.))
import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad
-- import Control.Monad.IO.Class (liftIO)
import Data.Default (def)
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

import Hledger
import Hledger.Cli hiding (progname,prognameandversion,green)
import Hledger.UI.UIOptions
import Hledger.UI.UITypes
import Hledger.UI.UIState (toggleHistorical)
import Hledger.UI.Theme
import Hledger.UI.AccountsScreen
import Hledger.UI.RegisterScreen

----------------------------------------------------------------------

main :: IO ()
main = do
  opts <- getHledgerUIOpts
  -- when (debug_ $ cliopts_ opts) $ printf "%s\n" prognameandversion >> printf "opts: %s\n" (show opts)
  run opts
    where
      run opts
        | "h"               `inRawOpts` (rawopts_ $ cliopts_ opts) = putStr (showModeUsage uimode) >> exitSuccess
        | "help"            `inRawOpts` (rawopts_ $ cliopts_ opts) = printHelpForTopic (topicForMode uimode) >> exitSuccess
        | "man"             `inRawOpts` (rawopts_ $ cliopts_ opts) = runManForTopic (topicForMode uimode) >> exitSuccess
        | "info"            `inRawOpts` (rawopts_ $ cliopts_ opts) = runInfoForTopic (topicForMode uimode) >> exitSuccess
        | "version"         `inRawOpts` (rawopts_ $ cliopts_ opts) = putStrLn prognameandversion >> exitSuccess
        | "binary-filename" `inRawOpts` (rawopts_ $ cliopts_ opts) = putStrLn (binaryfilename progname)
        | otherwise                                                = withJournalDoUICommand opts runBrickUi

-- XXX withJournalDo specialised for UIOpts
withJournalDoUICommand :: UIOpts -> (UIOpts -> Journal -> IO ()) -> IO ()
withJournalDoUICommand uopts@UIOpts{cliopts_=copts} cmd = do
  rulespath <- rulesFilePathFromOpts copts
  journalpath <- journalFilePathFromOpts copts
  ej <- readJournalFiles Nothing rulespath (not $ ignore_assertions_ copts) journalpath
  either error' (cmd uopts . journalApplyAliases (aliasesFromOpts copts)) ej

runBrickUi :: UIOpts -> Journal -> IO ()
runBrickUi uopts@UIOpts{cliopts_=copts@CliOpts{reportopts_=ropts}} j = do
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
                    [v | (k,v) <- rawopts_ copts, k=="args", not $ any (`isPrefixOf` v) ["depth","date"]],
            -- always disable boring account name eliding, unlike the CLI, for a more regular tree
            no_elide_=True,
            -- show items with zero amount by default, unlike the CLI
            empty_=True,
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
                  asInit d True $
                  UIState{
                    aopts=uopts'
                   ,ajournal=j
                   ,aScreen=asSetSelectedAccount acct accountsScreen
                   ,aPrevScreens=[]
                   ,aMode=Normal
                   }
  
    ui =
      (sInit scr) d True $
        (if change_ uopts' then toggleHistorical else id) $ -- XXX
        UIState{
          aopts=uopts'
         ,ajournal=j
         ,aScreen=scr
         ,aPrevScreens=prevscrs
         ,aMode=Normal
         }

    brickapp :: App (UIState) AppEvent Name
    brickapp = App {
        appStartEvent   = return
      , appAttrMap      = const theme
      , appChooseCursor = showFirstCursor
      , appHandleEvent  = \ui ev -> sHandle (aScreen ui) ui ev
      , appDraw         = \ui    -> sDraw   (aScreen ui) ui
      }

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
      $ \_ -> do

      -- start one or more background threads reporting changes in the directories of our files
      -- XXX many quick successive saves causes the problems listed in BUGS
      -- with Debounce increased to 1s it easily gets stuck on an error or blank screen
      -- until you press g, but it becomes responsive again quickly.
      -- withManagerConf defaultConfig{confDebounce=Debounce 1} $ \mgr -> do
      -- with Debounce at the default 1ms it clears transient errors itself
      -- but gets tied up for ages
      withManager $ \mgr -> do
        dbg1IO "fsnotify using polling ?" $ isPollingManager mgr
        files <- mapM canonicalizePath $ map fst $ jfiles j
        let directories = nub $ sort $ map takeDirectory files
        dbg1IO "files" files
        dbg1IO "directories to watch" directories

        forM_ directories $ \d -> watchDir
          mgr
          d
          -- predicate: ignore changes not involving our files
          (\fev -> case fev of
            Modified f _ -> f `elem` files
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
            dbg1IO "fsnotify" $ showFSNEvent fev
            writeChan eventChan FileChange
            )

        -- and start the app. Must be inside the withManager block
        void $ customMain (mkVty def) (Just eventChan) brickapp ui

showFSNEvent (Added    f _) = "Added "    ++ show f
showFSNEvent (Modified f _) = "Modified " ++ show f
showFSNEvent (Removed  f _) = "Removed "  ++ show f
