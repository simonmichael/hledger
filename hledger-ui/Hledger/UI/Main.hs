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
-- import Lens.Micro ((^.))
import Control.Monad
-- import Control.Monad.IO.Class (liftIO)
-- import Data.Default
-- import Data.Monoid              -- 
import Data.List
import Data.Maybe
-- import Data.Text (Text)
import qualified Data.Text as T
-- import Data.Time.Calendar
import Safe
import System.Exit

import qualified Graphics.Vty as V
import Brick

import Hledger
import Hledger.Cli hiding (progname,prognameandversion,green)
import Hledger.UI.UIOptions
import Hledger.UI.UITypes
-- import Hledger.UI.UIUtils
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
            -- ensure depth_ also reflects depth: args
            depth_=depthfromoptsandargs,
            -- remove depth: args from query_
            query_=unwords $ -- as in ReportOptions, with same limitations
                   [v | (k,v) <- rawopts_ copts, k=="args", not $ "depth" `isPrefixOf` v],
            -- show items with zero amount by default, unlike the CLI
            empty_=True
            }
         }
      }
      where
        q = queryFromOpts d ropts
        depthfromoptsandargs = case queryDepth q of 99999 -> Nothing
                                                    d     -> Just d

    -- XXX move this stuff into Options, UIOpts
    theme = maybe defaultTheme (fromMaybe defaultTheme . getTheme) $
            maybestringopt "theme" $ rawopts_ copts
    mregister = maybestringopt "register" $ rawopts_ copts

    (scr, prevscrs) = case mregister of
      Nothing   -> (accountsScreen, [])
      -- with --register, start on the register screen, and also put
      -- the accounts screen on the prev screens stack so you can exit
      -- to that as usual.
      Just apat -> (rsSetAccount acct registerScreen, [ascr'])
        where
          acct = headDef
                 (error' $ "--register "++apat++" did not match any account")
                 $ filter (regexMatches apat . T.unpack) $ journalAccountNames j
          -- Initialising the accounts screen is awkward, requiring
          -- another temporary AppState value..
          ascr' = aScreen $
                  asInit d True $
                  AppState{
                    aopts=uopts'
                   ,ajournal=j
                   ,aScreen=asSetSelectedAccount acct accountsScreen
                   ,aPrevScreens=[]
                   ,aMinibuffer=Nothing
                   }
  
    st = (sInit scr) d True
         AppState{
            aopts=uopts'
           ,ajournal=j
           ,aScreen=scr
           ,aPrevScreens=prevscrs
           ,aMinibuffer=Nothing
           }

    brickapp :: App (AppState) V.Event
    brickapp = App {
        appLiftVtyEvent = id
      , appStartEvent   = return
      , appAttrMap      = const theme
      , appChooseCursor = showFirstCursor
      , appHandleEvent  = \st ev -> sHandle (aScreen st) st ev
      , appDraw         = \st    -> sDraw   (aScreen st) st
         -- XXX bizarro. removing the st arg and parameter above,
         -- which according to GHCI does not change the type,
         -- causes "Exception: draw function called with wrong screen type"
         -- on entering a register. Likewise, removing the st ev args and parameters
         -- causes an exception on exiting a register.
      }

  void $ defaultMain brickapp st

