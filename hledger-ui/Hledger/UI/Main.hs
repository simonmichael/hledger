{-|
hledger-ui - a hledger add-on providing a curses-style interface.
Copyright (c) 2007-2015 Simon Michael <simon@joyful.com>
Released under GPL version 3 or later.

TODO:
reg: don't repeat date/description for postings in same txn
reg: show a hledger-web-style register
--
switch to next brick release
reg: use full width
reg: keep cursor at bottom of screen when jumping to end
page up/down
home/end
search
filter
--
show journal entries
add
edit
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Hledger.UI.Main where

-- import Control.Applicative
-- import Control.Lens ((^.))
import Control.Monad
-- import Data.Default
-- import Data.Monoid              -- 
-- import Data.List
-- import Data.Maybe
-- import Data.Time.Calendar
-- import Safe
import System.Exit

import qualified Graphics.Vty as V
import Brick

import Hledger
import Hledger.Cli hiding (progname,prognameandversion,green)
import Hledger.UI.Options
import Hledger.UI.UITypes
import Hledger.UI.UIUtils
import Hledger.UI.AccountsScreen as AS
import Hledger.UI.RegisterScreen as RS

----------------------------------------------------------------------

-- | The available screens.
appScreens = [
   AS.screen
  ,RS.screen
  ]

main :: IO ()
main = do
  opts <- getHledgerUIOpts
  -- when (debug_ $ cliopts_ opts) $ printf "%s\n" prognameandversion >> printf "opts: %s\n" (show opts)
  run opts
    where
      run opts
        | "help" `inRawOpts` (rawopts_ $ cliopts_ opts)            = putStr (showModeHelp uimode) >> exitSuccess
        | "version" `inRawOpts` (rawopts_ $ cliopts_ opts)         = putStrLn prognameandversion >> exitSuccess
        | "binary-filename" `inRawOpts` (rawopts_ $ cliopts_ opts) = putStrLn (binaryfilename progname)
        | otherwise                                                = withJournalDo' opts runBrickUi

withJournalDo' :: UIOpts -> (UIOpts -> Journal -> IO ()) -> IO ()
withJournalDo' opts cmd = do
  -- journalFilePathFromOpts (cliopts_ opts) >>= readJournalFile Nothing >>=
  --   either error' (cmd opts . journalApplyAliases (aliasesFromOpts $ cliopts_ opts))
  -- XXX head should be safe for now
  (head `fmap` journalFilePathFromOpts (cliopts_ opts)) >>= readJournalFile Nothing Nothing True >>=
    either error' (cmd opts . journalApplyAliases (aliasesFromOpts $ cliopts_ opts))

runBrickUi :: UIOpts -> Journal -> IO ()
runBrickUi opts j = do
  d <- getCurrentDay

  let
    args = words' $ query_ $ reportopts_ $ cliopts_ opts
    scr = head appScreens
    st = (sInitFn scr) d args
         AppState{
            aopts=opts
           ,aargs=args
           ,ajournal=j
           ,aScreen=scr
           ,aPrevScreens=[]
           }
         
    app :: App (AppState) V.Event
    app = App {
        appLiftVtyEvent = id
      , appStartEvent   = return
      , appAttrMap      = const customAttrMap
      , appChooseCursor = showFirstCursor
      , appHandleEvent  = \st ev -> (sHandleFn $ aScreen st) st ev
      , appDraw         = \st -> (sDrawFn $ aScreen st) st
      }

  void $ defaultMain app st
