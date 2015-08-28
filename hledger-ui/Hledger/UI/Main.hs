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
-- import Control.Lens ((^.))
import Control.Monad
-- import Control.Monad.IO.Class (liftIO)
-- import Data.Default
-- import Data.Monoid              -- 
-- import Data.List
import Data.Maybe
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
import Hledger.UI.AccountsScreen as AS
import Hledger.UI.RegisterScreen as RS

----------------------------------------------------------------------

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
    -- XXX move this stuff into Options, UIOpts
    theme = maybe defaultTheme (fromMaybe defaultTheme . getTheme) $
            maybestringopt "theme" $ rawopts_ $ cliopts_ opts
    mshowacct = maybestringopt "register" $ rawopts_ $ cliopts_ opts
    scr = case mshowacct of
      Nothing -> AS.screen
      Just apat -> RS.screen{rsAcct=acct}
        where
          acct = headDef
                 (error' $ "--register "++apat++" did not match any account")
                 $ filter (regexMatches apat) $ journalAccountNames j

    st = (sInitFn scr) d
         AppState{
            aopts=opts
           ,ajournal=j
           ,aScreen=scr
           ,aPrevScreens=[]
           }
         
    app :: App (AppState) V.Event
    app = App {
        appLiftVtyEvent = id
      , appStartEvent   = return
      , appAttrMap      = const theme
      , appChooseCursor = showFirstCursor
      , appHandleEvent  = \st ev -> (sHandleFn $ aScreen st) st ev
      , appDraw         = \st -> (sDrawFn $ aScreen st) st
      }

  void $ defaultMain app st

