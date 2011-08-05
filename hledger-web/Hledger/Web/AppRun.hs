{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Hledger.Web.AppRun (
               withApp
              ,withDevelApp
              ,withWaiHandlerDevelApp
              )
where

import Data.Dynamic (Dynamic, toDyn)
import Network.Wai (Application)
import System.IO.Storage (withStore, putValue)
import Yesod.Helpers.Static

import Hledger
import Hledger.Cli
import Hledger.Web.App
import Hledger.Web.Handlers
import Hledger.Web.Settings

-- This line actually creates our YesodSite instance. It is the second half
-- of the call to mkYesodData which occurs in App.hs. Please see
-- the comments there for more details.
mkYesodDispatch "App" resourcesApp

-- This function allocates resources (such as a database connection pool),
-- performs initialization and creates a WAI application. This is also the
-- place to put your migrate statements to have automatic database
-- migrations handled by Yesod.
withApp :: App -> (Application -> IO a) -> IO a
withApp a f = toWaiApp a >>= f

-- Called by yesod devel.
withDevelApp :: Dynamic
withDevelApp = toDyn (withApp a :: (Application -> IO ()) -> IO ())
   where a = App{
              getStatic=static Hledger.Web.Settings.staticdir
             ,appRoot=Hledger.Web.Settings.defapproot
             ,appOpts=[]
             ,appArgs=[]
             ,appJournal=nulljournal
             }

-- Called by wai-handler-devel.
-- Eg: cabal-dev/bin/wai-handler-devel 5001 AppRun withWaiHandlerDevelApp
withWaiHandlerDevelApp :: (Application -> IO ()) -> IO ()
withWaiHandlerDevelApp func = do
  let f = "/repos/hledger/hledger-web/demo.journal"
  ej <- readJournalFile Nothing f
  let Right j = ej
  let a = App{
              getStatic=static Hledger.Web.Settings.staticdir
             ,appRoot=Settings.defapproot
             ,appOpts=[File f]
             ,appArgs=[]
             ,appJournal=j
             }
  withStore "hledger" $ do
    putValue "hledger" "journal" j
    withApp a func
