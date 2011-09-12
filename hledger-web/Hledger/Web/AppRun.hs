{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Hledger.Web.AppRun (
               withApp
              ,withDevelAppPort
              )
where

import Data.Dynamic (Dynamic, toDyn)
import Network.Wai (Application)
import Network.Wai.Middleware.Debug (debugHandle)
import System.IO.Storage (withStore, putValue)
import Yesod.Logger (makeLogger, flushLogger, Logger, logLazyText, logString)
import Yesod.Static

import Hledger
import Hledger.Cli
import Hledger.Web.App
import Hledger.Web.Handlers
import Hledger.Web.Options
import Hledger.Web.Settings

-- This line actually creates our YesodSite instance. It is the second half
-- of the call to mkYesodData which occurs in App.hs. Please see
-- the comments there for more details.
mkYesodDispatch "App" resourcesApp

-- withApp :: App -> (Application -> IO a) -> IO a
-- withApp a f = toWaiApp a >>= f

-- This function allocates resources (such as a database connection pool),
-- performs initialization and creates a WAI application. This is also the
-- place to put your migrate statements to have automatic database
-- migrations handled by Yesod.
withApp :: AppConfig -> Logger -> (Application -> IO a) -> IO a
withApp conf logger f = do
#ifdef PRODUCTION
    s <- static Hledger.Web.Settings.staticDir
#else
    s <- staticDevel Hledger.Web.Settings.staticDir
#endif
    let h = App {settings=conf
                ,getLogger=logger
                ,getStatic=s
                ,appOpts=defwebopts
                ,appArgs=[]
                ,appJournal=nulljournal
              }
    toWaiApp h >>= f

-- withDevelApp :: Dynamic
-- withDevelApp = do
--   s <- static Hledger.Web.Settings.staticdir
--   let a = App{
--               getStatic=s
--              ,appRoot=Hledger.Web.Settings.defapproot
--              ,appOpts=defwebopts
--              ,appArgs=[]
--              ,appJournal=nulljournal
--              }
--   return $ toDyn (withApp a :: (Application -> IO ()) -> IO ())

-- for yesod devel
withDevelAppPort :: Dynamic
withDevelAppPort =
    toDyn go
  where
    go :: ((Int, Application) -> IO ()) -> IO ()
    go f = do
        conf <- Hledger.Web.Settings.loadConfig Hledger.Web.Settings.Development
        let port = appPort conf
        logger <- makeLogger
        logString logger $ "Devel application launched, listening on port " ++ show port
        withApp conf logger $ \app -> f (port, debugHandle (logHandle logger) app)
        flushLogger logger
      where
        logHandle logger msg = logLazyText logger msg >> flushLogger logger

-- -- Called by wai-handler-devel.
-- -- Eg: cabal-dev/bin/wai-handler-devel 5001 AppRun withWaiHandlerDevelApp
-- withWaiHandlerDevelApp :: (Application -> IO ()) -> IO ()
-- withWaiHandlerDevelApp func = do
--   let f = "./test.journal"
--   ej <- readJournalFile Nothing f
--   let Right j = ej
--   let a = App{
--               getStatic=static Hledger.Web.Settings.staticdir
--              ,appRoot="http://localhost:5002"
--              ,appOpts=defwebopts{cliopts_=defcliopts{file_=Just f}}
--              ,appArgs=[]
--              ,appJournal=j
--              }
--   withStore "hledger" $ do
--     putValue "hledger" "journal" j
--     withApp a func
