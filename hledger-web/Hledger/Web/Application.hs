{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module Hledger.Web.Application
  ( makeApplication
  , makeFoundation
  , makeFoundationWith
  ) where

import Data.IORef (newIORef, writeIORef)
import Network.Wai.Middleware.RequestLogger (logStdoutDev, logStdout)
import Network.HTTP.Client (defaultManagerSettings)
import Network.HTTP.Conduit (newManager)
import Yesod.Default.Config

import Hledger.Data (Journal, nulljournal)

import Hledger.Web.Handler.AddR
import Hledger.Web.Handler.MiscR
import Hledger.Web.Handler.EditR
import Hledger.Web.Handler.UploadR
import Hledger.Web.Handler.JournalR
import Hledger.Web.Handler.RegisterR
import Hledger.Web.Import
import Hledger.Web.WebOptions (WebOpts(serve_,serve_api_), corsPolicy)

-- mkYesodDispatch creates our YesodDispatch instance. 
-- It complements the mkYesodData call in App.hs,
-- but must be in a separate file for (TH?) reasons.
mkYesodDispatch "App" resourcesApp

-- This function allocates resources (such as a database connection pool),
-- performs initialization and creates a WAI application. This is also the
-- place to put your migrate statements to have automatic database
-- migrations handled by Yesod.
makeApplication :: WebOpts -> Journal -> AppConfig DefaultEnv Extra -> IO Application
makeApplication opts' j' conf' = do
    foundation <- makeFoundation conf' opts'
    writeIORef (appJournal foundation) j'
    (logWare . (corsPolicy opts')) <$> toWaiApp foundation
  where
    logWare | development  = logStdoutDev
            | serve_ opts' || serve_api_ opts' = logStdout
            | otherwise    = id

makeFoundation :: AppConfig DefaultEnv Extra -> WebOpts -> IO App
makeFoundation conf opts' = do
    manager <- newManager defaultManagerSettings
    s <- staticSite
    jref <- newIORef nulljournal
    return $ App conf s manager opts' jref

-- Make a Foundation with the given Journal as its state.
makeFoundationWith :: Journal -> AppConfig DefaultEnv Extra -> WebOpts -> IO App
makeFoundationWith j' conf opts' = do
    manager <- newManager defaultManagerSettings
    s <- staticSite
    jref <- newIORef j'
    return $ App conf s manager opts' jref
